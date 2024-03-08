#!/usr/bin/env -S scala-cli -S 3
//> using scala 3.4.0
//> using option -no-indent
//> using option -new-syntax
//> using option -Wunused:all
//> using toolkit typelevel:0.1.23
//> using dep org.typelevel::cats-parse:1.0.0
//> using dep io.circe::circe-core:0.14.6
//> using dep io.circe::circe-parser:0.14.6

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.*
import io.circe
import fs2.*
import fs2.io.process.*
import scala.concurrent.duration.*

// script that copies $LIMIT number of issues and their comments from the $SOURCE to the $TARGET github repo.
// it will include a link to the original issue in the description of the copied issue
// it will include a link to the original comment at the end of the copied comment
//
// what this script assumes about the state of the terminal it's run from:
// - that you have installed github cli https://cli.github.com/
// - that you did 'gh auth' and all that
//
// the script expects 2 required arguments + 1 optional:
//
//SOURCE = lorandszakacs/gh-issues-copy
//TARGET = lorandszakacs/test-target
//LIMIT = 1 -- optional, defaults to 50
//invoke script like:
//
// $ ./gh-issues-copy.scala -- lorandszakacs/gh-issues-copy lorandszakacs/test-target 40
//
// we use 3 gh cli commands in the implementation of this script:
// https://cli.github.com/manual/gh_issue_list
// https://cli.github.com/manual/gh_issue_create
// https://cli.github.com/manual/gh_issue_comment
object Main extends IOApp {
  // we could use a better rate limiter, tbh, but fs2.Stream.metered will have to do
  given waitingTime: FiniteDuration = 4.seconds

  // prints out commands
  val debugCommands: Boolean = false
  // enables some debug. Different from debugCommands. Prints json from gh responses
  val debug: Boolean = false

  override def run(args: List[String]): IO[ExitCode] = {
    val program = for {

      (source, target, limit) <- parseArgs(args)
      _ <- printer.info(
        s"""
           |Provided arguments:
           |    sourceRepo = $source
           |    targetRepo = $target
           |    nrOfIssues = $limit
           |""".stripMargin.trim
      )

      _ <- gh
        .listOpenIssues(source, limit)
        .evalMap(gh.copyIssue(target))
        .flatMap(copiedIssue => copiedIssue.commentsToCopyStream.evalMap(gh.copyComment(copiedIssue.newUrl)))
        .meteredStartImmediately(waitingTime)
        .compile
        .drain
    } yield ExitCode.Success
    program.recoverWith(e => printer.error(e).as(ExitCode.Error))
  }

  private def parseArgs(args: List[String]): IO[(SourceRepo, TargetRepo, Limit)] = args match {
    case s :: t :: l :: _ => (SourceRepo(s).pure[IO], TargetRepo(t).pure[IO], Limit(l)).tupled
    case s :: t :: _      => (SourceRepo(s).pure[IO], TargetRepo(t).pure[IO], Limit(50)).tupled
    case _ => badArgument("expected at least two params that should be 'OWNER/REPO' github repo paths").raiseError
  }

}

object gh {

  // https://cli.github.com/manual/gh_issue_list
  def listOpenIssues(source: SourceRepo, limit: Limit): Stream[IO, Issue] = {
    val command = NonEmptyList.of(
      "gh",
      "issue",
      "list",
      "--repo",
      source,
      "--json",
      """"id","title","body","url","comments","createdAt","author"""",
      "--state",
      "open",
      "--limit",
      limit.toString
    )
    val parsed: IO[List[Issue]] =
      printer.command(command) *> ProcessBuilder(command.head, command.tail).spawn[IO].use { p =>
        (
          printer.dumpStderr(p),
          p.stdout
            .through(fs2.text.utf8.decode)
            .compile
            .string
            .map((jsonBlob: String) => circe.parser.decode[List[Issue]](jsonBlob))
            .flatMap(_.liftTo[IO])
        ).parTupled
          .map(_._2)
          .flatTap(issues => printer.info(s"Found: ${issues.size} issues on github", indent = 4))
      }
    Stream.evalSeq(parsed) // too lazy to bring in fs2-data-json-circe
  }

  // https://cli.github.com/manual/gh_issue_create
  def copyIssue(target: TargetRepo)(issue: Issue): IO[CopiedIssue] =
    for {
      bodyWithNoTags <- UserTag.findAllTagsAndReplaceWithLinks(issue.body)
      title = CommandArgString(issue.title)
      source = s"This issue was copied over from: ${md.link(issue.url)}"
      openedBy = s"It was opened by: ${md.link(issue.author.login, gh.userProfileLink(issue.author.login))}"
      header = s"$source${md.nl}$openedBy"
      newBody = CommandArgString(s"$header${md.horizontalLine}$bodyWithNoTags")
      command = NonEmptyList.of(
        "gh",
        "issue",
        "create",
        "--repo",
        target,
        "--title",
        title,
        "--body",
        newBody
      )

      _ <- printer.debugJson("issueNoCommentsJson:", issue.copy(comments = List.empty))
      _ <- printer.command(command)
      newIssueURL <- ProcessBuilder(command.head, command.tail)
        .spawn[IO]
        .use(p => (parseNewIssueURLFromStdout(p), printer.dumpStderr(p)).parTupled.map(_._1))

      _ <- printer.info(s"created new issue @ $newIssueURL. starting to copy over comments...")
    } yield CopiedIssue(newIssueURL, issue.comments)

  // https://cli.github.com/manual/gh_issue_comment
  def copyComment(newIssueURL: NewIssueURL)(comment: Comment): IO[Unit] = {
    // for some reason github expands comment.urls into something clickable, while it does not do so with issues.
    val commentLink = s"This comment was copied over from: ${comment.url}"
    val authorProfileLink = md.link(comment.author.login, gh.userProfileLink(comment.author.login))
    val author = s"It was written by: $authorProfileLink"
    val header = s"$commentLink${md.nl}$author"

    for {
      bodyWithLinksInsteadOfTags <- UserTag.findAllTagsAndReplaceWithLinks(comment.body)
      body = CommandArgString(s"$header${md.horizontalLine}$bodyWithLinksInsteadOfTags")
      command = NonEmptyList.of(
        "gh",
        "issue",
        "comment",
        newIssueURL,
        "--body",
        body
      )
      _ <- printer.debugJson("commentJson:", comment)
      _ <- printer.command(command)
      _ <- ProcessBuilder(command.head, command.tail)
        .spawn[IO]
        .use(p => (printer.dumpStdout(p), printer.dumpStderr(p)).parTupled.void)
    } yield ()
  }

  // gh issue create --repo $TARGET --title "TITLE" --body-file "$FILE"
  // yields output:
  //
  // Creating issue in $TARGET
  //
  // https://github.com/$TARGET/issues/4
  //
  // returns: https://github.com/$TARGET/issues/4
  private def parseNewIssueURLFromStdout(p: Process[IO]): IO[NewIssueURL] =
    p.stdout
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .filterNot(_.isBlank)
      .compile
      .last
      .flatMap(_.liftTo[IO](Bug("Expected to have a github link as output. We somehow failed parsing.")))
      .map(_.trim)
      .map(NewIssueURL.apply)

  def userProfileLink(username: String): String = s"https://github.com/$username"
}

object printer {
  private lazy val seedling = "🌱"
  private lazy val fire = "🔥"
  private lazy val point = "👉"
  private lazy val what = "🤨"
  private lazy val github = "🐙🐈"
  private lazy val space = " "

  def info(s: String, indent: Int = 1): IO[Unit] = printlns(s, seedling, indent)
  def error(s: String, indent: Int = 1): IO[Unit] = printlns(s, fire, indent)
  def error(e: Throwable): IO[Unit] = error(
    s = s"""
         |Error: ${e.getClass.getName}
         |Message: ${e.getMessage}
         |Cause: ${Option(e.getCause).map(_.getMessage).getOrElse("none")}
         |""".stripMargin.trim,
    indent = 1
  )

  def dumpStdout(p: Process[IO], indent: Int = 1): IO[Unit] =
    printlns(
      p.stdout.through(fs2.text.utf8.decode).through(fs2.text.lines).filterNot(_.isBlank),
      s"$github$seedling",
      indent
    )

  def dumpStderr(p: Process[IO], indent: Int = 1): IO[Unit] =
    printlns(
      p.stderr.through(fs2.text.utf8.decode).through(fs2.text.lines).filterNot(_.isBlank),
      s"$github$fire",
      indent
    )

  def command(c: NonEmptyList[String]): IO[Unit] =
    if Main.debugCommands then printlns(s"running command:   ${c.mkString_(" ")}", point, 1) else IO.unit

  def debug(s: String, indent: Int = 1): IO[Unit] = if Main.debug then printlns(s, what, indent) else IO.unit
  def debugJson[A](prefix: String, a: A, indent: Int = 4)(using enc: circe.Encoder[A]): IO[Unit] =
    if Main.debug then printlns(s"$prefix: ${enc(a).printWith(circe.Printer.noSpaces)}", what, indent) else IO.unit

  private def printlns(s: Stream[IO, String], padding: String, indent: Int): IO[Unit] =
    lines(s, padding, indent).evalMap(IO.println).compile.drain

  private def printlns(s: String, padding: String, indent: Int): IO[Unit] =
    lines(s, padding, indent).evalMap(IO.println).compile.drain

  private def lines(s: String, padding: String, indent: Int): Stream[IO, String] =
    lines(Stream.emit(s), padding, indent)

  private def lines(s: Stream[IO, String], padding: String, indent: Int): Stream[IO, String] =
    if indent >= 0 then s.through(fs2.text.lines).map(line => s"$padding${space.repeat(indent)}$line")
    else Stream.raiseError(bug(s"Indent should be >= 0 but was: $indent"))
}

object md {
  lazy val nl: String = "\r\n"
  lazy val horizontalLine: String = s"$nl$nl---$nl$nl"
  def link(desc: String, link: String): String = s"[$desc]($link)"
  def link(s: String): String = s"[$s]($s)"
}

/** Instantiate only after you've copied over the issue */
case class CopiedIssue(
    newUrl: NewIssueURL,
    private val commentsToCopy: List[Comment]
) {

  /** Emits comments from oldest to newest, so that you can immediately create them as they are emitted
    */
  def commentsToCopyStream: Stream[IO, Comment] = Stream.emits(commentsToCopy.sortBy(_.createdAt))
}

case class Issue(
    id: String,
    title: String,
    body: String,
    url: String,
    author: Author,
    comments: List[Comment]
) derives circe.Decoder,
      circe.Encoder.AsObject

case class Author(
    login: String
) derives circe.Decoder,
      circe.Encoder.AsObject

case class Comment(
    id: String,
    body: String,
    url: String,
    author: Author,
    createdAt: String // we only keep as string. Just so we can explicitly sort by it
) derives circe.Decoder,
      circe.Encoder.AsObject

/** A username found in github content. Can only be identified by a preceding '@' The value does not actually contain
  * '@'
  */
opaque type UserTag <: String = String
object UserTag {
  import cats.parse.Rfc5234
  import cats.parse.Parser
  private val At: Char = '@'

  // private val AtParser = Parser.char(At)
  private val userNameStart = Rfc5234.alpha
  private val userNameMiddle = Rfc5234.alpha | Rfc5234.digit | Parser.charIn('-')

  // max 39 chars. Technically incorrect since the last char cannot be - :shrug:
  private val usernameParser: Parser[UserTag] = (for {
    first <- userNameStart
    middle <- userNameMiddle.rep(1, 38)
  } yield middle.prepend(first)).string

  // FIXME: a non hacky parser looks like this, roughly. But instead of figuring it out,
  //   we write the hack with `s.split(At)`, lol
  // private val parserAll: Parser0[List[UserTag]] =
  //  (Parser.until(AtParser).void *> usernameParser).rep0 <* Parser.anyChar.rep0

  private def findAll(s: String): Either[Throwable, List[UserTag]] = {
    // this is what we call a hack
    val substrings: Array[String] = s.split(At).drop(1) // first part never contains any element
    val allTags: Either[Throwable, List[UserTag]] = substrings.toList.traverse { substring =>
      usernameParser
        .parse(substring) // we know for sure that whatever starts in the string is a username
        .map(_._2) // we discard whatever is after it
        .map(_.stripSuffix("-")) // since we are lazy and our parser can also glob up an invalid dash at the end
        .leftMap(e => new Bug(s"parser error should never happen: ${e.toString}"))
    }

    // parserAll.parseAll(s).leftMap(e => new Bug(s"parser error should never happen: ${e.toString}"))
    allTags
  }

  /** Replaces all @tag in the given string with a link to the github profile.
    */
  def findAllTagsAndReplaceWithLinks(s: String): IO[String] =
    for {
      taggedUsers: List[UserTag] <- UserTag.findAll(s) match {
        case Left(e)      => printer.error(e).as(List.empty[UserTag]) // we just log errors and not fail
        case Right(value) => value.pure[IO]
      }

      _ <- printer.debug(s"Found following tags: $taggedUsers")

      // "@username" becomes [username](https://github.com/username)
      bodyWithLinksInsteadOfTags: String =
        taggedUsers.foldRight(s) { case (userTag: UserTag, commentBody: String) =>
          commentBody.replaceAll(userTag.withAt, md.link(userTag, gh.userProfileLink(userTag)))
        }
    } yield bodyWithLinksInsteadOfTags

  extension (t: UserTag) {
    private def withAt: String = s"$At$t"
  }
}

opaque type SourceRepo <: String = String
object SourceRepo { def apply(s: String): SourceRepo = s }

opaque type TargetRepo <: String = String
object TargetRepo { def apply(s: String): TargetRepo = s }

opaque type NewIssueURL <: String = String
object NewIssueURL { def apply(s: String): NewIssueURL = s }

opaque type Limit <: Int = Int
object Limit {
  def apply(l: Int): IO[Limit] = l.pure[IO].ensure(badArgument("limit has to be > 0"))(_ > 0)
  def apply(s: String): IO[Limit] = IO(s.toInt).flatMap(this.apply)
}

opaque type CommandArgString <: String = String
object CommandArgString {
  // thanks you fs2.io.process for handling all these aspects for us :relieved:
  def apply(s: String): CommandArgString = s
//    val escapedSingleQuotes = s.replace("'", "\\'")
//    s"$escapedSingleQuotes"
}

final class BadArgument(s: String) extends IllegalArgumentException(s) with scala.util.control.NoStackTrace
def badArgument(s: String): Throwable = new BadArgument(s)

final class Bug(s: String) extends IllegalArgumentException(s) with scala.util.control.NoStackTrace
def bug(s: String): Throwable = new Bug(s"This is a bug in the script. What: $s")
