#!/usr/bin/env -S scala-cli -S 3
//> using scala 3.4.0
//> using option -no-indent
//> using option -source:future
//> using option -new-syntax
//> using option -Wunused:all
//> using toolkit typelevel:0.1.23
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
//LIMIT = 10 -- optional, defaults to 50
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
  private val waitingTime: FiniteDuration = 3.seconds

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

      _ <- ghListIssues(source, limit)
        .evalMap(ghCopyOverIssue(target))
        .metered(waitingTime)
        .compile
        .drain
    } yield ExitCode.Success
    program.recoverWith(e => printer.error(e).as(ExitCode.Error))
  }

  private val nl = "\r\n"

  private def parseArgs(args: List[String]): IO[(SourceRepo, TargetRepo, Limit)] = args match {
    case s :: t :: l :: _ => (SourceRepo(s).pure[IO], TargetRepo(t).pure[IO], Limit(l)).tupled
    case s :: t :: _      => (SourceRepo(s).pure[IO], TargetRepo(t).pure[IO], Limit(50)).tupled
    case _ => badArgument("expected at least two params that should be 'OWNER/REPO' github repo paths").raiseError
  }

  // https://cli.github.com/manual/gh_issue_list
  private def ghListIssues(source: SourceRepo, limit: Limit): Stream[IO, Issue] = {
    val command = NonEmptyList.of(
      "gh",
      "issue",
      "list",
      "--repo",
      source,
      "--json",
      """"id","title","body","url","comments"""",
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
  private def ghCopyOverIssue(target: TargetRepo)(issue: Issue): IO[Unit] = {
    val title = CommandArgString(issue.title)
    val body = CommandArgString(
      s"${issue.body}$nl$nl---$nl${nl}This issue was copied over from: [${issue.url}](${issue.url})"
    )
    val command = NonEmptyList.of(
      "gh",
      "issue",
      "create",
      "--repo",
      target,
      "--title",
      title,
      "--body",
      body
    )
    for {
      _ <- printer.debugJson("issueNoCommentsJson:", issue.copy(comments = List.empty))
      _ <- printer.command(command)
      newIssueURL <- ProcessBuilder(command.head, command.tail)
        .spawn[IO]
        .use(p => (parseNewIssueURLFromStdout(p), printer.dumpStderr(p)).parTupled.map(_._1))

      _ <- printer.info(s"created new issue @ $newIssueURL. starting to copy over comments...")

      _ <- Stream
        .emits(issue.comments)
        .evalMap(ghCopyComment(newIssueURL))
        .meteredStartImmediately(waitingTime)
        .compile
        .drain
    } yield ()
  }

  // https://cli.github.com/manual/gh_issue_comment
  private def ghCopyComment(newIssueURL: NewIssueURL)(comment: Comment): IO[Unit] = {
    val body = CommandArgString(
      s"${comment.body}$nl$nl---$nl${nl}This comment was copied over from: [${comment.url}](${comment.url})"
    )
    val command = NonEmptyList.of(
      "gh",
      "issue",
      "comment",
      newIssueURL,
      "--body",
      body
    )
    for {
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

}

object gh {}

object printer {
  private lazy val seedling = "🌱"
  private lazy val fire = "🔥"
  private lazy val point = "👉"
  private lazy val what = "🤨"
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
    printlns(p.stdout.through(fs2.text.utf8.decode), seedling, indent)

  def dumpStderr(p: Process[IO], indent: Int = 1): IO[Unit] =
    printlns(p.stderr.through(fs2.text.utf8.decode), fire, indent)

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

case class Issue(
    id: String,
    title: String,
    body: String,
    url: String,
    comments: List[Comment]
) derives circe.Decoder,
      circe.Encoder.AsObject

case class Comment(
    id: String,
    body: String,
    url: String
) derives circe.Decoder,
      circe.Encoder.AsObject

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
  def apply(s: String): CommandArgString = {
    val escapedSingleQuotes = s.replace("'", "\\'")
    s"$escapedSingleQuotes"
  }
}

final class BadArgument(s: String) extends IllegalArgumentException(s) with scala.util.control.NoStackTrace
def badArgument(s: String): Throwable = new BadArgument(s)

final class Bug(s: String) extends IllegalArgumentException(s) with scala.util.control.NoStackTrace
def bug(s: String): Throwable = new Bug(s"This is a bug in the script. What: $s")
