#!/usr/bin/env -S scala-cli -S 3
//> using scala 3.4.0
//> using option -no-indent
//> using option -new-syntax
//> using option -Wunused:all
//> using toolkit typelevel:0.1.23
//> using dep io.circe::circe-core:0.14.6
//> using dep io.circe::circe-parser:0.14.6

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.*
import fs2.*
import fs2.io.process.*

object Cleanup extends IOApp {

  // run with:
  //   ./cleanup.scala ./gh-issues-copy.scala --main-class Cleanup -- lorandszakacs/test-target 1 2
  override def run(args: List[String]): IO[ExitCode] = parseArgs(args).flatMap {
    case (targetRepo: TargetRepo, start, end) =>
      Stream
        .range(start, end)
        .evalMap(deleteIssue(targetRepo))
        .meteredStartImmediately(Main.waitingTime)
        .compile
        .drain
        .as(ExitCode.Success)
        .recoverWith(e => printer.error(e).as(ExitCode.Error))
  }

  // lazily rename to run :shrug:
  def parseArgs(args: List[String]): IO[(TargetRepo, Int, Int)] = args match {
    case repo :: start :: finish :: _ => (TargetRepo(repo).pure[IO], IO(start.toInt), IO(finish.toInt)).tupled
    case _ => BadArgument(s"expected REPO START_NR END_NR bit got: ${args.mkString("")}").raiseError
  }

  /*
   * Used for cleanup, be careful :D
   */
  def deleteIssue(repo: TargetRepo)(issueNr: Int) = {
    val command = NonEmptyList.of(
      "gh",
      "issue",
      "delete",
      "--yes", // so it does not prompt
      "--repo",
      repo,
      issueNr.toString
    )
    for {
      _ <- printer.command(command)
      _ <- printer.info(s"deleting issue: $repo/issues/$issueNr")
      _ <- ProcessBuilder(command.head, command.tail)
        .spawn[IO]
        .use(p => (printer.dumpStdout(p), printer.dumpStderr(p)).parTupled.void)
    } yield ()
  }
}
