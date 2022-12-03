package day02

import cats.effect.kernel.Resource
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.*

import java.nio.file.Path
import scala.io.{ BufferedSource, Source, StdIn }
import scala.util.control.NoStackTrace
import scala.util.{ Failure, Success, Try }

object Solution extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      xs   <- readFileFromResources("day02.txt")
      total1 = totalScore(xs)
      total2 = totalScore2(xs)
      _    <- IO(println(s"Result 1: ${total1.sum}"))
      _    <- IO(println(s"Result 2: ${total2.sum}"))
    } yield ()

  private def readFileFromResources(filename: String): IO[List[String]] =
    IO.bracketFull(_ => IO(io.Source.fromResource(filename))) { buffer =>
      IO.delay(buffer.getLines.toList)
    } { (buffer, _) => IO.delay(buffer.close) }

  private def totalScore(moves: List[String]): List[Int] =
    moves
      .map { s =>
        val Array(a, b) = s.split(" ")
        a -> b
      }
      .map {
        // A for Rock, B for Paper, and C for Scissors
        // X for Rock, Y for Paper, and Z for Scissors
        case ("A", "X") => 1 + 3
        case ("A", "Y") => 2 + 6
        case ("A", "Z") => 3 + 0
        case ("B", "X") => 1 + 0
        case ("B", "Y") => 2 + 3
        case ("B", "Z") => 3 + 6
        case ("C", "X") => 1 + 6
        case ("C", "Y") => 2 + 0
        case ("C", "Z") => 3 + 3
      }

  private def totalScore2(moves: List[String]): List[Int] =
    moves
      .map { s =>
        val Array(a, b) = s.split(" ")
        a -> b
      }
      .map {
        // A for Rock, B for Paper, and C for Scissors
        // X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
        // 1 for Rock, 2 for Paper, and 3 for Scissors
        case ("A", "X") => 3 + 0
        case ("A", "Y") => 1 + 3
        case ("A", "Z") => 2 + 6
        case ("B", "X") => 1 + 0
        case ("B", "Y") => 2 + 3
        case ("B", "Z") => 3 + 6
        case ("C", "X") => 2 + 0
        case ("C", "Y") => 3 + 3
        case ("C", "Z") => 1 + 6
      }

}
