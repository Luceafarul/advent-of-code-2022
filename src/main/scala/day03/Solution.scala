package day03

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
      xs <- readFileFromResources("day03.txt")
      part1Result = xs
        .map(partOne)
        .map {
          case Some(value) => value
          case None        => 0
        }
        .sum
      part2Result = partTwo(xs).map {
        case Some(value) => value
        case None        => 0
      }.sum
      _ <- IO(println(s"Result 1: $part1Result"))
      _ <- IO(println(s"Result 2: $part2Result"))
    } yield ()

  private def readFileFromResources(filename: String): IO[List[String]] =
    IO.bracketFull(_ => IO(io.Source.fromResource(filename))) { buffer =>
      IO.delay(buffer.getLines.toList)
    } { (buffer, _) => IO.delay(buffer.close) }

  private val lowerLetters = ('a' to 'z').toList
  private val upperLetters = ('A' to 'Z').toList

  private val weight = 1 to (lowerLetters ++ upperLetters).size

  private val lettersAndWeight = (lowerLetters ++ upperLetters).zip(weight).toMap

  private def partOne(s: String): Option[Int] = {
    val (firstPart, secondPart) = s.splitAt(s.length / 2)

    firstPart.diff(secondPart)

    firstPart.find(c => secondPart.contains(c)).flatMap { c =>
      lettersAndWeight.get(c)
    }
  }

  private def partTwo(xs: List[String]) = {
    xs.sliding(3, 3).map { list =>
      val List(a, b, c) = list
      a.filter(ch => b.contains(ch)).find(ch => c.contains(ch)).flatMap(lettersAndWeight.get)
    }
  }
}
