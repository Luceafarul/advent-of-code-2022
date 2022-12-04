package day04

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
      xs         <- readFileFromResources("day04.txt")
      part1Result = partOne(xs)
      _          <- IO(println(s"Result 1: $part1Result"))
      part2Result = partTwo(xs)
      _          <- IO(println(s"Result 2: $part2Result"))
    } yield ()

  private def readFileFromResources(filename: String): IO[List[String]] =
    IO.bracketFull(_ => IO(io.Source.fromResource(filename))) { buffer =>
      IO.delay(buffer.getLines.toList)
    } { (buffer, _) => IO.delay(buffer.close) }

  private def partOne(xs: List[String]): Int =
    xs.map(_.toRangePair).count(_.fullyContains)

  private def partTwo(xs: List[String]): Int =
    xs.map(_.toRangePair).count(_.overlap)

  extension (s: String) {
    private def toRangePair: RangePair = {
      val Array(a, b) = s.split(",")
      RangePair(a.toRange, b.toRange)
    }

    private def toRange: Range = {
      val Array(start, end) = s.split("-")
      start.toInt to end.toInt
    }
  }

  private final case class RangePair(rangeOne: Range, rangeTwo: Range) {
    def fullyContains: Boolean = rangeOne.containsSlice(rangeTwo) || rangeTwo.containsSlice(rangeOne)

    def overlap: Boolean = rangeOne.exists(rangeTwo.contains)
  }
}
