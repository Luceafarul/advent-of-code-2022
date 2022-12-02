package day01

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
      xs      <- readFileFromResources("day01.txt")
      calories = toCalories(xs)
      sum      = maxCalories(calories)
      _        = println(s"Top 1 calories: $sum")
      top3Sum  = calories.sorted.takeRight(3).map(_.values.sum).sum
      _        = println(s"Top 3 calories sum: $top3Sum")
    } yield ()

  final case class ReadError(error: String) extends NoStackTrace
  final case class ConvertError(error: String) extends NoStackTrace

  final case class Calories(values: List[Int])
  object Calories {
    given Ordering[Calories] = new Ordering[Calories]:
      override def compare(x: Calories, y: Calories): Int = x.values.sum.compare(y.values.sum)
  }

  private def readFileFromResources(filename: String): IO[List[String]] =
    IO.bracketFull(_ => IO(io.Source.fromResource(filename))) { buffer =>
      IO.delay(buffer.getLines.toList)
    } { (buffer, _) => IO.delay(buffer.close) }

  private def toCalories(xs: List[String]): List[Calories] =
    val res = xs.foldLeft(List.empty[List[String]]) { (acc, elem) =>
      if acc.isEmpty then acc :+ List(elem)
      else if elem.isEmpty then acc :+ List.empty
      else acc.init :+ (acc.last :+ elem)
    }
    res
      .map(_.map(_.toInt))
      .map(Calories.apply)

  private def maxCalories(calories: List[Calories]): Int =
    calories.map(_.values.sum).max
}
