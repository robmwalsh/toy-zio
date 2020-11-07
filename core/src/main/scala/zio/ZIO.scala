package zio

import macros._
import Source._

import scala.util.Random

sealed trait Box[A] {
  self =>
  def *>[B](that: Box[B]): Box[B] = self.flatMap((_: A) => that)

  def map[B](f: A -=> B): Box[B] = self.flatMap(f andThen Box.succeed)

  def flatMap[B](f: A -=> Box[B]): Box[B] = Box.FlatMap(self, f)

  def zip[B](that: Box[B]): Box[(A, B)] = self.flatMap((a: A) => that.map((b: B) => (a, b)))
}

trait Debug {
  val debugInfo: DebugInfo
}

abstract class -=>[-A, +B](f: A => B) extends (A => B) with Debug {

  override def apply(a: A): B = f(a)

}

object -=> {
  implicit def f2tf[A, B](f: A => B): A -=> B = new -=>[A, B](f) {
    override lazy val debugInfo: DebugInfo = Source.debug(f)
  }
}

abstract class TracedFunction0[+A](f: () => A) extends (() => A) with Debug {
  override def apply(): A = f()
}

object TracedFunction0 {
  implicit def f2tf0[A](f: () => A): TracedFunction0[A] = new TracedFunction0(f) {
    override val debugInfo: DebugInfo = Source.debug(f)
  }

  implicit def bn2tf0[A](a: => A): TracedFunction0[A] = new TracedFunction0(a) {
    override val debugInfo: DebugInfo = Source.debug(a)
  }
}

object Box {
  def succeed[A](value: A): Box[A] = Box.Succeed(value)

  private def apply[A](effect: => A): Box[A] = Box.EffectTotal(() => effect)

  def apply[A](effect: TracedFunction0[A]): Box[A] = Box.EffectTotal(effect)

  def run[A](box: Box[A]): A = box match {
    case Succeed(value) => value
    case EffectTotal(effect) =>
      println(s"Effect Total: ${effect}")
      effect()
    case FlatMap(box, f) =>
      println(s"FlatMap: ${f.debugInfo}")
      run(f(run(box)))
  }

  case class FlatMap[A, B](box: Box[A], f: -=>[A, Box[B]]) extends Box[B]

  case class Succeed[A](value: A) extends Box[A]

  case class EffectTotal[A](effect: TracedFunction0[A]) extends Box[A]
}

object Example {

  def putStrLn(string: String): Box[Unit] = Box(println(string))

  val program: Box[Int] = for {
    x <- Box(Random.nextInt(999))
    y <- Box(Random.nextInt(999))
    _ <- putStrLn(s"x + y = ${x + y}")
  } yield 1

  def main(args: Array[String]): Unit =
    print(Box.run(program))
}