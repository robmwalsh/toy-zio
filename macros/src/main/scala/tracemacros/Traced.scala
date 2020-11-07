package tracemacros

import tracemacros.Traced.DebugInfo
import scala.language.experimental.macros
import scala.language.implicitConversions

trait Traced extends Serializable {
  val debugInfo: DebugInfo
}
object Traced {
  case class DebugInfo(lineNumber: Int, source: String)
}

abstract class -=>[-A, +B](f: A => B) extends (A => B) with Traced {
  override def apply(a: A): B = f(a)
}

object -=> {

  def apply[A, B](f: A => B)(debugInfo0: DebugInfo): -=>[A, B] = new -=>(f) {
    override val debugInfo: DebugInfo = debugInfo0
  }
  implicit def function2tf[A, B](f: A => B): A -=> B = macro Macros.function2tfImpl[A, B]

  abstract class TFn0[+A](f: () => A) extends (() => A) with Traced {
    override def apply(): A = f()
  }

  object TFn0 {
    implicit def thunk2tf0[A](f: () => A): TFn0[A] = ??? // macro f2tf0Impl

    implicit def byName2tf0[A](a: => A): TFn0[A] = ??? // macro bn2tf0Impl
  }

}
