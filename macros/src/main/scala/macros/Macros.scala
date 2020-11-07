package macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait Debug {
  val debugInfo: DebugInfo
}

sealed case class -=>[-A, +B](f: A => B) extends (A => B) with Debug {
  override def apply(a: A): B = f(a)
}

object -=> {
  implicit def f2tf[A, B](f: A => B): A -=> B = macro ???
}

sealed case class TracedFunction0[+A](f: () => A) extends (() => A) with Debug {
  override def apply(): A = f()
}

object TracedFunction0 {
  implicit def f2tf0[A](f: () => A): TracedFunction0[A] = macro ???

  implicit def bn2tf0[A](a: => A): TracedFunction0[A] = macro ???
}

object Source {
  //implicit def debugInfo: DebugInfo = macro Macros.lineImpl

}

case class DebugInfo(lineNumber: Int, source: String)
object DebugInfo {
  implicit def debug[A](f: A): DebugInfo = macro Macros.debugImpl[A]
}

object Macros {
  def lineImpl(
      c: blackbox.Context
  ): c.Expr[DebugInfo] = {
    import c.universe._
    val line = c.enclosingPosition.line
    c.Expr[DebugInfo](q"DebugInfo($line)")
  }

  def debugImpl[A: c.WeakTypeTag](c: blackbox.Context)(f: c.Expr[A]): c.Expr[DebugInfo] = {
    import c.universe._
    val line        = c.enclosingPosition.line
    val fileContent = new String(f.tree.pos.source.content)
    val start = f.tree.collect { case treeVal =>
      treeVal.pos match {
        case NoPosition ⇒ Int.MaxValue
        case p          ⇒ p.startOrPoint
      }
    }.min
    val g      = c.asInstanceOf[reflect.macros.runtime.Context].global
    val parser = g.newUnitParser(fileContent.drop(start))
    parser.expr()
    val end   = parser.in.lastOffset
    val txt   = fileContent.slice(start, start + end)
    val debug = q"""DebugInfo($line, $txt)"""
    c.Expr[DebugInfo](debug)
  }

}
