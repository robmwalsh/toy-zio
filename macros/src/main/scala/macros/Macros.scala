package macros

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Source {
  implicit def debugInfo: DebugInfo = macro Macros.lineImpl
  def debug[A](f: A): DebugInfo = macro Macros.debugImpl[A]
}

case class DebugInfo(lineNumber: Int, source: String)

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
