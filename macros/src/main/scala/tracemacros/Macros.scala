package tracemacros

import tracemacros.Traced.DebugInfo

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macros {
  def lineImpl(
      c: blackbox.Context
  ): c.Expr[DebugInfo] = {
    import c.universe._
    val line = c.enclosingPosition.line
    c.Expr[DebugInfo](q"DebugInfo($line)")
  }

  def function2tfImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(
      f: c.Expr[A => B]
  ): c.Expr[A -=> B] = {
    import c.universe._
    println(showRaw(f))
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
    val end = parser.in.lastOffset
    val txt = fileContent.slice(start, start + end)
    println
    println(s"txt $txt")
    println
    val debug = q"DebugInfo($line, $txt.intern)"
    println(showRaw(debug))
    c.Expr[(A -=> B)](q"-=>($f)($debug)")
  }
}
