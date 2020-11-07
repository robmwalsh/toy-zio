package macros

import scala.language.experimental.macros

object Source {
  implicit def debugInfo: DebugInfo = macro Macros.lineImpl
}

case class DebugInfo(lineNumber: Int)

object Macros {
  def lineImpl(c: scala.reflect.macros.blackbox.Context): c.Expr[DebugInfo] = {
    import c.universe._
    val line = c.enclosingPosition.line
    c.Expr[DebugInfo](q"DebugInfo($line)")
  }
}
