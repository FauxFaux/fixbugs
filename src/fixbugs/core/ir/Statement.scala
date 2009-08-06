package fixbugs.core.ir

/**
 * Pattern matches a Java statement
 */
sealed abstract class Statement {}
/** Pattern match a statement, and bind to a metavariable */
case class Label(lbl:Metavariable,stmt:Statement) extends Statement
/** Matches one of more statements */
case class Wildcard() extends Statement
case class Assignment(what:Metavariable,to:Expression) extends Statement
case class IfElse(trueBlock:SBlock,falseBlock:SBlock) extends Statement
case class While(body:SBlock) extends Statement
case class TryCatchFinally(tryBlock:SBlock,catchBlock:SBlock,finallyBlock:SBlock) extends Statement
case class SideEffectingExpression(expr:Expression) extends Statement

/**
 * Abbreviations for common patterns
 */
object Statement {
  def WildBlock = SBlock(List(Wildcard()))
  def If(trueBlock:SBlock) = IfElse(trueBlock,WildBlock)
  def TryCatch(tryBlock:SBlock,catchBlock:SBlock) = TryCatchFinally(tryBlock,catchBlock,WildBlock)
  def TryFinally(tryBlock:SBlock,finallyBlock:SBlock) = TryCatchFinally(tryBlock,WildBlock,finallyBlock)
}
