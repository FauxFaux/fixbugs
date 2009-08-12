package fixbugs.core.ir

/**
 * Pattern matches a Java statement
 */
sealed abstract class Statement {}
/** Pattern match a statement, and bind to a metavariable */
case class Label(lbl:String,stmt:Statement) extends Statement
/** Matches one of more statements */
case class Wildcard() extends Statement
case class Assignment(what:String,to:Expression) extends Statement
case class IfElse(cond:Expression,trueBlock:Statement,falseBlock:Statement) extends Statement
case class While(cond:Expression,body:Statement) extends Statement
// TODO: more loops
case class TryCatchFinally(tryBlock:SBlock,catchBlock:SBlock,finallyBlock:SBlock) extends Statement
case class SideEffectExpr(expr:Expression) extends Statement
case class SBlock(stmts:List[Statement]) extends Statement
case class Return(expr:Expression) extends Statement
case class Throw(expr:Expression) extends Statement
case class For(init:List[Expression],cond:Expression,updaters:List[Expression],body:Statement) extends Statement
case class ForEach(typee:TypePattern,id:String,expr:Expression,body:Statement)  extends Statement

/**
 * Abbreviations for common patterns
 */
object Statement {
  def WildBlock = SBlock(List(Wildcard()))
  def If(cond:Expression,trueBlock:SBlock) = IfElse(cond,trueBlock,WildBlock)
  def TryCatch(tryBlock:SBlock,catchBlock:SBlock) = TryCatchFinally(tryBlock,catchBlock,WildBlock)
  def WildLabel(lbl:String) = Label(lbl,Wildcard())
}
