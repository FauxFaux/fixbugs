package fixbugs.core.ir


/**
 * Pattern matches a Java statement
 */
sealed abstract class Statement {}
/** Pattern match a statement, and bind to a metavariable */
case class Label(lbl:String,stmt:Statement) extends Statement
/** Matches one of more statements */
case class Wildcard() extends Statement
case class Skip() extends Statement
case class StatementReference(lbl:String) extends Statement
// modifiers, match fragments
case class Assignment(typee:TypePattern,what:String,to:Expression) extends Statement
case class IfElse(cond:Expression,trueBlock:Statement,falseBlock:Statement) extends Statement
case class While(cond:Expression,body:Statement) extends Statement
case class TryCatchFinally(tryBlock:SBlock,catchBlock:SBlock,finallyBlock:SBlock) extends Statement
case class SideEffectExpr(expr:Expression) extends Statement
case class SBlock(stmts:List[Statement]) extends Statement {
    override def toString() = "{"+(stmts.foldLeft(new StringBuilder()){(acc,s) => acc.append("\n").append(s)}).toString+"\n}"
}
case class Return(expr:Expression) extends Statement
case class Throw(expr:Expression) extends Statement
case class For(init:List[Expression],cond:Expression,updaters:List[Expression],body:Statement) extends Statement
case class ForEach(typee:TypePattern,id:String,expr:Expression,body:Statement)  extends Statement
case class Do(body:Statement,cond:Expression) extends Statement
case class Synchronized(body:Statement,lock:Expression) extends Statement
case class Switch(stmts:List[Statement],cond:Expression) extends Statement
case class DefaultCase() extends Statement
case class Switchcase(expr:Expression) extends Statement
case class Break(metavar:String) extends Statement
case class Continue(metavar:String) extends Statement
case class Assert(expr:Expression) extends Statement
case class Constructor(exprs:List[Expression]) extends Statement
case class SuperConstructor(exprs:List[Expression]) extends Statement

/**
 * Abbreviations for common patterns
 */
object Statement {
  def WildBlock = SBlock(List(Wildcard()))
  def If(cond:Expression,trueBlock:SBlock) = IfElse(cond,trueBlock,WildBlock)
  def TryCatch(tryBlock:SBlock,catchBlock:SBlock) = TryCatchFinally(tryBlock,catchBlock,WildBlock)
  def WildLabel(lbl:String) = Label(lbl,Wildcard())
}

