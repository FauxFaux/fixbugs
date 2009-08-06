package fixbugs.core.ir

/**
 * Pattern Matching for Java expressions
 */
sealed abstract class Expression {}
case class Literal(lit:String) extends Expression
case class Method(name:String,args:List[Expression])
case class BinOp(l:Expression,r:Expression,op:Operator) extends Expression
case class UnOp(r:Expression,op:Operator) extends Expression

sealed abstract class Operator {}
case class +() extends Operator
case class -() extends Operator
case class *() extends Operator
case class /() extends Operator
case class %() extends Operator
case class &() extends Operator
case class &&() extends Operator
case class |() extends Operator
case class ||() extends Operator
case class !() extends Operator

