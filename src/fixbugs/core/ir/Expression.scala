package fixbugs.core.ir

import org.eclipse.jdt.core.dom.InfixExpression.{Operator => InfixOp}
import org.eclipse.jdt.core.dom.PostfixExpression.{Operator => PostFixOp}

/**
 * Pattern Matching for Java expressions
 */
sealed abstract class Expression {}
case class Metavar(name:String) extends Expression
case class Method(name:String,args:List[Expression]) extends Expression
case class BinOp(l:Expression,r:Expression,op:InfixOp) extends Expression
case class UnOp(expr:Expression,op:PostFixOp) extends Expression
case class Cast(expr:Expression,typee:TypePattern) extends Expression
// TODO: anonymous inner classes
// TODO: match expression
case class New(typee:TypePattern,args:List[Expression]) extends Expression
case class InstanceOf(typee:TypePattern,expr:Expression) extends Expression
case class ArrayInit(exprs:List[Expression]) extends Expression

/**
 * Note abbreviated names to avoid import conflicts with eclipse api
 */
sealed abstract class TypePattern {}
// Explicitly matches a primitve type, eg PrimType("int")
case class PrimType(name:String) extends TypePattern
// Explicitly matches an object type, eg SimpType("java.lang.Object")
case class SimpType(name:String) extends TypePattern
// Explicitly matches an array type, eg ArraType(PrimType("int"))
case class ArraType(typee:TypePattern) extends TypePattern
// Matches whatever type is bound to context["name"]
case class TypeMetavar(name:String) extends TypePattern
