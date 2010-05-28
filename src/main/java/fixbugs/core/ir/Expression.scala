
/**
 *
 * This file is part of Fixbugs.
 * 
 * Fixbugs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Fixbugs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fixbugs.  If not, see <http://www.gnu.org/licenses/>.
 *
 **/

package fixbugs.core.ir

import org.eclipse.jdt.core.dom.InfixExpression.{Operator => InfixOp}
import org.eclipse.jdt.core.dom.PostfixExpression.{Operator => PostFixOp}

/**
 * Pattern Matching for Java expressions
 */
sealed abstract class Expression {}
//case class Anything() extends Expression
case class Metavar(name:String) extends Expression
case class JavaLiteral(value:String) extends Expression
case class Method(expr:Expression,name:String,args:List[Expression]) extends Expression
case class NamedMethod(expr:Expression,name:String,args:List[Expression]) extends Expression
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

