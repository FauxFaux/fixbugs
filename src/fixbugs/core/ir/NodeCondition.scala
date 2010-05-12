
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

import org.scalacheck._
import Arbitrary._
import Gen._

/**
 * nc   :=  E Path
 *      |   A Path
 *      |   nu|mu X. nc
 *      |   nc ^ nc
 *      |   nc \/ nc
 *      |   ¬ nc
 *      |   NodePred
 *      |   True
 *      |   False
 */
sealed abstract class NodeCondition {}
case class All(path:Path) extends NodeCondition
case class Exists(path:Path) extends NodeCondition
case class Mu(varName:String,phi:NodeCondition) extends NodeCondition
case class Nu(varName:String,phi:NodeCondition) extends NodeCondition
case class And(left:NodeCondition,right:NodeCondition) extends NodeCondition
case class Or(left:NodeCondition,right:NodeCondition) extends NodeCondition
case class Not(phi:NodeCondition) extends NodeCondition
//case class LineNumber(number:Int) extends NodeCondition
case class NodePred(node:String) extends NodeCondition
case class True() extends NodeCondition
case class False() extends NodeCondition
case class StmtPred(stmt:Statement) extends NodeCondition

object NodeCondition {
  //  IR Generator crap
  // TODO: refactor common code
  
  val genLeaf = oneOf(value(True),value(False),for(e <- Arbitrary.arbitrary[String]) yield NodePred(e))

  def genUnary(sz:Int) = for { phi <- genTree(sz-1) } yield Not(phi)
  
  def genAnd(sz:Int) = for {
    phi <- genTree(sz/2)
    psi <- genTree(sz/2)
  } yield And(phi,psi)
  
  def genOr(sz:Int) = for {
    phi <- genTree(sz/2)
    psi <- genTree(sz/2)
  } yield And(phi,psi)
  
  def genPath(sz:Int) = oneOf(genFuture(sz),genGlobal(sz))
  
  def genFuture(sz:Int) = for { phi <- genTree(sz-1) } yield Future(phi)
  def genGlobal(sz:Int) = for { phi <- genTree(sz-1) } yield Global(phi)
  
  def genAll(sz:Int) = for { p <- genPath(sz-1) } yield All(p)
  def genExists(sz:Int) = for { p <- genPath(sz-1) } yield Exists(p)
  
  def genTree(sz:Int): Gen[NodeCondition] =
	  if(sz <= 0) genLeaf
	  else if (sz <= 1) oneOf(genUnary(sz),genLeaf)
	  else if(sz <= 2) oneOf(genAnd(sz),genOr(sz),genUnary(sz),genLeaf)
	  else oneOf(genExists(sz),genAll(sz))
  
  implicit val arbFoo: Arbitrary[NodeCondition] = Arbitrary { Gen.sized(sz => genTree(sz)) }
}
