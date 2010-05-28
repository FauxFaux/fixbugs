
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

case class VDecl(tp:TypePattern,mv:Metavar)

sealed abstract class Transformation {}
case class Replace(from:Statement,to:Statement,cond:SideCondition) extends Transformation {
    override def toString() = "from: %s\nto: %s\ncond: %s".format(from,to,cond)
}
case class AddMethod(ret:VDecl,args:List[VDecl],stmts:List[Statement],named:Metavar,cond:SideCondition) extends Transformation
case class Then(trans:List[Transformation]) extends Transformation
case class Pick(trans:List[Transformation]) extends Transformation


