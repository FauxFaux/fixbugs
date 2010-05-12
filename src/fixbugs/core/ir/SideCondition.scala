
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

sealed abstract class SideCondition {}
case class Satisfies(name:String,cond:NodeCondition) extends SideCondition
case class SAnd(left:SideCondition,right:SideCondition) extends SideCondition
case class SOr(left:SideCondition,right:SideCondition) extends SideCondition
case class SNot(phi:SideCondition) extends SideCondition
case class STrue() extends SideCondition
case class SFalse() extends SideCondition
case class TypePred(name:String,pattern:TypePattern) extends SideCondition
case class MethodPred(name:String) extends SideCondition
//case class Interface(name:String) extends SideCondition

