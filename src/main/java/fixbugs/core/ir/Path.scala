
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

//import fixbugs.mc.ir.NodeCondition

/**
 * Path :=  F nc
 *      |   G nc
 *      |   X nc
 *      |   nc U nc
 */
abstract class Path {}
case class Future(cond:NodeCondition) extends Path
case class Global(cond:NodeCondition) extends Path
case class Next(phi:NodeCondition) extends Path
case class Until(phi:NodeCondition,psi:NodeCondition) extends Path
