
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

package fixbugs.mc.sets;

/**
 *  Represents a set of possible valuations over a closed domain
 *  Possibly Mutable
 */
trait ClosedEnvironment[V] {
    def add(value:Map[String,V]):ClosedEnvironment[V]
    def restrictByKey(key:String,value:V):ClosedEnvironment[V]
    def union(other:ClosedEnvironment[V]):ClosedEnvironment[V]
    def intersect(other:ClosedEnvironment[V]):ClosedEnvironment[V]
    def difference(other:ClosedEnvironment[V]):ClosedEnvironment[V]
    def join(other:ClosedEnvironment[V],attributes:Set[String]):ClosedEnvironment[V]
    def equalByKey(keepKey:String,otherKey:String):ClosedEnvironment[V]
    def copyKey(fromKey:String,toKey:String):ClosedEnvironment[V]
    def restrictKeyTo(key:String,values:Set[V]):ClosedEnvironment[V]
    def mapKey[T](key:String,f:Map[V,Set[V]]):ClosedEnvironment[V]
    def copy:ClosedEnvironment[V]
    def filter(f:((Map[String,V])=>Boolean)):ClosedEnvironment[V]
    
    def negate():ClosedEnvironment[V] = domain.all().difference(this)
    def allValues():Set[Map[String,V]]
    val domain:ClosedDomain[V]
}






