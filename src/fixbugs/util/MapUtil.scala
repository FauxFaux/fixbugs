
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

package fixbugs.util

import scala.collection.mutable.{Map => MMap,HashMap => HMMap}

object MapUtil {

  def reverse[A,B](m:MMap[A,Set[B]]):MMap[B,Set[A]] = m.foldLeft(new HMMap[B,Set[A]]())((acc,kv) => {
    val (k,vals) = kv
    vals.foreach(v => {
      val s = acc.getOrElse(v,Set[A]())
      acc += v -> (s + k)
    })
    acc
  })
  
  def crossWith[K,V](env:Set[Map[K,V]],key:K, values:Set[V]):Set[Map[K,V]] =
    env.flatMap(x => values.map(v => x + (key -> v)))
  
//  def productWith[K,V](env:Map[K,V],key:K):Map[K,V] = env + env.values.map(v => 
    
}
