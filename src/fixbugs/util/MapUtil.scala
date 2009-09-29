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
