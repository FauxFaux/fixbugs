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
    
    def negate():ClosedEnvironment[V] = domain.all().difference(this)
    def allValues():Set[Map[String,V]]
    val domain:ClosedDomain[V]
}






