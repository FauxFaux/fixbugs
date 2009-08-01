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
    def join(other:ClosedEnvironment[V],attributes:Set[String]):ClosedEnvironment[V]
    def equalByKey(keepKey:String,otherKey:String):ClosedEnvironment[V]
    def mapKey[T](key:String,f:Map[V,Set[V]]):ClosedEnvironment[V]

    def negate():ClosedEnvironment[V] = domain.all().intersect(this)
    val domain:ClosedDomain[V]
}






