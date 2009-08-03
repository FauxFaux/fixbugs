package fixbugs.mc.sets

import scala.collection.immutable.HashMap

class SetClosedEnvironment[V](initialValues:Set[Map[String,V]],factory:SetClosedDomain[V]) extends ClosedEnvironment[V] {

    def add(value:Map[String,V]) = newWith(values + value)
    def restrictByKey(key:String,what:V) = newWith(values.filter(what == _))
    def union(other:ClosedEnvironment[V]) = newWith(other.asInstanceOf[SetClosedEnvironment[V]].values ++ values)
    def intersect(other:ClosedEnvironment[V]) = newWith(values ** other.asInstanceOf[SetClosedEnvironment[V]].values)
    def difference(other:ClosedEnvironment[V]) = newWith(values -- other.asInstanceOf[SetClosedEnvironment[V]].values)
    def equalByKey(keepKey:String,otherKey:String) = newWith(values.filter((v) => v(keepKey) == v(otherKey)))
    def mapKey[T](key:String,f:Map[V,Set[V]]) =
      newWith(values.flatMap((v) => f(v(key)).map((value) => v.update(key,value))))
    def copyKey(fromKey:String,toKey:String):ClosedEnvironment[V] =
      newWith(values.map(x => x + (toKey -> x(fromKey))))
    
    def restrictKeyTo(key:String,within:Set[V]) = newWith(values.filter(x => within contains x(key)))
    
    def copy:ClosedEnvironment[V] = newWith(values.map(x => Map() ++ x))
    
    /**
     * NB: Inefficient
     */
    def join(other:ClosedEnvironment[V],attributes:Set[String]):ClosedEnvironment[V] = {
    	val o2 = other.asInstanceOf[SetClosedEnvironment[V]]
        var temp = Set[Map[String,V]]()
        values.foreach((v) => {
            o2.values.foreach((ov) => {
                if (attributes.foldLeft(true)((acc,a) => (ov(a) == v(a)) && acc)) {
                    temp = temp + v + ov
                }
            })
        })
        newWith(temp)
    }
    
    def allValues() = values

    val domain = factory
    val values = initialValues

    def newWith(vals:Set[Map[String,V]]) = new SetClosedEnvironment(vals,domain)
    
    override def equals(o:Any) = {
      if (o.isInstanceOf[SetClosedEnvironment[V]]) {
        o.asInstanceOf[SetClosedEnvironment[V]].values.equals(values)
      } else {
        false
      }
    }
    
    
    override def hashCode = values.hashCode

}