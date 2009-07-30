import scala.collection.mutable.HashMap

/**
 *  Represents a set of possible valuations over a closed domain
 *  Possibly Mutable
 */
trait ClosedEnvironment[V] {
    def add(value:HashMap[String,V]):ClosedEnvironment[V]
    def removeByKey(key:String,values:Set[V]):ClosedEnvironment[V]
    def union(other:ClosedEnvironment[V]):ClosedEnvironment[V]
    def intersect(other:ClosedEnvironment[V]):ClosedEnvironment[V]
    def join(other:ClosedEnvironment[V],attributes:Set[String]):ClosedEnvironment[V]

    def negate():ClosedEnvironment[V] = domain.all().intersect(this)
    val domain:ClosedDomain[V]
}

/**
 * A factory for Environments.
 */
trait ClosedDomain[V] {
    def all() = some(allValues)
    def none() = some(Set())
    def some(Set[HashMap[String,V]]):ClosedEnvironment[V]
    private val allValues:Set[HashMap[String,V]]
}

class SetClosedDomain[V](all:Set[HashMap[String,V]]) extends ClosedDomain[V] {
    def some(values:Set[HashMap[String,V]]) = SetClosedEnvionrment(values,this)
    private val allValues = all
}

class SetClosedEnvionrment[V](initialValues:Set[HashMap[String,V]],factory:SetClosedDomain[V]) extends ClosedEnvironment[V] {

    def add(value) = newWith(values + value)
    def removeByKey(key,what) = newWith(values.filter(what.contains(_)))
    def union(other) = newWith(other.values ++ values)
    def intersect(other) = newWith(values ** other.values)
    /**
     * NB: Inefficient
     */
    def join(other,attributes) = {
        var temp = Set()
        values.foreach((v) => {
            other.values.foreach((ov) => {
                if (attributes.foldLeft(true,(a,acc) => ov[a] == v[a] && acc))
                    temp = tempt + ov + v
            })
        })
    }

    val domain = factory
    val values = initialValues

    def newWith(vals) = SetClosedEnvionrment(vals,domain)

}
