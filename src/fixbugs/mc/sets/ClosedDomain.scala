package fixbugs.mc.sets

/**
 * A factory for Environments.
 */
trait ClosedDomain[V] {
	def some(vals:Set[Map[String,V]]):ClosedEnvironment[V]
    def all() = some(allValues)
    def none() = some(Set())
    val allValues:Set[Map[String,V]]
}