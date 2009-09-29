package fixbugs.mc.sets

import scala.collection.immutable.HashMap

class SetClosedDomain[V](all:Set[Map[String,V]]) extends ClosedDomain[V] {
    def some(values:Set[Map[String,V]]) = new SetClosedEnvironment(values,this)
    val allValues = all

    override def toString() = allValues.toString
}
