package fixbugs.core.ir

sealed abstract class Transformation {}
case class Replace(from:Statement,to:Statement,cond:SideCondition) extends Transformation
case class Then(trans:List[Transformation]) extends Transformation
case class Pick(trans:List[Transformation]) extends Transformation
