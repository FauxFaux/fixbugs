package fixbugs.core.ir

sealed abstract class Transformation {}
case class Replace(from:SBlock,to:SBlock,cond:SideCondition) extends Transformation
case class Then(l:Transformation,r:Transformation)extends Transformation