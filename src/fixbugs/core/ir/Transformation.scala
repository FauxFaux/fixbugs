package fixbugs.core.ir

sealed abstract class Transformation {}
case class Replace(from:SBlock,to:SBlock,cond:SideCondition)
case class Then(l:Transformation,r:Transformation)