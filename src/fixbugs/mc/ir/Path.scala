package fixbugs.mc.ir

//import fixbugs.mc.ir.NodeCondition

/**
 * Path :=  F nc
 *      |   G nc
 *      |   X nc
 *      |   nc U nc
 */
abstract class Path {}
case class Future(cond:NodeCondition) extends Path
case class Global(cond:NodeCondition) extends Path
case class Next(phi:NodeCondition) extends Path
case class Until(phi:NodeCondition,psi:NodeCondition) extends Path