package fixbugs.mc

/**
 * Path :=  F nc
 *      |   G nc
 *      |   X nc
 *      |   nc U nc
 */
abstract class Path {}
case class Future(cond:NodeCondition) extends Path
case class Global(cond:NodeCondition) extends Path
case class Next(cond:NodeCondition) extends Path
case class Until(phi:NodeCondition,psi:NodeCondition) extends Path

/**
 * nc   :=  E Path
 *      |   A Path
 *      |   LineNUmber
 *      |   nc ^ nc
 *      |   nc \/ nc
 *      |   ¬ nc
 */
sealed abstract class NodeCondition {}
case class All(path:Path) extends NodeCondition
case class Exists(path:Path) extends NodeCondition
case class LineNumber(number:Int) extends NodeCondition
case class And(left:NodeCondition,right:NodeCondition) extends NodeCondition
case class Or(left:NodeCondition,right:NodeCondition) extends NodeCondition
case class Not(phi:NodeCondition) extends NodeCondition

