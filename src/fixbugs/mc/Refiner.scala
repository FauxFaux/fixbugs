package fixbugs.mc

import fixbugs.mc.ir._

/**
 * Refines and simplifies Intermediate Representation
 */
object Refiner {
  
	/**
     * refine to minimal set of temporal connectives: EG, EU, EX
     */
    def refine(nc:NodeCondition):NodeCondition = {
        nc match {
            case All(p) => innerRefine(new All(refinePath(p)))
            case Exists(p) => innerRefine(new Exists(refinePath(p)))
            case And(l,r) => new And(refine(l),refine(r))
            case Or(l,r) => new Or(refine(l),refine(r))
            case Not(phi) => new Not(refine(phi))
            case x => x
        }
    }
  
	/**
	 *  Removes Future
     */
    private def refinePath(p:Path):Path = {
        p match {
            case Future(nc) => new Until(True(),refine(nc))
            case Global(nc) => new Global(refine(nc))
            case Next(nc) => new Next(refine(nc))
            case Until(l,r) => new Until(refine(l),refine(r))
        }
    }

    /**
     * Removes AX,AG,AU
     */
    private def innerRefine(nc:NodeCondition):NodeCondition = {
        nc match {
          case All(Next(phi)) => Not(Exists(Next(Not(refine(phi)))))
          case All(Global(phi)) => Not(Exists(Until(True(),(Not(innerRefine(phi))))))
          case All(Until(phi,psi)) => {
            val rphi = innerRefine(phi)
            val rpsi = innerRefine(psi)
            Not(Or(Exists(Until(Not(rpsi),Not(Or(rphi,rpsi)))),Exists(Global(Not(rpsi)))))
          }
          case All(Future(phi)) => throw new Exception("Future should have been refined by this point")
          case Exists(Future(phi)) => throw new Exception("Future should have been refined by this point")
          case Exists(path) => new Exists(refinePath(path))
          case x => x
        }
    }
}