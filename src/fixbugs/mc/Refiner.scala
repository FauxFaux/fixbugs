object Refiner {
    def refine(p:Path) = {
        p match {
            case Future(nc) => Until(True(),refine(nc))
            case Global(nc) => Until(refine(nc),True())
            case Next(nc) => Next(refine(nc))
            case Until(l,r) => Until(refine(l),refine(r))
        }
    }

    def refine(nc:NodeCondition) = {
        nc match {
            case All(p) => All(refine(p))
            case Exists(p) => Exists(refine(p))
            case And(l,r) => And(refine(l),refine(r))
            case Or(l,r) => Or(refine(l),refine(r))
            case Not(phi) => Not(refine(phi))
            case x => x
        }
    }
}

