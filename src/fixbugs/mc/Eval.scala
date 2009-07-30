class Eval(dom:ClosedDomain[Int]) {

    val domain = dom
    
    def eval(p:Path,node:Node) = {
        p match {
//            case Next(phi) => 
            case x => x
        }
    }

    def eval(nc:NodeCondition,node:Node) = {
        nc match {
            case False() => domain.none()
            case True() => domain.all()
            case NodePred(key) => domain.all().restrictByKey(key,node.lineNumber)
            case Not(phi) => eval(phi,n).negate()
            case Or(l,r) => eval(l,n) union eval(r,n)
            case And(l,r) => eval(l,n) intersect eval(r,n)
            // TODO: temporal cases
            case x => domain.none()
        }
    }

}
