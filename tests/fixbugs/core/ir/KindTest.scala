package fixbugs.core.ir

import org.scalatest._
import matchers._

class KindTest extends Spec with ShouldMatchers {

    def hook(stmt:Statement):Unit = hook(stmt,Skip())
    def hook(from:Statement,to:Statement):Unit = hook(Replace(from,to,STrue()))
    def hook(t:Transformation):Unit =
        try {
            KindChecker.check(t)
        } catch {
            case KindCheckingException(msg) => println(msg)
        }

    describe("Simple") {
        hook(new Label("x",new SideEffectExpr(new Metavar("x"))))
        hook(new SideEffectExpr(new Metavar("_from")))
        hook(new SideEffectExpr(new Metavar("x")),new SideEffectExpr(new Metavar("y")))
    }

}
