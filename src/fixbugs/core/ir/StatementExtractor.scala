package fixbugs.core.ir

import fixbugs.core.ir._
import fixbugs.mc.sets._
import scala.collection.mutable.{Map => MMap}
import org.slf4j.{Logger,LoggerFactory}
import org.objectweb.asm.tree.MethodNode

object StatementExtractor {

    val log = LoggerFactory getLogger(this getClass)

    def comb2[T](l:(List[Statement],T),r:(List[Statement],T),f:((T,T)=>T)):(List[Statement],T) =
        ((l._1 ++ r._1).toList,f(l._2,r._2))

    def comb1[T,R](phi:(List[Statement],T),f:((T) => R)):(List[Statement],R) =
        (phi._1,f(phi._2))

    var index = -1

    def get(sc:SideCondition):(List[Statement],SideCondition) = sc match {
        case SAnd(l,r) => comb2[SideCondition](get(l),get(r),{SAnd(_,_)})
        case SOr(l,r) => comb2[SideCondition](get(l),get(r),{SOr(_,_)})
        case SNot(phi) => comb1[SideCondition,SideCondition](get(phi),{SNot(_)})
        case Satisfies(name,phi) => comb1[NodeCondition,SideCondition](get(phi),{Satisfies(name,_)})
        case x => (Nil,x)
    }
    
    def get(nc:NodeCondition):(List[Statement],NodeCondition) = nc match {
        case Not(phi) => comb1[NodeCondition,NodeCondition](get(phi),{Not(_)})
        case Or(l,r) => comb2[NodeCondition](get(l),get(r),{Or(_,_)})
        case And(l,r) => comb2[NodeCondition](get(l),get(r),{And(_,_)})
        case Exists(phi) => comb1[Path,NodeCondition](get(phi),{Exists(_)})
        case All(phi) => comb1[Path,NodeCondition](get(phi),{All(_)})
        case StmtPred(stmt) =>{
            index += 1
            val name = "stmt"+index
            (List(Label(name,stmt)),NodePred(name))
        }
        case x => (Nil,x)
    }

    def get(pc:Path):(List[Statement],Path) = pc match {
        case Future(phi) => comb1[NodeCondition,Path](get(phi),{Future(_)})
        case Global(phi) => comb1[NodeCondition,Path](get(phi),{Global(_)})
        case Next(phi) => comb1[NodeCondition,Path](get(phi),{Next(_)})
        case Until(phi,psi) => {
            val (x,y) = (get(phi),get(psi))
            (x._1 ++ y._1,Until(x._2,y._2))
        }
    }
}
