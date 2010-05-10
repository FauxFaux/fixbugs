package fixbugs.mc;

import fixbugs.core.ir._
import fixbugs.mc.sets._
import org.objectweb.asm.Type
import scala.collection.mutable.{Map => MMap}
import org.slf4j.{Logger,LoggerFactory}

case class TypeEnvironment (mvNames:MMap[String,String],mvTypes:MMap[String,TypePattern],fields:Map[String,Type],vars:Map[String,Type])


/**
 * Implements the type and interface predicates
 */
class TypeComparator(env:TypeEnvironment) {

    val log = LoggerFactory getLogger(Main getClass)

    def typePred(metavar:String,pattern:TypePattern) = {
        log debug ("type(%s,%s)".format(metavar,pattern))
        val varName = env.mvNames(metavar)
        val madeType = makeType(pattern)
        env.vars(varName).equals(madeType) || env.fields(varName).equals(madeType)
    }

    def makeType(pattern:TypePattern):Type = pattern match {
        case PrimType("int") => Type.INT_TYPE
        case PrimType("long") => Type.LONG_TYPE
        case PrimType("short") => Type.SHORT_TYPE
        case PrimType("byte") => Type.BYTE_TYPE
        case PrimType("boolean") => Type.BOOLEAN_TYPE
        case PrimType(x) => throw new Error(x + "hasn't been implemented yet for type")
        case SimpType(name) => Type.getType("L" + ("name".replace(".","/")))
        case ArraType(innerType) => Type.getType("["+makeType(innerType).getDescriptor)
        case TypeMetavar(mv) => makeType(env.mvTypes(mv))
    }
    
//    def hasInterface(metavar:String,pattern:String) = {}

}

