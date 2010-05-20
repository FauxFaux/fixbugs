
/**
 *
 * This file is part of Fixbugs.
 * 
 * Fixbugs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Fixbugs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fixbugs.  If not, see <http://www.gnu.org/licenses/>.
 *
 **/

package fixbugs.mc;

import fixbugs.core.ir._
import fixbugs.mc.sets._
import org.objectweb.asm.Type
import org.eclipse.jdt.core.dom.{Type => EclipseType,PrimitiveType,ArrayType,SimpleType,QualifiedType,Name}
//import scala.collection.mutable.{Map => MMap}
import org.slf4j.{Logger,LoggerFactory}

/**
 * Holds information about field and local variable types for a given method
 * This is extracted from the bytecode
 */
case class TypeEnvironment (fields:Map[String,Type],vars:Map[String,Type])

/**
 * Implements the type and interface predicates
 * Finds satisfying bindings from a given Binding
 * Note - 1 instance per method
 */
class TypeComparator(typeEnv:TypeEnvironment) {

    val log = LoggerFactory getLogger(this getClass)

    def typePred(metavar:String,pattern:TypePattern,env:Map[String,Any]) = {
        log debug ("type(%s,%s)".format(metavar,pattern))
        val mapVal = env(metavar)
        //log debug (mapVal.asInstanceOf[Object].getClass.getName)
        if(mapVal.isInstanceOf[Name]) {
            val varName = mapVal.asInstanceOf[Name].getFullyQualifiedName
            makeType(pattern,env) match {
                case Some(madeType) => {
                    if (typeEnv.vars.contains(varName)) {
                        log debug ("local variable: {}",varName)
                        typeEnv.vars(varName).equals(madeType)
                    }
                    else typeEnv.fields.contains(varName) && typeEnv.fields(varName).equals(madeType)
                }
                case None => {
                    log debug ("\tImpossible to construct pattern: ",pattern)
                    false
                }
            }
        } else {
            log debug ("\tincorrect metavar type for {} - {}",metavar,mapVal.asInstanceOf[Object].getClass.getName)
            false
        }
    }

    /**
     * Builds instances of an asm type from a TypePattern and environment
     */
    def makeType(pattern:TypePattern,env:Map[String,Any]):Option[Type] = pattern match {
        case PrimType("int") => Some(Type.INT_TYPE)
        case PrimType("long") => Some(Type.LONG_TYPE)
        case PrimType("short") => Some(Type.SHORT_TYPE)
        case PrimType("byte") => Some(Type.BYTE_TYPE)
        case PrimType("boolean") => Some(Type.BOOLEAN_TYPE)
        case PrimType(x) => throw new Error(x + "hasn't been implemented yet for type")

        case SimpType(name) => Some(Type.getType(toDescriptor(name)))
        case ArraType(innerType) => makeType(innerType,env).map(toArray(_))
        case TypeMetavar(mv) => {
            val maybeType = env(mv)
            if (maybeType.isInstanceOf[EclipseType])
                Some(convertType(maybeType.asInstanceOf[EclipseType]))
            else
                None
        }
    }

    /**
     * Converts from eclipse types to ASM Types
     */
    def convertType(eclipse:EclipseType):Type = eclipse match {
        case e:PrimitiveType => e.getPrimitiveTypeCode match {
            case PrimitiveType.BOOLEAN => Type.BOOLEAN_TYPE
            case PrimitiveType.BYTE => Type.BYTE_TYPE
            case PrimitiveType.CHAR => Type.CHAR_TYPE
            case PrimitiveType.DOUBLE => Type.DOUBLE_TYPE
            case PrimitiveType.FLOAT => Type.FLOAT_TYPE
            case PrimitiveType.INT => Type.INT_TYPE
            case PrimitiveType.LONG => Type.LONG_TYPE
            case PrimitiveType.SHORT => Type.SHORT_TYPE
            case PrimitiveType.VOID => Type.VOID_TYPE
        }
        case e:SimpleType => Type.getType(toDescriptor(e.toString))
        case e:QualifiedType => Type.getType(toDescriptor(e.toString))
        case e:ArrayType => toArray(convertType(e.getComponentType))
    }
    
    // Utility Methods

    def toDescriptor(className:String):String = "L" + className.replace(".","/") + ";"
    def toArray(t:Type):Type = Type.getType("["+t.getDescriptor)
    
//    def hasInterface(metavar:String,pattern:String) = {}

}

