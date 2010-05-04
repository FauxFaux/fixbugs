package fixbugs

import fixbugs.core.ir.Parser.replace
import scala.io.Source
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.rewrite._
import org.eclipse.jface.text.Document
import fixbugs.core.ir.{Statement => Stmt,_}
//ASTPatternMatcher,ASTPatternGenerator,Context,Replace,Then,Transformation,SideCondition}
import fixbugs.core.ir.Parser._
import fixbugs.mc.ModelCheck
import fixbugs.mc.sets.SetClosedDomain
import scala.collection.mutable.{HashMap => HMap,HashSet => HSet,Map => MMap}
import org.slf4j.{Logger,LoggerFactory}
import org.apache.log4j.BasicConfigurator;

/**
 * Main entry point to the fixbugs system
 */
object Main {

    val log = LoggerFactory getLogger(Main getClass)

    def readFile(name:String) = Source.fromFile(name).getLines.foldLeft("")((acc,line) => acc+line)
    
    def main(args:Array[String]) = {

        // Setup log4j backend for slf4j
        BasicConfigurator.configure();
        
        // argument parsing
        if(args.length != 3) {
            log error ("should be called with three arguments:")
            log error ("java {this.getClass.getName} Classname.java Classname.class spec_file.trans")
            System.exit(-1)
        }
        val (src::bytecode::spec::Nil) = List[String]() ++ args
        log info("using source file: {}",src)
        log info("using bytecode file: {}",bytecode)
        log info("applying specification: {}",spec)

        val srcContents = readFile(src)
        val specSrc = readFile(spec)
        trans.apply(new lexical.Scanner(specSrc)) match {
            case Success(ord, _) => apply_trans(ord,srcContents,bytecode)
            case Failure(msg, _) => println("fail ",specSrc,msg)
            case Error(msg, _) => println("error ",specSrc,msg)
        }
    }

    def apply_trans(trans:Transformation,srcContents:String,bytecode:String):List[String] =
        trans match {
            case Replace(from,to,phi) => {
                // Setup the eclipse parsing framework
                val parser = ASTParser.newParser(AST.JLS3)
                parser.setSource(srcContents.toCharArray)
                val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
                val ast = cu.getAST

                // pattern match the source
                val matches = check((new ASTPatternMatcher).unifyAll(cu,from),bytecode,cu,ast,phi)
                // generate replacement programs
                    .map(con => rewrite(ast,to,con,srcContents))
                    .toList

                matches.foreach(log debug(_))
                matches
            }
            case Then(transformations) => transformations.foldLeft(List(srcContents))((acc,t) => acc.flatMap(apply_trans(t,_,bytecode)))
            case Pick(transformations) => transformations.flatMap(apply_trans(_,srcContents,bytecode))
        }

    /**
     * NB Assumes: 1 line/statement - consider how to fix
     */
    def check(contexts:Iterator[Context],className:String,cu:CompilationUnit,ast:AST, phi:SideCondition) = {
        // printf("contexts = %s",contexts.toList)
        // apply ast -> line number lookup and generate inverse
        val allValues = new HSet[(Map[String,Int],Context)]()
        contexts.foreach(con => {
            val valuation = new HMap[String,Int]()
            for((k,v) <- con.values) {
                // 
                if(v.isInstanceOf[ASTNode] && ! v.isInstanceOf[PrimitiveType]) {
                    printf("v = %s, start = %s, line = %s\n ",v,v.getStartPosition, cu.getLineNumber(v.getStartPosition))
                    valuation += (k -> cu.getLineNumber(v.getStartPosition))
                } else {
                    log debug("Non-Statement: {}", v.getClass.getName)
                }
            }
            allValues += ((Map[String,Int]() ++ valuation,con))
        })

        val domain = new SetClosedDomain(Set[Map[String,Int]]() ++ allValues.map({case (nums,nodes) => nums}))
        log debug ("domain = {}",domain)

        // do model check, we don't care about methods atm, and converts back to collections from ClosedEnvironment[Int]
        val results = ModelCheck.check(className,phi,domain).values.flatMap(_.allValues.elements).toList.removeDuplicates
        log debug ("results= {}",results)

        // convert back from line numbers to contexts
        val checked = allValues
            .filter({case (nums,nodes) => results contains nums})
            .map({case (nums,nodes) => nodes})

        checked.foreach(log debug("checked: {}",_))
        checked
    }

    /**
     * Returns generated string from replacement with eclipse IR
     */
    def rewrite(ast:AST,to:Stmt,context:Context,srcContents:String):String = {
      val from  = context("_from")
      val rewriter = ASTRewrite.create(ast)
      val gen = new ASTPatternGenerator(ast,rewriter,Map() ++context.values,true)
      val doc = new Document(srcContents)
      //println(from.getAST == ast)
      //println(gen.generate(to).getAST == ast)
      rewriter.replace(from,gen.generate(to),null)
      val edit = rewriter.rewriteAST(doc,null)
      edit.apply(doc)
      val output = doc.get
      output
    }

}
