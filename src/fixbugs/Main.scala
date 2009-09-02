package fixbugs

import fixbugs.core.ir.Parser.replace
import scala.io.Source
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.rewrite._
import org.eclipse.jface.text.Document
import fixbugs.core.ir.{Statement => Stmt,ASTPatternMatcher,ASTPatternGenerator,Context,Replace}
import fixbugs.core.ir.Parser._

/**
 * Main entry point to the fixbugs system
 * TODO: bytecode hookup
 */
object Main {

    def readFile(name:String) = Source.fromFile(name).getLines.foldLeft("")((acc,line) => acc+line)
    
    def main(args:Array[String]) = {
        // argument parsing
        if(args.length != 3) {
            println("should be called with three arguments:")
            println("java {this.getClass.getName} Classname.java Classname.class spec_file.trans")
            System.exit(-1)
        }
        val (src::bytecode::spec::Nil) = List[String]() ++ args
        //printf("src = %s, bc = %s, spec = %s\n",src,bytecode,spec)

        val parser = ASTParser.newParser(AST.JLS3)
        val srcContents = readFile(src)
        parser.setSource(srcContents.toCharArray)
        val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
        val ast =  cu.getAST
        
        //println("parsed source")

        val specSrc = readFile(spec)
        replace.apply(new lexical.Scanner(specSrc)) match {
            case Success(ord, _) => {
                val Replace(from,to,cond) = ord
                //printf("from = %s, to = %s\n",from,to)
                // pattern match the source
                check((new ASTPatternMatcher).unifyAll(cu,from))
                    // generate replacement programs
                    .map(con => rewrite(ast,to,con,srcContents))
                    .foreach(println(_))
            }
            case Failure(msg, _) => println("fail ",specSrc,msg)
            case Error(msg, _) => println("error ",specSrc,msg)
        }
        ()
    }

    def check(context:Iterator[Context]) = {
        context
    }

    /**
     * Returns generated string from replacement with eclipse IR
     */
    def rewrite(ast:AST,to:Stmt,context:Context,srcContents:String) = {
      val from  = context("_from")
      val rewriter = ASTRewrite.create(ast)
      val gen = new ASTPatternGenerator(ast,rewriter,Map() ++context.values,true)
      val doc = new Document(srcContents)
      //println(from.getAST == ast)
      //println(gen.generate(to).getAST == ast)
      rewriter.replace(from,gen.generate(to),null)
      val edit = rewriter.rewriteAST(doc,null)
      edit.apply(doc)
      doc.get
    }

}
