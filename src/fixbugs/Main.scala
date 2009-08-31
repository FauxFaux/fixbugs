package fixbugs

/**
 * Main entry point to the fixbugs system
 */
object Main {
    
    def main(args:Array[String]) = {
        if(args.length != 3) {
            println("should be called with two arguments:")
            println("java {this.getClass.getName} Classname.java Classname.class")
            System.exit(-1)
        }

        val (src::bytecode::spec::Nil) = List() ++ args
        
        ()
    }

}
