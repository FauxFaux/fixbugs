package fixbugs.mc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.*;
import org.objectweb.asm.commons.EmptyVisitor;

@SuppressWarnings("unchecked")
public class TypeExtractor {

    //public static Logger log = LoggerFactory.getLogger(TypeExtractor.class);

	public static Map<String, Type> lookupFieldTypes(final ClassNode cn) {
		final Map<String, Type> values = new HashMap<String, Type>();
		final List fields = cn.fields;
        //log.debug("fields = {}",fields);
		if(fields != null) {
			final Iterator fieldIt = fields.iterator();
			while(fieldIt.hasNext()) {
				final FieldNode field =  (FieldNode) fieldIt.next();
				values.put(field.name, Type.getType(field.desc));
			}
		}
		return values;
	}
	
	public static Map<String,Type> lookupVarTypes(final MethodNode mn) {
		final Map<String, Type> values = new HashMap<String, Type>();
		final List variables = mn.localVariables;
        //VariableVisitor vv = new VariableVisitor();
        //mn.accept(vv);
        //log.debug("variables = {}",variables);
		if(variables != null) {
			final Iterator varIt = variables.iterator();
			while(varIt.hasNext()) {
				final LocalVariableNode lvn = (LocalVariableNode) varIt.next();
				values.put(lvn.name, Type.getType(lvn.desc));
			}
		}
		return values;
	}
	
	public static void main(String[] args) throws Exception {
		final String className = "fixbugs.test.TestIntType";
		final ClassNode cn = new ClassNode();
		final ClassReader cr = new ClassReader(className);
		cr.accept(cn, 0);

		System.out.println(lookupFieldTypes(cn));
		final Iterator methIt = cn.methods.iterator();
		while(methIt.hasNext()) {
			final MethodNode mn = (MethodNode) methIt.next();
			System.out.println(mn.name + " - " + lookupVarTypes(mn));
		}
	}
	
}

/*
class VariableVisitor extends MethodAdapter {

//    public final List<

    public VariableVisitor() {
        super(new EmptyVisitor());
    }

    @Override
    public void visitLocalVariable(String name, String desc, String signature, Label start, Label end, int index) {
        System.out.println("visited: "+name);
    }

    public void visitMaxs(int maxStack,int maxLocals) {
        System.out.println("fFFFFFFFFFFFFFFUUUUUUUU"+maxStack+" "+maxLocals);
    }

    public void visitEnd() {
        System.out.println("vv done");
    }


}*/
