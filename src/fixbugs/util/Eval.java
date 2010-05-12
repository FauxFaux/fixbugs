
/** @author RLMW */
package fixbugs.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject.Kind;

/**
 * Abstracts the Java Compiler API 
 * @author richard
 */
public class Eval {
	
	private static String tmpDir = System.getProperty("java.io.tmpdir");
	static String newline = System.getProperty("line.separator");

    private static HashSet<Kind> classFiles = new HashSet<Kind>(Arrays.asList(Kind.CLASS));

	public static void setTmpDir(String tmpDir) {
		Eval.tmpDir = tmpDir;
	}
	
	public static void main(String[]args) throws Exception {
    	final String test = "public class $100 {public static void main(String[] args) {System.out.println(\"hello world\");}}";
    	//System.out.println(compile("$100", test));
    	System.out.println(compile("$100", test));
    }

	/**
	 * Compile a string
	 * @return A list of generated classes
	 */
	public static List<String> compile(final String name, final String src) throws Exception {
		final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		final PipedReader pipe = new PipedReader();
		final BufferedReader in = new BufferedReader(pipe);
		final PipedWriter out = new PipedWriter(pipe);
        final StandardJavaFileManager man = compiler.getStandardFileManager(null,null,null);
        final InfoFileManager ifm = new InfoFileManager(man);

		try {
			final CompilationTask task = compiler.getTask(
					out, ifm,
					null, Arrays.asList("-g", "-d",tmpDir), null,
					Arrays.asList(new JavaSourceFromString(name,src)));
			
			if (task.call()) {
				return ifm.getFiles();
			} else {
				out.close();
				final List<String> strings = new ArrayList<String>();
				String s;
				while((s=in.readLine())!=null) {
					strings.add(s);
				}
				throw new CompileException(strings);
			}
		} finally {
			ifm.close();
		}
	}
	
}

class InfoFileManager extends ForwardingJavaFileManager<StandardJavaFileManager> {

	private final List<String> files;
	
	public List<String> getFiles() {
		return files;
	}

	@Override
	public FileObject getFileForOutput(Location location, String packageName,
			String relativeName, FileObject sibling) throws IOException {
		FileObject fileForOutput = super.getFileForOutput(location, packageName, relativeName, sibling);
		files.add(fileForOutput.toString());
		return fileForOutput;
	}

	@Override
	public JavaFileObject getJavaFileForOutput(Location location,
			String className, Kind kind, FileObject sibling) throws IOException {
		JavaFileObject javaFileForOutput = super.getJavaFileForOutput(location, className, kind, sibling);
		files.add(javaFileForOutput.toString());
		return javaFileForOutput;
	}

	protected InfoFileManager(StandardJavaFileManager man) {
		super(man);
		this.files = new ArrayList<String>();
	}
}

class CompileException extends Exception {
	
	private static final long serialVersionUID = 1L;
	private final List<String> errors;
	
	public List<String> getErrors() {
		return errors;
	}

	public CompileException(List<String> errors) {
		this.errors = errors;
	}
	
	@Override
	public String getMessage() {
		final StringBuilder sb = new StringBuilder();
		for (String s : errors) {
			sb.append(s);
			sb.append(Eval.newline);
		}
		return sb.toString();
	}
}

/**
 * A file object used to represent source coming from a string.
 */
class JavaSourceFromString extends SimpleJavaFileObject {
	/**
	 * The source code of this "file".
	 */
	final String code;

	/**
	 * Constructs a new JavaSourceFromString.
	 * 
	 * @param name
	 *            the name of the compilation unit represented by this file
	 *            object
	 * @param code
	 *            the source code for the compilation unit represented by this
	 *            file object
	 */
	JavaSourceFromString(String name, String code) {
		super(URI.create("string:///" + name.replace('.', '/')
				+ Kind.SOURCE.extension), Kind.SOURCE);
		this.code = code;
	}

	@Override
	public CharSequence getCharContent(boolean ignoreEncodingErrors) {
		return code;
	}
}
