package fixbugs.test;

public class Simple {
	
	//private static final long TestField = 0;

	public static void main(String[] args) {
        if(true) {
            System.out.println("Foo");
        } else {
            System.out.println("bar");
        }
        int x = 2;
		int y = x + 3;
        int z = 4;
		for(int i = 0;i<10;i++) {
			System.out.println(i);
		}
		try {
            System.out.println("Baz");
		} catch (Exception e) {
			e.printStackTrace();
		}
		long finalVar = 2l;
	}
	
}
