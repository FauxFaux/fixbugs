package fixbugs.test;

public class Simple {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int y = 3;
		int x = y + 5;
		if(x > 5) {
			System.out.println(x);
		} else {
			System.out.println(y);
		}
	}

}
