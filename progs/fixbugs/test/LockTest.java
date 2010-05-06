package fixbugs.test;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class LockTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Lock l = new ReentrantLock();
		l.lock();
		String s = "inside lock";
		l.unlock();
	}

}
