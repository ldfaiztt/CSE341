// CSE 341 - Java
// Program to illustrate covariant typing with Java arrays
// and a runtime type exception

import java.awt.Point;

class ArrayTest {
    public static void main(String[] args) {
	String[] s;
	s = new String[10];
	s[0] = "hi there";
	// test takes an array of objects.  Since Java regards an
	// array of strings as a subtype of array of object, we can
	// pass s as a parameter.
	test(s);
    }

    public static void test(Object[] a) {
	System.out.println("in test - before storing into a");
	// as far as test is concerned, 'a' is just an array
	// of objects, so the compiler will let us store a point 
	// into it.  However, this will raise a runtime exception.
	a[1] = new Point(10,20);
	System.out.println("in test - after storing into a");
    }
}
