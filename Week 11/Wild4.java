// CSE 341 - Java generics
// Example of casting a generic of unknown (wildcard) type to
// a known type
//
// The Java compiler will compile this program (although with a complaint
// about unsafe operations).  At runtime it will optimistically let this 
// statement be executed without raising an exception:
//        LinkedList<Integer> ilist = (LinkedList<Integer>) s;
// although in fact s isn't necessarily a list of Integers.
// However, type safety isn't lost -- the for loop will get an exception
// when it finds a non-integer in the list.

// Here is the warning the compiler gives:
// javac  -Xlint:unchecked Wild4.java
// Wild4.java:22: warning: [unchecked] unchecked cast
// found: java.util.LinkedList<capture of ?>
// required: java.util.LinkedList<java.lang.Integer>
// LinkedList<Integer> ilist = (LinkedList<Integer>) s;


import java.util.LinkedList;
import java.util.Iterator;
import java.awt.Point;

class Wild4 {

    public static void printAll(LinkedList<?> s) {
        System.out.println("entering printAll");
        LinkedList<Integer> ilist = (LinkedList<Integer>) s;
        System.out.println(
            "successfully cast LinkedList<?> to LinkedList<Integer>");
	for (Integer i : ilist) {
	    System.out.println("printing a list element");
	    System.out.println(i);
	}
	System.out.println("leavinging printAll");
    }

    public static void main(String[] args) {
	LinkedList<Integer> ilist = new LinkedList<Integer>(); 
	ilist.add(new Integer(3)); 
	ilist.add(new Integer(5)); 
	ilist.add(null);
	printAll(ilist);

	LinkedList<Point> plist = new LinkedList<Point>(); 
	plist.add(new Point(10,20)); 
	printAll(plist);
    }
}
