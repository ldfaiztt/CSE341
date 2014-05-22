// CSE 341
// Variant on Class Wild2 that uses a type parameter rather than a wildcard.
// The Sun Java tutorial says this isn't as good style as using a wildcard -
// it's included here for comparison sake.

import java.util.LinkedList;
import java.util.Iterator;
import java.awt.Point;

class Wild2a {

    // note that we couldn't declare the parameter as LinkedList<Object> !

    public static <E> void printAll(LinkedList<E> s) {
	for (E e : s) {
	    System.out.println(e);
	}
    }

    // another version using an iterator
    public static <E> void printAll2(LinkedList<E> s) {
	Iterator<E> it = s.iterator();
	while (it.hasNext()) {
	    System.out.println(it.next());
	}
    }

    public static void main(String[] args) {
	LinkedList<Integer> ilist = new LinkedList<Integer>(); 
	ilist.add(new Integer(3)); 
	ilist.add(new Integer(5)); 
	ilist.add(null);
	printAll(ilist);
	printAll2(ilist);

	LinkedList<Point> plist = new LinkedList<Point>(); 
	plist.add(new Point(10,20)); 
	printAll(plist);
	printAll2(plist);
    }
}
