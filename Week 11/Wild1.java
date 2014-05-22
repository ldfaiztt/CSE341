// CSE 341
// Simple example of using a wildcard.
// Make a linked list of ?
// The only value we can add to it is null (since this is a legal value
// of any type).  We can get an iterator for the linked list, but we
// don't know what kind of types it produces.  (But they must be objects.)

import java.util.LinkedList;
import java.util.Iterator;

class Wild1 {

    public static void main(String[] args) {
	LinkedList<?> s = new LinkedList<Integer>(); 
	s.add(null);
	Iterator<?> x = s.iterator();
	Object o = x.next();
	System.out.println(o);

	// note that LinkedList<?> is NOT the same type as LinkedList<Object>
	// here are some statements that wouldn't compile:
	// s.add(new Integer(3)); 
	// Iterator<Object> x = s.iterator();
    }
}
