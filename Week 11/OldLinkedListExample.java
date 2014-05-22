// CSE 341
// Example of collection code written without generics.
// Make a linked list, add a couple of integers to it,
// get an iterator over the list, and get the integers out.
// Since the type of the LinkedList and of the iterator 
// doesn't use a parameterized type, we need to use a cast 
// when extracting the integers.

import java.util.LinkedList;
import java.util.Iterator;

class OldLinkedListExample {

    public static void main(String[] args) {
	LinkedList myIntList = new LinkedList(); 
	myIntList.add(new Integer(3)); 
	myIntList.add(new Integer(4)); 
	Iterator i = myIntList.iterator();
	Integer x = (Integer) i.next();  
	Integer y = (Integer) i.next();
	System.out.println(x);
	System.out.println(y);
    }
}
