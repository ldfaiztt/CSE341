// CSE 341
// An array-like class (written without generics)

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.awt.Point;

public class OldMyArray {
    /* internal array to actually hold the data */
    private Object[] internalArray;

    // the constructor
    public OldMyArray(int n) {
	internalArray = new Object[n];
    }

    public int length() {
	return internalArray.length;
    }

    // get an element
    public Object at(int i) {
	return internalArray[i];
    }

    // set an element
    public void set(int i, Object value) {
	internalArray[i] = value;
    }

    /**
     * inner class for the iterator
     */
    class MyIterator implements Iterator {
	int index;
	MyIterator() {
	    index = 0;
	}
	public Object next() {
	    Object temp;
	    if (!hasNext())
		throw new NoSuchElementException("no next element available");
	    temp = internalArray[index];
	    index++;
	    return temp;
	}
	public boolean hasNext() {
	    return index < internalArray.length;
	}
	public void remove() {
	    /* this is an optional operation - we don't support it */
	    throw new UnsupportedOperationException("remove operation not supported");
	}
    }
    
    /**
     * return an iterator for this array
     */
    public Iterator iterator() {
	return new MyIterator();
    }

    public static void main(String[] args) {
	OldMyArray a = new OldMyArray(4);
	a.set(0, new Integer(50));
	a.set(1, new Integer(100));
	a.set(2, new Point(10,20));
	a.set(3, "hi there");
	Iterator it = a.iterator();
	while(it.hasNext()) {
	    System.out.println(it.next());
	}
    }
}
