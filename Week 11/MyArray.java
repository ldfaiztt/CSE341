// CSE341
// An array-like class (written with generics)

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.awt.Point;

public class MyArray<E> {
    /* internal array to actually hold the data */
    private E [] internalArray;

    // the constructor
    public MyArray (int n) {
	internalArray = (E[]) new Object[n];
	// unfortunately this doesn't work due to current Java limitations:
	// internalArray = new E[n];
    }

    public int length() {
	return internalArray.length;
    }

    // get an element
    public E at(int i) {
	return internalArray[i];
    }

    // set an element
    public void set(int i, E value) {
	internalArray[i] = value;
    }

    /**
     * inner class for the iterator
     */
    class MyIterator implements Iterator<E> {
	int index;
	MyIterator() {
	    index = 0;
	}
	public E next() {
	    E temp;
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
    public Iterator<E> iterator() {
	return new MyIterator();
    }

    public static void main(String[] args) {
	MyArray<Integer> a = new MyArray<Integer>(2);
	a.set(0, new Integer(50));
	a.set(1, new Integer(100));
	Iterator<Integer> it = a.iterator();
	while(it.hasNext()) {
	    System.out.println(it.next());
	}
    }
}
