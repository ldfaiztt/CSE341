// CSE 341
// Variant on Class Wild3 that uses a bounded type parameter rather than 
// a wildcard.
// As with Wild2a, the Sun Java tutorial says this isn't as good style as 
// using a wildcard - it's included here for comparison sake.

import java.util.LinkedList;
import java.util.Iterator;
import java.awt.Point;
import java.awt.geom.RectangularShape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Ellipse2D;


class Wild3a {

    // version 1 of maxWidth -- note that we need a cast to get 
    // rectangular shapes out of the list
    public static <E> double maxWidth1 (LinkedList<E> s) {
	double m = 0.0;
	for (E e : s) {
	    RectangularShape r = (RectangularShape) e;
	    if (r.getWidth() > m) {
		m = r.getWidth();
	    }
	}
	return m;
    }

    // version 2 of maxWidth, using a bounded wildcard
    public static <E extends RectangularShape> double maxWidth2 (LinkedList<E> s) {
	double m = 0.0;
	for (RectangularShape e : s) {
	    // no cast needed!
	    RectangularShape r = e;
	    if (r.getWidth() > m) {
		m = r.getWidth();
	    }
	}
	return m;
    }

    public static void main(String[] args) {
	// demonstrate the use of the maxWidth method by calling it
	// with lists of different kinds of rectangular shapes
	// first make a linked list of rectangles
	LinkedList<Rectangle2D> rlist = 
	    new LinkedList<Rectangle2D>(); 
	rlist.add(new Rectangle2D.Double(0.0, 0.0, 50.0, 100.0)); 
	rlist.add(new Rectangle2D.Double(0.0, 0.0, 12.0, 18.0)); 
	double ans1 = maxWidth1(rlist);
	System.out.println(ans1);	
	double ans2 = maxWidth2(rlist);
	System.out.println(ans2);	
	// now do the same thing, but with ellipses
	LinkedList<Ellipse2D> elist = 
	    new LinkedList<Ellipse2D>(); 
	elist.add(new Ellipse2D.Double(0.0, 0.0, 20.0, 30.0));
	elist.add(new Ellipse2D.Double(0.0, 0.0, 24.0, 30.0));
	double ans1e = maxWidth1(elist);
	System.out.println(ans1e);	
	double ans2e = maxWidth2(elist);
	System.out.println(ans2e);	


    }
}
