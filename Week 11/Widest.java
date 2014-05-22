// CSE 341
// Example of a generic method

import java.util.LinkedList;
import java.util.Iterator;
import java.awt.Point;
import java.awt.geom.RectangularShape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Ellipse2D;


class Widest {

    // First a non-generic version - return the widest shape from a
    // list of rectangular shapes.  Note that we only know that the result 
    // is a rectangular shape.  Also, we can't call this method with e.g.
    // a list of ellipses.
    public static RectangularShape widest_rect (LinkedList<RectangularShape> s) {
        RectangularShape widest = null;
	double max_so_far = 0.0;
	for (RectangularShape r : s) {
	    if (r.getWidth() > max_so_far) {
		max_so_far = r.getWidth();
                widest = r;
	    }
	}
	return widest;
    }

    // Generic version - return the widest shape from a list containing
    // some subtype of rectangular shape
    public static <E extends RectangularShape> E widest (LinkedList<E> s) {
        E widest = null;
	double max_so_far = 0.0;
	for (E r : s) {
	    if (r.getWidth() > max_so_far) {
		max_so_far = r.getWidth();
                widest = r;
	    }
	}
	return widest;
    }

    public static void main(String[] args) {
	// demonstrate the use of the widest method by calling it
	// with lists of different kinds of rectangular shapes
	// first make a linked list of rectangles
	LinkedList<Rectangle2D> rlist = new LinkedList<Rectangle2D>(); 
	rlist.add(new Rectangle2D.Double(0.0, 0.0, 50.0, 100.0)); 
	rlist.add(new Rectangle2D.Double(0.0, 0.0, 12.0, 18.0)); 
	Rectangle2D ans1 = widest(rlist);
	System.out.println(ans1.getWidth());	
	// now do the same thing, but with ellipses
	LinkedList<Ellipse2D> elist = new LinkedList<Ellipse2D>(); 
	elist.add(new Ellipse2D.Double(0.0, 0.0, 20.0, 30.0));
	elist.add(new Ellipse2D.Double(0.0, 0.0, 24.0, 30.0));
	Ellipse2D ans2 = widest(elist);
	System.out.println(ans2.getWidth());
    }
}
