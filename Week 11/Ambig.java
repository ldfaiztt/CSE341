/* CSE 341 - demonstrate potential for ambiguity in deciding which 
   overloaded method should be used. */

public class Ambig {

    public static void test(String s, Object o) {
        System.out.println("in test with string first");
        System.out.println(s);
        System.out.println(o);
    }

    public static void test(Object o, String s) {
        System.out.println("in test with object first");
        System.out.println(s);
        System.out.println(o);
    }

    public static void main(String[] args) {
        // this method call generates a compile error:
        test("squid", "octopus");
        // however, this one works!
        // test("squid", (Object) "octopus");
    }
}

/* Another source of ambiguity is if you have two methods with the same name,
   one that takes a parameter of type A (for an interface A), and another that 
   takes a parameter of type B (for an interface B).  If you call it with an
   object of type C, where C is a class that implements both A and B, Java
   doesn't know which to use. */
