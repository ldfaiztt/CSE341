# CSE341, Autumn 2012
# Example of using coerce to convert to a more general class when
# implementing methods for binary operations

# Suppose we are implementing complex numbers.  (There's a built-in
# class Complex, so for illustration let's make a new class
# MyComplex.)  If c is a complex number, it's easy to see how to
# implement a method for c+3.14 ... but what about 3.14+c?  We almost
# certainly don't want to tinker with the + method for Float.  Ruby's
# solution is to turn the problem back to the complex number, using
# the 'coerce' method.  'coerce' should return an array consisting of
# 3.14 coerced into a more suitable class (in this case also a complex
# number), and c.  Then we re-try the addition message.


class MyComplex

  attr_reader :r, :i

  def initialize(r,i)
    @r = r
    @i = i
  end

  def + (c1)
    c2 = c1.asComplex
    return MyComplex.new(r+c2.r, i+c2.i)
  end

  def * (c1)
    c2 = c1.asComplex
    return MyComplex.new(r*c2.r - i*c2.i, r*c2.i + i*c2.r)
  end

  def coerce(n)
    # coerce gets called when we try to add or multiply a number and
    # a complex
    return [n.asComplex, self]
  end

  def asComplex
    return self
  end
end

class Numeric
  def asComplex
    return MyComplex.new(self,0)
  end
end


# Rather than adding a asComplex number to Numeric (the superclass of
# integers, floats, etc), we could have defined + for MyComplex like
# this.  (:i is a Symbol)
#
#   def + (n)
#     if n.respond_to?(:i)
#       c = n
#     else
#       c = MyComplex.new(n,0)
#     return MyComplex.new(r+c.r, i+c.i)
#   end
# If you do that, change the coerce method as well to avoid using asComplex.

# Examples to try:
# c = MyComplex.new(2,5)
# c+3
# 3+c
# 5*c
# c*5
