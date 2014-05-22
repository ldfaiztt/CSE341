# CSE341 -  mixins

# mixins provide a convenient way to define some functionality --
# outside of a class -- that can be 'mixed in' to any class.  Ruby
# only has single inheritance (at most one superclass per class), but
# you can mix in as many mixins as you want.

# let's define a mixin called Doubler
module Doubler
  def double
    self + self # uses self's + message, not defined in Doubler
  end
end

class String
  include Doubler
end

# now try "octopus ".double

# mix it into another class
class Array
  include Doubler
end

# we could also add it to all numeric classes (integers, floats etc):
# class Numeric
#   include Doubler
# end

# The Comparable and Enumerable mixins are probably the most commonly
# used ones in the Ruby library

# For Comparable you define <=>,
# and you get ==, >, <, >=, <= from the mixin
# (overrides Object's ==, adds the others)
class Name
  attr_accessor :first, :middle, :last
  include Comparable
  def initialize(first,last,middle="")
    @first = first
    @last = last
    @middle = middle
  end
  def <=> other
    l = @last <=> other.last # <=> defined on strings
    return l if l != 0
    f = @first <=> other.first
    return f if f != 0
    @middle <=> other.middle
  end
end

# For Enumerable you define each, and you get map, any?, etc.  
# If the objects in the collection implement a meaningful <=>
# operator, you also get max, min, and sort.
# (This will be true for MyRange if you give it numbers for low and high.)

# Note that map returns an array though.
# (There is a builtin class Range so let's not mess that up.)
class MyRange
  include Enumerable
  def initialize(low,high)
    @low = low
    @high = high
  end
  def each
    i=@low
    while i <= @high
      yield i
      i=i+1
    end
  end
end

# here is how module Enumerable could implement map:
# def map
#   arr = []
#   each {|x| arr.push x }
#   arr
# end

