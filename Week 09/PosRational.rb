# CSE 341 - a basic Ruby class
class PosRational 

  # initialize is special; immediately called on result of PosRational.new
  def initialize(num,den=1) # default arguments
    if num < 0 || den <= 0
      raise "PosRational received an inappropriate argument"
    end
    @num = num # fields created when you assign to them
    @den = den
    reduce
  end

  def to_s  # override the standard to_s (to_string) method
    ans = @num.to_s
    if @den != 1 # everything true except false _and_ nil objects
      ans = ans + "/" + @den.to_s
    end
    return ans
  end

  def to_s_variant # using things like Racket's quasiquote and unquote
    return "#{@num}#{if @den==1 then "" else "/" + @den.to_s end}"
  end

  def add r
    a = r.num # only works b/c of protected methods below
    b = r.den # only works b/c of protected methods below
    c = @num
    d = @den
    @num = (a * d) + (b * c)
    @den = b * d
    reduce
    self # convenient for stringing calls
  end

  # a functional addition, dynamic typing means now + works on rationals
  def + r
    ans = PosRational.new(@num,@den)
    ans.add(r)
    ans
  end
    
protected  
  # there is very common sugar for this (attr_reader)
  # the better way:
  # attr_reader :num, :den
  # protected :num, :den
  # there is also attr_writer, and attr_accessor (which creates both)
  def num
    @num
  end
  def den
    @den
  end

private
  def gcd(x,y)
    if x == y
      x
    elsif x < y
      gcd(x,y-x)
    else
      gcd(y,x)
    end
  end

  def reduce 
    d = gcd(@num,@den)
    @num = @num / d
    @den = @den / d
  end
end

# you can define methods outside a class; they are just part of the main
# class (which is available in the rep-loop)

def double1 x
  x + x # sugar for x.+(x)
end

def double2 x
  x * 2 # sugar for x.*(2)
end

# What type of things do our double methods take?
# Duck typing: anything that can respond to + or multiply by 2

# ok = double1(PosRational.new(1,2))
# not_ok = double2 (PosRational.new(1,2))

# we could make this work
# add more methods to PosRational (anywhere by anyone)

# class PosRational
#   def * i
#     if i != 2
#       raise "PosRational only knows how to double"
#     end
#     self.+(self) # self method call (first self necessary for syntax)
#   end
# end

#now_ok = double2(PosRational.new(1,2))

# but this still does not work
def double3 x
  2 * x # sugar for 2.*(x)
end

# double3(PosRational.new(1,2))

# another useful (?) example: + and * and - on arrays
# arrays are very common and have _lots_ of methods
