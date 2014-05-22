# CSE341 Autumn 2012 - blocks, procs, inheritance, and overriding

# (1) blocks, Procs, iterators
# (2) subclassing, inheritance, overriding, dynamic dispatch

#### block, iterators, Procs ####

def play
  # many library methods "take a block", which is essentially a closure
  # avoids almost all uses of explicit loops 
  # (more concise, separates traversal from processing)
  3.times { puts "hi" }
  [4,6,8].each { puts "hi" } # can "ignore" argument
  y = 7
  [4,6,8].each { |x|
    y = y + x
    puts y
  }
  arr = [4,6,8,10]
  arr2 = arr.map { |x| x + 1 }
  puts "map result "
  puts arr2

  sum = arr.inject { |acc,elt| acc + elt }
  puts "inject result "
  puts sum

  puts (arr.any? { |elt| elt < 0 })

  # if only the immediate callee needs the block,
  # convenient to use the "yield" language feature (how the methods
  #  above are implemented)
  puts (foo { |x| x + x })

  # lambda is a built-in method that returns a Proc that is 
  # _exactly_ a closure, you call it with the call method
  cl = lambda {|z| z * y}
  q = cl.call(9)
  puts q
  
  # Proc values can be passed around, returned, stored in objects, etc.
  puts (foo2 (lambda { |x| x + x }))

  # see the block_proc.rb lecture notes for information on using
  # "& argument" so caller uses a block (not a Proc) but callee gets a Proc
  # (how lambda is implemented -- just a method of Object)
end

def foo
  eight = yield 4
  twelve = yield 6
  eight + twelve
end

def foo2 fun
  eight = fun.call 4
  twelve = bar fun
  eight + twelve
end

def bar f
  f.call 6
end

# no real reason to do this over arrays, but shows recursive blocks
# moreover, some of the methods below are really inefficient for long lists
class MyList
  def initialize(x,xs)
    @head = x
    @tail = xs
  end
  def length
    if @tail.nil?
      1
    else
      1 + @tail.length
    end
  end
  def to_a 
    if @tail.nil?
      [@head]
    else
      [@head] + @tail.to_a
    end
  end
  def self.from_a a # weird syntax for class method (Java static method)
    a.reverse.inject(nil) { |acc,elt| MyList.new(elt,acc) }
  end    
  def map # would be more convenient to add map to NilClass?
    if @tail.nil?
      MyList.new(yield(@head), nil)
    else
      MyList.new(yield(@head), @tail.map {|x| yield x})
    end
  end
  def map_p proc # would be more convenient to add map_p to NilClass?
    if @tail.nil?
      MyList.new(proc.call(@head), nil)
    else
      MyList.new(proc.call(@head), @tail.map_p(proc))
    end
  end    
end
    
#### inheritance and overriding ####
class Point
  attr_reader :x, :y
  attr_writer :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end
  def distFromOrigin
    Math.sqrt(@x * @x  + @y * @y) # why a module method? Less OO :-(
  end
  def distFromOrigin2
    Math.sqrt(x * x + y * y) # uses getter methods
  end

end

#hmm, without overriding "why not" just add a method to Point instead?
# design choice, really (but adding would affect ThreeDPoint too!)
class ColorPoint < Point
  attr_reader :color
  attr_writer :color # just our choice to make mutable like this
  # (say p.color = "green" rather than needing
  #   p.myColorSetter("green") which does @color="green" in its body)

  def initialize(x,y,c="clear") # or could skip this and color starts unset
    super(x,y)
    @color = c
  end
end

# design question: "Is a 3D-point a 2D-point?"
class ThreeDPoint < Point
  attr_reader :z
  attr_writer :z

  def initialize(x,y,z)
    super(x,y)
    @z = z
  end
  def distFromOrigin
    d = super
    Math.sqrt(d * d + @z * @z)
  end
  def distFromOrigin2
    d = super
    Math.sqrt(d * d + z * z)
  end
end

class PolarPoint < Point
  # Interesting: by not calling super constructor, no x and y field
  # In Java or Smalltalk would just have unused x and y fields
  # In any of these languages, must override distFromOrigin or it will
  # do the wrong thing, but we can leave distFromOrigin2 as inherited
  def initialize(r,theta)
    @r = r
    @theta = theta
  end
  def x
    @r * Math.cos(@theta)
  end
  def y
    @r * Math.sin(@theta)
  end
  def x= a
    b = y # avoids multiple calls to y method
    @theta = Math.atan(b / a)
    @r = Math.sqrt(a*a + b*b)
    self
  end
  def y= b
    a = y # avoid multiple calls to y method
    @theta = Math.atan(b / a)
    @r = Math.sqrt(a*a + b*b)
    self
  end
  def distFromOrigin
    @r
  end
  # distFromOrigin2 already works!!
end
