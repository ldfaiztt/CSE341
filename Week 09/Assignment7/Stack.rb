# Chun-Wei Chen
# CSE 341
# Assignment 7
# 11/30/12

# Link stores the item of Stack
class Link
  attr_reader :value
  attr_accessor :next
  # initialize a new Link
  def initialize(value)
    @value = value
    @next = nil
  end
end

# The Stack class represents a last-in-first-out (LIFO) stack of objects
class Stack
  attr_reader :items
  # initializes a new Stack
  def initialize()
    @items = nil
  end
  
  # pushes an object onto the top of the stack and returns the stack itself
  def push arg
    if @items.nil?
	  @items = Link.new(arg)
	else
	  n = Link.new(arg)
	  n.next = @items
	  @items = n
	end
	return @items
  end
  
  # removes the object at the top of the stack and returns the object
  def pop
    if @items.nil?
	  raise "Empty Stack -- nothing can be poped"
    end
	pt = @items
	@items = @items.next
	pt.next = nil
	return pt.value
  end
  
  # tests if this stack is empty
  def empty?
    return @items.nil?
  end
end