# Chun-Wei Chen
# CSE 341
# Assignment 7
# 11/30/12

# Implement binary trees

# defines a mixin called Equal, which contains a == method 
# that tests whether two nodes are structurally equal
module Equal
  def == arg
    if arg.class != self.class
      return false
    elsif arg.is_a? Leaf
      return self.value == arg.value
    else
      return self.value == arg.value && self.left == arg.left && self.right == arg.right
    end
  end
end

# a Leaf has one value at the node
class Leaf
  attr_reader :value
  include Enumerable, Equal
  # initializes a new Leaf
  def initialize(value)
    @value = value
  end
  
  # takes a block and invokes it with each value in the tree
  def each
    yield(@value)
  end
end

# a BinaryNode has a value at the node, and left and right child nodes
class BinaryNode
  include Enumerable, Equal
  attr_reader :value, :left, :right
  # initializes a new BinaryNode
  def initialize(value, left, right)
    @value = value
	@left = left
	@right = right
  end
  
  # takes a block and invokes it with each value in the tree
  def each(&p)
    @left.each(&p)
	yield(@value)
	@right.each(&p)
  end
end 