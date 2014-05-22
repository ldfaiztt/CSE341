# Chun-Wei Chen
# CSE 341
# Assignment 7
# 11/30/12

require 'test/unit'
load 'assign7.rb'

# Unit tests for Leaf and BinarNode
class TestBinaryTree < Test::Unit::TestCase
  # tests ==
  def test_equal
    n = Leaf.new(1)
	m = Leaf.new(3)
	o = BinaryNode.new(2, n, m)
	p = Leaf.new(5)
	q = BinaryNode.new(4, o, p)
	assert_equal(true, n == Leaf.new(1))
	assert_equal(false, n == m)
	assert_equal(false, n == 1)
	assert_equal(true, o == BinaryNode.new(2, Leaf.new(1), Leaf.new(3)))
	assert_equal(false, o == BinaryNode.new(2, n, n))
	assert_equal(false, o == 2)
	assert_equal(true, q == BinaryNode.new(4, BinaryNode.new(2, Leaf.new(1), Leaf.new(3)), Leaf.new(5)))
	assert_equal(false, q == BinaryNode.new(4, o, Leaf.new(6)))
  end
  
  # tests each
  def test_each
    n = Leaf.new(1)
	m = Leaf.new(3)
	o = BinaryNode.new(2, n, m)
	p = Leaf.new(5)
	q = BinaryNode.new(4, o, p)
	assert_equal([1], n.map {|x| x})
	assert_equal(true, n.map {|x| x} == Leaf.new(1).map {|x| x})
	assert_equal(false, n.map {|x| x} == m.map {|x| x})
	assert_equal([1, 2, 3], o.map {|x| x})
	assert_equal(true, o.map {|x| x} == BinaryNode.new(2, Leaf.new(1), Leaf.new(3)).map {|x| x})
	assert_equal(false, o.map {|x| x} == BinaryNode.new(2, m, n).map {|x| x})
	assert_equal(false, [1, 2] == o.map {|x| x})
	assert_equal(false, [2, 1, 3]  == o.map {|x| x})
	assert_equal(false, [1, 3, 2]  == o.map {|x| x})
	assert_equal([1, 2, 3, 4, 5], q.map {|x| x})
	assert_equal(false, [1, 2, 3, 4] == q.map {|x| x})
	assert_equal(false, [1, 3, 2, 5, 4] == q.map {|x| x})
	assert_equal(false, [4, 2, 1, 3, 5] == q.map {|x| x})
	assert_equal(false, q.map {|x| x} == o.map {|x| x})
  end
  
  # tests min
  def test_min
    n = Leaf.new(1)
	m = Leaf.new(3)
	o = BinaryNode.new(2, n, m)
	p = Leaf.new(5)
	q = BinaryNode.new(4, o, p)
	assert_equal(1, n.min)
	assert_equal(3, m.min)
	assert_equal(false, n.min == 2)
	assert_equal(1, o.min)
	assert_equal(1, BinaryNode.new(2, m, n).min)
	assert_equal(2, BinaryNode.new(2, m, m).min)
	assert_equal(1, q.min)
	assert_equal(1, BinaryNode.new(4, p, o).min)
	assert_equal(4, BinaryNode.new(4, p, p).min)
  end	
end