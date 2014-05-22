# Chun-Wei Chen
# CSE 341
# Assignment 7
# 11/30/12

require 'test/unit'
load 'Stack.rb'

# Unit tests for Stack
class TestStack < Test::Unit::TestCase
  # tests initialize
  def test_initialize
	n = Stack.new()
	assert_nil(n.items)
  end
  
  # tests push & pop
  def test_push_and_pop
	n = Stack.new()
	n.push(3)
	n.push(4)
	n.push(5)
	assert_equal(5, n.pop)
	assert_equal(4, n.pop)
	assert_equal(3, n.pop)
	assert_raise(RuntimeError) {n.pop}
  end
  
  # tests empty?
  def test_empty?
    n = Stack.new()
	assert_equal(true, n.empty?)
	n.push(3)
	assert_equal(false, n.empty?)
	n.push(4)
	assert_equal(false, n.empty?)
	n.pop
	assert_equal(false, n.empty?)
	n.pop
	assert_equal(true, n.empty?)
  end
end