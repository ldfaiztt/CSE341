# example of using unit tests in Ruby -- test the (improved) PosRational class

# run Ruby on this file to run all the tests:
#     ruby PosRationalTester.rb
#
# or ask it to run a particular test:
#     ruby PosRationalTester.rb --name test_raise_zero_divide


# import the unit test framework and load PosRational
# 'require' only loads the module if it isn't already loaded
require 'test/unit'
load 'PosRational.rb'

# you can also use 'require' for PosRational, but you seem to need to include
# a path to it (not sure why), e.g.
# require './PosRational'

# make TestPosRational a subclass of TestCase
class TestPosRational < Test::Unit::TestCase

  # test the to_s method.  Note that this also tests the initialize method
  # and checks that the rational number is properly reduced (since those
  # are needed to get a correct answer from to_s)
  def test_print
    r = PosRational.new(4,1)
    # assert_equal takes the expected result, then the actual result
    assert_equal("4", r.to_s)
    s = PosRational.new(6,16)
    assert_equal("3/8", s.to_s)
  end

  def test_add
    r = PosRational.new(4,1)
    s = PosRational.new(6,16)
    r.add(s)
    assert_equal("35/8", r.to_s)
  end

  def test_plus
    r = PosRational.new(4,1)
    s = PosRational.new(6,16)
    assert_equal("35/8", (r+s).to_s)
  end

  # test that an exception is raised for a negative numerator, or 0 or
  # negative denominator
  def test_initialize_exception
    assert_raise(RuntimeError) {PosRational.new(5,0)}
    assert_raise(RuntimeError) {PosRational.new(-3,2)}
  end

  # OK ... not actually testing PosRational at all -- but showing how to 
  # test for other kinds of exceptions
  def test_raise_zero_divide
    assert_raise(ZeroDivisionError) {1/0}
  end

end
