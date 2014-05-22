# CSE341 -- converting between blocks and Procs

# converting blocks to Procs
# method1 and method2 do the same thing.  In method1, we provide a block
# in the usual way.  In method2, we convert the block to a Proc, which is
# bound to the argument p.  (To this, put an ampersand in front of the last
# parameter for the method, as in method2)
class Octopus
  def method1(x)
    ans = x + yield(8)
    puts "method1 result: " + ans.to_s
  end
  def method2(x, &p)
    ans = x + p.call(8)
    puts "method2 result: " + ans.to_s
  end
end

def octopus_test
  o = Octopus.new
  o.method1(2) {|x| x+10}
  o.method2(2) {|x| x+10}
end

# converting Procs to blocks

def proc_block_test
  p = lambda {|x| puts x.to_s}
  a = [1,2,3]
  # print each element in the array using a standard call to 'each'
  a.each {|x| puts x.to_s}
  # same thing, but pass a Proc to the each method
  a.each(&p)
end

# run these to print the results:
# octopus_test
# proc_block_test
