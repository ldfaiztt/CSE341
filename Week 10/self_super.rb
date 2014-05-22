# CSE341 - more about self and super in Ruby

# 'self' always means the object that received the message (even if we use
# the variable 'self' in an inherited method)
#
# 'super' is not a variable in Ruby -- instead, 'super' means "invoke the
# method with the same name as the current method, starting the lookup in
# the superclass of the method that uses 'super' (*not* necessarily the
# superclass of the class of the receiver).

class Animal
  def description
    return "an animal" + self.additionalDescription
  end
  def additionalDescription
    return ""
  end
end

class Cephalopod < Animal
  def additionalDescription
    return "; also a cephalopod"
  end
end

class Octopus < Cephalopod
  def additionalDescription
    # unlike Java, 'super' here means to invoke the method of the same
    # name, but starting the lookup in the superclass of the method with
    # the 'super' in it  
    return super + " and an octopus"
  end
  def tentacles
    return 8
  end
end

class MutantOctopus < Octopus
  def tentacles
    return 7
  end
end

# demonstrate the Animal class hierarchy
def demo1
  a = Animal.new
  c = Cephalopod.new
  o = Octopus.new
  m = MutantOctopus.new
  puts a.description
  puts c.description
  puts o.description  
  puts m.description  
  puts o.tentacles
  puts m.tentacles
end

# mixins and overriding
# When searching for a method:
#   first look in the given class
#   then its mixins (leftmost first),
#   then its superclass,
#   then the mixins of the superclass,
#   and on up the inheritance hierarchy.
# Raise an exception if no method with the given name is found.

class C1
  def test1
    puts "in C1 test1"
  end
  def test2
    puts "in C1 test2"
  end
end

module M1
  def test1
    puts "in mixin M1 test1"
  end
  def test2
    puts "in mixin M1 test2"
    super
  end
end

module M2
  def test1
    puts "in mixin M2 test1"
  end
  def test2
    puts "in mixin M2 test2"
    super
  end
  def test3
    puts "in mixin M2 test3"
  end
end

class C2 < C1
  include M1, M2
end

class C3 < C1
  include M1, M2
  def test1
    puts "in C3 test1"
  end
end

class C4 < C1
  include M1
end

class C5 < C3
end

def demo2
  c1 = C1.new
  c2 = C2.new
  c3 = C3.new
  c4 = C4.new
  c1.test1
  c2.test1
  c3.test1
  c3.test3
  c4.test1
end

