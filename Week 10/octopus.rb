class Octopus
  @@octo_var = 2      # class variable
  TENTACLES = 8   # constant
  def initialize(n)
    @name = n
  end
  def speak
    puts "I'm an octopus named #{@name}"
  end
  def print_vars
    puts @@octo_var.to_s
    puts TENTACLES.to_s
  end
  def Octopus.classgreeting
    puts "hi from class Octopus"
  end
end

# to try:
# o = Octopus.new("oscar")
# o.speak
# o.print_vars
# Octopus.classgreeting
