require 'RubyClasses'

include RubyClasses

class Person
  def age
    name.length + 1
  end
end

p = Person.new("leech")
puts p.name
puts p.age

a = Animal.new("cat")
puts a.name
