#!/usr/bin/env ruby

module Fluent
  module Factory
    def self.included(klass)
      klass.extend(ClassMethods)
    end

    module ClassMethods
      def new(*args, &block)
        if klass = args_to_class(*args, &block)
          instance = klass.allocate
          instance.__send__(:initialize, *args, &block)
          instance
        else
          super(*args)
        end
      end

      def args_to_class(*args, &block)
        raise NotImplementedError.new("you should define this class method to return the relevant class.")
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
   class Animal
    include Fluent::Factory
    
    def self.args_to_class(*args, &block)
      return if not args.first.is_a?(String)
      case args.first.downcase
      when /garfield/ then Cat
      when /bucephalus/ then Horse
      when /snoopy/ then Dog
      end
    end

    def initialize(name)
      @name = name
    end
  end
  
  class Dog < Animal; end
  class Cat < Animal; end
  class Horse < Animal; end

  raise "Failed" if Animal.new("snoopy").class != Dog
  raise "Failed" if Animal.new("garfield").class != Cat
  raise "Failed" if Animal.new("bucephalus").class != Horse

  puts "Fluent::Factory works."
end