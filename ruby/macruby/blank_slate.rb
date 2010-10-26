class BlankSlate
  def self.hide(name)
    if instance_methods.include?(name.to_s) and name !~ /^(__|instance_eval)/
      @hidden_methods ||= {}
      @hidden_methods[name.to_sym] = instance_method(name)
      undef_method name
    end
  end

  instance_methods.each { |m| hide(m) }
end

class Computer
  instance_methods.each { |m| 
    undef_method m unless m.to_s =~ /^__|method_missing|respond_to?/
  }

  def initialize(computer_id, data_source)
    @id = computer_id
    @data_source = data_source
  end

  def method_missing(name, *args)
    super if !respond_to?(name)
    info   = @data_source.send("get_#{name}_info", args[0])
    price  = @data_source.send("get_#{name}_price", args[0])
    result = "#{name.to_s.capitalize}: #{info} ($#{price})"
    return "* #{result}" if price >= 100
  end

  def respond_to?(method)
    @data_source.respond_to?("get_#{method}_info") || super
  end
end


require "using"
require "test/unit"

class TestUsing < Test::Unit::TestCase
  class Resource
    def dispose
      @disposed = true
    end

    def disposed?
      @disposed
    end
  end

  def test_disposes_of_resources
    r = Resource.new
    using(r) {}
    assert r.disposed?
  end

  def test_case_name
    assert_raise(Exception) { 
      using (r) { raise Exception }
    }
  end

end


