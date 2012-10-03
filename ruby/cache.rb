#!/usr/bin/env ruby19
# -*- encoding: utf-8 -*-

module Cache
  def cache_method(target_method)
    hidden_target_method = "__#{target_method.to_s}".to_sym
    alias_method hidden_target_method, target_method

    define_method(target_method) do |*args|
      cache_name = "@__#{target_method.to_s}_cache"
      cache = instance_variable_get(cache_name)
      cache = instance_variable_set(cache_name, Hash.new) if cache.nil?

      if cache.include?(args)
        return cache[args]
      else
        return cache[args] = send(hidden_target_method, *args)
      end
    end
  end
end 

class Series
  def fibo n
    return 1 if n == 1 || n == 2
    return fibo(n-1) + fibo(n-2)
  end
end                             

puts "Normal Execution"
t = Time.new
f = Series.new

iteration = 50

#puts f.fibo(iteration)
#puts "Elapsed: #{Time.new - t}"

puts "Cached Execution"
t = Time.new
Series.extend(Cache)
Series.instance_eval do
  cache_method :fibo
end                 

puts f.fibo(iteration)
puts "Elapsed: #{Time.new - t}"
