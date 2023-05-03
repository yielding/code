#!/usr/bin/env ruby

def pr(arr)
  arr.each do |item|
    print item, " "
  end
  puts ""
end

oids = Array.new
for i in (0..10)
  oids.push("1.2.3.4.5.6.7.8.9.#{i}.2")
end

nums = Array.new
oids.each do |oid|
  if oid =~ /\.(\d+)$/
    nums.push($1.to_i)
  end
end

if __FILE__ == $0
  nums.uniq!
  pr(nums)
end
