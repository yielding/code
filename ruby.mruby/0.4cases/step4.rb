begin
  puts "welcome to step4."

  a = plus(1,2)
  puts "plus(1,2) => #{a.inspect}"

  b = plus('1','2')
  puts "plus('1','2') => #{b.inspect}"

  c = plus(plus('hello',' world'), plus(' from',' mruby'))
  puts c
rescue => e
  p e
end

