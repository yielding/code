require 'pp'

p VIM.desc
p VIM::VIM_MAJOR
p VIM::VIM_MINOR
VIM.set_option("set nu")

begin
  # VIM::throw_test()
  VIM.set_option("set exception")
rescue Exception => ex
  p "caught it! with #{ex.message}"
end

begin
  p VIM::Buffer.count
  b = VIM::Buffer[1]
  b = VIM::Buffer.current
rescue Exception => ex
  p "caught it! with #{ex.message}"
end

begin
  p VIM::Window.count
  VIM::Window[1]
  w = VIM::Window.current
  p w.height
  p w.width
  w.height += 20
  p w.height
  
rescue Exception => ex
  p "caught it! with #{ex.message}"
end

w = VIM::Window.current
pp w.cursor
w.cursor = [100, 200]
pp w.cursor
