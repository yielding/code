# require 'pp'
# 
# m = Std::Map.new
# m['leech'] = 1
# p m['leech']
# 
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
  p VIM::Window.count
  VIM::Window[1]
  w = VIM::Window.current
rescue Exception => ex
  p "caught it! with #{ex.message}"
end

begin
  p VIM::Buffer.count
  VIM::Buffer[1]
  b = VIM::Buffer.current
rescue Exception => ex
  p "caught it! with #{ex.message}"
end