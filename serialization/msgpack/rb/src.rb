require 'msgpack/rpc'

class MyHandler
  def add(x,y) return x+y end
end

svr = MessagePack::RPC::Server.new
svr.listen('0.0.0.0', 18800, MyHandler.new)
svr.run
