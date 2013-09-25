#!/usr/bin/env ruby2.0

require "socket"

class KakaoClient
  def initialize(ip="127.0.0.1")
    @s = TCPSocket.new(ip, 7781)
  end

  def ping
    packet = [1].pack("c") + [5].pack("N") + "MDSBC"
    @s.send(packet, 0)
    return @s.recv(6)[1..-1] == "MDSBC"
  end

  def decrypt ciphered, salt
    packed_len4 = lambda { |str| [str.length].pack("N") }

    packet = [2].pack("c") + packed_len4.call(ciphered) + ciphered + packed_len4.call(salt) + salt
    @s.send(packet, 0)
    @s.recv(1)
    len = @s.recv(4).unpack("N")
    @s.recv(len[0])
  end

  def kill_server
    @s.send([0].pack("c"), 0)
  end

end

kakao = KakaoClient.new("192.168.0.129")
#kakao = KakaoClient.new()
# ping
# print kakao.ping

# decrypt
ciphered = "m+oavcl6PVEo1RBcCFlKSQ=="
salt     = "23303370"
100.times {
  10.times { |i|
    printf "%d, %s\n", i, kakao.decrypt(ciphered, salt)
  }
  sleep(1)
}

# kill server
# kakao.kill_server
