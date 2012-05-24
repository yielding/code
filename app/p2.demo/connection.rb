#!/usr/bin/env ruby

require 'datastructure'
require 'socket'
require 'pp'

class Connection
  attr_accessor :uid, :worker
  
  def initialize(uid, worker)
    @uid = uid
    @worker = worker
    @ra = nil
  end
  
  def open(ip, port, timeout)
    @ra = TCPSocket.new(ip, port)
    linger = [1, 0].pack("ii")
    @ra.setsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER, linger)
    return false if @ra == nil
    true
  end

  def close
    @ra.close
  end

  # def execute(opcode, cmd, in_pl)
  #   nil
  # end
end

class RFIDKTReaderConnection < Connection
  def execute(cmd, pid, data)
    lengths = { 3 => 37, 6 => 37 }

    pdu = ByteBuffer.new(lengths[cmd])
    len = data.length + 3
    pdu.set_int1(0xF0)    # stx
    pdu.set_int1(0xF0)    # stx
    pdu.set_int1(cmd)     # cmd
    pdu.set_int2(len)     # cmd
    pdu.set_int2(0x00)    # reserved
    pdu.set_int1(pid)     # pid
    pdu.set_string_without_null(data) if data.length > 0
    pdu.set_int1(0xFE)    # etx
    pdu.set_int1(0xFE)    # etx
    sent = @ra.send(pdu.to_packet, 0)
  end

  def recv cmd
    lengths = { 3 => 10, 6 => 10 }
    received = @ra.recv(lengths[cmd])
    received
  end
end

class RFIDKTReaderErrorConnection < Connection
  def execute(cmd, pid, data)
    lengths = { 3 => 37, 6 => 37 }

    pdu = ByteBuffer.new(lengths[cmd])
    len = data.length + 3
    pdu.set_int1(0xF0)    # stx
    pdu.set_int1(0xF0)    # stx
    pdu.set_int1(cmd)     # cmd
    pdu.set_int2(len)     # cmd
    pdu.set_int2(0x00)    # reserved
    pdu.set_int1(pid)     # pid
    pdu.set_string_without_null(data) if data.length > 0
    pdu.set_int1(0xFE)    # etx
    pdu.set_int1(0xFE)    # etx
    sent = @ra.send(pdu.to_packet, 0)
  end

  def recv cmd
    lengths = { 3 => 10, 6 => 10 }
    received = @ra.recv(lengths[cmd])
    received
  end
end
