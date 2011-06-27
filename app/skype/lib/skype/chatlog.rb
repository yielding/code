require "little_endian"

module Skype
  class ChatLog
    include LittleEndian

    attr_reader   :stream
    attr_accessor :offset

    def initialize path
      @offset, @path = 0, path
      @ok, @stream   = false, nil
    end

    def open
      @steam = File.open(@path, "rb")
      @ok = (@stream == nil)
    end

    def open?
      @ok
    end

  end
end
