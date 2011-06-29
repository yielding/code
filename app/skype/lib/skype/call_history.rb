module Skype
  class ChatHistory
    attr_reader :offset
    attr_reader :stream

    def initialize(path)
      @path = path
      @stream, @index, @ok = nil, 0, false;
    end

    def open
      @stream = File.open(@path, "rb")
      @ok = (@stream != nil)
    end

    def open?; @ok end
  end
end
