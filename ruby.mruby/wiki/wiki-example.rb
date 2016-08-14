module WikiExample
  class WikiManager
    attr_accessor :active

    def initialize()
     @active = false 
    end

    def connect
      self.active = _connected_with_int(3)

      self.active = _connected_with_c_str("Hello world")

      self.active = _connected_with_c_str_len("Hello world")

      self.active = _connected_with_ruby_str("Hello world")

      self.active = _connected_with_ruby_str_arr(["Hello", nil, "world", nil])

      return true
    end

    def execute(wiki)
      puts "wiki.active: #{wiki.active}"

      return 2
    end

    def get_version
      # test for extension httprequest
      #h = HttpRequest.new
      #p h.get("http://www.nexon.net")

      return 5
    end
  end
end
