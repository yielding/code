module WikiExample
  class WikiManager
    attr_accessor :active

    def connect
      self.active = _we_connected(3)
      return true
    end

    def get_version
      h = HttpRequest.new
      p h.get("http://www.nexon.net")

      return 5
    end

    # _we_connected() is defined in C
  end
end
