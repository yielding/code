module WikiExample
  class WikiManager
    attr_accessor :active

    def connect
      self.active = _we_connected(3)
      p self.active
    end

    def get_version
      return 1
    end

    # _we_connected() is defined in C
  end
end
