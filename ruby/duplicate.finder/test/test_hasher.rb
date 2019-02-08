require "helper"

class TestHasher < MiniTest::Test
  def setup
    @hasher = Hasher.new(name:"SHA1", size: 0)
  end

  def test_123
    actual = @hasher.buffer_hash(buffer: "abc")
    assert_equal("a9993e364706816aba3e25717850c26c9cd0d89d", actual)
  end

  def test_data
    path = "./data/91leech.jpg"
    assert File.exists? path 
    data = File.read(path)
  end

  def test_dir
    images = File.join("**", "data", "**", "*.{jpg,png,gif}")

    Dir.glob(images) do |d| 
      path = "#{Dir.getwd}/#{d}"
      if File.file?(path)
        a = @hasher.buffer_hash(buffer:File.read(path)) 
        b = @hasher.file_hash(fname:path)
        assert_equal(a, b)
      end
    end
  end
end
