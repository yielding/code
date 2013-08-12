require "digest/sha1"
require "digest/md5"

class Hasher
  def initialize name:SHA1, size: 100
    @size = size

    digests  = [Digest::MD5, Digest::SHA1, Digest::SHA2]
    selected = digests.detect { |digest| digest if digest.name.end_with?(name) }
    @digest  = selected.nil? ? Digest::SHA1.new : selected.new
  end

  def buffer_hash(buffer: "")
    return @digest.hexdigest(buffer)
  end

  def file_hash(fname: "")
    return @digest.file(fname).hexdigest
  end
end
