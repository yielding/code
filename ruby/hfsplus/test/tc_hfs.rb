require "test/unit"

require "hfs"

class TestHFS < Test::Unit::TestCase
  def setup
    @io = File.open("data/hfsplus.bin")
    @io.read(1024)
  end

  def teardown
    @io.close
  end

  def test_read_header
    header = HFSPlusVolumeHeader.read(@io)
    assert_equal(header.signature, 0x4858)
    assert_equal(header.version, 0x0005)
    assert_equal(header.attributes, 0xc0002100)
    v = header.lastMountedVersion
    b1 = (v == 0x6673636B)  # fsck
    b2 = (v == 0x4846534a)  # HFSJ
    assert(b1 || b2)
    assert_equal(header.journalInfoBlock, 0x1d)
    assert_equal(header.createDate, 0xca9d1f92)
  end
end
