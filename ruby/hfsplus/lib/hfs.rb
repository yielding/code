#!/usr/bin/env ruby
require "structs"

class HFSFile
  def initialize(volume, hfsfork, fileID, deleted=false)
    @volume       = volume
    @block_size   = volume.block_size
    @fileID       = fileID
    @total_blocks = hfsfork.totalBlocks
    @logical_size = hfsfork.logicalSize
    @extents      = []
    @deleted      = deleted

    bc = 0
    hfsfork.extentRecords.each { |extent|
      @extents << extent
      bc += extent.blockCount
    }

    while bc != hfsfork.totalBlocks
      key, value = @volume.get_extents_overflow_for_file(@fileID, bc)
      unless value
        puts "extents overflow missing, startblock=#{bc}"
        break
      end

      value.each { |extent| @extents << extent; bc += extent.blockCount }
    end
  end

  def read_all(output_file, truncate=true)
  end

  def read_all_buffer(truncate=true)
  end

  def process_block(block, lba)
  end

  def read_block(block, lba)
    bs = @volume.block_size
    raise Exception("BLOCK OUT OF BOUND ") if lba * bs > @logical_size
  end
end

class HFSVolume
  attr_reader :header, :block_size

  def initialize(filename, write_=false, offset=0)
    @file = File.open(filename, "rb")
    @offset = offset

    begin
      data = read(0, 0x1000)
      @header = HFSPlusVolumeHeader.read(data[0x400..0x800])
    rescue
      raise Exception "exception while reading header"
    ensure
      @file.close
    end

    @block_size  = @header.blockSize
    @volume_size = @block_size * @header.totalBlocks

    puts "WARNING: image appears to be truncated" unless File.size(filename) == @volume_size

    @allocation_file = HFSFile.new(self, @header.allocationFile, KHFSAllocationFileID)
  end

  def read(offset, size)
    @file.seek(offset, IO::SEEK_SET)
    @file.read(size)
  end

  def write(offset, data)
  end

  def write_block(lba, block)
  end

  def volume_id
  end

  def unallocated_blocks
  end

  def get_extents_overflow_for_file
  end

  def get_xattr
  end

  def get_file_by_path(path)
  end

  def list_folder_contents(path)
  end

  def list_xattrs(path)
  end

  def read_file(path, return_str=false)
  end

  def read_journal
  end

end
