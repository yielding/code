#!/usr/bin/env ruby

require "byte_buffer"
require "ostruct"
require "pp"

module SQLite
  class DatabaseHeader
    attr_reader :signature, :page_size,
       :max_embedded_payload_fraction, :min_embedded_payload_fraction,
       :leaf_payload_fraction,
       :file_change_counter, :db_file_size_in_pages,
       :first_free_list_page_no, :total_no_of_free_list_pages,
       :schema_cookie, :schema_format_no,
       :default_cache_size, :largest_root_btree_page_no,
       :db_text_encoding, :user_version, :vaccum_mode,
       :version_valid_for_no, :sqlite_version_no

    def initialize(header, offset=0)
      b = ByteBuffer.new(header)
      @signature                     = b.get_ascii
      @page_size                     = b.get_uint2_be
      @file_format_write_version     = b.get_int1
      @file_format_read_version      = b.get_int1
      @reserved_size                 = b.get_int1
      @max_embedded_payload_fraction = b.get_int1
      @min_embedded_payload_fraction = b.get_int1
      @leaf_payload_fraction         = b.get_int1
      @file_change_counter           = b.get_uint4_be
      @db_file_size_in_pages         = b.get_uint4_be
      @first_free_list_page_no       = b.get_uint4_be
      @total_no_of_free_list_pages   = b.get_uint4_be
      @schema_cookie                 = b.get_uint4_be
      @schema_format_no              = b.get_uint4_be
      @default_cache_size            = b.get_uint4_be
      @largest_root_btree_page_no    = b.get_uint4_be
      @db_text_encoding              = b.get_uint4_be

      @user_version = b.get_uint4_be
      @vaccum_mode  = b.get_uint4_be
      b.pos += 24
      @version_valid_for_no = b.get_uint4_be
      @sqlite_version_no    = b.get_uint4_be
    end

    def encoding
      case @db_text_encoding   
      when 1; "utf-8"
      when 2; "utf16-le"
      when 3; "utf16-be"
      else  ; raise Exception.new("wrong encoding")
      end
    end
  end

  class PageHeader
    attr_reader :no_of_cells, :right_pointer
    attr_reader :first_cell_offset
    attr_reader :cells

    def initialize(buffer, offset=0)
      b = ByteBuffer.new(buffer, offset)

      @flag = b.get_int1
      @offset_first_free_block = b.get_uint2_be
      @no_of_cells             = b.get_uint2_be
      @first_cell_offset       = b.get_uint2_be
      @no_of_fragmented_free_bytes = b.get_int1
      @right_pointer               = b.get_uint4_be if interior_index? or interior_table?

      @cells = []
      @no_of_cells.times { |n| @cells[n] = b.get_uint2_be }
    end

    def leaf_table?;     @flag == 0x0d end
    def leaf_index?;     @flag == 0x0a end
    def interior_table?; @flag == 0x05 end
    def interior_index?; @flag == 0x02 end
  end

  class TableInternalRecord
    attr_reader :pointer
    attr_reader :key

    def initialize(buffer, offset)
      b = ByteBuffer.new(buffer, offset)
      @pointer = b.get_uint4_be
      @key = b.get_varint
    end

    def to_s
      "pointer: 0x#{@pointer.to_s(16)}, key: 0x#{@key.to_s(16)}" 
    end
  end

  class TableLeafRecord
    attr_reader :length, :rid, :size
    attr_reader :column_types, :fields

    def initialize(buffer, offset=0)
      b = ByteBuffer.new(buffer, offset)
      @length, sz0 = b.get_varint_with_size
      @rid, sz1    = b.get_varint_with_size
      @size = length + sz0 + sz1
      @column_types = []
      @fields       = []
      hdr_len, sz = b.get_varint_with_size
      left = hdr_len - sz
      while left > 0
        len, sz = b.get_varint_with_size
        @column_types << len
        left -= sz
      end

      @column_types.each { |col| @fields << field_from(col, b) }
    end

    def field_from(serial, b)
      return b.get_int1    if serial == 1
      return b.get_int2_be if serial == 2
      return b.get_int3_be if serial == 3
      return b.get_int4_be if serial == 4
      return b.get_int6_be if serial == 5
      return b.get_int8_be if serial == 6
      return b.get_int8_be if serial == 7  # TODO return as double
      return 0             if serial == 8
      return 1             if serial == 9
      return b.get_string((serial - 12) / 2) if serial >= 12 and serial.even?
      return b.get_string((serial - 13) / 2) if serial >= 13 and serial.odd?
    end
  end
  
  class SQLiteAnalyzer
    attr_reader :schemas

    def initialize path
      raise Exception.new("File not exist") unless File.exists?(path)
      @path = path

      root_page  = File.binread(path, 0x1000)
      @db_header = SQLite::DatabaseHeader.new(root_page)
      @page_size = @db_header.page_size
      @schemas   = read_schema(1)
    end

    def table_names
      @schemas.select { |schema| schema.type == "table" }
              .map    { |schema| schema.name }
    end

    private
    def read_schema(page_no)
      schemas = []
      page    = read_page(page_no)
      offset  = page_no == 1 ? 100 : 0
      phdr    = PageHeader.new(page, offset)
      if phdr.interior_table?
        # 1. all records except right_most one
        phdr.no_of_cells.times do |i|
          ir = TableInternalRecord.new(page, phdr.cells[i])
          schemas += read_schema(ir.pointer)
        end

        # 2. right_most record
        schemas += read_schema(phdr.right_pointer)
      elsif phdr.leaf_table?
        phdr.no_of_cells.times do |i|
          leaf = TableLeafRecord.new(page, phdr.cells[i])
          schema = OpenStruct.new
          schema.type      = leaf.fields[0]
          schema.name      = leaf.fields[1]
          schema.tbl_name  = leaf.fields[2]
          schema.root_page = leaf.fields[3]
          schema.sql       = leaf.fields[4]
          schemas << schema
        end
      end

      return schemas
    end

    def read_page(page_no)
      addr = (page_no - 1) * @page_size
      File.binread(@path, @page_size, addr)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
end
