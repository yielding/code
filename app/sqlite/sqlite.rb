#!/usr/bin/env ruby

require_relative "byte_buffer"

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
    def initialize(buffer, offset=0)
      b = ByteBuffer.new(buffer)

      @flag = b.get_int1
      @offset_first_free_block = b.get_uint2_be
      @no_of_cells             = b.get_uint2_be
      @offset_first_cell_contents_area = b.get_uint2_be
      @no_of_fragmented_free_bytes     = b.get_int1
      if (interior_index? || interior_table?)
        @right_pointer = b.get_uint4_be
      end
    end

    def leaf_table?
      @flag == 0x0d
    end

    def leaf_index?
      @flag == 0x0a
    end

    def interior_table?
      @flag == 0x05
    end

    def interior_index?
      @flag == 0x02
    end

  end

end

if __FILE__ == $PROGRAM_NAME
end
