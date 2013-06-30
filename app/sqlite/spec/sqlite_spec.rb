require "sqlite"

describe SQLite do
  context "DB Header" do
    before (:each) do
      @path = "/Users/yielding/Desktop/work/KakaoTalk.db"
      File.exists?(@path).should == true
      @header = File.binread(@path, 0x1000)
      @header.length.should == 0x1000

      @dh = SQLite::DatabaseHeader.new(@header)
    end

    it "should be the same as" do
      @dh.signature.should == "SQLite format 3"
      @dh.page_size.should == 0x1000
      @dh.max_embedded_payload_fraction.should == 0x40
      @dh.min_embedded_payload_fraction.should == 0x20
      @dh.leaf_payload_fraction.should == 0x20
      @dh.file_change_counter.should == 0x830e
      @dh.db_file_size_in_pages.should == 0x01eb
      @dh.first_free_list_page_no.should == 0x0000
      @dh.total_no_of_free_list_pages.should == 0x0000
      @dh.schema_cookie.should == 0x0067
      @dh.schema_format_no.should == 0x0003
      @dh.default_cache_size.should == 0x0000
      @dh.largest_root_btree_page_no.should == 0x0029
      @dh.db_text_encoding.should == 0x0001
      @dh.user_version.should == 0x003a
      @dh.vaccum_mode.should == 0x0000
      @dh.version_valid_for_no.should == 0x830e
      @dh.sqlite_version_no.should == 0x002de21d
    end

    it "should read root page" do
      ph = SQLite::PageHeader.new(@header, 100)
      ph.interior_table?.should == true
      ph.no_of_cells.should == 1
      ph.right_pointer.should == 0x67
      ph.cells.length.should == 1
      offset = ph.first_cell_offset
      r0 = SQLite::TableInternalRecord.new(@header, offset)
      r0.pointer.should == 0x68
      r0.key.should == 0x0d
    end

    it "should read master records" do
      ph = SQLite::PageHeader.new(@header, 100)

      offset = ph.first_cell_offset
      r0 = SQLite::TableInternalRecord.new(@header, offset)
      lp_addr = (r0.pointer - 1) * @dh.page_size
      lp_addr.should == 0x67000
      page = File.binread(@path, 0x1000, lp_addr)
      p68  = SQLite::PageHeader.new(page)
      p68.leaf_table?.should == true
      p68.no_of_cells.should == 0x0d

      offset = 0
      for i in 0...p68.no_of_cells
        offset = p68.cells[i]
        r0 = SQLite::TableLeafRecord.new(page, offset)
        p r0.fields[0]
        p r0.fields[1]
        p r0.fields[2]
        p r0.fields[3]
        p r0.fields[4]
      end

      #offset = p68.first_cell_offset
      #r0 = SQLite::TableLeafRecord.new(page, offset)
      #r0.fields[0].should == "table"
      #r0.fields[1].should == "chat_rooms"
      #r0.fields[2].should == "chat_rooms"
      #r0.fields[3].should == 15

      #offset += r0.size
      #p offset.to_s(16)
      #r1 = SQLite::TableLeafRecord.new(page, offset)
      #p r1.fields[0]
      #p r1.fields[1]
      #p r1.fields[2]
    end
  end
end
