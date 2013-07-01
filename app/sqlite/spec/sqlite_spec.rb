require "sqlite"

describe SQLite do
  context "DB Header" do
    before (:each) do
      @path = "/Users/yielding/Desktop/work/KakaoTalk.db"
      File.exists?(@path).should == true
      @root_page = File.binread(@path, 0x1000)
      @root_page.length.should == 0x1000

      @dh = SQLite::DatabaseHeader.new(@root_page)
      @ph = SQLite::PageHeader.new(@root_page, 100)
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

    it "should read root page header like this" do
      @ph.interior_table?.should == true
      @ph.no_of_cells.should == 1
      @ph.right_pointer.should == 0x67
      @ph.cells.length.should == 1
    end

    it "should read all records except right_most pointer" do
      offset = @ph.first_cell_offset
      r0 = SQLite::TableInternalRecord.new(@root_page, offset)
      r0.pointer.should == 0x68
      r0.key.should == 0x0d

      lp_addr = (r0.pointer - 1) * @dh.page_size
      lp_addr.should == 0x67000
      page = File.binread(@path, 0x1000, lp_addr)
      p68  = SQLite::PageHeader.new(page)
      p68.leaf_table?.should == true
      p68.no_of_cells.should == 0x0d

      for i in 0...p68.no_of_cells
        offset = p68.cells[i]
        r0 = SQLite::TableLeafRecord.new(page, offset)
        #p r0.fields[0..4]
      end
    end

    it "should read right master records" do
      rp_addr = (@ph.right_pointer - 1) * @dh.page_size
      page = File.binread(@path, 0x1000, rp_addr)
      p67 = SQLite::PageHeader.new(page)
      p67.leaf_table?.should == true
      p67.no_of_cells.should == 0x1a

      offset = 0
      for i in 0...p67.no_of_cells
        offset = p67.cells[i]
        r0 = SQLite::TableLeafRecord.new(page, offset)
        #p r0.fields[0..4]
      end
    end
  end

  context "DB Header" do
    before (:each) do
      @path   = "/Users/yielding/Desktop/work/KakaoTalk.db"
      @sqlite = SQLite::SQLiteAnalyzer.new(@path)
    end

    it "should contain tables" do
      tables = [ "android_metadata", "schema_migrations", "ip_table", "friends",
                 "sqlite_sequence", "chat_rooms", "chat_logs", "chat_sending_logs",
                 "recently_emoticons", "item", "item_resource", "track_logs",
                 "wifi_cache", "wifi_cache_bssid", "apps" ]
      #pp @sqlite.table_names.should 

    end
  end

end
