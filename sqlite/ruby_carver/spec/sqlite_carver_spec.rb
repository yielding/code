require "sqlite_carver"

describe SQLiteCarver do
  context "The header of SQLite" do
    before (:each) do
      @sqlite = SQLiteCarver.new("resources/maptile.db")
    end

    it "consists of 100 byte header" do
      @sqlite.size.should > 100
      @sqlite.size.should == 4190208
    end

    it "starts with `SQLite`" do
      @sqlite.signature.should == "SQLite"
    end
  end

  context "Page size" do
  end
end
