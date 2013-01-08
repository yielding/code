# encoding: utf-8

require "bindata"

module Sky
  class Book < BinData::Record
    endian :little
    uint16 :signature
    string :magic,   :read_length => 4
    string :skip,    :read_length => 10
    array  :records, :read_until => :eof do
      uint16  :rid
      uint16  :next_rid        # next_id
      uint16  :group_index
      uint16  :phone_no_index
      uint16  :aniversary_index
      uint16  :email_index
      stringz :name
      string  :skip1, 
              :read_length => lambda { 0x214 - 12 - (name.length + 1) }
    end

    def names
      actual_recs.map { |r| r.name.encode("utf-8", "euc-kr") }
    end

    def rids
      actual_recs.map { |r| r.rid }
    end

    def phone_no_indices
      actual_recs.map { |r| r.phone_no_index }
    end

    def count
      rids.size
    end

    def record_by_rid rid
      actual_recs.select { |r| r.rid == rid }  
    end

    def record_count
      records.size
    end

    def actual_recs
      @acture_recs ||= 
        records.select { |r| r.rid > 0 }
    end
  end
end
