# encoding: utf-8

require "bindata"

module Sky
  class PhoneNumber < BinData::Record
    endian :little
    uint16 :signature
    string :magic,   :read_length => 6
    string :skip,    :read_length => 8
    array  :records, :read_until  => :eof do
      uint16  :rid
      uint16  :next_rid
      uint16  :skip0
      uint16  :book_rid
      string  :skip1,   :read_length => 3
      uint8   :type2
      stringz :phone_no
      string  :skip2,   
              :read_length => lambda { 0x2e - 12 - (phone_no.length + 1) }
    end

    def numbers
      actual_recs.map { |r| r.phone_no }
    end

    def rids
      actual_recs.map { |r| r.rid }
    end

    def count
      rids.size
    end

    def record_count
      records.size
    end

    def actual_recs
      @acture_res ||= records.select { |r| r.rid > 0 } 
    end
  end
end
