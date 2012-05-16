#!/usr/bin/env ruby
# encoding: utf-8

require "bindata"

module Sky
  class PhoneNumber < BinData::Record
    endian :little
    uint16 :signature
    string :magic, :read_length => 6
    string :skip,  :read_length => 8
    array  :records, :read_until => :eof do
      uint16 :rid
      uint16 :next_rid
      string :skip0,    :read_length => 7
      uint8  :type
      string :phone_no, :read_length => 34
    end

    def numbers
      actual_recs.map { |r| r.phone_no.strip }
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

    private
    def actual_recs
      records.select { |r| r.rid > 0 } 
    end
  end
end
