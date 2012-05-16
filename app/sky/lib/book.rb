#!/usr/bin/env ruby
# encoding: utf-8

require "bindata"

module Sky
  class Book < BinData::Record
    endian :little
    uint16 :signature
    string :magic, :read_length => 4
    string :skip,  :read_length => 10
    array  :records, :read_until => :eof do
      uint16 :rid
      uint16 :self_index       
      uint16 :group_index
      uint16 :phone_no_index
      uint16 :aniversary_index
      uint16 :email_index
      string :name,  :read_length => 16
      string :skip1, :read_length => 0x214 - 12 - 16
    end

    def names
      records.map { |r| r.name.strip.encode("utf-8", "euc-kr") }
    end

    def rids
      actual_recs.map { |r| r.rid }
    end

    def self_indices
      actual_recs.map { |r| r.self_index }
    end

    def phone_no_indices
      actual_recs.map { |r| r.phone_no_index  }
    end

    def count
      return 0 if records.size == 0 
      records.to_ary.index { |r| r.rid == 0 } - 1
    end

    def record_count
      records.size
    end

    private
    def actual_recs
      records.select { |r| r.self_index > 0 }
    end
  end
end
