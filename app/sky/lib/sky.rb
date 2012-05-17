$:.unshift(File.dirname(__FILE__)) unless
  $:.include?(File.dirname(__FILE__)) or
  $:.include?(File.expand_path(File.dirname(__FILE__)))

require 'sky/book'
require 'sky/phone_numbers'

module Sky
  class Contacts
    attr_reader :book, :phno

    def initialize book, phno
      @book = book
      @phno = phno
      @contacts = Hash.new { |h, k| h[k] = [] }
      synthesize
    end

    def print
      @contacts.each do |key, value|
        no = value.join(", ")
        printf "%-10s: %s\n", key, no
      end
    end

    def synthesize
      @phno.actual_recs.each do |rec| 
        # In case we analyze unused area, there is no ordering 
        # in the records. So, search every record instead of direct access
        #
        r = @book.record_by_rid(rec.book_rid)[0]
        unless r.nil?
          name = r.name.encode("utf-8", "euc-kr")
          @contacts[name] << rec.phone_no
        end
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  book = Sky::Book.new;        book.read(File.open("../resources/book.pbk"))
  phno = Sky::PhoneNumber.new; phno.read(File.open("../resources/number.pbk"))
  contacts = Sky::Contacts.new(book, phno)
  contacts.print
end
