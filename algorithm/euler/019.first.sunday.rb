#!/usr/bin/env ruby19

=begin
  1901 1월             

일 월 화 수 목 금 토  
       1  2  3  4  5 
 6  7  8  9 10 11 12 
13 14 15 16 17 18 19 
20 21 22 23 24 25 26
27 28 29 30 31       
=end

class Numeric
  def is_leep?
    self % 4 == 0 && (self % 100 != 0 || self % 400 == 0)
  end
end

days_of_month1 = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
days_of_month2 = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

days = [0, 0]
for year in 1901..2000
  dom   = year.is_leep? ? days_of_month2 : days_of_month1
  days += dom.reduce([]) { |sum, m| sum + [*1..m] }
end

p days.each_slice(7).select { |c| c.first == 1 } \
                    .size
