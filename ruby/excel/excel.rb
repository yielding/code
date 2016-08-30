#!/usr/bin/env ruby

require 'spreadsheet'
require 'pp'

book = Spreadsheet::Workbook.new
sheet1 = book.create_worksheet
sheet2 = book.create_worksheet :name => 'My Second Worksheet'
sheet1.name = 'My First Worksheet'
sheet1.row(0).concat %w{Name Country Acknowlegement}
sheet1[1,0] = 'Japan'
row = sheet1.row(1)
row.push 'Creator of Ruby'
row.unshift 'Yukihiro Matsumoto'
sheet1.row(2).replace [ 'Daniel J. Berger', 'U.S.A.',
                        'Author of original code for Spreadsheet::Excel' ]
sheet1.row(3).push 'Charles Lowe', 'Author of the ruby-ole Library'
sheet1.row(3).insert 1, 'Unknown'
sheet1.update_row 4, 'Hannes Wyss', 'Switzerland', 'Author'

book.write "test.xls"
