require 'date'

cervantes = DateTime.iso8601("1616-04-11", Date::ITALY)
shakespeare = DateTime.iso8601("1616-04-11", Date::ENGLAND)

puts cervantes.gregorian     #=> 1616-40-11T00:00:00+00:00
puts shakespeare.gregorian   #=> 1616-40-21T00:00:00+00:00