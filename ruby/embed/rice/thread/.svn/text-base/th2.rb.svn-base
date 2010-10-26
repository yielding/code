require 'socket' 
#require 'resolv-replace' 

count  = 0 
thread = Thread.new { Thread.pass; loop { count += 1; } } 
puts IPSocket.getaddress("www.ruby-lang.org") # => "221.186.184.68" 
puts count
