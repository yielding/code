#!/usr/bin/env ruby19

require "matrix"
require "pp"

=begin rdoc
   |f(n+1)|   |1 1|   |f(n)  | 
   |      | = |   | x |      | 
   |f( n )|   |1 0|   |f(n-1)|

              |1 1|n  | 1 |
            = |   | x |   |  
              |1 0|   | 0 |
=end

m = Matrix[[1, 1], [1, 0]]
p (m**20000 * Vector[1, 0])[1]
