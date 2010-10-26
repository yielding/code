#!/usr/bin/env ruby1.9 -wKU

## no. 1.1
# res = 0
# 1.upto(10000 -1) { |n|
#   res += n if (n % 3 == 0) or (n % 5 == 0) 
# }
# puts res

# no 1.2
 res = 0;
 def sum_div_by n, target
   p = target / n
   n * p * (p + 1) / 2
 end
 
 a = sum_div_by( 3, 10000-1)
 b = sum_div_by( 5, 10000-1)
 c = sum_div_by(15, 10000-1)
 printf "%d, %d, %d, %d", a, b, c, a + b -c
