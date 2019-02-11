#!/usr/bin/env ruby

a0 = [:one, :two, :three, :four, :five, :six, :seven, :eight, :nine]

a1 = [:ten, :eleven, :twelve, :thirteen, :fourteen, :fifteen, 
      :sixteen, :seventeen, :eighteen, :nineteen]

a2 = [:twenty, :thirty, :forty, :fifty, :sixty, :seventy, :eighty, :ninety]


# 1의 자리
oneth = a0.map {|e| e.size }.reduce(:+)
p oneth

# 10의 자리
tenth_1 = a1.map {|e| e.size}.reduce(:+)

temth_2_tmp = (a2.product(a0) + a2).flatten
#p temth_2_tmp
tenth_2 = (a2.product(a0) + a2).flatten.map { |e| e.size }.reduce(:+)

tenth = tenth_1 + tenth_2
p tenth

one2_99 = oneth + tenth

# 100의 자리
hundth_1 = oneth * 10 * 10    # 1-9
hundth_2 = one2_99 * 9        # 1-99
hundth_3 = :hundred.size * 9  # 100, 200, 300, .. 900
hundth_4 = 9 * 99 * 10        # (hundred and:9) * 99 * 9
hundth   = hundth_1 + hundth_2 + hundth_3 + hundth_4

p oneth + tenth + hundth + [:one, :thousand].map { |e| e.size }.reduce(:+)
