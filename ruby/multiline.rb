#!/usr/bin/env ruby

def msg2
   "My name is leech
    what do you want to do?
   ".gsub(/^( |\t)+/, "")
end

messgae = <<-EOF.gsub /^( |\t)+/, ""
  My name is Lee Chang Ha
EOF


puts messgae
puts msg2

puts ""

store_id = 10
sql = "SELECT A.id AS app_id, V.ver_id AS ver_id, A.name AS app_name, IF(A.store_id = 2, A.ios_guid, A.packages) AS db_path 
       FROM apps A 
       JOIN (SELECT app_id, MAX(id) ver_id FROM versions GROUP BY app_id) V ON A.id = V.app_id 
       LEFT OUTER JOIN `schemas` S ON V.ver_id = S.ver_id AND S.id IS NULL 
       JOIN (SELECT app_id FROM nodes GROUP BY app_id) N ON V.app_id = N.app_id 
       WHERE A.use_yn = 'Y' AND A.store_id = #{store_id} 
       ORDER BY A.id".gsub(/^( |\t)+/, "")

puts sql
