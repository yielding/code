#!/usr/bin/env ruby -wKU
# -*- encoding: utf-8 -*-

require 'ya2yaml'

tree = { 'name'  => 'ruby', 
         'users' => ['leech'  => { 'age' => 40, "sex" => 'woman' },
                     '이윤정' => { 'age' => 40, "sex" => 'woman' }
                    ] 
}

data = tree.ya2yaml(:syck_compatible => true)
puts data

File.open("tree.yaml", "w") { |f| f.puts data }
