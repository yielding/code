#!/usr/bin/env ruby
# coding: UTF-8

require 'numo/narray'
require 'numo/gnuplot'

# データの作成
x  = Numo::DFloat.new(60).seq(0, 0.1) # 0から6まで0.1刻みで生成
y1 = Numo::DFloat::Math.sin(x)
y2 = Numo::DFloat::Math.cos(x)

# 数値データを確認用に出力
puts "x: #{x.to_a.join(' ')}"
puts "y1: #{y1.to_a.join(' ')}"
puts "y2: #{y2.to_a.join(' ')}"

# グラフの描画
g = Numo::gnuplot do
  set term: {png: {size: [640, 480]}} # 画像サイズ
  set output: 'ruby_graph.png'
  set title: 'sin \& cos' # タイトル
  set key: 'box left bottom'
  set offset: [0, 0, 0, 0]
  plot x, y1, {w: 'lines', lw: 3, title: 'sin'},
       x, y2, {w: 'lines', lw: 3, title: 'cos'}
end
