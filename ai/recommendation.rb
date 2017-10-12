#!/usr/bin/env ruby

require 'pp'

class Array
  def sum
    inject(nil) { |s, e| s ? s + e : e }
  end
end

critics = {
  'Lisa Rose' => {
     'Lady in the Water'  => 2.5, 
     'Snakes on a Plane'  => 3.5,
     'Just My Luck'       => 3.0, 
     'Superman Returns'   => 3.5, 
     'You, Me and Dupree' => 2.5, 
     'The Night Listener' => 3.0 },
  'Gene Seymour' => {
     'Lady in the Water'  => 3.0, 
     'Snakes on a Plane'  => 3.5, 
     'Just My Luck'       => 1.5, 
     'Superman Returns'   => 5.0, 
     'The Night Listener' => 3.0, 
     'You, Me and Dupree' => 3.5 }, 
   'Michael Phillips' => {
     'Lady in the Water'  => 2.5, 
     'Snakes on a Plane'  => 3.0, 
     'Superman Returns'   => 3.5, 
     'The Night Listener' => 4.0 },
   'Claudia Puig' => {
     'Snakes on a Plane'  => 3.5, 
     'Just My Luck'       => 3.0,
     'The Night Listener' => 4.5, 
     'Superman Returns'   => 4.0, 
     'You, Me and Dupree' => 2.5 },
   'Mick LaSalle' => {
     'Lady in the Water'  => 3.0, 
     'Snakes on a Plane'  => 4.0, 
     'Just My Luck'       => 2.0, 
     'Superman Returns'   => 3.0, 
     'The Night Listener' => 3.0, 
     'You, Me and Dupree' => 2.0 }, 
   'Jack Matthews' => {
     'Lady in the Water'  => 3.0, 
     'Snakes on a Plane'  => 4.0, 
     'The Night Listener' => 3.0, 
     'Superman Returns'   => 5.0, 
     'You, Me and Dupree' => 3.5 },
   'Toby' => {
     'Snakes on a Plane'  => 4.5,
     'You, Me and Dupree' => 1.0,
     'Superman Returns'   => 4.0 }
}

def sim_pearson(prefs, p1, p2)
  si = {}
  prefs[p1].each_key { |key| si[key]=1 if prefs[p2].has_key?(key) }
  n = si.length
  return 0 if si.length == 0
  
  sum1   = si.keys.map {|it| prefs[p1][it] }.sum
  sum2   = si.keys.map {|it| prefs[p2][it] }.sum
  sum1sq = si.keys.map {|it| prefs[p1][it] ** 2 }.sum
  sum2sq = si.keys.map {|it| prefs[p2][it] ** 2 }.sum

  sum_all = si.keys.map {|it| prefs[p1][it]*prefs[p2][it]}.sum
  num = sum_all - (sum1 * sum2/n)
  den = Math.sqrt((sum1sq-sum1**2/n) * (sum2sq-sum2**2/n))

  return 0 if den == 0
  return num / den
end

puts sim_pearson(critics, 'Lisa Rose', 'Gene Seymour')
