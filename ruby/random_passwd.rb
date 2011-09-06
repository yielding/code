#!/usr/bin/env ruby
#
require "digest"

def gen_password length=8
  (1..length).map { (32 + rand(95)).chr }.join
end

# brute-force search
def guess_password digest, limit=8
  letters  = (' '..'~').collect(&:chr)
  password = nil
  matched  = false

  (1..limit).each do |r|
    puts "guess #{r} level"
    # 중복 순열
    letters.repeated_permutation(r).each do |a|
      if Digest::MD5.hexdigest(a.join) == digest
        matched = true
        password = a.join
        break
      end
    end

    break if matched
  end
  return password
end

password1 = gen_password 4
puts "generated password: \"#{password1}\""

digest = Digest::MD5.hexdigest password1
password2 = guess_password digest

puts "found password: \"#{password2}\""
