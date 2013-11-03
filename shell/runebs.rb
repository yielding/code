#!/usr/bin/env ruby

RADIO_ADDR = "rtmp://ebsandroid.nefficient.com/fmradiofamilypc/familypc1m"
RADIO_NAME = "ebs_radio"

PROGRAM_NAME  = ARGV[0].chomp
RECORD_MINS   = ARGV[1].chomp
DEST_DIR      = ARGV[2].chomp

REC_DATE      = `date +%Y-%m-%d`.chomp
TEMP_ASX      = `mktemp -u -t ebs`.chomp
TEMP_WAV      = `mktemp -u -t ebs`.chomp + '.wav'
MP3_FILE_NAME = "#{DEST_DIR}/#{PROGRAM_NAME}-#{REC_DATE}.mp3"

ID3_TITLE     = "#{REC_DATE}_#{PROGRAM_NAME}"
ID3_ARTIST    = RADIO_NAME
ID3_ALBUM     = PROGRAM_NAME
ID3_YEAR      = `date +%Y`.chomp

DEBUG = false

dump = "/opt/local/bin/rtmpdump -r #{RADIO_ADDR} -o #{TEMP_ASX.chomp}"
puts dump if DEBUG

#pid = fork { `#{dump}` }
pid = fork { exec("#{dump}") }
puts "before wait pid = #{pid}"
sleep(RECORD_MINS.to_i * 60)
Process.kill("KILL", pid)
Process.waitall

puts""
puts "after wait and kill"

wave = "/opt/local/bin/ffmpeg -i #{TEMP_ASX.chomp} -acodec pcm_s16le -ac 2 -ab 128k -vn -y #{TEMP_WAV.chomp}"
puts wave if DEBUG
`#{wave}`

mp3 = "/opt/local/bin/lame --preset voice --tt #{ID3_TITLE} --ta #{ID3_ARTIST} --tl #{ID3_ALBUM} --ty #{ID3_YEAR} #{TEMP_WAV} #{MP3_FILE_NAME} &> /dev/null 2>&1"
puts mp3 if DEBUG
`#{mp3}`

tmp1 = "rm #{TEMP_WAV}"
puts tmp1 if DEBUG
`#{tmp1}`

tmp2 = "rm #{TEMP_ASX}"
`#{tmp2}`
