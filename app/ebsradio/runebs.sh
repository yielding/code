#!/opt/local/bin/bash

day=$(LC_ALL=C /bin/date +%d)
year=$(LC_ALL=C /bin/date +%Y)
month=$(LC_ALL=C /bin/date +%b)
date=$(LC_ALL=C /bin/date +%Y-%m-%d-%a)
program=$1
duration=$2

filename="/Users/yielding/ebsradio/"$program$date".mp3"

###################################################################
# Record streaming audio
###################################################################
/Users/yielding/code/app/ebsradio/ebs ${filename} ${duration}

sleep 1

###################################################################
# Write ID3v2 tag
#   Title : Program Name Date (ig. PowerEnglish 2010-07-12-Mon)
#   Artist : EBS
#   Album : Program Name Year Month (ig. PowerEnglish 2010 Jul)
#   Genre : Podcast
#   Year : Year (ig. 2010)
#   Track Number : Day (ig. 12)
#
# Note
#   tagwriter is an example program from tagLib.
#   tagLib's website is 
#       http://developer.kde.org/~wheeler/taglib.html
###################################################################
/Users/yielding/code/app/ebsradio/tagwriter -t "$program $date" -a "EBS" -A "$program $year $month" -g "Podcast" -y "$year" -T "$day" $filename

mv $filename /Users/yielding/Dropbox/ebs
