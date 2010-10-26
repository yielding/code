#!/usr/local/bin/python
from ftplib import FTP

print '---connecting..'
ftp = FTP('www.joonim.or.kr')

print '--login..'
ftp.login('joonim', 'joonim!')

print '--list..'
ftp.retrlines('LIST')

print '--get dirt..'
print ftp.dir()

print '--quit'
ftp.quit()


print '---finished--'
