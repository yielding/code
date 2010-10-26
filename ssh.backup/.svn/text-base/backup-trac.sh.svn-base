#!/usr/local/bin/bash

#BACKUP_DATE=`date +%Y%m%d%k%M`
BACKUP_DATE=`date +%Y%m%d`
BACKUP_FILE="trac$BACKUP_DATE.tar.gz"
tar cvzf - /var/trac/panther | ssh yielding@mac "cat >> backup/$BACKUP_FILE"
cat /usr/local/etc/apache21/httpd.conf | ssh yielding@mac "cat >> backup/httpd.conf"
