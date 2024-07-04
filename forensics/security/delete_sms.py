#!/usr/bin/env python

from sqlite3 import dbapi2 as sqlite

def message_read(flags):
  """reimplementation of an sqlite user defined function called by a trigger
  on the messages table.
  the trigger checks the message flags to see if a message has been read to
  see if the counter of unread messages in another needs to be updated when
  a message is deleted.
  """
  # second bit is the "was read" flag
  return (int(flags) & 0x02) >> 1

db = sqlite.connect('3d0d7e5fb2ce288813306e4d4636395e047a3d28')
# register the user-defined function used by delete trigger
db.create_function('read', 1, message_read)

c = db.cursor()
c.execute('update message set text="Test12345";')
db.commit()
