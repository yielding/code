#!/usr/bin/env python

import pexpect

child = pexpect.spawn('ssh yielding@localhost')
child.expect('Password:')
child.sendline('alsrudk!')
child.interact()
