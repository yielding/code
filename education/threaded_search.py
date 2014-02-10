#!/usr/bin/env python

import threading
import time

class SearchThread(threading.Thread):
    def __init__(self, threadID, name, counter):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.counter = counter

    def run(self):
        print "Starting " + self.name
        threadLock.acquire()
        print_time(self.name, self.counter, 3)
        threadLock.release()

def print_time(threadName, delay, counter):
    while counter:
        time.sleep(delay)
        print "%s: %s" % (threadName, time.ctime(time.time()))
        counter -= 1

threadLock = threading.Lock()

th1 = SearchThread(1, "Thread-1", 1)
th2 = SearchThread(2, "Thread-2", 2)

th1.start()
th2.start()
threads = []
threads.append(th1)
threads.append(th2)

for t in threads:
    t.join()

print "Exiting Main Thread"
