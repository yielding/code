#!/usr/bin/env python

from twisted.internet.protocol import Protocol, Factory
from twisted.internet          import reactor

class Echo(Protocol):
  def dataReceived(self, data):
      self.transport.write(data)

def main():
    f = Factory()
    f.protocol = Echo
    reactor.listenTCP(8000, f)
    reactor.run()

if __name__ == '__main__':
    main()
