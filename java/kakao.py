from socket import *
from struct import *

class KakaoClient:
    def __init__(self, ip="127.0.0.1"):
        self.s = socket(AF_INET, SOCK_STREAM)
        self.s.connect((ip, 7781))

    def ping(self):
        packet = pack("B", 1) + pack(">I", 5) + "MDSBC"
        self.s.send(packet)
        return self.s.recv(6)[1:] == "MDSBC"

    def decrypt(self, ciphered, salt):
        packet = pack("B", 2) + pack(">I", len(ciphered)) + ciphered + pack(">I", len(salt)) + salt
        self.s.send(packet)
        self.s.recv(1)
        l, = unpack(">I", self.s.recv(4))
        return self.s.recv(l)

    def kill_server(self):
        packet = pack("B", 0)
        self.s.send(packet)

""" ping
kakao = KakaoClient()
print kakao.ping()
"""

""" decrypt
kakao = KakaoClient()
ciphered = "m+oavcl6PVEo1RBcCFlKSQ=="
salt     = "23303370"
print kakao.decrypt(ciphered, salt)
"""

""" terminate
kakao = KakaoClient()
kakao.kill_server()
"""
