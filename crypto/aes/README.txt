
KEY: 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
     0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
     0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
     0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f
IV:  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
PLAIN TEXT:   encrypt me
CIPHER TEXT:  338d2a9e28208cad84c457eb9bd91c81


$ echo -n "encrypt me" > to_encrypt
$ openssl enc -in to_encrypt -out encrypted -e -aes-256-cbc \
> -K 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f \
> -iv 0000000000000000
$ hexdump -C encrypted
00000000  33 8d 2a 9e 28 20 8c ad  84 c4 57 eb 9b d9 1c 81  |3.*.( ....W.....|
00000010
$ openssl enc -in encrypted -out plain_text -d -aes-256-cbc \
> -K 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f \
> -iv 0000000000000000
$ hexdump -C plain_text 
00000000  65 6e 63 72 79 70 74 20  6d 65                    |encrypt me|
0000000a
