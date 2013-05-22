from Crypto.Cipher import AES
from base64 import b64encode, b64decode
import os
from datetime import datetime
from re import sub

# AES is a block cipher so you need to define size of block.
# Valid options are 16, 24, and 32
BLOCK_SIZE = 32

# Your input has to fit into a block of BLOCK_SIZE.
# To make sure the last block to encrypt fits
# in the block, you may need to pad the input.
# This padding must later be removed after decryption so a standard padding would help.
# Based on advice from Using Padding in Encryption,
# the idea is to separate the padding into two concerns: interrupt and then pad
# First you insert an interrupt character and then a padding character
# On decryption, first you remove the padding character until 
# you reach the interrupt character
# and then you remove the interrupt character
INTERRUPT = u'\u0001'
PAD = u'\u0000'

# Since you need to pad your data before encryption, 
# create a padding function as well
# Similarly, create a function to strip off the padding after decryption
def AddPadding(data, interrupt, pad, block_size):
    new_data = ''.join([data, interrupt])
    new_data_len = len(new_data)
    remaining_len = block_size - new_data_len
    to_pad_len = remaining_len % block_size
    pad_string = pad * to_pad_len
    return ''.join([new_data, pad_string])
def StripPadding(data, interrupt, pad):
    return data.rstrip(pad).rstrip(interrupt)

# AES requires a shared key, which is used to encrypt and decrypt data
# It MUST be of length 16, 24, or 32
# Make sure it is as random as possible 
# (although the example below is certainly not random)
# Based on comments from lighthill,
# you should use os.urandom() or Crypto.Random to generate random secret key
# I also use the GRC Ultra High Security Password Generator to generate a secret key
SECRET_KEY = u'a1b2c3d4e5f6g7h8a1b2c3d4e5f6g7h8'

# Initialization Vector (IV) should also always be provided
# With the same key but different IV, the same data is encrypted differently
# IV is similar to a 'salt' used in hashing
# It MUST be of length 16
# Based on comments from lighthill,
# you should NEVER use the same IV if you use MODE_OFB
# In any case, especially if you are encrypting, say data to be store in a database,
# you should try to use a different IV for different data sets,
# even if you use the same secret key
IV = u'12345678abcdefgh'

# Now you must choose a 'mode'. Options are available from Module AES.
# Although the default is MODE_ECB, it's highly recommended not to use it.
# For more information on different modes, read Block cipher modes of operation.
# In this example, I had used MODE_OFB
# But based on comments from lighthill,
# I switched over to MODE_CBC, which seems quite popular

# Let's create our cipher objects
cipher_for_encryption = AES.new(SECRET_KEY, AES.MODE_OFB, IV)
cipher_for_decryption = AES.new(SECRET_KEY, AES.MODE_OFB, IV)
cipher_for_encryption = AES.new(SECRET_KEY, AES.MODE_CBC, IV)
cipher_for_decryption = AES.new(SECRET_KEY, AES.MODE_CBC, IV)

# So you now have cipher objects
# Each operation that you perform on these objects alters its state
# So mostly you would want to perform a single operation on it each time
# For encrypting something, create a cipher object and encrypt the data
# For decrypting, create another cipher object and pass it the data to be decrypted
# This is the reason I called the cipher objects 
# 'cipher_for_encryption' and 'cipher_for_decryption'
#
#
#
# You will want to create encryption and decryption functions 
# so that it's easier to encrypt and decrypt data
def EncryptWithAES(encrypt_cipher, plaintext_data):
    plaintext_padded = AddPadding(plaintext_data, INTERRUPT, PAD, BLOCK_SIZE)
    encrypted = encrypt_cipher.encrypt(plaintext_padded)
    return b64encode(encrypted)
def DecryptWithAES(decrypt_cipher, encrypted_data):
    decoded_encrypted_data = b64decode(encrypted_data)
    decrypted_data = decrypt_cipher.decrypt(decoded_encrypted_data)
    return StripPadding(decrypted_data, INTERRUPT, PAD)

# We are now ready to encrypt and decrypt our data
our_data_to_encrypt = u'123456789012345678901234567890abc'
encrypted_data = EncryptWithAES(cipher_for_encryption, our_data_to_encrypt)
print ('Encrypted string:', encrypted_data)

# And let's decrypt our data
decrypted_data = DecryptWithAES(cipher_for_decryption, encrypted_data)
print ('Decrypted string:', decrypted_data)

