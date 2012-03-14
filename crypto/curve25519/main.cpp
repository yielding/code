#include "curve25519-donna.h"
#include "ByteBuffer.h"

#include <openssl/aes.h>
#include <openssl/sha.h>

#include <iostream>
#include <vector>

using namespace std;
using namespace utility::hex;

void curve25519()
{
}

int main(int argc, char const* argv[])
{
    auto z           = ByteBuffer::from_hexcode("04000000080000000200000048000000000000000000000000000000000000000000000002917dc2542198edeb1078c4d1ebab74d9ca87890657ba02b9825dadf20a002f44360c6f87743fac0236df1f9eedbea801e31677aef3a09adfb4e10a37ae27facf419ab3ea3f39f4");
    auto my_secret_  = ByteBuffer::from_hexcode("99b66345829d8c05041eea1ba1ed5b2984c3e5ec7a756ef053473c7f22b49f14").get_buffer();
    auto my_public_  = ByteBuffer::from_hexcode("b1c652786697a5feef36a56f36fde524a21193f4e563627977ab515f600fdb3a").get_buffer();
    auto his_public_ = z.slice(36, 36+32);

    cout << ByteBuffer::to_hexcode(his_public_.get_buffer()) << endl;

    uint8_t* my_secret  = &my_secret_[0];
    uint8_t* his_public = his_public_;
    uint8_t* my_public  = &my_public_[0];

    vector<uint8_t> shared(32, 0);

    my_secret[ 0] &= 248;
    my_secret[31] &= 127;
    my_secret[31] |=  64;
    curve25519_donna(&shared[0], my_secret, his_public);

    cout << ByteBuffer::to_hexcode(shared) << endl;

    uint8_t arr[] = { 0, 0, 0, 1 };

    vector<uint8_t> result(32, 0);

    SHA256_CTX ctx;
    SHA256_Init(&ctx);
    SHA256_Update(&ctx, arr, 4);
    SHA256_Update(&ctx, &shared[0], 32);
    SHA256_Update(&ctx, his_public, 32);
    SHA256_Update(&ctx, my_public,  32);
    SHA256_Final(&result[0], &ctx);
    cout << ByteBuffer::to_hexcode(result) << endl;

    AES_KEY aes_key;
    AES_set_decrypt_key(&result[0], 32*8, &aes_key);

    auto wrapped = (uint8_t*)z.slice(36+32, 36+32+40);
    vector<uint8_t> final(32, 0);
    AES_unwrap_key(&aes_key, NULL, &final[0], wrapped, 40);
    cout << ByteBuffer::to_hexcode(final) << endl;

    return 0;
}
