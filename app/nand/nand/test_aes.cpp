#include <gtest/gtest.h>

#include <boost/tuple/tuple.hpp>
#include <iostream>

#include "AES.h"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class AESTest: public testing::Test 
{
protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
TEST_F(AESTest, AES256EncryptTestSingle)
{
    uint8_t in_key[] = {
        0x60, 0x3d, 0xeb, 0x10, 0x15, 0xca, 0x71, 0xbe,
        0x2b, 0x73, 0xae, 0xf0, 0x85, 0x7d, 0x77, 0x81,
        0x1f, 0x35, 0x2c, 0x07, 0x3b, 0x61, 0x08, 0xd7,
        0x2d, 0x98, 0x10, 0xa3, 0x09, 0x14, 0xdf, 0xf4
    };

    uint8_t in_iv[] = {
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
    };

    auto res = ByteBuffer::from_hexcode("f58c4c04d6e5f1ba779eabfb5f7bfbd6");

    auto key = ByteBuffer::from_hexcode("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4");
    for (int i=0; i<32; i++)
        ASSERT_EQ(in_key[i], key[i]);

    auto iv = ByteBuffer::from_hexcode("000102030405060708090A0B0C0D0E0F");
    for (int i=0; i<16; i++)
        ASSERT_EQ(in_iv[i], iv[i]);

    AES aes(key, AES::CBC, iv);
    auto test = ByteBuffer::from_hexcode("6bc1bee22e409f96e93d7e117393172a");
    auto ciphered = aes.encrypt(test);

    ASSERT_TRUE(ciphered == res);
}

TEST_F(AESTest, AES128EncryptTestAll)
{    
    auto key = ByteBuffer::from_hexcode("2b7e151628aed2a6abf7158809cf4f3c");
    char const* ivs[] = {
        "000102030405060708090A0B0C0D0E0F", "7649ABAC8119B246CEE98E9B12E9197D",
        "5086CB9B507219EE95DB113A917678B2", "73BED6B8E3C1743B7116E69E22229516"
    };

    char const* tests[] = {
        "6bc1bee22e409f96e93d7e117393172a", "ae2d8a571e03ac9c9eb76fac45af8e51",
        "30c81c46a35ce411e5fbc1191a0a52ef", "f69f2445df4f9b17ad2b417be66c3710"
    };

    char const* ciphered_texts[] = {
        "7649abac8119b246cee98e9b12e9197d", "5086cb9b507219ee95db113a917678b2",
        "73bed6b8e3c1743b7116e69e22229516", "3ff1caa1681fac09120eca307586e1a7"
    };

    for (int i=0; i<4; i++)
    {
        auto iv       = ByteBuffer::from_hexcode(ivs[i]);
        auto test     = ByteBuffer::from_hexcode(tests[i]);
        auto ciphered = ByteBuffer::from_hexcode(ciphered_texts[i]);

        AES aes(key, AES::CBC, iv);
        auto res = aes.encrypt(test);

        ASSERT_TRUE(ciphered == res);
    }
}

TEST_F(AESTest, AES192EncryptTestAll)
{
    auto key = ByteBuffer::from_hexcode("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b");
    char const* ivs[] = {
        "000102030405060708090A0B0C0D0E0F", "4F021DB243BC633D7178183A9FA071E8",
        "B4D9ADA9AD7DEDF4E5E738763F69145A", "571B242012FB7AE07FA9BAAC3DF102E0"
    };

    char const* tests[] = {
        "6bc1bee22e409f96e93d7e117393172a", "ae2d8a571e03ac9c9eb76fac45af8e51",
        "30c81c46a35ce411e5fbc1191a0a52ef", "f69f2445df4f9b17ad2b417be66c3710"
    };

    char const* ciphered_texts[] = {
        "4f021db243bc633d7178183a9fa071e8", "b4d9ada9ad7dedf4e5e738763f69145a",
        "571b242012fb7ae07fa9baac3df102e0", "08b0e27988598881d920a9e64f5615cd"
    };

    auto size = sizeof(ivs) / sizeof(ivs[0]);
    for (size_t i=0; i<size; i++)
    {
        auto iv       = ByteBuffer::from_hexcode(ivs[i]);
        auto test     = ByteBuffer::from_hexcode(tests[i]);
        auto ciphered = ByteBuffer::from_hexcode(ciphered_texts[i]);

        AES aes(key, AES::CBC, iv);
        auto res = aes.encrypt(test);

        ASSERT_TRUE(ciphered == res);
    }
}

TEST_F(AESTest, AES256EncryptTestAll)
{
    auto key = ByteBuffer::from_hexcode("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4");

    char const* ivs[] = {
        "000102030405060708090A0B0C0D0E0F", "F58C4C04D6E5F1BA779EABFB5F7BFBD6",
        "9CFC4E967EDB808D679F777BC6702C7D", "39F23369A9D9BACFA530E26304231461"
    };

    char const* tests[] = {
        "6bc1bee22e409f96e93d7e117393172a", "ae2d8a571e03ac9c9eb76fac45af8e51",
        "30c81c46a35ce411e5fbc1191a0a52ef", "f69f2445df4f9b17ad2b417be66c3710"
    };

    char const* ciphered_texts[] = {
        "f58c4c04d6e5f1ba779eabfb5f7bfbd6", "9cfc4e967edb808d679f777bc6702c7d",
        "39f23369a9d9bacfa530e26304231461", "b2eb05e2c39be9fcda6c19078c6a9d1b"
    };

    for (int i=0; i<4; i++)
    {
        auto iv       = ByteBuffer::from_hexcode(ivs[i]);
        auto test     = ByteBuffer::from_hexcode(tests[i]);
        auto ciphered = ByteBuffer::from_hexcode(ciphered_texts[i]);

        AES aes(key, AES::CBC, iv);
        auto res = aes.encrypt(test);

        ASSERT_TRUE(ciphered == res);
    }
}

TEST_F(AESTest, AES128DecryptTestAll)
{
    char const* cts[] = {
        "0336763e966d92595a567cc9ce537f5e", "a9a1631bf4996954ebc093957b234589",
        "ff4f8391a6a40ca5b25d23bedd44a597", "dc43be40be0e53712f7e2bf5ca707209"
    };

    char const* pts[] = {
        "f34481ec3cc627bacd5dc3fb08f273e6", "9798c4640bad75c7c3227db910174e72",
        "96ab5c2ff612d9dfaae8c31f30c42168", "6a118a874519e64e9963798a503f1d35"
    };

    auto size = sizeof(cts) / sizeof(cts[0]);
    auto key  = ByteBuffer::from_hexcode("00000000000000000000000000000000");
    auto iv   = ByteBuffer::from_hexcode("00000000000000000000000000000000");
    for (int i=0; i<size; i++)
    {
        auto plain    = ByteBuffer::from_hexcode(pts[i]);
        auto ciphered = ByteBuffer::from_hexcode(cts[i]);

        AES aes(key, AES::CBC, iv);
        auto res = aes.decrypt(ciphered);
        ASSERT_TRUE(plain == res);
    }
}

TEST_F(AESTest, AES256DecryptTestAll)
{
    char const* cts[] = {
        "5c9d844ed46f9885085e5d6a4f94c7d7", "a9ff75bd7cf6613d3731c77c3b6d0c04",
        "623a52fcea5d443e48d9181ab32c7421", "38f2c7ae10612415d27ca190d27da8b4"
    };

    char const* pts[] = {
        "014730f80ac625fe84f026c60bfd547d", "0b24af36193ce4665f2825d7b4749c98",
        "761c1fe41a18acf20d241650611d90f1", "8a560769d605868ad80d819bdba03771"
    };

    auto size = sizeof(cts) / sizeof(cts[0]);
    auto key  = ByteBuffer::from_hexcode("0000000000000000000000000000000000000000000000000000000000000000");
    auto iv   = ByteBuffer::from_hexcode("00000000000000000000000000000000");
    for (int i=0; i<size; i++)
    {
        auto plain    = ByteBuffer::from_hexcode(pts[i]);
        auto ciphered = ByteBuffer::from_hexcode(cts[i]);

        AES aes(key, AES::CBC, iv);
        auto res = aes.encrypt(plain);
        ASSERT_TRUE(ciphered == res);
    }
}

struct AESWrapTest
{
    string kek;
    string data;
    string expected_ciphertext;
};

TEST_F(AESTest, AESWrapUnWrap)
{
    string keks[] = {
        "000102030405060708090A0B0C0D0E0F", 
        "000102030405060708090A0B0C0D0E0F1011121314151617",
        "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F",
        "000102030405060708090A0B0C0D0E0F1011121314151617",
        "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F",
        "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"
    };

    string data[] = {
        "00112233445566778899AABBCCDDEEFF", 
        "00112233445566778899AABBCCDDEEFF",
        "00112233445566778899AABBCCDDEEFF", 
        "00112233445566778899AABBCCDDEEFF0001020304050607",
        "00112233445566778899AABBCCDDEEFF0001020304050607",
        "00112233445566778899AABBCCDDEEFF000102030405060708090A0B0C0D0E0F"
    };

    string ciphered_texts[] = {
        "1FA68B0A8112B447AEF34BD8FB5A7B829D3E862371D2CFE5",
        "96778B25AE6CA435F92B5B97C050AED2468AB8A17AD84E5D",
        "64E8C3F9CE0F5BA263E9777905818A2A93C8191E7D6E8AE7",
        "031D33264E15D33268F24EC260743EDCE1C6C7DDEE725A936BA814915C6762D2",
        "A8F9BC1612C68B3FF6E6F4FBE30E71E4769C8B80A32CB8958CD5D17D6B254DA1",
        "28C9F404C4B810F4CBCCB35CFB87F8263F5786E2D80ED326CBC7F0E71A99F43BFB988B9B7A02DD21"
    };

    /*
    for kek, data, expected in test_vectors:
        ciphertext = AESwrap(kek.decode("hex"), data.decode("hex"))
        assert ciphertext == expected.decode("hex")
        assert AESUnwrap(kek.decode("hex"), ciphertext) == data.decode("hex")
    print "All tests OK !"
    */

    auto const size = sizeof(keks) / sizeof(keks[0]);
    for (int i=0; i<size; i++)
    {
        auto kek      = ByteBuffer::from_hexcode(keks[i]);
        auto datum    = ByteBuffer::from_hexcode(data[i]);
        auto ciphered = ByteBuffer::from_hexcode(ciphered_texts[i]);
        AES aes(kek);
        auto res = aes.unwrap(ciphered);
        EXPECT_EQ(ByteBuffer::to_hexcode(datum), ByteBuffer::to_hexcode(res));
    }

    for (int i=0; i<size; i++)
    {
        auto kek      = ByteBuffer::from_hexcode(keks[i]);
        auto datum    = ByteBuffer::from_hexcode(data[i]);
        auto ciphered = ByteBuffer::from_hexcode(ciphered_texts[i]);
        AES aes(kek);
        auto res = aes.wrap(datum);
        EXPECT_EQ(ByteBuffer::to_hexcode(ciphered), ByteBuffer::to_hexcode(res));
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
