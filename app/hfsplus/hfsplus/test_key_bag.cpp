#include "key_bag.h"
#include "Base64.h"

#include <algorithm>
#include <iostream>
#include <string>

#include <gtest/gtest.h>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
    string key_bag_data = 
        "REFUQQAABORWRVJTAAAABAAAAANUWVBFAAAABAAAAABVVUlEAAAA"
        "EKthNJkMg0bzknnn9XQq2tJITUNLAAAAKEOQnk7LbBJd5dPWSkrI"
        "SkI8fdbM/e0ksB3vY1CVnZc2AxXUcAaasvFXUkFQAAAABAAAAAFT"
        "QUxUAAAAFJhg4pjyCGodKU/XsvoXleEC8BZFSVRFUgAAAAQAAMNQ"
        "VVVJRAAAABDvoDSML5xGMYDrDuacQLbTQ0xBUwAAAAQAAAALV1JB"
        "UAAAAAQAAAABS1RZUAAAAAQAAAAAV1BLWQAAACBmc9f3BkjZ+4Xd"
        "N2YAO4eoqFwVXkTHMOWKWbJZT7/IFlVVSUQAAAAQuGoKNDiZSN+r"
        "MDXWWSreIUNMQVMAAAAEAAAACldSQVAAAAAEAAAAA0tUWVAAAAAE"
        "AAAAAFdQS1kAAAAopdQCBtT16H0KquGq1shqmrHlYbFRoAt8oqwY"
        "KNTq9mTzN726dR5qRFVVSUQAAAAQA6ARrwhSSySLyzZ1ZcFKX0NM"
        "QVMAAAAEAAAACVdSQVAAAAAEAAAAA0tUWVAAAAAEAAAAAFdQS1kA"
        "AAAotdiBFxLfpzWBAFI2etmglyGiMhp1mUyDgNtEAg6blL23Dklm"
        "h1uF4FVVSUQAAAAQWAROO/AyR4G6t8QBbr/UIUNMQVMAAAAEAAAA"
        "CFdSQVAAAAAEAAAAAUtUWVAAAAAEAAAAAFdQS1kAAAAgoZhfSrN1"
        "wREFA8koe/4Rey9ziMU8tDn2XAgRWy6dhbhVVUlEAAAAEC+mOBAO"
        "skj4jJiUy5DQMsZDTEFTAAAABAAAAAdXUkFQAAAABAAAAANLVFlQ"
        "AAAABAAAAABXUEtZAAAAKGKwbYaYeaDMlHgrRR8tr27OA39mc3KK"
        "5HAPpGEzGvWrMLz484x0BKtVVUlEAAAAEIhcEBUipkBKr3aMtHNd"
        "t9lDTEFTAAAABAAAAAZXUkFQAAAABAAAAANLVFlQAAAABAAAAABX"
        "UEtZAAAAKGXBqpWwN+Z2FWmA3AwfgXbeWbBKpJ14QqNYywFSqoTG"
        "aXIQtT7P7fFVVUlEAAAAEA8HdVtC2km8tClhFxSOemVDTEFTAAAA"
        "BAAAAAVXUkFQAAAABAAAAANLVFlQAAAABAAAAABXUEtZAAAAKM5O"
        "GljkjBUwZXd6i0dkQoxSOc7S6e1mqavQmh2JblDiIVUWFFqGWnRV"
        "VUlEAAAAEGe30ZXJOEZ3sRuwVZU3aFVDTEFTAAAABAAAAANXUkFQ"
        "AAAABAAAAANLVFlQAAAABAAAAABXUEtZAAAAKBARP5iu3bcvKDdi"
        "pEBgDbmocN8pWbskuP5U1jgqRRXpWOIqzTP1edlVVUlEAAAAEBEf"
        "o9kCm0mVmxb71ODxkgVDTEFTAAAABAAAAAJXUkFQAAAABAAAAANL"
        "VFlQAAAABAAAAAFXUEtZAAAAKNDJETPoCNuIEG9EDs4QreBqVUZe"
        "sSZxMUSECNwAp7jgiqWbrxlmqXtQQktZAAAAIFisefTsm+mc0H5m"
        "6xWJvcpe9/BJQKpSXM6MfJJRT+xzVVVJRAAAABAGga/uDCJNPaeq"
        "szSVFm46Q0xBUwAAAAQAAAABV1JBUAAAAAQAAAADS1RZUAAAAAQA"
        "AAAAV1BLWQAAACj7uLlyRmWi8133d8BQ3QcN5hMS8zPwVzuUbbrD"
        "fdIXZgrp1SSYmiSFU0lHTgAAABQpkGMJYKBEFfL5nUDsELUoiXV2"
        "hg==";

    string key835 = 
        "0b46b3166f8e885a12b75c27ab3d24e0";
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class KegBagTest: public testing::Test
{
protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }
};

TEST_F(KegBagTest, True)
{
    EXPECT_EQ(1, 1);
}

TEST_F(KegBagTest, ParseBinaryBlob)
{
    auto data = utility::codec::base64::decode(key_bag_data);
    ByteBuffer buffer(data);

    KeyBag k(buffer);

    EXPECT_TRUE(k.init_ok());

    // auto res = tlv_to_map(buffer);

    // for (auto it=res.begin(); it!= res.end(); ++it)
    // {
    //   // cout << it->first << " : " << it->second << endl;
    //   cout << it->first << endl;
    // }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
