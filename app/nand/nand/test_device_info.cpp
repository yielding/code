#include <gtest/gtest.h>
#include <iostream>
#include "DeviceInfo.h"
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

using namespace std;

class DeviceInfoTest: public testing::Test 
{
public:
    DeviceInfoTest()
    {
      string path = "/Users/yielding/code/app/nand/nand/resource/d0686b9ba2.plist";
      m_dev_info.load(path);
    }

protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }

protected:
    DeviceInfo m_dev_info;

};

TEST_F(DeviceInfoTest, DKey)
{
    EXPECT_EQ(m_dev_info.dkey(), 
              "4fdb45c67da9266c3e7347a8214f5e48a9aecc5deceaaf7ca26c2c0f3674d7d6");
}

TEST_F(DeviceInfoTest, ECID)
{
    EXPECT_EQ(m_dev_info.ecid(), "789888271309");
}

TEST_F(DeviceInfoTest, EMF)
{
    EXPECT_EQ(m_dev_info.emf(), "d38d70e2e62d33c19253816bd255ad06ddb6e101b1b8e10329748ea97569dd8e");
}

TEST_F(DeviceInfoTest, KeyBagKeys)
{
    auto s1 = boost::trim_copy(m_dev_info.keybag_keys());
    auto s2 = 
        "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\n"
        "	Xvf0OFtegg9vdPtlQg==";
    EXPECT_EQ(s1, s2); 
}

TEST_F(DeviceInfoTest, BtMac)
{
    EXPECT_EQ(m_dev_info.bt_mac(), "28:e0:2c:41:00:36");
}

TEST_F(DeviceInfoTest, DataVolumeOffset)
{
    EXPECT_EQ(m_dev_info.data_volume_offset(), 151552);
}

TEST_F(DeviceInfoTest, DataVolumeUUID)
{
    EXPECT_EQ(m_dev_info.data_volume_uuid(), "0000000000000000");
}

TEST_F(DeviceInfoTest, HWModel)
{
    EXPECT_EQ(m_dev_info.hw_model(), "N90AP");
}

TEST_F(DeviceInfoTest, IMEI)
{
    EXPECT_EQ(m_dev_info.imei(), "012849003371735");
}

TEST_F(DeviceInfoTest, KernelArgs)
{
    // see the last space char ' '
    EXPECT_EQ(m_dev_info.kernel_boot_args(), "pio-error=0 -v rd=md0 nand-disable=1 ");
}

TEST_F(DeviceInfoTest, KeyXXX)
{
    EXPECT_EQ(m_dev_info.key835(), "0b46b3166f8e885a12b75c27ab3d24e0");
    EXPECT_EQ(m_dev_info.key899(), "25d016f10086ef953495859269119c8e");
    EXPECT_EQ(m_dev_info.key89A(), "c8b6207bd9594bce1f6f10515c8ccafe");
    EXPECT_EQ(m_dev_info.key89B(), "54170b7be360985fb7bdf6a3737e25fb");
}

TEST_F(DeviceInfoTest, wifiMac)
{
    EXPECT_EQ(m_dev_info.wifi_mac(), "28:e0:2c:41:00:37");
}

TEST_F(DeviceInfoTest, Nand)
{
    auto nand = m_dev_info.nand();
    EXPECT_EQ(nand["#block-pages"], "128");
    EXPECT_EQ(nand["#bootloader-bytes"], "1536");
    EXPECT_EQ(nand["#ce"], "4");
    EXPECT_EQ(nand["#ce-blocks"], "4100");
    EXPECT_EQ(nand["#page-bytes"], "8192");
    EXPECT_EQ(nand["#spare-bytes"], "448");
    EXPECT_EQ(nand["banks-per-ce"], "1");
    EXPECT_EQ(nand["bbt-format"], "1");
    auto s1 = boost::trim_copy(nand["boot-from-nand"]);
    EXPECT_EQ(s1, "AQAAAA==");
    EXPECT_EQ(nand["device-readid"], "848619416");
    EXPECT_EQ(nand["dumpedPageSize"], "8212");
    EXPECT_EQ(nand["meta-per-logical-page"], "12");
    auto s2 = boost::trim_copy(nand["metadata-whitening"]);
    EXPECT_EQ(s2, "AQAAAA==");
    auto s3 = boost::trim_copy(nand["name"]);
    EXPECT_EQ(s3, "ZGlzawA=");
}

TEST_F(DeviceInfoTest, NandPartition)
{
    auto nand_partitions = m_dev_info.nand_partitions();
    EXPECT_EQ(nand_partitions.size(), 1);

    auto a_fs = nand_partitions["Filesystem"];
    EXPECT_EQ(a_fs["Block Count"], "4084");
    EXPECT_EQ(a_fs["Block Offset"], "16");
}

TEST_F(DeviceInfoTest, ClassKeys)
{
    char const* ckeys[] = {
        "871ef1859af573b9d54d943dcd834d0e9057665f77ba5887b097c70a3263a4ae",
        "0737bd40c3b7c2ad9d983c1d19d0816aa24ed91441fd9bc5e349db195763f0a6",
        "3ca2fea42f271c614735e5feadcc12e700c2bed9e38d355eb4ac318000c9f477",
        "4fdb45c67da9266c3e7347a8214f5e48a9aecc5deceaaf7ca26c2c0f3674d7d6",
        "218be9428d4c736d052ade9dd6af7c0717aa319ae7453f78cdea9c539ccbf041",
        "4ea73499ee1c8ea69263c8045dde0708231f2a70fe1af92241feb975bc65764b",
        "1ab6281c6f1f4d45c4246242917e65239c111538d9481595b627241a343f6837",
        "8e65ee0954d3ecfbb087466f3b38d4f946c3a3921078cb9337b20dfd5011d7ac",
        "2182c68e3a7e1848af08eec5f4ee171fa146738f4da3d9247952cb0b238d6568",
        "7b93b93a0b03c29809d02ff513b5492b827ef7f7681bd38f0815a842c898a236",
        "7eb32a76613f99219fcb92b71a3c60a176e7547adfbde1e81d29feed22a05bae"
    };

    auto class_keys = m_dev_info.class_keys();
    EXPECT_EQ(class_keys.size(), 11);

    for (auto it=class_keys.begin(); it!=class_keys.end(); ++it)
    {
        auto cls = it->first;
        auto key = it->second;
        int index = boost::lexical_cast<int>(cls);
        EXPECT_EQ(ckeys[index-1], key);
    }
}
