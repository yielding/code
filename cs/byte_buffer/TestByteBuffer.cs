using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;

using MD.Utility;

namespace TestMDUExtractor
{
    [TestClass]
    public class TestByteBuffer
    {
        [TestInitialize()]
        public void SetUp()
        {
        }

        [TestCleanup()]
        public void TearDown()
        {
        }

        [TestMethod]
        public void TestDefaultConstruct()
        {
            var bb = new ByteBuffer();
            Assert.AreEqual<int>((int)bb.Size, 0);
            Assert.AreEqual<int>((int)bb.Offset, 0);
            Assert.IsTrue(bb.Empty());
        }

        [TestMethod]
        public void Test10KBuffer()
        {
            int sz = 1024 * 10;
            var bb = new ByteBuffer(sz);
            Assert.AreEqual(bb.Size, 0);
            Assert.AreEqual(bb.Capacity, sz);
            Assert.AreEqual(bb.hasRemaining(), false);
        }

        [TestMethod]
        [ExpectedException(typeof(ArgumentOutOfRangeException), "Message What?")]
        public void TestInvalidAccesstt()
        {
            var lb = new List<byte>();
            lb[10] = 10;
        }

        [TestMethod]
        public void TestListOfByte()
        {
            var l0 = new List<byte>();
            l0.Add(10);
            Assert.AreEqual(l0[0], 10);

            var b1 = new byte[] { 1, 2, 3, 4, 5 };
            var l1 = new List<byte>(b1);
            for (int i=0; i<b1.Length; i++)
                Assert.AreEqual<byte>(b1[i], l1[i]);

            Assert.AreEqual<int>(b1.Length, l1.Count);
        }
    }
}