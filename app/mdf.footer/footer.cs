using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Collections.Generic;

using MD.Parameters;
using MD.ExtractType;
using MD.Utility;
using MD.Hash;

namespace MDUExtractor
{
    public class MDFFooter
    {
        private int    version;
        private string organization = "";
        private string department = "";
        private string person = "";
        private string maker = "";
        private string model = "";
        private string platform = "";
        private DateTime startTime;
        private DateTime endTime;
        // private ExtractTypes type;
        private string type = "";
        private Dictionary<HashTypes, byte[]> hash;
        //private Dictionary<string, byte[]> hash;
        private Int64  size;
        private bool isCompressed = false;
        private bool hasPasswd = false;
        private bool hasCheckSum = false;
        private string magicNo = "";
        private string extractOption = "";

        public MDFFooter()
        {
            hash = new Dictionary<HashTypes, byte[]>();
        }

        public int Version
        {
            get { return version;  }
            set { version = value; }
        }

        public string Organization
        {
            get { return organization;  }
            set { organization = value; }
        }

        public string Department
        {
            get { return department;  }
            set { department = value; }
        }

        public string Person
        {
            get { return person; }
            set { person = value; }
        }
        
        public string Maker
        {
            get { return maker;  }
            set { maker = value; }
        }

        public string Model
        {
            get { return model;  }
            set { model = value; }
        }

        public string Platform
        {
            get { return platform; }
            set { platform = value; }
        }

        public DateTime StartTime
        {
            get { return startTime;  }
            set { startTime = value; }
        }

        public DateTime EndTime
        {
            get { return endTime;  }
            set { endTime = value; }
        }

        public Dictionary<HashTypes, byte[]> Hash
        {
            get { return hash; }
        }

        public string Type
        {
            get { return type;  }
            set { type = value; }
        }

        public Int64 Size
        {
            get { return size; }
            set { size = value; }
        }

        public bool Compressed
        {
            get { return isCompressed; }
            set { isCompressed = value; }
        }

        public bool HasPassword
        {
            get { return hasPasswd; }
            set { hasPasswd = value; }
        }

        public bool HasCheckSum
        {
            get { return hasCheckSum; }
            set { hasCheckSum = value; }
        }

        public string ExtractOption
        {
            get { return extractOption; }
            set { extractOption = value; }
        }

        public bool WriteTo(string path)
        {
            var pageSize = (int)Parameters.PageSize;
            var limit    = pageSize * 2;
            var buffer   = new ByteBuffer(limit);
            buffer.SetAscii("ORG").SetUnicode (this.organization)
                  .SetAscii("DPT").SetUnicode (this.department) 
                  .SetAscii("PRS").SetUnicode (this.person) 
                  .SetAscii("MKR").SetUnicode (this.maker) 
                  .SetAscii("MDL").SetUnicode (this.model) 
                  .SetAscii("PLF").SetUnicode (this.platform) 
                  .SetAscii("SDT").SetSystemTime(this.startTime) 
                  .SetAscii("EDT").SetSystemTime(this.endTime);

            foreach (var item in hash)
            {
                var name = HashType2Name(item.Key);
                buffer.SetAscii(name).SetBytes(item.Value);
            }

            buffer.SetAscii("TYP").SetUnicode(this.type)
                  .SetAscii("AMT").SetInt64LE(size)
                  .SetAscii("ENC").SetByte(this.isCompressed)
                  .SetAscii("PWD").SetByte(this.hasPasswd)
                  .SetAscii("CHK").SetByte(this.hasCheckSum)
                  .SetAscii("EXO").SetUnicode(this.extractOption)
                  .SetAscii("ECPV5    ");

            this.version = 5;
            if (buffer.Offset() >= limit)
                throw new OverflowException("메타 정보의 크기가 올바르지 않습니다.");

            using (var fs = new FileStream(path, FileMode.Append, FileAccess.Write))
            {
                var sz = fs.Seek(0, SeekOrigin.End);

                /// Write null padding
                var padSize = isCompressed ? (int)((pageSize - (sz % pageSize)) % pageSize)
                                           : (int)((pageSize - (this.size % pageSize)) % pageSize);
                var padding = Enumerable.Repeat((byte)1, padSize).ToArray();
                if (padSize > 0)
                    fs.Write(padding, 0, padSize);

                // Write actual data
                fs.Write(buffer.ToArray(), 0, buffer.Offset());
            }

            return true;
        }

        public int GetImageVersion(string path)
        {
            int res = 0;
            if (!File.Exists(path))
                return res;

            var fi = new FileInfo(path);
            if (fi.Length < 10)
                return res;

            var fs = new FileStream(path, FileMode.Open, FileAccess.Read);
            fs.Seek(-10, SeekOrigin.End);
            using (var br = new BinaryReader(fs))
            {
                var buffer = new byte[10];

                br.Read(buffer, 0, 10);
                var verStr = Encoding.ASCII.GetString(buffer);
                if (verStr.StartsWith("ECPLATFORM")) return 1;
                if (verStr.StartsWith("ECPV2")) return 2;
                if (verStr.StartsWith("ECPV3")) return 3;
                if (verStr.StartsWith("ECPV4")) return 4;
                if (verStr.StartsWith("ECPV5")) return 5;
                if (verStr.StartsWith("ECPV6")) return 6;
            }

            return res;
        }

        public bool FormatV1(string path) 
        { 
            return false; 
        }

        public bool FormatV2(string path) 
        { 
            return false; 
        }
        
        public bool FormatV3(string path) 
        {
            var fileSize = new FileInfo(path).Length;
            var metaSize = fileSize % (int)Parameters.PageSize;

            var buffer = new byte[(int)Parameters.PageSize * 2];
            var fs = new FileStream(path, FileMode.Open, FileAccess.Read);
            for (int i = 0; i < 10; i++)
            {
                fs.Seek(fileSize - metaSize, SeekOrigin.Begin);
                fs.Read(buffer, 0, (int)metaSize);
                var start = BitConverter.ToUInt32(buffer, 0);
                if (start == 0x47524f)
                    break;

                metaSize += (int)Parameters.PageSize;
            }

            var bb = new ByteBuffer(buffer);

            while (bb.Offset() < metaSize)
            {
                var name = bb.GetAscii();

                if (name == "ORG")
                {
                    organization = bb.GetUnicode();
                }
                else if (name == "DPT")
                {
                    department = bb.GetUnicode();
                }
                else if (name == "PRS")
                {
                    person = bb.GetUnicode();
                }
                else if (name == "MKR")
                {
                    maker = bb.GetUnicode();
                }
                else if (name == "MDL")
                {
                    model = bb.GetUnicode();
                }
                else if (name == "PLF")
                {
                    platform = bb.GetUnicode();
                }
                else if (name == "SDT")
                {
                    startTime = bb.GetSystemTime();
                }
                else if (name == "EDT")
                {
                    endTime = bb.GetSystemTime();
                }
                else if (name == "MD5")
                {
                    hash[HashTypes.MD5] = parseHash(bb, name);
                }
                else if (name == "RIPEMD128")
                {
                    hash[HashTypes.RIPEMD_128] = parseHash(bb, name);
                }
                else if (name == "RIPEMD160")
                {
                    hash[HashTypes.RIPEMD_160] = parseHash(bb, name);
                }
                else if (name == "RIPEMD256")
                {
                    hash[HashTypes.RIPEMD_256] = parseHash(bb, name);
                }
                else if (name == "RIPEMD320")
                {
                    hash[HashTypes.RIPEMD_320] = parseHash(bb, name);
                }
                else if (name == "SHA1")
                {
                    hash[HashTypes.SHA_1] = parseHash(bb, name);
                }
                else if (name == "SHA224")
                {
                    hash[HashTypes.SHA_224] = parseHash(bb, name);
                }
                else if (name == "SHA256")
                {
                    hash[HashTypes.SHA_256] = parseHash(bb, name);
                }
                else if (name == "SHA384")
                {
                    hash[HashTypes.SHA_384] = parseHash(bb, name);
                }
                else if (name == "SHA512")
                {
                    hash[HashTypes.SHA_512] = parseHash(bb, name);
                }
                else if (name == "AMT")
                {
                    this.size = bb.GetInt64LE();
                }
                else if (name == "ENC")
                {
                    this.isCompressed = (bb.GetByte() == 1);
                }
                else if (name == "PWD")
                {
                    this.hasPasswd = (bb.GetByte() == 1);
                }
                else if (name == "CHK")
                {
                    this.hasCheckSum = (bb.GetByte() == 1);
                }
                else if (name == "TYP")
                {
                    // REMARK : left something to do.
                    // this.type = parseExtractType(bb);
                    this.type = bb.GetUnicode();
                }
                else if (name.StartsWith("ECP"))
                {
                    this.magicNo = name.TrimEnd(' ');
                }
            }

            fs.Close();

            return true; 
        }

        public bool ReadFrom(string path)
        {
            this.version = GetImageVersion(path);
            switch (version)
            {
                case 1: return FormatV1(path);
                case 2:
                case 3:
                case 4: return FormatV2(path);
                case 5: return FormatV3(path);
            }

            return false;
        }

        // 
        // TODO 1) Refactoring!
        //      2) Test bizarre size!
        // 
        private byte[] parseHash(ByteBuffer buffer, string key)
        {
            var dict = new Dictionary<string, int> () {
                { "MD5",       16 }, { "RIPEMD128", 16 }, { "RIPEMD160", 20 }, { "RIPEMD256", 32 },
                { "RIPEMD320", 40 }, { "SHA1",      20 }, { "SHA224",    28 }, { "SHA256",    32 },
                { "SHA384",    48 }, { "SHA512",    64 }
            };

            var size = dict[key];
            return buffer.GetBytes(size);
        }

        static public string HashType2Name(HashTypes type)
        {
            var dict = new Dictionary<HashTypes, string>() {
                { HashTypes.MD5, "MD5" },      
                { HashTypes.RIPEMD_128, "RIPEMD128" }, 
                { HashTypes.RIPEMD_160, "RIPEMD160" }, 
                { HashTypes.RIPEMD_256, "RIPEMD256" },
                { HashTypes.RIPEMD_320, "RIPEMD320" }, 
                { HashTypes.SHA_1  , "SHA1" }, 
                { HashTypes.SHA_224, "SHA224" }, 
                { HashTypes.SHA_256, "SHA256" },
                { HashTypes.SHA_384, "SHA384" }, 
                { HashTypes.SHA_512, "SHA512" }
            };

            return dict[type];
        }

        private ExtractTypes parseExtractType(ByteBuffer buffer)
        {
            var dict = new Dictionary<string, ExtractTypes> () {
                { "DM" , ExtractTypes.DM  }, { "DL" , ExtractTypes.DL  }, { "JT" , ExtractTypes.JT  },
                { "MR" , ExtractTypes.MR  }, { "US" , ExtractTypes.US  }, { "WL" , ExtractTypes.WL  },
                { "WP" , ExtractTypes.WP  }, { "IP" , ExtractTypes.IP  }, { "SD" , ExtractTypes.SD  },
                { "SDE", ExtractTypes.SDE }, { "SDI", ExtractTypes.SDI }, { "MO" , ExtractTypes.MO  },
                { "LV" , ExtractTypes.LV  }
            };

            var key = buffer.GetUnicode();
            return dict[key];
        }
    }
}
