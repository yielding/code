using System;
using System.Collections.Generic;
using System.Text;

namespace MD.Utility
{
    public class ByteBuffer
    {
        private uint offset;
        private List<byte> data = null;

        public ByteBuffer() : this(0) { }

        public ByteBuffer(int capacity)
        {
            this.data   = new List<byte>(capacity);
            this.offset = 0;
        }

        public ByteBuffer(byte[] buffer)
        {
            this.data   = new List<byte>(buffer.Length);
            this.offset = 0;
        }

        public bool Empty()
        {
            return this.data.Count == 0;
        }

        public bool hasRemaining()
        {
            return this.data.Count > offset;
        }

        public uint Offset
        {
            get { return offset;  }
            set { offset = value; }
        }

        public int Capacity
        {
            get { return data.Capacity; }
        }

        public int Size
        {
            get { return data.Count;  }
        }
    }
}
