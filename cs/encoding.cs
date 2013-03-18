using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Encoding
{
    class Program
    {
        static void Main(string[] args)
        {
            string s = "김희선";
            Console.WriteLine("원본문자열 : {0}", s);

            // 코드페이지 번호는 http://msdn.microsoft.com/ko-kr/library/system.text.encoding.aspx 에서 확인하시면 됩니다.
            int euckrCodepage = 51949;
            
            // 인코딩을 편리하게 해주기 위해서 인코딩클래스 변수를 만듭니다.
            System.Text.Encoding utf8 = System.Text.Encoding.UTF8;
            System.Text.Encoding euckr = System.Text.Encoding.GetEncoding(euckrCodepage);

            // 위에서 만든 변수를 이용하여 Byte의 배열로 문자열을 인코딩하여 얻는 부분입니다.
            byte[] utf8Bytes = utf8.GetBytes(s);
            Console.Write("UTF-8 : ");
            foreach (byte b in utf8Bytes)
            {
                Console.Write("{0:X} ", b); // byte를 16진수로 표기합니다.
            }
            Console.Write("\n");

            byte[] euckrBytes = euckr.GetBytes(s);
            Console.Write("EUC-KR : ");
            foreach (byte b in euckrBytes)
            {
                Console.Write("{0:X} ", b); // byte를 16진수로 표기합니다.
            }
            Console.Write("\n");

            // 인코딩된것을 문자열로 변환하기
            string decodedStringByEUCKR = euckr.GetString(euckrBytes);
            string decodedStringByUTF8 = utf8.GetString(utf8Bytes);
            Console.WriteLine("EUC-KR로 디코딩된 문자열 : " + decodedStringByEUCKR);
            Console.WriteLine("UTF-8로 디코딩된 문자열 : " + decodedStringByUTF8);
        }
    }
}
