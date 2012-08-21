#include <iostream>
#include <fstream>
#include <string>
#include <boost/crc.hpp>

using namespace std;

int main0(int argc, const char *argv[])
{
    boost::crc_32_type result;
    ifstream ifs("./crc32.cpp", ios_base::binary);

    int const BUF_SIZE = 1024;

    if (ifs) 
        do
        {
            char buffer[BUF_SIZE] = { 0 };
            ifs.read(buffer, BUF_SIZE);
            result.process_bytes(buffer, ifs.gcount());
        }
        while(ifs);

    cout << "res: " << hex << result.checksum();

    return 0;
}

int main()
{
    string a = "1234567890";

    boost::crc_32_type result;
    result.process_bytes(a.data(), a.size());
    cout << result.checksum() << endl;

    result.reset(0x1234);
    result.process_bytes(a.data(), a.size());
    cout << uint32_t(result.checksum()) << endl;
}
