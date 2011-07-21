//
// sync_client.cpp
// ~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2011 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>
#include <istream>
#include <ostream>
#include <string>
#include <sstream>
#include <fstream>
#include <boost/asio/ip/tcp.hpp>

using boost::asio::ip::tcp;
using namespace std;

size_t read_size(char* buffer, uint32_t& offset)
{
    char ss[16] = { 0 };
    int i = 0;
    while (true)
    {
        string senti(&buffer[offset], 2);
        if (senti == "\r\n")
        {
            offset += 2;
            break;
        }
        
        ss[i++] = buffer[offset++];
    }
    
    return strtol(ss, 0, 16);
}

int main(int argc, char* argv[])
{
    try
    {
        //    if (argc != 3)
        //    {
        //      cout << "Usage: http_client <server> <path>\n";
        //      cout << "Example:\n";
        //      cout << "  http_client www.boost.org /LICENSE_1_0.txt\n";
        //      return 1;
        //    }
        
        char const* h = "192.168.1.7";
        char const* q = "/personalportal/pictureview?c=5&id=f9f3ca42-0c31-49fc-9d07-8367e133f7f7";
        boost::asio::ip::tcp::iostream s;
        
        // The entire sequence of I/O operations must complete within 60 seconds.
        // If an expiry occurs, the socket is automatically closed and the stream
        // becomes bad.
        s.expires_from_now(boost::posix_time::seconds(600));
        
        // Establish a connection to the server.
        s.connect(h, "8080");
        if (!s)
        {
            cout << "Unable to connect: " << s.error().message() << "\n";
            return 1;
        }
        
        // Send the request. We specify the "Connection: close" header so that the
        // server will close the socket after transmitting the response. This will
        // allow us to treat all data up until the EOF as the content.
        s << "GET " << q << " HTTP/1.0\r\n";
        s << "Host: " << h << "\r\n";
        s << "Accept: */*\r\n";
        s << "Connection: close\r\n\r\n";
        
        // By default, the stream is tied with itself. This means that the stream
        // automatically flush the buffered output before attempting a read. It is
        // not necessary not explicitly flush the stream at this point.
        
        // Check that response is OK.
        string http_version;      s >> http_version;
        unsigned int status_code; s >> status_code;
        string status_message;
        getline(s, status_message);
        if (!s || http_version.substr(0, 5) != "HTTP/")
        {
            cout << "Invalid response\n";
            return 1;
        }
        
        if (status_code != 200)
        {
            cout << "Response returned with status code " << status_code << "\n";
            return 1;
        }
        
        // Process the response headers, which are terminated by a blank line.
        bool chunk_based = false;
        string header;
        while (getline(s, header) && header != "\r")
        {
            if (header.find("chunk") != string::npos)
                chunk_based = true;
            
            cout << header << "\n";
            cout << flush;
        }
        
        // if (chunk_based)
        
        ofstream out; out.open("x.jpg", ios_base::binary);
        bool should_stop = false;
        while (true)
        {
            if (should_stop)
                break;
            
            int const SIZE    = 1024 * 20;
            char buffer[SIZE] = { 0 };
            s.read(buffer, SIZE);
            auto r = s.gcount();
            if (r != SIZE)
                should_stop = true;
            
            uint32_t offset = 0;
            while (offset < r)
            {
                auto size = read_size(buffer, offset);
                if (size == 0)
                  break;

                out.write(buffer + offset, size);
                offset += size;
                offset += 2;
            }
        }

        out.close();

        // cerr << s.rdbuf();
    }
    catch (exception& e)
    {
        cout << "Exception: " << e.what() << "\n";
    }
    
    return 0;
}
