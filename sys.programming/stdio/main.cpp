#include <iostream>
#include <string>
#include <sstream>
#include <map>
#include <cctype>

using namespace std;

int read_content_length() 
{
  string line;
  int content_length = -1;

  // 헤더 파싱
  while (getline(cin, line)) 
  {
    if (line == "\r" || line.empty())
      break; // 헤더 끝

    // 양 끝 공백 제거
    line.erase(0, line.find_first_not_of(" \t\r\n"));
    line.erase(line.find_last_not_of(" \t\r\n") + 1);

    if (line.starts_with("Content-Length"))
    {
      auto value = line.substr(15);
      value.erase(0, value.find_first_of(":") + 1);
      content_length = stoi(value);
    }
  }

  return content_length;
}

auto read_stdin_message() -> string 
{
  int content_length = read_content_length();

  if (content_length <= 0) 
    throw runtime_error("Invalid Content-Length");

  string content(content_length, '\0');
  cin.read(&content[0], content_length);

  return content;
}

void send_lsp_response(const string& json) 
{
  cout << "Content-Length: " << json.size() << "\r\n\r\n";
  cout << json;
  cout.flush();
}

int main() 
{
  while (true) 
  {
    try 
    {
      auto message = read_stdin_message();
      cerr << "Received: " << message << endl;

      // 예시 응답
      string response = R"({"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}})";
      send_lsp_response(response);
    } 
    catch (const exception& ex) 
    {
      cerr << "Error reading message: " << ex.what() << endl;
      break;
    }
  }

  return 0;
}
