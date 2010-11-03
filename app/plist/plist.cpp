#include <plist/plist.h>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  plist_t arr = plist_new_array();
  plist_array_append_item(arr, plist_new_string("UserDatabases"));

  plist_t dict = plist_new_dict();
  plist_dict_insert_item(dict, "Sources", arr);
  char *buffer;
  unsigned len = 0;

  plist_to_xml(dict, &buffer, &len);

  cout << buffer << endl;


  char* message = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    "<plist version=\"1.0\">"
    "<dict>"
    "<key>Sources</key>"
    "<array>"
    "<string>UserDatabases</string>"
    "</array>"
    "</dict>"
    "</plist>";

cout << message << endl;


  return 0;
}
