#include <string>

#include <locale>
#include <iostream>
#include "rwstdmsg.h"

int main ()
{
  using namespace std;

  locale loc;

  // Get a reference to the messages<char> facet
  const messages<char>& mess =
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<messages<char> >(loc);
#else
  use_facet(loc,(messages<char>*)0);
#endif

  // Open a catalog and try to grab
  // both some valid messages, and an invalid message
  string def("Message Not Found");
  messages<char>::catalog cat =
#ifdef __WIN32__
    mess.open("rwstdmsg.dll",loc);
#else
  mess.open("./rwstdmsg.cat",loc);
#endif
  if (cat != -1)
  {
    string msg0 = mess.get(cat,1,_RW_MSG_HELLO,def);
    string msg1 = mess.get(cat,1,_RW_MSG_GOODBYE,def);
    string msg2 = mess.get(cat,1,_RW_MSG_NOGOOD,def);  // invalid msg #
    string msg3 = mess.get(cat,2,_RW_MSG_TREES,def);

    mess.close(cat);
    cout << msg0 << endl << msg1 << endl
      << msg2 << endl << msg3 << endl;
  }
  else
    cout << "Unable to open message catalog" << endl;

  return 0;
}

