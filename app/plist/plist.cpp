#include <plist/plist.h>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  char const* sources[] = {
    "AppleSupport",
    "Network",
    "VPN",
    "WiFi",
    "UserDatabases",
    "CrashReporter",
    "tmp",
    "SystemConfigurations",
    "Caches",
    "MobileWirelessSync",
    "Lockdown",
    "MobileInstallation",
    "SafeHarbor",
    NULL
  };

  plist_t arr = plist_new_array();
  for (int i=0; sources[i] != NULL; ++i)
    plist_array_append_item(arr, plist_new_string(sources[i]));

  plist_t dict = plist_new_dict();
  plist_dict_insert_item(dict, "Sources", arr);

  unsigned len = 0;
  char *buffer;
  plist_to_xml(dict, &buffer, &len);

  cout << buffer << endl;

  delete [] buffer;

  return 0;
}
