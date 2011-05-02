#include <iostream>

using namespace std;

int main()
{
    try
    {
        PTreeParser ptree("/Users/yielding/develop/data/Manifest.xml");
        if (!ptree.init())
            return 0;

        string key1 = "plist.dict";
        string key2 = "plist.dict.dict";
        string filter = "key, string, true, false, date";
        auto l1 = ptree.enumerate(key1, 0, filter)
                       .enumerate(key2, 0, filter).pairs();

        Manifest m;
        for (auto it = l1.begin(); it !=l1.end(); ++it)
        {
            auto const& key = it->first; 
            if (key == "IsEncrypted")
                m.is_encrypted = it->second == "true";
            else if (key == "WasPasscodeSet")
                m.was_passcode_set = it->second == "true";
            else if (key == "ProductVersion")
                m.product_version = it->second;
            else if (key == "ProductType")
                m.product_type = it->second;
            else 
                continue;
        }

        ptree.clear()
             .enumerate(key2, 1, "key, string, dict")
             .filter("first")
             .reduce_to(m.apps);

        cout << m.to_s();
    }
    catch (std::exception &e)
    {
        std::cout << "error: " << e.what() << "\n";
    }

    return 0;
}
