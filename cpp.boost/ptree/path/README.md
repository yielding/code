libptree : a library for json, xml 
==================================

Goal
----
  * json, xml(plist) and bplist *three* types shall be treated transparently
  * hide verbosing boost includes by implementing dynamic library
  * support same or nearly same interface of jsoncpp
    cf) <http://soncpp.sourceforge.net/>

Methods
-------
  * use boost::property_tree to manage 3 different types(json, xml, plist) at 
    the same type

JSON
----
  * read, write Korean 
    - boost::property_tree has error in treating Korean writing

  * support type 
    - boost::property_tree need specail caution when putting down types 
      other than string)

  * key separator
    - make it easy to use separator

User Code
---------
  * won't use template at least in the header file

  Tree t;
  t.read_from_json("a.json");
  int   v0 = t.get("root.internal.int_leaf").as_int();
  float v1 = t.get("root.internal.float_leaf").as_float();
  vector<int> v2
           = t.get_array("root.arr.int").as_int();

  vector<float> v3
           = t.get("root.arr.real").as_float();

  vector<string> v4
           = t.get("root.string_array").as_string();

  map<string, int> v5
           = t.get("root.arr.string").as_???;
Etcs
----
  * variants have nothing to do with this library
  * ptree parser will be substituted by this class

JSON parser Interface
---------------------
  - key separator
    constructor
    set_separator
    
  - disk i/o

  - primitives and composite
    get("key", optional_default_value)
        as_int
        as_float
        as_string

    // set as_int
    // set as_float
    // set as_string

  - conversion
    to_xml
    to_json
    to_plist
