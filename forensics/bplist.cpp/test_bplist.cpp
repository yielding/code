#include "catch2/catch.hpp"

#include "bplist.hpp"
#include <cstdlib>
#include <string>

using namespace std;

TEST_CASE("BPlist Interface") {
  SECTION("bplist_test.plist sample") {
    auto path = string(getenv("HOME")) + "/develop/data/bplist_test.plist";
    util::parser::PropertyList plist;
    auto res = plist.open(path);

    REQUIRE(res);
    REQUIRE(plist.offset_size() == 1);
    REQUIRE(plist.offset_ref_size() == 1);
    REQUIRE(plist.object_count() == 16);
    REQUIRE(plist.top_object_start() == 0);
    REQUIRE(plist.offset_table_start() == 127);
  }

  SECTION("second") {
    REQUIRE(true);
  }
}
