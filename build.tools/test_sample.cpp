#include "catch2/catch.hpp"

TEST_CASE("make things comiples") {
  SECTION("name") {
    REQUIRE(1 == 1);
    REQUIRE_FALSE(1 == 2);
    REQUIRE_THROW_AS(true, logic_error);
  }
}
