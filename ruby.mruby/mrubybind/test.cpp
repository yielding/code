#include "mrubybind.hpp"

#include <iostream>
#include <string>
#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// self test: ownership (owned copy vs borrowed host object), type-checked
// arguments, collection conversion, and error reporting with file/line info.
//
////////////////////////////////////////////////////////////////////////////////
namespace {

  int g_alive = 0;

  class track
  {
  public:
    track(const string title)
      : _title(title)
    {
      g_alive++;
    }

    track(const track& rhs)
      : _title(rhs._title)
    {
      g_alive++;
    }

    ~track()
    {
      g_alive--;
    }

  public:
    auto title() -> string { return _title; }
    auto rate(const int stars) -> int { return stars * 2; }

  private:
    string _title;
  };

  class player
  {
  public:
    player()
    {
      _tracks.push_back(track("intro"));
      _tracks.push_back(track("verse"));
      _tracks.push_back(track("outro"));
    }

  public:
    auto tracks() -> vector<track>& { return _tracks; }
    auto volume() -> double { return 0.75; }
    auto data() -> vector<uint8_t> { return { 0x41, 0x42, 0x43 }; }

  private:
    vector<track> _tracks;
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto main() -> int
{
  auto failed = 0;

  {
    player pl;  // host-owned, exposed to the script as borrowed $player

    mrubybind::vm vm;
    if (!vm.ok())
      return 1;

    auto mrb = vm.state();

    mrubybind::klass<track>::define(mrb, "Track")
      .ctor<string>()
      .method<&track::title>("title")
      .method<&track::rate>("rate");

    mrubybind::klass<player>::define(mrb, "Player")
      .method<&player::tracks>("tracks")
      .method<&player::volume>("volume")
      .method<&player::data>("data");

    vm.set_global("$player", mrubybind::klass<player>::wrap(mrb, &pl, false));

    auto ok = vm.run_string(R"(
      t = Track.new("intro")
      puts "title  : #{t.title}"
      puts "rate   : #{t.rate(3)}"

      begin
        Track.new(123)
        raise "FAIL: TypeError expected"
      rescue TypeError => e
        puts "checked: #{e.class}"
      end

      puts "volume : #{$player.volume}"
      $player.tracks.each { |tr| puts "track  : #{tr.title}" }
      puts "data   : #{$player.data.inspect}"
    )", "test.rb");

    if (!ok)
      failed = 1;

    // parse error must be reported with file and line, not crash
    puts("expect a parse error report below:");
    vm.run_string("def broken", "broken.rb");
  }

  cout << "alive after close: " << g_alive << " (expect 0)\n";
  if (g_alive != 0)
    failed = 1;

  cout << (failed ? "self test FAILED\n" : "self test ok\n");

  return failed;
}
