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

  class Track
  {
  public:
    Track(const string title)
      : _title(title)
    {
      g_alive++;
    }

    Track(const Track& rhs)
      : _title(rhs._title)
    {
      g_alive++;
    }

    ~Track()
    {
      g_alive--;
    }

  public:
    auto title() -> string { return _title; }
    auto rate(const int stars) -> int { return stars * 2; }

  private:
    string _title;
  };

  class Player
  {
  public:
    Player()
    {
      _tracks.push_back(Track("intro"));
      _tracks.push_back(Track("verse"));
      _tracks.push_back(Track("outro"));
    }

  public:
    auto tracks() -> vector<Track>& { return _tracks; }
    auto volume() -> double { return 0.75; }
    auto data() -> vector<uint8_t> { return { 0x41, 0x42, 0x43 }; }

  private:
    vector<Track> _tracks;
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
    Player player;  // host-owned, exposed to the script as borrowed $player

    mrubybind::VM vm;
    if (!vm.ok())
      return 1;

    auto mrb = vm.state();

    mrubybind::Klass<Track>::define(mrb, "Track")
      .ctor<string>()
      .method<&Track::title>("title")
      .method<&Track::rate>("rate");

    mrubybind::Klass<Player>::define(mrb, "Player")
      .method<&Player::tracks>("tracks")
      .method<&Player::volume>("volume")
      .method<&Player::data>("data");

    vm.set_global("$player", mrubybind::Klass<Player>::wrap(mrb, &player, false));

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
