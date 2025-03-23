#include <SFML/Graphics.hpp>
#include <optional>

using namespace sf;
using namespace std;

int main()
{
  RenderWindow window(VideoMode(Vector2u(200, 200)), "SFML works!");
  CircleShape shape(100.f);
  shape.setFillColor(Color::Green);

  while (window.isOpen())
  {
    while (const optional event = window.pollEvent())
    {
      if (event->is<Event::Closed>())
        window.close();
    }

    window.clear();
    window.draw(shape);
    window.display();
  }

  return 0;
}
