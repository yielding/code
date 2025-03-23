#include <SFML/Graphics.hpp>
#include <iostream>

class Game
{
public:
    static void Run()
    {
        // create the window
        sf::RenderWindow window(sf::VideoMode({800, 600}), "My window");

        // 폰트 로드 (폰트 파일이 필요함)
        sf::Font font;
        if (!font.openFromFile("/System/Library/Fonts/Supplemental/Arial.ttf")) {
            std::cerr << "폰트 로드 실패!" << std::endl;
        }

        sf::Text text(font); // a font is required to make a text object

        // set the string to display
        text.setString("Hello world");

        // set the character size
        text.setCharacterSize(24); // in pixels, not points!

        // set the color
        text.setFillColor(sf::Color::Red);

        // set the text style
        text.setStyle(sf::Text::Bold | sf::Text::Underlined);


        // run the program as long as the window is open
        while (window.isOpen())
        {
            // check all the window's events that were triggered since the last iteration of the loop
            while (const std::optional event = window.pollEvent())
            {
                // "close requested" event: we close the window
                if (event->is<sf::Event::Closed>())
                    window.close();
            }

            // clear the window with black color
            window.clear(sf::Color::Black);

            // draw everything here...
            // window.draw(...);

            window.draw(text);

            // end the current frame
            window.display();
        }

    }
};
