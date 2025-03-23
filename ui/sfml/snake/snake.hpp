#pragma once

#include <queue>

using namespace std;

class Snake 
{
public:
    Snake(int x, int y, int length, int speed)
    {
        this->x = x;
        this->y = y;
        this->length = length;
        this->speed = speed;
        this->direction = 0;
        body.push({x, y});
    }

    void next()
    {
        switch (direction)
        {
            case 0: x += speed; break;
            case 1: y += speed; break;
            case 2: x -= speed; break;
            case 3: y -= speed; break;
        }

        body.push({x, y});
        if (body.size() > length)
        {
            body.pop();
        }
    }

private:
    queue<pair<int, int>> body;
    int direction;
    int length;
    int speed;
    int x, y;
};

