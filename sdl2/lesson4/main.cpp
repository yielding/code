#include <string>
#include <iostream>

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

using namespace std;

const int SCREEN_WIDTH  = 640;
const int SCREEN_HEIGHT = 480;

void logSDLError(ostream &os, const string &msg)
{
  os << msg << " error: " << SDL_GetError() << endl;
}

SDL_Texture* loadTexture(const string &file, SDL_Renderer *ren)
{
  SDL_Texture *texture = IMG_LoadTexture(ren, file.c_str());
  if (texture == nullptr)		
    logSDLError(cout, "LoadTexture");
  return texture;
}

void renderTexture(SDL_Texture *tex, SDL_Renderer *ren, int x, int y, int w, int h)
{
  SDL_Rect dst;
  dst.x = x;
  dst.y = y;
  dst.w = w;
  dst.h = h;
  SDL_RenderCopy(ren, tex, NULL, &dst);
}

void renderTexture(SDL_Texture *tex, SDL_Renderer *ren, int x, int y)
{
  int w, h;
  SDL_QueryTexture(tex, NULL, NULL, &w, &h);
  renderTexture(tex, ren, x, y, w, h);
}

int main(int argc, char** argv)
{
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
  {
    logSDLError(cout, "SDL_Init");
    return 1;
  }

  auto window = SDL_CreateWindow("Lesson 4", SDL_WINDOWPOS_CENTERED, 
      SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);

  if (window == nullptr)
  {
    logSDLError(cout, "CreateWindow");
    return 2;
  }

  auto renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (renderer == nullptr)
  {
    logSDLError(cout, "CreateRenderer");
    return 3;
  }

  //The texture we'll be using
  auto image = loadTexture("image.png", renderer);
  if (image == nullptr)
    return 4;

  //Our texture size won't change, so we can get it here
  //instead of constantly allocating/deleting ints in the loop
  int iW, iH;
  SDL_QueryTexture(image, NULL, NULL, &iW, &iH);
  int x = SCREEN_WIDTH / 2 - iW / 2;
  int y = SCREEN_HEIGHT / 2 - iH / 2;

  //Our event structure
  SDL_Event e;
  //For tracking if we want to quit
  bool quit = false;
  while (!quit)
  {
    //Read any events that occured, for now we'll just quit if any event occurs
    while (SDL_PollEvent(&e))
    {
      if (e.type == SDL_QUIT)            quit = true;
      if (e.type == SDL_KEYDOWN)         quit = true;
      if (e.type == SDL_MOUSEBUTTONDOWN) quit = true;
    }

    SDL_RenderClear(renderer);

    renderTexture(image, renderer, x, y);

    SDL_RenderPresent(renderer);
  }

  //Destroy the various items
  SDL_DestroyTexture(image);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  IMG_Quit();
  SDL_Quit();

  return 0;
}
