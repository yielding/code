#include <SDL2/SDL.h>
#include <iostream>

using namespace std;

int main(int argc, const char *argv[])
{
  SDL_Init(SDL_INIT_EVERYTHING);

  auto window = SDL_CreateWindow("My Game Window",
      100, 100,
      640, 480,
      SDL_WINDOW_SHOWN);

  auto ren = SDL_CreateRenderer(window, -1,
      SDL_RENDERER_ACCELERATED | 
      SDL_RENDERER_PRESENTVSYNC);

  auto bmp = SDL_LoadBMP("character.bmp");
  if (bmp == nullptr)
  {
    return 1;
  }

  auto tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == nullptr)
  {
    return 1;
  }

  SDL_RenderClear(ren);
  SDL_RenderCopy(ren, tex, NULL, NULL);
  SDL_RenderPresent(ren);

  SDL_Delay(5000);

  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(ren);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
