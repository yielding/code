#include <SDL2/SDL.h>
#include <string>
#include <iostream>

using namespace std;

// Screen attributes
const int SCREEN_WIDTH  = 640;
const int SCREEN_HEIGHT = 480;

void logSDLError(ostream &os, const string &msg)
{
	os << msg << " error: " << SDL_GetError() << endl;
}

/**
* Loads a BMP image into a texture on the rendering device
* @param file The BMP image file to load
* @param ren The renderer to load the texture onto
* @return the loaded texture, or nullptr if something went wrong.
*/
SDL_Texture* loadTexture(const string &file, SDL_Renderer *ren)
{
  // Load the image
  auto loadedImage = SDL_LoadBMP(file.c_str());
  // If the loading went ok, convert to texture and return the texture
  if (loadedImage == nullptr)
  {
    logSDLError(cout, "LoadBMP");
    return nullptr;
  }

  auto texture = SDL_CreateTextureFromSurface(ren, loadedImage);
  SDL_FreeSurface(loadedImage);
  // Make sure converting went ok too
  if (texture == nullptr)
    logSDLError(cout, "CreateTextureFromSurface");

  return texture;
}

/**
* Draw an SDL_Texture to an SDL_Renderer at position x, y, preserving
* the texture's width and height
* @param tex The source texture we want to draw
* @param ren The renderer we want to draw too
* @param x The x coordinate to draw too
* @param y The y coordinate to draw too
*/
void renderTexture(SDL_Texture *tex, SDL_Renderer *ren, int x, int y)
{
	// Setup the destination rectangle to be at the position we want
	SDL_Rect dst;
	dst.x = x;
	dst.y = y;

	// Query the texture to get its width and height to use
	SDL_QueryTexture(tex, NULL, NULL, &dst.w, &dst.h);
	SDL_RenderCopy(ren, tex, NULL, &dst);
}

int main(int argc, char** argv)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
  {
		logSDLError(cout, "SDL_Init");
		return 1;
	}

	//Setup our window and renderer
	auto window = SDL_CreateWindow("Lesson 2", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
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

	// The textures we'll be using
	auto background = loadTexture("background.bmp", renderer);
	auto image      = loadTexture("image.bmp", renderer);

	// Make sure they both loaded ok
	if (background == nullptr || image == nullptr)
		return 4;

	// Clear the window
	SDL_RenderClear(renderer);

	// Get the width and height from the texture so we know how much to move x,y by
	// to tile it correctly
	int bW, bH;
	SDL_QueryTexture(background, NULL, NULL, &bW, &bH);

	//We want to tile our background so draw it 4 times
	renderTexture(background, renderer, 0, 0);
	renderTexture(background, renderer, bW, 0);
	renderTexture(background, renderer, 0, bH);
	renderTexture(background, renderer, bW, bH);

	// Draw our image in the center of the window
	// We need the foreground image's width to properly compute the position
	// of it's top left corner so that the image will be centered
	int iW, iH;
	SDL_QueryTexture(image, NULL, NULL, &iW, &iH);
	int x = SCREEN_WIDTH / 2 - iW / 2;
	int y = SCREEN_HEIGHT / 2 - iH / 2;
	renderTexture(image, renderer, x, y);

	// Update the screen
	SDL_RenderPresent(renderer);
	SDL_Delay(2000);

	// Destroy the various items
	SDL_DestroyTexture(background);
	SDL_DestroyTexture(image);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	SDL_Quit();
	
	return 0;
}
