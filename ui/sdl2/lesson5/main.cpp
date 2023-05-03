#include <string>
#include <iostream>

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

const int SCREEN_WIDTH  = 640;
const int SCREEN_HEIGHT = 480;

void logSDLError(std::ostream &os, const std::string &msg)
{
	os << msg << " error: " << SDL_GetError() << std::endl;
}

SDL_Texture* loadTexture(const std::string &file, SDL_Renderer *ren)
{
	auto texture = IMG_LoadTexture(ren, file.c_str());
	if (texture == nullptr)		
		logSDLError(std::cout, "LoadTexture");

	return texture;
}

void renderTexture(SDL_Texture *tex, SDL_Renderer *ren, SDL_Rect dst, SDL_Rect *clip = nullptr)
{
	SDL_RenderCopy(ren, tex, clip, &dst);
}

void renderTexture(SDL_Texture *tex, SDL_Renderer *ren, int x, int y, SDL_Rect *clip = nullptr)
{
	SDL_Rect dst;
	dst.x = x;
	dst.y = y;
	if (clip != nullptr)
  {
		dst.w = clip->w;
		dst.h = clip->h;
	}
	else
  {
		SDL_QueryTexture(tex, NULL, NULL, &dst.w, &dst.h);
  }

	renderTexture(tex, ren, dst, clip);
}

int main(int argc, char** argv)
{
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
  {
		logSDLError(std::cout, "SDL_Init");
		return 1;
	}

	auto window = SDL_CreateWindow("Lesson 5", SDL_WINDOWPOS_CENTERED, 
		SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);

	if (window == nullptr)
  {
		logSDLError(std::cout, "CreateWindow");
		return 2;
	}

	auto renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if (renderer == nullptr)
  {
		logSDLError(std::cout, "CreateRenderer");
		return 3;
	}

	auto image = loadTexture("image.png", renderer);
	if (image == nullptr)
		return 4;

	int iW = 100, iH = 100;
	int x = SCREEN_WIDTH / 2 - iW / 2;
	int y = SCREEN_HEIGHT / 2 - iH / 2;

	//Setup the clips for our image
	SDL_Rect clips[4];
	//Since our clips our uniform in size we can generate a list of their
	//positions using some math (the specifics of this are covered in the lesson)
	for (int i = 0; i < 4; ++i){
		clips[i].x = i / 2 * iW;
		clips[i].y = i % 2 * iH;
		clips[i].w = iW;
		clips[i].h = iH;
	}
	//Specify a default clip to start with
	int useClip = 0;

	SDL_Event e;
	bool quit = false;
	while (!quit)
  {
		//Event Polling
		while (SDL_PollEvent(&e))
    {
			if (e.type == SDL_QUIT)
				quit = true;
			//Use number input to select which clip should be drawn
			if (e.type == SDL_KEYDOWN)
      {
				switch (e.key.keysym.sym)
        {
					case SDLK_1:
						useClip = 0;
						break;
					case SDLK_2:
						useClip = 1;
						break;
					case SDLK_3:
						useClip = 2;
						break;
					case SDLK_4:
						useClip = 3;
						break;
					case SDLK_ESCAPE:
						quit = true;
						break;
					default:
						break;
				}
			}
		}

		//Rendering
		SDL_RenderClear(renderer);

		//Draw the image
		renderTexture(image, renderer, x, y, &clips[useClip]);

		//Update the screen
		SDL_RenderPresent(renderer);
	}
	//Clean up
	SDL_DestroyTexture(image);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	IMG_Quit();
	SDL_Quit();

	return 0;
}
