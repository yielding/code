#include <SDL2/SDL.h>
#include <SDL2/SDL_mixer.h>
#include <iostream>

//#define WAV_PATH "Roland-GR-1-Trumpet-C5.wav"
#define WAV_PATH "/Users/yielding/Desktop/test.wav"
#define MUS_PATH "HR2_Friska.ogg"

// Our wave file
Mix_Chunk *wave = NULL;
// Our music file
Mix_Music *music = NULL;

using namespace std;

int main(int argc, char* argv[])
{
	// Initialize SDL.
	if (SDL_Init(SDL_INIT_AUDIO) < 0)
		return -1;
			
cout << "1" << endl;

	//Initialize SDL_mixer 
	if( Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 4096 ) == -1 ) 
		return -1; 
	
cout << "2" << endl;
	// Load our sound effect
	wave = Mix_LoadWAV(WAV_PATH);
	if (wave == NULL)
		return -1;
	
cout << "3" << endl;
	// Load our music
	//music = Mix_LoadMUS(MUS_PATH);
	//if (music == NULL)
		//return -1;
	
	if ( Mix_PlayChannel(-1, wave, 0) == -1 )
		return -1;
	
cout << "4" << endl;
	//if ( Mix_PlayMusic( music, -1) == -1 ) return -1;
		
	while ( Mix_PlayingMusic() ) ;
	
cout << "5" << endl;
	// clean up our resources
	Mix_FreeChunk(wave);
	//Mix_FreeMusic(music);
	
	// quit SDL_mixer
	Mix_CloseAudio();
	
	return 0;
}
