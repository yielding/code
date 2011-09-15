// art.c
// A simple program that uses LoadLibrary and 
// GetProcAddress to access FindArtist function from ART.DLL. 
#include <stdio.h> 
#include <windows.h> 

//Define the function prototype
typedef short (CALLBACK* FindArtistType)(char*);

void main(void) 
{ 
	BOOL freeResult, runTimeLinkSuccess = FALSE; 
	HINSTANCE dllHandle = NULL;              
	FindArtistType FindArtistPtr = NULL;

	//Load the dll and keep the handle to it
	dllHandle = LoadLibrary("art.dll");

	// If the handle is valid, try to get the function address. 
	if (NULL != dllHandle) 
	{ 
		//Get pointer to our function using GetProcAddress:
		FindArtistPtr = (FindArtistType)GetProcAddress(dllHandle,
			"FindArtist");

		// If the function address is valid, call the function. 
		if (runTimeLinkSuccess = (NULL != FindArtistPtr))
		{
			char* myArtist = "Duchamp";
			short retVal = FindArtistPtr(myArtist);
		}

		//Free the library:
		freeResult = FreeLibrary(dllHandle);       
	}

	//If unable to call the DLL function, use an alternative. 
	if(!runTimeLinkSuccess)
		printf("message via alternative method\n"); 
}
