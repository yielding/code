//this header file defines a class that will use a fictional dll called art.dll

#if !defined (_ARTDLL_H_)
#define _ARTDLL_H_

#include "PDll.h"

//this header file defines a class that will use a fictional dll called art.dll
//it contains the following functions:

/*short FindArtist(LPCTSTR artist);
short AddArtist(LPCTSTR artist, LPCTSTR country, short century);
short FindArtifact(LPCTSTR name, LPCTSTR artist);
short AddArtifact(LPCTSTR artist, LPCSTR name, short price);
*/

//use our base dll class
//definitions for the ddf functions

class ArtDll: public PDLL {
public:
	//declare our functions
	DECLARE_FUNCTION1(short, FindArtist, LPCTSTR)
	DECLARE_FUNCTION3(short, AddArtist, LPCTSTR, LPCTSTR, short)
	DECLARE_FUNCTION2(short, FindArtifact, LPCTSTR, LPCTSTR)
	DECLARE_FUNCTION3(short, AddArtifact, LPCTSTR, LPCTSTR, short)
private:
	//use the class declaration macro
	DECLARE_CLASS(ArtDll)

};

#endif
