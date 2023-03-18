//compress.h
#include "pdll.h"
//dll wrapper class using the virtual class
//so that the functions will show up in the drop down browser

class VirtualComp
{
	short CompressData(void* input, void* output, long size, short type) = 0;
	short DecompressData(void* input, void* output, long size, short type) = 0;
};

class CompDll: public PDLL, public VirtualComp
{
	//call the macro and pass your class name
	DECLARE_CLASS(CompDll)
	//use DECLARE_FUNCTION4 since this function has 4 parameters
	DECLARE_FUNCTION4(short, CompressData, void*, void*, long, short)
	DECLARE_FUNCTION4(short, DecompressData, void*, void*, long, short)

	//hard code the dll name in the constructor
	CompDll(){Initialize("compress.dll")};
};
