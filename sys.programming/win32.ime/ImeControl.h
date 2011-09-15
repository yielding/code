// ImeControl.h: interface for the CImeControl class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __IME__ 
#define __IME__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CImeControl  {
public:
	CImeControl();
	virtual ~CImeControl();

	static void Hangul_On   (HWND hWnd);
	static void Hangul_Off  (HWND hWnd);
	static void IMENormal   (HWND hWnd);
	static void FullMode_On (HWND hWnd);
	static void FullMode_Off(HWND hWnd);
	static void Hangul_Full (HWND hWnd);
};

#endif
