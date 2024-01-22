#pragma once
#include "imageloader.h"
#include <iostream>
#include <string>

class CSplashScreen
{
private:
	// Window Class name
	std::basic_string <TCHAR> m_strSplashClass;
	HINSTANCE m_hInstance;
	BLENDFUNCTION m_blend ;
	DWORD m_nFadeoutEnd ;
	DWORD m_nFadeoutTime ;
	std::basic_string <TCHAR> m_strFullPath;
	std::basic_string <TCHAR> m_strPrefix;
	std::basic_string <TCHAR> m_strAppFileName;
	CImageLoader *m_pImgLoader;
private:
	void SetSplashImage(HWND hwndSplash, HBITMAP hbmpSplash);
	void RegisterWindowClass();
	void UnregisterWindowClass();
	HWND CreateSplashWindow();
	HANDLE LaunchWpfApplication();
	bool FadeWindowOut(HWND hwnd);
	inline DWORD PumpMsgWaitForMultipleObjects(HWND hWnd, DWORD nCount, LPHANDLE pHandles, DWORD dwMilliseconds);
public:
	CSplashScreen(HINSTANCE hInstance, DWORD nFadeoutTime, CImageLoader *pImgLoader, LPCTSTR lpszPrefix, LPCTSTR lpszAppFileName);
	~CSplashScreen(void);

	void SetFullPath(LPCTSTR lpszPath) ;
	void CSplashScreen::Show();
};
