#include "StdAfx.h"
#include "resource.h"
#include "SplashScreen.h"
#include "shlwapi.h"

CSplashScreen::CSplashScreen(HINSTANCE hInstance, DWORD nFadeoutTime, CImageLoader *pImgLoader, LPCTSTR lpszPrefix, LPCTSTR lpszAppFileName)
{
	m_strSplashClass =  _T("SplashWindow") ;
	m_strSplashClass += lpszPrefix;
	m_hInstance=hInstance;
	m_nFadeoutTime =nFadeoutTime;
	m_strPrefix=lpszPrefix;
	m_strAppFileName=lpszAppFileName;
	m_pImgLoader=pImgLoader;

	memset (&m_blend, 0, sizeof(m_blend));
	m_nFadeoutEnd=0;
}

CSplashScreen::~CSplashScreen(void)
{
}

// Calls UpdateLayeredWindow to set a bitmap (with alpha) as the content of the splash window.
void CSplashScreen::SetSplashImage(HWND hwndSplash, HBITMAP hbmpSplash)
{
	// get the size of the bitmap
	BITMAP bm;
	POINT ptZero = { 0 };

	GetObject(hbmpSplash, sizeof(bm), &bm);

	SIZE sizeSplash = { bm.bmWidth, bm.bmHeight };

	// get the primary monitor's info
	HMONITOR hmonPrimary = MonitorFromPoint(ptZero, MONITOR_DEFAULTTOPRIMARY);
	MONITORINFO monitorinfo = { 0 };
	monitorinfo.cbSize = sizeof(monitorinfo);
	GetMonitorInfo(hmonPrimary, &monitorinfo);

	// center the splash screen in the middle of the primary work area
	const RECT & rcWork = monitorinfo.rcWork;
	POINT ptOrigin;

	ptOrigin.x = rcWork.left + (rcWork.right - rcWork.left - sizeSplash.cx) / 2;
	ptOrigin.y = rcWork.top + (rcWork.bottom - rcWork.top - sizeSplash.cy) / 2;

	// create a memory DC holding the splash bitmap
	HDC hdcScreen = GetDC(NULL);
	HDC hdcMem = CreateCompatibleDC(hdcScreen);
	HBITMAP hbmpOld = (HBITMAP) SelectObject(hdcMem, hbmpSplash);

	// use the source image's alpha channel for blending
	m_blend.BlendOp = AC_SRC_OVER;
	m_blend.SourceConstantAlpha = 0xff;
	m_blend.AlphaFormat = AC_SRC_ALPHA;

	// paint the window (in the right location) with the alpha-blended bitmap
	UpdateLayeredWindow(hwndSplash, hdcScreen, &ptOrigin, &sizeSplash,hdcMem, &ptZero, RGB(0, 0, 0), &m_blend, ULW_ALPHA);

	// delete temporary objects
	SelectObject(hdcMem, hbmpOld);
	DeleteDC(hdcMem);
	ReleaseDC(NULL, hdcScreen);

	::SetWindowPos(hwndSplash ,       // handle to window
				HWND_TOPMOST,  // placement-order handle
				ptOrigin.x,     // horizontal position
				ptOrigin.y,      // vertical position
				sizeSplash.cx,  // width
				sizeSplash.cy, // height
				SWP_SHOWWINDOW); // window-positioning options);
}



// Creates the splash owner window and the splash window.
HWND CSplashScreen::CreateSplashWindow()
{
	HWND hwndOwner = CreateWindow(m_strSplashClass.c_str(), NULL, WS_POPUP,0, 0, 0, 0, NULL, NULL, m_hInstance, NULL);
	return CreateWindowEx(WS_EX_LAYERED, m_strSplashClass.c_str(), NULL, WS_POPUP | WS_VISIBLE, 0, 0, 0, 0, hwndOwner, NULL, m_hInstance, NULL);
}

// Registers a window class for the splash and splash owner windows.
void CSplashScreen::RegisterWindowClass()
{
	WNDCLASS wc = { 0 };
	wc.lpfnWndProc = DefWindowProc;
	wc.hInstance = m_hInstance;
	wc.hIcon = LoadIcon(m_hInstance, MAKEINTRESOURCE(IDI_SPLASHICON));
	wc.hCursor = LoadCursor(NULL, IDC_ARROW); 
	wc.lpszClassName = m_strSplashClass.c_str();

	RegisterClass(&wc);
}
// Registers a window class for the splash and splash owner windows.
void CSplashScreen::UnregisterWindowClass() {
	UnregisterClass(m_strSplashClass.c_str(), m_hInstance);
}

HANDLE CSplashScreen::LaunchWpfApplication()
{
	// get folder of the current process
	TCHAR szCurrentFolder[MAX_PATH] = { 0 };

	GetModuleFileName(NULL, szCurrentFolder, MAX_PATH);

	PathRemoveFileSpec(szCurrentFolder);

	// add the application name to the path
	TCHAR szApplicationPath[MAX_PATH];
	if (m_strFullPath.length()>0) {
		lstrcpy(szApplicationPath, m_strFullPath.c_str());
	}
	else {
		PathCombine(szApplicationPath, szCurrentFolder, m_strAppFileName.c_str());
	}

	// start the application
	STARTUPINFO si = { 0 };
	si.cb = sizeof(si);
	PROCESS_INFORMATION pi = { 0 };
	CreateProcess(szApplicationPath, GetCommandLine(), NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi);

	return pi.hProcess;
}

bool CSplashScreen::FadeWindowOut(HWND hWnd) {
	DWORD dtNow = GetTickCount();
	if (dtNow >= m_nFadeoutEnd) 
	{
		return true;
	} 
	else
	{ 
		double fade = ((double)m_nFadeoutEnd - (double)dtNow) / (double)m_nFadeoutTime;
		m_blend.SourceConstantAlpha = (byte)(255 * fade);
		
		UpdateLayeredWindow(hWnd, NULL, NULL, NULL,NULL, NULL, RGB(0, 0, 0), &m_blend, ULW_ALPHA);
		return false;
	} 
	
}

inline DWORD CSplashScreen::PumpMsgWaitForMultipleObjects(HWND hWnd, DWORD nCount, LPHANDLE pHandles, DWORD dwMilliseconds)
{
	// useful variables
	const DWORD dwStartTickCount = ::GetTickCount();
	// loop until done
	for (;;)
	{
		// calculate timeout
		const DWORD dwElapsed = GetTickCount() - dwStartTickCount;
		const DWORD dwTimeout = dwMilliseconds == INFINITE ? INFINITE :dwElapsed < dwMilliseconds ? dwMilliseconds - dwElapsed : 0;

		// wait for a handle to be signaled or a message
		const DWORD dwWaitResult = MsgWaitForMultipleObjects(nCount, pHandles, FALSE, dwTimeout, QS_ALLINPUT);

		if (dwWaitResult == WAIT_OBJECT_0 + nCount)
		{
			// pump messages
			MSG msg;

			while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) != FALSE)
			{
				// check for WM_QUIT
				if (msg.message == WM_QUIT)
				{
					// repost quit message and return
					PostQuitMessage((int) msg.wParam);
					return WAIT_OBJECT_0 + nCount;
				}

				// dispatch thread message
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
		}
		else
		{
			// check fade event (pHandles[1]).  If the fade event is not set then we simply need to exit.  
			// if the fade event is set then we need to fade out  
			const DWORD dwWaitResult = MsgWaitForMultipleObjects(1, &pHandles[1], FALSE, 0, QS_ALLINPUT);
			if (dwWaitResult == WAIT_OBJECT_0) {
				MSG msg;
				// timeout on actual wait or any other object
				SetTimer(hWnd, 1, 30,NULL);
				m_nFadeoutEnd = GetTickCount()+m_nFadeoutTime;
				BOOL bRet;

				while( (bRet = GetMessage( &msg, hWnd, 0, 0 )) != 0)
				{ 
					if (bRet == -1)
					{
						// handle the error and possibly exit
					}
					else
					{
						if (msg.message==WM_TIMER) {
							if (FadeWindowOut(hWnd)) { // finished
								return dwWaitResult;
							}
						}
						TranslateMessage(&msg); 
						DispatchMessage(&msg); 
					}
				}
			}
			return dwWaitResult;
		}
	}
}

void CSplashScreen::Show() {
	CoInitialize(0);

	// create the named close splash screen event, making sure we're the first process to create it
	SetLastError(ERROR_SUCCESS);

	std::basic_string <TCHAR> strEvent1 = _T("CloseSplashScreenEvent")+m_strPrefix;
	HANDLE hCloseSplashEvent = CreateEvent(NULL, TRUE, FALSE, strEvent1.c_str());

	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		ExitProcess(0);
	}

	std::basic_string <TCHAR> strEvent2 = _T("CloseSplashScreenWithoutFadeEvent")+m_strPrefix;
	HANDLE hCloseSplashWithoutFadeEvent = CreateEvent(NULL, TRUE, FALSE, strEvent2.c_str());
	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		ExitProcess(0);
	}

	HBITMAP hb = m_pImgLoader->LoadSplashImage();
	HWND wnd= NULL;
	RegisterWindowClass();

	if (hb!=NULL) {
		wnd=CreateSplashWindow();
		SetSplashImage(wnd, hb);
	}

	// launch the WPF application
	HANDLE hProcess = LaunchWpfApplication();

	AllowSetForegroundWindow(GetProcessId(hProcess));

	if (wnd!=NULL) {
		// display the splash screen for as long as it's needed
		HANDLE aHandles[3] = { hProcess, hCloseSplashEvent, hCloseSplashWithoutFadeEvent };
		PumpMsgWaitForMultipleObjects(wnd, 3, &aHandles[0], INFINITE);
	}

	CloseHandle(hCloseSplashEvent);
	CloseHandle(hCloseSplashWithoutFadeEvent);

	UnregisterWindowClass();
}

void CSplashScreen::SetFullPath(LPCTSTR lpszPath) {
	m_strFullPath=lpszPath;
}