// SplashScreenStarter.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "SplashScreenStarter.h"

#include "SplashScreen.h"
#include "Objidl.h"
#include <iostream>
#include <string>
#include "ResourceImageLoader.h"
#include "shlwapi.h"
#include "FileImageLoader.h"

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
					 int       nCmdShow) {
	CSplashScreen splash(
		hInstance, 
		// Length of time in milliseconds to display splashscreen fading 
		1500, 
		// Specifies the way to load the image for the splashscreen 
		// To load from the resources, as we have done here, provide the 
		// resource name and the resource type name.  We have used a jpg.
		// If you use a PNG, its opacity should be honored to allow you 
		// to display partially transparent splashscreens.
		// To edit the resources in a C++ application go to the resource 
		// view tab
		// To load from a file use this line instead, where filename is the file
		// you wish to load:
		// new CFileImageLoader(filename), 
		new CResourceImageLoader(MAKEINTRESOURCE(IDB_PNG1), _T("PNG")),
		// Application prefix.  This will be added to the event name so there are no 
		// conflicts between applications.
		_T("QuickPlot"), 
		// File name of your executable to run.  The extension does not need to be .exe.  
		// If you want to stop your users from starting your application without displaying
		// the splashscreen you could use a different extension.
		// Is assumed it is in the same folder as this program 
		// If it is not you can call splash.SetFullPath
		_T("d3d_qp.exec") 
		) ;
//#ifdef _DEBUG
//	// In debug mode you may wish to a specify a full path to the application as it may not 
//	// be in your output folder to run using a relative path 
//	splash.SetFullPath(_T("D:\\projectswpf\\SplashScreen\\SplashScreenTester.exe"));
//#endif
	splash.Show();
}

// in WPF code
//private void CloseSplashScreen()
//{
//    // signal the native process (that launched us) to close the splash screen
//    using (var closeSplashEvent = new EventWaitHandle(false,
//        EventResetMode.ManualReset, "CloseSplashScreenEvent"+Prefix))
//    {
//        closeSplashEvent.Set();
//    }
//}

