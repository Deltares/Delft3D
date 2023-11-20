#include "mex.h"

#include "stdafx.h"
#include "SplashScreen.h"

// in WPF code
void CloseSplashScreen()
{
    //// signal the native process (that launched us) to close the splash screen
    //using (var closeSplashEvent = new EventWaitHandle(false,
    //    EventResetMode.ManualReset, "CloseSplashScreenEvent"+Prefix))
    //{
    //    closeSplashEvent.Set();
    //}
	std::basic_string <TCHAR> strEvent1 = _T("CloseSplashScreenEventQuickPlot");
	HANDLE hCloseSplashEvent = CreateEvent(NULL, TRUE, FALSE, strEvent1.c_str());
	if (! SetEvent(hCloseSplashEvent) )
	{
		printf("CloseSplashScreenEvent failed");
	}
	CloseHandle(hCloseSplashEvent);
}

void mexFunction(
int nlhs,              // Number of left hand side (output) arguments
mxArray *plhs[],       // Array of left hand side arguments
int nrhs,              // Number of right hand side (input) arguments
const mxArray *prhs[]  // Array of right hand side arguments
)
{
	CloseSplashScreen();
}
