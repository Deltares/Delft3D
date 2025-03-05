// Copyright (C) 2010-2011 Deltares
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief rtcTools.cpp
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */

#include "rtcTools.h"
#include "rtcToolsRuntime.h"


// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    cout << "\nUsage:\n"
            "    rtcToolsRuntime [options]\n\n"
            "This program invokes RTC Tools, and then simulates the configured\n"
            "model, computes the cost function value and its gradient, performs\n"
			"an optimization depending on the options provided.\n\n"
            "Options:\n"
            "    -?                                  Show this help.\n\n"
            "Providing options is optional. When specified, the options provided\n"
            "will override settings defined in the configuration xml-files.\n"
            "Otherwise, settings defined in those files will be used.\n\n"
         << endl;
}

// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argc, char *argv[]) {
    //_CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
	//_crtBreakAlloc = 91727;
	//_crtBreakAlloc = 3079;

	versionInfo info;
#ifdef PROPRIETARY
	info = versionInfo(true, STRINGIFY(OSS_Version), STRINGIFY(OSS_Revision));
#else
	info = versionInfo(false, STRINGIFY(OSS_Version), STRINGIFY(OSS_Revision));
#endif

    int errorCode = rtcToolsRuntime::main(argc, argv, info);
    
    return errorCode;
}

