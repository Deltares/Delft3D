// Copyright (C) 2010 Deltares
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
 * @brief xxx
 * @author Camiel van Breugel
 * @version 1.0
 * @date 2010
 */

#ifndef RTCTOOLS_DLL_DEFS_H_
#define RTCTOOLS_DLL_DEFS_H_

//include preprocessor statement "RTCTOOLS_DLL_DLL" for using or compiling as library 
//include preprocessor statement "RTCTOOLS_DLL_DLL_EXPORTS" for compiling as library 

// Generic helper definitions for shared library support
#if defined _WIN32 || defined __CYGWIN__
  #define RTCTOOLS_DLL_HELPER_DLL_IMPORT __declspec(dllimport)
  #define RTCTOOLS_DLL_HELPER_DLL_EXPORT __declspec(dllexport)
#else
  #if __GNUC__ >= 4
    #define RTCTOOLS_DLL_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
    #define RTCTOOLS_DLL_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
  #else
    #define RTCTOOLS_DLL_HELPER_DLL_IMPORT
    #define RTCTOOLS_DLL_HELPER_DLL_EXPORT
  #endif
#endif

// Now we use the generic helper definitions above to define RTCTOOLS_DLL_API.
// RTCTOOLS_DLL_API is used for the public API symbols. It either DLL imports or DLL exports (or does nothing for static build)

#ifdef RTCTOOLS_DLL_DLL // defined if RTCTOOLS_DLL is compiled as a DLL
  #ifdef RTCTOOLS_DLL_DLL_EXPORTS // defined if we are building the RTCTOOLS_DLL library (instead of using it)
    #define RTCTOOLS_DLL_API RTCTOOLS_DLL_HELPER_DLL_EXPORT
  #else
    #define RTCTOOLS_DLL_API RTCTOOLS_DLL_HELPER_DLL_IMPORT
  #endif // RTCTOOLS_DLL_DLL_EXPORTS
#else // RTCTOOLS_DLL_DLL is not defined: this means RTCTOOLS_DLL is a static library.
  #define RTCTOOLS_DLL_API
#endif // RTCTOOLS_DLL_DLL

#endif /* RTCTOOLS_DLL_DEFS_H_ */
