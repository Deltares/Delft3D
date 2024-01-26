//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: log.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Log Object - Definitions
//
//  Irv.Elshoff@Deltares.NL
//  30 oct 11
//------------------------------------------------------------------------------

#pragma once
// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#if HAVE_CONFIG_H
#   include "config.h"
#endif

#include "bmi.h" //For enum Level
#include "clock.h"
#include <cstdio>
#include <pthread.h>
#ifdef WIN32
#include "Windows.h"
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern "C" {
	typedef void(STDCALL * WriteCallback)(char* time, char* message, unsigned int level);
}

class Log {
	

public:
	Log( FILE * output, Clock * clock, Level level = INFO, Level feedbackLevel = INFO );

	~Log( void );

	Level GetLevel( void );

	void SetLevel( Level level );

	Level GetFeedbackLevel( void );

	void SetFeedbackLevel( Level feedbackLevel );

	void RegisterThread( const char * id );

	void RenameThread( const char * id );

	void UnregisterThread( void );

	const char * AddLeadingZero(int, int);

	bool Write(Level level, int rank, const char * format, ...);

	void SetWriteCallBack( WriteCallback writeCallback );

	void SetExternalLogger( BMILogger logger );

    void logLevelToString( int level, char ** levelString );


private:
	FILE *        output;
	Clock *       clock;
	Level         level;
	Level         feedbackLevel;

	pthread_key_t thkey;      // contains key for thread-specific log data
	WriteCallback writeCallback;
	BMILogger        externalLogger;


public:
	char *        redirectFile;
};

#ifdef WIN32
#   define DllExport   __declspec( dllexport )
#  define strdup _strdup
#else
#   define DllExport
#endif

extern "C" {
	DllExport void set_dimr_logger(Log *);
	DllExport void set_logger_callback(WriteCallback);
}
