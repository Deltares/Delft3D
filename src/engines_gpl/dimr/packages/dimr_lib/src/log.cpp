//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
// $Id: log.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Log Object - Implementation
//
//  Irv.Elshoff@Deltares.NL
//  25 oct 11
//------------------------------------------------------------------------------


#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#if HAVE_CONFIG_H
#   include "config.h"
#endif


#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "clock.h"
#include <ctime>
#if HAVE_CONFIG_H
#   include <sys/wait.h>
#   include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <fstream>
#include <string>
#include <mpi.h>
#include <map>
#include "dimr_control_block.h"
#include "dimr_components.h" 
#include "dimr_coupler.h"
#include "dimr_couplers.h"



#if defined (WIN32)
#   define strdup _strdup
#endif

Log::Log( FILE *  output, Clock * clock, Level level, Level feedbackLevel) {
	this->output = output;
	this->clock = clock;
	this->level = level;
	this->feedbackLevel = feedbackLevel;
	this->redirectFile = NULL;

	this->writeCallback = NULL;
	this->externalLogger = NULL;

	if (pthread_key_create(&this->thkey, NULL) != 0)
		throw Exception(true, Exception::ERR_PTHREADS, "Pthreads error in Log: Cannot create thread-specific key: %s", strerror(errno));
	if (pthread_setspecific(this->thkey, NULL) != 0)
		throw Exception(true, Exception::ERR_PTHREADS, "Pthreads error in Log constructor: Cannot set thread-specific key: %s", strerror(errno));
}


Log::~Log( void ) {
	this->writeCallback = NULL;
	// nothing to do
}


//------------------------------------------------------------------------------


Level Log::GetLevel( void ) {
	return this->level;
}


void Log::SetLevel( Level level ) {
	this->level = min(max(level,ALL),FATAL);

    char * levelString = new char[MAXSTRING];
    logLevelToString(this->level, &levelString);
	this->Write(DEBUG, 0, "Log level set to %s", levelString);
    delete [] levelString;
}


Level Log::GetFeedbackLevel( void ) {
	return this->feedbackLevel;
}


void Log::SetFeedbackLevel( Level feedbackLevel) {
	this->feedbackLevel = min(max(feedbackLevel,ALL), NONE);

    char * levelString = new char[MAXSTRING];
    logLevelToString(this->feedbackLevel, &levelString);
	this->Write(DEBUG, 0, "feedbackLevel set to %s", levelString);
    delete [] levelString;
}


void Log::RegisterThread( const char * id ) {
	char * idCopy = strdup(id);
	if (pthread_setspecific(this->thkey, (void *)idCopy) != 0)
		throw Exception(true, Exception::ERR_PTHREADS, "Pthreads error in Log::RegisterThread: Cannot set thread-specific key: %s", strerror(errno));
}


void Log::RenameThread( const char * id ) {
	this->UnregisterThread();
	this->RegisterThread(id);
}


void Log::UnregisterThread( void ) {
	char * id = (char *)pthread_getspecific(this->thkey);
	if (id == NULL)
		throw Exception(true, Exception::ERR_PTHREADS, "Log thread key not set in UnregisterThread");

	free(id);
}


bool Log::Write( Level level, int rank, const char *  format, ... ) {
	const int bufsize = 256 * 1024;
	char * buffer = new char[bufsize]; // really big temporary buffer, just in case

	va_list arguments;
	va_start(arguments, format);
	int len = vsnprintf(buffer, bufsize - 1, format, arguments);
	va_end(arguments);
	buffer[bufsize - 1] = '\0';

	if (this->externalLogger){
		this->externalLogger(level, buffer);
	}

	if (this->level > level) {
	    delete[] buffer;
		return false;
	}

	char * clock = new char[100];
	clock[0] = '\0';
	this->clock->Now(clock);

	char * threadID = (char *)pthread_getspecific(this->thkey);
	if (threadID == NULL)
		threadID = "<anonymous>";

	if (redirectFile != NULL) {
		// Append to file:
		FILE * fp;
		fp = fopen(redirectFile, "a");
        fprintf (fp, "Dimr [%s] #%d >> %s\n",
            clock,
			rank,
			buffer
			);
		fclose(fp);
    } else {
		// Write to stdout:
        fprintf (this->output, "Dimr [%s] #%d >> %s\n",
            clock,
			rank,
			buffer
			);
		fflush(this->output);
	}

	// Write to Callback (if registered)
	// Use separate write Level
	if (this->writeCallback && this->feedbackLevel <= level) {
		this->writeCallback(&clock[0], buffer, level);
	}

	delete[] buffer;
	delete[] clock;
	return true;
}


void Log::SetWriteCallBack( WriteCallback writeCallback ) {
	this->writeCallback = writeCallback;
	this->Write(DEBUG, 0, "WriteCallBack is set");
}


void Log::SetExternalLogger( BMILogger logger ) {
	this->externalLogger = logger;
	this->Write(DEBUG, 0, "External logger is set");
}


void Log::logLevelToString( int level, char ** levelString ){
    strcpy(*levelString, "UNKNOWN");
    if (level <= 0) strcpy(*levelString, "ALL");
    switch(level) {
    case 0: { strcpy(*levelString, "ALL"); break; }
    case 1: { strcpy(*levelString, "DEBUG"); break;}
    case 2: { strcpy(*levelString, "INFO"); break;}
    case 3: { strcpy(*levelString, "WARNING"); break;}
    case 4: { strcpy(*levelString, "ERRORS"); break;}
    case 5: { strcpy(*levelString, "FATAL"); break; }
    case 6: { strcpy(*levelString, "NONE"); break; }
    }
    if (level >= 5) strcpy(*levelString, "FATAL");
}
