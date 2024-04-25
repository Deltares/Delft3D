//-------------------------------------------------------------------------------
//  DelftOnline
//  Log Class -- IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  24 apr 12
//-------------------------------------------------------------------------------
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
// $Id$
// $HeadURL$


#include "dol.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


namespace DOL {

Log::Log (
    FILE *  output,
    Clock * clock,
    Verbosity level
    ) {

    this->output    = output;
    this->clock     = clock;
    this->level     = level;

    if (pthread_key_create (&this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log: Cannot create thread-specific key: %s", strerror (errno));
    if (pthread_setspecific (this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log constructor: Cannot set thread-specific key: %s", strerror (errno));
    }


Log::~Log (
    void
    ) {

    // nothing to do
    }


//------------------------------------------------------------------------------


void
Log::RegisterThread (
    const char * id
    ) {

    char * idCopy = strdup (id);
    if (pthread_setspecific (this->thkey, (void *) idCopy) != 0)
        throw new Exception (true, "Pthreads error in Log::RegisterThread: Cannot set thread-specific key: %s", strerror (errno));
    }


void
Log::UnregisterThread (
    void
    ) {

    char * id = (char *) pthread_getspecific (this->thkey);
    if (id == NULL)
        throw new Exception (true, "Log thread key not set in UnregisterThread");

    free (id);

    if (pthread_setspecific (this->thkey, NULL) != 0)
        throw new Exception (true, "Pthreads error in Log::UnregisterThread: Cannot set thread-specific key: %s", strerror (errno));
    }


void
Log::RenameThread (
    const char * id
    ) {

    this->UnregisterThread ();
    this->RegisterThread (id);
    }


bool
Log::Write (
    Verbosity level,
    const char *  format,
    ...
    ) {

    if (level > this->level)
        return false;

    const int bufsize = 256*1024;
    char * buffer = new char [bufsize]; // really big temporary buffer, just in case

    va_list arguments;
    va_start (arguments, format);
    vsnprintf (buffer, bufsize-1, format, arguments);
    va_end (arguments);
    buffer[bufsize-1] = '\0';

    char clock [100];
    this->clock->Now (clock);

    char * threadID = (char *) pthread_getspecific (this->thkey);
    if (threadID == NULL)
        threadID = (char *) "<anon>";

    fprintf (this->output, "DOL [%s] <%s>  %s\n",
                        clock,
                        threadID,
                        buffer
                        );

    fflush (this->output);
    delete [] buffer;
    return true;
    }

}
