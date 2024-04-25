//-------------------------------------------------------------------------------
//  DelftOnline -- Internal Include File
//
//  Irv.Elshoff@Deltares.NL
//  27 jun 12
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

#pragma once

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <new>

#if defined (WIN32) || defined(WIN64)
#   include <process.h>
#   include <ws2tcpip.h>

#   define DELFTONLINE_LIB
#   if !defined(errno)
#      define errno    (WSAGetLastError ())
#   endif
#   define getpid   _getpid
#   define random   rand
#   define srandom  srand
#   define strdup   _strdup
#   define usleep Sleep
#else
#   include <netdb.h>
#   include <unistd.h>

#   define INVALID_SOCKET (-1)

#endif


#define DOL_SOURCE

#include "DelftOnline.h"

#include "clock.h"
#include "linkedlist.h"
#include "log.h"
#include "sortedbag.h"


namespace DOL {

typedef unsigned long long Timestamp;       // microseconds, since start of epoch

const int maxMessagePayload = 10*1000*1000; // size of static buffer for IPC


//-------------------------------------------------------------------------------


#define LOCK { \
    if (pthread_mutex_lock (&this->mutex) != 0) \
        throw new Exception (true, "Cannot lock mutex"); \
    }

#define UNLOCK { \
    if (pthread_mutex_unlock (&this->mutex) != 0) \
        throw new Exception (true, "Cannot unlock mutex"); \
    }


//-------------------------------------------------------------------------------


#define SERIALIZE_STRING(X) { \
    int len = strlen (X) + 1; \
    if ((size += len) >= bufsize) return 0; \
    strcpy (cp, (X)); \
    cp += len; \
    }

#define UNSERIALIZE_STRING(X) { \
    int len = strlen (cp) + 1; \
    (X) = (len > 1) ? strdup (cp) : NULL; \
    cp += len; \
    }


//-------------------------------------------------------------------------------
//  Internal Routines


bool
LookupHostname (
    char *  hostname,
    struct sockaddr * addr
    );

const char *
MessageTypeString (
    Message::Type type
    );

size_t
Receive (
    int     sock,
    Message::Header * mesg
    );

const char *
ResolvePathName (
    const char * pathname,
    const char * curDir
    );

void
Send (
    int     sock,
    Message::Header * mesg
    );

}
