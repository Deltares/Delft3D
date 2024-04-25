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
//------------------------------------------------------------------------------
//  DelftStream
//  Stream Class Definition
//
//  Irv.Elshoff@deltares.nl
//  10 nov 05
//
//------------------------------------------------------------------------------


#ifndef STREAM_H
#define STREAM_H

#include <platformst.h>

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include <pthread.h>

#if !defined (IRIX)
#include <cstddef>
#endif

#ifdef WITH_MPI
#undef SEEK_CUR
#undef SEEK_END
#undef SEEK_SET
#include <mpi.h>
#endif


//------------------------------------------------------------------------------
//  Stream object


class Stream {
    public:
        typedef enum {
            MAXHANDLE = 100,
            X = 0
            } StreamConstants;

        typedef enum {
            UNDEFINED = 200,
            TCPIP,
            MPI,
            DEFAULT = TCPIP
            } StreamType;

        Stream (
            StreamType streamtype,
            void (*errorfunction) (char *) = NULL,
            void (*tracefunction) (char *) = NULL
            );
        Stream (
            StreamType streamtype,
            const char * hostport,
            void (*errorfunction) (char *) = NULL,
            void (*tracefunction) (char *) = NULL
            );

        ~Stream (void);

        void    Connect             (void);
        void    Send                (const char * buffer, int size);
        void    Receive             (char * buffer, int size);
        char *  LocalHandle         (void);
        char *  RemoteHandle        (void);

    private:
        static bool initialized;
        static pthread_mutex_t mutex;

        StreamType  streamtype;
        bool        connected;

        typedef struct {
            char *      handle;     // TCP/IP & MPI
            Sockaddr    addr;       // TCP/IP packed protocol, host address, port
            int         sock;       // TCP/IP socket for I/O operations
            int         rank;       // MPI
            int         seqn;       // MPI
            } Address;

        Address     local;
        Address     remote;

        void    (*errorfunction)    (char *);
        void    (*tracefunction)    (char *);

        // Internal functions

        void    initialize          (void);
        void    construct_TCPIP     (Stream *);
        void    connect_TCPIP       (Stream *, const char *);
        void    first_receive_TCPIP (Stream *);
        void    receive_TCPIP       (Stream *, char *, int);

#ifdef WITH_MPI
        void    construct_MPI       (Stream *);
        void    connect_MPI         (Stream *, const char *);
        void    first_receive_MPI   (Stream *);
        void    receive_MPI         (Stream *, char *, int);
#endif

        char *  dotipaddr           (IPaddr);
        char *  hostname            (void);
        char *  lookup_dotaddr      (char *);
        char *  lookup_host         (char *);
        int     next_seqn           (void);
        void    parse_name          (char *, char *, int *);
        void    error               (char *, ...);
        void    trace               (char *, ...);
    };

#endif

