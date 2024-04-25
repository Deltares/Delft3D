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
//  Stream Class Implementation - MPI-only Version
//
//  The file "stream-mpi.cpp" is a test/debug version that only supports MPI
//  and uses the MPI-1.2 C bindings.
//
//  Irv.Elshoff@deltares.nl
//  24 oct 05
//
//------------------------------------------------------------------------------


#include <stream.h>

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>


#define CONNECT_TAG 174000000           // magic number for MPI mode


//------------------------------------------------------------------------------
//  Constants


enum {
    MAXSTRING   = 1000     // max string length in bytes
    };


//------------------------------------------------------------------------------
//  Constructors and Destructor - Public Functions


Stream::Stream (
    StreamType streamtype,
    void (*errorfunction) (char *),
    void (*tracefunction) (char *)
    ) {

    // Create an unconnected (local) stream end-point.
    // The caller has to pass the resulting handle to the remote peer
    // by other means, and then invoke the Connect method to complete
    // the stream connection.  Once the remote peer instantiates a
    // Stream using the handle sends and receives can be done.
    // This call does not block; Connect does.

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset (&this->local,  0, sizeof this->local);
    memset (&this->remote, 0, sizeof this->remote);

    this->connected = false;
    this->streamtype = streamtype;

    this->local.seqn = next_seqn ();
    MPI_Comm_rank (MPI_COMM_WORLD, &this->local.rank);
    this->local.handle = new char [Stream::MAXHANDLE];
    sprintf (this->local.handle, "mpi:%d:%d", this->local.rank, this->local.seqn);
    this->connected = false;

    if (this->tracefunction != NULL)
        trace ("Created handle %s", this->local.handle);
    }


Stream::Stream (
    StreamType streamtype,
    const char *  handle,
    void (*errorfunction) (char *),
    void (*tracefunction) (char *)
    ) {

    // Create the other half of a two-sided stream connection.
    // This is a blocking call that waits for a Connect from the peer that
    // initially created the stream.

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset (&this->local,  0, sizeof this->local);
    memset (&this->remote, 0, sizeof this->remote);

    this->streamtype = streamtype;
    this->remote.handle = new char [Stream::MAXHANDLE];
    strcpy (this->remote.handle, handle);
    if (sscanf (handle, "%*[^:]:%d:%d", &this->remote.rank, &this->remote.seqn) != 2)
        error ("Invalid MPI stream handle \"%s\"", handle);

    this->local.seqn = next_seqn ();
    MPI_Comm_rank (MPI_COMM_WORLD, &this->local.rank);
    this->local.handle = new char [Stream::MAXHANDLE];
    sprintf (this->local.handle, "mpi:%d:%d", this->local.rank, this->local.seqn);
    this->connected = true;

    // Send first message with local handle so peer knows who I am

    MPI_Send ((void *) this->local.handle, Stream::MAXHANDLE, MPI_CHAR, this->remote.rank, CONNECT_TAG + this->remote.seqn, MPI_COMM_WORLD);

    if (this->tracefunction != NULL)
        trace ("Created handle %s attached to %s", this->local.handle, this->remote.handle);
    }


Stream::~Stream (
    void
    ) {

    if (local.handle  != NULL) delete local.handle;
    if (remote.handle != NULL) delete remote.handle;
    }


//------------------------------------------------------------------------------
//  Send and Receive - Public Functions


void
Stream::Connect (
    void
    ) {

    // This function is called by the first end-point of a stream to
    // complete the connection. This is a blocking call.

    this->remote.handle = new char [Stream::MAXHANDLE];

    MPI_Status status;
    MPI_Recv ((void *) this->remote.handle, Stream::MAXHANDLE, MPI_CHAR, MPI_ANY_SOURCE, CONNECT_TAG + this->local.seqn, MPI_COMM_WORLD, &status);

    if (sscanf (this->remote.handle, "%*[^:]:%d:%d", &this->remote.rank, &this->remote.seqn) != 2)
        error ("Invalid MPI stream handle \"%s\"", this->remote.handle);

    this->connected = true;

    if (this->tracefunction != NULL)
        trace ("Created handle %s attached to %s", this->local.handle, this->remote.handle);
    }


void
Stream::Send (
    const char * buffer,
    int     length
    ) {

    if (! this->connected)
        error ("Attempting to send to an unconnected stream (local side is %s)", this->local.handle);

    if (this->tracefunction != NULL)
        trace ("Sending message from %s to %s (%d bytes)", this->local.handle, this->remote.handle, length);

    MPI_Send ((void *) buffer, length, MPI_CHAR, this->remote.rank, this->remote.seqn, MPI_COMM_WORLD);

    if (this->tracefunction != NULL)
        trace ("Sent message from %s to %s (%d bytes)", this->local.handle, this->remote.handle, length);
    }


void
Stream::Receive (
    char *  buffer,
    int     length
    ) {

    if (this->tracefunction != NULL)
        trace ("Waiting for message from %s to %s (%d bytes)", this->remote.handle, this->local.handle, length);

    MPI_Status status;
    MPI_Recv ((void *) buffer, length, MPI_CHAR, this->remote.rank, this->local.seqn, MPI_COMM_WORLD, &status);

    int count;
    MPI_Get_count (&status, MPI_CHAR, &count);
    if (length != count)
        error ("Receive gets message from %s to %s of different length (%d) than requested (%d)", this->remote.handle, this->local.handle, count, length);

    if (this->tracefunction != NULL)
        trace ("Got message from %s on %s (%d bytes)", this->remote.handle, this->local.handle, length);
    }


//------------------------------------------------------------------------------
//  Other Public Functions


char *
Stream::LocalHandle (
    void
    ) {

    return this->local.handle;
    }


char *
Stream::RemoteHandle (
    void
    ) {

    if (this->connected)
        return this->remote.handle;

    else {
        switch (streamtype) {
            case Stream::MPI:
                return "[mpi]:*:*";
            default:
                return NULL;
            }
        }
    }


//------------------------------------------------------------------------------
//  Private functions


int
Stream::next_seqn (
    void
    ) {

    static int seqn = 101;      // for MPI
    return seqn++;
    }


void
Stream::trace (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    this->tracefunction (string);
    }



void
Stream::error (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTRING];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    if (this->errorfunction != NULL)
        this->errorfunction (string);

    else {
        fflush (stdout);
        fprintf (stderr, "Fatal error: %s\n", string);
        fflush (stderr);
        exit (2);
        }
    }

