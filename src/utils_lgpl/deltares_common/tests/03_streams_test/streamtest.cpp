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
//  Delft3D-MORFLOW - Stream test program
//
//  Irv.Elshoff@deltares.nl
//  3 aug 06
//
//------------------------------------------------------------------------------


#include "stream.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#if defined (WIN32)
#include <windows.h>
#include <process.h>
#include <atltime.h>
#else
#include <sys/time.h>
#include <sys/wait.h>
#endif

#define MAXSTR          1000


typedef long long Timestamp;


void        Abort               (char *, ...);
Timestamp   GetTime             (void);
void        Log                 (char *, ...);
void        MasterProcess       (Stream *, int, int);
void        SlaveProcess        (Stream::StreamType, char *, int, int, int);
void        StreamError         (char *);
void        StreamTrace         (char *);
void        Usage               (char *);

#if defined (WITH_MPI)
void        StartMPI            (void);
#endif
void        StartTCPIP          (void);


//------------------------------------------------------------------------------


int     bufferSize      = 10 * 1024;
int     numMessages     = 128 * 1024;
int     verbose         = 0;
char *  streamHandle    = NULL;
char *  remoteHost      = NULL;
char    hostname [MAXSTR+1];
bool    mpi_initialized = false;
char *  remote_shell    = "rsh";
char *  exePath         = NULL;

int
main (
    int     argc,
    char    *argv[],
    char    *envp[]
    ) {

    // Parse command-line arguments

    Stream::StreamType streamtype = Stream::UNDEFINED;
    int c;
        int optind;
        char * s;
        optind=1;
    //while ((c = getopt (argc, argv, "b:Mm:n:r:SsTv?")) != -1) {
    while ( optind < argc ) {
                s = argv[optind];
        if (*s == '-') s++;
                c = int(*s);
        switch (c) {
            case 'b':
                                optind++;
                bufferSize = atoi (argv[optind]);
                                optind++;
                break;
            case 'M':
                                optind++;
#if defined (WITH_MPI)
                streamtype = Stream::MPI;
#else
                Abort ("MPI  mode not supported in this build");
#endif
                break;
            case 'm':
                                optind++;
                streamtype = Stream::TCPIP;
                streamHandle = new char [strlen (argv[optind])];
                strcpy (streamHandle, argv[optind]);
                                optind++;
                break;
            case 'n':
                                optind++;
                numMessages = atoi (argv[optind]);
                                optind++;
                break;
            case 'r':
                                optind++;
#if defined (WIN32)
                Abort ("Remote host is not supported on Windows");
#else
                                streamtype = Stream::TCPIP;
                remoteHost = new char [strlen (argv[optind])];
                strcpy (remoteHost, argv[optind]);
                                optind++;
#endif
                                break;
            case 'S':
                                optind++;
                remote_shell = NULL;
                break;
            case 's':
                                optind++;
#if defined (WIN32)
                Abort ("Remote shell is not supported on Windows");
#else
                                remote_shell = "ssh";
#endif
                                break;
            case 'T':
                                optind++;
                streamtype = Stream::TCPIP;
                break;
            case 'v':
                                optind++;
                verbose++;
                break;
            case '?':
                Usage (argv[0]);
                exit (0);

            default:
                Usage (argv[0]);
                Abort ("Invalid command-line arguments");
            }
        }

    if (optind < argc) {
        Usage (argv[0]);
        Abort ("Invalid command-line argument");
        }


#if defined (WIN32)
        // Always without remote_shell
        remote_shell = NULL;
    char * w32ExePath = new char [strlen (argv[0])];
        exePath = w32ExePath;
#else
    if (gethostname (hostname, sizeof hostname) != 0)
        Abort ("Cannot get hostname");
#endif

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            StartMPI ();
            break;
#endif
        case Stream::TCPIP:
            StartTCPIP ();
            break;
        default:
            Abort ("Either TCP/IP (-T) or MPICH-2 (-M) must be specified");
        }

    exit (0);
    }


//------------------------------------------------------------------------------


void
StartTCPIP (
    void
    ) {

    // Play (remote) slave role if a stream handle was specified

    if (streamHandle != NULL) {
        if (remoteHost != NULL)
            Abort ("Cannot specify both remote host and master handle");

        SlaveProcess (Stream::TCPIP, streamHandle, numMessages, bufferSize, verbose);
        exit (0);
        }

#if defined (WIN32)
#else
        // Get full run-time path name for this executable

    if ((exePath = getenv ("_")) == NULL)
        Abort ("Cannot get executable name from environment ($_)");

    if (exePath[0] != '/') {
        char * pwd;
        if ((pwd = getenv ("PWD")) == NULL)
            Abort ("Cannot get working directory ($PWD) from environment");

        char * buffer = new char [strlen (pwd) + strlen (exePath) + 2];
        sprintf (buffer, "%s/%s", pwd, exePath);
        exePath = buffer;
        }
#endif

    // Create a stream

    Stream * stream;
    if (verbose > 1)
        stream = new Stream (Stream::TCPIP, &StreamError, &StreamTrace);
    else
        stream = new Stream (Stream::TCPIP, &StreamError);

    if (verbose)
        Log ("Master stream (TCP/IP): local=\"%s\", remote=\"%s\"",
                    stream->LocalHandle (),
                    stream->RemoteHandle ()
                    );

    // Fork a remote shell process to run slave

    char * command = new char [MAXSTR];
    if (remote_shell == NULL) {
        sprintf (command, "%s -b %d -n %d -m %s %s %s",
                    exePath,
                    bufferSize,
                    numMessages,
                    stream->LocalHandle (),
                    (verbose > 0) ? "-v" : "",
                    (verbose > 1) ? "-v" : ""
                    );
        }
    else {
        sprintf (command, "%s -n %s '%s -b %d -n %d -m %s %s %s'",
                    remote_shell,
                    (remoteHost == NULL) ? "localhost" : remoteHost,
                    exePath,
                    bufferSize,
                    numMessages,
                    stream->LocalHandle (),
                    (verbose > 0) ? "-v" : "",
                    (verbose > 1) ? "-v" : ""
                    );
        }


#if defined (WIN32)
    STARTUPINFO    si;
    PROCESS_INFORMATION  pi;

    GetStartupInfo(&si);

    CreateProcess(      NULL, (LPWSTR) command, // Name of app to launch
                                        NULL,                                   // Default process security attributes
                                        NULL,                                   // Default thread security attributes
                                        FALSE,                                  // Don't inherit handles from the parent
                                        0,                                              // Normal priority
                                        NULL,                                   // Use the same environment as the parent
                                        NULL,                                   // Launch in the current directory
                                        &si,                                    // Startup Information
                                        &pi);                                   // Process information stored upon return
#else
    int pid;
    if ((pid = fork ()) == 0) {
        if (system (command) == 0)
            exit (0);
        else
            Abort ("Trouble running \"%s\"", command);
        }
#endif

    if (verbose)
        Log ("Master forked \"%s\"", command);

    // Play master role

    MasterProcess (stream, numMessages, bufferSize);

    // Wait for child to terminate

#if defined (WIN32)
#else
    if (wait ((int *) NULL) != pid)
        Abort ("Wait for child process %d fails\n", pid);
#endif

    if (verbose)
        Log ("Master terminating");

    return;
    }


#if defined (WITH_MPI)

#define MASTER  0
#define SLAVE   1

void
StartMPI (
    void
    ) {

    // Initialize multi-threaded MPI environment

#if defined (NO_CPP_MPI)
    int argc;
    char ** argv;
    int mpithreads = MPI_THREAD_MULTIPLE;
    int provided;
    MPI_Init_thread (&argc, &argv, mpithreads, &provided);
    if (provided != mpithreads)
        Abort ("MPI does not support threading the way we need it to");

    int rank;
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    int size;
    MPI_Comm_size (MPI_COMM_WORLD, &size);
#else
    int mpithreads = MPI::THREAD_MULTIPLE;
    if (MPI::Init_thread (mpithreads) != mpithreads)
        Abort ("MPI does not support threading the way we need it to");

    int rank = MPI::COMM_WORLD.Get_rank ();
    int size = MPI::COMM_WORLD.Get_size ();
#endif

    mpi_initialized = true;

    if (size < 2)
        Abort ("Too few MPI processes (need at least two)");

    if (rank == MASTER) {
        Stream * stream;
        if (verbose > 1)
            stream = new Stream (Stream::MPI, &StreamError, &StreamTrace);
        else
            stream = new Stream (Stream::MPI, &StreamError);

        char * handle = stream->LocalHandle ();

        if (verbose)
            Log ("Master stream (MPI): local=\"%s\", remote=\"%s\"",
                        stream->LocalHandle (),
                        stream->RemoteHandle ()
                        );

#if defined (NO_CPP_MPI)
        MPI_Send (handle, strlen (handle), MPI_CHAR, SLAVE, 0, MPI_COMM_WORLD);
#else
        MPI::COMM_WORLD.Send (handle, strlen (handle), MPI::CHAR, SLAVE, 0);
#endif

        MasterProcess (stream, numMessages, bufferSize);

        if (verbose)
            Log ("Master terminating");
        }

    else if (rank == SLAVE) {
        char handle [Stream::MAXHANDLE];
#if defined (NO_CPP_MPI)
        MPI_Status status;
        MPI_Recv (handle, Stream::MAXHANDLE, MPI_CHAR, MASTER, 0, MPI_COMM_WORLD, &status);
#else
        MPI::COMM_WORLD.Recv (handle, Stream::MAXHANDLE, MPI::CHAR, MASTER, 0);
#endif

        SlaveProcess (Stream::MPI, handle, numMessages, bufferSize, verbose);
        }

#if defined (NO_CPP_MPI)
    MPI_Finalize ();
#else
    MPI::Finalize ();
#endif
    }

#endif


//------------------------------------------------------------------------------


void
MasterProcess (
    Stream * stream,
    int     numMessages,
    int     bufferSize
    ) {

    // Determine the current wall time

    Timestamp startTime = GetTime ();

    stream->Connect ();

    // Read and write many messages

    char * buffer = new char [bufferSize];
    for (int i = 0 ; i < numMessages/2 ; i++) {
        stream->Receive (buffer, bufferSize);
        stream->Send (buffer, bufferSize);
        }

    // Print elapsed time

    Timestamp duration = GetTime () - startTime;
    Log ("Master %lld microsec per send/receive of %d bytes (%d in %lld sec)",
                    duration / numMessages,
                    bufferSize,
                    numMessages,
                    duration / 1000000
                    );
    }


void
SlaveProcess (
    Stream::StreamType  streamtype,
    char *  streamHandle,
    int     numMessages,
    int     bufferSize,
    int     verbose
    ) {

    // Connect to stream

    Stream * stream;
    if (verbose > 1)
        stream = new Stream (streamtype, streamHandle, &StreamError, &StreamTrace);
    else
        stream = new Stream (streamtype, streamHandle, &StreamError);

    if (verbose)
        Log ("Slave stream: local=\"%s\", remote=\"%s\"",
                    stream->LocalHandle (),
                    stream->RemoteHandle ()
                    );

    // Determine the current wall time

    Timestamp startTime = GetTime ();

    // Write and read many messages

    char * buffer = new char [bufferSize];
    for (int i = 0 ; i < bufferSize ; i++)
        buffer[i] = 0x55;

    for (int i = 0 ; i < numMessages/2 ; i++) {
        stream->Send (buffer, bufferSize);
        stream->Receive (buffer, bufferSize);
        }

    // Print elapsed time

    Timestamp duration = GetTime () - startTime;
    Log ("Slave  %lld microsec per send/receive of %d bytes (%d in %lld sec)",
                    duration / numMessages,
                    bufferSize,
                    numMessages,
                    duration / 1000000
                    );

    if (verbose)
        Log ("Slave terminating");
    }


//------------------------------------------------------------------------------


void
Usage (
    char * progname
    ) {
    printf ("Usage: %s <options>\n\
    Options:\n\
        -b <size>   Buffer size in bytes\n\
        -m <handle> Run as slave and communicate with master through handle\n\
        -n <num>    Number of messages\n\
        -r <host>   Run slave on remote host\n\
        -v          Verbose mode (more -v more verbose)\n\
        \n", progname);
    }


Timestamp
GetTime (
    void
    ) {
#if defined (WIN32)
        CTime t = CTime::GetCurrentTime();
    return ((Timestamp) t.GetTime());
#else
    struct timeval  tv;

    if (gettimeofday (&tv, NULL) != 0)
        Abort ("Cannot get time of day");

    return ((Timestamp) tv.tv_sec * 1000000) + tv.tv_usec;
#endif
    }


void
StreamError (
    char * message
    ) {

    Abort (message);
    }


void
StreamTrace (
    char * message
    ) {

    printf ("Stream: %s\n", message);
    fflush (stdout);
    }


void
Abort (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTR];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    printf ("ABORT: %s\n", string);
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
    if (mpi_initialized) MPI_Finalize ();
#else
    if (mpi_initialized) MPI::Finalize ();
#endif
#endif
    exit (1);
    }


void
Log (
    char * reason,
    ...
    ) {

    va_list     arguments;
    char        string [MAXSTR];

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);

    printf ("%s: %s\n", hostname, string);
    fflush (stdout);
    }


