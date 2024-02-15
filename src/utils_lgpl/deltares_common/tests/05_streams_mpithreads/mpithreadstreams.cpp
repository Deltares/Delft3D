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
//  Delft-Stream - MPI Stream test program using threads
//
//  This program tests the MPI implementation.  Two nodes are used, called
//  left (rand 0) and right (rank 1).  On each node, a number of threads are
//  created, half of which are leaders, the other half followers.  The leaders
//  create a new stream and put the handle in a table, then send it to the
//  corresponding follower on the other node using an MPI Send.  Followers wait
//  for this message, them complete the stream connection.  They then send both
//  local and remote handles back to their leaders, who check to make sure both
//  sides have the same notion of the stream endpoints.
//
//  Usage: mpithreads [-n <leaderthreads>]
//
//
//  Irv.Elshoff@deltares.nl
//  3 aug 06
//
//------------------------------------------------------------------------------


#if defined (WITH_MPI)

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include "stream.h"

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>


#define MAXSTR          1000
#define MAXTHR          100     // max number of threads


void    Abort           (char *, ...);
void    Log             (int, char *, ...);
void    Usage           (char *);
void    InitThreads     (void);
void    TestStreams     (void);
void    StreamError     (char *);
void    StreamTrace     (char *);
void *  Leader          (void *);
void *  Follower        (void *);


enum {
    LEADER_HANDLE = 170001
    };


typedef struct {
    pthread_t thread;           // key containing thread ID (small integer)

    Stream *  stream;           // local stream object
    char      lochand [Stream::MAXHANDLE]; // local stream handle
    char      remhand [Stream::MAXHANDLE]; // remote stream handle

    } Thread;


struct {
    int     nleaders;           // number of leader threads on each node
    int     verbose;            // verbosity level

    int     rank;               // MPI rank (0 or 1)
    int     otherside;          // MPI rank of the other size (1 or 0)
    bool    mpiInit;            // true iff MPI initialized

    Thread leader [MAXTHR];     // key containing thread ID (small integer)
    Thread follower [MAXTHR];   // key containing thread ID (small integer)
    pthread_key_t thidkey;      // key containing thread ID (small integer)
    pthread_attr_t thattr;      // attributes for thread creation

    } Global;


//------------------------------------------------------------------------------


int
main (
    int     argc,
    char    *argv[],
    char    *envp[]
    ) {

    // Parse command-line arguments

    Global.nleaders = 10;
    Global.verbose  = 0;
    Global.mpiInit  = false;

    int c;
    while ((c = getopt (argc, argv, "n:v?")) != -1) {
        switch (c) {
            case 'n':
                Global.nleaders = atoi (optarg);
                break;
            case 'v':
                Global.verbose++;
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

    // Initialize multi-threaded MPI environment

    int required = MPI_THREAD_MULTIPLE;
    int provided;
    MPI_Init_thread (&argc, &argv, required, &provided);
    if (provided != required)
        Abort ("MPI does not support threading the way we need it to");

    int size;
    MPI_Comm_size (MPI_COMM_WORLD, &size);
    if (size < 2)
        Abort ("Too few MPI processes (need at least two)");

    MPI_Comm_rank (MPI_COMM_WORLD, &Global.rank);
    Global.mpiInit = true;

    // Run test on two nodes

    if (Global.rank <= 1) {
        InitThreads ();
        Global.otherside = 1 - Global.rank;
        TestStreams ();
        }

    MPI_Finalize ();
    exit (0);
    }


//------------------------------------------------------------------------------

void
InitThreads (
    void
    ){

    static int id = 0;

    // Create a thread-specific key for storing the thread ID

    if (pthread_key_create (&Global.thidkey, NULL) != 0)
        Abort ("Pthreads error: Cannot create thread-specific key, errno=%d", errno);
    if (pthread_setspecific (Global.thidkey, (void *) &id) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for main thread, errno=%d", errno);

    // Get thread attributes

    if (pthread_attr_init (&Global.thattr) != 0)
        Abort ("Pthreads error: Cannot initialize thread attributes, errno=%d", errno);

    // Set scheduling scope

    if (pthread_attr_setscope (&Global.thattr, PTHREAD_SCOPE_SYSTEM) != 0)
        Abort ("Pthreads error: Cannot set thread scope attribute, errno=%d", errno);

    // Get thread stack size and extend it if the default is less than our minimum

    unsigned int stacksize;
    if (pthread_attr_getstacksize (&Global.thattr, &stacksize) != 0)
        Abort ("Pthreads error: Cannot get stack size, errno=%d", errno);

    unsigned int minstack = 10*1024*1024;    // ten megabytes
    if (stacksize < minstack) {
        if (pthread_attr_setstacksize (&Global.thattr, minstack) != 0)
            Abort ("Pthreads error: Cannot set stack size to %d bytes, errno=%d", minstack, errno);
        if (pthread_attr_getstacksize (&Global.thattr, &stacksize) != 0)
            Abort ("Pthreads error: Cannot get stack size (second time), errno=%d", errno);

        if (stacksize < minstack)
            Abort ("Thread stack size (%d bytes) is less than minimum (%d bytes)", stacksize, minstack);
        }

    Log (2, "Threads initialized, stacksize=%d", stacksize);
    }


//------------------------------------------------------------------------------


void
TestStreams (
    void
    ) {

    // Create a bunch of leader threads

    Log (2, "Creating leaders");
    for (int id = 1 ; id <= Global.nleaders ; id++) {
        if (pthread_create (&Global.leader[id].thread, &Global.thattr, &Leader, (void *) id) != 0)
            Abort ("Pthreads error: Cannot create leader thread %d, errno=%d", id, errno);
        }

    // Create a bunch of follower threads

    Log (2, "Creating followers");
    for (int id = 1 ; id <= Global.nleaders ; id++) {
        if (pthread_create (&Global.follower[id].thread, &Global.thattr, &Follower, (void *) id) != 0)
            Abort ("Pthreads error: Cannot create follower thread %d, errno=%d", id, errno);
        }

    // Wait for all threads to terminate

    Log (2, "Waiting for leaders and followers to terminate");
    for (int id = 1 ; id <= Global.nleaders ; id++) {
         if (pthread_join (Global.leader[id].thread, NULL) != 0)
            Abort ("Pthreads error: Cannot join with leader thread, errno=%d", errno);
         if (pthread_join (Global.follower[id].thread, NULL) != 0)
            Abort ("Pthreads error: Cannot join with follower thread, errno=%d", errno);
        }

    // Print stream handle table for analysis by external program

    if (Global.rank == 0) {
        for (int id = 1 ; id <= Global.nleaders ; id++)
            Log (0, "xx L: %s -> %s", Global.leader[id].lochand, Global.leader[id].remhand);
        for (int id = 1 ; id <= Global.nleaders ; id++)
            Log (0, "xx F: %s -> %s", Global.follower[id].lochand, Global.follower[id].remhand);
        }
    else {
        for (int id = 1 ; id <= Global.nleaders ; id++)
            Log (0, "xx L: %s -> %s", Global.leader[id].remhand, Global.leader[id].lochand);
        for (int id = 1 ; id <= Global.nleaders ; id++)
            Log (0, "xx F: %s -> %s", Global.follower[id].remhand, Global.follower[id].lochand);
        }

    Log (2, "Done");
    }


//------------------------------------------------------------------------------


void *
Leader (
    void *  argument
    ) {

    int id = ((long) argument); // thread ID
    if (pthread_setspecific (Global.thidkey, (void *) &id) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for leader thread %d, errno=%d", id, errno);

    Log (1, "Leader %d starting", id);

    // Create a new, unpaired stream

    Global.leader[id].stream = new Stream (Stream::MPI, &StreamError, &StreamTrace);
    char * lochand = Global.leader[id].stream->LocalHandle ();
    strncpy (Global.leader[id].lochand, lochand, Stream::MAXHANDLE);

    // Send the handle to my follower on the other node

    int otherside = 1 - Global.rank;
    MPI_Send (lochand, strlen (lochand), MPI_CHAR, Global.otherside, LEADER_HANDLE, MPI_COMM_WORLD);

    // Complete the connection

    Global.leader[id].stream->Connect ();
    char * remhand = Global.leader[id].stream->RemoteHandle ();
    strncpy (Global.leader[id].remhand, remhand, Stream::MAXHANDLE);

    // Make sure local handle has not changed

    if (strcmp (lochand, Global.leader[id].stream->LocalHandle ()) != 0)
        Abort ("Local handle has changed");

    Log (1, "Leader %d terminating", id);
    return NULL;
    }


void *
Follower (
    void *  argument
    ) {

    int id = ((long) argument); // thread ID
    int iid = -id;
    if (pthread_setspecific (Global.thidkey, (void *) &iid) != 0)
        Abort ("Pthreads error: Cannot set thread-specific key for follower thread %d, errno=%d", id, errno);

    Log (1, "Follower %d starting", id);

    // Get handle from my leader on the other node

    char * remhand = Global.follower[id].remhand;
    MPI_Status status;
    MPI_Recv (remhand, Stream::MAXHANDLE, MPI_CHAR, Global.otherside, LEADER_HANDLE, MPI_COMM_WORLD, &status);

    // Create paired stream

    Global.follower[id].stream = new Stream (Stream::MPI, remhand, &StreamError, &StreamTrace);
    char * lochand = Global.follower[id].stream->LocalHandle ();
    strncpy (Global.follower[id].lochand, lochand, Stream::MAXHANDLE);

    Log (1, "Follower %d terminating", id);
    return NULL;
    }


//------------------------------------------------------------------------------


void
Usage (
    char * progname
    ) {
    printf ("Usage: %s <options>\n\
    Options:\n\
        -n <leaders>  Number of leaders on each node\n\
        -v            Verbose mode (more -v more verbose)\n\
        \n", progname);
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

    Log (3, "Stream: %s", message);
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
    Log (0, "ABORT: %s", string);
    if (Global.mpiInit) MPI_Finalize ();
    exit (1);
    }


void
Log (
    int level,
    char * reason,
    ...
    ) {

    if (Global.verbose < level)  return;

    va_list     arguments;
    char        string [MAXSTR];

    int * idp;
    if ((idp = (int *) pthread_getspecific (Global.thidkey)) == NULL) {
        printf ("ABORT: Pthreads error: Cannot get thread-specific key for thread in Log(), errno=%d\n", errno);
        va_start (arguments, reason);
        vsprintf (string, reason, arguments);
        va_end (arguments);
        printf ("ABORT: Original Message=\"%s\"\n", string);
        fflush (stdout);
        exit (1);
        }

    int id = *idp; // thread ID

    char thname [10];
    if (id < 0) {
        id *= -1;
        sprintf (thname, "F%d", id);
        }
    else if (id == 0)
        sprintf (thname, "mt");
    else
        sprintf (thname, "L%d", id);

    va_start (arguments, reason);
    vsprintf (string, reason, arguments);
    va_end (arguments);
    printf ("@[N%d,%s]:\t%s\n", Global.rank, thname, string);
    fflush (stdout);
    }

#else

main () {}
#endif

