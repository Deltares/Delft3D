//-------------------------------------------------------------------------------
//  DelftOnline -- Server API Routines
//
//  Irv.Elshoff@Deltares.NL
//  28 jun 12
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

namespace DOL {

bool Server::initialized = false;

void
Server::initialize (
    void
    ) {

    if (! Server::initialized) {
        Server::initialized = true;
#if defined (WIN32)
        WORD versionRequested = MAKEWORD (1, 1);
        WSADATA wsaData;
        if (WSAStartup (versionRequested, &wsaData) != 0)
            throw new Exception (true, "Windows Socket API initialization fails");
#endif
        }
    }


//-------------------------------------------------------------------------------


static char *
GenerateURL (
    int port
    );


static void *
clientGreeterInitiator (
    void *
    );


//-------------------------------------------------------------------------------
//  Constructors


Server::Server (
    bool            startRunning,
    bool            allowControl,
    Verbosity       verbosity,
    const char *    logFile,
    uint16_t        firstPort,
    uint16_t        lastPort
    ) {

    Server::initialize ();

    // Process constructor arguments

    if (! startRunning && ! allowControl)
        throw Error (true, "Server", "StartRunning and AllowControl are both false; how do you expect to step through milestones?");

    this->status.running      = startRunning;
    this->status.allowControl = allowControl;
    this->status.generation   = 0;
    this->status.clients      = 0;
    this->status.distance     = 0;
    this->status.milestone    = 0;

    this->barrier       = NULL;
    this->breakpoints   = new SortedBag ();
    this->clientID      = 0;
    this->clients       = new LinkedList ();
    this->clock         = new Clock ();
    this->description   = NULL;
    this->ready         = false;
    this->rootDir       = new Directory ("/", NULL);
    this->startRunning  = startRunning;

    if (logFile == NULL)
        this->log = new Log (stdout, this->clock, verbosity);

    else {
        FILE * f = fopen (logFile, "a");
        if (f == NULL)
            throw Error (true, "Server", "Cannot open log file \"%s\": %s", logFile, strerror(errno));

        this->log = new Log (f, this->clock, verbosity);
        }

    this->log->Write (INFO, "DelftOnline version %d.%d starting", VERSION_MAJOR, VERSION_MINOR);

    // Find an open a TCP port for clients to connect to

    if ((this->sock = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP)) == INVALID_SOCKET)
        throw Error (true, "Server", "Cannot create socket client connections: (%d) %s", errno, strerror (errno));

    this->addr.sin_family = AF_INET;
    this->addr.sin_addr.s_addr = INADDR_ANY;

    uint16_t port;
    uint16_t first = (firstPort == 0) ? TCPPORT_FIRST : firstPort;
    uint16_t last  = (lastPort  == 0) ? TCPPORT_LAST  : lastPort;

    if (firstPort > 0 && lastPort == 0) {
        port = firstPort;
        this->addr.sin_port = htons (port);
        if (bind (this->sock, (struct sockaddr *) &this->addr, sizeof (struct sockaddr)) != 0)
            throw Error (true, "Server", "Cannot bind to explicitly requested port %d: (%d) %s", port, errno, strerror (errno));
        }

    else {
        for (port = first ; port <= last ; port++) {
            this->addr.sin_port = htons (port);
            if (bind (this->sock, (struct sockaddr *) &this->addr, sizeof (struct sockaddr)) == 0)
                break;
            }
        }

    if (port > last)
        throw Error (true, "Server", "Cannot bind socket for clients to any port in range [%d,%d]", first, last);

    if (listen (this->sock, TCP_BACKLOG) != 0)
        throw Error (true, "Server", "Cannot listen to TCP/IP socket: (%d) %s", errno, strerror (errno));

    // Generate URL handle

    this->handle = GenerateURL (port);
    if (this->handle == NULL)
        throw Error (true, "Server", "Cannot generate URL (hostname determination failure)");

    this->log->Write (INFO, "Handle is \"%s\"", this->handle);

    // Initialize threading

    this->numThreads = 0;

    for (int i = 0 ; i < MaxThreads ; i++) {
        this->thr[i] = new Thread ();
        this->thr[i]->id     = THID_FREE;
        this->thr[i]->name   = NULL;
        this->thr[i]->curDir = NULL;
        }

    this->thr[0]->id     = 0;
    this->thr[0]->name   = "<main>";
    this->thr[0]->curDir = this->rootDir;

    // Create a thread-specific key to store thread ID in

    if (pthread_key_create (&this->thread, NULL) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot create thread-specific key: %s", strerror (errno));
    if (pthread_setspecific (this->thread, (void *) 0) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot set thread-specific key for main thread: %s", strerror (errno));

    // Setup a mutex to serialize server operations and a condition variable to start/stop

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot create server mutex: %s", strerror (errno));
    if (pthread_cond_init (&this->pause, NULL) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot create condition variable for start/stop: %s", strerror (errno));

    // Create a thread to service incoming client connections

    if (pthread_create (&this->clientGreeter, NULL, &clientGreeterInitiator, (void *) this) != 0)
        throw Error (true, "Server", "Pthreads error: Cannot create client greeter thread, errno=%d", errno);

    }


Server::Server (
    const char *    serverURL
    ) {

    throw Error (true, "Server", "ToDo !! Implement slave server support");
    }


//-------------------------------------------------------------------------------
//  Destructor


Server::~Server (
    void
    ) {

    if (this->barrier != NULL) delete this->barrier;
    if (this->handle != NULL) delete [] this->handle;
    if (this->description != NULL) free (this->description);

    for (int i = 0 ; i < MaxThreads ; i++)
        delete this->thr[i];

    delete this->log;
    delete this->clock;
    }


//-------------------------------------------------------------------------------


static void *
clientGreeterInitiator (
    void * argument
    ) {

    ((Server *) argument)->ClientGreeter ();
    return NULL;
    }


void
Server::ClientGreeter (
    void
    ) {

    while (true) {
	fd_set read;    FD_ZERO (&read);
	fd_set write;   FD_ZERO (&write);
	fd_set except;  FD_ZERO (&except);
	FD_SET (this->sock, &read);

	if (select (this->sock + 1, &read, &write, &except, NULL) != 1)
            throw Error (true, "Server::clientGreeterThread", "Select on TCP connection fails: %s", strerror (errno));

	int clientsock;
	struct sockaddr clientaddr;
	socklen_t addrlen = sizeof clientaddr;

	if ((clientsock = accept (this->sock, &clientaddr, &addrlen)) == -1)
            throw Error (true, "Server::clientGreeterThread", "Accept on TCP connection fails: %s", strerror (errno));

        LOCK
        new ClientConnection (this->clientID++, this, clientsock, &clientaddr);
        UNLOCK
	}
    }


//-------------------------------------------------------------------------------


char *
Server::Handle (
    void
    ) {

    return this->handle;
    }


void
Server::SetDescription (
    const char *  description
    ) {

    LOCK
    this->description = strdup (description);
    UNLOCK
    }


//-------------------------------------------------------------------------------
//  Thread functions


void
Server::RegisterThread (
    int             numThreads,
    const char *    name
    ) {

    LOCK

    if (numThreads < 1)
        throw Error (true, "RegisterThread", "NumThreads is not positive (%d)", numThreads);
    if (name == NULL || strcmp (name, "") == 0)
        throw Error (true, "RegisterThread", "Thread name is empty");

    static int seqn = 1;        // thread sequence number
    int id = seqn++;

    if (id >= MaxThreads)
        throw Error (true, "RegisterThread", "Too many threads (limit is %d)", MaxThreads);

    if (pthread_setspecific (this->thread, (void *) id) != 0)
        throw Error (true, "RegisterThread", "Pthreads error: Cannot set thread-specific key, errno=%d", errno);

    if (id > 1 && numThreads != this->numThreads)
        throw Error (true, "RegisterThread", "Inconsistent number of threads specified (%d != %d)", numThreads, this->numThreads);

    this->numThreads      = numThreads;
    this->thr[id]->name   = name;
    this->thr[id]->curDir = this->rootDir;

    this->log->Write (INFO, "Registering thread %s (%d of %d)", name, id, numThreads);
    this->log->RegisterThread (name);
    UNLOCK
    }


int
Server::GetThreadID (
    void
    ) {

    // The following print statement is added for debug purpose and will be removed again as soon as possible
    // printf("GetThreadID called\n");
    void * spec = pthread_getspecific (this->thread);
    if (spec < 0)
        throw Error (true, "GetThreadID", "Invalid thread-specific key");

    return (long) spec & 0xFFFFFFFF;
    }


void
Server::UnregisterThread (
    void
    ) {

    LOCK

    int id = this->GetThreadID ();

    this->log->Write (INFO, "Unregistering thread %s", this->thr[id]->name);
    this->log->UnregisterThread ();

    this->thr[id]->id     = THID_FREE;
    this->thr[id]->name   = NULL;
    this->thr[id]->curDir = NULL;

    UNLOCK
    }


//-------------------------------------------------------------------------------


bool
Server::waitOnStart ( void ) {
    return (! this->status.running);
}

//-------------------------------------------------------------------------------
//  Milestone function


void
Server::PassMilestone (
    Milestone milestone
    ) {

    Milestone nextStop;

    // Phase one:
    //      Wait for all server threads to pass this milestone

    LOCK

    if (! this->ready) {
        this->barrier = new Barrier (this->numThreads);
        this->ready = true;
        }

    static Milestone m;
    int threadID = this->GetThreadID ();
    if (threadID == 1)
        m = milestone;

    UNLOCK
    this->barrier->Pass ();

    // Phase two:
    //      Increment milestone
    //      Wake up waiting clients if the milestone is far enough along for them

    LOCK

    if (m != milestone)
        throw Error (true, "PassMilestone", "Threads do not agree on milestone value");

    if (threadID == 1) {
        this->status.milestone = milestone;

        Server::ClientConnection * client;
        nextStop = this->breakpoints->Min ((void **) &client);
        if (nextStop >= 0 && nextStop < milestone)
            throw Error (true, "PassMilestone", "Internal Error: nextStop < milestone");

        if (nextStop == milestone) {
            this->breakpoints->Delete (nextStop);
            this->status.running = false;
            this->log->Write (TRACE, "Waking up client %d at milestone %d", client->clientID, milestone);
            if (pthread_mutex_unlock (&client->sync) != 0)
                throw new Exception (true, "Pthreads error: Cannot unlock Server::ClientConnection step/stop mutex: %s", strerror (errno));
            }
        }

    UNLOCK
    this->barrier->Pass ();
    LOCK

    // Phase three:
    //      Transition to the stopped state if we're at a milestone breakpoint

    if (! this->status.running) {
        if (threadID == 1)
            this->log->Write (INFO, "Pausing at milestone %d", milestone);

        if (pthread_cond_wait (&this->pause, &this->mutex) != 0)
            throw Error (true, "PassMilestone", "Pthreads error: pthread_cond_wait fails in PassMilestone %s", strerror (errno));

        if (threadID == 1)
            this->log->Write (INFO, "Resuming at milestone %d", milestone);
        }

    UNLOCK
    }


//-------------------------------------------------------------------------------
//  Control routines


bool
Server::Start (
    void
    ) {

    if (! this->status.allowControl) return false;
    LOCK

    if (this->status.running) {
        UNLOCK
        return false;
        }

    this->status.running = true;
    if (pthread_cond_broadcast (&this->pause) != 0)
        throw new Exception (true, "Pthreads error: pthread_cond_broadcast fails in Server::Start: %s", strerror (errno));

    UNLOCK
    return true;
    }


bool
Server::Step (
    int steps,
    ClientConnection * client
    ) {

    if (! this->status.allowControl) return false;
    LOCK

    if (this->status.running) {
        UNLOCK
        return false;
        }

    this->status.running = true;
    if (pthread_cond_broadcast (&this->pause) != 0)
        throw new Exception (true, "Pthreads error: pthread_cond_broadcast fails in Server::Start: %s", strerror (errno));

    this->StopAt (this->status.milestone + steps, client);
    return true;
    }


bool
Server::Stop (
    ClientConnection * client
    ) {

    if (! this->status.allowControl) return false;
    LOCK

    if (! this->status.running) {
        UNLOCK
        return false;
        }

    this->StopAt (this->status.milestone + 1, client);

    return true;
    }


void
Server::StopAt (
    Milestone milestone,
    ClientConnection * client
    ) {

    this->breakpoints->Add (milestone, client);
    UNLOCK

    client->log->Write (TRACE, "Waiting for milestone %d", milestone);

    if (pthread_mutex_lock (&client->sync) != 0)
        throw new Exception (true, "Pthreads error: Cannot lock Server::ClientConnection step/stop mutex: %s", strerror (errno));

    client->log->Write (TRACE, "Resuming at milestone %d", milestone);
    }


bool
Server::Terminate (
    int clientID
    ) {

    if (! this->status.allowControl) return false;

    this->log->Write (ERROR, "Termination ordered by DOL client %d", clientID);
    return true;
    }


//-------------------------------------------------------------------------------


Exception *
Server::Error (
    bool    fatal,
    const char *  function,
    const char *  reason,
    ...
    ) {

    UNLOCK

    va_list arguments;
    va_start (arguments, reason);

    const int size = Exception::MaxErrorMesgLen;
    char mesg [size + 1];

    if (vsnprintf (mesg, size, reason, arguments) < 0)
        return new Exception (true, "An error occurred, the the reason string cannot be formatted!");

    va_end (arguments);
    return new Exception (fatal, "[%s] %s", function, mesg);
    }


//-------------------------------------------------------------------------------
//  Utilities


static char *
GenerateURL (
    int port
    ) {

    // Create the DOL handle URL

    char hostname [1000];
    if (gethostname (hostname, sizeof hostname) != 0)
        return NULL;

    char palette [] = "ABCDEFGHIJKLMNOPQRSTUVWZYXabcdefghijklmnopqrstuvwzyx0123456789";
    char key [URLkeyLength+1];

    srandom (getpid ());
    for (int i = 0 ; i < URLkeyLength ; i++)
        key[i] = palette[random () % (sizeof palette-1)];

    key[URLkeyLength] = '\0';

    char * url = new char [20 + strlen (hostname) + URLkeyLength];
    sprintf (url, "%s%s:%d/%s", URLprefix, hostname, port, key);
    return url;
    }

}
