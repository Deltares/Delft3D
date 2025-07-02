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
// $Id$
// $HeadURL$
//------------------------------------------------------------------------------
//  DelftStream
//  Stream Class Implementation - TCP/IP and MPI
//
//  The file "stream.cpp" is the only souce file for the Delft-Stream library.
//  It support both TCP/IP and MPI mode, and contains both C and C++ MPI bindings.
//  The latter are MPI-2, and will prevail over the C bindings once MPI-2
//  becomes more widely implemented.
//
//  Irv.Elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  02 dec 08
//
//------------------------------------------------------------------------------

#include "stream.h"

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#if defined (WIN32)
//#include <Winsock2.h>
#   define close _close
#endif

#if defined (ALTIX)
// Intel C compiler (icc/icpc) on Itanium doesn't like GNU __extension__ functions
#undef htons
#undef ntohs
#undef htonl
#undef ntohl
#define htons(x) __bswap_constant_16(x)
#define ntohs(x) __bswap_constant_16(x)
#define htonl(x) __bswap_constant_32(x)
#define ntohl(x) __bswap_constant_32(x)
#endif


//------------------------------------------------------------------------------
//  Static class members and Constants


bool            Stream::initialized = false;
pthread_mutex_t Stream::mutex;


enum {
    MAXSTRING   = 1000,     // max string length in bytes
    BACKLOG     = 10,       // max queue of pending connections for listen
    MAXTRIES    = 20,       // number of connect attempts
    TRYSLEEP    = 250,      // milliseconds to sleep between tries

    // starting and ending TCP port numbers for availability search
    FIRST_PORT = 17000,
    LAST_PORT  = ((1 << (sizeof (IPport) * 8)) - 1),

    CONNECT_TAG = 17400000  // magic number for MPI mode
    };




// Provide missing entities for Windows
//
#if defined (WIN32)

typedef int socklen_t;   // From WINSOCK.H which defines the accept() prototype

void usleep( long time ) {
    Sleep( time/1000.0 );    // sleep() under Windows uses milliseconds - don't ask why
}
#endif

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

    Stream::initialize ();

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset (&this->local,  0, sizeof this->local);
    memset (&this->remote, 0, sizeof this->remote);

    this->connected = false;
    this->streamtype = streamtype;

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            construct_MPI (this);
            break;
#endif
        case Stream::TCPIP:
            construct_TCPIP (this);
            break;

        default:
            error ("Unknown stream type in initial constructor");
            break;
        }
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

    Stream::initialize();

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset (&this->local,  0, sizeof this->local);
    memset (&this->remote, 0, sizeof this->remote);

    this->streamtype = streamtype;
    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            connect_MPI (this, handle);
            break;
#endif
        case Stream::TCPIP:
            connect_TCPIP (this, handle);
            break;

        default:
            error ("Unknown stream type in secondary constructor");
            break;
        }
    }


Stream::~Stream (
    void
    ) {

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            if (local.handle  != NULL) delete local.handle;
            if (remote.handle != NULL) delete remote.handle;
            break;
#endif
        case Stream::TCPIP:
            close (this->local.sock);
            close (this->remote.sock);

            if (local.handle  != NULL) delete local.handle;
            if (remote.handle != NULL) delete remote.handle;
            break;

        default:
            error ("Unknown stream type in destructor");
            break;
        }
    }


//------------------------------------------------------------------------------
//  Constructors - Private TCP/IP Functions


void
Stream::construct_TCPIP (
    Stream * stream
    ) {

    if ((stream->local.sock = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
        error ("Cannot create local socket for unpaired stream");

    stream->local.addr.sin_family = AF_INET;
    stream->local.addr.sin_addr.s_addr = INADDR_ANY;

    // Find an available port

    IPport port;
    for (port = FIRST_PORT ; port < LAST_PORT ; port++) {
        stream->local.addr.sin_port = htons (port);
        if (bind (stream->local.sock, (struct sockaddr *) &stream->local.addr, sizeof (struct sockaddr)) ==  0) {
            break;
            }
        }

    if (port >= LAST_PORT)
        error ("Cannot bind stream socket to any port in range [%d,%d]\n", FIRST_PORT, LAST_PORT - 1);

    char buffer [MAXSTRING];
    sprintf (buffer, "%s:%d", hostname (), port);
    stream->local.handle = new char [strlen (buffer) + 1];
    strcpy (stream->local.handle, buffer);

    if (stream->tracefunction != NULL)
        trace ("Created handle %s", stream->local.handle);
    }


void
Stream::connect_TCPIP (
    Stream * stream,
    const char *  handle
    ) {

    if (handle == NULL || handle[0] == '\0')
        error ("Null or empty handle (hostname:port) string in Stream constructor");

    // Parse the hostname:port string

    char hostname [MAXSTRING];
    char *hp = hostname;
    int port;

    while (*handle != '\0' && *handle != ':')
        *hp++ = *handle++;

    *hp = '\0';
    port = atoi (handle+1);

    if ((stream->remote.sock = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
        error ("Cannot create remote socket for paired stream");

    stream->remote.addr.sin_family = AF_INET;
    stream->remote.addr.sin_addr.s_addr = inet_addr (lookup_host (hostname));
    stream->remote.addr.sin_port = htons (port);

    char buffer [MAXSTRING];
    sprintf (buffer, "%s:%d", hostname, port);
    stream->remote.handle = new char [strlen (buffer) + 1];
    strcpy (stream->remote.handle, buffer);

    // Connect to remote address.  Try stream a few times bacause the other side may
    // not have done a receive yet.

    if (stream->tracefunction != NULL)
        trace ("Attempting to connect to %s", stream->remote.handle);

    int attempt;
    for (attempt = 0 ; attempt < MAXTRIES ; attempt++) {
        if (connect (stream->remote.sock, (struct sockaddr *) &stream->remote.addr, sizeof (struct sockaddr)) == 0)
            break;
        usleep (1000 * TRYSLEEP);
        }

    if (attempt == MAXTRIES)
        error ("Cannot connect to remote socket for paired stream (%s): %s",
                            stream->remote.handle,
                            strerror (errno)
                            );

#if defined(WIN32)
    int got = recvfrom (stream->remote.sock, (char *) buffer, MAXSTRING, 0, NULL, 0);
#else
    int got = recvfrom (stream->remote.sock, (void *) buffer, MAXSTRING, 0, NULL, 0);
#endif
    if (got == -1)
        error ("Recvfrom of local side from new peer fails (%s)", strerror (errno));

    stream->local.handle = new char [strlen (buffer) + 1];
    strcpy (stream->local.handle, buffer);

    stream->connected = true;

    if (stream->tracefunction != NULL)
        trace ("Created handle %s attached to %s", stream->local.handle, stream->remote.handle);
    }


//------------------------------------------------------------------------------
//  Constructors - Private MPI Functions

#if defined (WITH_MPI)

void
Stream::construct_MPI (
    Stream * stream
    ) {

    stream->local.seqn = next_seqn ();
#if defined (NO_CPP_MPI)
    MPI_Comm_rank (MPI_COMM_WORLD, &stream->local.rank);
#else
    stream->local.rank = MPI::COMM_WORLD.Get_rank ();
#endif
    stream->local.handle = new char [Stream::MAXHANDLE];
    sprintf (stream->local.handle, "%s:%d:%d", hostname (), stream->local.rank, stream->local.seqn);
    stream->connected = false;

    if (stream->tracefunction != NULL)
        trace ("Created handle %s", stream->local.handle);
    }


void
Stream::connect_MPI (
    Stream * stream,
    const char *  handle
    ) {

    stream->remote.handle = new char [Stream::MAXHANDLE];
    strcpy (stream->remote.handle, handle);
    if (sscanf (handle, "%*[^:]:%d:%d", &stream->remote.rank, &stream->remote.seqn) != 2)
        error ("Invalid MPI stream handle \"%s\"", handle);

    stream->local.seqn = next_seqn ();
#if defined (NO_CPP_MPI)
    MPI_Comm_rank (MPI_COMM_WORLD, &stream->local.rank);
#else
    stream->local.rank = MPI::COMM_WORLD.Get_rank ();
#endif
    stream->local.handle = new char [Stream::MAXHANDLE];
    sprintf (stream->local.handle, "%s:%d:%d", hostname (), stream->local.rank, stream->local.seqn);
    stream->connected = true;

    // Send first message with local handle so peer knows who I am

#if defined (NO_CPP_MPI)
    MPI_Send ((void *) stream->local.handle, Stream::MAXHANDLE, MPI_CHAR, stream->remote.rank, CONNECT_TAG + stream->remote.seqn, MPI_COMM_WORLD);
#else
    MPI::COMM_WORLD.Send ((void *) stream->local.handle, Stream::MAXHANDLE, MPI::CHAR, stream->remote.rank, CONNECT_TAG + stream->remote.seqn);
#endif

    if (stream->tracefunction != NULL)
        trace ("Created handle %s attached to %s", stream->local.handle, stream->remote.handle);
    }

#endif


//------------------------------------------------------------------------------
//  Send and Receive - Public Functions


void
Stream::Connect (
    void
    ) {

    // This function is called by the first end-point of a stream to
    // complete the connection. This is a blocking call.

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            first_receive_MPI (this);
            break;
#endif
        case Stream::TCPIP:
            first_receive_TCPIP (this);
            break;

        default:
            error ("Unknown stream type in Connect");
            break;
        }
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

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
#if defined (NO_CPP_MPI)
            MPI_Send ((void *) buffer, length, MPI_CHAR, this->remote.rank, this->remote.seqn, MPI_COMM_WORLD);
#else
            MPI::COMM_WORLD.Send ((void *) buffer, length, MPI::CHAR, this->remote.rank, this->remote.seqn);
#endif
            break;
#endif
        case Stream::TCPIP:
#if defined(WIN32)
            if (sendto (this->remote.sock, (char *) buffer, length, 0, (struct sockaddr *) &this->local.addr, sizeof (struct sockaddr)) != length)
                error ("Sendto %s fails (%s)", this->remote.handle, strerror (errno));
#else
            if (sendto (this->remote.sock, (void *) buffer, length, 0, (struct sockaddr *) &this->local.addr, sizeof (struct sockaddr)) != length)
                error ("Sendto %s fails (%s)", this->remote.handle, strerror (errno));
#endif
            break;

        default:
            error ("Unknown stream type in Send");
            break;
        }

    if (this->tracefunction != NULL)
        trace ("Sent message from %s to %s (%d bytes)", this->local.handle, this->remote.handle, length);
    }


void
Stream::Receive (
    char *  buffer,
    int     length
    ) {

    switch (streamtype) {
#if defined (WITH_MPI)
        case Stream::MPI:
            receive_MPI (this, buffer, length);
            break;
#endif

        case Stream::TCPIP:
            receive_TCPIP (this, buffer, length);
            break;

        default:
            error ("Unknown stream type in Receive");
            break;
        }
    }


//------------------------------------------------------------------------------
//  Receive - Private TCP/IP Functions


void
Stream::first_receive_TCPIP (
    Stream * stream
    ) {

    if (listen (stream->local.sock, BACKLOG) != 0)
        error ("Cannot listen to local socket");

    if (stream->tracefunction != NULL)
        trace ("Waiting for connection on %s", stream->local.handle);

    socklen_t addrlen = sizeof (struct sockaddr);
    if ((stream->remote.sock = accept (stream->local.sock, (struct sockaddr *) &stream->remote.addr, &addrlen)) == -1)
        error ("Cannot accept connection on stream");

    IPaddr addr = stream->remote.addr.sin_addr.s_addr;
    IPport port = stream->remote.addr.sin_port;
    stream->remote.handle = new char [Stream::MAXHANDLE];
    sprintf (stream->remote.handle, "%s:%d", lookup_dotaddr (dotipaddr (addr)), ntohs (port));

#if defined(WIN32)
    if (sendto (stream->remote.sock, (char *) stream->remote.handle, Stream::MAXHANDLE, 0, (struct sockaddr *) &stream->remote.addr, sizeof (struct sockaddr)) != Stream::MAXHANDLE)
        error ("Sendto of local side to %s fails (%s)", stream->remote.handle, strerror (errno));
#else
    if (sendto (stream->remote.sock, (void *) stream->remote.handle, Stream::MAXHANDLE, 0, (struct sockaddr *) &stream->remote.addr, sizeof (struct sockaddr)) != Stream::MAXHANDLE)
        error ("Sendto of local side to %s fails (%s)", stream->remote.handle, strerror (errno));
#endif

    stream->connected = true;

    if (stream->tracefunction != NULL)
        trace ("Created handle %s attached to %s", stream->local.handle, stream->remote.handle);
    }


void
Stream::receive_TCPIP (
    Stream * stream,
    char *  buffer,
    int     length
    ) {

    if (stream->tracefunction != NULL)
        trace ("Waiting for message from %s to %s (%d bytes)", stream->remote.handle, stream->local.handle, length);

    int need = length;
    while (need > 0) {
        int got;
#if defined(WIN32)
        if ((got = recvfrom (stream->remote.sock, (char *) buffer, need, 0, NULL, 0)) < 0) {
            error ("Recvfrom %s fails (%s; %d)", stream->remote.handle, strerror (errno), WSAGetLastError());
                        return;} // Endless loop otherwise
#else
        if ((got = recvfrom (stream->remote.sock, (void *) buffer, need, 0, NULL, 0)) < 0)
            error ("Recvfrom %s fails (%s)", stream->remote.handle, strerror (errno));
#endif
        if (got == 0)
            error ("Recvfrom %s returns 0 bytes. Is the peer process dead?", stream->remote.handle);

        need -= got;
        buffer += got;
        }

    if (stream->tracefunction != NULL)
        trace ("Got message from %s on %s (%d bytes)", stream->remote.handle, stream->local.handle, length);
    }


//------------------------------------------------------------------------------
//  Receive - Private MPI Functions


#if defined (WITH_MPI)

void
Stream::first_receive_MPI (
    Stream * stream
    ) {

    stream->remote.handle = new char [Stream::MAXHANDLE];

#if defined (NO_CPP_MPI)
    MPI_Status status;
    MPI_Recv ((void *) stream->remote.handle, Stream::MAXHANDLE, MPI_CHAR, MPI_ANY_SOURCE, CONNECT_TAG + this->local.seqn, MPI_COMM_WORLD, &status);
#else
    MPI::Status status;
    MPI::COMM_WORLD.Recv ((void *) stream->remote.handle, Stream::MAXHANDLE, MPI::CHAR, MPI::ANY_SOURCE, CONNECT_TAG + this->local.seqn, status);
#endif

    if (sscanf (stream->remote.handle, "%*[^:]:%d:%d", &stream->remote.rank, &stream->remote.seqn) != 2)
        error ("Invalid MPI stream handle \"%s\"", stream->remote.handle);

    stream->connected = true;

    if (stream->tracefunction != NULL)
        trace ("Created handle %s attached to %s", stream->local.handle, stream->remote.handle);
    }


void
Stream::receive_MPI (
    Stream * stream,
    char *  buffer,
    int     length
    ) {

    if (stream->tracefunction != NULL)
        trace ("Waiting for message from %s to %s (%d bytes)", stream->remote.handle, stream->local.handle, length);

#if defined (NO_CPP_MPI)
    MPI_Status status;
    MPI_Recv ((void *) buffer, length, MPI_CHAR, stream->remote.rank, stream->local.seqn, MPI_COMM_WORLD, &status);

    int count;
    MPI_Get_count (&status, MPI_CHAR, &count);
    if (length != count)
        error ("Receive gets message from %s to %s of different length (%d) than requested (%d)", stream->remote.handle, stream->local.handle, count, length);
#else
    MPI::Status status;
    MPI::COMM_WORLD.Recv ((void *) buffer, length, MPI::CHAR, stream->remote.rank, stream->local.seqn, status);

    if (length != status.Get_count (MPI::CHAR))
        error ("Receive gets message from %s to %s of different length (%d) than requested (%d)", stream->remote.handle, stream->local.handle, status.Get_count (MPI::CHAR), length);
#endif

    if (stream->tracefunction != NULL)
        trace ("Got message from %s on %s (%d bytes)", stream->remote.handle, stream->local.handle, length);
    }

#endif


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
#if defined (WITH_MPI)
            case Stream::MPI:
                return "[mpi]:*:*";
#endif
            case Stream::TCPIP:
                return "[unknown]:0";
            default:
                return NULL;
            }
        }
    }


//------------------------------------------------------------------------------
//  Private functions


void
Stream::initialize (
    void
    ) {

    if (! Stream::initialized) {
        Stream::initialized = true;
#if defined(WIN32)
        // See: http://www.exegesis.uklinux.net/gandalf/winsock/winsock1.htm
        WORD wVersionRequested = MAKEWORD(1, 1);
                WSADATA wsaData;
        if ( WSAStartup( wVersionRequested, &wsaData ) != 0 )
            error("Initialising sockets on Windows failed");
#else
        if (pthread_mutex_init (&Stream::mutex, NULL) != 0)
            error ("Pthreads error: Cannot create stream class mutex, errno=%d", errno);
#endif

        }
    }


int
Stream::next_seqn (
    void
    ) {

    static int seqn = 101;

#if defined(WIN32)
    // nothing
#else
    if (pthread_mutex_lock (&Stream::mutex) != 0)
        error ("Pthreads error: Cannot lock stream class mutex, errno=%d", errno);
#endif

    int nextseqn = seqn++;

#if defined(WIN32)
    // nothing
#else
    if (pthread_mutex_unlock (&Stream::mutex) != 0)
        error ("Pthreads error: Cannot unlock stream class mutex, errno=%d", errno);
#endif

    return nextseqn;
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


char *
Stream::hostname (
    void
    ) {

    static char buffer [MAXSTRING+1];       // not thread safe, but OK
    static bool initialized = false;

    if (! initialized) {
        initialized = true;

        buffer[MAXSTRING] = '\0';
        if (gethostname (buffer, sizeof buffer) != 0)
            error ("Cannot get hostname");
        if (strlen (buffer) == MAXSTRING)
            error ("hostname too long");

        for (char *bp = buffer ; *bp != '\0' ; *bp++) {     // truncate domain
            if (*bp == '.') {
                *bp = '\0';
                break;
                }
            }
        }

    return buffer;
    }


void
Stream::parse_name (
    char *  string,     // in
    char *  hostname,   // out, MAXSTRING chars available
    int *   port        // out
    ) {

    if (string == NULL)
        error ("Null string in parse_name");

    while (*string != '\0' && *string != ':')
        *hostname++ = *string++;

    *hostname = '\0';
    *port = atoi (string+1);
    }


char *
Stream::lookup_host (
    char *  hostname
    ) {

    // Map host name string to dotted ip address string

    static char ipaddr [MAXSTRING];     // not thread safe, but OK
    struct hostent * hostinfo;

    if ((hostinfo = gethostbyname (hostname)) == NULL)
        { printf ("Cannot get address of host \"%s\"\n", hostname); exit (1); }

    sprintf (ipaddr, "%d.%d.%d.%d",
                            (unsigned char) hostinfo->h_addr_list[0][0],
                            (unsigned char) hostinfo->h_addr_list[0][1],
                            (unsigned char) hostinfo->h_addr_list[0][2],
                            (unsigned char) hostinfo->h_addr_list[0][3]
                            );
    return ipaddr;
    }


char *
Stream::lookup_dotaddr (
    char *  ipdotaddr
    ) {

    // Map dotted ip address to an unqualified host name

    static char hostname [MAXSTRING];   // not thread safe, but OK
    struct hostent *hostinfo;

    unsigned int a, b, c, d;
    if (sscanf (ipdotaddr, "%d.%d.%d.%d", &a, &b, &c, &d) != 4)
        error ("Cannot parse dotted IP address \"%s\"\n", ipdotaddr);

#if defined(linux) || defined(IRIX)
    IPaddr addr = a | b << 8 | c << 16 | d << 24;
#elif defined(WIN32)
    struct in_addr addr;
    addr.s_addr = inet_addr(ipdotaddr);
#elif defined(reversebyteorder)
    IPaddr addr = d | c << 8 | b << 16 | a << 24;
#else
    undefined system type in lookup_dotaddr
#endif

#if defined(WIN32)
    if ((hostinfo = gethostbyaddr ((char *)&addr, 4, AF_INET)) == NULL)
        error ("Cannot get hostname of \"%s\" (0x%x) -- error code: %d\n", ipdotaddr, addr, WSAGetLastError());
#else
    if ((hostinfo = gethostbyaddr ((char *)&addr, sizeof addr, AF_INET)) == NULL)
        error ("Cannot get hostname of \"%s\" (0x%x)\n", ipdotaddr, addr);
#endif

    const char * cp = hostinfo->h_name;
    char * dp = hostname;
    while (cp != NULL && *cp != '\0' && *cp != '.')
        *dp++ = *cp++;

    *dp = '\0';
    return hostname;
    }


char *
Stream::dotipaddr (
    IPaddr   addr
    ) {

    // Convert 32-bit packed ip address to dotted string

    static char dotaddr [16];   // not thread safe, but OK
    sprintf (dotaddr, "%d.%d.%d.%d",
                        (addr >>  0) & 0xFF,
                        (addr >>  8) & 0xFF,
                        (addr >> 16) & 0xFF,
                        (addr >> 24) & 0xFF
                        );
    return dotaddr;
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

