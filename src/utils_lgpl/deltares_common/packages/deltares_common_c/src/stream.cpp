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

// Network resolution includes
#if defined(WIN32)
#include <winsock2.h> // Must be first; includes ws2def.h
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#define close closesocket // Use closesocket for sockets on Windows
#else
#include <sys/types.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#endif

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#if defined(ALTIX)
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

bool Stream::initialized = false;
pthread_mutex_t Stream::mutex;

enum
{
    MAXSTRING = 1000, // max string length in bytes
    BACKLOG = 10,     // max queue of pending connections for listen
    MAXTRIES = 20,    // number of connect attempts
    TRYSLEEP = 250,   // milliseconds to sleep between tries

    // starting and ending TCP port numbers for availability search
    FIRST_PORT = 17000,
    LAST_PORT = ((1 << (sizeof(IPport) * 8)) - 1),

    CONNECT_TAG = 17400000 // magic number for MPI mode
};

// Provide missing entities for Windows
//
#if defined(WIN32)

typedef int socklen_t; // From WINSOCK.H which defines the accept() prototype

void usleep(long time)
{
    Sleep(time / 1000.0); // sleep() under Windows uses milliseconds - don't ask why
}
#endif

//------------------------------------------------------------------------------
//  Constructors and Destructor - Public Functions

Stream::Stream(
    StreamType streamtype,
    void (*errorfunction)(char *),
    void (*tracefunction)(char *))
{

    // Create an unconnected (local) stream end-point.
    // The caller has to pass the resulting handle to the remote peer
    // by other means, and then invoke the Connect method to complete
    // the stream connection.  Once the remote peer instantiates a
    // Stream using the handle sends and receives can be done.
    // This call does not block; Connect does.

    Stream::initialize();

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset(&this->local, 0, sizeof this->local);
    memset(&this->remote, 0, sizeof this->remote);

    this->connected = false;
    this->streamtype = streamtype;

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
        construct_MPI();
        break;
#endif
    case Stream::TCPIP:
        construct_TCPIP();
        break;

    default:
        error("Unknown stream type in initial constructor");
        break;
    }
}

Stream::Stream(
    StreamType streamtype,
    const char *handle,
    void (*errorfunction)(char *),
    void (*tracefunction)(char *))
{

    // Create the other half of a two-sided stream connection.
    // This is a blocking call that waits for a Connect from the peer that
    // initially created the stream.

    Stream::initialize();

    this->errorfunction = errorfunction;
    this->tracefunction = tracefunction;

    memset(&this->local, 0, sizeof this->local);
    memset(&this->remote, 0, sizeof this->remote);

    this->streamtype = streamtype;

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
        connect_MPI(handle);
        break;
#endif
    case Stream::TCPIP:
        connect_TCPIP(handle);
        break;

    default:
        error("Unknown stream type in secondary constructor");
        break;
    }
}

Stream::~Stream(
    void)
{

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
        if (local.handle != NULL)
            delete local.handle;
        if (remote.handle != NULL)
            delete remote.handle;
        break;
#endif
    case Stream::TCPIP:
        close(this->local.sock);
        close(this->remote.sock);

        if (local.handle != NULL)
            delete local.handle;
        if (remote.handle != NULL)
            delete remote.handle;
        break;

    default:
        error("Unknown stream type in destructor");
        break;
    }
}

//------------------------------------------------------------------------------
//  Constructors - Private TCP/IP Functions

void Stream::construct_TCPIP(void)
{

    IPport port; // Declare outside branches

    // Try IPv6 dual-stack first
    this->local.sock = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
    this->is_ipv6 = (this->local.sock != -1);
    if (this->is_ipv6)
    {
        int opt = 0;
        setsockopt(this->local.sock, IPPROTO_IPV6, IPV6_V6ONLY, (char *)&opt, sizeof(opt));

        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&this->local.addr;
        sin6->sin6_family = AF_INET6;
        sin6->sin6_addr = IN6ADDR_ANY_INIT;

        // Find an available port (IPv6)
        bool port_found = false;
        for (port = FIRST_PORT; port < LAST_PORT; port++)
        {
            sin6->sin6_port = htons(port);
            if (bind(this->local.sock, (struct sockaddr *)sin6, sizeof(struct sockaddr_in6)) == 0)
            {
                port_found = true;
                break;
            }
        }

        if (!port_found)
        {
            error("Cannot bind IPv6 stream socket to any port in range [%d,%d]\n", FIRST_PORT, LAST_PORT - 1);
        }
    }
    else
    {
        // Fallback to IPv4
        this->local.sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (this->local.sock == -1)
            error("Cannot create local socket for unpaired stream");

        struct sockaddr_in *sin = (struct sockaddr_in *)&this->local.addr;
        sin->sin_family = AF_INET;
        sin->sin_addr.s_addr = INADDR_ANY;

        // Find an available port (IPv4)
        bool port_found = false;
        for (port = FIRST_PORT; port < LAST_PORT; port++)
        {
            sin->sin_port = htons(port);
            if (bind(this->local.sock, (struct sockaddr *)sin, sizeof(struct sockaddr_in)) == 0)
            {
                port_found = true;
                break;
            }
        }

        if (!port_found)
        {
            error("Cannot bind IPv4 stream socket to any port in range [%d,%d]\n", FIRST_PORT, LAST_PORT - 1);
        }
    }

    char buffer[MAXSTRING];
    sprintf(buffer, "%s:%d", hostname(), port);
    this->local.handle = new char[strlen(buffer) + 1];
    strcpy(this->local.handle, buffer);

    trace("Created handle %s", this->local.handle);
}

void Stream::connect_TCPIP(
    const char *handle)
{

    if (handle == NULL || handle[0] == '\0')
        error("Null or empty handle (hostname:port) string in Stream constructor");

    // Parse the hostname:port string

    char hostname_str[MAXSTRING];
    char *hp = hostname_str;
    int port;

    while (*handle != '\0' && *handle != ':')
        *hp++ = *handle++;

    *hp = '\0';
    port = atoi(handle + 1);

    char *ipstr = this->lookup_host(hostname_str);

    // Try IPv6 dual-stack first
    this->remote.sock = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
    this->is_ipv6 = (this->remote.sock != -1);
    if (this->is_ipv6)
    {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&this->remote.addr;
        sin6->sin6_family = AF_INET6;
        sin6->sin6_port = htons(port);

        struct in6_addr in6;
        if (inet_pton(AF_INET6, ipstr, &in6) != 1)
        {
            // Assume IPv4 and map to IPv4-mapped IPv6
            struct in_addr in4;
            if (inet_pton(AF_INET, ipstr, &in4) != 1)
            {
                error("Invalid IP address format for host '%s': %s", hostname_str, ipstr);
            }
            memset(&in6, 0, sizeof(in6));
            in6.s6_addr[10] = 0xFF;
            in6.s6_addr[11] = 0xFF;
            memcpy(&in6.s6_addr[12], &in4, sizeof(in4));
        }
        sin6->sin6_addr = in6;
    }
    else
    {
        // Fallback to IPv4
        this->remote.sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (this->remote.sock == -1)
        {
            error("Cannot create remote socket for paired stream");
        }

        struct sockaddr_in *sin = (struct sockaddr_in *)&this->remote.addr;
        sin->sin_family = AF_INET;
        sin->sin_port = htons(port);
        if (inet_pton(AF_INET, ipstr, &sin->sin_addr) != 1)
        {
            error("Invalid IPv4 address format for host '%s': %s", hostname_str, ipstr);
        }
    }

    char buffer[MAXSTRING];
    sprintf(buffer, "%s:%d", hostname_str, port);
    this->remote.handle = new char[strlen(buffer) + 1];
    strcpy(this->remote.handle, buffer);

    // Connect to remote address.  Try a few times because the other side may
    // not have done a listen yet.

    trace("Attempting to connect to %s", this->remote.handle);

    int attempt;
    for (attempt = 0; attempt < MAXTRIES; attempt++)
    {
        socklen_t addrlen = this->is_ipv6 ? sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
        if (connect(this->remote.sock, (struct sockaddr *)&this->remote.addr, addrlen) == 0)
            break;
        usleep(1000 * TRYSLEEP);
    }

    if (attempt == MAXTRIES)
        error("Cannot connect to remote socket for paired stream (%s): %s",
              this->remote.handle,
              strerror(errno));

    int got = recv(this->remote.sock, buffer, MAXSTRING, 0);
    if (got == -1)
        error("Recv of local side from new peer fails (%s)", strerror(errno));

    this->local.handle = new char[strlen(buffer) + 1];
    strcpy(this->local.handle, buffer);

    this->connected = true;

    trace("Created handle %s attached to %s", this->local.handle, this->remote.handle);
}

//------------------------------------------------------------------------------
//  Constructors - Private MPI Functions

#if defined(WITH_MPI)

void Stream::construct_MPI(void)
{

    this->local.seqn = next_seqn();
#if defined(NO_CPP_MPI)
    MPI_Comm_rank(MPI_COMM_WORLD, &this->local.rank);
#else
    this->local.rank = MPI::COMM_WORLD.Get_rank();
#endif
    this->local.handle = new char[Stream::MAXHANDLE];
    sprintf(this->local.handle, "%s:%d:%d", hostname(), this->local.rank, this->local.seqn);
    this->connected = false;

    trace("Created handle %s", this->local.handle);
}

void Stream::connect_MPI(
    const char *handle)
{

    this->remote.handle = new char[Stream::MAXHANDLE];
    strcpy(this->remote.handle, handle);
    if (sscanf(handle, "%*[^:]:%d:%d", &this->remote.rank, &this->remote.seqn) != 2)
        error("Invalid MPI stream handle \"%s\"", handle);

    this->local.seqn = next_seqn();
#if defined(NO_CPP_MPI)
    MPI_Comm_rank(MPI_COMM_WORLD, &this->local.rank);
#else
    this->local.rank = MPI::COMM_WORLD.Get_rank();
#endif
    this->local.handle = new char[Stream::MAXHANDLE];
    sprintf(this->local.handle, "%s:%d:%d", hostname(), this->local.rank, this->local.seqn);
    this->connected = true;

    // Send first message with local handle so peer knows who I am

#if defined(NO_CPP_MPI)
    MPI_Send((void *)this->local.handle, Stream::MAXHANDLE, MPI_CHAR, this->remote.rank, CONNECT_TAG + this->remote.seqn, MPI_COMM_WORLD);
#else
    MPI::COMM_WORLD.Send((void *)this->local.handle, Stream::MAXHANDLE, MPI::CHAR, this->remote.rank, CONNECT_TAG + this->remote.seqn);
#endif

    trace("Created handle %s attached to %s", this->local.handle, this->remote.handle);
}

#endif

//------------------------------------------------------------------------------
//  Send and Receive - Public Functions

void Stream::Connect(void)
{

    // This function is called by the first end-point of a stream to
    // complete the connection. This is a blocking call.

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
        first_receive_MPI();
        break;
#endif
    case Stream::TCPIP:
        first_receive_TCPIP();
        break;

    default:
        error("Unknown stream type in Connect");
        break;
    }
}

void Stream::Send(
    const char *buffer,
    int length)
{

    if (!this->connected)
        error("Attempting to send to an unconnected stream (local side is %s)", this->local.handle);

    trace("Sending message from %s to %s (%d bytes)", this->local.handle, this->remote.handle, length);

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
#if defined(NO_CPP_MPI)
        MPI_Send((void *)buffer, length, MPI_CHAR, this->remote.rank, this->remote.seqn, MPI_COMM_WORLD);
#else
        MPI::COMM_WORLD.Send((void *)buffer, length, MPI::CHAR, this->remote.rank, this->remote.seqn);
#endif
        break;
#endif
    case Stream::TCPIP:
        if (send(this->remote.sock, buffer, length, 0) != length) {
            error("Send to %s fails (%s)", this->remote.handle, strerror(errno));
        }
        break;

    default:
        error("Unknown stream type in Send");
        break;
    }

    trace("Sent message from %s to %s (%d bytes)", this->local.handle, this->remote.handle, length);
}

void Stream::Receive(
    char *buffer,
    int length)
{

    switch (streamtype)
    {
#if defined(WITH_MPI)
    case Stream::MPI:
        receive_MPI(buffer, length);
        break;
#endif

    case Stream::TCPIP:
        receive_TCPIP(buffer, length);
        break;

    default:
        error("Unknown stream type in Receive");
        break;
    }
}

//------------------------------------------------------------------------------
//  Receive - Private TCP/IP Functions

void Stream::first_receive_TCPIP(void)
{

    if (listen(this->local.sock, BACKLOG) != 0)
        error("Cannot listen to local socket");

    trace("Waiting for connection on %s", this->local.handle);

    socklen_t addrlen = this->is_ipv6 ? sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
    if ((this->remote.sock = accept(this->local.sock, (struct sockaddr *)&this->remote.addr, &addrlen)) == -1)
        error("Cannot accept connection on stream");

    char addr_str[INET6_ADDRSTRLEN];
    IPport port;
    if (this->is_ipv6)
    {
        inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&this->remote.addr)->sin6_addr, addr_str, sizeof(addr_str));
        port = ((struct sockaddr_in6 *)&this->remote.addr)->sin6_port;
    }
    else
    {
        inet_ntop(AF_INET, &((struct sockaddr_in *)&this->remote.addr)->sin_addr, addr_str, sizeof(addr_str));
        port = ((struct sockaddr_in *)&this->remote.addr)->sin_port;
    }

    this->remote.handle = new char[Stream::MAXHANDLE];
    sprintf(this->remote.handle, "%s:%d", lookup_dotaddr(addr_str), ntohs(port));

    if (send(this->remote.sock, this->remote.handle, Stream::MAXHANDLE, 0) != Stream::MAXHANDLE)
        error("Send of remote side to %s fails (%s)", this->remote.handle, strerror(errno));

    this->connected = true;

    trace("Created handle %s attached to %s", this->local.handle, this->remote.handle);
}

void Stream::receive_TCPIP(
    char *buffer,
    int length)
{

    trace("Waiting for message from %s to %s (%d bytes)", this->remote.handle, this->local.handle, length);

    int need = length;
    while (need > 0)
    {
        int got;
        if ((got = recv(this->remote.sock, buffer, need, 0)) < 0)
            error("Recv from %s fails (%s)", this->remote.handle, strerror(errno));
        if (got == 0)
            error("Recv from %s returns 0 bytes. Is the peer process dead?", this->remote.handle);

        need -= got;
        buffer += got;
    }

    trace("Got message from %s on %s (%d bytes)", this->remote.handle, this->local.handle, length);
}

//------------------------------------------------------------------------------
//  Receive - Private MPI Functions

#if defined(WITH_MPI)

void Stream::first_receive_MPI(void)
{

    this->remote.handle = new char[Stream::MAXHANDLE];

#if defined(NO_CPP_MPI)
    MPI_Status status;
    MPI_Recv((void *)this->remote.handle, Stream::MAXHANDLE, MPI_CHAR, MPI_ANY_SOURCE, CONNECT_TAG + this->local.seqn, MPI_COMM_WORLD, &status);
#else
    MPI::Status status;
    MPI::COMM_WORLD.Recv((void *)this->remote.handle, Stream::MAXHANDLE, MPI::CHAR, MPI::ANY_SOURCE, CONNECT_TAG + this->local.seqn, status);
#endif

    if (sscanf(this->remote.handle, "%*[^:]:%d:%d", &this->remote.rank, &this->remote.seqn) != 2)
        error("Invalid MPI stream handle \"%s\"", this->remote.handle);

    this->connected = true;

    trace("Created handle %s attached to %s", this->local.handle, this->remote.handle);
}

void Stream::receive_MPI(
    char *buffer,
    int length)
{

    trace("Waiting for message from %s to %s (%d bytes)", this->remote.handle, this->local.handle, length);

#if defined(NO_CPP_MPI)
    MPI_Status status;
    MPI_Recv((void *)buffer, length, MPI_CHAR, this->remote.rank, this->local.seqn, MPI_COMM_WORLD, &status);

    int count;
    MPI_Get_count(&status, MPI_CHAR, &count);
    if (length != count)
        error("Receive gets message from %s to %s of different length (%d) than requested (%d)", this->remote.handle, this->local.handle, count, length);
#else
    MPI::Status status;
    MPI::COMM_WORLD.Recv((void *)buffer, length, MPI::CHAR, this->remote.rank, this->local.seqn, status);

    if (length != status.Get_count(MPI::CHAR))
        error("Receive gets message from %s to %s of different length (%d) than requested (%d)", this->remote.handle, this->local.handle, status.Get_count(MPI::CHAR), length);
#endif

    trace("Got message from %s on %s (%d bytes)", this->remote.handle, this->local.handle, length);
}

#endif

//------------------------------------------------------------------------------
//  Other Public Functions

char *
Stream::LocalHandle(
    void)
{

    return this->local.handle;
}

char *
Stream::RemoteHandle(
    void)
{

    if (this->connected)
        return this->remote.handle;

    else
    {
        switch (streamtype)
        {
#if defined(WITH_MPI)
        case Stream::MPI:
            return (char *)"[mpi]:*:*";
#endif
        case Stream::TCPIP:
            return (char *)"[unknown]:0";
        default:
            return NULL;
        }
    }
}

//------------------------------------------------------------------------------
//  Private functions

void Stream::initialize(
    void)
{

    if (!Stream::initialized)
    {
        Stream::initialized = true;
#if defined(WIN32)
        WORD wVersionRequested = MAKEWORD(2, 2);
        WSADATA wsaData;
        if (WSAStartup(wVersionRequested, &wsaData) != 0)
            error("Initialising Winsock 2 on Windows failed: %d", WSAGetLastError());
#else
        if (pthread_mutex_init(&Stream::mutex, NULL) != 0)
            error("Pthreads error: Cannot create stream class mutex, errno=%d", errno);
#endif
    }
}

int Stream::next_seqn(
    void)
{

    static int seqn = 101;

#if defined(WIN32)
    // nothing
#else
    if (pthread_mutex_lock(&Stream::mutex) != 0)
        error("Pthreads error: Cannot lock stream class mutex, errno=%d", errno);
#endif

    int nextseqn = seqn++;

#if defined(WIN32)
    // nothing
#else
    if (pthread_mutex_unlock(&Stream::mutex) != 0)
        error("Pthreads error: Cannot unlock stream class mutex, errno=%d", errno);
#endif

    return nextseqn;
}

void Stream::trace(
    const char *const reason,
    ...)
{

    if (this->tracefunction != NULL)
    {
        va_list arguments;
        char string[MAXSTRING];

        va_start(arguments, reason);
        vsprintf(string, reason, arguments);
        va_end(arguments);

        this->tracefunction(string);
    }
}

char *
Stream::hostname(
    void)
{

    static char buffer[MAXSTRING + 1]; // not thread safe, but OK
    static bool initialized = false;

    if (!initialized)
    {
        initialized = true;

        buffer[MAXSTRING] = '\0';
        if (gethostname(buffer, sizeof buffer) != 0)
            error("Cannot get hostname");
        if (strlen(buffer) == MAXSTRING)
            error("hostname too long");

        for (char *bp = buffer; *bp != '\0'; *bp++)
        { // truncate domain
            if (*bp == '.')
            {
                *bp = '\0';
                break;
            }
        }
    }

    return buffer;
}

void Stream::parse_name(
    char *string,   // in
    char *hostname, // out, MAXSTRING chars available
    int *port       // out
)
{

    if (string == NULL)
        error("Null string in parse_name");

    while (*string != '\0' && *string != ':')
        *hostname++ = *string++;

    *hostname = '\0';
    *port = atoi(string + 1);
}

char *
Stream::lookup_host(
    char *hostname)
{

    static char ipaddr[INET6_ADDRSTRLEN]; // Not thread-safe, but OK as-is
    struct addrinfo hints = {0};
    struct addrinfo *result = NULL;

    // Try IPv6 first
    hints.ai_family = AF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    int status = getaddrinfo(hostname, NULL, &hints, &result);
    if (status == 0 && result != NULL)
    {
        if (result->ai_addrlen == sizeof(struct sockaddr_in6))
        {
            struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)result->ai_addr;
            inet_ntop(AF_INET6, &ipv6->sin6_addr, ipaddr, sizeof(ipaddr));
            freeaddrinfo(result);
            return ipaddr;
        }
        freeaddrinfo(result);
    }

    // Fallback to IPv4
    hints.ai_family = AF_INET;
    status = getaddrinfo(hostname, NULL, &hints, &result);
    if (status != 0)
    {
        error("Cannot get IP address of host \"%s\": %s", hostname, gai_strerror(status));
        return NULL;
    }

    if (result == NULL || result->ai_addrlen != sizeof(struct sockaddr_in))
    {
        freeaddrinfo(result);
        error("No valid IP address found for \"%s\"", hostname);
        return NULL;
    }

    struct sockaddr_in *ipv4 = (struct sockaddr_in *)result->ai_addr;
    inet_ntop(AF_INET, &ipv4->sin_addr, ipaddr, sizeof(ipaddr));

    freeaddrinfo(result);
    return ipaddr;
}

char *
Stream::lookup_dotaddr(
    char *ipdotaddr)
{

    // Map IP address to an unqualified host name (IPv6 dual-stack, cross-platform)

    static char hostname[MAXSTRING]; // Not thread-safe, but OK
    char host[MAXSTRING];
    struct sockaddr_in6 addr = {0};
    addr.sin6_family = AF_INET6;
    int status = inet_pton(AF_INET6, ipdotaddr, &addr.sin6_addr);
    int name_status;

    if (status == 1)
    {
        if (IN6_IS_ADDR_UNSPECIFIED(&addr.sin6_addr))
        {
            return (char *)"localhost"; // Or appropriate hostname
        }
        socklen_t addrlen = sizeof(struct sockaddr_in6);
        name_status = getnameinfo((struct sockaddr *)&addr, addrlen, host, MAXSTRING, NULL, 0, NI_NAMEREQD);
    }
    else
    {
        // Fallback to IPv4
        struct sockaddr_in addr4 = {0};
        addr4.sin_family = AF_INET;
        status = inet_pton(AF_INET, ipdotaddr, &addr4.sin_addr);
        socklen_t addrlen = sizeof(struct sockaddr_in);
        name_status = getnameinfo((struct sockaddr *)&addr4, addrlen, host, MAXSTRING, NULL, 0, NI_NAMEREQD);
    }

    if (name_status != 0)
    {
        // Fallback to IP string if reverse DNS fails
        strncpy(hostname, ipdotaddr, MAXSTRING);
        hostname[MAXSTRING - 1] = '\0';
        return hostname;
    }

    // Truncate at first dot (unqualified hostname)
    char *dp = hostname;
    const char *cp = host;
    while (*cp != '\0' && *cp != '.')
    {
        *dp++ = *cp++;
    }
    *dp = '\0';

    return hostname;
}

char *
Stream::dotipaddr(
    struct in6_addr addr)
{

    // Convert IPv6 address to string (handles IPv4-mapped)

    static char dotaddr[INET6_ADDRSTRLEN]; // not thread safe, but OK

    if (IN6_IS_ADDR_V4MAPPED(&addr))
    {
        // Extract and format IPv4
        uint32_t ipv4 = ntohl(((uint32_t *)&addr.s6_addr[12])[0]);
        snprintf(dotaddr, sizeof(dotaddr), "%d.%d.%d.%d", (ipv4 >> 24) & 0xFF, (ipv4 >> 16) & 0xFF, (ipv4 >> 8) & 0xFF, ipv4 & 0xFF);
    }
    else
    {
        if (inet_ntop(AF_INET6, &addr, dotaddr, sizeof(dotaddr)) == NULL)
        {
            // Handle rare failure
            strcpy(dotaddr, "[invalid]");
        }
    }

    return dotaddr;
}

void Stream::error(
    const char *const reason,
    ...)
{

    va_list arguments;
    char string[MAXSTRING];

    va_start(arguments, reason);
    vsprintf(string, reason, arguments);
    va_end(arguments);

    if (this->errorfunction != NULL)
    {
        this->errorfunction(string);
    }
    else
    {
        fflush(stdout);
        fprintf(stderr, "Fatal error: %s\n", string);
        fflush(stderr);
        exit(2);
    }
}