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
//  Platform-dependent definitions
//
//  Irv.Elshoff@deltares.nl
//  8 oct 05
//
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
//  Linux

#if defined (linux) || defined (IRIX)
#   include <arpa/inet.h>
#   include <netdb.h>
#   include <netinet/in.h>
#   include <sys/socket.h>
#   include <sys/time.h>
#   include <unistd.h>
typedef in_addr_t           IPaddr;         // IP address
typedef in_port_t           IPport;         // IP port number
typedef struct sockaddr_in6 Sockaddr;       // socket address
#   define MicroSleep  usleep


//------------------------------------------------------------------------------
//  Microsoft Windows

#elif defined (WIN32)
#   ifndef _WIN32_WINNT
#       define _WIN32_WINNT 0x0600  // Enable Winsock 2 (Vista+)
#   endif
#   define WIN32_LEAN_AND_MEAN
#   define _WINSOCK_DEPRECATED_NO_WARNINGS
#   include <winsock2.h>  // Use Winsock 2 instead of winsock.h
#   include <ws2tcpip.h>
#   include <io.h>
// ToDo: Replace following with real definitions
typedef int                 IPaddr;         // IP address
typedef short               IPport;         // IP port number
typedef struct sockaddr_in6 Sockaddr;       // socket address
#   define MicroSleep  _sleep


//------------------------------------------------------------------------------
//  Undefined platform; syntax error to force compiler abort

#else
    Error: Platform not set!
#endif
