//-------------------------------------------------------------------------------
//  DelftOnline -- General Utilities
//
//  Irv.Elshoff@Deltares.NL
//  30 jun 12
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


unsigned int
BaseTypeSize (
    BaseType       basetype
    ) {

    // ToDo: Is this the same on 32-bit and 64-bit systems?
    // Is this compiler (option) dependent?

    return (
        (basetype == OPAQUE)           ?  0 :
        (basetype == INTEGER)          ?  4 :
        (basetype == REAL)             ?  4 :
        (basetype == DOUBLE)           ?  8 :
        (basetype == DOUBLECOMPLEX)    ? 16 :
        (basetype == COMPLEX)          ?  8 :
        (basetype == LOGICAL)          ?  4 :
        (basetype == CHARACTER)        ?  1 :
        0
        );
    }


//-------------------------------------------------------------------------------
//  DOL Constant-to-string Functions


const char *
BaseTypeString (
    BaseType       basetype
    ) {

    return (
        (basetype == OPAQUE)           ? "Opaque" :
        (basetype == INTEGER)          ? "Integer" :
        (basetype == REAL)             ? "Real" :
        (basetype == DOUBLE)           ? "DoublePrecision" :
        (basetype == DOUBLECOMPLEX)    ? "DoublePrecisionComplex" :
        (basetype == COMPLEX)          ? "Complex" :
        (basetype == LOGICAL)          ? "Logical" :
        (basetype == CHARACTER)        ? "Character" :
        "unknown"
        );
    }


const char *
AccessModeString (
    AccessMode     accessmode
    ) {

    return (
        (accessmode == IN)             ? "In" :
        (accessmode == OUT)            ? "Out" :
        (accessmode == INOUT)          ? "In+Out" :
        "unknown"
        );
    }


const char *
MessageTypeString (
    Message::Type type
    ) {

    return (
        (type == Message::ARRAY_SHAPE)       ? "ARRAY_SHAPE" :
        (type == Message::CALL_FUNCTION)     ? "CALL_FUNCTION" :
        (type == Message::CHANGE_DIR)        ? "CHANGE_DIR" :
        (type == Message::GET_DATA)          ? "GET_DATA" :
        (type == Message::GET_DESCRIPTION)   ? "GET_DESCRIPTION" :
        (type == Message::GET_DIR)           ? "GET_DIR" :
        (type == Message::GET_ELEMENT)       ? "GET_ELEMENT" :
        (type == Message::GET_FUNCTION)      ? "GET_FUNCTION" :
        (type == Message::GET_THREADCOUNT)   ? "GET_THREADCOUNT" :
        (type == Message::GET_THREADNAME)    ? "GET_THREADNAME" :
        (type == Message::GOODBYE)           ? "GOODBYE" :
        (type == Message::HELLO)             ? "HELLO" :
        (type == Message::PUT_DATA)          ? "PUT_DATA" :
        (type == Message::SERVER_STATUS)     ? "SERVER_STATUS" :
        (type == Message::START)             ? "START" :
        (type == Message::STEP)              ? "STEP" :
        (type == Message::STOP)              ? "STOP" :
        (type == Message::TERMINATE)         ? "TERMINATE" :
        "<unknown>"
        );
    }


//-------------------------------------------------------------------------------
//  Message Passing Routines


size_t
Receive (
    int     sock,
    Message::Header * mesg
    ) {

    size_t received = recv (sock, (char *) mesg, Message::maxMesgSize, 0);
    if (received < 0)
        throw strerror (errno);
    if (received == 0)
        throw (char *) "Zero bytes read (peer dead?)";

//    FILE * logfile = fopen ("DOL.log", "a");
//    if (logfile == NULL)
//        throw (char *) "Cannot open DOL.log";

//    fprintf (logfile, "Received message %5d  %7d bytes (mesg->size = %d)\n", mesg->seqn, received, mesg->size);
//    fclose (logfile);

    return received;
    }


void
Send (
    int     sock,
    Message::Header * mesg
    ) {

//    FILE * logfile = fopen ("DOL.log", "a");
//    if (logfile == NULL)
//        throw (char *) "Cannot open DOL.log";

//    fprintf (logfile, "Sending  message %5d  %7d bytes\n", mesg->seqn, mesg->size);
//    fclose (logfile);

    usleep(2);

    int size = sizeof (Message::Header) + mesg->size;
    ssize_t sent = send (sock, (char *) mesg, (ssize_t) size, 0);
    if (sent < 0)
        throw strerror (errno);
    if (sent == 0)
        throw (char *) "Zero bytes written (peer dead?)";
    if (sent < size)
        throw (char *) "Sent fewer bytes than message size";
    }


//-------------------------------------------------------------------------------
//  String processing for directory operations


const char *
ResolvePathName (
    const char * pathname,
    const char * curDir
    ) {

    //  This function takes a pathname and resolves it relative to a
    //  current directory.  The easy case is when the pathname is absolute.
    //  If it's relative we could just concatentate the current directory,
    //  a slash and the pathname and be done.
    //  In any case we clean up the result by eliminating path components
    //  dot and dot dot, and double and trailing slashes, so that the result
    //  pathname is "optimal".
    //  We do this by deleting superfluous characters from the concatention
    //  and then compacting the result.
    //  This function MUST be called with MUTEX because of a static result.


    if (pathname[0] == '/') {
        if (pathname[1] == '\0')
            return "/";
        else
            curDir = "";
        }

    int len = strlen (pathname) + strlen (curDir) + 2;
    if (len > MaxPathNameLength)
        return NULL;

    static char buffer [MaxPathNameLength+4];
    for (int i = 0 ; i < MaxPathNameLength+4 ; i++) buffer[i] = '\0';

    sprintf (buffer, "%s/%s", curDir, pathname);

    //  Get rid of trailing slashes

    for (int i = len-1 ; i >= 0 ; i--)
        if (buffer[i] == '/')
            buffer[i] = '\0';
        else if (buffer[i] != '\0')
            break;

    //  Get rid of double slashes

    for (int i = 0 ; i < len ; i++)
        if (buffer[i] == '/' && buffer[i+1] == '/')
            buffer[i] = '\0';

    //  Get rid of /./

    for (int i = 0 ; i < len ; i++)
        if (buffer[i] == '/' && buffer[i+1] == '.' && (buffer[i+2] == '/' || buffer[i+2] == '\0')) {
            buffer[i] = '\0';
            buffer[i+1] = '\0';
            }

    //  Get rid of leading ./

    for (int i = 0 ; i < len ; i++) {
        if (buffer[i] == '\0')
            continue;
        if (buffer[i] != '.')
            break;

        if (buffer[i+1] == '/') {
            buffer[i] = '\0';
            buffer[i+1] = '\0';
            break;
            }
        }

    //  Get rid of /../

    for (int i = 0 ; i < len ; i++) {
        if (buffer[i] == '/' && buffer[i+1] == '.' && buffer[i+2] == '.' && (buffer[i+3] == '/' || buffer[i+3] == '\0')) {
            for (int j = i-1 ; j >= 0 ; j--) {
                if (buffer[j] == '/') {
                    for (int k = j ; k < i+3 ; k++)
                        buffer[k] = '\0';
                    break;
                    }
                }
            }
        }

    //  The above may result in a path that starts with /../ if there are too
    //  many .. components.  Get rid of that prefix

    for (int i = 0 ; i < len ; i++) {
        if (buffer[i] == '\0')
            continue;
        if (buffer[i] != '/')
            break;

        if (buffer[i+1] == '.' && buffer[i+2] == '.' && buffer[i+3] == '/') {
            buffer[i+1] = '\0';
            buffer[i+2] = '\0';
            buffer[i+3] = '\0';
            break;
            }
        }

    // If nothing is left over, return "/"

    bool nothing = true;
    for (int i = 0 ; i < len ; i++) {
        if (buffer[i] != '\0') {
            nothing = false;
            break;
            }
        }

    if (nothing)
        return "/";

    //  Compact buffer

    static char result [MaxPathNameLength+4];
    for (int i = 0 ; i < MaxPathNameLength+4 ; i++) result[i] = '\0';

    for (int i = 0, j = 0 ; i < len ; i++)
        if (buffer[i] != '\0')
            result[j++] = buffer[i];

    return result;
    }


//-------------------------------------------------------------------------------
//  Networking


bool
LookupHostname (
    char *  hostname,
    struct sockaddr * addr
    ) {

    //  This routine looks up the specified hostname (unqualified, FQDN, or
    //  dot notation IP-address) and fills in the provided sockaddr structure
    //  Returns true if all went well, false otherwise.

    struct protoent *proto;
    if ((proto = getprotobyname ("tcp")) == NULL)
        return false;

    struct addrinfo hints;
    memset ((void *) &hints, 0, sizeof hints);
    hints.ai_flags = AI_CANONNAME;
    hints.ai_family = PF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = proto->p_proto;

    struct addrinfo *result;
    if (getaddrinfo (hostname, NULL, &hints, &result) != 0 || result == NULL)
        return false;

    *addr = *result->ai_addr;
    freeaddrinfo (result);
    return true;
    }

}


//-------------------------------------------------------------------------------


char *
GETVERSION (
    void
    ) {

    extern char * getfullversionstring_delftonline (void);
    return getfullversionstring_delftonline();
    }

