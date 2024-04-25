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
// smalltest.cpp --
//     Small test program for the stream library suitable for Windows
//

#include "stream.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if HAVE_CONFIG_H
#define Sleep usleep
#define SEC 1000000.0
#else
#define SEC 1000.0
#endif

// printError, printTrace --
//     Auxiliary functions to monitor what is going on
//
void printError( char *msg ) {
    printf( "Error: %s\n", msg );
}
void printTrace( char *msg ) {
    printf( "Trace: %s\n", msg );
}


// sendMessages --
//     Send ten short messages and stop
//
// Arguments:
//     host        Name of the host to send to (including port)
//     streamtype  Type of stream
//

void sendMessages( char *host, Stream::StreamType streamtype ) {

    FILE *infile = fopen( "smalltest.handle", "r");
    fgets( host, 20, infile );
    fclose( infile ) ;

    Stream stream( streamtype, host, printError, printTrace );

    int msgcount = 10;
    char msg[20];
    sprintf( msg, "%d", msgcount );
    stream.Send( msg, sizeof(msg) ); /* NOTE: send and receive must use the same message size! */
    for ( int i = 1; i <= msgcount; i ++ ) {
        sprintf( msg, "Message %d", i );
        stream.Send( msg, sizeof(msg) ); /* NOTE: send and receive must use the same message size! */
            printf( "Sleep for 3 seconds ...\n" );
        Sleep ( 3 * SEC );
    }
        printf( "Sleep for 5 seconds ...\n" );
        Sleep ( 5 * SEC );
    exit(0);
}


// receiveMessages --
//     ReceiveSend ten short messages and stop
//
// Arguments:
//     host        Name of the host to send to (including port)
//     streamtype  Type of stream
//

void receiveMessages( char *host, Stream::StreamType streamtype ) {
    Stream stream( streamtype, printError, printTrace );

    FILE *outfile = fopen( "smalltest.handle", "w");
    fputs( stream.LocalHandle(), outfile );
    fclose( outfile ) ;

    stream.Connect();

    int msgreccount;
    char msg[20];
    stream.Receive( msg, sizeof(msg) );
    msgreccount = atoi (msg);
    for ( int i = 1; i <= msgreccount; i ++ ) {
        stream.Receive( msg, sizeof(msg) );
        printf( "Received: %s\n", msg );
    }
    exit(0);
}


// main program:
// Arguments:
// send     Send a set of messages to the localhost, after that exit
// receive  Receive messages from the client and print on the console

int main( int argc, char *argv[], char *envp[] ) {

    Stream::StreamType streamtype = Stream::TCPIP;

    if ( argc <= 1 ) {
        printf("Usage: %s send|receive\n", argv[0]);
        exit(0);
    }

    if ( strcmp(argv[1], "send") == 0 ) {
        char host[] = "localhost:17000";
        sendMessages(host, streamtype);
    } else {
        receiveMessages(NULL, streamtype);
    }
    return 0;
}


