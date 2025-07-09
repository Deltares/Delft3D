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
//  Interface to TCP/IP Stream library for getarray/putarray functionality
//
//  Contains C routines called from Fortran.  Only Linux and Intel compiler
//  bindings are supported.
//
//  Irv.elshoff@deltares.nl
//  Adri.Mourits@deltares.nl
//  20 mar 06
//
//------------------------------------------------------------------------------



#include "getputarray.h"

#if defined(linux)
#define Sleep usleep
#define SEC 1000000.0
#else
#define SEC 1000.0
#endif


//------------------------------------------------------------------------------
//  API Functions

int STDCALL
CREATESTREAM (
    char    *filen,
    int     filen_length
    ) {

    char * filename = new char [filen_length+1];

    fortstr2cstr (filen, filen_length, filename);

    // Create a stream and write its handle to a file

    Stream * stream = new Stream (Stream::DEFAULT, &StreamError, &StreamTrace);

    FILE * handlefile;
    if ((handlefile = fopen (filename, "w")) == NULL) {
        fprintf (stderr, "ABORT: CreateStream cannot create file %s\n", filename);
        exit (1);
        }

    char * handle = stream->LocalHandle ();
    fputs (handle, handlefile);
    fclose (handlefile);

    // printf ("CreateStream put handle \"%s\" into file \"%s\"\n", handle, filename);
    fflush (stdout);

    // Complete the connection to our peer

    stream->Connect ();
    // printf ("CreateStream connected to peer on \"%s\"\n", handle);
    fflush (stdout);

    if (++Maxstream >= MAXSTREAMS) {
        fprintf (stderr, "ABORT: Too many streams in CreateStream\n");
        exit (1);
        }

    Danostream [Maxstream] = stream;
    return Maxstream;
    }


int STDCALL
GETSTREAM (
    char    *filen,
    int     filen_length
    ) {

    if (++Maxstream >= MAXSTREAMS) {
        fprintf (stderr, "ABORT: Too many streams in CreateStream\n");
        exit (1);
        }
    char * filename = new char [filen_length+1];
    fortstr2cstr (filen, filen_length, filename);

    // Wait for file to appear, then read stream handle and delete file

    // printf ("GetStream waiting for file \"%s\"...\n", filename);
    fflush (stdout);

    char handle [BUFSIZE];
    while (true) {
        FILE * handlefile;
        if ((handlefile = fopen (filename, "r")) != NULL) {
            fgets (handle, BUFSIZE, handlefile);
            fclose (handlefile);

            if (unlink (filename) != 0) {
                fprintf (stderr, "ABORT: GetStream cannot delete file %s\n", filename);
                exit (1);
                }

            break;
            }

        Sleep ( 1.0 * SEC );
        }

    // printf ("GetStream got handle \"%s\" from file \"%s\"\n", handle, filename);
    fflush (stdout);

    // Connect to stream

    Stream * stream = new Stream (Stream::DEFAULT, handle, &StreamError, &StreamTrace);
    // printf ("GetStream connected to handle \"%s\"\n", handle);
    fflush (stdout);

    Danostream [Maxstream] = stream;
    return Maxstream;
    }


void STDCALL
GETARRAY (
    int *   handleID,
    double * array,
    int *   size
    ) {

    Stream * stream = Danostream [*handleID];
    stream->Receive ((char *) array, sizeof (double) * *size);
    }


void STDCALL
PUTARRAY (
    int *   handleID,
    double * array,
    int *   size
    ) {

    Stream * stream = Danostream [*handleID];
    stream->Send ((char *) array, sizeof (double) * *size);
    }




//------------------------------------------------------------------------------
//  Utility Functions


void
fortstr2cstr (
    char *  fstr,
    int     len,
    char *  cstr
    ) {

    // Function to convert a space-padded FORTRAN string to a
    // null-terminated C string.

    int     i;

    //    Look for the last non-space character in the character array

    for (i = len-2 ; i >= 0 ; i--)
        if (fstr[i] != ' ')
            break;

    //    Copy backwards all the relevant characters

    cstr[i+1] = '\0';
    for ( ; i >= 0 ; i--)
        cstr[i] = fstr[i];
    }


void
StreamError (
    char *  reason
    ) {

    printf ("GetPutArrayError: ABORT %s\n", reason);
    exit(1);
    }


void
StreamTrace (
    char *  reason
    ) {

    // printf ("GetPutArrayTrace: %s\n", reason);
    }



