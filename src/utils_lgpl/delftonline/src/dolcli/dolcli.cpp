//------------------------------------------------------------------------------
//  DelftOnline -- Interactive Client Program
//  MAIN
//
//  Irv.Elshoff@Deltares.NL
//  24 may 12
//
//------------------------------------------------------------------------------
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


#define DOLCLIENT_MAIN

#include "dolcli.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#if (!defined (WIN32))
#include <unistd.h>
#else
static int getpid() { return 0; }
#endif


static void
usage (
    void
    ) {

    printf ("Usage: %s [<options>] <url>\n", Global->progname);
    printf ("\t -v     Increase DOL verbosity\n");
    printf ("\t -?     Print this help\n");
    }


//-------------------------------------------------------------------------------
//  MAIN


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    Global = new struct Global_st;
    Global->progname = argv[0];

    int verbosity = 0;

    // Process command-line arguments

    int c;
    while ((c = getopt (argc, argv, "v?")) != -1) {
        switch (c) {
            case 'v':
                verbosity++;
                break;

            case '?':
                usage ();
                return 0;

            default:
                usage ();
                Abort ("Invalid command-line arguments");
                break;
            }
        }

    if (optind+1 != argc) {
        usage ();
        Abort ("Invalid command-line arguments");
        }

    char * url = argv[argc-1];

    // Set verbosity level

    DOL::Verbosity dolverb;
    switch (verbosity) {
        case 0:     dolverb = DOL::SILENT; break;
        case 1:     dolverb = DOL::ERROR;  break;
        case 2:     dolverb = DOL::INFO;   break;
        default:    dolverb = DOL::TRACE;  break;
        }

    // Connect to DOL server

    try {
        printf ("Client connecting to \"%s\"\n", url);
        Global->dol = new DOL::Client (url, dolverb, NULL);
        }
    catch (DOL::Exception * ex) {
        Abort ("DOL client initialization fails: %s", ex->message);
        }

    // Process commands

    const char * prompt = NULL;
    bool history = false;
    srandom (getpid ());

    if (isatty (0)) {
        printf ("Interactive DOL version %d.%d client ready\n", DOL::VERSION_MAJOR, DOL::VERSION_MINOR);
        printf ("Type \"?\" for a command synopsis\n");
        prompt = ">> ";
        using_history ();
        history = true;
	}

    char * line = NULL;
    char lastline [KILOBYTE];
    lastline [0] = '\0';

    while (! Global->input.quit) {
        if (line != NULL) free (line);

        if (prompt == NULL) {
            if ((line = (char *) malloc (KILOBYTE+1)) == NULL)
                Abort ("Cannot allocate %d bytes for command input", KILOBYTE+1);

            if (fgets (line, KILOBYTE, stdin) == NULL) break;
            line [strlen (line)-1] = '\0';
            }

        else {
            if ((line = readline (prompt)) == NULL) break;

            char * expanded;
            switch (history_expand (line, &expanded)) {
                case 1:
                    printf ("%s%s\n", prompt, expanded);
                    // fall thru
                case 0:
                    free (line);
                    line = expanded;
                    break;

                case -1:
                    printf ("History expansion error: %s\n", expanded);
                    free (expanded);
                    line [0] = '\0';
                    break;

                case 2:
                    printf ("History expansion is: \"%s\"\n", expanded);
                    free (expanded);
                    line [0] = '\0';
                    break;

                default:
                    break;
                }
            }

        Global->input.linenum++;
        if (strlen (line) == 0) continue;

        strcpy (Global->input.line, line);
        strcat (Global->input.line, "\n");   // give yylex a newline to chew on

        Global->input.ip = Global->input.line;
        Global->input.ep = Global->input.line + strlen (Global->input.line);

        try {
            if (yyparse () != 0)
                throw "Warning: Parser terminated abnormally";
            }
        catch (DOL::Exception * ex) {
            printf ("ERROR: %s\n", ex->message);
            if (ex->fatal) exit(1);
            }
        catch (char * ex) {
            if (ex != NULL)
                printf ("%s\n", ex);
            }

        if (history && strcmp (line, lastline) != 0 && line[0] != 'q') {
            add_history (line);
            strncpy (lastline, line, KILOBYTE-1);
            lastline [KILOBYTE-1] = '\0';
            }
        }

    if (line != NULL) free (line);

    delete Global->dol;
    printf ("Done.\n");
    }


//-------------------------------------------------------------------------------



void
GetElement (
    const char * name,
    const char * filename
    ) {

    //FILE * file = stdout;

    DOL::Client::DataElement * elt = Global->dol->GetDataElement (name);
    if (elt == NULL) {
        printf ("DataElement \"%s\" not found\n", name);
        return;
        }

    unsigned char * buffer = new unsigned char [elt->size];
    Global->dol->GetData (elt->pathname, buffer, elt->size);

    if (filename != NULL) {
        int file;
        if ((file = open (filename, O_WRONLY | O_CREAT | O_TRUNC)) < 0) {
            printf ("ERROR: Cannot create output file \"%s\": %s\n", filename, strerror (errno));
            delete [] buffer;
            return;
            }

        int count = elt->size;
        int position = 0;
        int written;
        while (count > 0 && (written = write (file, buffer+position, elt->size)) > 0) {
            count -= written;
            position += written;
            }

        if (written < 0)
            printf ("ERROR: Cannot write to output file \"%s\": %s\n", filename, strerror (errno));

        delete [] buffer;
        return;
        }


    if (elt->arrayshape == NULL) {
        switch (elt->basetype) {
            case DOL::OPAQUE:
                printf ("Opaque scalars not supported in this client\n");
                break;
            case DOL::INTEGER:
                printf ("%d\n", *((int *) buffer));
                break;
            case DOL::REAL:
                printf ("%g\n", *((float *) buffer));
                break;
            case DOL::DOUBLE:
                printf ("%g\n", *((double *) buffer));
                break;
            case DOL::DOUBLECOMPLEX:
                printf ("DoubleComplex scalars not supported in this client\n");
                break;
            case DOL::COMPLEX:
                printf ("Complex scalars not supported in this client\n");
                break;
            case DOL::LOGICAL:
                printf ("Logical scalars not supported in this client\n");
                break;
            case DOL::CHARACTER:
                printf ("%c\n", buffer[0]);
                break;
            }
        }

    else {
        switch (elt->basetype) {
            case DOL::OPAQUE:
                printf ("Opaque arrays not supported in this client\n");
                break;
            case DOL::INTEGER:
                for (unsigned int i = 0 ; i < elt->arrayelements ; i++)
                    printf ("%d\t%d\n", i, *((int *) buffer+i));
                break;
            case DOL::REAL:
                for (unsigned int i = 0 ; i < elt->arrayelements ; i++)
                    printf ("%d\t%g\n", i, *((float *) buffer+i));
                break;
            case DOL::DOUBLE:
                for (unsigned int i = 0 ; i < elt->arrayelements ; i++)
                    printf ("%d\t%g\n", i, *((double *) buffer+i));
                break;
            case DOL::DOUBLECOMPLEX:
                printf ("DoubleComplex arrays not supported in this client\n");
                break;
            case DOL::COMPLEX:
                printf ("Complex arrays not supported in this client\n");
                break;
            case DOL::LOGICAL:
                printf ("Logical arrays not supported in this client\n");
                break;
            case DOL::CHARACTER:
                for (unsigned int i = 0 ; i < elt->arrayelements ; i++)
                    printf ("%d\t%c\n", i, buffer[i]);
                break;
            }
        }





    delete [] buffer;
    }



//-------------------------------------------------------------------------------


void
ListDirectory (
    const char * dirname
    ) {

    DOL::Client::Directory * dir = Global->dol->GetDirectory (dirname);
    if (dir == NULL)
        printf ("Directory \"%s\" not found\n", dirname);

    else {
        printf ("Directory \"%s\" contents:\n", dir->pathname);
        if (dir->subdirs.count > 0) {
            printf ("    Directories:\n");
            for (int i = 0 ; i < dir->subdirs.count ; i++)
                printf ("        %s\n", dir->subdirs.name[i]);
            }

        if (dir->arrays.count > 0) {
            printf ("    ArrayShapes:\n");
            for (int i = 0 ; i < dir->arrays.count ; i++)
                printf ("        %s\n", dir->arrays.name[i]);
            }

        if (dir->elements.count > 0) {
            printf ("    DataElements:\n");
            for (int i = 0 ; i < dir->elements.count ; i++)
                printf ("        %s\n", dir->elements.name[i]);
            }

        if (dir->functions.count > 0) {
            printf ("    Functions:\n");
            for (int i = 0 ; i < dir->functions.count ; i++)
                printf ("        %s\n", dir->functions.name[i]);
            }

        delete dir;
        }
    }


//-------------------------------------------------------------------------------


void
Abort (
    const char *    format,
    ...
    ) {

    int size = 100*1000;    // 1000*1000 causes problems in Java threads
    char buffer [size];     // really big temporary buffer

    va_list arguments;
    va_start (arguments, format);
    vsnprintf (buffer, size, format, arguments);
    va_end (arguments);

    printf ("%s ABORT: %s\n", Global->progname, buffer);
    exit (1);
    }
