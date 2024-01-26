//------------------------------------------------------------------------------
//  DelftOnline -- Interactive Client Program
//  Definitions
//
//  Irv.Elshoff@Deltares.NL
//  7 may 12
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

#pragma once

#include "DelftOnline.h"

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if (!defined (WIN32))
#include <unistd.h>
#include <readline/history.h>
#include <readline/readline.h>
#else
#   define getpid   _getpid
#   define random   rand
#   define srandom  srand
#   define strdup   _strdup
#   define usleep Sleep
#endif



#define KILOBYTE    1000
#define HIST_SIZE   200             // max number of commands to read from history

#define max(A,B)        ((A) > (B) ? (A) : (B))
#define min(A,B)        ((A) < (B) ? (A) : (B))


void    Abort           (const char *, ...);

void
GetElement (
    const char * name,
    const char * filename
    );

void
ListDirectory (
    const char * dirname
    );


//-------------------------------------------------------------------------------


struct Global_st {
    char *          progname;
    DOL::Client *   dol;

    struct {
        int         linenum;            // line number of command input
        char        line [KILOBYTE];    // command line read from (standard) input
        char *      ip;                 // input pointer in line
        char *      ep;                 // end pointer in line
        bool        quit;               // true means stop reading input
        } input;
    };


#ifdef DOLCLIENT_MAIN
#define Extern
#else
#define Extern extern
#endif

Extern Global_st * Global;


//------------------------------------------------------------------------------
//  Lex/YACC declarations (for config.[ly] and command.[ly]


extern int  yylex      (void);
extern int  yyparse    (void);
