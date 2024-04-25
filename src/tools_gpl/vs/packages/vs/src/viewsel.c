//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
/*

 System      : Viewer/Selector

 $Header: /delft3d/libraries/vs/progsrc/viewsel.c 4     9/09/05 14:44 Mooiman $

 Programmer  : Abe.Hoekstra - CSO
 Part        : Viewsel

 $Log: /delft3d/libraries/vs/progsrc/viewsel.c $
*/

static char rcsid[] = "$Id$";

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gen.h"

extern void getFullVersionString_VS(char *);
extern void yyparse ( void ) ;


char * Gl_pager;

/*  @@
 */
int main( void )
{
    char * GL_pager;
    int  pos        ;
    char format[30] ;

    char * ident = (char *) malloc(256*sizeof(char));
    getFullVersionString_VS(ident);

#ifndef MSDOS
#ifndef WIN32
    /* Ignore signals from a pipe */
    signal ( SIGPIPE, SIG_IGN );
#endif
#endif
#if defined WIN32
    signal (SIGINT, SIG_IGN );
#endif

    (BVoid) GEN_init( );

    /* try to find the pager program */
    GL_pager = getenv ( "PAGER" );
    if ( GL_pager == NULL ) {
      fprintf ( stderr, "Environment Variable PAGER not set\n" );
      exit ( 1 );
    }

    Gl_pager = (char *) malloc( strlen(GL_pager)+3);
    Gl_pager[0] = '\0';
    strcat(Gl_pager, "\"");
    strcat(Gl_pager, GL_pager);
    strcat(Gl_pager, "\"");

    /* parse de file */
    pos = strchr(ident,';')-&ident[4] ;
    sprintf( format, "* %%%d.%ds\n", pos, pos ) ;
    fprintf ( stderr, "*\n");
    fprintf ( stderr, "* %s\n", &ident[4]);
    fprintf ( stderr, "*\n");
    fprintf ( stderr, "type ? for help\n>>" );
    yyparse () ;

    free(ident);

    return 0;
}
