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
/* readline.c --

   Routine(s) to facilitate reading text files

   Arjen Markus
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "readline.h"

/* ------------------------------------------------------------------
    Function: readLine()
    Author:   Arjen Markus
    Purpose:  Read a line like fgets()
    Context:  Used by ODS routines
    Pseudo Code:
              Read a line using fgets(), but then check if we
              have reached the end yet. If not, read until the
              newline character. Keep the newline (but delete
              a carriage return)
    Note:
              If an error occurs the buffer is emptied, this
              is a different behaviour from fgets().
------------------------------------------------------------------ */
char *
readLine( char *buffer, int bufs, FILE *stream )
{
   char  dummy[100] ;
   char *result     ;
   char *pstr       ;

   /* Read the buffer */
   result = fgets( buffer, bufs, stream ) ;

   /* Decide if it is necessary to carry on reading */
   if ( result == NULL )
   {
      buffer[0] = '\0' ;
      return NULL ;
   }
   else
   {
      pstr = strchr( result, '\n' ) ;
      while ( pstr == NULL && ! feof(stream) )
      {
         result = fgets( dummy, sizeof(dummy), stream ) ;
         pstr = strchr( result, '\n' ) ;
      }

      /* Get rid of "\r" if there is any */
      pstr = strchr( buffer, '\r' ) ;
      if ( pstr != NULL ) *pstr = '\n' ;
   }

   if ( feof(stream) )
   {
      strcat( buffer, "\n" ) ;
   }

   if ( ferror(stream) )
   {
      buffer[0] = '\0' ;
      return NULL ;
   }
   else
   {
      return buffer ;
   }
}

/* Test driver */
#if defined(TEST_READLINE)
int main( int argc, char *argv[] )
{
   char  line[20] ;
   FILE *infile   ;

   infile = fopen( "arjen.inp", "r" ) ;

   while( !feof(infile) )
   {
      readLine( line, sizeof(line), infile ) ;
      printf( "line: >%s<\n", line ) ;
   }
   fclose( infile ) ;

   return 0 ;
}
#endif
