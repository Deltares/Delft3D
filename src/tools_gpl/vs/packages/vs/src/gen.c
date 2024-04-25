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

        Viewer/Selector convenience functions
        -------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>

#ifdef HAVE_CONFIG_H
#ifdef HAVE_MALLOC_H
#   include <malloc.h>
#endif
#ifdef HAVE_MALLOC_MALLOC_H
#   include <malloc/malloc.h>
#endif
#else
#if !defined (__convex)
#   include <malloc.h>
#endif
#endif

#if defined MSDOS || WIN32
#   include <io.h>
#else /* !MSDOS */
#   include <unistd.h>
#endif

#include "au.h"

extern BText Gl_pager;
extern void getFullVersionString_VS(char *);

static FILE * AuErrFile = NULL;
static FILE * pipefile  = NULL;


BVoid GEN_init ()
{
   AuErrFile=stderr;
}


BData GEN_malloc ( BInt4 size )
{
  BData p;
#ifdef MSDOS
    return halloc ( (size + size % 2)/2, 2 );
#else
    p = (BData) malloc( size );
    return p;
#endif
}

BVoid GEN_free ( BData ptr )
{
#ifdef MSDOS
    hfree ( ptr );
#else
    free ( ptr );
#endif
}

/*  @@
 *
 */
/*VARARGS*/
BVoid GEN_print(
    FILE        * file,
    const BText   format,        /* I  printf() format string        */
    ...          )               /* I  printf() arguments            */
{
    va_list arguments;
    BInt4 error;
    BChar error_string[134];
/*    extern BVoid DisplayMessage(); */

    va_start ( arguments, format );
    error = Neferr(0, error_string);
   (BVoid)vfprintf ( file, format, arguments );
/*    DisplayMessage( "%s", error_string); */
    va_end( arguments );
}

BVoid GEN_mallinfo ( BVoid )
{
    return;
}


FILE * GEN_get_output_stream ( BVoid )
{
    /* pipe output through more */
#if defined WIN32
    pipefile = _popen ( Gl_pager, "w" );
#else
    pipefile = (FILE *) popen ( Gl_pager, "w" );
#endif
    return pipefile;
}

BVoid GEN_close_output_stream ( BVoid )
{
    if ( pipefile != NULL ) {
#if defined WIN32
        (BVoid)_pclose ( pipefile );
#else
    (BVoid) pclose ( pipefile );
#endif
    }
}

BInt4 GEN_string_compare (
    const BText a,
    const BText b
    )
{
    size_t i;
    size_t j;

    for ( i = strlen(a) ;  a[--i] == ' ' ; ) ;
    for ( j = strlen(b) ;  b[--j] == ' ' ; ) ;
    if ( j == i ) {
        return strncmp ( a, b, ++j );
    }
    return 1;
}


/*  @@  Declare error file unit number.
 */
BVoid GEN_declare_error_file ( FILE * tmp )
{
    AuErrFile = tmp;
}


/*  @@  Function to print errormessages
 */
/*VARARGS*/
BVoid GEN_message_to_errorfile ( BInt4 errcode, ... )
{
    va_list  ap;
    BText    sval;
    BInt4    ival;

    va_start ( ap, errcode );

    switch ( errcode ) {
    case 1:
        GEN_print ( AuErrFile, " [****] Out of memory\n\n" );
        break;

    case 2:
        ival = va_arg( ap, BInt4 );
        GEN_print ( AuErrFile, " [****] Error reading def.file (Nefis error %d)\n\n", ival );
        break;

    case 3:
        GEN_print ( AuErrFile, " [****] Error opening def.file\n\n" );
        break;

    case 4:
        GEN_print ( AuErrFile, " [****] Error opening datafile\n\n" );
        break;

    case 5:
        GEN_print ( AuErrFile, " [****] Error opening files\n\n" );
        break;

    case 6:
        sval = va_arg( ap, BText );
        GEN_print ( AuErrFile, " [****] Element %s does not exist\n\n",
                           sval );
        break;

    case 8:
        ival = va_arg( ap, BInt4 );
        GEN_print ( AuErrFile, " [****] Error reading datafile (Nefis error %d)\n\n", ival );
        break;

    case 9:
        GEN_print ( AuErrFile, " [****] Variable already exsists\n\n" );
        break;

    case 10:
        GEN_print ( AuErrFile, " [****] Group does not exsists\n\n" );
        break;

    case 12:
        GEN_print ( AuErrFile, " [****] Element indices not correct\n\n" );
        break;

    case 13:
        GEN_print ( AuErrFile, " [****] Group indices not correct\n\n" );
        break;

    case 101:
        sval = va_arg ( ap, BText );
        GEN_print ( AuErrFile, " [****] Variable %s does not exist\n\n",
                           sval );
        break;

    case 102:
        sval = va_arg ( ap, BText );
        GEN_print (AuErrFile, " [****] Unable to open file %s\n\n",
                           sval );
        break;

    case 108:
        sval = va_arg ( ap, BText );
        GEN_print ( AuErrFile, " [****] Variable %s and variable", sval );
        sval = va_arg ( ap, BText );
        GEN_print ( AuErrFile, " %s have different structures\n\n", sval );
        break;

    case 109:
        sval = va_arg ( ap, BText );
        GEN_print(AuErrFile, " [****] Variable %s not created\n\n", sval );
        break;

    case 110:
        GEN_print ( AuErrFile, " [****] Variable type incorrect for function\n\n" );
        break;

    case 111:
        GEN_print ( AuErrFile, " [****] Variables have different structure\n\n" );
        break;

    case 201:
        sval = va_arg ( ap, BText );
        GEN_print ( AuErrFile, " [****] Process %s cannot be opened",
                           sval );
        break;

    default:
        GEN_print ( AuErrFile, "unknown errorcode %d\n", errcode );
        break;
    }
    va_end ( ap );

    if ( isatty(0) == 0 ) {
        /* input not from a terminal device, so stop */
        exit ( 1 );
    }
}


/*  @@
 */
BVoid GEN_display_help ( BVoid )
{
    FILE * tfp;
    char * ident = (char *) malloc(256*sizeof(char));

    getFullVersionString_VS(ident);

    tfp = GEN_get_output_stream ();
    GEN_print (tfp, "\
\n%s\n\n\
DISP MEMO|STAT [TO filename]|groupname\n\
EXEC process [WITH variable ...] [PAR parameter ...] [RETN]\n\
EXIT|QUIT|STOP\n\
HELP|?\n\
LET variable = value\n\
LET variable = variable +|-|*|/ variable|value\n\
LET variable = MAX|MIN|AVG variable [exclude_value]\n\
LET variable = elementname [range] FROM groupname [range]\n\
RELE ALL|variable\n\
USE datafile DEF definitionfile\n\
WRITE variable ... [TO filename [blockname]]\n\
!command\n\n\0",&ident[4]);

    GEN_close_output_stream();
    free(ident);
}


BText GEN_tekst(
   BText yytext              /* aangeleverde tekst-array */
    )

{
    BText return_value = NULL;

    if ( yytext != (BText) NULL ) {
        return_value = GEN_malloc (( strlen(yytext)+1) * sizeof(BChar));
        if ( return_value != NULL ) {
            (void)strcpy ( return_value, yytext );
        }
    }
    return return_value;
}
