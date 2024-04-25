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
/*
 *  ods_gnf.c -  ODS routines to "generically" read NEFIS files
 *
 *  Copyright (C) 2002-2024 Deltares
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the following functions:
 *  - GNF_GetFileInfo():     Find the file_info structure or create a new one
 *  - GNF_FreeFileInfo():    Free all resources concerning the NEFIS file
 *  - GNF_ClearAllFiles():   Make sure all once opened files are forgotten
 *  - GNF_GetFileContents(): Read the NEFIS file and get a table of contents
 *  - GNF_GetStringBuffer(): Get a string buffer from the file
 *  - GNF_GetRealBuffer():   Get a real buffer from the file
 *  - GNF_GetDoubleBuffer(): Get a double-precision real buffer from the file
 *  - GNF_GetIntBuffer():    Get an integer buffer from the file
 *  - GNF_CheckParameters(): Get the number of parameters
 *  - GNF_CheckTimes():      Get the number of times
 *  - GNF_CheckLocations():  Get the number of locations
 *  - GNF_FindParameter():   Find the given parameter in the list
 *  - GNF_GetDim():          Get the dimensions (ODS)
 *  - GNF_GetPar():          Get the parameters (ODS)
 *  - GNF_GetLoc():          Get the locations (ODS)
 *  - GNF_GetMat():          Get the actual data (ODS)
 *  - GNF_GetGrd():          Get the grid information array (ODS)
 *  - main():                Test driver
 *
 *  TODO:
 *  - Cache some things like the times in the file - in GetMat()
 *  - Find a solution for TRAM/TRAH files where it is impossible to know
 *    if a parameter is present or not from the element's size (unlike
 *    TRIM/TRIH, though that is also imperfect
 *  - BOTM: no reference date
 *  - TRIM/TRIH: acceptable time steps?
 *
 */

/*
 *  $Author: Markus $
 *  $Date: 23/08/07 11:44 $
 *  $Source$
*/

/*
 * Include files and definitions
 * Note:
 * The file welcome.h defines the version number
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ods.h"
#include "nefis.h"
#include "opnclose.h"
#include "ods_gnf.h"
#include "ods_conv.h"
#include "ods_spcf.h"

#include "gnf_data.h"

/* (AM, ARS 6848) Some strange error occurred and I had to experiment
   with fabs() to find out what was going on
*/
#if 0
static double _fabs( double d ) {
   double _d ;

#if 0
   _d = fabs(d) ;
   /* fprintf( stderr, "fabs: %lg\n", d ) ; fflush(stderr); */
   return fabs(d) ;
#endif
   if ( d > 0.0 ) return   d  ;
   if ( d < 0.0 ) return (-d) ;
   return 0.0 ;
}
#define fabs _fabs
#endif

#define EPSTIM        1.0e-7
static int almost_equal( double d1, double d2 ) {
   if ( d1 > d2-EPSTIM && d1 < d2+EPSTIM )
   {
      return 1 ;
   }
   else
   {
      return 0 ;
   }
}

/*
 * Macros:
 * MAXNAMES   - maximum number of elements in a group
 * MAXNFFILES - maximum number of files that can be cached
 * EPSTIM     - circa 0.01 second
 */
#define MAXNAMES    100
#define MAXNFFILES 1000

#ifdef TEST
#define Clnfdf Clsdef
#define Clnfdt Clsdat
#endif

/*
 * Static variables and functions
 */

/* #define TEST2 */
#ifdef TEST2
static void *_Malloc_( size_t size, int line )
{
   void *ptr = malloc(size) ;
   fprintf( stderr, "Malloc: %p (%d) at line %d\n", ptr, size, line ) ;
   return ptr ;
}
static void _Free_( void *ptr, int line )
{
   fprintf( stderr, "Free: %p at line %line\n", ptr, line ) ;
   free( ptr ) ; ptr = NULL;
}
/* Note the order! */
#define malloc(a) _Malloc_((a),__LINE__)
#define free(a)   _Free_((a),__LINE__)
#endif

static NefisFileInfoPtr
   GNF_GetFileContents(
      TString   filename,
      TInt4     filetype,
      TInt4   * ierror   ) ;

static TVoid
   GNF_CheckParameters(
      NefisFileInfoPtr   file_info,
      TInt4            * ndim,
      TString            param_names,
      TInt4            * param_codes
   ) ;

static void
   GNF_FreeCaches(void);

static NefisFileInfoPtr  file_info_array[MAXNFFILES] ; /* Store all the file info */
static TInt4             no_file_info_used =  0      ; /* Number currently stored */
static TInt4             previous_file     = -1      ; /* Index of previous file - used for caching */
static TReal4           *gnf_real_buffer   = NULL    ; /* Cached reals */
static TReal8           *gnf_double_buffer = NULL    ; /* Cached double-precision reals */
static TInt4            *gnf_int_buffer    = NULL    ; /* Cached integers */
static TChar            *gnf_char_buffer   = NULL    ; /* Cached character strings */
static TInt4             gnf_real_idx      =  0      ; /* Index of cached real element */
static TInt4             gnf_double_idx    =  0      ; /* Index of cached double-precision real element */
static TInt4             gnf_int_idx       =  0      ; /* Index of cached integer element */
static TInt4             gnf_char_idx      =  0      ; /* Index of cached character element */
static TInt4             gnf_real_cell[15]           ; /* Cells of cached real element */
static TInt4             gnf_double_cell[15]         ; /* Cells of cached double-precision real element */
static TInt4             gnf_int_cell[15]            ; /* Cells of cached integer element */
static TInt4             gnf_char_cell[15]           ; /* Cells of cached character element */

/* -------------------------------------------------------------------
    Function: GNF_StripSpaces()
    Author:   Arjen Markus
    Purpose:  Strip trailing blanks off a string
    Context:  Used by various routines in this file
    Pseudo Code:
              Start at the end and set the characters to '\0' until
              you hit a non-blank.
------------------------------------------------------------------- */

static TVoid
   GNF_StripSpaces(
      TString             string )       /* IO String to strip                */
{
   TInt4 i      ;
   TInt4 length ;

   length = strlen( string ) ;
   for ( i = length-1 ; i >= 0 ; i -- )
   {
      if ( string[i] == ' ' )
      {
         string[i] = '\0' ;
      }
      else
      {
         break ;
      }
   }
}

/* -------------------------------------------------------------------
    Function: GNF_GetFileInfo()
    Author:   Arjen Markus
    Purpose:  Find the file_info structure or create a new one
    Context:  Used by various routines
    Pseudo Code:
              Scan the file_info_array and return a pointer to the
              correct entry. If not found (and there is room), make
              a new one.
------------------------------------------------------------------- */

static NefisFileInfoPtr
   GNF_GetFileInfo(
      TString             filename,      /* I Name of the file                */
      TInt4               filetype,      /* I Type of the file                */
      TInt4             * ierror         /* I Error code                      */
   )
{
   TInt4 i      ;
   TInt4 newidx ;

/* printf( "GNF_GetFileInfo\n" ) ; */
   /* If the file is already opened, then we have a file_info structure
   */
   newidx = no_file_info_used ;

   for ( i = 0 ; i < no_file_info_used ; i ++ )
   {
      if ( file_info_array[i] == NULL )
      {
         newidx = i ; /* Save the index to fill it in in the second part */
         continue   ; /* Skip any holes */
      }

      if ( strcmp( &file_info_array[i]->filename[0][0], &filename[0]             ) == 0 &&
           strcmp( &file_info_array[i]->filename[1][0], &filename[0+NFFILELEN]   ) == 0 &&
           strcmp( &file_info_array[i]->filename[2][0], &filename[0+2*NFFILELEN] ) == 0 &&
           file_info_array[i]->filetype == filetype                                     )
      {
         if ( previous_file != i ) {
             GNF_FreeCaches();
             previous_file = i;
         }
         return file_info_array[i] ;
      }
   }

   /* Apparently this is the first time - otherwise we would not be here
   */
   if ( newidx == no_file_info_used )
   {
      no_file_info_used ++ ;
   }
   if ( no_file_info_used >= MAXNFFILES )
   {
      *ierror = IENOHA ;
      return NULL ;
   }

   file_info_array[newidx] = GNF_GetFileContents( filename, filetype, ierror ) ;

   if ( file_info_array[newidx] == NULL )
   {
      no_file_info_used -- ;
      *ierror = IENOFI     ;
   }

   return file_info_array[newidx] ;
}

/* -------------------------------------------------------------------
    Function: GNF_FreeFileInfo()
    Author:   Arjen Markus
    Purpose:  Free all resources concerning the NEFIS file
    Context:  Used by various routines
    Pseudo Code:
              Close the NEFIS file, if open, free the info structure
------------------------------------------------------------------- */

static void
   GNF_FreeFileInfo(
      NefisFileInfoPtr    file_info      /* I Structure with information      */
   )
{
   TInt4 ierror ;

   /* Close the NEFIS file via the low-level routines */
   CLOSAL( &file_info->filename[0][0], &ierror ) ;

   if ( file_info != NULL )
   {
      if ( file_info->items != NULL )
      {
         free( file_info->items ) ;
      }
      free( file_info ) ;
   }
}

/* -------------------------------------------------------------------
    Function: GNF_FreeCaches()
    Author:   Arjen Markus
    Purpose:  Free the caches - if any
    Context:  Used by GNF_GetFileInfo()
    Pseudo Code:
              Check for any caches, free the arrays
------------------------------------------------------------------- */

static void
   GNF_FreeCaches(
       void
   )
{
   if ( gnf_real_buffer != NULL )
   {
      free( gnf_real_buffer );
      gnf_real_buffer = NULL;
   }
   if ( gnf_int_buffer != NULL )
   {
      free( gnf_int_buffer );
      gnf_int_buffer = NULL;
   }
   if ( gnf_char_buffer != NULL )
   {
      free( gnf_char_buffer );
      gnf_char_buffer = NULL;
   }
}

/* -------------------------------------------------------------------
    Function: GNF_CheckCache()
    Author:   Arjen Markus
    Purpose:  Check if a particular cache can be reused
    Context:  Used by GNF_Get*Buffer()
    Pseudo Code:
              Check the properties of the cache
------------------------------------------------------------------- */

static TInt4
   GNF_CheckCache(
       TInt4       type,
       TInt4       idx,
       TInt4      *cells
   )
{
   TInt4  reuse ;
   TInt4  i     ;

   reuse = 0;
   switch ( type )
   {
      case 0: /* Reals */
         if ( idx == gnf_real_idx )
         {
            reuse = 1;
            for ( i = 0; i < 15; i ++ )
            {
                if ( gnf_real_cell[i] != cells[i] )
                {
                   reuse = 0 ;
                   break ;
                }
            }
         }
         break ;

      case 1: /* Integers */
         if ( idx == gnf_int_idx )
         {
            reuse = 1;
            for ( i = 0; i < 15; i ++ )
            {
                if ( gnf_int_cell[i] != cells[i] )
                {
                   reuse = 0 ;
                   break ;
                }
            }
         }
         break ;

      case 2: /* Strings */
         if ( idx == gnf_char_idx )
         {
            reuse = 1;
         }
         break ;

      default: /* Should not occur */
         reuse = 0 ;
         break ;
   }

   return reuse ;
}


/* -------------------------------------------------------------------
    Function: GNF_ClearAllFiles()
    Author:   Arjen Markus
    Purpose:  Make sure all files are forgotten about
    Context:  Used by CLOSAL
    Pseudo Code:
              Reset no_file_info_used
------------------------------------------------------------------- */

void
   GNF_ClearAllFiles(
      void
   )
{
   no_file_info_used = 0 ;
}

/* -------------------------------------------------------------------
    Function: GNF_GetFileContents()
    Author:   Arjen Markus
    Purpose:  Read the NEFIS file and get a table of contents
    Context:  Used by various routines
    Pseudo Code:
              Use NEFIS routines to get a list of groups and elements
              within the groups and store the information. The result
              is a structure containing an array with the
              relevant information
------------------------------------------------------------------- */

static NefisFileInfoPtr
   GNF_GetFileContents(
      TString   filename,                  /* I Name of the NEFIS file (both) */
      TInt4     filetype,                  /* I Type of the NEFIS file        */
      TInt4   * ierror                     /* O Error code                    */
   )
{
   TChar               group_name[NFNAMELEN] ;
   TChar               group_def[NFNAMELEN]  ;
   TChar               cell_name[NFNAMELEN]  ;
   TChar               elem_name[MAXNAMES][NFNAMELEN]  ;
   TChar               elem_type[NFNAMELEN]  ;
   TChar               elem_qty [NFNAMELEN]  ;
   TChar               elem_unit[NFNAMELEN]  ;
   TChar               elem_desc[5*NFNAMELEN]  ;

   BInt4               group_ndim            ;
   BInt4               elem_ndim             ;
   BInt4               group_dims[NFDIM]     ;
   BInt4               group_order[NFDIM]    ;
   BInt4               elem_dims [NFDIM]     ;

   NefisFileInfoPtr    file_info ;
   NefisItemInfoPtr    item_info ;
   TInt4               i         ;
   TInt4               error1    ;
   TInt4               error2    ;
   TInt4               no_items  ;
   TInt4               max_items ;
   BInt4               no_elems  ;
   BInt4               nbytsg    ;

   TInt4               filetype2 ;

/*   printf( "GNF_getFileContents\n" ) ; */
   file_info = (NefisFileInfoPtr) malloc( sizeof(NefisFileInfoStruct) ) ;
   if ( file_info == NULL )
   {
      return NULL ;
   }
   file_info->items = NULL ;

   strcpy( &file_info->filename[0][0], &filename[0]             ) ;
   strcpy( &file_info->filename[1][0], &filename[0+NFFILELEN]   ) ;
   strcpy( &file_info->filename[2][0], &filename[0+2*NFFILELEN] ) ;
   file_info->filetype = filetype                                 ;

   filetype2 = 5001 ;
   OPNNEF( filename, &filetype2, file_info->datfds, file_info->deffds, ierror ) ;

   if ( *ierror != 0 )
   {

      GNF_FreeFileInfo( file_info ) ;
      return NULL ;
   }

   max_items = 0 ;

   /* Start scanning the groups
   */
   error1   = Inqfst( file_info->datfds, group_name, group_def ) ;
   no_items = -1 ;
   while ( error1 == 0 )
   {
      /* The properties of the group
      */
      group_ndim    = 0 ;
      group_dims[0] = 0 ;
      group_dims[1] = 0 ;
      group_dims[2] = 0 ;
      group_dims[3] = 0 ;
      group_dims[4] = 0 ;

      error2 = Inqgrp( file_info->deffds, group_def, cell_name, &group_ndim,
                       group_dims, group_order ) ;
      if ( error2 != 0 )
      {
         GNF_FreeFileInfo( file_info ) ;
         return NULL ;
      }

      if ( group_ndim == 0 )
      {
          continue ;
      }
      if ( group_dims[group_ndim-1] == 0 )
      {
         error2 = Inqmxi( file_info->datfds, group_def,
                     &group_dims[group_ndim-1] ) ;
         if ( error2 != 0 )
         {
            GNF_FreeFileInfo( file_info ) ;
            return NULL ;
         }
      }

      /* Read the elements in the group
      */
      no_elems = MAXNAMES ;
      error2   = Inqcel( file_info->deffds, cell_name, &no_elems, elem_name ) ;

      for ( i = 0 ; i < no_elems ; i ++ )
      {
         no_items ++ ;
         if ( no_items >= max_items )
         {
            max_items = max_items + 10 ;
            file_info->items = (NefisItemInfoPtr)
               realloc( file_info->items,
                  (sizeof(NefisItemInfoStruct) * (size_t) max_items) ) ;
            if ( file_info->items == NULL )
            {
               GNF_FreeFileInfo( file_info ) ;
               return NULL ;
            }
         }

         /* Store the information
         */
         item_info = &file_info->items[no_items] ;

         strcpy( item_info->group_name, group_name   ) ;
         elem_name[i][NFNAMELEN-1] = '\0' ; /* Force the element name to be a proper C string */
         strcpy( item_info->elem_name,  elem_name[i] ) ;
         GNF_StripSpaces( item_info->group_name ) ;
         GNF_StripSpaces( item_info->elem_name  ) ;
         elem_ndim    = 5 ;
         elem_dims[0] = 0 ;
         elem_dims[1] = 0 ;
         elem_dims[2] = 0 ;
         elem_dims[3] = 0 ;
         elem_dims[4] = 0 ;

         error2 = Inqelm( file_info->deffds, item_info->elem_name, elem_type,
                          &nbytsg, elem_qty, elem_unit, elem_desc,
                          &elem_ndim, elem_dims ) ;

         if ( error2 != 0 )
         {
            GNF_FreeFileInfo( file_info ) ;
            return NULL ;
         }

         item_info->elem_type       = 1             ; /* TODO */  // elem_type is een char !
         item_info->number_bytes    = nbytsg        ;

         item_info->group_ndim      = group_ndim    ;
         item_info->group_dims[0]   = group_dims[0] ;
         item_info->group_dims[1]   = group_dims[1] ;
         item_info->group_dims[2]   = group_dims[2] ;
         item_info->group_dims[3]   = group_dims[3] ;
         item_info->group_dims[4]   = group_dims[4] ;

         item_info->elem_ndim       = elem_ndim     ;
         item_info->elem_dims[0]    = elem_dims[0]  ;
         item_info->elem_dims[1]    = elem_dims[1]  ;
         item_info->elem_dims[2]    = elem_dims[2]  ;
         item_info->elem_dims[3]    = elem_dims[3]  ;
         item_info->elem_dims[4]    = elem_dims[4]  ;
      }

      /* The next group ...
      */
      error1 = Inqnxt( file_info->datfds, group_name, group_def ) ;
   }

   file_info->no_items = no_items + 1 ;

   GNF_IdentifyDefinition( file_info, filetype ) ;

   return file_info ;
}

/* -------------------------------------------------------------------
    Function: GNF_GetStringBuffer()
    Author:   Arjen Markus
    Purpose:  Get a string buffer from the file
    Context:  Used by various routines
    Pseudo Code:
              Allocate space for the buffer. Get it via the NEFIS
              routines.
    Note:
              Unlike the real and int versions, this function
              assumes that the data group is one-dimensional
              (or just one cell) and ditto th element
------------------------------------------------------------------- */

TVoid
   GNF_GetStringBuffer(
      NefisFileInfoPtr      file_info,        /* I   Contents of the file     */
      TInt4                 idx,              /* I   Index of the element     */
      TString             * buffer,           /* O   Allocated buffer         */
      TInt4               * string_length,    /* O   Length individual string */
      TInt4               * number_strings )  /* O   Number of strings        */
{
   NefisItemInfoPtr   item         ;
   BInt4              ierror       ;
   BInt4              buflen       ;
   TInt4              i            ;
   BInt4              uindex[5][3] ;
   BInt4              usrord[5]    ;

/*   printf( "GNF_GetStringBuffer\n" ) ; */
   item = &file_info->items[idx] ;

   /* Allocate the buffer
   */
   *number_strings = item->elem_dims[0] * item->group_dims[0] ; /* Assumption: one-dimensional! */
   *string_length  = item->number_bytes ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      uindex[i][0] = 0 ;
      uindex[i][1] = 0 ;
      uindex[i][2] = 0 ;
   }

   buflen          = (*number_strings) * (*string_length) ;
   *buffer = (TString)
      malloc( sizeof(TChar) * buflen ) ;

   /* Assumption: the data group is a one-dimensional array */
   uindex[0][0] = 1 ;
   uindex[0][1] = item->group_dims[0] ;
   uindex[0][2] = 1 ;
   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;

   /* Was it cached? */
   if ( gnf_char_buffer != NULL )
   {
       if ( GNF_CheckCache( 2, idx, NULL ) )
       {
           memcpy( *buffer, gnf_char_buffer, buflen ) ;
           return ;
       }
   }

   free( gnf_char_buffer ) ;
   gnf_char_buffer = (TString) malloc( sizeof(TChar) * buflen ) ;

   gnf_char_idx = idx ;

   ierror = Getelt( file_info->datfds,
                    item->group_name, item->elem_name,
                    uindex, usrord, &buflen, *buffer ) ;

   memcpy( gnf_char_buffer, *buffer, buflen ) ;

   /* Make sure the strings are decent enough C strings
   */
   /* ARS 6983: this causes truncation of the names in some cases
      Hence commented out
   */
#if 0
   for ( i = 1 ; i <= (*number_strings) ; i ++ )
   {
      (*buffer)[i*(*string_length)-1] = '\0' ; /* TODO: strip off trailing spaces */
   }
#endif
}

/* -------------------------------------------------------------------
    Function: GNF_GetRealBuffer()
    Author:   Arjen Markus
    Purpose:  Get a real buffer from the file
    Context:  Used by various routines
    Pseudo Code:
              Allocate space for the buffer. Get it via the NEFIS
              routines.
    Note:
              The cell index is to be filled according to the
              conventions of NEFIS, with the sole exception of
              the counting starting at 0, in the C like way.
              More concretely:
              - cell_index[0][0] = the first index (of the first
                                   dimension)
              - cell_index[0][1] = the last index (of the first
                                   dimension)
              - cell_index[0][2] = the step size (for the first
                                   dimension)
              - ... (if needed)  = similarly for the other dimensions
              The buffer is allocated and must be freed when no
              longer needed. This guarantees that enough memory
              is available. (It may seem a slow algorithm, when you
              retrieve a small buffer many times, but I/O is even slower
              of course in most cases.)
------------------------------------------------------------------- */

TVoid
   GNF_GetRealBuffer(
      NefisFileInfoPtr      file_info,        /* I   Contents of the file     */
      TInt4                 idx,              /* I   Index of the element     */
      TInt4               * cell_index,       /* I   Indices of the cell to
                                                     retrieve                 */
      TReal4             ** buffer,           /* O   Allocated buffer         */
      TInt4               * elem_size,        /* O   Size of individual
                                                     elements (number of reals) */
      TInt4               * number_cells   )  /* O   Number of cells retrieved */
{
   NefisItemInfoPtr   item         ;
   BInt4              ierror       ;
   BInt4              buflen       ;
   TInt4              i            ;
   TInt4              nd           ;
   BInt4              uindex[5][3] ;
   BInt4              usrord[5]    ;

/*   printf( "GNF_GetRealBuffer\n" ) ; */
   item = &file_info->items[idx] ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      uindex[i][0] = 0 ;
      uindex[i][1] = 0 ;
      uindex[i][2] = 0 ;
   }

   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;

   /* Allocate the buffer
   */
   (*elem_size) = 1 ;
   for ( i = 0 ; i < item->elem_ndim ; i ++ )
   {
      (*elem_size) *= item->elem_dims[i] ;
   }

   (*number_cells) = 1 ;
   for ( i = 0 ; i < item->group_ndim ; i ++ )
   {
      uindex[i][0]    = cell_index[3*i+0] + 1 ;
      if ( cell_index[3*i+1] != ALL_CELLS )
      {
         uindex[i][1]    = cell_index[3*i+1] + 1 ;
      }
      else
      {
         uindex[i][1]    = item->group_dims[i] ;
      }
      uindex[i][2]    = cell_index[3*i+2]     ;

      nd              =  1 + ( uindex[i][1] - uindex[i][0] )
                                / uindex[i][2] ;
      (*number_cells) *= nd ;
   }

   buflen  = sizeof(TReal4) * (*number_cells) * (*elem_size) ;
   *buffer = (TReal4 *) malloc( buflen ) ;

   /* Was it cached? */
   if ( gnf_real_buffer != NULL )
   {
       if ( GNF_CheckCache( 0, idx, cell_index ) )
       {
           memcpy( *buffer, gnf_real_buffer, buflen ) ;
           return ;
       }
   }


   ierror = Getelt( file_info->datfds,
                    item->group_name, item->elem_name,
                    uindex[0], usrord, &buflen, (*buffer) ) ;

   if ( ierror == 0 )
   {
      free( gnf_real_buffer ) ;
      gnf_real_buffer = (TReal4 *) malloc( buflen ) ;

   gnf_real_idx = idx ;
   memcpy( gnf_real_cell, cell_index, sizeof(gnf_real_cell) );

      memcpy( gnf_real_buffer, *buffer, buflen ) ;
   }
   else
   {
      free( *buffer ) ;
      *buffer = NULL ;
   }

/* printf( "error (%s/%s): %d\n",
                    item->group_name, item->elem_name, ierror ) ; */
}

/* -------------------------------------------------------------------
    Function: GNF_GetDoubleBuffer()
    Author:   Arjen Markus
    Purpose:  Get a double-precision real buffer from the file
    Context:  Used by various routines
    Pseudo Code:
              Allocate space for the buffer. Get it via the NEFIS
              routines.
    Note:
              The cell index is to be filled according to the
              conventions of NEFIS, with the sole exception of
              the counting starting at 0, in the C like way.
              More concretely:
              - cell_index[0][0] = the first index (of the first
                                   dimension)
              - cell_index[0][1] = the last index (of the first
                                   dimension)
              - cell_index[0][2] = the step size (for the first
                                   dimension)
              - ... (if needed)  = similarly for the other dimensions
              The buffer is allocated and must be freed when no
              longer needed. This guarantees that enough memory
              is available. (It may seem a slow algorithm, when you
              retrieve a small buffer many times, but I/O is even slower
              of course in most cases.)
------------------------------------------------------------------- */

TVoid
   GNF_GetDoubleBuffer(
      NefisFileInfoPtr      file_info,        /* I   Contents of the file     */
      TInt4                 idx,              /* I   Index of the element     */
      TInt4               * cell_index,       /* I   Indices of the cell to
                                                     retrieve                 */
      TReal8             ** buffer,           /* O   Allocated buffer         */
      TInt4               * elem_size,        /* O   Size of individual
                                                     elements (number of reals) */
      TInt4               * number_cells   )  /* O   Number of cells retrieved */
{
   NefisItemInfoPtr   item         ;
   BInt4              ierror       ;
   TInt4              buflen       ;
   TInt4              i            ;
   TInt4              nd           ;
   TInt4              uindex[5][3] ;
   TInt4              usrord[5]    ;

/*   printf( "GNF_GetRealBuffer\n" ) ; */
   item = &file_info->items[idx] ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      uindex[i][0] = 0 ;
      uindex[i][1] = 0 ;
      uindex[i][2] = 0 ;
   }

   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;

   /* Allocate the buffer
   */
   (*elem_size) = 1 ;
   for ( i = 0 ; i < item->elem_ndim ; i ++ )
   {
      (*elem_size) *= item->elem_dims[i] ;
   }

   (*number_cells) = 1 ;
   for ( i = 0 ; i < item->group_ndim ; i ++ )
   {
      uindex[i][0]    = cell_index[3*i+0] + 1 ;
      if ( cell_index[3*i+1] != ALL_CELLS )
      {
         uindex[i][1]    = cell_index[3*i+1] + 1 ;
      }
      else
      {
         uindex[i][1]    = item->group_dims[i] ;
      }
      uindex[i][2]    = cell_index[3*i+2]     ;

      nd              =  1 + ( uindex[i][1] - uindex[i][0] )
                                / uindex[i][2] ;
      (*number_cells) *= nd ;
   }

   buflen  = sizeof(TReal8) * (*number_cells) * (*elem_size) ;
   *buffer = (TReal8 *) malloc( buflen ) ;

   /* Was it cached? */
   if ( gnf_double_buffer != NULL )
   {
       if ( GNF_CheckCache( 0, idx, cell_index ) )
       {
           memcpy( *buffer, gnf_double_buffer, buflen ) ;
           return ;
       }
   }

   free( gnf_double_buffer ) ;
   gnf_double_buffer = (TReal8 *) malloc( buflen ) ;

   gnf_double_idx = idx ;
   memcpy( gnf_double_cell, cell_index, sizeof(gnf_double_cell) );

   ierror = Getelt( file_info->datfds,
                    item->group_name, item->elem_name,
                    uindex, usrord, &buflen, *buffer ) ;

   memcpy( gnf_double_buffer, *buffer, buflen ) ;

/* printf( "error (%s/%s): %d\n",
                    item->group_name, item->elem_name, ierror ) ; */
}

/* -------------------------------------------------------------------
    Function: GNF_GetIntBuffer()
    Author:   Arjen Markus
    Purpose:  Get an integer buffer from the file
    Context:  Used by various routines
    Pseudo Code:
              Allocate space for the buffer. Get it via the NEFIS
              routines. See GNF_GetRealBuffer() as it is a copy of
              that routine! Reason: better type checking.
------------------------------------------------------------------- */

TVoid
   GNF_GetIntBuffer(
      NefisFileInfoPtr      file_info,        /* I   Contents of the file     */
      TInt4                 idx,              /* I   Index of the element     */
      TInt4               * cell_index,       /* I   Indices of the cell to
                                                     retrieve                 */
      TInt4              ** buffer,           /* O   Allocated buffer         */
      TInt4               * elem_size,        /* O   Size of individual
                                                     elements (number of reals) */
      TInt4               * number_cells   )  /* O   Number of cells retrieved */
{
   NefisItemInfoPtr   item         ;
   BInt4              ierror       ;
   BInt4              buflen       ;
   TInt4              i            ;
   TInt4              nd           ;
   BInt4              uindex[5][3] ;
   BInt4              usrord[5]    ;

/*   printf( "GNF_GetIntBuffer\n" ) ; */
   item = &file_info->items[idx] ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      uindex[i][0] = 0 ;
      uindex[i][1] = 0 ;
      uindex[i][2] = 0 ;
   }

   /* Allocate the buffer
   */
   (*elem_size) = 1 ;
   for ( i = 0 ; i < item->elem_ndim ; i ++ )
   {
      (*elem_size) *= item->elem_dims[i] ;
   }

   (*number_cells) = 1 ;
   for ( i = 0 ; i < item->group_ndim ; i ++ )
   {
      uindex[i][0]    = cell_index[3*i+0] + 1 ;
      if ( cell_index[3*i+1] != ALL_CELLS )
      {
         uindex[i][1]    = cell_index[3*i+1] + 1 ;
      }
      else
      {
         uindex[i][1]    = item->group_dims[i] > 0? item->group_dims[i] : 1 ;
      }
      uindex[i][2]    = cell_index[3*i+2]     ;

      nd              =  1 + ( uindex[i][1] - uindex[i][0] )
                                / uindex[i][2] ;
      (*number_cells) *= nd ;
   }

   buflen  = sizeof(TInt4) * (*number_cells) * (*elem_size) ;
   *buffer = (TInt4 *) malloc( buflen ) ;

   usrord[0]    = 1 ;
   usrord[1]    = 2 ;
   usrord[2]    = 3 ;
   usrord[3]    = 4 ;
   usrord[4]    = 5 ;

   /* Was it cached? */
   if ( gnf_int_buffer != NULL )
   {
       if ( GNF_CheckCache( 1, idx, cell_index ) )
       {
           memcpy( *buffer, gnf_int_buffer, buflen ) ;
           return ;
       }
   }

   free( gnf_int_buffer ) ;
   gnf_int_buffer = (TInt4 *) malloc( buflen ) ;

   gnf_int_idx = idx ;
   memcpy( gnf_int_cell, cell_index, sizeof(gnf_int_cell) );

   ierror = Getelt( file_info->datfds,
                    item->group_name, item->elem_name,
                    uindex, usrord, &buflen, *buffer ) ;

   memcpy( gnf_int_buffer, *buffer, buflen ) ;
}

/* -------------------------------------------------------------------
    Function: GNF_CheckParameters()
    Author:   Arjen Markus
    Purpose:  Get the number of parameters
    Context:  Used by GNF_GetDim() and GNF_GetPar()
    Pseudo Code:
              Go through the list of parameters available according
              to the file definition and check which ones are in the
              data file.
------------------------------------------------------------------- */

static TVoid
   GNF_CheckParameters(
      NefisFileInfoPtr   file_info,     /* I   Contents of the file          */
      TInt4            * ndim,          /* O   Array of dimensions           */
      TString            param_names,   /* O   Array of names                */
      TInt4            * param_codes    /* O   Array of codes                */
   )
{
   ParameterInfoPtr  defined_params ;
   TInt4             i              ;
   TInt4             idx            ;
   TInt4             zdim           ;
   TInt4             idxdata        ;
   TInt4             idxnam         ;
   TInt4             idxitm         ;
   TInt4             idxlst         ;
   TInt4             maxlst         ;
   TInt4             nopars         ;
   TInt4             nodims         ;
   TInt4             nrlst          ;
   TInt4             string_length  ;
   TInt4             length         ;
   TInt4             pos            ;
   TInt4             number_strings ;
   TInt4             non_scalar     ;
   TString           buffer         ;
   TString           pstr           ;
   TChar             dummy_char[2]  ; /* Compiler does not like a potentially uninitialised buffer */

   buffer = dummy_char; /* Make the compiler stop moaning - it is a false positive */

   defined_params = file_info->defined_params ;

   maxlst = ndim[1] ;
   nrlst  = 0       ;
   i      = 0       ;
   idx    = 0       ;
   while( defined_params[i].type != END_OF_ARRAY )
   {
      /* Decide whether elements with just one number are okay or not
         (per cell)
      */
      if ( defined_params[i].location_info->type == LOC_NAMES ||
           defined_params[i].location_info->type == BAL_NAMES )
      {
         non_scalar = 0 ; /* Was: 0 */
      }
      else
      {
         non_scalar = 1 ;
      }

      if ( strcmp( defined_params[i].public_name, "@" ) != 0 )
      {
         if ( GNF_FindParameter( defined_params[i].group_name,
                  defined_params[i].elem_name, non_scalar, file_info->no_items,
                  file_info->items, &idxitm ) == 1 )
         {
            /* Check the third dimension - in case of depth-averaged
               parameters
            */
            if ( defined_params[i].type & DPT_AVERAGE )
            {
               zdim = 2 ;
               if ( defined_params[i].location_info->type == LOC_NAMES ||
                    defined_params[i].location_info->type == BAL_NAMES )
               {
                  zdim = 1 ;
               }
               if ( file_info->items[idxitm].elem_dims[zdim] < 2 )
               {
                  i ++     ;
                  continue ;
               }
            }

            /* We must always count the number of matches and
               sometimes save the public names (maxlst gives the
               space available for this)
            */
            if ( defined_params[i].index_name == -1 )
            {
               if ( file_info->filetype == ODS_TRISULA_HIS_NEFIS
                    && ODS_HeuristicCheckAccept( file_info, &defined_params[i], -1 ) == 0 )
               {
                  i ++ ;
                  continue ;
               }

               if ( idx < maxlst )
               {
                  strcpy( &param_names[21*idx], defined_params[i].public_name ) ;
                  param_codes[idx] = i+1 ;
                  idx ++ ;
               }
               nrlst ++ ; /* Increase the counter in all cases */
            }
            else
            {
               /* Complications due to convention in Delft3D-FLOW files
                  Note:
                  The call to GNF_FindParameters() is assumed to succeed
                  in all cases -- TODO
               */
               nodims = file_info->items[idxitm].elem_ndim ;
               nopars = 0                                  ;
               if ( nodims > 0 )
               {
                  nopars = file_info->items[idxitm].elem_dims[nodims-1] ;
               }

               if ( defined_params[i].names_elem != NULL )
               {
                  (void) GNF_FindParameter( defined_params[i].names_group,
                            defined_params[i].names_elem, 0,
                            file_info->no_items,
                            file_info->items, &idxlst ) ;

                  if ( file_info->filetype == ODS_TRISULA_HIS_NEFIS
                       && ODS_HeuristicCheckAccept( file_info, &defined_params[i],
                              defined_params[i].index_name ) == 0 )
                  {
                     i ++ ;
                     continue ;
                  }

                  GNF_GetStringBuffer( file_info, idxlst,
                     &buffer, &string_length, &number_strings ) ;
               }

               if ( defined_params[i].index_name > 0 )
               {
                  idxnam  = defined_params[i].index_name-1 ;
                  idxdata = idxnam                         ;
               }
               else
               {
                  idxnam  = number_strings-(LAST-defined_params[i].index_name) ;
                  idxdata = LAST-defined_params[i].index_name-1 ;
               }

               /* Only if there is a name/entry - identified by the
                  last dimension!
               */
               if ( idxdata < nopars )
               {
                  if ( idx < maxlst )
                  {
                     if ( defined_params[i].names_elem != NULL )
                     {
                        pstr = strchr( defined_params[i].public_name, '*' ) ;
                        if ( pstr == NULL )
                        {
                           pstr = defined_params[i].public_name ;
                        }
                        pos    = (int) (pstr-defined_params[i].public_name) ;
                        length = 20 - pos ;
                        strcpy(  &param_names[21*idx],
                                 defined_params[i].public_name ) ;
                        strncpy( &param_names[21*idx+pos],
                                 &buffer[idxnam*string_length], length ) ;
                     }
                     else
                     {
                        strcpy( &param_names[21*idx],
                                defined_params[i].public_name ) ;
                     }
                     param_names[21*idx+20] = '\0' ;
                     param_codes[idx]       = i+1  ;
                     idx   ++ ;
                  }
                  nrlst ++ ; /* Increase the counter: the constituent is present */
               }
               if ( defined_params[i].names_elem != NULL )
               {
                  free( buffer ) ;
               }
            }
         }
      }
      i ++ ;
   }

   ndim[1] = nrlst ; /* Report the number of parameters */

}

/* -------------------------------------------------------------------
    Function: GNF_CheckTimes()
    Author:   Arjen Markus
    Purpose:  Get the number of times
    Context:  Used by GNF_GetDim() and GNF_GetPar()
    Pseudo Code:
              Identify the size of the time-information element
              (e.g. number of cells), return the number or if needed,
              also return the times (in julian date)
------------------------------------------------------------------- */

static TVoid
   GNF_CheckTimes(
      NefisFileInfoPtr   file_info,     /* I   Contents of the file          */
      TInt4              parcod,        /* I   Parameter code                */
      TInt4            * ndim,          /* O   Array of dimensions           */
      TReal8           * times          /* O   Array of times                */
   )
{
   DateTimeInfoPtr   datetime_info    ;
   TString           buffer           ;
   TInt4             i                ;
   TInt4             idx              ;
   TInt4             idxtim           ;
   TInt4             cell_index[5][3] ;
   TInt4             nrlst            ;
   TInt4             maxlst           ;
   TInt4             elem_size        ;
   TInt4             string_length    ;
   TInt4             number_strings   ;

   static TInt4              number_cells         ;
   static TInt4            * int_times     = NULL ; /* Buffer for times expressed as integers */
   static TReal8             refdate              ;
   static TReal8             tscale               ;
   static NefisFileInfoPtr   previous_file = NULL ;
   static DateTimeInfoPtr    previous_time = NULL ;

/*   printf( "GNF_CheckTimes\n" ) ; */
/* Take care of "general" requests */
   if ( parcod == -1 )
   {
      parcod = 1 ;
   }

   datetime_info  = file_info->defined_params[parcod-1].datetime_info ;

   /* If we have no information on the time definition */
   if ( datetime_info == NULL )
   {
      ndim[0] =  0 ;
      ndim[1] = -1 ;
      return       ;
   }

   maxlst = ndim[1] ;
   nrlst  = 0       ;
   i      = 0       ;
   idx    = 0       ;

   /* Get the relevant data directly from the file.
      We have to distinguish the various methods though
   */
   switch ( datetime_info->type )
   {
   case CONSTANT : /* The parameter is independent of time */
      ndim[0] =  1 ;
      ndim[1] =  0 ;
      break ;

   case ITDATE : /* An array of integers that need to be converted */
   case IT0102 :
   case ITNONE :

      /* Step 1: Determine the size and get the raw data
      */
      if ( previous_file != file_info || previous_time != datetime_info || int_times == NULL )
      {
         previous_file = file_info ;
         previous_time = datetime_info ;
         if ( int_times != NULL )
         {
             free( int_times ) ;
             int_times = NULL ;
         }

         (void) GNF_FindParameter(
                   datetime_info->group_name, datetime_info->elem_name, 0,
                   file_info->no_items, file_info->items, &idxtim ) ;
         cell_index[0][0] = 0         ;
         cell_index[0][1] = ALL_CELLS ;
         cell_index[0][2] = 1         ;
         GNF_GetIntBuffer( file_info, idxtim, &cell_index[0][0], &int_times, &elem_size,
            &number_cells )           ;

#if 0
         Original code

         if ( maxlst != 0 )
         {
#if 0
            TODO
            Controleer de tijdstap en dergelijke!!
            default_scale?
#endif
            /* Step 2: determine the reference time
            */
            ODS_GetReferenceTime( file_info, datetime_info, &refdate, &tscale ) ;
         }
#endif

         /* We need this information later on, so get it now and save it */
         ODS_GetReferenceTime( file_info, datetime_info, &refdate, &tscale ) ;
      }

      nrlst = number_cells ;
      for ( i = 0 ; i < maxlst ; i ++ )
      {
         times[i] = refdate + tscale * int_times[i] ;
      }

      /* free( int_times ) ; Cached */

      break ;

   case IDTISO : /* An array of strings in ISO format */

      /* Step 1: Determine the size and get the raw data
      */
      (void) GNF_FindParameter(
                datetime_info->group_name, datetime_info->elem_name, 0,
                file_info->no_items, file_info->items, &idxtim ) ;
      GNF_GetStringBuffer( file_info, idxtim, &buffer, &string_length,
         &number_strings )         ;
      nrlst = number_strings       ;

      if ( maxlst != 0 )
      {
         for ( i = 0 ; i < maxlst ; i ++ )
         {
            ODS_GetISODateTime( &buffer[i*string_length], &times[i] ) ;
         }
      }
      free( buffer ) ;

      break ;

   default : /* Nothing we can do. Just say so! */
      ndim[0] =  0 ;
      ndim[1] = -1 ;
      break ;
   }

   ndim[1] = nrlst ; /* Report the number of parameters */
}

/* -------------------------------------------------------------------
    Function: GNF_CheckLocations()
    Author:   Arjen Markus
    Purpose:  Get the number of locations
    Context:  Used by GNF_GetDim() and GNF_GetLoc()
    Pseudo Code:
              Examine the element with information on the locations
              or the grid (e.g. number of cells), return the number(s)
              or if needed, also return the location names
------------------------------------------------------------------- */

static TVoid
   GNF_CheckLocations(
      NefisFileInfoPtr   file_info,      /* I   Contents of the file          */
      TInt4              parcod,         /* I   Parameter code                */
      TInt4            * ndim,           /* O   Array of dimensions           */
      TString            location_names, /* O   Array of location names       */
      TInt4            * location_ids    /* O   Array of location ids         */
   )
{
   ParameterInfoPtr  param_info       ;
   LocationInfoPtr   location_info    ;
   TInt4             cell_index[5][3] ;
   TInt4             i                ;
   TInt4             general          ;
   TInt4             idx              ;
   TInt4             idxitm           ;
   TInt4             idxloc           ;
   TInt4             idxloc1          ;
   TInt4             idxloc2          ;
   TInt4             idxpar           ;
   TInt4           * index1           ;
   TInt4           * index2           ;
   TInt4             maxlst           ;
   TInt4             nrlst            ;
   TInt4             elem_size        ;
   TInt4             number_indices   ;
   TInt4             number_strings   ;
   TInt4             string_length    ;
   TInt4             idxzcrd          ;
   TInt4             idxsigma         ;
   TString           buffer           ;

/*   printf( "GNF_CheckLocations\n" ) ; */
/* Take care of "general" requests */
   general = 0 ;
   if ( parcod == -1 )
   {
      general = 1 ;
      parcod  = 1 ;
   }

   location_info  = file_info->defined_params[parcod-1].location_info ;

   /* If we have no information on the location definition */
   if ( location_info == NULL )
   {
      ndim[0] =  0 ;
      ndim[1] = -1 ;
      return       ;
   }

   maxlst = ndim[1] ;
   nrlst  = 0       ;
   i      = 0       ;
   idx    = 0       ;

   /* Get the relevant data directly from the file.
      We have to distinguish the various methods of course
   */
   switch ( location_info->type )
   {
   case LOC_NAMES : /* The locations are named */
      ndim[0] =  1 ;
      ndim[1] =  0 ;

      (void) GNF_FindParameter(
                location_info->group_name, location_info->elem_name, 0,
                file_info->no_items, file_info->items, &idxloc ) ;
      GNF_GetStringBuffer( file_info, idxloc, &buffer, &string_length,
         &number_strings ) ;

      ndim[1] = number_strings ;

      /* TRIH-files complicate matters, the number of dimensions for the
         parameter may be 2, indicating layers
         Oh, and depth-averaged quantities correct this again ...
      */

      (void) GNF_FindParameter(
                file_info->defined_params[parcod-1].group_name,
                file_info->defined_params[parcod-1].elem_name, 1,
                file_info->no_items, file_info->items, &idxpar ) ;

      /* If the parameter is filtered out: no locations
      */
      if ( idxpar == -1 )
      {
          ndim[0] = 1;
          ndim[1] = 0;
          if ( maxlst > 0 )
          {
             for ( i = 0 ; i < maxlst ; i ++ )
             {
                location_names[21*i] = '\0' ;
                location_ids[i]      = 0    ;
             }
          }
          return;
      }

      /* Bizarre numbering? Yes, but correct in the light of
         handling by Delft-GPP
      */
      if ( file_info->items[idxpar].elem_ndim  ==  2 &&
           file_info->defined_params[parcod-1].index_name == -1 &&
           ! (file_info->defined_params[parcod-1].type & DPT_AVERAGE)  )
      {
         ndim[0] =  3 ;
         ndim[2] = file_info->items[idxpar].elem_dims[1] ;
      }
      if ( file_info->items[idxpar].elem_ndim  ==  3 &&
           file_info->defined_params[parcod-1].index_name != -1 &&
           ! (file_info->defined_params[parcod-1].type & DPT_AVERAGE)  )
      {
         ndim[0] =  3 ;
         ndim[2] = file_info->items[idxpar].elem_dims[1] ;
      }

      if ( file_info->items[idxpar].elem_ndim  ==  2 &&
           file_info->defined_params[parcod-1].type == COORD_DATA_3D )
      {
         ndim[0] =  2 ;
         ndim[2] = file_info->items[idxpar].elem_dims[1] + 1 ;
      }

      /* Store the names
      */
      if ( maxlst > 0 )
      {
         for ( i = 0 ; i < maxlst ; i ++ )
         {
            strncpy( &location_names[21*i], &buffer[i*string_length], 20 ) ;
            location_names[21*i+20] = '\0' ;
            location_ids[i]         = i+1  ;
         }
      }
      free( buffer ) ;
      break ;

   case BAL_NAMES : /* The locations must be constructed */
      ODS_ConstructBalanceNames( file_info, location_info, location_names, location_ids, &maxlst );
      ndim[0] = 1;
      ndim[1] = maxlst;
      /* TODO */
      break ;

   case LOC_IDS : /* The locations are known by IDs (x/y indices) */
      ndim[0] =  1 ;
      ndim[1] =  0 ;
      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;

      (void) GNF_FindParameter(
                location_info->group_name, location_info->elem_name, 0,
                file_info->no_items, file_info->items, &idxloc1 ) ;
      (void) GNF_FindParameter(
                location_info->gridinfo_group, location_info->gridinfo_elem, 0,
                file_info->no_items, file_info->items, &idxloc2 ) ;
      GNF_GetIntBuffer( file_info, idxloc1, &cell_index[0][0],
          &index1, &elem_size, &number_indices ) ;
      GNF_GetIntBuffer( file_info, idxloc2, &cell_index[0][0],
          &index2, &elem_size, &number_indices ) ;

      ndim[1] = elem_size * number_indices ;

      if ( maxlst > 0 )
      {
         for ( i = 0 ; i < maxlst ; i ++ )
         {
            sprintf( &location_names[21*i], "(%d,%d)", index1[i], index2[i] ) ;
            location_ids[i] = i+1  ;
         }
      }
      free( index1 ) ;
      free( index2 ) ;
      break ;

   case GRID_LGRID : /* A full matrix with grid cell numbers is available */
      ndim[0] =  1 ;
      ndim[1] =  0 ;

      break ;

   case GRID_TRIVIAL : /* A full matrix but no active/inactive info */
   case GRID_KCS     : /* A full matrix with active/inactive encodings */
   case GRID_KCSINV  : /* A full matrix with active/inactive encodings
                          but stored with reversed indices */
      ndim[0] =  1 ;
      ndim[1] =  0 ;
      /* Step 1: Determine the size of the element holding the
                 parameter's data - this is equivalent to the grid size
                 and also holds the number of layers (!)
                 The maximum dimension is 3!
      */
      idx    = parcod - 1 ;
      idxitm = -1         ;

      while ( 1 )
      {
         (void) GNF_FindParameter( file_info->defined_params[idx].group_name,
                   file_info->defined_params[idx].elem_name, 1,
                   file_info->no_items,
                   file_info->items, &idxitm ) ;
         if ( idxitm == -1 )
         {
            if ( general )
            {
               idx ++ ;
               if ( file_info->defined_params[idx].type == END_OF_ARRAY )
               {
                  general = 0 ; /* Fall through */
               }
            }

            if ( !general )
            {
               ndim[0] =  0 ;
               ndim[1] = -1 ;
               return       ;
            }
         }
         else
         {
            break ; /* We have found one! */
         }
      }

      ndim[0] = file_info->items[idxitm].elem_ndim    ;
      if ( ndim[0] > 3 ) ndim[0] = 3 ;

      ndim[1] = file_info->items[idxitm].elem_dims[0] ;
      ndim[2] = file_info->items[idxitm].elem_dims[1] ;
      ndim[3] = file_info->items[idxitm].elem_dims[2] ;

      if ( location_info->type == GRID_KCSINV )
      {
         ndim[1] = file_info->items[idxitm].elem_dims[1] ;
         ndim[2] = file_info->items[idxitm].elem_dims[0] ;
      }

      /* Step 2: Watch out for depth-averaged quantities and for
                 the vertical coordinate
      */
      if ( (file_info->defined_params[parcod-1].type & DPT_AVERAGE) )
      {
         ndim[0] = 2 ;
         ndim[3] = 0 ;
      }
      if ( file_info->defined_params[parcod-1].type == SCALAR_DATA_FACE )
      {
         if ( ndim[3] > 0 )
         {
            ndim[3] = ndim[3] - 1 ; /* Get rid of extra layer */
         }
      }
      if ( file_info->defined_params[parcod-1].type == COORD_DATA_3D )
      {
         ndim[0] = 3 ;
         ndim[3] = 2 ;
         (void) GNF_FindParameter( location_info->zcoord_group,
                   location_info->zcoord_elem, 1,
                   file_info->no_items,
                   file_info->items, &idxzcrd ) ;
         (void) GNF_FindParameter( location_info->sigmalayer_group,
                   location_info->sigmalayer_elem, 1,
                   file_info->no_items,
                   file_info->items, &idxsigma ) ;
         if ( idxzcrd > -1 && file_info->items[idxzcrd].elem_dims[0] > 0 )
         {
            ndim[3] = file_info->items[idxzcrd].elem_dims[0] + 1 ;
         }
         if ( idxsigma > -1 && file_info->items[idxsigma].elem_dims[0] > 0 )
         {
            ndim[3] = file_info->items[idxsigma].elem_dims[0] + 1 ;
         }
      }
      break ;

   default : /* Nothing we can do. Just say so! */
      ndim[0] =  0 ;
      ndim[1] = -1 ;
      break ;
   }
}

/* -------------------------------------------------------------------
    Function: GNF_FindParameter()
    Author:   Arjen Markus
    Purpose:  Find the given parameter in the list
    Context:  Used by various routines
    Pseudo Code:
              Go through the list of parameters available in the file
              and return the information.
    Note:
              Parameters only count if they have more than 1 single
              value. This may cause problems when dealing with a
              history file with only one station, but that is a
              problem to be solved with another heuristic!
              For the time being: solved with a parameter "non_scalar"
              to indicate whether a scalar parameter is permissable.
------------------------------------------------------------------- */

TInt4 GNF_FindParameter(
   TString            group_name,    /* I   Name of the group             */
   TString            elem_name,     /* I   Name of the element           */
   TInt4              non_scalar,    /* I   Require (1) or not (0)
                                            more than 1 value  */
   TInt4              no_items,      /* I   Number of items in the file   */
   NefisItemInfoPtr   items,         /* I   Items in the file             */
   TInt4            * idx            /* O   Index of the parameter        */
   )
{
   NefisItemInfoPtr   item           ;
   TInt4              i              ;

/*   printf( "GNF_FindParameter\n" ) ; */

   for ( i = 0 ; i < no_items ; i ++ )
   {
      item = &items[i] ;

      if ( strcmp( group_name, item->group_name) == 0 &&
           strcmp( elem_name,  item->elem_name)  == 0 )
      {
         /* Always require more than one! */
         if ( !non_scalar || item->elem_ndim  > 1 || item->elem_dims[0]  > 1 )
         {
         /* Ignore difficulties with stored names first */
            *idx = i ;
            return 1 ;
         }
      }
   }

   /* Name not found */
   *idx = -1 ;
   return 0 ;
}

/* -------------------------------------------------------------------
    Function: GNF_GetDim()
    Author:   Arjen Markus
    Purpose:  Get the dimensions (ODS)
    Context:  Used by ODS getdim()
    Pseudo Code:
              Read the NEFIS file's contents and store the structure
              Depending on the type of dimension:
              - Delegate the request to the appropriate routine.
              - Return the relevant information.
------------------------------------------------------------------- */

TVoid GNF_GetDim(
   TString    filename,   /* I   Full filename, including extension       */
   TInt4    * itype,      /* I   Filetype, see types.inc for definitions  */
   TString    dim,        /* I   Dimension required.                      */
   TInt4    * pardep,     /* I   parameter dependency of dimension        */
   TInt4    * timdep,     /* I   time dependency of dimension             */
   TInt4    * locdep,     /* I   location dependency of dimension         */
   TInt4    * ndim,       /* O   dimension.                               */
   TInt4    * ierror,     /* O   Errorcode, see errors.inc for definition */
   TString    option      /* I/O option (reserved for future extensions)  */
   )
{
   NefisFileInfoPtr file_info ;

/*   printf( "GNF_GetDim\n" ) ; */
   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Delegate the work
   */
   switch( dim[0] )
   {
      case 't':  /* tim */
      case 'T':
         ndim[0] = 1 ;
         ndim[1] = 0 ;
         GNF_CheckTimes( file_info, (*pardep), ndim, NULL );
         break;

      case 'p':  /* par */
      case 'P':
         ndim[0] = 1 ;
         ndim[1] = 0 ;
         GNF_CheckParameters( file_info, ndim, NULL, NULL );
         break;

      case 'l':  /* loc */
      case 'L':
         ndim[0] = 1 ;
         ndim[1] = 0 ;
         GNF_CheckLocations( file_info, (*pardep), ndim, NULL, NULL );
         break;

      default:
         *ierror = IEOTHR;
         break;
   } /* switch dim */
}

/* -------------------------------------------------------------------
    Function: GNF_GetPar()
    Author:   Arjen Markus
    Purpose:  Get the parameters (ODS)
    Context:  Used by ODS getpar()
    Pseudo Code:
              Read the NEFIS file's contents and store the structure
              Call GNF_CheckParameters() to get the actual information
    Note:
              This implementation ignores the mask defined by
              pardef[], as this is never used in Delft-GPP.
------------------------------------------------------------------- */

TVoid
   GNF_GetPar(
      TString   filename,  /*  I   Full filename, including extension      */
      TInt4   * itype,     /*  I   Filetype, see types.inc for definitions */
      TString   pardef,    /*  I   List of parameters wanted.              */
      TInt4   * maxdef,    /*  I   Max. nr of parameters wanted.           */
      TInt4   * timdep,    /*  I   time dependency parameters              */
      TInt4   * locdep,    /*  I   location dependency parameters          */
      TInt4   * maxlst,    /*  I   Max. nr of parameters to return.        */
      TInt4   * lang,      /*  I   code for language                       */
      TString   parlst,    /*  O   List of parameters found.               */
      TString   paruni,    /*  O   List of units for parameters found.     */
      TInt4   * partyp,    /*  O   List of parameter types.                */
      TInt4   * parcod,    /*  O   List of parameter indices.              */
      TInt4   * nrlst,     /*  O   Nr of parameters returned.              */
      TInt4   * ierror,    /*  O   Errorcode, see errors.inc for definitio */
      TString   option     /* I/O  option (reserved for future extensions) */
   )
{
   NefisFileInfoPtr file_info      ;
   ParameterInfoPtr defined_params ;
   TInt4            ndim[5]        ;
   TInt4            i              ;
   TInt4            idx            ;

/*   printf( "GNF_GetPar\n" ) ; */
   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Delegate the work
   */
   ndim[0] = 1       ;
   ndim[1] = *maxlst ;
   GNF_CheckParameters( file_info, ndim, parlst, parcod );

   defined_params = file_info->defined_params ;

   *nrlst = ndim[1] ;
   for ( i = 0 ; i < (*nrlst) ; i ++ )
   {
      strcpy( &paruni[0+21*i], "" ) ; /* TODO? */

      idx = parcod[i]-1 ;
      if ( defined_params[idx].location_info->type != LOC_NAMES &&
           defined_params[idx].location_info->type != BAL_NAMES &&
           defined_params[idx].location_info->type != LOC_IDS      )
      {
         partyp[i] = IPLMNK ;
         if ( defined_params[idx].datetime_info->type != CONSTANT  )
         {
            partyp[i] += IPTDEP  ;
         }
      }
      else
      {
         partyp[i] = IPLDEP ;
      }
   }
}

/* -------------------------------------------------------------------
    Function: GNF_GetTme()
    Author:   Arjen Markus
    Purpose:  Get the times (ODS)
    Context:  Used by ODS gettme()
    Pseudo Code:
              Read the NEFIS file's contents and store the structure
              Call GNF_CheckTimes() to get the actual information
------------------------------------------------------------------- */

TVoid
   GNF_GetTme(
      TString   filename,  /*  I   Full filename, including extension      */
      TInt4   * itype,     /*  I   Filetype, see types.inc for definitions */
      TReal8  * timdef,    /*  I   List of time masks                      */
      TInt4   * maxdef,    /*  I   Number of time masks                    */
      TInt4   * pardep,    /*  I   parameter dependency times              */
      TInt4   * locdep,    /*  I   location dependency times               */
      TInt4   * maxlst,    /*  I   Max. nr of times to return.             */
      TReal8  * timlst,    /*  O   List of times found.                    */
      TInt4   * timtyp,    /*  O   List of time types                      */
      TInt4   * nrlst,     /*  O   Nr of parameters returned.              */
      TInt4   * ierror,    /*  O   Errorcode, see errors.inc for definitio */
      TString   option     /* I/O  option (reserved for future extensions) */
   )
{
   NefisFileInfoPtr file_info ;
   TInt4            ndim[5]   ;
   TInt4            i         ;

/*   printf( "GNF_GetTme\n" ) ; */
   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Delegate the work
   */
   ndim[0] = 1       ;
   ndim[1] = *maxlst ;
   GNF_CheckTimes( file_info, *pardep, ndim, timlst );

   for ( i = 0 ; i < ndim[1] ; i ++ )
   {
      timtyp[i] = 0 ; /* Actually: never used! */
   }
   *nrlst  = ndim[1] ;
   return ;
}

/* -------------------------------------------------------------------
    Function: GNF_GetLoc()
    Author:   Arjen Markus
    Purpose:  Get the locations (ODS)
    Context:  Used by ODS getloc()
    Pseudo Code:
              Read the NEFIS file's contents and store the structure
              Call GNF_CheckLocations() to get the actual information
------------------------------------------------------------------- */

TVoid
   GNF_GetLoc(
      TString   filename,  /*  I   Full filename, including extension      */
      TInt4   * itype,     /*  I   Filetype, see types.inc for definitions */
      TString   locdef,    /*  I   List of location masks                  */
      TInt4   * maxdef,    /*  I   Number of location masks                */
      TInt4   * pardep,    /*  I   parameter dependency locations          */
      TInt4   * timdep,    /*  I   time dependency locations               */
      TInt4   * maxlst,    /*  I   Max. nr of locations to return          */
      TString   loclst,    /*  O   List of locations found                 */
      TInt4   * loctyp,    /*  O   List of location types                  */
      TInt4   * locid,     /*  O   List of location IDs                    */
      TInt4   * nrlst,     /*  O   Nr of locations returned.               */
      TInt4   * ierror,    /*  O   Errorcode, see errors.inc for definitio */
      TString   option     /* I/O  option (reserved for future extensions) */
   )
{
   NefisFileInfoPtr file_info ;
   TInt4            ndim[5]   ;
   TInt4            i         ;

/*   printf( "GNF_GetLoc\n" ) ; */
   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Delegate the work
   */
   ndim[0] = 1       ;
   ndim[1] = *maxlst ;
   GNF_CheckLocations( file_info, *pardep, ndim, loclst, locid );

   for ( i = 0 ; i < ndim[1] ; i ++ )
   {
      loctyp[i] = 0 ; /* Actually: never used! */
   }

   *nrlst = ndim[1] ;
}

/* -------------------------------------------------------------------
    Function: GNF_GetMat()
    Author:   Arjen Markus
    Purpose:  Get the actual values (ODS)
    Context:  Used by ODS getmat()
    Pseudo Code:
              Read the NEFIS file's contents and store the structure.
              Use GNF_GetActualData() to retrieve the selected values.
              Check for all necessary calculations and carry them out.
------------------------------------------------------------------- */
TVoid
   GNF_GetMat(
      TString   filename,  /*  I   Full filename, including extension      */
      TInt4   * itype,     /*  I   Filetype, see types.inc for definitions */
      TInt4   * parcod,    /*  I   Parameter for which to retrieve the data*/
      TInt4   * loc_in,    /*  I   location matrix                         */
      TReal8  * times,     /*  I   start and stop time of the data         */
      TReal4  * misval,    /*  I   reserved value for indicating missing   */
      TInt4   * i3gl,      /*  I   Language encoding                       */
      TInt4   * maxdim,    /*  I   Size of the available buffer            */
      TReal4  * data_out,  /*  O   Data buffer to be filled                */
      TInt4   * ierror,    /*  O   Errorcode, see errors.inc for definitio */
      TString   option     /* I/O  option (reserved for future extensions) */
   )
{
   NefisFileInfoPtr    file_info   ;
   ParameterInfoPtr    param_info  ;
   ParameterInfoPtr    param_info2 ;
   ParameterInfoPtr    param_info3 ;
   ParameterInfoStruct wl_info     ;
   ParameterInfoStruct depth_info  ;
   TInt4               ndim[5]     ;
   TInt4               indloc[1]   ;
   TInt4             * indx        ;
   TInt4               cont        ;
   TInt4               i           ;
   TInt4               non_scalar  ;
   TInt4               parcod2     ;
   TInt4               idxpar      ;
   TInt4               idxpar2     ;
   TInt4               idxpar3     ;
   TInt4               loc_local[9];
   TInt4               cell_index[5][3] ;
   TInt4               igisty      ;
   TInt4               nocell      ;
   TInt4               notimes     ;
   TInt4               nodata      ;
   TInt4               nodata2     ;
   TInt4               nodata3     ;
   TInt4               result      ;
   TInt4               truegrid    ;
   TInt4               partype     ;
   TInt4               elem_size   ;
   TInt4               number_cells;
   TInt4             * loc         ; /* Either points to loc_in or loc_local */
   TReal4            * data        ; /* Either allocated or pointer to data_out */
   TReal4            * data2       ;
   TReal4            * data3       ;
   TReal4            * data4       ;
   TInt4             * idata1      ;
   TInt4             * idata2      ;
   TReal4            * zcrd        ;
   TReal4              factor      ;
   TReal8            * timlst      ;

   /* printf( "GNF_GetMat parcod - %d - time %ld\n", *parcod, (long)time(NULL) ) ; */

   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Determine which times fall within the given range

      TODO: cache the intermediate results somehow
   */
   /* printf( "GNF_CheckTimes - begin - time %ld\n", (long)time(NULL) ) ; */

   ndim[0] = 1       ;
   ndim[1] = 0       ;
   GNF_CheckTimes( file_info, *parcod, ndim, NULL );
   timlst  = (TReal8 *) malloc( ndim[1] * sizeof(TReal8) ) ;
   GNF_CheckTimes( file_info, *parcod, ndim, timlst );

   /* printf( "GNF_CheckTimes - end - time %ld\n", (long)time(NULL) ) ; */

   notimes = ndim[1] ;
   cell_index[0][0] = 0         ;
   cell_index[0][1] = ALL_CELLS ;
   cell_index[0][2] = 1         ;

   if ( notimes > 0 )
   {
      for ( i = 0 ; i < notimes ; i ++ )
      {
#if 0
         if ( fabs( times[0] - timlst[i] ) < EPSTIM )
         {
            cell_index[0][0] = i ;
         }
         if ( fabs( times[1] - timlst[i] ) < EPSTIM )
         {
            cell_index[0][1] = i ;
         }
#endif
         if ( almost_equal( times[0], timlst[i] ) )
         {
            cell_index[0][0] = i ;
         }
         if ( almost_equal( times[1], timlst[i] ) )
         {
            cell_index[0][1] = i ;
         }
      }
   }
   else
   {
      /* Parameter independent of time */
      cell_index[0][1] = 0 ;
   }

   free( timlst ) ;

   /* Find the information on the parameter
   */
   param_info = &file_info->defined_params[*parcod-1] ;

   if ( file_info->defined_params[*parcod-1].location_info->type == LOC_NAMES ||
        file_info->defined_params[*parcod-1].location_info->type == BAL_NAMES )
   {
      non_scalar = 0 ;
   }
   else
   {
      non_scalar = 1 ;
   }

   (void) GNF_FindParameter(
             param_info->group_name, param_info->elem_name, non_scalar,
             file_info->no_items, file_info->items, &idxpar ) ;

   /* Get the actual data:
      - The parameter may require further calculations. The results
        will be stored in the array "data", but we need to take care
        of intermediate storage as well.
      - The field "type" of the ParameterInfo structure indicates if
        further calculations are required.
      - Special care is needed for depth-averaged quantities
   */
   ndim[0] = loc_in[1] - loc_in[0] + 1 ;
   ndim[1] = loc_in[4] - loc_in[3] + 1 ;
   ndim[2] = loc_in[7] - loc_in[6] + 1 ;
   notimes = cell_index[0][1] - cell_index[0][0] + 1 ;

   if ( param_info->location_info->type == GRID_KCSINV )
   {
      ndim[1] = loc_in[1] - loc_in[0] + 1 ;
      ndim[0] = loc_in[4] - loc_in[3] + 1 ;
   }

   nodata  = ndim[0] * ndim[1] * ndim[2] * notimes ;

   if ( ! (param_info->type & DPT_AVERAGE) )
   {
      data = data_out ;
      loc  = loc_in   ;
   }
   else
   {
      loc  = loc_local ;
      for ( i = 0 ; i < 9 ; i ++ )
      {
         loc[i] = loc_in[i] ;
      }
      loc[6] = 0 ;
      if ( loc[3] == loc_in[4] )
      {
         loc[7] = file_info->items[idxpar].elem_dims[1] - 1 ;
      }
      else
      {
         loc[7] = file_info->items[idxpar].elem_dims[2] - 1 ;
      }
      ndim[2] = loc[7] - loc[6] + 1 ;
      nodata  = ndim[0] * ndim[1] * ndim[2] * notimes ;
      data    = (TReal4 *) malloc( sizeof(TReal4) * nodata ) ;
   }

   if ( param_info->type == COORD_DATA_3D )
   {
      param_info2 = &file_info->defined_params[*parcod] ;
      ndim[4]     = notimes ;

      ODS_GetGrdIndices( filename, itype, param_info->location_info->type, ndim,
         &indx, &truegrid ) ;
      ODS_DetermineZcoordinate( file_info, param_info, param_info2,
         cell_index, loc, ndim, indx, *misval, &data ) ;
      return ;
   }

   /* printf( "GNF_GetActualData - begin - time %ld\n", (long)time(NULL) ) ; */

   GNF_GetActualData( file_info, param_info, idxpar, cell_index,
      loc, *misval, data, &nodata ) ;

   /* printf( "GNF_GetActualData - end - time %ld\n", (long)time(NULL) ) ; */

   if ( param_info->type == SCALAR_DATA )
   {
      return ;
   }

   /* In any other case, we need some extra treatment
      Note: there is very little we can do but implement every
      treatment individually or pairwise

      Case 1:
         Vector data - the two components are not the cartesian
         components. So we need to rotate them.
         Element (*parcod-1): "U" component
         Element (*parcod):   "V" component
         Element (*parcod+1): local grid angle

      Depth averaging comes last - so filter it out
   */
   partype = ( param_info->type % DPT_AVERAGE ) ;

   if ( (partype == LOCAL_VECTOR_U)   ||
        (partype == LOCAL_VECTOR_V)   ||
        (partype == LOCAL_VECTOR_DIR) ||
        (partype == LOCAL_VECTOR_MAG)    )
   {
      data2 = (TReal4 *) malloc( sizeof(TReal4) * nodata ) ;
      data3 = (TReal4 *) malloc( sizeof(TReal4) * nodata ) ;
      data4 = (TReal4 *) malloc( sizeof(TReal4) * nodata ) ;

      param_info2 = &file_info->defined_params[*parcod] ;
      (void) GNF_FindParameter(
                param_info2->group_name, param_info2->elem_name, non_scalar,
                file_info->no_items, file_info->items, &idxpar2 ) ;

      GNF_GetActualData( file_info, param_info2, idxpar2, cell_index,
         loc, *misval, data2, &nodata2 ) ;

      param_info3 = &file_info->defined_params[(*parcod)+1] ;
      (void) GNF_FindParameter(
                param_info3->group_name, param_info3->elem_name, non_scalar,
                file_info->no_items, file_info->items, &idxpar3 ) ;

      GNF_GetActualData( file_info, param_info3, idxpar3, cell_index,
         loc, *misval, data3, &nodata3 ) ;

      /* If we are dealing with TRIH files (locations with layers), then
         be particularly careful
      */
      if ( param_info->location_info->type == LOC_NAMES ||
           param_info->location_info->type == BAL_NAMES )
      {
         truegrid = 0         ;
         ndim[3]  = 1         ; /* Cells per layer */
         ndim[4]  = notimes   ; /* Number of times */
         indx = (TInt4 *) malloc( sizeof(TInt4) * (ndim[0]*ndim[1]) ) ;
         for ( i = 0 ; i < ndim[0]*ndim[1] ; i ++ )
         {
            indx[i] = i+1 ; /* Construct a horizontal "grid" */
         }
      }
      else
      {
         truegrid  = 1               ;
         if ( param_info->location_info->type == GRID_KCSINV )
         {
            truegrid  = 2 ;
         }
         ndim[3]   = ndim[0]*ndim[1] ; /* Cells per layer */
         ndim[4]   = notimes         ; /* Number of times */
         indx      = (TInt4 *) malloc( sizeof(TInt4) * (ndim[0]*ndim[1]) ) ;
         indloc[0] = 1 ;
         GNF_GetGrd( filename, itype, indloc, indx, &nocell, &igisty, ierror ) ;
      }

      if ( (partype == LOCAL_VECTOR_U)   ) result = 0 ;
      if ( (partype == LOCAL_VECTOR_V)   ) result = 1 ;
      if ( (partype == LOCAL_VECTOR_DIR) ) result = 2 ;
      if ( (partype == LOCAL_VECTOR_MAG) ) result = 3 ;

      ODS_ConvertVectorComponents( ndim, indx, data, data2, data3, truegrid,
         *misval, result, data4, &nodata ) ;
      ODS_CopyArray( data4, data, nodata ) ;

      free( data2 ) ;
      free( data3 ) ;
      free( data4 ) ;
      free( indx  ) ;

   }

   /* Accumulate parameters */
   if ( partype == SCALAR_DATA_SUMMED || partype == SCALAR_DATA_SUBTRACTED )
   {
      factor  = 1.0         ;
      if ( partype == SCALAR_DATA_SUBTRACTED )
      {
         factor = -1.0 ;
      }
      ndim[3] =   1         ;
      ndim[4] = notimes     ;

      data2   = (TReal4 *) malloc( sizeof(TReal4) * nodata ) ;
      cont    =   1         ;
      parcod2 = (*parcod)-1 ; /* Note the offset by -1! */
      while ( cont )
      {
         parcod2 ++ ;
         param_info2 = &file_info->defined_params[parcod2] ;
         (void) GNF_FindParameter(
                   param_info2->group_name, param_info2->elem_name, non_scalar,
                   file_info->no_items, file_info->items, &idxpar2 ) ;
         if ( idxpar2 >= 0 && strcmp( param_info2->public_name, "@" ) == 0 )
         {
            GNF_GetActualData( file_info, param_info2, idxpar2,
               cell_index, loc, *misval, data2, &nodata2 ) ;
            ODS_AccumulateData( *misval, ndim, data, data2, factor ) ;
         }
         else
         {
            cont = 0 ;
         }
      }
   }

   if ( (partype == ENCODED_DATA_U)   ||
        (partype == ENCODED_DATA_V)      )
   {
      GNF_GetIntBuffer( file_info, idxpar, &cell_index[0][0], &idata1, &elem_size,
         &number_cells ) ;
      GNF_GetIntBuffer( file_info, idxpar+1, &cell_index[0][0], &idata2, &elem_size,
         &number_cells ) ;

      indx = (TInt4 *) malloc( sizeof(TInt4) * (ndim[0]*ndim[1]) ) ;
      indloc[0] = 1 ;
      GNF_GetGrd( filename, itype, indloc, indx, &nocell, &igisty, ierror ) ;

      ODS_EncodeDryWetInformation( ndim, indx, idata1, idata2, data,
         param_info->location_info->type ) ;
      free( idata1 ) ;
      free( idata2 ) ;
      free( indx   ) ;
   }


   /* Determine the vertical coordinate
   */

   /* Last case: (Always) average over depth
   */
   if ( (param_info->type & DPT_AVERAGE) )
   {
      ODS_GetGrdIndices( filename, itype, param_info->location_info->type, ndim,
                         &indx, &truegrid ) ;
      ndim[4] = notimes ;

      zcrd = NULL ;
      param_info2 = &file_info->defined_params[0] ;
      param_info3 = &file_info->defined_params[1] ;
      ndim[2]     = ndim[2] + 1 ; /* We need an extra layer ... */
      ODS_DetermineZcoordinate( file_info, param_info2, param_info3,
         cell_index, loc, ndim, indx, *misval, &zcrd ) ;

      ndim[2]     = ndim[2] - 1 ; /* Correct again ... */
      ODS_AverageOverDepth( ndim, indx, data, zcrd,
         *misval, data_out, &nodata ) ;

      free( zcrd   ) ;
      free( data   ) ;
      free( indx   ) ;
   }

   /* printf( "GNF_GetMat - end - time %ld\n", (long)time(NULL) ) ; */
}

/* -------------------------------------------------------------------
    Function: GNF_GetActualData()
    Author:   Arjen Markus
    Purpose:  Get the actual values (raw routine)
    Context:  Used by ODS_GetMat()
    Pseudo Code:
              Call GNF_GetRealBuffer() to get the raw values. Then
              copy the required selection from the buffer into the
              data array.
------------------------------------------------------------------- */

TVoid
GNF_GetActualData(
   NefisFileInfoPtr   file_info,         /* I Information on the file */
   ParameterInfoPtr   param_info,        /* I Information on the parameter */
   TInt4              idxpar,            /* I Index in items array */
   TInt4              input_index[5][3], /* I array of cell indices */
   TInt4            * loc,               /* I array of requested location indices */
   TReal4             misval,            /* I reserved value for missing data */
   TReal4           * data,              /* O array with retrieved data */
   TInt4            * nodata     )       /* O number of data filled */
{
   LocationInfoPtr    grid_info  ;
   TInt4              data_index[5][4] ;
   TInt4              cell_index[5][3] ;
   TInt4              cell_index_const[5][3] ;
   TInt4              ndim[4]    ;
   TInt4              i          ;
   TInt4              i0         ;
   TInt4              i1         ;
   TInt4              i2         ;
   TInt4              i3         ;
   TInt4              i4         ;
   TInt4              j          ;
   TInt4              k          ;
   TInt4              k0         ;
   TInt4              k0_incr    ;
   TInt4              n0_size    ;
   TInt4              n1_size    ;
   TInt4              dim0       ;
   TInt4              dim1       ;
   TInt4              dimlay     ;
   TInt4              dimtyp     ;
   TInt4              idxloc     ;
   TInt4              pardim     ;
   TInt4            * lgrid      ;
   TInt4              truegrid   ;
   TInt4              elem_size  ;
   TInt4              number_cells ;
   TReal4             filter_value ;
   TReal4           * rbuffer    ;
   TInt4              offset     ;

   TInt4                     do_free               ;
   static TReal4           * rscalar        = NULL ;
   static TReal4           * rarray         = NULL ;
   static NefisFileInfoPtr   previous_file  = NULL ;
   static ParameterInfoPtr   previous_param = NULL ;

   /* printf( "GNF_GetActualData - begin - time %ld\n", (long)time(NULL) ) ; */
   /* We trust the cell_index array, unless the parameter is constant
   */
   for ( j = 0 ; j < 5 ; j ++ )
   {
      cell_index[j][0] = input_index[j][0] ;
      cell_index[j][1] = input_index[j][1] ;
      cell_index[j][2] = input_index[j][2] ;

      cell_index_const[j][0] = input_index[j][0] ;
      cell_index_const[j][1] = input_index[j][1] ;
      cell_index_const[j][2] = input_index[j][2] ;
   }
   /* Treat the special case of constant data being converted
      into timeseries - otherwise NEFIS complains
   */
   cell_index_const[0][1] = cell_index_const[0][0] ;

   if ( param_info->datetime_info->type == CONSTANT )
   {
      cell_index_const[0][0] = 0 ;
      cell_index_const[0][1] = 0 ;
   }

   /* printf( "GNF_RealBuffer - begin - time %ld\n", (long)time(NULL) ) ; */

   if ( file_info->items[idxpar].group_ndim    == 1 &&
        file_info->items[idxpar].group_dims[0] == 1    )
   {
      GNF_GetRealBuffer( file_info, idxpar, &cell_index_const[0][0],
         &rscalar, &elem_size, &number_cells ) ;
      rbuffer = rscalar ;
      do_free = 1       ;
   }
   else
   {
      do_free = 0;
      if ( previous_file != file_info || previous_param != param_info || rarray == NULL ||
           !GNF_CheckCache( 0, idxpar, &cell_index[0][0] ) )
      {
         /* printf( "GNF_RealBuffer - reading buffer - time %ld\n", (long)time(NULL) ) ; */
         previous_file  = file_info  ;
         previous_param = param_info ;
         if ( rarray != NULL )
         {
            free( rarray ) ;
            rbuffer = NULL ;
         }

         GNF_GetRealBuffer( file_info, idxpar, &cell_index[0][0], &rarray,
             &elem_size, &number_cells ) ;
      }
      rbuffer = rarray ;
   }

   /* printf( "GNF_RealBuffer - end - time %ld\n", (long)time(NULL) ) ; */

   /* Copying the data is easy if the element contains nothing more
      than what is needed. Otherwise a small piece is required.
      We have the following cases:
      - The NEFIS element is defined on a grid (i.e. element dimension = 2 or 3),
        there is only one actual parameter in the element
      - The NEFIS element is defined on a grid (i.e. element dimension = 2 or 3),
        but there are several actual parameters in the element (fixed_last_index > 0)
      - The NEFIS element is defined on named locations, but
        there is only one actual parameter in the element
      - The NEFIS element is defined on named locations, but
        there are several actual parameters in the element
      The distinction between a grid and named locations is: either the
      first or the second dimension is "flat". (The third dimension may
      be used for layers).
      The distinction between one or more parameters is: fixed_last_index > 0

      There is one case that is not easily solved in this setup: the
      data for the total depth are constant, but in some cases need to
      be copied over the index for time ...

   */
   dim0 = 0 ;
   dim1 = 1 ;
   if ( param_info->location_info->type == GRID_KCSINV )
   {
      dim0 = 1 ;
      dim1 = 0 ;
   }
   data_index[dim0][0] = loc[0]                                ;
   data_index[dim0][1] = loc[1]                                ;
   data_index[dim0][2] = loc[2]                                ;
   data_index[dim0][3] = MAX(1, file_info->items[idxpar].elem_dims[dim0] ) ;
   data_index[dim1][0] = loc[3]                                ;
   data_index[dim1][1] = loc[4]                                ;
   data_index[dim1][2] = loc[5]                                ;
   data_index[dim1][3] = MAX(1, file_info->items[idxpar].elem_dims[dim1] ) ;

   /* Careful: 2D parameters may be mixed up with 3D parameters, due to
      extra processing
      And of course, we need to deal with locations with layers too
   */
   dimtyp = 0 ; /* Unknown */
   if ( param_info->fixed_last_index == -1 )
   {
      dimlay = file_info->items[idxpar].elem_ndim ;
   }
   else if ( param_info->fixed_last_index < SCALE_FIRST )
   {
      dimlay = file_info->items[idxpar].elem_ndim - 1 ;
   }
   else
   {
      dimlay = 1 ; /* This is for the mass balances in Delft3D-FLOW */
   }

   switch ( dimlay )
   {
   case 1:
      dimtyp = 10 ; /* 1D */
      break ;
   case 2:
      dimtyp = 20 ; /* 2DH or 2DV */
      if ( loc[3] == loc[4] )
      {
         dimtyp = 21 ; /* 2DV */
      }
      break ;
   case 3:
      dimtyp = 30 ; /* 3D */
   }

   /* Handle according the dimension type
   */
   if ( dimtyp == 30 )
   {
      data_index[2][0] = MIN(loc[6], file_info->items[idxpar].elem_dims[2]-1 ) ;
      data_index[2][1] = MIN(loc[7], file_info->items[idxpar].elem_dims[2]-1 ) ;
      data_index[2][2] = loc[8]                                                ;
      data_index[2][3] = MAX(1, file_info->items[idxpar].elem_dims[2] )        ;

      if ( param_info->type == SCALAR_DATA_FACE )
      {
         /* Skip the first layer */
         data_index[2][0] ++ ;
         data_index[2][1] ++ ;
         data_index[2][3] = MAX(1, file_info->items[idxpar].elem_dims[2] - 1 ) ;
      }
   }
   if ( dimtyp == 21 )
   {
      /* Locations with layers? More general than loc[3] == 0
         (includes vertical cut-outs)
         -- 2DV
      */
      if ( loc[3] == loc[4] && file_info->items[idxpar].elem_ndim > 1 )
      {
         data_index[2][0] = MIN(loc[6], file_info->items[idxpar].elem_dims[1]-1 ) ;
         data_index[2][1] = MIN(loc[7], file_info->items[idxpar].elem_dims[1]-1 ) ;
         data_index[2][2] = loc[8]                                                ;
         data_index[2][3] = MAX(1, file_info->items[idxpar].elem_dims[1] )        ;
         data_index[1][3] = 1                                                     ;
      }
   }
   if ( dimtyp == 20 || dimtyp == 10 )
   {
      /* 1D and 2D parameters */
      data_index[2][0] = 0 ;
      data_index[2][1] = 0 ;
      data_index[2][2] = loc[8] ;
      if ( dimtyp == 20 )
      {
         data_index[2][3] = MAX(1, file_info->items[idxpar].elem_dims[2] ) ;
      }
      else
      {
         data_index[1][3] = 1      ; /* Wonderful complications ... */
         data_index[2][3] = loc[8] ;
      }
   }
   if ( dimtyp == 10 || dimtyp == 21 )
   {
      data_index[0][0] -- ; /* Correct the offset of 1 */
      data_index[0][1] -- ;
   }

   if ( param_info->fixed_last_index == -1 )
   {
      data_index[3][0] = 0      ;
      data_index[3][1] = 0      ;
      data_index[3][2] = 1      ;
      data_index[3][3] = 1      ;
   }
   else
   {
      data_index[3][0] = param_info->fixed_last_index % SCALE_FIRST - 1 ; /* Correct for offset */
      data_index[3][1] = param_info->fixed_last_index % SCALE_FIRST - 1 ; /* Correct for offset */
      data_index[3][2] = 1                                ;

      pardim = file_info->items[idxpar].elem_ndim - 1 ;
      data_index[3][3] = MAX(1, file_info->items[idxpar].elem_dims[pardim] ) ;
   }

   data_index[4][0] = 0                                   ;
   data_index[4][1] = cell_index[0][1] - cell_index[0][0] ;
   data_index[4][2] = 1                                   ;
   data_index[4][3] = number_cells                        ;

   /* In case of Delft3D-FLOW mass balances, we need to shift some of the dimensions
   */
   if ( param_info->fixed_last_index >= FIRST_IS_1 )
   {
      data_index[1][3] = MAX(1, file_info->items[idxpar].elem_dims[1] ) ; /* Restore */
      for ( i = 3; i >= 1; i -- )
      {
         data_index[i][0] = data_index[i-1][0] ;
         data_index[i][1] = data_index[i-1][1] ;
         data_index[i][2] = data_index[i-1][2] ;
      }
      data_index[0][0] = param_info->fixed_last_index / SCALE_FIRST - 1 ;
      data_index[0][1] = param_info->fixed_last_index / SCALE_FIRST - 1 ;
      data_index[0][2] = 1 ;
   }


   /* Keep track of the whole cascade
   */
   if ( param_info->location_info->type != GRID_KCSINV )
   {
      n0_size = 1                ;
      n1_size = data_index[0][3] ;
   }
   else
   {
      n0_size = data_index[1][3] ;
      n1_size = 1                ;
   }

   k0_incr = (data_index[1][1]-data_index[1][0]+1) / data_index[1][2] *
             (data_index[0][1]-data_index[0][0]+1) / data_index[0][2] ;

   data_index[1][3] = data_index[1][3] * data_index[0][3] ;
   data_index[2][3] = data_index[2][3] * data_index[1][3] ;
   data_index[3][3] = data_index[3][3] * data_index[2][3] ;
   data_index[4][3] = data_index[4][3] * data_index[3][3] ;

   if ( file_info->items[idxpar].group_ndim    == 1 &&
        file_info->items[idxpar].group_dims[0] == 1    )
   {
      k0 = -k0_incr - data_index[1][0] * n1_size -
                      data_index[0][0] * n0_size ;
      for ( i4 = data_index[4][0] ; i4 <= data_index[4][1] ;
            i4 += data_index[4][2] )
      {
         for ( i3 = data_index[3][0] ; i3 <= data_index[3][1] ;
               i3 += data_index[3][2] )
         {
            for ( i2 = data_index[2][0] ; i2 <= data_index[2][1] ;
                  i2 += data_index[2][2] )
            {
               k0 = k0 + k0_incr ;
               for ( i1 = data_index[1][0] ; i1 <= data_index[1][1] ;
                     i1 += data_index[1][2] )
               {
                  j = i3 * data_index[2][3] +
                      i2 * data_index[1][3] +
                      i1 * data_index[0][3]   ;
                  for ( i0 = data_index[0][0] ; i0 <= data_index[0][1] ;
                        i0 += data_index[0][2] )
                  {
                     k       = k0 + i1 * n1_size + i0 * n0_size ;
                     data[k] = rbuffer[j+i0] ;
                  }
               }
            }
         }
      }
   }
   else
   {
      k0 = -k0_incr - data_index[1][0] * n1_size -
                      data_index[0][0] * n0_size ;
      for ( i4 = data_index[4][0] ; i4 <= data_index[4][1] ;
            i4 += data_index[4][2] )
      {
         for ( i3 = data_index[3][0] ; i3 <= data_index[3][1] ;
               i3 += data_index[3][2] )
         {
            for ( i2 = data_index[2][0] ; i2 <= data_index[2][1] ;
                  i2 += data_index[2][2] )
            {
               k0 = k0 + k0_incr ;
               for ( i1 = data_index[1][0] ; i1 <= data_index[1][1] ;
                     i1 += data_index[1][2] )
               {
                  j = i4 * data_index[3][3] +
                      i3 * data_index[2][3] +
                      i2 * data_index[1][3] +
                      i1 * data_index[0][3]   ;
                  for ( i0 = data_index[0][0] ; i0 <= data_index[0][1] ;
                        i0 += data_index[0][2] )
                  {
                     k       = k0 + i1 * n1_size + i0 * n0_size ;
                     data[k] = rbuffer[j+i0] ;
                  }
               }
            }
         }
      }
   }


/*
   printf( "Buffer:          %d %d\n", number_cells, elem_size ) ;
   printf( "Number data:     %d\n", k ) ;
   printf( "Data-index:      %d %d %d\n", data_index[0][0], data_index[0][1], data_index[0][2] ) ;
   printf( "                 %d %d %d\n", data_index[1][0], data_index[1][1], data_index[1][2] ) ;
   printf( "                 %d %d %d\n", data_index[2][0], data_index[2][1], data_index[2][2] ) ;
   printf( "                 %d %d %d\n", data_index[3][0], data_index[3][1], data_index[3][2] ) ;
   printf( "                 %d %d %d\n", data_index[4][0], data_index[4][1], data_index[4][2] ) ;
   printf( "Location matrix: %d\n",
      (loc[1]-loc[0]+1)*(loc[4]-loc[3]+1)*(loc[7]-loc[6]+1) ) ;
*/

   *nodata = k0_incr *
             (data_index[2][1]-data_index[2][0]+1) / data_index[2][2] *
             (data_index[3][1]-data_index[3][0]+1) / data_index[3][2] *
             (data_index[4][1]-data_index[4][0]+1) / data_index[4][2] ;

   /* Filter out the missing value -999.0
      (specifically for the z-layer model of Delft3D-FLOW)
      Note the equality, as the value -999.0 is exact!
   */
   filter_value = -999.0 ;
   if ( param_info->type == SCALAR_MISSING_ZERO )
   {
       filter_value = 0.0 ;
   }

   for ( i = 0 ; i < (*nodata) ; i ++ )
   {
      if ( data[i] == filter_value )
      {
         data[i] = misval ;
      }
   }


   /* Special processing is done here
      SPECIAL_DEPTH_CENTRE: Delft3D-FLOW specific determination of depth
                            in cell centre
   */
   if ( param_info->type == SPECIAL_DEPTH_CENTRE )
   {
      for ( i = 0 ; i < (*nodata) ; i ++ )
      {
         rbuffer[i] = data[i] ;
      }

      ndim[0]   = file_info->items[idxpar].elem_dims[0]      ;
      ndim[1]   = file_info->items[idxpar].elem_dims[1]      ;
      grid_info = file_info->defined_params[0].location_info ;

      GNF_FindParameter(
         grid_info->gridinfo_group, grid_info->gridinfo_elem, 1,
         file_info->no_items, file_info->items, &idxloc ) ;

      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;

      ODS_GetGrdIndices( file_info->filename[0], &file_info->filetype, param_info->location_info->type, ndim,
         &lgrid, &truegrid ) ;

/*
      GNF_GetIntBuffer( file_info, idxloc, cell_index, &lgrid,
         &elem_size, &number_cells ) ;
*/

      ODS_DetermineDepthInCellCentre(
         file_info, ndim, lgrid, rbuffer, misval, data ) ;
      free( lgrid ) ;
   }


   if ( do_free )
   {
      free( rscalar ) ;
   }

   /* printf( "GNF_GetActualData - end - time %ld\n", (long)time(NULL) ) ; */
}

/* -------------------------------------------------------------------
    Function: GNF_GetGrd()
    Author:   Arjen Markus
    Purpose:  Get the grid information array (ODS)
    Context:  Used by ODS getgrd()
    Pseudo Code:
              Read the grid information from file directly or construct
              it using one of the known methods.
------------------------------------------------------------------- */

TVoid
   GNF_GetGrd(
      TString    filename,     /*  I   Full filename, including extension      */
      TInt4    * itype,        /*  I   Filetype                                */
      TInt4    * indloc,       /*  I   Location index array (3*3)              */
      TInt4    * indx,         /*  O   Array with cell index information       */
      TInt4    * nocell,       /*  O   Number of cells (model data) in grid    */
      TInt4    * igisty,       /*  O   Type of geographical data               */
      TInt4    * ierror        /*  O   Number of cells (model data) in grid etc*/
   )
{
   NefisFileInfoPtr   file_info   ;
   ParameterInfoPtr   grid_info   ;
   LocationInfoPtr    loc_info    ;
   TChar              group_name[32] ;
   TChar              elem_name[32]  ;
   TString            pchr        ;
   TInt4              i           ;
   TInt4              idxloc      ;
   TInt4              cell_index[5][3] ;
   TInt4            * ibuffer     ;
   TInt4              elem_size   ;
   TInt4              no_nef_cells;


/*   printf( "GNF_GetGrd\n" ) ; */
   /* Initialisation
   */
   *ierror = IEOK ;
   file_info = GNF_GetFileInfo( filename, *itype, ierror ) ;

   if ( *ierror != IEOK )
   {
      return ;
   }

   /* Find out which element to get from the file, then get the
      (integer) data
      Assumption: all parameters have the same grid structure
   */
   grid_info  = &file_info->defined_params[0] ;
   loc_info   = grid_info->location_info      ;

   strncpy( group_name, loc_info->gridinfo_group, 32 ) ;
   strncpy( elem_name,  loc_info->gridinfo_elem,  32 ) ;

   pchr = strchr( group_name, '|' ) ; if ( pchr != NULL ) *pchr = '\0' ;
   pchr = strchr( elem_name,  '|' ) ; if ( pchr != NULL ) *pchr = '\0' ;

   (void) GNF_FindParameter( group_name, elem_name, 1,
             file_info->no_items, file_info->items, &idxloc ) ;

   /* Do we need to look for a possible alternative? */
   if ( idxloc == -1 )
   {
      pchr = strchr( loc_info->gridinfo_group, '|' ) ;
      if ( pchr != NULL ) strcpy( group_name, pchr+1 ) ;

      pchr = strchr( loc_info->gridinfo_elem, '|' ) ;
      if ( pchr != NULL ) strcpy( elem_name, pchr+1 ) ;

      (void) GNF_FindParameter( group_name, elem_name, 1,
                file_info->no_items, file_info->items, &idxloc ) ;
   }

   /* Alas, not found! */
   if ( idxloc == -1 )
   {
      *ierror = IEOTHR ;
      return ;
   }

   /* Request for information? Well, dummy usage actually */
   if ( indloc[0] == -1 )
   {
      switch( loc_info->type )
      {
      case GRID_LGRID :
         *ierror = IEUNKN ; /* I have no examples yet! */
         break ;

      case GRID_KCS     :
      case GRID_TRIVIAL :
         indx[0] = file_info->items[idxloc].elem_dims[0] ;
         indx[1] = file_info->items[idxloc].elem_dims[1] ;
         indx[2] = 1 ;
         indx[3] = indx[0] * indx[1] ;
         *nocell = indx[3] ;
         break ;
      }
   }
   else
   {
      cell_index[0][0] = 0         ;
      cell_index[0][1] = ALL_CELLS ;
      cell_index[0][2] = 1         ;

 ibuffer = NULL;

      if ( loc_info->type != GRID_TRIVIAL )
      {
         GNF_GetIntBuffer( file_info, idxloc, &cell_index[0][0], &ibuffer,
            &elem_size, &no_nef_cells ) ;
      }

      switch ( loc_info->type )
      {
      case GRID_LGRID :   /* No special processing required - just copy */
         *igisty = IGCURV    ;
         *nocell = 0         ;
         for ( i = 0 ; i < elem_size ; i ++ )
         {
            indx[i] = ibuffer[i] ;
            if ( *nocell < indx[i] )
            {
               *nocell = indx[i] ; /* At least: for now!! */
            }
         }
         break ;

      case GRID_KCS :     /* Conversion required */
         *igisty = IGCURV ;
         *nocell = file_info->items[idxloc].elem_dims[0] *
                   file_info->items[idxloc].elem_dims[1]   ;
         ODS_ConvertKCS( file_info->items[idxloc].elem_dims, ibuffer, indx,
            loc_info->type ) ;
         break ;

      case GRID_KCSINV :     /* Conversion required */
         *igisty = IGCURV ;
         *nocell = file_info->items[idxloc].elem_dims[0] *
                   file_info->items[idxloc].elem_dims[1]   ;
         ODS_ConvertKCS( file_info->items[idxloc].elem_dims, ibuffer, indx,
            loc_info->type ) ;
         break ;

      case GRID_TRIVIAL :     /* Conversion required */
         *igisty = IGCURV ;
         *nocell = file_info->items[idxloc].elem_dims[0] *
                   file_info->items[idxloc].elem_dims[1]   ;
         ibuffer = (TInt4 *) malloc( sizeof(TInt4) ) ; /* Just because of free() */
         ODS_ConstructTrivialGrid( file_info->items[idxloc].elem_dims, indx ) ;
         break ;

      default  :
         /* Should not happen! */
         *ierror = IEUNKN ;
      }

      free( ibuffer ) ;
   }
}

/* -------------------------------------------------------------------
    Function: main()
    Author:   Arjen Markus
    Purpose:  Test driver
    Context:  --
    Pseudo Code:
              --
------------------------------------------------------------------- */

#ifdef TEST
#include <float.h>
int main( int argc, char *argv[] )
{
   TChar             option[1]             ;
   TChar             filename[3*NFFILELEN] ;
   NefisFileInfoPtr  file_info             ;
   TInt4             i                     ;
   TInt4             k                     ;
   TInt4             k2                    ;
   TInt4             idx                   ;
   TInt4             idxitm                ;
   TInt4             dummy                 ;
   TInt4             loc[9]                ;
   TInt4             ndim[5]               ;
   TInt4             ndiml[5]              ;
   TInt4             ftype                 ;
   TInt4             ierror                ;
   TInt4             i3gl                  ;
   TInt4             igisty                ;
   TInt4             maxdim                ;
   TInt4             maxdata               ;
   TInt4             maxlst                ;
   TInt4             nrlst                 ;
   TInt4             nrloc                 ;
   TInt4             nrtim                 ;
   TString           parlst                ;
   TString           paruni                ;
   TString           locnam                ;
   TInt4           * partyp                ;
   TInt4           * parcod                ;
   TInt4           * locid                 ;
   TInt4           * loctyp                ;
   TInt4             cell_index[15]        ;
   TInt4             elem_size             ;
   TInt4             number_cells          ;
   TInt4           * indx                  ;
   TInt4           * timtyp                ;
   TReal4            misval                ;
   TReal4          * rbuffer               ;
   TReal4          * data                  ;
   TReal8          * times                 ;
   TReal8            times_to_get[2]       ;
   TReal8            timdummy[2]           ;

   if ( argc < 3 )
   {
      printf( "Usage: %s datfile deffile\n", argv[0] ) ;
      exit(1) ;
   }

   strcpy( &filename[0],           argv[1] ) ;
   strcpy( &filename[0+NFFILELEN], argv[2] ) ;
   strcpy( &filename[0+2*NFFILELEN], "" );
   sscanf( argv[3], "%d", &ftype ) ;

/*
   if ( strncmp( &filename[0], "trim", 4 ) == 0 )
   {
      ftype = ODS_TRISULA_MAP_NEFIS ;
   }
   else
   {
      ftype = ODS_TRISULA_HIS_NEFIS ;
   }
*/
   file_info = GNF_GetFileContents( filename, ftype, &ierror ) ;

   if ( file_info == NULL )
   {
      printf( "Reading not successful!\n" ) ;
      exit( 1 ) ;
   }

   printf( "Group, element, dimensions\n" ) ;
   for ( i = 0 ; i < file_info->no_items ; i ++ )
   {
      printf( "%d - %s - %s - (number dims %d) %d %d %d %d %d\n",
         i, file_info->items[i].group_name,
            file_info->items[i].elem_name,
            file_info->items[i].elem_ndim,
            file_info->items[i].elem_dims[0],
            file_info->items[i].elem_dims[1],
            file_info->items[i].elem_dims[2],
            file_info->items[i].elem_dims[3],
            file_info->items[i].elem_dims[4] ) ;

   }

   printf( "GetDim:" )           ;
   dummy = 0                     ;

   GNF_GetDim( filename, &ftype, "P", &dummy, &dummy, &dummy, ndim,
               &ierror, option ) ;

   printf( "GetDim: %d %d %d %d %d\n", ndim[0], ndim[1], ndim[2],
      ndim[3], ndim[4] ) ;

   maxlst = ndim[1] ;
   parlst = (TString) malloc( 21*maxlst*sizeof(TChar) ) ;
   paruni = (TString) malloc( 21*maxlst*sizeof(TChar) ) ;
   partyp = (TInt4 *) malloc(    maxlst*sizeof(TInt4) ) ;
   parcod = (TInt4 *) malloc(    maxlst*sizeof(TInt4) ) ;

   printf( "GetPar:\n" ) ;
   GNF_GetPar( filename, &ftype, "*", &dummy, &dummy, &dummy, &maxlst,
               &dummy, parlst, paruni, partyp, parcod, &nrlst, &ierror,
               option ) ;

   maxdim = maxlst ;
   i3gl   = 0      ;

   for ( i = 2 ; i < nrlst ; i ++ )
   {
      GNF_GetDim( filename, &ftype, "T", &parcod[i], &dummy, &dummy, ndim,
                  &ierror, option ) ;
      GNF_GetDim( filename, &ftype, "L", &parcod[i], &dummy, &dummy, ndiml,
                  &ierror, option ) ;
      printf( "%d - %s - %d (number times: %d; locations: %d %d %d %d\n",
         i, &parlst[21*i], parcod[i], ndim[1], ndiml[0], ndiml[1],
         ndiml[2], ndiml[3] ) ;

      if ( ftype == ODS_TRISULA_HIS_NEFIS ||
           ftype == ODS_MORSYS_TRAH_NEFIS ||
           ftype == ODS_MORSYS_BOTH_NEFIS   )
      {
         maxlst = ndiml[1] ;
         locnam = (TString) malloc( 21*maxlst*sizeof(TChar) ) ;
         locid  = (TInt4 *) malloc(    maxlst*sizeof(TInt4) ) ;
         loctyp = (TInt4 *) malloc(    maxlst*sizeof(TInt4) ) ;
         GNF_GetLoc( filename, &ftype, "*", &dummy, &parcod[i], &dummy,
               &maxlst, locnam, loctyp, locid, &nrloc, &ierror, option ) ;
         for ( k = 0 ; k < maxlst ; k ++ )
         {
            printf( "    %s %d\n", &locnam[21*k], locid[k] ) ;
         }
         free( locnam ) ;
         free( locid  ) ;
         free( loctyp ) ;

         maxlst = ndim[1] ;
         maxdata= maxlst  ;
         times  = (TReal8*) malloc( maxlst*sizeof(TReal8) ) ;
         timtyp = (TInt4 *) malloc( maxlst*sizeof(TInt4)  ) ;
         data   = (TReal4*) malloc( maxlst*sizeof(TReal4) ) ;

         timdummy[0] = 0.0 ;
         timdummy[1] = FLT_MAX ;
         GNF_GetTme( filename, &ftype, timdummy, &dummy, &parcod[i], &dummy,
               &maxlst, times, timtyp, &nrtim, &ierror, option ) ;

         loc[0] = 1 ;
         loc[1] = 1 ;
         loc[2] = 1 ;
         loc[3] = 0 ;
         loc[4] = 0 ;
         loc[5] = 1 ; /* Important! */
         loc[6] = 0 ;
         loc[7] = 0 ;
         loc[8] = 1 ; /* Important! */
         if ( maxlst > 10 ) maxlst = 10 ;
         times[1] = times[maxlst-1] ;
         GNF_GetMat( filename, &ftype, &parcod[i], loc, times, &misval,
               &i3gl, &maxdata, data, &ierror, option ) ;

         for ( k = 0 ; k < maxlst ; k ++ )
         {
            printf( "    %lf %f\n", times[k], data[k] ) ;
         }
         free( times  ) ;
         free( timtyp ) ;
         free( data   ) ;
      }
      else
      {
         maxlst = ndim[1] ;
         times  = (TReal8*) malloc( maxlst*sizeof(TReal8) ) ;
         timtyp = (TInt4 *) malloc( maxlst*sizeof(TInt4)  ) ;
         maxdata= (ndiml[1]+1)*(ndiml[2]+1)*(ndiml[3]+1)    ;
         data   = (TReal4*) malloc( maxdata*sizeof(TReal4) ) ;
         GNF_GetTme( filename, &ftype, timdummy, &dummy, &parcod[i], &dummy,
               &maxlst, times, timtyp, &nrtim, &ierror, option ) ;

         loc[0] = 0 ;
         loc[1] = ndiml[1] - 1 ;
         loc[2] = 1 ;
         loc[3] = 0 ;
         loc[4] = ndiml[2] - 1 ;
         loc[5] = 1 ; /* Important! */
         loc[6] = 0 ;
         loc[7] = MAX(0, ndiml[3] - 1) ; /* Take care of 2D parameters! */
         loc[8] = 1 ; /* Important! */
         for ( k = 0 ; k < MAX(1,maxlst) ; k ++ )
         {
            times_to_get[0] = times[k] ;
            times_to_get[1] = times[k] ;

            GNF_GetMat( filename, &ftype, &parcod[i], loc, times_to_get, &misval,
                &i3gl, &maxdata, data, &ierror, option ) ;
            printf( "    %lf\n", times[k] ) ;
            for ( k2 = 0 ; k2 < 1000 ; k2 += 100 )
            {
               printf( "       %g %g %g %g %g %g %g %g %g %g\n",
                  data[k2],   data[k2+1], data[k2+2], data[k2+3], data[k2+4],
                  data[k2+5], data[k2+6], data[k2+7], data[k2+8], data[k2+9]  ) ;
            }
         }
         free( times  ) ;
         free( timtyp ) ;
         free( data   ) ;
      }
   }

   if ( ftype == ODS_TRISULA_MAP_NEFIS )
   {
      indx = (TInt4*) malloc( (loc[1]+1)*(loc[4]+1)*sizeof(TInt4) ) ;
      GNF_GetGrd( filename, &ftype, loc, indx, &number_cells, &igisty,
         &ierror ) ;
      printf( "Type:   %d\n", igisty       ) ;
      printf( "Nocell: %d\n", number_cells ) ;
      printf( "Error:  %d\n", ierror       ) ;

      for ( k2 = 0 ; k2 < 1000 ; k2 += 100 )
      {
         printf( "       %d %d %d %d %d %d %d %d %d %d\n",
            indx[k2],   indx[k2+1], indx[k2+2], indx[k2+3], indx[k2+4],
            indx[k2+5], indx[k2+6], indx[k2+7], indx[k2+8], indx[k2+9]  ) ;
      }
      free(indx) ;
   }

#if 0
   idx = parcod[0] - 1 ; /* Water level */
   (void) GNF_FindParameter( file_info->defined_params[idx].group_name,
             file_info->defined_params[idx].elem_name, 1,
             file_info->no_items,
             file_info->items, &idxitm ) ;
   cell_index[0] = 0 ;
   cell_index[1] = 0 ;
   cell_index[2] = 1 ;
   GNF_GetRealBuffer( file_info, idxitm, cell_index, &rbuffer, &elem_size,
      &number_cells ) ;


   /*
   ndim[0] =  1 ;
   ndim[1] = 10 ;
   GNF_CheckTimes( file_info, parcod[0], ndim, times ) ;

   printf( "Times: %lf %lf\n", times[0], times[1] ) ;
   */

   printf( "Element 0: (index = %d\n", idx ) ;
   printf( "   elem_size    = %d\n", elem_size    ) ;
   printf( "   number_cells = %d\n", number_cells ) ;

   for ( i = 0 ; i < 100 ; i += 5 )
   {
      printf( " %12.4e", rbuffer[i] ) ;
      printf( " %12.4e", rbuffer[i+1] ) ;
      printf( " %12.4e", rbuffer[i+2] ) ;
      printf( " %12.4e", rbuffer[i+3] ) ;
      printf( " %12.4e\n", rbuffer[i+4] ) ;
   }

   free( rbuffer ) ;

   cell_index[0] = 1 ;
   cell_index[1] = 1 ;
   cell_index[2] = 1 ;
   GNF_GetRealBuffer( file_info, idxitm, cell_index, &rbuffer, &elem_size,
      &number_cells ) ;

   printf( "Element 0: second cell\n" ) ;
   printf( "   elem_size    = %d\n", elem_size    ) ;
   printf( "   number_cells = %d\n", number_cells ) ;

   for ( i = 0 ; i < 100 ; i += 5 )
   {
      printf( " %12.4e", rbuffer[i] ) ;
      printf( " %12.4e", rbuffer[i+1] ) ;
      printf( " %12.4e", rbuffer[i+2] ) ;
      printf( " %12.4e", rbuffer[i+3] ) ;
      printf( " %12.4e\n", rbuffer[i+4] ) ;
   }

   free( rbuffer ) ;
#endif

   return 0 ;
}
#endif
