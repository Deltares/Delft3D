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
/*
 *   <oc.c> - Functions related to open en close of NEFIS file set
 *
 *
 *   J. Mooiman
 */
/*
 *   CVS/RCS/PVCS keywords
 *   ....
 */
/*
 *   Comment:
 *
 */
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>

#if defined(_WIN32) || defined(salford32)
#  include <io.h>
#  include <sys\stat.h>
#elif defined(linux)
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#elif defined(USE_SUN)
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/fcntl.h>
#endif

#if defined(GNU_PC) || defined (linux)
#  include <unistd.h>
#endif

#if defined(_WIN32)
#  define FILE_OPEN        _open
#  define FILE_CLOSE       _close
#  define FILE_READ_WRITE (_O_RDWR   | _O_BINARY)
#  define FILE_READ_ONLY  (_O_RDONLY | _O_BINARY)
#  define FILE_CREATE     (_O_CREAT  | _O_TRUNC | _O_RDWR     | _O_BINARY)
#  define FILE_MODE       (_S_IREAD  | _S_IWRITE)
#elif defined(GNU_PC) || defined(linux) || defined(salford32)
#  define FILE_OPEN        open
#  define FILE_CLOSE       close
#  define FILE_READ_WRITE  O_RDWR;
#  define FILE_READ_ONLY   O_RDONLY
#  define FILE_CREATE     (O_CREAT  | O_TRUNC  | O_RDWR )
#  define FILE_MODE       (S_IRUSR  | S_IWUSR  | S_IRGRP     | S_IROTH)
#elif defined(USE_SUN)
#  define FILE_OPEN        open64
#  define FILE_CLOSE       _close
#  define FILE_READ_WRITE  O_RDWR;
#  define FILE_READ_ONLY   O_RDONLY
#  define FILE_CREATE     (O_CREAT  | O_TRUNC  | O_LARGEFILE | O_RDWR )
#  define FILE_MODE       (S_IRUSR  | S_IWUSR  | S_IRGRP     | S_IROTH)
#else
#  define FILE_OPEN        this_file_open_not_supported
#endif

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "oc.h"

extern char * getcompanystring_nefis(void);
extern char * getfileversionstring_nefis(void);
extern char * getfullversionstring_nefis(void);

static BChar   tree_name[MAX_TREE_NAME];
static BInt4   first_create   = TRUE;
static BInt4   first_retrieve = TRUE;
static BInt4   first_tree     = TRUE;

BInt4   daf_fds;
BInt4   dat_fds;
BInt4   def_fds;
BInt4   nefis_flush;
BInt4   nr_nefis_files;
BInt4   cap_nefis_files;
BInt4   incr_nefis_files;

BUInt8 *** new_capacity_retrieve_var( BUInt8 ***, BInt4, BInt4);


BInt4 create_nefis_files ( BInt4 * fd_nefis   ,
                           BText   dat_file   ,
                           BText   def_file   ,
                           BChar   coding     ,
                           BChar   access_type)
{
  static BChar dathdr[LHDRDT+1];
  static BChar defhdr[LHDRDF+1];
  static BChar dafhdr[LHDRDAF+1];
  static BInt4 first_oc = 1    ;

  BInt4   from_xdr;
  BText   cp ;
  BText   p_str = NULL;
  BData   vp ;
  BInt4   i;
  BInt4   j;
  BUInt8  n_read     = 0;
  BUInt8  n_written  = 0;
  BInt4   new_files  = FALSE;
  BInt4   set        = -1;

  /* Make sure a char is one (1) byte */
  if (sizeof(BChar) != 1)
  {
    nefis_errno = 8000;
    sprintf(error_text,
      "This size of character (!=1) is not supported\n");
    return 1;
  }

#if !defined(_WIN32)
    assert( sizeof( off_t ) >= sizeof( long ) );
#endif

  if ( first_oc == 1 )
  {
    nefis_errno  = 0;
    nefis_errcnt = 0;
    first_oc     = 0;
  }

  *fd_nefis = -1;

  dat_fds   = -1;
  def_fds   = -1;
  daf_fds   = -1;
  if ( first_create == TRUE )
  {
    first_create = FALSE;
    nr_nefis_files   = 0;
    cap_nefis_files  = 0;
    incr_nefis_files = 1;

    for (i=0; i<MAX_NEFIS_FILES; i++ )
    {
      nefis[i].dat_fds = -1;
      nefis[i].def_fds = -1;
      nefis[i].daf_fds = -1;
      nefis[i].exist   = -1;
      nefis[i].file_retrieve = FALSE;
      strcpy(nefis[i].dat_name, "");
      strcpy(nefis[i].def_name, "");
    }
  }

/*
 * Check on existing nefis files
 */
  for (i=0; i<nr_nefis_files; i++ )
  {
    if ( nefis[i].exist != -1 )
    {
      if ( (strcmp(nefis[i].dat_name, dat_file) == 0) &&
           (strcmp(nefis[i].def_name, ""      ) != 0)    )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8001;
        sprintf(error_text,
          "Data file \'%s\' has already been opened",
          nefis[i].dat_name);
        return nefis_errno;
      }

      if ( (strcmp(nefis[i].dat_name, ""      ) != 0) &&
           (strcmp(nefis[i].def_name, def_file) == 0)    )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8002;
        sprintf(error_text,
          "Definition file \'%s\' has already been opened",
          nefis[i].def_name);
        return nefis_errno;
      }
    }
  }

  set = -1;
  for (i=0; i<nr_nefis_files; i++ )
  {
    if ( nefis[i].exist == -1 )
    {
      set = i;
      nefis[i].exist = i;
      break;
    }
    if ( (strcmp(nefis[i].dat_name, "") == 0) &&
         (strcmp(nefis[i].def_name, "") != 0)    )
    {
      set = i;
      break;
    }
    if ( (strcmp(nefis[i].dat_name, "") != 0) &&
         (strcmp(nefis[i].def_name, "") == 0)    )
    {
      set = i;
      break;
    }
  }
  if (set == -1)
  {
      set = i;
      nefis[set].exist = set;
      nr_nefis_files += 1;
  }

  if ( set >= MAX_NEFIS_FILES )
  {
    nefis_errcnt += 1;
    nefis_errno   = 8003;
    sprintf(error_text,
      "Maximum number (=%d) of open files has been achieved",
       MAX_NEFIS_FILES);
    return nefis_errno;
  }

  if ( set != 0 )
  {
    set = set;  /* Debug statement */
  }

  nefis[set].one_file    = FALSE;
  nefis[set].file_retrieve = FALSE;
  nefis[set].file_version = Version_5;
  if ( strcmp(dat_file, def_file) == 0 )
  {
    nefis[set].one_file = TRUE;
  }

/*
 * In case data file needs to be created
 */
  if ( (BInt4)strlen(dat_file) != 0 && nefis[set].one_file == FALSE )
  {

/*
 * Create or open data file
 */
    nefis_errno = open_nefis_file( dat_file, access_type, &dat_fds, &new_files);
    if ( nefis_errno != 0 )
    {
      nefis[set].exist = -1;
      return nefis_errno;
    }
/*
 * Allocate pointer to retrieve_var array,
 * first dimension : file set descriptor
 * second dimension: variable group counter
 * thirth dimension: actual file location where data is stored
 *                   1: group pointer
 *                   2: actual length of variable group dimension (N)
 *                   3: data pointer to index 0 (fixed to -1, never used)
 *                   4: data pointer to index 1
 *                   5: data pointer to index 2
 *                      .......
 *                 N+3: data pointer to index N
 *
 * this array will be reallocated when variable dimensions are used
 * the array will be freed when the last nefis file set is closed
 */
    if ( cap_nefis_files == 0 )
    {
      cap_nefis_files = 1;
      retrieve_var = (BUInt8 ***) malloc ( cap_nefis_files * sizeof(BUInt8) );
      for ( i=0; i<cap_nefis_files; i++ )
      {
        retrieve_var[i] = (BUInt8 **) malloc ( MAX_VAR_GROUPS * sizeof(BUInt8) );
      }
      for ( i=0; i<cap_nefis_files; i++ )
      {
        for ( j=0; j<MAX_VAR_GROUPS ; j++ )
        {
          retrieve_var[i][j]    = (BUInt8 *) malloc ( 1 * sizeof(BUInt8) );
          retrieve_var[i][j][0] = (BUInt8) BUINT8_MAX;
        }
      }
    }
    if (set >= cap_nefis_files)
    {
        cap_nefis_files += incr_nefis_files;
        retrieve_var = new_capacity_retrieve_var( retrieve_var, cap_nefis_files-incr_nefis_files, cap_nefis_files);
    }

    strncpy( nefis[set].dat_name, dat_file, (BInt4)strlen(dat_file));
    nefis[set].dat_name[strlen(dat_file)]='\0';
    nefis[set].dat_fds     = dat_fds;
    nefis[set].dat_neutral = FALSE ;

    for ( i=0; i<LHDRDT; i++ )
    {
      dathdr[i]=' ';
    }

    if ( new_files == TRUE )
    {
/*
 *      Generate new data file
 */
        {
            char * string;
            string = getcompanystring_nefis();
            sprintf(dathdr,"%s",string);
            strcat(dathdr, ", NEFIS Data File; ");
            string = getfileversionstring_nefis();
            strcat(dathdr, string);
            dathdr[LHDRDT] = '\0';
        }

        if (coding == 'N' || coding == 'n') coding = 'L';
        nefis[set].dat_neutral = setConversionNewFile(coding, dathdr, LHDRDT);
/*
 *      fill hash table with nil values
 */
        for ( i = 0; i<LHSHDT+2; i++ )
        {
            nefis[set].dat.fds[i] = NIL;
        }

/*
 *      calculate file length
 */
        nefis[set].dat.fds[1] = LHDRDT+SIZE_BINT8*(LHSHDT+1);

/*
 *      write headers to data file
 */
        n_written = GP_write_file ( dat_fds, dathdr, 0, LHDRDT );

        if ( n_written != LHDRDT )
        {
            nefis_errcnt += 1;
            nefis_errno   = 8004;
            sprintf(error_text,
                "Header was not written to data file \'%s\'",
                nefis[set].dat_name);
            return nefis_errno;
        }
/*
 *      flush hash table to data file
 */
        if ( nefis[set].dat_neutral == TRUE )
        {
            from_xdr = 0;
            vp = (BData) &nefis[set].dat.fds[1];
            cp = (BText) malloc( SIZE_BINT8 * (LHSHDT+1) );
            nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDT+1), SIZE_BINT8, "INTEGER", from_xdr);
        }
        else
        {
            cp = (BText) &nefis[set].dat.fds[1];
        }
        n_written = GP_write_file_pointers( dat_fds, cp, LHDRDT, SIZE_BINT8*(LHSHDT+1), set );

        if ( n_written != SIZE_BINT8*(LHSHDT+1) )
        {
            nefis_errcnt += 1;
            nefis_errno   = 8005;
            sprintf(error_text,
                "Hashtable was not written to data file \'%s\'",
                nefis[set].dat_name);
        }
        if ( nefis[set].dat_neutral == TRUE )
        {
            free( (void *) cp );
            cp = NULL;
        }
        if (nefis_errno != 0)
        {
            return nefis_errno;
        }
    }
    else {
      nefis[set].file_retrieve = TRUE;  /* assure reading pointers to data from file */
/*
 *  Read header and hash-table from existing data file
 */

      n_read = GP_read_file ( dat_fds, &dathdr[0], 0, LHDRDT );
      if ( n_read == -1 )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8007;
        sprintf(error_text,
          "Unable to access data file \'%s\'",
           nefis[set].dat_name);
        return nefis_errno;
      }
      if ( n_read > LHDRDT )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8008;
        sprintf(error_text,
          "During reading header of data file \'%s\'",
           nefis[set].dat_name);
        return nefis_errno;
      }
      dathdr[LHDRDT] = '\0'; 
/*
 *  Is the given file a NEFIS data file, yes or no
 */
      p_str = strstr(dathdr,"NEFIS Data File");
      if ( p_str == NULL)
      {
          p_str = strstr(dathdr,"DATA FILE");
      }
      if ( p_str == NULL)
          {
          p_str = strstr(dathdr,"NEFIS Definition and Data File");
          if (p_str != NULL)
          {
              nefis[set].one_file = TRUE;
              strcpy(def_file, dat_file);
          }
      }
      if (nefis[set].one_file == FALSE )
      {
        if ( p_str == NULL )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8009;
          sprintf(error_text,
            "File \'%s\' is not a NEFIS data file",
             nefis[set].dat_name);
          strcpy(nefis[set].dat_name, "");
          return nefis_errno;
        }

/*
 * Detect file version number
 */
        nefis_errno = detect_version( dathdr, &(nefis[set].file_version) );
        if ( nefis_errno != 0 )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8032;
          sprintf(error_text,
            "Data File \'%s\' has unregcognized version number",
           nefis[set].dat_name);
          return nefis_errno;
        }

      nefis[set].dat_neutral = setConversionExistingFile(set, coding, dathdr, LHDRDT);

/*
 *    Read hash table from NEFIS data file, skip file handle
 */
        n_read =  GP_read_file_pointers( dat_fds,
                          &nefis[set].dat.str[SIZE_BINT8],
                          LHDRDT,
                          SIZE_BINT8*(LHSHDT+1),
                          set);

        if ( n_read > SIZE_BINT8*(LHSHDT+1) )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8010;
          sprintf(error_text,
            "During reading hashtable of data file  \'%s\'",
             nefis[set].dat_name);
          return nefis_errno;
        }

        if ( nefis[set].dat_neutral == TRUE )
        {
          from_xdr = 1;
          vp = (BData) malloc( SIZE_BINT8 * (LHSHDT+1) );
          cp = &nefis[set].dat.str[SIZE_BINT8];

          nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDT+1),
                                      SIZE_BINT8, "INTEGER", from_xdr);
          for ( i=0; i<LHSHDT+1; i++ )
          {
            nefis[set].dat.fds[1+i] = ( *((BUInt8 *)vp + i));
          }
          free( (BData) vp );
        }
      }
    }
  }

/*
 * In case definition file needs to be created
 */
  if ( (BInt4)strlen(def_file) != 0 && nefis[set].one_file == FALSE )
  {

/*
 * Create or open definition file
 */
    nefis_errno = open_nefis_file( def_file, access_type, &def_fds, &new_files);
    if ( nefis_errno != 0 )
    {
      nefis[set].exist = -1;
      return nefis_errno;
    }
    strncpy( nefis[set].def_name, def_file, (BInt4)strlen(def_file));
    nefis[set].def_name[strlen(def_file)]='\0';
    nefis[set].def_fds     = def_fds;
    nefis[set].def_neutral = FALSE ;

    for ( i=0; i<LHDRDF; i++ )
    {
      defhdr[i]=' ';
    }

    if ( new_files == TRUE )
    {
/*
 *    Generate new definition file
 */
        {
            char * string;
            string = getcompanystring_nefis();
            sprintf(defhdr,"%s",string);
            strcat(defhdr, ", NEFIS Definition File; ");
            string = getfileversionstring_nefis();
            strcat(defhdr, string);
            defhdr[LHDRDF] = '\0';
        }

      if (coding == 'N' || coding == 'n') coding = 'L';
      coding = (BChar) toupper(coding);
      nefis[set].def_neutral = setConversionNewFile(coding, defhdr, LHDRDF);

/*
 *    fill hash table with nil values
 */
      for ( i = 0; i<LHSHDF+2; i++ )
      {
        nefis[set].def.fds[i] = NIL;
      }

/*
 *    calculate file length
 */
      nefis[set].def.fds[1] = LHDRDF+SIZE_BINT8*(LHSHDF+1);

/*
 *    write headers to definition file
 */
      n_written = GP_write_file ( def_fds, defhdr, 0, LHDRDF );

      if ( n_written != LHDRDF )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8011;
        sprintf(error_text,
          "Unable to write header of definition file \'%s\'",
           nefis[set].def_name);
        return nefis_errno;
      }

/*
 *    flush hash table to definition file
 */
        if ( nefis[set].def_neutral == TRUE )
        {
            from_xdr = 0;
            vp = (BData) &nefis[set].def.fds[1];
            cp = (BText) malloc( SIZE_BINT8 * (LHSHDF+1) );

            nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDF+1),
                                        SIZE_BINT8, "INTEGER", from_xdr);
        }
        else
        {
            cp = (BText)&nefis[set].def.fds[1];
        }
        n_written = GP_write_file_pointers ( def_fds, cp,
                                             LHDRDF, SIZE_BINT8*(LHSHDF+1), set);

        if ( n_written != SIZE_BINT8*(LHSHDF+1) )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8012;
          sprintf(error_text,
            "Unable to write hashtable of definition file \'%s\'",
            nefis[set].def_name);
        }
        if ( nefis[set].def_neutral == TRUE )
        {
            free( (void *) cp );
            cp = NULL;
        }
        if (nefis_errno != 0)
        {
            return nefis_errno;
        }
    }
    else
    {

/*
 *    Read header and hash-table from existing definition file
 */
      n_read = GP_read_file( def_fds, defhdr, 0, LHDRDF );
      if ( n_read == -1 )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8014;
        sprintf(error_text,
          "Unable to access definition file \'%s\'",
           nefis[set].def_name);
        return nefis_errno;
      }
      if ( n_read > LHDRDF )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8015;
        sprintf(error_text,
          "During reading header of definition file \'%s\'",
           nefis[set].def_name);
        return nefis_errno;
      }
      defhdr[LHDRDF] = '\0';
/*
 *  Is the given file a NEFIS definition file, yes or no
 */
      p_str = strstr(defhdr,"NEFIS Definition File");
      if ( p_str == NULL)
      {
        p_str = strstr(defhdr,"DEFN FILE");
      }
      if ( p_str == NULL)
          {
          p_str = strstr(dathdr,"NEFIS Definition and Data File");
          if (p_str != NULL)
          {
              nefis[set].one_file = TRUE;
              strcpy(dat_file, def_file);
          }
      }
      if (nefis[set].one_file == FALSE )
      {
        if ( p_str == NULL )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8016;
          sprintf(error_text,
            "File \'%s\' is not a NEFIS definition file",
             nefis[set].def_name);
          strcpy(nefis[set].def_name, "");
          return nefis_errno;
        }

/*
 * Detect file version number
 */
        nefis_errno = detect_version( defhdr, &nefis[set].file_version );
        if ( nefis_errno != 0 )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8033;
          sprintf(error_text,
            "Definition File \'%s\' has unregcognized version number",
           nefis[set].def_name);
          return nefis_errno;
        }

        nefis[set].def_neutral = setConversionExistingFile(set, coding, defhdr, LHDRDF);

/*
 *    Read hash table from NEFIS definition file, skip file handle
 */
        n_read = GP_read_file_pointers ( def_fds,
                         &nefis[set].def.str[SIZE_BINT8],
                         LHDRDF,
                         SIZE_BINT8*(LHSHDF+1),
                         set);

        if ( n_read > SIZE_BINT8*(LHSHDF+1) )
        {
          nefis_errcnt += 1;
          nefis_errno   = 8017;
          sprintf(error_text,
            "On reading hashtable of definition file \'%s\'",
             nefis[set].def_name);
          return nefis_errno;
        }

        if ( nefis[set].def_neutral == TRUE )
        {
          from_xdr = 1;
          vp = (BData) malloc( SIZE_BINT8 * (LHSHDF+1) );
          cp = &nefis[set].def.str[SIZE_BINT8];

          nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDF+1),
                                      SIZE_BINT8, "INTEGER", from_xdr);

          for ( i=0; i<(LHSHDF+1); i++ )
          {
            nefis[set].def.fds[1+i] = ( *((BUInt8 *)vp + i));
          }
          free( (BData) vp );
        }
      }
    }
  }
/*
 * In case DefinitionData file needs to be created
 */
    if ( nefis[set].one_file == TRUE )
    {

/*
 * Create or open DefinitionData file
 */
        nefis_errno = open_nefis_file( def_file, access_type, &daf_fds, &new_files);
        if ( nefis_errno != 0 )
        {
            nefis[set].exist = -1;
            return nefis_errno;
        }
/*
 * Allocate pointer to retrieve_var array,
 * first dimension : file set descriptor
 * second dimension: variable group counter
 * thirth dimension: actual file location where data is stored
 *                   1: group pointer
 *                   2: actual length of variable group dimension (N)
 *                   3: data pointer to index 0 (fixed to -1, never used)
 *                   4: data pointer to index 1
 *                   5: data pointer to index 2
 *                      .......
 *                 N+3: data pointer to index N
 *
 * this array will be reallocated when variable dimensions are used
 * the array will be freed when the last nefis file set is closed
 */
        if ( cap_nefis_files == 0 )
        {
            cap_nefis_files = 1;
            retrieve_var = (BUInt8 ***) malloc ( cap_nefis_files * sizeof(BUInt8) );
            for ( i=0; i<cap_nefis_files; i++ )
            {
                retrieve_var[i] = (BUInt8 **) malloc ( MAX_VAR_GROUPS * sizeof(BUInt8) );
            }
            for ( i=0; i<cap_nefis_files; i++ )
            {
                for ( j=0; j<MAX_VAR_GROUPS ; j++ )
                {
                    retrieve_var[i][j]    = (BUInt8 *) malloc ( 1 * sizeof(BUInt8) );
                    retrieve_var[i][j][0] = (BUInt8) BUINT8_MAX;
                }
            }
        }
        if (set >= cap_nefis_files)
        {
            cap_nefis_files += incr_nefis_files;
            retrieve_var = new_capacity_retrieve_var( retrieve_var, cap_nefis_files-incr_nefis_files, cap_nefis_files);
        }

        strncpy( nefis[set].daf_name, def_file, (BInt4)strlen(def_file));
        nefis[set].daf_name[strlen(def_file)]='\0';
        nefis[set].daf_fds     = daf_fds;
        nefis[set].daf_neutral = FALSE;
        nefis[set].dat_neutral = FALSE;
        nefis[set].def_neutral = FALSE;

        for ( i=0; i<LHDRDAF; i++ )
        {
            dafhdr[i]=' ';
        }

        if ( new_files == TRUE )
        {
/*
 *    Generate new DefinitionData file
 */
            {
                char * string;
                string = getcompanystring_nefis();
                sprintf(dafhdr,"%s",string);
                strcat(dafhdr, ", NEFIS Definition and Data File; ");
                string = getfullversionstring_nefis();
                strcat(dafhdr, string);
            }

            if (coding == 'N' || coding == 'n') coding = 'L';
            nefis[set].daf_neutral = setConversionNewFile(coding, dafhdr, LHDRDAF);

/*
 *    fill hash table with nil values
 */
            for ( i = 0; i<LHSHDAF+2; i++ )
            {
                nefis[set].daf.fds[i] = NIL;
            }

/*
 *    calculate file length
 */
            nefis[set].daf.fds[1] = LHDRDAF+SIZE_BINT8*(LHSHDAF+1);

/*
 *    write headers to DefinitionData file
 */
            n_written = GP_write_file ( daf_fds, dafhdr, 0, LHDRDAF );

            if ( n_written != LHDRDAF )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8018;
                sprintf(error_text,
                    "Unable to write header of DefinitionData file \'%s\'",
                    nefis[set].daf_name);
                return nefis_errno;
            }

/*
 *    flush hash table to DefinitionData file
 */
            if ( nefis[set].daf_neutral == TRUE )
            {
                from_xdr = 0;
                vp = (BData) &nefis[set].daf.fds[1];
                cp = (BText) malloc( SIZE_BINT8 * (LHSHDAF+1) );

                nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDAF+1),
                                             SIZE_BINT8, "INTEGER", from_xdr);
            }
            else
            {
                cp = (BText) &nefis[set].daf.fds[1];
            }
            n_written = GP_write_file_pointers ( daf_fds, cp, LHDRDAF, SIZE_BINT8*(LHSHDAF+1), set);

            if ( n_written != SIZE_BINT8*(LHSHDAF+1) )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8019;
                sprintf(error_text,
                    "Unable to write hashtable of DefinitionData file \'%s\'",
                    nefis[set].daf_name);
            }
            if ( nefis[set].daf_neutral == TRUE )
            {
                free( (void *) cp );
                cp = NULL;
            }
            if (nefis_errno != 0)
            {
                return nefis_errno;
            }
        }
        else
        {

/*
 *    Read header and hash-table from existing DefinitionData file
 */
            n_read = GP_read_file( daf_fds, dafhdr, 0, LHDRDAF );
            if ( n_read == -1 )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8021;
                sprintf(error_text,
                    "Unable to access DefinitionData file \'%s\'",
                    nefis[set].daf_name);
                return nefis_errno;
            }
            if ( n_read > LHDRDAF )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8022;
                    sprintf(error_text,
                    "During reading header of DefinitionData file \'%s\'",
                    nefis[set].daf_name);
                return nefis_errno;
            }

/*
 *  Is the given file a NEFIS DefinitionData file, yes or no
 */
            p_str = strstr(dafhdr,"NEFIS Definition and Data File");
            if ( p_str == NULL )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8023;
                sprintf(error_text,
                    "File \'%s\' is not a NEFIS DefinitionData file",
                    nefis[set].daf_name);
                strcpy(nefis[set].daf_name, "");
                return nefis_errno;
            }

/*
 * Detect file version number
 */
            nefis_errno = detect_version( dafhdr, &nefis[set].file_version);
            if ( nefis_errno != 0 )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8034;
                sprintf(error_text,
                    "DefinitionData File \'%s\' has unregcognized version number",
                    nefis[set].daf_name);
                strcpy(nefis[set].daf_name, "");
                return nefis_errno;
            }

            nefis[set].daf_neutral =  setConversionExistingFile(set, coding, dafhdr, LHDRDAF);

/*
 *    Read hash table from NEFIS DefinitionData file, skip file handle
 */
            n_read = GP_read_file_pointers ( daf_fds ,
                       &nefis[set].daf.str[SIZE_BINT8],
                       LHDRDAF,
                       SIZE_BINT8*(LHSHDAF+1),
                       set);

            if ( n_read > SIZE_BINT8*(LHSHDAF+1) )
            {
                nefis_errcnt += 1;
                nefis_errno   = 8024;
                sprintf(error_text,
                    "On reading hashtable of DefinitionData file \'%s\'",
                    nefis[set].daf_name);
                return nefis_errno;
            }

            if ( nefis[set].daf_neutral == TRUE )
            {
                from_xdr = 1;
                vp = (BData) malloc( sizeof(BUInt8) * (LHSHDAF+1) );
                cp = &nefis[set].daf.str[SIZE_BINT8];

                nefis_errno = convert_ieee( &vp, &cp, SIZE_BINT8*(LHSHDAF+1),
                                            SIZE_BINT8, "INTEGER", from_xdr);
                for ( i=0; i<(LHSHDAF+1); i++ )
                {
                    nefis[set].daf.fds[1+i] = ( *((BUInt8 *)vp + i));
                }
                free( (BData) vp );
            }
        }
    }

    *fd_nefis = set;

    return nefis_errno;
}
/*===================================================================*/

BInt4 close_nefis_files ( BInt4 * fd_nefis )
{
  BInt4   error = 0;
  BInt4   clean_up;
  BInt4   i;
  BInt4   j;
  BInt4   set;

/*
 *  flush hash table to file
 */
  set= *fd_nefis;
  if ( set == -1 )
  {
    return nefis_errno;
  }
  if ( (nefis[set].dat_fds == -1) &&
       (nefis[set].def_fds == -1) &&
       (nefis[set].daf_fds == -1)    )
  {
    *fd_nefis = -1;
    nefis[set].exist = -1;
    nefis[set].array_retrieve_ptrs = FALSE;
    strcpy(nefis[set].daf_name, "");
    strcpy(nefis[set].dat_name, "");
    strcpy(nefis[set].def_name, "");
    return nefis_errno;
  }

/*
 *  Data file
 */
  if ( nefis[set].one_file == FALSE )
  {
    if (nefis[set].dat_fds != -1)
    {
      if ( nefis_flush == TRUE )
      {
        nefis_errno = GP_flush_hash( nefis[set].dat_fds, set);
        if ( nefis_errno != 0 )
        {
          return nefis_errno;
        }
      }

      error = FILE_CLOSE( nefis[set].dat_fds );

      if ( error != 0 )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8025;
        sprintf(error_text,
          "Unable to close data file \'%s\'",
           nefis[set].dat_name);
        return nefis_errno;
      }
    }

/*
 *  Definition file
 */
    if (nefis[set].def_fds != -1)
    {
      if ( nefis_flush == TRUE )
      {
        nefis_errno = GP_flush_hash( nefis[set].def_fds, set);
        if ( nefis_errno != 0 )
        {
          return nefis_errno;
        }
      }

      error = FILE_CLOSE( nefis[set].def_fds );

      if ( error != 0 )
      {
        nefis_errcnt += 1;
        nefis_errno   = 8026;
        sprintf(error_text,
          "Unable to close definition file \'%s\'",
           nefis[set].def_name);
        return nefis_errno;
      }
    }
  }
  else if (nefis[set].one_file == TRUE && nefis[set].daf_fds != -1 )
  {
    if ( nefis_flush == TRUE )
    {
      nefis_errno = GP_flush_hash( nefis[set].daf_fds, set);
      if ( nefis_errno != 0 )
      {
        return nefis_errno;
      }
    }

    error = FILE_CLOSE( nefis[set].daf_fds );

    if ( error != 0 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 8027;
      sprintf(error_text,
        "Unable to close DefinitionData file \'%s\'",
         nefis[set].def_name);
      return nefis_errno;
    }
  }
/*
 * Free the retrieve_var array if all nefis file set are closed
 * and set first_retrieve TRUE
 */
  clean_up = TRUE;
  nefis[set].exist   = -1;
  for ( i=0; i<cap_nefis_files; i++ )
  {
    if ( nefis[i].exist != -1 )
    {
      clean_up = FALSE;
      break;
    }
    else
    {
        /* invalidate the pointers for this file set */
        for ( j=0; j<MAX_VAR_GROUPS ; j++ )
        {
          retrieve_var[i][j][0] = (BUInt8) BUINT8_MAX;
        }
    }
  }

  if ( clean_up == TRUE &&
       ( nefis[set].dat_fds != -1 ||
         nefis[set].daf_fds != -1    ) )
  {
    for ( i=0; i<cap_nefis_files; i++ )
    {
        for ( j=0; j<MAX_VAR_GROUPS ; j++ )
        {
            free ((BData) retrieve_var[i][j] );
            retrieve_var[i][j] = NULL;
        }
        free ( (BData) retrieve_var[i] );
        retrieve_var[i] = NULL;
        /*
         * Free the link-list retrieve pointers
         */
        if ( array_retrieve_ptrs[i] != NULL )
        {
          array_retrieve_ptrs[i] = free_retrieve_ptrs( array_retrieve_ptrs[i] );
        }
    }
    free ( (BData) retrieve_var);
    retrieve_var = NULL;
    first_retrieve = TRUE;
    nr_nefis_files  = 0;
    cap_nefis_files = 0;
  }
/*
 * Release file descriptor
 */
  strcpy(nefis[set].dat_name, "");
  strcpy(nefis[set].def_name, "");
  strcpy(nefis[set].daf_name, "");
  nefis[set].array_retrieve_ptrs = FALSE;
  nefis[set].dat_fds = -1;
  nefis[set].def_fds = -1;
  nefis[set].daf_fds = -1;
  dat_fds            = -1;
  def_fds            = -1;
  daf_fds            = -1;
  *fd_nefis          = -1;

  return nefis_errno;
}
/*===================================================================*/
BInt4 OC_close_all_nefis_files(void)
{
	BInt4 nefis_errno;
	BInt4 i;
	BInt4 set;

	nefis_errno = 0;
	for (i=0; i<MAX_NEFIS_FILES; i++)
	{
		if (nefis_errno!= 0) break;
		if (nefis[i].exist == i) 
		{
			set = i; // copy because do not change loop variable
			nefis_errno = close_nefis_files(&set);
		}
	}
	return nefis_errno;
}
/*===================================================================*/

BInt4 open_nefis_file( BText   file_name  ,
                       BChar   access_type,
                       BInt4 * p_fds      ,
                       BInt4 * new_files  )
/*
 * Function determines file_descriptor (fds)
 */
{
  BInt4   fds    = -1;
  BInt4   acType = -1;

  access_type = (BChar) tolower( (BInt4) access_type );

/*
 * Determine if the file should be created and/or flushed (NEFIS hash buffer)
 */
  if ( ( access_type != 'c' ) &&
       ( access_type != 'u' ) &&
       ( access_type != 'r' )   )
  {
      nefis_errcnt += 1;
      nefis_errno   = 8029;
      sprintf(error_text,
        "File \'%s\' can not be opened with unsupported NEFIS access type \'%c\'",
       file_name, access_type);
      return nefis_errno;
  }

  /*
   * Open the file read-only. If that does not work, it is assumed
   * the file does not exist. This is not completely correct.
   * It might be that the user has no access!!
   */
  fds = FILE_OPEN( file_name, FILE_READ_ONLY );
  if ( fds == -1 )
  {
/*
 *  File does not exist, access_type to create file only with
 *  apppropriate access_type
 */
    if ( ( access_type == 'c' ) || ( access_type == 'u' ) )
    {
/*
 *    create the file
 */
      acType      = FILE_CREATE;
      nefis_flush = TRUE;
      *new_files  = TRUE;
    }
    else
    {
/*
 *    only read the file
 */
      nefis_flush   = FALSE;
      *new_files    = FALSE;
      nefis_errcnt += 1;
      nefis_errno   = 8031;
      sprintf(error_text,
        "File \'%s\' can not be opened as read only\n --- %s", file_name, strerror(errno) );
    }
  }
  else
  {
/*
 *  File does exist
 */
    (BVoid)FILE_CLOSE( fds );
    fds   = -1;
    if ( access_type == 'u' )
    {
/*
 *    update existing file (read and write, random access)
 */
      acType      = FILE_READ_WRITE;
      nefis_flush = TRUE;
      *new_files  = FALSE;
    }
    else if ( access_type == 'r' )
    {
/*
 *    only read the file
 */
      acType      = FILE_READ_ONLY;
      nefis_flush = FALSE;
      *new_files  = FALSE;
    }
    else if ( access_type == 'c' )
    {
/*
 *    remove existing file and create new file
 */
      acType      = FILE_CREATE;
      nefis_flush = TRUE;
      *new_files  = TRUE;
    }
    else
    {
      nefis_errcnt += 1;
      nefis_errno   = 8029;
      sprintf(error_text,
        "File \'%s\' cann't be opened with unsupported NEFIS access type \'%c\'\n --- %s",
       file_name ,access_type, strerror(errno));
    }
  }

  if ( nefis_errno == 0 )
  {
/*
 * open file with appropriate access type
 */
    if ( fds == -1 )
    {
        if ( acType == FILE_CREATE )
        {
            fds   = FILE_OPEN( file_name, acType, FILE_MODE );
        }
        else
        {
            fds   = FILE_OPEN( file_name, acType );
        }
    }
    if ( fds == -1 )
    {
      nefis_errcnt += 1;
      nefis_errno   = 8030;
      sprintf(error_text,
        "Cannot open file \'%s\' for access type \'%c\'\n --- %s",
        file_name, access_type, strerror(errno));
    }
  }

  *p_fds = fds;

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Detect version of the NEFIS file
 */
BInt4 detect_version( BText file_header, File_Version * file_version )
{
    BInt4 error = 0;

    if ( strstr( file_header, "Versie 1.") )
    {
        *file_version = Version_1;
    }
    else if ( strstr( file_header, "File; Version 4.") )
    {
        *file_version = Version_1;
    }
    else if ( strstr( file_header, "File; 5.") || strstr( file_header, "NEFIS Version 5."))
    {
        *file_version = Version_5;
    }
    else
    {
        error = 1;
    }
    return error;
}
/*==========================================================================*/
/*
 * Reset version of the NEFIS file
 */
BInt4 OC_reset_file_version( BInt4 set, BInt4 file_version )
{
    BInt4 error = 0;

    if ( file_version==3 )
    {
        nefis[set].file_version = Version_1;
    }
    else if ( file_version==5 ) 
    {
        nefis[set].file_version = Version_5;
    }
    else
    {
        error = 1;
    }
    return error;
}
/*==========================================================================*/
/*
 * Get the version string from the NEFIS library
 */
BInt4 OC_get_version ( BText * nefis_version)
{
   *nefis_version = getfullversionstring_nefis();
   return 0;
}
/*==========================================================================*/
/*
 *   Debug function to print the call tree
 */
BInt4 DBG_nefis_tree ( BInt4  in_out,
                       BText  func_name)
{
  BInt4   i;

  if ( first_tree == TRUE )
  {
    first_tree = FALSE;
    strcpy (tree_name, "NEFIS");
  }

  if ( in_out == 1 )
  {
    strcat( tree_name,":");
    strcat( tree_name,func_name);
    strcat( tree_name,"\0");
    printf("Start %s\n",tree_name);
  }
  else
  {
    printf("Exit  %s\n",tree_name);
    i = (BInt4)strlen(tree_name);
    while( tree_name[i] != ':' )
    {
      tree_name[i] = '\0';
      i--;
    }
    tree_name[i] = '\0';
  }

  return nefis_errno;
}

/*==========================================================================*/
BInt4 endianness( void )
{
   long int i = 1;
   const char *p = (const char *) &i;
   if (p[0] == 1)  /* Lowest address contains the least significant byte */
   {
      return 0; /* LITTLE_ENDIAN */
   }
   else
   {
      return 1; /* BIG_ENDIAN; */
   }
}

/*==========================================================================*/
BInt4 setConversionNewFile( BChar coding, BText hdr, BUInt4 length)
{
    BInt4 retval = FALSE ; /* default no conversion */
    if (coding != 'L' && coding != 'B')
    {
        hdr[length-1] = 'L';
        if (endianness() == 0)
        {
            retval = FALSE ;
        }
        else if (endianness() == 1)
        {
            retval = TRUE ; /* convert to little endian */
        }
    }
    else
    {
        hdr[length-1] = coding;
        if (endianness() == 0 && coding == 'B' ||
            endianness() == 1 && coding == 'L')
        {
            retval = TRUE ; /* convert */
        }
        else if (endianness() == 0 && coding == 'L' ||
                 endianness() == 1 && coding == 'B')
        {
            retval = FALSE ;
        }
    }
    return retval;
}

/*==========================================================================*/
BInt4 setConversionExistingFile( BInt4 set, BChar coding, BText hdr, BUInt4 length)
{
    BInt4 retval = FALSE ; /* default no conversion */

    if (nefis[set].file_version == Version_5)
    {
        if ( coding != 'L' && coding != 'B' )
        {
            if (endianness() == 0 && hdr[length-1] == 'L') /* Neutral == Little endian architecture and Little endian file */
            {
                retval = FALSE;
            }
            else if (endianness() == 1 && hdr[length-1] == 'L') /* Neutral == Big endian architecture and Little endian file */
            {
                retval = TRUE; /* convert */
            }
            if (endianness() == 0 && hdr[length-1] == 'B') /* Neutral == Little endian architecture and Big endian file */
            {
                retval = TRUE;
            }
            else if (endianness() == 1 && hdr[length-1] == 'B') /* Neutral == Big endian architecture and Big endian file */
            {
                retval = FALSE;
            }
        }
        else
        {
            if (endianness() == 0 && hdr[length-1] == 'L' ||
                endianness() == 1 && hdr[length-1] == 'B')
            {
                retval = FALSE;
            }
            else if (endianness() == 0 && hdr[length-1] == 'B' ||
                     endianness() == 1 && hdr[length-1] == 'L')
            {
                retval = TRUE; /* convert */
            }
        }
    }
    else if (nefis[set].file_version == Version_1)
    {
        if ( coding != 'N' && coding != 'B' )
        {
            if (endianness() == 1 && hdr[length-1] == 'N')      /* Neutral == Big endian architecture and Big endian file */
            {
                retval = FALSE; /* conversion */
            }
            else if (endianness() == 0 && hdr[length-1] == 'N') /* Neutral == Little endian architecture and Big endian file*/
            {
                retval = TRUE; /* conversion */
            }
            else if (hdr[length-1] == 'B')
            {
                retval = FALSE;
            }
        }
        else
        {
            if (endianness() == 1 && coding == 'N' )     /* Neutral == Big endian architecture and convert to Big endian*/
            {
                retval = FALSE;
            }
            else if (endianness() == 0 && coding == 'N') /* Neutral == Little endian architectur and convert to Big endian */
            {
                retval = TRUE;
            }
            else if (coding == 'B')  /* Binairy: Read file as binairy, no conversion is done */
            {
                retval = FALSE;
            }
        }
    }
    return retval;
}
/*==========================================================================*/
BUInt8 *** new_capacity_retrieve_var( BUInt8 *** retrieve, BInt4 length, BInt4 new_length)
{
    long i, j;

    retrieve = (BUInt8***) realloc(retrieve, (new_length)*sizeof(BUInt8));
    for (i=length; i<new_length; i++)  {
        retrieve[i] = (BUInt8 **) malloc(MAX_VAR_GROUPS * sizeof(BUInt8) );
    }
    for (j=0; j<MAX_VAR_GROUPS; j++)  {
        for (i=length; i<new_length; i++)  {
            retrieve[i][j] = (BUInt8 *) malloc(1 * sizeof(BUInt8) );
            retrieve[i][j][0] = (BUInt8) BUINT8_MAX;
        }
    }

    return retrieve;
}
