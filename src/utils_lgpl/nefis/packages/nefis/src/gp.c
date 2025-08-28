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
/*
 *   <gp.c> - Read and write items to NEFIS data and definiton file
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
 */
#if defined(linux)
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>
#include <time.h>

#if defined(_WIN32)
#include <io.h>
#elif defined(salford32)
#include <io.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

#include "btps.h"
#include "nefis.h" /* needed for definition of LENGTH_ERROR_MESSAGE */
#include "nef-def.h"
#include "gp.h"

#if defined(_WIN32)
#define FILE_READ _read
#define FILE_SEEK _lseeki64
#define FILE_WRITE _write
#elif defined(linux)
#define FILE_READ read
#define FILE_SEEK lseek
#define FILE_WRITE write
#else
#define FILE_READ FILE_READ_not_defined
#define FILE_SEEK FILE_SEEK_not_defined
#define FILE_WRITE FILE_WRITE_not_defined
#endif

#define A2D(m, n) ((n) + DEFINE_STEPS * (m))

extern BInt4 HS_check_ecg(BInt4, BInt4, BText, BUInt8, BUInt8 *,
                          BInt4, BUInt8 *, BUInt4 *, BUInt4 *);
extern BInt4 HS_get_cont_dat(BInt4, BUInt8, BUInt8 *, BText, BText);
extern BInt4 HS_get_cont_cel(BInt4, BUInt8, BUInt8 *, BText, BText *,
                             BUInt4 *, BUInt8 *);
extern BInt4 HS_get_cont_elm(BInt4, BUInt8, BUInt8 *, BText, BText,
                             BText, BText, BText, BUInt4 *, BUInt4 *,
                             BUInt4 *, BUInt8 *);
extern BInt4 HS_get_cont_grp(BInt4, BUInt8, BUInt8 *, BText, BText,
                             BUInt4 *, BUInt4 *, BUInt4 *);
extern BInt4 convert_ieee(BData *, BText *, BUInt8, BInt4, BText,
                          BInt4);
/*
 * Static prototypes
 */
static BVoid Swap(BUInt8 *, BInt4, BInt4);
static BVoid QuickSort(BUInt8 *, BInt4, BInt4);
static void FileSeek(BInt4, BUInt8);

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function sets the read/write pointer in a nefis file
 *
 *  input : fds         file handle
 *          startPos    start address on nefis file
 */
/*==========================================================================*/
static void FileSeek(
    BInt4 fds,
    BUInt8 startPos)
{
  BInt8 retVal;
  BUInt8 rest;

  /* long domain (values >= 0) is a subset of unsigned long domain.
   * Since lseek expects a long for the position parameter and startPos
   * (which is an unsigned long) may contain a value too large for
   * a long, we have to call lseek in a loop to reach position startPos.
   */

  retVal = FILE_SEEK(fds, 0, SEEK_SET);
  assert(retVal != -1);
  rest = startPos;
  while (rest > NEF_SEEK_MAX)
  {
    retVal = FILE_SEEK(fds, NEF_SEEK_MAX, SEEK_CUR);
    rest -= NEF_SEEK_MAX;
  }
  retVal = FILE_SEEK(fds, rest, SEEK_CUR);
  assert(retVal != -1);
}

/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read several bytes from a nefis file
 *
 *  input : file        file handle
 *          string      character string to read
 *          start       start address on nefis file
 *          n_bytes     number of bytes to read
 *  output  string      read character string
 *  return: n_read      number bytes read
 */
/*==========================================================================*/
BUInt8 GP_read_file(BInt4 fds,
                    BText string,
                    BUInt8 start,
                    BUInt8 n_bytes)
{
  BUInt8 n_read = 0;
  BUInt4 tmp_bytes;

  if (fds != -1)
  {
    FileSeek(fds, start);

    tmp_bytes = (BUInt4)n_bytes;
    n_read = (BUInt8)FILE_READ(fds, (BData)string, tmp_bytes);
  }

  return n_read;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read several bytes from a nefis file
 *      depending on the version number the number of bytes to read
 *      have to be divided by two
 *
 *  input : file        file handle
 *          string      character string to read
 *          start       start address on nefis file
 *          n_bytes     number of bytes to read
 *          set         nefis file
 *  output  string      read character string
 *  return: n_read      number bytes read
 */
/*==========================================================================*/
BUInt8 GP_read_file_pointers(BInt4 fds,
                             BText string,
                             BUInt8 start,
                             BUInt8 n_bytes,
                             BInt4 set)
{
  BUInt8 n_read = 0;

  if (nefis[set].file_version == Version_1)
  {
    n_read = GP_read_file(fds, string, start, n_bytes / 2);
    n_read = 2 * n_read;
    if (nefis[set].daf_neutral == TRUE ||
        nefis[set].dat_neutral == TRUE ||
        nefis[set].def_neutral == TRUE)
    {
      BUInt4 tmp_bytes;
      tmp_bytes = (BUInt4)n_bytes / 2;
      MCR_shift_string_forward(string, tmp_bytes, tmp_bytes);
      tmp_bytes = (BUInt4)n_bytes / 8;
      MCR_B_from_int4_to_int8((BInt4 *)string, tmp_bytes);
    }
    else
    {
      BUInt4 tmp_bytes;
      tmp_bytes = (BUInt4)n_bytes / 2;
      MCR_shift_string_forward(string, tmp_bytes, tmp_bytes);
      tmp_bytes = (BUInt4)n_bytes / 8;
      MCR_L_from_int4_to_int8((BInt4 *)string, tmp_bytes);
    }
  }
  else
  {
    n_read = GP_read_file(fds, string, start, n_bytes);
  }

  return n_read;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function writes several bytes to a nefis file
 *
 *  input : file        file handle
 *          string      character string to write
 *          start       start address on nefis file
 *          n_bytes     number of bytes to write
 *  output:             none
 *  return: n_written   number bytes written to nefis file
 */
/*==========================================================================*/
BUInt8 GP_write_file(BInt4 fds,
                     BText string,
                     BUInt8 start,
                     BUInt8 n_bytes)
{
  BUInt8 n_written;
  BUInt8 rest;
  BUInt4 tmp_rest;

  n_written = 0;
  if (fds != -1)
  {
    FileSeek(fds, start);

    rest = n_bytes;
    while (rest > UINT_MAX)
    {
      n_written += (BUInt8)FILE_WRITE(fds, string, UINT_MAX);
      rest -= UINT_MAX;
    }
    tmp_rest = (BUInt4)rest;
    n_written += (BUInt8)FILE_WRITE(fds, string, tmp_rest);
    rest -= rest;
  }

  return n_written;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function write several bytes to a nefis file
 *      depending on the version number the number of bytes to write
 *      have to be divided by two
 *
 *  input : file        file handle
 *          string      character string to write
 *          start       start address on nefis file
 *          n_bytes     number of bytes to write
 *          set         nefis file
 *  output:             none
 *  return: n_written   number bytes written to nefis file
 */
/*==========================================================================*/
BUInt8 GP_write_file_pointers(BInt4 fds,
                              BText string,
                              BUInt8 start,
                              BUInt8 n_bytes,
                              BInt4 set)
{
  BUInt8 n_written = 0;

  if (nefis[set].file_version == Version_1)
  {
    if (nefis[set].daf_neutral == TRUE ||
        nefis[set].dat_neutral == TRUE)
    {
      MCR_B_from_int8_to_int4((BInt4 *)string, n_bytes / 8);
    }
    else
    {
      MCR_L_from_int8_to_int4((BInt4 *)string, n_bytes / 8);
    }
    n_written = GP_write_file(fds, string, start, n_bytes / 2);
    n_written *= 2;
  }
  else
  {
    n_written = GP_write_file(fds, string, start, n_bytes);
  }
  return n_written;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function write the file description to a NEFIS file
 *
 *  input : file        file handle
 *          set         nefis file set descriptor
 *  return: 0           no error occured
 *          !=0         error occured
 *  errno : -7011
 *          -8011
 */
/*==========================================================================*/
BInt4 GP_flush_hash(BInt4 fds,
                    BInt4 set)
{
  BText cp;
  BInt4 daf_fds;
  BInt4 dat_fds;
  BInt4 def_fds;
  BInt4 from_xdr;
  BUInt8 n_written;
  BData vp;

  daf_fds = nefis[set].daf_fds;
  dat_fds = nefis[set].dat_fds;
  def_fds = nefis[set].def_fds;

  /*
   * Data file
   */
  if (fds == dat_fds && nefis[set].one_file == FALSE)
  {
    if (nefis[set].dat_neutral == TRUE)
    {
      from_xdr = 0;
      vp = (BData)&nefis[set].dat.fds[1];
      cp = (BText)malloc(sizeof(BUInt8) * (LHSHDT + 1));

      nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * (LHSHDT + 1),
                                 SIZE_BINT8, "INTEGER", from_xdr);

      n_written = GP_write_file_pointers(dat_fds, cp, LHDRDT, SIZE_BINT8 * (LHSHDT + 1), set);
      free((BData)cp);
    }
    else
    {
      n_written = GP_write_file_pointers(dat_fds, (BText)&nefis[set].dat.fds[1],
                                         LHDRDT, SIZE_BINT8 * (LHSHDT + 1), set);
    }
    if (n_written != SIZE_BINT8 * (LHSHDT + 1))
    {
      nefis_errcnt += 1;
      nefis_errno = 6001;
      sprintf(error_text,
              "Hashtable not written to data file \'%s\'",
              nefis[set].dat_name);
      return nefis_errno;
    }
  }

  /*
   * Definition file
   */
  if ((fds == def_fds) && (nefis[set].one_file == FALSE))
  {
    if (nefis[set].def_neutral == TRUE)
    {
      from_xdr = 0;
      vp = (BData)&nefis[set].def.fds[1];
      cp = (BText)malloc(sizeof(BUInt8) * (LHSHDF + 1));

      nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * (LHSHDF + 1),
                                 SIZE_BINT8, "INTEGER", from_xdr);

      n_written = GP_write_file_pointers(def_fds, cp, LHDRDF, SIZE_BINT8 * (LHSHDF + 1), set);
      free((BData)cp);
    }
    else
    {
      n_written = GP_write_file_pointers(def_fds, (BText)&nefis[set].def.fds[1],
                                         LHDRDF, SIZE_BINT8 * (LHSHDF + 1), set);
    }
    if (n_written != SIZE_BINT8 * (LHSHDF + 1))
    {
      nefis_errcnt += 1;
      nefis_errno = 6002;
      sprintf(error_text,
              "Hashtable not written to definition file \'%s\'",
              nefis[set].def_name);
      return nefis_errno;
    }
  }

  /*
   * DefinitionData file
   */
  if ((fds == daf_fds) && (nefis[set].one_file == TRUE))
  {
    if (nefis[set].daf_neutral == TRUE)
    {
      from_xdr = 0;
      vp = (BData)&nefis[set].daf.fds[1];
      cp = (BText)malloc(sizeof(BUInt8) * (LHSHDAF + 1));

      nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * (LHSHDAF + 1),
                                 SIZE_BINT8, "INTEGER", from_xdr);

      n_written = GP_write_file_pointers(daf_fds, cp, LHDRDAF, SIZE_BINT8 * (LHSHDAF + 1), set);
      free((BData)cp);
    }
    else
    {
      n_written = GP_write_file_pointers(daf_fds, (BText)&nefis[set].daf.fds[1],
                                         LHDRDAF, SIZE_BINT8 * (LHSHDAF + 1), set);
    }
    if (n_written != SIZE_BINT8 * (LHSHDAF + 1))
    {
      nefis_errcnt += 1;
      nefis_errno = 6002;
      sprintf(error_text,
              "Hashtable not written to DefinitionData file \'%s\'",
              nefis[set].daf_name);
      return nefis_errno;
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read  the cel definition on a NEFIS file
 *
 *  input : set           nefis file set descriptor
 *  output: cel_name      cel name
 *          cel_num_dim   number of elements in this cel
 *          elm_names     element names
 *          cel_num_bytes number of bytes occupied by this cel
 *  return: 0             no error occured
 *          !=0           error occured
 *  errno :
 *
 */
/*==========================================================================*/
BInt4 GP_inquire_cel(BInt4 set,
                     BText cel_name,
                     BUInt4 *cel_num_dim,
                     BText *elm_names,
                     BUInt8 *cel_num_bytes)
{
  BInt4 fds_file;
  BUInt4 new_entry;
  BUInt8 next_pointer = 0;
  BUInt8 pnt_hash;
  BUInt4 pnt_bucket;

  if (nefis[set].one_file == TRUE)
  {
    fds_file = nefis[set].daf_fds;
    nefis_errno = HS_check_ecg(set, fds_file, cel_name, NIL,
                               &nefis[set].daf.fds[2 + LHSHEL],
                               LHSHCL, &pnt_hash, &pnt_bucket, &new_entry);
  }
  else
  {
    fds_file = nefis[set].def_fds;
    nefis_errno = HS_check_ecg(set, fds_file, cel_name, NIL,
                               &nefis[set].def.fds[2 + LHSHEL],
                               LHSHCL, &pnt_hash, &pnt_bucket, &new_entry);
  }

  if (new_entry == 1)
  {
    nefis_errcnt += 1;
    nefis_errno = 6003;
    sprintf(error_text,
            "Cell \'%s\' does not exist in definition file", cel_name);
  }

  if (nefis_errno == 0)
  {
    nefis_errno = HS_get_cont_cel(set, pnt_hash, &next_pointer,
                                  cel_name, elm_names, cel_num_dim,
                                  cel_num_bytes);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read  the group name on a data file and
 *      returns the pointer where the data group starts and
 *      group definition on the definition file
 */
/*==========================================================================*/
BInt4 GP_inquire_dat(BInt4 set,
                     BUInt8 *grp_pointer,
                     BText grp_name,
                     BText grp_defined)
{
  BInt4 fds_file;
  BUInt4 new_entry = (BUInt4)-1;
  BUInt8 next_pointer = NIL;
  BUInt8 pnt_hash;
  BUInt4 pnt_bucket;

  if (nefis[set].one_file == TRUE)
  {
    fds_file = nefis[set].daf_fds;
    nefis_errno = HS_check_ecg(set, fds_file, grp_name, NIL,
                               &nefis[set].daf.fds[2 + LHSHEL + LHSHCL + LHSHGR],
                               LHSHDT, &pnt_hash, &pnt_bucket, &new_entry);
  }
  else
  {
    fds_file = nefis[set].dat_fds;
    nefis_errno = HS_check_ecg(set, fds_file, grp_name, NIL,
                               &nefis[set].dat.fds[2],
                               LHSHDT, &pnt_hash, &pnt_bucket, &new_entry);
  }

  *grp_pointer = pnt_hash;

  if (new_entry == 1)
  {
    nefis_errcnt += 1;
    nefis_errno = 6004;
    sprintf(error_text,
            "Group \'%s\' does not exist in data file\n", grp_name);
    return nefis_errno;
  }

  if (nefis_errno == 0)
  {
    nefis_errno = HS_get_cont_dat(set,
                                  pnt_hash, &next_pointer,
                                  grp_name, grp_defined);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read  the element definition on a NEFIS file
 */
/*==========================================================================*/
BInt4 GP_inquire_elm(BInt4 set,
                     BText elm_name,
                     BText elm_type,
                     BText elm_quantity,
                     BText elm_unity,
                     BText elm_desc,
                     BUInt4 *elm_num_dim,
                     BUInt4 *elm_dimens,
                     BUInt4 *elm_single_bytes,
                     BUInt8 *elm_num_bytes)
{
  BInt4 fds_file;
  BUInt4 new_entry;
  BUInt8 next_pointer = 0;
  BUInt4 pnt_bucket;
  BUInt8 pnt_hash;

  if (nefis[set].one_file == TRUE)
  {
    fds_file = nefis[set].daf_fds;
    nefis_errno = HS_check_ecg(set, fds_file, elm_name, NIL,
                               &nefis[set].daf.fds[2],
                               LHSHEL, &pnt_hash, &pnt_bucket, &new_entry);
  }
  else
  {
    fds_file = nefis[set].def_fds;
    nefis_errno = HS_check_ecg(set, fds_file, elm_name, NIL,
                               &nefis[set].def.fds[2],
                               LHSHEL, &pnt_hash, &pnt_bucket, &new_entry);
  }

  if (new_entry == 1)
  {
    nefis_errcnt += 1;
    nefis_errno = 6005;
    sprintf(error_text,
            "Element \'%s\' does not exist in definition file\n",
            elm_name);
  }

  if (nefis_errno == 0)
  {
    nefis_errno = HS_get_cont_elm(set, pnt_hash, &next_pointer,
                                  elm_name, elm_type, elm_quantity,
                                  elm_unity, elm_desc, elm_num_dim,
                                  elm_dimens, elm_single_bytes, elm_num_bytes);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function read  the group definition on a NEFIS definition file
 */
/*==========================================================================*/
BInt4 GP_inquire_grp_def(BInt4 set,
                         BText grp_defined,
                         BText cel_name,
                         BUInt4 *grp_num_dim,
                         BUInt4 *grp_dimens,
                         BUInt4 *grp_order)
{
  BInt4 def_fds;
  BUInt4 new_entry;
  BUInt8 next_pointer;
  BUInt4 pnt_bucket;
  BUInt8 pnt_hash;

  /*
   *     file is a definition file
   */
  if (nefis[set].one_file == TRUE)
  {
    def_fds = nefis[set].daf_fds;
    nefis_errno = HS_check_ecg(set, def_fds, grp_defined, NIL,
                               &nefis[set].daf.fds[2 + LHSHEL + LHSHCL],
                               LHSHGR, &pnt_hash, &pnt_bucket, &new_entry);
  }
  else
  {
    def_fds = nefis[set].def_fds;
    nefis_errno = HS_check_ecg(set, def_fds, grp_defined, NIL,
                               &nefis[set].def.fds[2 + LHSHEL + LHSHCL],
                               LHSHGR, &pnt_hash, &pnt_bucket, &new_entry);
  }

  if (new_entry == 1)
  {
    nefis_errcnt += 1;
    nefis_errno = 6007;
    sprintf(error_text,
            "Group \'%s\' does not exist in definition file\n",
            grp_defined);
  }
  if (nefis_errno == 0)
  {
    nefis_errno = HS_get_cont_grp(set, pnt_hash, &next_pointer,
                                  grp_defined, cel_name, grp_num_dim,
                                  grp_dimens, grp_order);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *    - C equivalence of fortran INQDAT
 *    - This function read  the group name on a data file and returns
 *      group definition on the defintion file
 */
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function write the integer attribute to the data file
 */
/*==========================================================================*/
BInt4 GP_put_attribute(BInt4 set,
                       BText grp_name,
                       BText att_name,
                       BData att_value,
                       BText att_type)
{
  union data_put
  {
    BChar st[SIZE_DAT_BUF];
    BInt4 in[SIZE_DAT_BUF / SIZE_BINT4];
    BRea4 re[SIZE_DAT_BUF / SIZE_BINT4];
  } dat_buf;

  BInt4 fds;
  BInt4 from_xdr;
  BInt4 index;
  BInt4 shift_to_name;
  BInt4 shift_to_value;
  BUInt8 grp_pointer;
  BInt4 i;
  BInt4 j = -1;
  BUInt8 n_read = 0;
  BRea4 *f_value;
  BChar a_name[MAX_NAME + 1];
  BText cp;
  BData vp;

  if (nefis[set].one_file == TRUE)
  {
    fds = nefis[set].daf_fds;
  }
  else
  {
    fds = nefis[set].dat_fds;
  }

  a_name[MAX_NAME] = '\0';

  nefis_errno = GP_inquire_dat(set, &grp_pointer, grp_name, a_name);

  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  n_read = GP_read_file(fds, dat_buf.st, grp_pointer, (BUInt8)SIZE_DAT_BUF);
  if (n_read > (BUInt8)SIZE_DAT_BUF)
  {
    nefis_errcnt += 1;
    nefis_errno = 6009;
    sprintf(error_text,
            "On reading attribute of group \'%s\'",
            grp_name);
    return nefis_errno;
  }

  /*
   * Integer attribute
   */

  if (strncmp(att_type, "INTEGER", 7) == 0)
  {
    shift_to_name = 3 * SIZE_BINT8 + 2 * MAX_NAME;
    shift_to_value = shift_to_name + MAX_DIM * MAX_NAME;
    for (i = 0; i < MAX_DIM; i++)
    {
      if ((strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, MAX_NAME) == 0) ||
          (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, "      ", 6) == 0))
      {
        j = i;
        break;
      }
    }
    if (j == -1)
    {
      nefis_errcnt += 1;
      nefis_errno = 6010;
      sprintf(error_text,
              "No space left in data file for integer attribute of group \'%s\'",
              grp_name);
      return nefis_errno;
    }

    strncpy(dat_buf.st + shift_to_name + MAX_NAME * j, att_name, MAX_NAME);
    index = shift_to_value / SIZE_BINT4;
    dat_buf.in[index + j] = *((BInt4 *)att_value);

    if (nefis[set].daf_neutral == TRUE ||
        nefis[set].dat_neutral == TRUE)
    {
      from_xdr = 0;
      vp = (BData)&dat_buf.in[index + j];
      cp = (BText)malloc(sizeof(BInt4) * 1);

      nefis_errno = convert_ieee(&vp, &cp, 1 * SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
      dat_buf.in[31 + j] = *((BUInt4 *)cp);
      free((BData)cp);
    }
    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_name + MAX_NAME * j,
                          grp_pointer + shift_to_name + MAX_NAME * j, MAX_NAME);
    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_value + SIZE_BINT4 * j,
                          grp_pointer + shift_to_value + SIZE_BINT4 * j, SIZE_BINT4);
  }

  /*
   * Real attribute
   */
  if (strncmp(att_type, "REAL", 4) == 0)
  {
    shift_to_name = 3 * SIZE_BINT8 + 2 * MAX_NAME + MAX_DIM * (MAX_NAME + SIZE_BINT4);
    shift_to_value = 3 * SIZE_BINT8 + 2 * MAX_NAME + MAX_DIM * (MAX_NAME + SIZE_BINT4) + MAX_DIM * MAX_NAME;
    for (i = 0; i < MAX_DIM; i++)
    {
      if ((strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, MAX_NAME) == 0) ||
          (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, "      ", 6) == 0))
      {
        j = i;
        break;
      }
    }
    if (j == -1)
    {
      nefis_errcnt += 1;
      nefis_errno = 6011;
      sprintf(error_text,
              "No space left in data file for real attribute of group \'%s\'",
              grp_name);
      return nefis_errno;
    }

    strncpy(dat_buf.st + shift_to_name + MAX_NAME * j, att_name, MAX_NAME);
    f_value = (BRea4 *)att_value;
    index = shift_to_value / SIZE_BINT4;
    dat_buf.re[index + j] = *f_value;

    if (nefis[set].daf_neutral == TRUE ||
        nefis[set].dat_neutral == TRUE)
    {
      from_xdr = 0;
      vp = (BData)&dat_buf.re[index + j];
      cp = (BText)malloc(SIZE_BINT4 * 1);

      nefis_errno = convert_ieee(&vp, &cp, 1 * SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
      dat_buf.re[56 + j] = *((BRea4 *)cp);
      free((BData)cp);
    }
    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_name + MAX_NAME * j,
                          grp_pointer + shift_to_name + MAX_NAME * j, MAX_NAME);
    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_value + SIZE_BINT4 * j,
                          grp_pointer + shift_to_value + SIZE_BINT4 * j, SIZE_BINT4);
  }

  /*
   * Character attribute
   */

  if (strncmp(att_type, "CHARACTE", 8) == 0)
  {
    shift_to_name = 3 * SIZE_BINT8 + 2 * MAX_NAME + 2 * MAX_DIM * (MAX_NAME + SIZE_BINT4);
    shift_to_value = 3 * SIZE_BINT8 + 2 * MAX_NAME + 2 * MAX_DIM * (MAX_NAME + SIZE_BINT4) + MAX_DIM * MAX_NAME;
    for (i = 0; i < MAX_DIM; i++)
    {
      if ((strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, MAX_NAME) == 0) ||
          (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, "      ", 6) == 0))
      {
        j = i;
        break;
      }
    }
    if (j == -1)
    {
      nefis_errcnt += 1;
      nefis_errno = 6012;
      sprintf(error_text,
              "No space left in data file for string attribute of group \'%s\'",
              grp_name);
      return nefis_errno;
    }
    strncpy(dat_buf.st + shift_to_name + MAX_NAME * j, att_name, MAX_NAME);
    strncpy(dat_buf.st + shift_to_value + MAX_NAME * j, (BText)att_value, MAX_NAME);

    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_name + MAX_NAME * j,
                          grp_pointer + shift_to_name + MAX_NAME * j, MAX_NAME);
    (BVoid) GP_write_file(fds, dat_buf.st + shift_to_value + MAX_NAME * j,
                          grp_pointer + shift_to_value + MAX_NAME * j, MAX_NAME);
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 *  input : set         nefis file set desciptor
 *          find        given  look for specific attribute (att_name := given)
 *                      first  look for first attribute (att_name := empty)
 *                      next   look for next attribute
 *          grp_name    group name on data file
 *          att_name    attribute name
 *  output: att_value   attribute value (either int, BRea4 or string)
 *  return: 0           no error occured
 *          1           error occured
 */
BInt4 GP_get_attribute(BInt4 set,
                       BInt4 find,
                       BText grp_name,
                       BText att_name,
                       BData att_value,
                       BText att_type)
{
  static BInt4 previous_in = 0;
  static BInt4 previous_re = 0;
  static BInt4 previous_st = 0;

  union data_get
  {
    BChar st[SIZE_DAT_BUF];
    BInt4 in[SIZE_DAT_BUF / SIZE_BINT4];
    BRea4 re[SIZE_DAT_BUF / SIZE_BINT4];
  } dat_buf;

  BText cp;
  BInt4 fds;
  BInt4 from_xdr;
  BUInt8 grp_pointer;
  BInt4 i;
  BInt4 index;
  BInt4 j;
  BInt4 min_comp;
  BUInt8 n_read = 0;
  BUInt4 new_entry;
  BInt4 given = 0;
  BInt4 first = 1;
  BInt4 next = 2;
  BUInt4 pnt_bucket;
  BInt4 shift_to_name;
  BInt4 shift_to_value;
  BInt4 start;
  BInt4 size_int;
  BData vp;

  if (nefis[set].one_file == TRUE)
  {
    fds = nefis[set].daf_fds;
    nefis_errno = HS_check_ecg(set, fds, grp_name, NIL,
                               &nefis[set].daf.fds[2 + LHSHEL + LHSHCL + LHSHGR],
                               LHSHDT, &grp_pointer, &pnt_bucket, &new_entry);
  }
  else
  {
    fds = nefis[set].dat_fds;
    nefis_errno = HS_check_ecg(set, fds, grp_name, NIL,
                               &nefis[set].dat.fds[2],
                               LHSHDT, &grp_pointer, &pnt_bucket, &new_entry);
  }

  if (new_entry == 1)
  {
    nefis_errcnt += 1;
    nefis_errno = 6013;
    sprintf(error_text,
            "Group \'%s\' does not exist in data file\n", grp_name);
    return nefis_errno;
  }
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  n_read = GP_read_file(fds, dat_buf.st, grp_pointer, (BUInt8)SIZE_DAT_BUF);
  if (n_read > (BUInt8)SIZE_DAT_BUF)
  {
    nefis_errcnt += 1;
    nefis_errno = 6014;
    sprintf(error_text,
            "On reading attribute of group \'%s\'",
            grp_name);
    return nefis_errno;
  }

  min_comp = min(MAX_NAME, (BInt4)strlen(att_name));
  start = 0;
  if (nefis[set].file_version == Version_1)
  {
    size_int = SIZE_BINT4;
  }
  else
  {
    size_int = SIZE_BINT8;
  }

  /*
   * Integer attribute
   */

  if (strncmp(att_type, "INTEGER", 7) == 0)
  {
    shift_to_name = 3 * size_int + 2 * MAX_NAME;
    shift_to_value = 3 * size_int + 2 * MAX_NAME + MAX_DIM * MAX_NAME;
    j = -1;
    if (find == next)
    {
      start = previous_in + 1;
    }
    for (i = start; i < MAX_DIM; i++)
    {
      if (find == given)
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, min_comp) == 0)
        {
          j = i;
          previous_in = i;
          break;
        }
      }
      if ((find == first) ||
          (find == next))
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, " ", 1) != 0)
        {
          j = i;
          previous_in = i;
          strncpy(att_name, dat_buf.st + shift_to_name + MAX_NAME * i, MAX_NAME);
          break;
        }
      }
    }
    if (j == -1)
    {
      if (find == given)
      {
        nefis_errcnt += 1;
        nefis_errno = 6015;
        sprintf(error_text,
                "Integer attribute \'%s\' of group \'%s\'not found",
                att_name, grp_name);
      }
      else
      {
        nefis_errcnt += 1;
        nefis_errno = 6016;
        sprintf(error_text,
                "No valid attribute name found");
      }
    }
    else
    {
      if (nefis[set].daf_neutral == TRUE ||
          nefis[set].dat_neutral == TRUE)
      {
        from_xdr = 1;
        index = shift_to_value / SIZE_BINT4;
        vp = (BData)malloc(SIZE_BINT4 * 1);
        cp = (BText)&dat_buf.in[index + j];

        nefis_errno = convert_ieee(&vp, &cp, 1 * SIZE_BINT4, SIZE_BINT4, "INTEGER", from_xdr);
        for (i = 0; i < SIZE_BINT4; i++)
        {
          *(((BText)att_value) + i) = *(((BText)vp) + i);
        }
        free((BData)vp);
      }
      else
      {
        for (i = 0; i < SIZE_BINT4; i++)
        {
          *(((BText)att_value) + i) = *((BText)dat_buf.st + shift_to_value + SIZE_BINT4 * j + i);
        }
      }
    }
  }

  /*
   * Real attribute
   */

  if (strncmp(att_type, "REAL", 4) == 0)
  {
    shift_to_name = 3 * size_int + 2 * MAX_NAME + MAX_DIM * (MAX_NAME + SIZE_BINT4);
    shift_to_value = 3 * size_int + 2 * MAX_NAME + MAX_DIM * (MAX_NAME + SIZE_BINT4) + MAX_DIM * MAX_NAME;
    j = -1;
    if (find == next)
    {
      start = previous_re + 1;
    }
    for (i = start; i < MAX_DIM; i++)
    {
      if (find == given)
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, min_comp) == 0)
        {
          j = i;
          previous_re = i;
          break;
        }
      }
      if ((find == first) ||
          (find == next))
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, " ", 1) != 0)
        {
          j = i;
          previous_re = i;
          strncpy(att_name, dat_buf.st + shift_to_name + MAX_NAME * i, MAX_NAME);
          break;
        }
      }
    }

    if (j == -1)
    {
      if (find == given)
      {
        nefis_errcnt += 1;
        nefis_errno = 6017;
        sprintf(error_text,
                "Real attribute \'%s\' of group \'%s\'not found",
                att_name, grp_name);
      }
      else
      {
        nefis_errcnt += 1;
        nefis_errno = 6018;
        sprintf(error_text,
                "No valid attribute name found");
      }
    }
    else
    {
      if (nefis[set].daf_neutral == TRUE ||
          nefis[set].dat_neutral == TRUE)
      {
        from_xdr = 1;
        index = shift_to_value / SIZE_BINT4;
        vp = (BData)malloc(SIZE_BINT4 * 1);
        cp = (BText)&dat_buf.re[index + j];

        nefis_errno = convert_ieee(&vp, &cp, 1 * SIZE_BINT4, SIZE_BINT4, "REAL", from_xdr);
        for (i = 0; i < SIZE_BINT4; i++)
        {
          *(((BText)att_value) + i) = *(((BText)vp) + i);
        }
        free((BData)vp);
      }
      else
      {
        for (i = 0; i < SIZE_BINT4; i++)
        {
          *(((BText)att_value) + i) = *((BText)dat_buf.st + shift_to_value + SIZE_BINT4 * j + i);
        }
      }
    }
  }

  /*
   * Character attribute
   */

  if (strncmp(att_type, "CHARACTE", 8) == 0)
  {
    shift_to_name = 3 * size_int + 2 * MAX_NAME + 2 * MAX_DIM * (MAX_NAME + SIZE_BINT4);
    shift_to_value = 3 * size_int + 2 * MAX_NAME + 2 * MAX_DIM * (MAX_NAME + SIZE_BINT4) + MAX_DIM * MAX_NAME;
    j = -1;
    if (find == next)
    {
      start = previous_st + 1;
    }
    for (i = start; i < MAX_DIM; i++)
    {
      if (find == given)
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, att_name, min_comp) == 0)
        {
          j = i;
          previous_st = i;
          break;
        }
      }
      else if ((find == first) ||
               (find == next))
      {
        if (strncmp(dat_buf.st + shift_to_name + MAX_NAME * i, " ", 1) != 0)
        {
          j = i;
          previous_st = i;
          strncpy(att_name, dat_buf.st + shift_to_name + MAX_NAME * i, MAX_NAME);
          break;
        }
      }
    }
    if (j == -1)
    {
      if (find == given)
      {
        nefis_errcnt += 1;
        nefis_errno = 6019;
        sprintf(error_text,
                "String attribute \'%s\' of group \'%s\'not found",
                att_name, grp_name);
      }
      else
      {
        nefis_errcnt += 1;
        nefis_errno = 6020;
        sprintf(error_text,
                "No valid attribute name found");
      }
    }
    else
    {
      strncpy((BText)att_value, dat_buf.st + shift_to_value + MAX_NAME * j, MAX_NAME);
    }
  }

  return nefis_errno;
}
/*==========================================================================*/
BInt4 GP_get_next_grp(BInt4 set,         /* I file set descriptor */
                      BInt4 next,        /* I first(0) or next(1) */
                      BText grp_name,    /* O */
                      BText grp_defined) /* O */
{
  BUInt4 i;
  static BUInt8 dat_hash[LHSHDT];
  static BInt4 hash_pointer;
  BUInt8 next_pointer = NIL;
  static BInt4 num_hash;
  static BUInt8 pointer = (BUInt8)BUINT8_MAX;

  /*
   * File description already in memory
   */
  /*
   * Sort hash table in increasing order
   * First: lowest  hash key
   * Last : highest hash key
   */
  if (nefis[set].one_file == TRUE)
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHDT; i++)
      {
        if (nefis[set].daf.fds[2 + LHSHEL + LHSHCL + LHSHGR + i] != NIL)
        {
          num_hash += 1;
          dat_hash[num_hash] = nefis[set].daf.fds[2 + LHSHEL + LHSHCL + LHSHGR + i];
        }
      }
      if (num_hash == -1)
      {
        nefis_errcnt += 1;
        nefis_errno = -6021;
        sprintf(error_text,
                "No data groups available in DefinitonData file \'%s\'\n",
                nefis[set].dat_name);
        return nefis_errno;
      }

      QuickSort(dat_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = dat_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6021;
      sprintf(error_text,
              "No more data groups available in DefinitonData file \'%s\'\n",
              nefis[set].daf_name);
      return nefis_errno;
    }
  }
  else
  /*
   * Two files; definition and data file.
   */
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHDT; i++)
      {
        if (nefis[set].dat.fds[2 + i] != NIL)
        {
          num_hash += 1;
          dat_hash[num_hash] = nefis[set].dat.fds[2 + i];
        }
      }
      if (num_hash == -1)
      {
        nefis_errcnt += 1;
        nefis_errno = -6022;
        sprintf(error_text,
                "No data groups available in data file \'%s\'\n",
                nefis[set].dat_name);
        return nefis_errno;
      }

      QuickSort(dat_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = dat_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6022;
      sprintf(error_text,
              "No more data groups available in data file \'%s\'\n",
              nefis[set].dat_name);
      return nefis_errno;
    }
  }

  /*
   * Read data definition on data file
   */
  nefis_errno = HS_get_cont_dat(set,
                                pointer, &next_pointer,
                                grp_name, grp_defined);
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  if (next_pointer != NIL)
  {
    pointer = next_pointer;
  }
  else
  {
    assert(hash_pointer < INT_MAX);
    hash_pointer += 1;
    pointer = dat_hash[hash_pointer];
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get contents of element
 */
/*==========================================================================*/
BInt4 GP_get_next_elm(BInt4 set,                /* I file set descriptor */
                      BInt4 next,               /* I first(0) or next(1) */
                      BText elm_name,           /* O */
                      BText elm_type,           /* O */
                      BText elm_quantity,       /* O */
                      BText elm_unity,          /* O */
                      BText elm_desc,           /* O */
                      BUInt4 *elm_num_dim,      /* O */
                      BUInt4 *elm_dimens,       /* O */
                      BUInt4 *elm_single_bytes, /* O */
                      BUInt8 *elm_num_bytes)    /* O */
{
  BInt4 i;
  static BUInt8 elm_hash[LHSHEL];
  static BInt4 hash_pointer;
  static BUInt8 next_pointer;
  static BInt4 num_hash;
  static BUInt8 pointer = (BUInt8)BUINT8_MAX;

  /*
   * Sort hash table in increasing order
   * First: lowest  hash key
   * Last : highest hash key
   */
  if (nefis[set].one_file == TRUE)
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHEL; i++)
      {
        if (nefis[set].daf.fds[2 + i] != NIL)
        {
          num_hash += 1;
          elm_hash[num_hash] = nefis[set].daf.fds[2 + i];
        }
      }

      QuickSort(elm_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = elm_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6023;
      sprintf(error_text,
              "No more elements available in DefinitionData file \'%s\'\n",
              nefis[set].daf_name);
      return nefis_errno;
    }
  }
  else
  /*
   * Two files; definition and data file.
   */
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHEL; i++)
      {
        if (nefis[set].def.fds[2 + i] != NIL)
        {
          num_hash += 1;
          elm_hash[num_hash] = nefis[set].def.fds[2 + i];
        }
      }

      QuickSort(elm_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = elm_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6024;
      sprintf(error_text,
              "No more elements available in definition file \'%s\'\n",
              nefis[set].def_name);
      return nefis_errno;
    }
  }

  /*
   * Read contents of element from definition file
   */

  nefis_errno = HS_get_cont_elm(set, pointer,
                                &next_pointer, elm_name,
                                elm_type, elm_quantity,
                                elm_unity, elm_desc,
                                elm_num_dim, elm_dimens,
                                elm_single_bytes, elm_num_bytes);
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  if (next_pointer != NIL)
  {
    pointer = next_pointer;
  }
  else
  {
    assert(hash_pointer < INT_MAX);
    hash_pointer += 1;
    pointer = elm_hash[hash_pointer];
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get contents of cell
 */
/*=========================================================================*/
BInt4 GP_get_next_cell(BInt4 set,             /* I file set descriptor */
                       BInt4 next,            /* I first(0) or next(1) */
                       BText cel_name,        /* O */
                       BText *elm_names,      /* O */
                       BUInt4 *cel_num_dim,   /* O */
                       BUInt8 *cel_num_bytes) /* O */
{
  BUInt4 i;
  static BUInt8 cel_hash[LHSHCL];
  static BInt4 hash_pointer;
  static BUInt8 next_pointer;
  static BInt4 num_hash;
  static BUInt8 pointer = (BUInt8)BUINT8_MAX;

  /*
   * Sort hash table in increasing order
   * First: lowest  hash key
   * Last : highest hash key
   */
  if (nefis[set].one_file == TRUE)
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHCL; i++)
      {
        if (nefis[set].daf.fds[2 + LHSHEL + i] != NIL)
        {
          num_hash += 1;
          cel_hash[num_hash] = nefis[set].daf.fds[2 + LHSHEL + i];
        }
      }

      QuickSort(cel_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = cel_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6025;
      sprintf(error_text,
              "No more cells available in DefinitionData file \'%s\'\n",
              nefis[set].daf_name);
      return nefis_errno;
    }
  }
  else
  /*
   * Two files; definition and data file.
   */
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHCL; i++)
      {
        if (nefis[set].def.fds[2 + LHSHEL + i] != NIL)
        {
          num_hash += 1;
          cel_hash[num_hash] = nefis[set].def.fds[2 + LHSHEL + i];
        }
      }

      QuickSort(cel_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = cel_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6026;
      sprintf(error_text,
              "No more cells available in definition file \'%s\'\n",
              nefis[set].def_name);
      return nefis_errno;
    }
  }

  /*
   * Read contents of cell from definition file
   */

  nefis_errno = HS_get_cont_cel(set, pointer, &next_pointer,
                                cel_name, elm_names, cel_num_dim,
                                cel_num_bytes);
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  if (next_pointer != NIL)
  {
    pointer = next_pointer;
  }
  else
  {
    assert(hash_pointer < INT_MAX);
    hash_pointer += 1;
    pointer = cel_hash[hash_pointer];
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * Get contents of group as defined on definition file
 */
/*=========================================================================*/
BInt4 GP_get_next_def_grp(BInt4 set,           /* I file set descriptor */
                          BInt4 next,          /* I first(0) or next(1) */
                          BText grp_name,      /* O */
                          BText cel_name,      /* O */
                          BUInt4 *grp_num_dim, /* O */
                          BUInt4 *grp_dimens,  /* O */
                          BUInt4 *grp_order)   /* O */
{
  BInt4 i;
  static BUInt8 grp_hash[LHSHGR];
  static BInt4 hash_pointer;
  static BUInt8 next_pointer;
  static BInt4 num_hash;
  static BUInt8 pointer = (BUInt8)BUINT8_MAX;

  /*
   * Sort hash table in increasing order
   * First: lowest  hash key
   * Last : highest hash key
   */
  if (nefis[set].one_file == TRUE)
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHGR; i++)
      {
        if (nefis[set].daf.fds[2 + LHSHEL + LHSHCL + i] != NIL)
        {
          num_hash += 1;
          grp_hash[num_hash] = nefis[set].daf.fds[2 + LHSHEL + LHSHCL + i];
        }
      }

      QuickSort(grp_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = grp_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6027;
      sprintf(error_text,
              "No more defined groups available in DefinitionData file \'%s\'\n",
              nefis[set].daf_name);
      return nefis_errno;
    }
  }
  else
  /*
   * Two files; definition and data file.
   */
  {
    if (next == 0)
    {
      num_hash = -1;
      for (i = 0; i < LHSHGR; i++)
      {
        if (nefis[set].def.fds[2 + LHSHEL + LHSHCL + i] != NIL)
        {
          num_hash += 1;
          grp_hash[num_hash] = nefis[set].def.fds[2 + LHSHEL + LHSHCL + i];
        }
      }

      QuickSort(grp_hash, 0, num_hash);

      hash_pointer = 0;
      pointer = grp_hash[hash_pointer];
    }
    else if (hash_pointer > num_hash)
    {
      nefis_errcnt += 1;
      nefis_errno = -6028;
      sprintf(error_text,
              "No more defined groups available in definition file \'%s\'\n",
              nefis[set].def_name);
      return nefis_errno;
    }
  }

  /*
   * Read contents of cell from definition file
   */

  nefis_errno = HS_get_cont_grp(set, pointer, &next_pointer,
                                grp_name, cel_name, grp_num_dim,
                                grp_dimens, grp_order);

  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  if (next_pointer != NIL)
  {
    pointer = next_pointer;
  }
  else
  {
    assert(hash_pointer < INT_MAX);
    hash_pointer += 1;
    pointer = grp_hash[hash_pointer];
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 *  input : set         nefis file set descriptor
 *          var_index   group index of variable dimension
 *  output:
 *  return: 0           no error occured
 *          1           error occured
 */

BInt4 GP_variable_pointer(BInt4 set,
                          BUInt8 *grp_pointer,
                          BUInt4 var_index,
                          BUInt8 *var_pointer)
{
  union data_var
  {
    BChar st[8 * 257];
    BUInt8 ptr[257];
  } pointer_buf;

  BText cp;
  BUInt8 end_file;
  BInt4 fds;
  BInt4 from_xdr;
  BInt4 i;
  BInt4 j;
  BUInt4 mask1;
  BUInt4 mask2;
  BUInt4 masked;
  BUInt8 n_read = 0;
  BUInt8 start_table;
  BData vp;
  BUInt4 where;
  BUInt8 n_written = 0;

  /*
   * Add one to var_index for fortran convention (start with counting at one)
   */
  var_index += 1;

  if (nefis[set].one_file == TRUE)
  {
    fds = nefis[set].daf_fds;
  }
  else
  {
    fds = nefis[set].dat_fds;
  }

  start_table = *grp_pointer + SIZE_DAT_BUF + SIZE_BINT8;
  if (nefis[set].file_version == Version_1)
  {
    start_table = *grp_pointer + SIZE_DAT_BUF - 3 * SIZE_BINT4 + SIZE_BINT4;
  }

  n_read = GP_read_file_pointers(fds, pointer_buf.st, start_table, SIZE_BINT8 * 256, set);
  if (n_read > (BUInt8)SIZE_BINT8 * 256)
  {
    nefis_errcnt += 1;
    nefis_errno = 6029;
    sprintf(error_text,
            "On reading first variable pointer table");
    return nefis_errno;
  }

  if (nefis[set].daf_neutral == TRUE ||
      nefis[set].dat_neutral == TRUE)
  {
    from_xdr = 1;
    vp = (BData)malloc(SIZE_BINT8 * 256);
    cp = pointer_buf.st;
    nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * 256, SIZE_BINT8, "INTEGER", from_xdr);
    for (i = 0; i < 256; i++)
    {
      pointer_buf.ptr[i] = *((BUInt8 *)vp + (BUInt8)i);
    }
    free((BData)vp);
  }

  /*
   * Determine the pointer table
   * Divide var_index bit pattern into 4 parts
   * bits : 31,24; 23,16; 15,8; 7,0
   * 63,56:  2^23 < var_index < 2^64-1=          , at least one bit !=0
   * 55,48:  2^23 < var_index < 2^56-1=          , at least one bit !=0
   * 47,40:  2^23 < var_index < 2^48-1=          , at least one bit !=0
   * 39,32:  2^23 < var_index < 2^40-1=          , at least one bit !=0
   * 31,24:  2^23 < var_index < 2^32-1=2147483647, at least one bit !=0
   * 23,16:  2^15 < var_index < 2^24-1=8388607   , at least one bit !=0
   * 15, 8:  2^7  < var_index < 2^16-1=32767     , at least one bit !=0
   *  7, 0:    0  < var_index < 2^8 -1=255       , at least one bit !=0
   */

  start_table = *grp_pointer + SIZE_DAT_BUF + SIZE_BINT8;
  if (nefis[set].file_version == Version_1)
  {
    start_table = *grp_pointer + SIZE_DAT_BUF - 3 * SIZE_BINT4 + SIZE_BINT4;
  }
  if (nefis[set].one_file == TRUE)
  {
    end_file = nefis[set].daf.fds[1];
  }
  else
  {
    end_file = nefis[set].dat.fds[1];
  }
#if defined GNU_PC
  /*  mask1  = 0xff00000000000000ULL; */
  mask1 = 0xff000000UL;
#else
  /*  mask1  = 0xff00000000000000; */
  mask1 = 0xff000000;
#endif
  /*  for ( j=56; j>=0; j=j-8 ) */
  for (j = 24; j >= 0; j = j - 8)
  {
    masked = mask1 & var_index;
    where = (masked >> j);
    /*
     *  where points into the pointer table, normalized to the interval 0,255.
     *  therefor a sequence of four pointer tables is at least written to the
     *  data file.
     */
    if (pointer_buf.ptr[where] == BUINT8_MAX)
    {
      /*
       *    this pointer was never created, so creat a new pointer table (j!=0) at the end of the file
       *    or else (j==0) a pointer to the end of the file where the data will be
       *    written
       *
       *    update and write current_pointer_table
       */

      pointer_buf.ptr[where] = end_file;

      if (nefis[set].daf_neutral == TRUE ||
          nefis[set].dat_neutral == TRUE)
      {
        from_xdr = 0;
        vp = &pointer_buf.st[0];
        cp = (BText)malloc(SIZE_BINT8 * 256);
        nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * 256, SIZE_BINT8, "INTEGER", from_xdr);
        for (i = 0; i < 256; i++)
        {
          pointer_buf.ptr[i] = *((BUInt8 *)cp + (BUInt8)i);
        }
        free((BData)cp);
      }

      n_written = GP_write_file_pointers(fds, pointer_buf.st, start_table, SIZE_BINT8 * 256, set);

      if (n_written != (BUInt8)SIZE_BINT8 * 256)
      {
        nefis_errcnt += 1;
        nefis_errno = 6030;
        sprintf(error_text,
                "On writing variable pointer table, table %ld", j);
        return nefis_errno;
      }
      start_table = end_file;

      /*
       *    initialize new pointer table
       */

      if (j != 0)
      {
        for (i = 0; i < 256; i++)
        {
          pointer_buf.ptr[i] = (BUInt8)BUINT8_MAX;
        }
        if (nefis[set].file_version == Version_1)
        {
          end_file += SIZE_BINT4 * 256;
        }
        else
        {
          end_file += SIZE_BINT8 * 256;
        }
      }
      else
      {
        if (nefis[set].one_file == TRUE)
        {
          if ((end_file == BUINT8_MAX) ||
              (end_file < nefis[set].daf.fds[1]))
          {
            nefis_errcnt += 1;
            nefis_errno = 6033;
            sprintf(error_text,
                    "Maximum size reached in DataDefinition file\n");
            return nefis_errno;
          }

          nefis[set].daf.fds[1] = end_file;
          *var_pointer = nefis[set].daf.fds[1];
        }
        else
        {
          if ((end_file == BUINT8_MAX) ||
              (end_file < nefis[set].dat.fds[1]))
          {
            nefis_errcnt += 1;
            nefis_errno = 6034;
            sprintf(error_text,
                    "Maximum size reached in data file\n");
            return nefis_errno;
          }

          nefis[set].dat.fds[1] = end_file;
          *var_pointer = nefis[set].dat.fds[1];
        }
      }
    }
    else
    {
      /*
       *    pointer already created
       *    read new pointer table (j!=0) or return the pointer (j==0)
       */
      start_table = pointer_buf.ptr[where];
      if (j != 0)
      {
        n_read = GP_read_file_pointers(fds, pointer_buf.st, start_table,
                                       SIZE_BINT8 * 256, set);
        if (n_read > (BUInt8)SIZE_BINT8 * 256)
        {
          nefis_errcnt += 1;
          nefis_errno = 6031;
          sprintf(error_text,
                  "On reading variable pointer table, table %ld", j);
          return nefis_errno;
        }

        if (nefis[set].daf_neutral == TRUE ||
            nefis[set].dat_neutral == TRUE)
        {
          from_xdr = 1;
          vp = (BData)malloc(sizeof(BUInt8) * 256);
          cp = pointer_buf.st;
          nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * 256, SIZE_BINT8, "INTEGER", from_xdr);
          for (i = 0; i < 256; i++)
          {
            pointer_buf.ptr[i] = *((BUInt8 *)vp + (BUInt8)i);
          }
          free((BData)vp);
        }
      }
      else
      {
        *var_pointer = pointer_buf.ptr[where];
      }
    }
    /*
     * shift mask pattern 8 bits to right
     */
    mask2 = mask1 >> 8;
    mask1 = mask2 & ~mask1;
  }

  return nefis_errno;
}
/*==========================================================================*/
/*
 * QuickSort
 * qsort: sort v[left], ... , v[right] into (numeric) increasing order
 * from kernighan and ritchie
 */
static BVoid QuickSort(BUInt8 *v,
                       BInt4 left,
                       BInt4 right)
{
  BInt4 i;
  BInt4 last;

  if (left >= right) /* do nothing if array contains */
  {
    return; /* fewer than two elements      */
  }
  Swap(v, left, (left + right) / 2);
  last = left;
  for (i = left + 1; i <= right; i++)
  {
    if (v[i] < v[left])
    {
      Swap(v, ++last, i);
    }
  }

  Swap(v, left, last);
  QuickSort(v, left, last - 1);
  QuickSort(v, last + 1, right);
}
/*
 * Swap
 */
static BVoid Swap(BUInt8 *v, BInt4 i, BInt4 j)
{
  BUInt8 temp;

  temp = v[i];
  v[i] = v[j];
  v[j] = temp;
}
/*==========================================================================*/
/*     * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
 *
 *    - This function determines the maximum index for variable dimensions
 *
 *  input : set           nefis file set descriptor
 *          grp_name      group name for which maximum index must be derived
 *  output: max_index     maximum index
 *  return: 0             no error occured
 *          1             error occured
 *
 */
/*==========================================================================*/
BInt4 GP_inquire_max(BInt4 set,
                     BText grp_name,
                     BUInt4 *max_index)
{
  union dat_var
  {
    BChar st[8 * 256];
    BUInt8 ptr[256];
  } pointer_buf;

  BChar cel_name[MAX_NAME + 1];
  BText cp;
  BInt4 fds;
  BChar grp_defined[MAX_NAME + 1];
  BUInt4 grp_dimens[MAX_DIM];
  BUInt4 grp_num_dim;
  BUInt4 grp_order[MAX_DIM];
  BUInt8 grp_pointer = 0;
  BInt4 from_xdr;
  BUInt4 i;
  BUInt4 j;
  BInt4 k;
  BUInt8 n_read = 0;
  BUInt8 start_table;
  BUInt4 size_dat_buf = SIZE_DAT_BUF;
  BUInt4 var_dim = 0;
  BData vp;

  for (i = 0; i < MAX_NAME; i++)
  {
    cel_name[i] = ' ';
  }
  cel_name[MAX_NAME] = '\0';
  for (i = 0; i < MAX_NAME; i++)
  {
    grp_defined[i] = ' ';
  }
  grp_defined[MAX_NAME] = '\0';

  if (nefis[set].one_file == TRUE)
  {
    fds = nefis[set].daf_fds;
  }
  else
  {
    fds = nefis[set].dat_fds;
  }

  nefis_errno = GP_inquire_dat(set, &grp_pointer, grp_name, grp_defined);
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  nefis_errno = GP_inquire_grp_def(set,
                                   grp_defined, cel_name, &grp_num_dim,
                                   grp_dimens, grp_order);
  if (nefis_errno != 0)
  {
    return nefis_errno;
  }

  *max_index = 0;
  start_table = grp_pointer + size_dat_buf + SIZE_BINT8;
  if (nefis[set].file_version == Version_1)
  {
    size_dat_buf -= 3 * SIZE_BINT4;
    start_table = grp_pointer + size_dat_buf + SIZE_BINT4;
  }
  for (i = 0; i < grp_num_dim; i++)
  {
    *max_index = max(*max_index, grp_dimens[i]);
    if (grp_dimens[i] == 0)
    {
      *max_index = 0;
      var_dim = 1;
      for (j = 0; j < NR_VAR_TABLES; j++) /* pointer table is divided into eight/four parts */
      {
        /*
         *      now we are dealing with a group with variable dimension
         *      read pointer table
         */
        n_read = GP_read_file_pointers(fds, pointer_buf.st, start_table,
                                       SIZE_BINT8 * 256, set);
        if (n_read > (BUInt8)SIZE_BINT8 * 256)
        {
          nefis_errcnt += 1;
          nefis_errno = 6032;
          sprintf(error_text,
                  "On reading variable pointer table, table %ld", j);
          return nefis_errno;
        }
        if (nefis[set].daf_neutral == TRUE ||
            nefis[set].dat_neutral == TRUE)
        {
          from_xdr = 1;
          vp = (BData)malloc(sizeof(BUInt8) * 256);
          cp = pointer_buf.st;
          nefis_errno = convert_ieee(&vp, &cp, SIZE_BINT8 * 256, SIZE_BINT8, "INTEGER", from_xdr);
          for (i = 0; i < 256; i++)
          {
            pointer_buf.ptr[i] = *((BUInt8 *)vp + (BUInt8)i);
          }
          free((BData)vp);
        }
        for (k = 255; k > -1; k--)
        {
          start_table = 0;
          if (pointer_buf.ptr[k] != BUINT8_MAX)
          {
            start_table = pointer_buf.ptr[k];
            *max_index = *max_index * 256 + k;
            break;
          }
        }
        if (start_table == 0)
        {
          break;
        }
      }
    }
    if (var_dim == 1)
    {
      break;
    }
  }

  /*
   * if no variable dimension is found set max_index to -1,
   * at this moment max_index contains the maximum index also for fixed dimensions
   */
  /*
        if ( var_dim == 0 )
        {
          nefis_errno = 0;
          *max_index = -1;
        }
   */

  return nefis_errno;
}
