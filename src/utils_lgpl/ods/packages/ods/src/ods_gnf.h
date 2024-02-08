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
/*  ods_gnf.h - Define the data structures for the generic NEFIS routines
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the data structures and macros for the generic
 *  NEFIS routines
 */

/*
 *  $Author: Markus $
 *  $Date: 11-02-05 10:04 $
 *  $Source$
*/

#ifndef ODS_GNF_H_INCLUDED
#define ODS_GNF_H_INCLUDED

/*
 * Macros and enums
 */

/* The macro LAST indicates that the name is to be taken from the end of
   the array - (hopefully) for Delft3D-FLOW only
*/
#define LAST  (-1000)

#define MAX(a,b) ((a)>(b)? (a) : (b))
#define MIN(a,b) ((a)<(b)? (a) : (b))

typedef enum {
  END_OF_ARRAY,          /* End of data array marker */
  NO_DATA,               /* Indicates we need to fill an array with zeroes */
  SCALAR_DATA,
  SCALAR_DATA_SUMMED,
  SCALAR_DATA_SUBTRACTED,
  SCALAR_DATA_FACE,      /* Data defined on the horizontal face, thus
                            introducing an extra layer */
  SCALAR_MISSING_ZERO,   /* The value 0.0 represents the missing value */
  ADD_TO_PREVIOUS,
  SPECIAL_DEPTH_CENTRE,
  LOCAL_VECTOR_U,
  LOCAL_VECTOR_V,
  LOCAL_VECTOR_MAG,
  LOCAL_VECTOR_DIR,
  ENCODED_DATA_U,
  ENCODED_DATA_V,
  COORD_DATA_3D,
  DPT_AVERAGE = 1024     /* Special attribute */
} ParameterType ;

/* NOTE: geared to use with Delft3D-FLOW only! (It is assumed that there are two parameter types */
typedef enum {
  FIRST_IS_1  = 10000,           /* For mass balances D3D-FLOW */
  FIRST_IS_2  = 20000,
  SCALE_FIRST = 10000            /* Divide by this factor */
} SpecialParameterHandler ;

typedef enum {
  LOC_IDS,
  LOC_NAMES,
  BAL_NAMES,             /* Specially constructed names */
  GRID_LGRID,
  GRID_KCS,
  GRID_KCSINV,           /* Reversed indices */
  GRID_TRIVIAL,          /* Means there is no actual grid info */
  END_OF_LOC_TYPES       /* End of data array marker */
} LocationType ;

typedef enum {
  END_OF_DATE_TYPES,     /* End of data array marker */
  CONSTANT,              /* Time-independent */
  ITDATE,                /* Method used by Delft3D-FLOW, for instance */
  IDTISO,                /* ISO-compliant string with "-" and ":" separators */
  IT0102,                /* Method used in Delft3D communications file */
  ITNONE                 /* No reference date/time given */
} DateTimeType ;

typedef enum {
  N
} NefisDataType ;

/*
 * Information on date/time
 * Note:
 * The default scale factor determines how to go from the actual
 * time-information (expressed in some unit) to seconds
 */
typedef struct _DateTimeInfoStruct {
   TString         datetime_id       ; /* ID for date/time                        */
   DateTimeType    type              ; /* Type of date/time                       */
   TString         group_name        ; /* Name of the group that holds the data   */
   TString         elem_name         ; /* Name of the element within that group   */
   TString         refdate_group     ; /* Name of the group with reference date   */
   TString         refdate_elem      ; /* Name of the element within that group   */
   TString         timestep_group    ; /* Name of the group with timestep         */
   TString         timestep_elem     ; /* Name of the element within that group   */
   TString         scale_group       ; /* Name of the group with scale factor     */
   TString         scale_elem        ; /* Name of the element within that group   */
   TReal8          default_scale     ; /* Default scale factor for time           */
} DateTimeInfoStruct, *DateTimeInfoPtr ;

/*
 * Information on locations
 */
typedef struct _LocationInfoStruct {
   TString         location_id       ; /* ID for location                         */
   LocationType    type              ; /* Type of locations                       */
   TString         group_name        ; /* Name of the group that holds the data   */
   TString         elem_name         ; /* Name of the element within that group   */
   TString         gridinfo_group    ; /* Name of the group with grid information */
   TString         gridinfo_elem     ; /* Name of the element within that group   */
   TString         depth_group       ; /* Name of the group with depth            */
   TString         depth_elem        ; /* Name of the element within that group   */
   TString         zcoord_group      ; /* Name of the group with z-coordinate     */
   TString         zcoord_elem       ; /* Name of the element within that group   */
   TString         sigmalayer_group  ; /* Name of the group with sigma-layers     */
   TString         sigmalayer_elem   ; /* Name of the element within that group   */
   TString         orientation_group ; /* Name of the group with grid orientation */
   TString         orientation_elem  ; /* Name of the element within that group   */
} LocationInfoStruct, *LocationInfoPtr ;

/*
 * Information on parameters
 */
typedef struct _ParameterInfoStruct {
   TString         public_name       ; /* Public name exported to caller          */
   TInt4           type              ; /* Type of parameter                       */
   TString         group_name        ; /* Name of the group that holds the data   */
   TString         elem_name         ; /* Name of the element within that group   */
   TInt4           fixed_last_index  ; /* Value of the fixed last array dimension */
   TString         names_group       ; /* Group holding parameter names           */
   TString         names_elem        ; /* Element with the actual names           */
   TInt4           index_name        ; /* Index into the list of names            */
   TString         datetime_id       ; /* ID for date/time properties             */
   DateTimeInfoPtr datetime_info     ; /* Pointer to data/time info               */
   TString         location_id       ; /* ID for location properties              */
   LocationInfoPtr location_info     ; /* Pointer to location info                */
} ParameterInfoStruct, *ParameterInfoPtr ;

/*
 * Information on the contents of a NEFIS file - group/element
 */
#define NFNAMELEN  17
#define NFFILELEN 256
#define NFDIM       5
#define ALL_CELLS (-1)
typedef struct _NefisItemInfoStruct {
   TChar           group_name[NFNAMELEN]   ; /* Name of the group                 */
   TChar           elem_name[NFNAMELEN]    ; /* Name of the element               */
   NefisDataType   elem_type               ; /* Encoding of the data type         */
   TInt4           number_bytes            ; /* Number of bytes                   */
   TInt4           group_ndim              ; /* Number of group dimensions        */
   TInt4           group_last_dim_free     ; /* Whether last dimension is free    */
   TInt4           elem_ndim               ; /* Number of element dimensions      */
   TInt4           group_dims[NFDIM]       ; /* Group dimensions                  */
   TInt4           elem_dims[NFDIM]        ; /* Element dimensions                */
} NefisItemInfoStruct, *NefisItemInfoPtr ;

typedef struct _NefisFileInfoStruct {
   TInt4              no_items               ; /* Number of items               */
   TInt4              maxitems               ; /* Maximum number of items       */
   TInt4              filetype               ; /* ODS file type (identification)*/
   ParameterInfoPtr   defined_params         ; /* Defined parameters            */
   NefisItemInfoPtr   items                  ; /* Array of item structures      */
   TChar              filename[3][NFFILELEN] ; /* Name(s) of the actual files   */
   BInt4              datfds[999]            ; /* Handle for data file          */
   BInt4              deffds[2997]           ; /* Handle for definition file    */
} NefisFileInfoStruct, *NefisFileInfoPtr ;


/* Prototypes */
TVoid
   GNF_GetStringBuffer(
      NefisFileInfoPtr      file_info,
      TInt4                 idx,
      TString             * buffer,
      TInt4               * string_length,
      TInt4               * number_strings ) ;

TVoid
   GNF_GetRealBuffer(
      NefisFileInfoPtr      file_info,
      TInt4                 idx,
      TInt4               * cell_index,
      TReal4             ** buffer,
      TInt4               * elem_size,
      TInt4               * number_cells   ) ;

TVoid
   GNF_GetIntBuffer(
      NefisFileInfoPtr      file_info,
      TInt4                 idx,
      TInt4               * cell_index,
      TInt4              ** buffer,
      TInt4               * elem_size,
      TInt4               * number_cells   ) ;

TInt4 GNF_FindParameter(
   TString            group_name,
   TString            elem_name,
   TInt4              non_scalar,
   TInt4              no_items,
   NefisItemInfoPtr   items,
   TInt4            * idx
   );

TVoid GNF_GetDim(
   TString    filename,
   TInt4    * itype,
   TString    dim,
   TInt4    * pardep,
   TInt4    * timdep,
   TInt4    * locdep,
   TInt4    * ndim,
   TInt4    * ierror,
   TString    option
   );

TVoid
   GNF_GetPar(
      TString   filename,
      TInt4   * itype,
      TString   pardef,
      TInt4   * maxdef,
      TInt4   * timdep,
      TInt4   * locdep,
      TInt4   * maxlst,
      TInt4   * lang,
      TString   parlst,
      TString   paruni,
      TInt4   * partyp,
      TInt4   * parcod,
      TInt4   * nrlst,
      TInt4   * ierror,
      TString   option
   );

TVoid
   GNF_GetTme(
      TString   filename,
      TInt4   * itype,
      TReal8  * timdef,
      TInt4   * maxdef,
      TInt4   * pardep,
      TInt4   * locdep,
      TInt4   * maxlst,
      TReal8  * timlst,
      TInt4   * timtyp,
      TInt4   * nrlst,
      TInt4   * ierror,
      TString   option
   );

TVoid
   GNF_GetLoc(
      TString   filename,
      TInt4   * itype,
      TString   locdef,
      TInt4   * maxdef,
      TInt4   * pardep,
      TInt4   * timdep,
      TInt4   * maxlst,
      TString   loclst,
      TInt4   * loctyp,
      TInt4   * locid,
      TInt4   * nrlst,
      TInt4   * ierror,
      TString   option
   );

TVoid
   GNF_GetMat(
      TString   filename,
      TInt4   * itype,
      TInt4   * parcod,
      TInt4   * loc,
      TReal8  * times,
      TReal4  * misval,
      TInt4   * i3gl,
      TInt4   * maxdim,
      TReal4  * data,
      TInt4   * ierror,
      TString   option
   );

TVoid
   GNF_GetGrd(
      TString    filename,
      TInt4    * itype,
      TInt4    * indloc,
      TInt4    * indx,
      TInt4    * nocell,
      TInt4    * igisty,
      TInt4    * ierror
   );

TVoid
   GNF_ClearAllFiles(
      void
   );

TVoid
GNF_GetActualData(
   NefisFileInfoPtr      file_info,
   ParameterInfoPtr      param_info,
   TInt4                 idxpar,
   TInt4                 cell_index[5][3],
   TInt4               * loc,
   TReal4                misval,
   TReal4              * data,
   TInt4               * nodata
   ) ;

/* end ODS_GNF_H_INCLUDED */
#endif
