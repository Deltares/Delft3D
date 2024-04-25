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
/*  ods_conv.h - Conversion routines for ODS
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the general routines for converting raw data
 *  in the context of ODS
 */

/*
 *  $Author: Markus $
 *  $Date: 5-09-03 14:30 $
 *  $Source$
*/

#ifndef ODS_CONV_H_INCLUDED
#define ODS_CONV_H_INCLUDED

TVoid
ODS_AverageOverDepth(
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4            * data3d,
      TReal4            * zcrd,
      TReal4              misval,
      TReal4            * data,
      TInt4             * nodata ) ;

TVoid
ODS_AccumulateData(
      TReal4              misval,
      TInt4             * ndim,
      TReal4            * data,
      TReal4            * data_to_add,
      TReal4              factor      );

TVoid
ODS_AccumulateOverTime(
      TInt4             * ndim,
      TReal4            * data,
      TReal4            * data_to_add,
      TReal4              deltat      ) ;

TVoid
ODS_ConvertVectorComponents(
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4            * data_u,
      TReal4            * data_v,
      TReal4            * angle,
      TInt4               truegrid,
      TReal4              misval,
      TInt4               result_idx,
      TReal4            * result,
      TInt4             * nodata ) ;

TVoid
   ODS_ConvertKCS(
      TInt4             * dims,
      TInt4             * ibuffer,
      TInt4             * indx,
      TInt4               grid_type ) ;

TVoid
   ODS_ConstructTrivialGrid(
      TInt4             * dims,
      TInt4             * indx   ) ;
/* end ODS_CONV_H_INCLUDED */
#endif
