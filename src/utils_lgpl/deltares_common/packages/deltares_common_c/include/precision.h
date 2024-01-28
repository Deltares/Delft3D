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
//
// Delft3D-FLOW Precision definition
//
// Jan.Mooiman@deltares.nl
// Adri.Mourits@deltares.nl
//
// Nov 09, 2004
//

// See comments in file libsrc\flow_modsrc\precision.f90

#ifndef _D3D_FLOW_PRECISION
#define _D3D_FLOW_PRECISION

#define REAL_HP double              // double or higher
#define REAL_HP_TYPE DoubleType     // double or higher

#  define REAL_SP float             // float, double or higher
#  define REAL_SP_TYPE FloatType    // float, double or higher



#define FLOW_DOUBLE_PRECISION  // activate this   define when fp == hp (high   precision)
//#undef FLOW_DOUBLE_PRECISION      // activate this undefine when fp == sp (single precision)

#define PREC_DOUBLE_PRECISION     // activate this   define when prec == hp (high   precision)
// #undef PREC_DOUBLE_PRECISION   // activate this undefine when prec == sp (single precision)



#if defined FLOW_DOUBLE_PRECISION
#  define REAL_FP double            // double or higher
#  define REAL_FP_TYPE DoubleType   // double or higher
#else
#  define REAL_FP float             // float, double or higher
#  define REAL_FP_TYPE FloatType    // float, double or higher
#endif

#if defined PREC_DOUBLE_PRECISION
#  define REAL_PREC double            // double or higher
#  define REAL_PREC_TYPE DoubleType   // double or higher
#else
#  define REAL_PREC float             // float, double or higher
#  define REAL_PREC_TYPE FloatType    // float, double or higher
#endif

#endif
