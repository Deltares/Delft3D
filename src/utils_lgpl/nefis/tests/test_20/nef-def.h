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

#ifndef __NEFIS_DEF__
#define __NEFIS_DEF__

#include "btps.h"

#define NOREFF(a)   ((a)=(a))

#define LHDRDF 60
#define LHSHDF ( (LHSHCL+LHSHEL+LHSHGR) )
#define LHSHCL 997
#define LHSHEL 997
#define LHSHGR 997

#define LHDRDT 60
#define LHSHDT 997

#define LHDRDAF (BInt8) 128
#define LHSHDAF ( (LHSHCL+LHSHEL+LHSHGR+LHSHDT) )

#define MAX_CEL_DIM          100
#define MAX_DESC              64
#define MAX_DIM                5
#define MAX_NAME              16
#define MAX_NEFIS_FILES       1024/2  /* FOPEN_MAX/2 */
#define MAX_TREE_NAME        255
#define MAX_TYPE               8

#define MAX_VAR_GROUPS        10
#define DEFINE_STEPS           3

#define BYTE_SIZE              8 /* one byte contains eight bits */

#define SIZE_BINT4           ( (BInt4) sizeof(BInt4) )
#define SIZE_REAL4           ( (BInt4) sizeof(BRea4) )
#define SIZE_BINT8           ( (BInt4) sizeof(BInt8) )
#define NR_VAR_TABLES          4

#if defined PTR8
/* pointer 8 long */
#  define NIL             (BUInt8) -1
#  define ALIGNMENT        4
#  define SIZE_CEL_BUF    52    /*   36      */
#  define SIZE_ELM_BUF   180    /*   164     */
#  define SIZE_GRP_BUF   100    /* niet deelbaar door 8;   88      */
#  define SIZE_DAT_BUF   416    /*   404     */
#elif defined PTR4
/* pointer 4 long */
#  define NIL             -1
#  define ALIGNMENT        4
#  define SIZE_CEL_BUF    36
#  define SIZE_ELM_BUF   164
#  define SIZE_GRP_BUF    88
#  define SIZE_DAT_BUF   404
#endif

typedef enum _boolean{
    FALSE,
    TRUE
} BBool;

typedef enum _file_version{
    Version_0,
    Version_1,
    Version_5
} File_Version;

#define MCR_from_int8_to_int4( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=0; mcr_i<(lst_bt); mcr_i++) \
        { \
            *((a)+mcr_i) = *((a)+2*mcr_i); \
        } \
    }
#define MCR_L_from_int8_to_int4( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=0; mcr_i<(lst_bt); mcr_i++) \
        { \
            *((a)+mcr_i) = *((a)+2*mcr_i); \
        } \
    }
#define MCR_B_from_int8_to_int4( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=0; mcr_i<(lst_bt); mcr_i++) \
        { \
            *((a)+mcr_i) = *((a)+2*mcr_i+1); \
        } \
    }
#define MCR_from_int4_to_int8( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=(lst_bt); mcr_i>0; mcr_i--) \
        { \
            *((a)+2*mcr_i-1) = *((a)+mcr_i-1) == -1 ? -1 : 0; \
            *((a)+2*mcr_i-2) = *((a)+mcr_i-1); \
        } \
    }
#define MCR_L_from_int4_to_int8( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=(lst_bt); mcr_i>0; mcr_i--) \
        { \
            *((a)+2*mcr_i-1) = *((a)+mcr_i-1) == -1 ? -1 : 0; \
            *((a)+2*mcr_i-2) = *((a)+mcr_i-1); \
        } \
    }
#define MCR_B_from_int4_to_int8( a, lst_bt) \
    { \
        BUInt4 mcr_i;  \
        for (mcr_i=(lst_bt); mcr_i>0; mcr_i--) \
        { \
            *((a)+2*mcr_i-1) = *((a)+mcr_i-1); \
            *((a)+2*mcr_i-2) = *((a)+mcr_i-1) == -1 ? -1 : 0; \
        } \
    }

#define MCR_shift_string_forward( a, lngth, shft) \
    { \
        BInt4 mcr_i;  \
        for (mcr_i=(lngth)-1; mcr_i>=0; mcr_i--) \
        { \
            *((a)+mcr_i+(shft)) = *((a)+mcr_i); \
        } \
    }
#define MCR_shift_string_backward( a, lngth, shft) \
    { \
        BInt4 mcr_i;  \
        for (mcr_i=0; mcr_i<(lngth); mcr_i++) \
        { \
            *((a)+mcr_i-(shft)) = *((a)+mcr_i); \
        } \
    }
/*
#define SIZE_ELM_BUF           SIZE_BINT8 + \    8
                               SIZE_BINT8 + \    8
                               SIZE_BINT8 + \    8
                               MAX_NAME + \     16
                               MAX_TYPE + \      8
                               SIZE_BINT8 + \    8
                               SIZE_BINT4 + \    4
                               MAX_NAME + \     16
                               MAX_NAME + \     16
                               MAX_DESC + \     64
                               SIZE_BINT4 * ( 1 + MAX_DIM ) 24

#define SIZE_CEL_BUF           SIZE_BINT8 + \    8
                               SIZE_BINT8 + \    8
                               SIZE_BINT8 + \    8
                               MAX_NAME + \     16
                               SIZE_BINT8 + \    8
                               SIZE_BINT4        4

#define SIZE_GRP_BUF           SIZE_BINT8 + \
                               SIZE_BINT8 + \
                               SIZE_BINT8 + \
                               MAX_NAME + \
                               MAX_NAME + \
                               SIZE_BINT4 * ( 1 + 2 * MAX_DIM )

#define SIZE_DAT_BUF           SIZE_BINT8 + \
                               SIZE_BINT8 + \
                               SIZE_BINT8 + \
                               MAX_NAME + \
                               MAX_NAME + \
                               MAX_DIM *( MAX_NAME + SIZE_BINT4 ) + \
                               MAX_DIM *( MAX_NAME + SIZE_REAL4 ) + \
                               MAX_DIM *( MAX_NAME + MAX_NAME   )
*/

#endif

