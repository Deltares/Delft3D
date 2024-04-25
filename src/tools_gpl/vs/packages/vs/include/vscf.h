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

#ifndef _VSCF_INCLUDED
#   define _VSCF_INCLUDED
    struct St_var{
        BChar           varnam[MAX_NAME+1];
        BChar           elmtyp[MAX_TYPE+1];
        BInt4           nbytsg;
        BChar           elmqty[MAX_NAME+1];
        BChar           elmunt[MAX_NAME+1];
        BChar           elmdes[MAX_DESC+1];
        BInt4           nintat;
        BChar           intatn[MAX_DIM][MAX_NAME+1];
        BInt4           intatv[MAX_DIM];
        BInt4           nreaat;
        BChar           reaatn[MAX_DIM][MAX_NAME+1];
        float           reaatv[MAX_DIM];
        BInt4           nstrat;
        BChar           stratn[MAX_DIM][MAX_NAME+1];
        BChar           stratv[MAX_DIM][MAX_NAME+1];
        BChar           grpnam[MAX_NAME+1];
        BInt4           grpndm;
        BInt4           grpdms[MAX_DIM];
        BChar           elmnam[MAX_NAME+1];
        BInt4           elmndm;
        BInt4           elmdms[MAX_DIM];
        BVoid         * varpnt;
        struct St_var * next;
    };

    enum STORAGE_TYPE {DBLCMPL, COMPLEX, DOUBLE, FLOAT, INT, SHORT, \
                       BOOL, BOOLSHRT, CHAR, UNDEF} ;

#   if defined(__STDC__) || defined(MSDOS) || defined(WIN32)
    extern struct St_var * VS_create_new_var ( BText, BText,
                                BInt4, BInt4, BInt4[], BInt4, BInt4[] );
    extern size_t          VS_calc_number_of_values (
                                struct St_var * );
    extern struct St_var * VS_read_var_from_pipe( BVoid );
    extern BVoid           VS_write_var_to_pipe( struct St_var * );
    extern BInt4           VS_get_data_type ( struct St_var * );
    extern BVoid         * VS_malloc        ( size_t );
    extern BVoid           VS_free          ( BData );
#   else /* not __STDC__ */
    extern struct St_var * VS_create_new_var ();
    extern size_t          VS_calc_number_of_values ()
    extern struct St_var * VS_read_var_from_pipe();
    extern BVoid           VS_write_var_to_pipe();
    extern BInt4           VS_get_data_type ();
    extern BVoid         * VS_malloc ();
    extern BVoid           VS_free ();
#   endif /* __STDC__ || MSDOS */
#endif
