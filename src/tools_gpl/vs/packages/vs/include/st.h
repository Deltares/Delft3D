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
#include "nef-def.h"

#ifndef _ST_INCLUDED

    struct St_grp{
        BChar           grpnam[MAX_NAME+1];
        BChar           grpdef[MAX_NAME+1];
        BInt4           grpdms[MAX_DIM];
        BInt4           nintat;
        BChar           intatn[MAX_DIM][MAX_NAME+1];
        BInt4           intatv[MAX_DIM];
        BInt4           nreaat;
        BChar           reaatn[MAX_DIM][MAX_NAME+1];
        float           reaatv[MAX_DIM];
        BInt4           nstrat;
        BChar           stratn[MAX_DIM][MAX_NAME+1];
        BChar           stratv[MAX_DIM][MAX_NAME+1];
        struct St_grp * next;
    };

    struct St_def{
        BChar           grpdef[MAX_NAME+1];
        BChar           celnam[MAX_NAME+1];
        BInt4           grpndm;
        BInt4           grpord[MAX_DIM];
        struct St_def * left;
        struct St_def * right;
    };

    struct St_cel{
        BChar           celnam[MAX_NAME+1];
        BInt4           nelems;
        BChar           elmnms[100][MAX_NAME+1];
        struct St_cel * left;
        struct St_cel * right;
    };

    struct St_elm{
        BChar           elmnam[MAX_NAME+1];
        BChar           elmtyp[MAX_TYPE+1];
        BInt4           nbytsg;
        BChar           elmqty[MAX_NAME+1];
        BChar           elmunt[MAX_NAME+1];
        BChar           elmdes[MAX_DESC+1];
        BInt4           elmndm;
        BInt4           elmdms[MAX_DIM];
        struct St_elm * left;
        struct St_elm * right;
    };

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

    struct St_inf {
        int             open_file;
        BChar         * dtflnm;
        BChar         * dfflnm;
        BChar         * coding;
        struct St_grp * grpinf;
        struct St_def * definf;
        struct St_cel * celinf;
        struct St_elm * elminf;
        struct St_var * varinf;
        BInt4           neffds[1];
        BInt4           datfds[1];
        BInt4           deffds[1];
    };

#   define _ST_INCLUDED
#endif
