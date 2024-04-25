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
#include "btps.h"
#include "nef-def.h"
#include "neftyp.h"
#include "nefis.h"



typedef struct St_cel  * VsCelData;

typedef struct St_cel {
        BChar                   celnam[MAX_NAME+1];
        BInt4                   nelems;
        BChar                   elmnms[MAX_CEL_DIM][MAX_NAME+1];
        VsCelData left;
        VsCelData right;
} VsCel;

typedef struct St_var * VsVarData;

typedef struct St_var {
        BChar                   varnam[MAX_NAME+1];
        BChar                   grpnam[MAX_NAME+1];
        BInt4                   grpndm;
        BInt4                   grpdms[MAX_DIM];
        BChar                   elmnam[MAX_NAME+1];
        BChar                   elmtyp[MAX_TYPE+1];
        BInt4                   nbytsg;
        BChar                   elmqty[MAX_NAME+1];
        BChar                   elmunt[MAX_NAME+1];
        BChar                   elmdes[MAX_DESC+1];
        BInt4                   elmndm;
        BInt4                   elmdms[MAX_DIM];
        BData                   varpnt;

        VsVarData               next;
} VsVar;

typedef struct St_grp * VsGrpData;

typedef struct St_grp {
        BData                   next;
        BInt4                   grpdms[MAX_DIM];
        BInt4                   nintat;
        BInt4                   nreaat;
        BInt4                   nstrat;
        BInt4                   IntAttrValue[MAX_DIM];
        BRea4                   RealAttrValue[MAX_DIM];
        BChar                   grpdef[MAX_NAME+1];
        BChar                   grpnam[MAX_NAME+1];
        BChar                   IntAttrName [MAX_DIM][MAX_NAME+1];
        BChar                   RealAttrName [MAX_DIM][MAX_NAME+1];
        BChar                   StringAttrName [MAX_DIM][MAX_NAME+1];
        BChar                   StringAttrValue[MAX_DIM][MAX_NAME+1];

/*      VsGrpData               next; */
}VsGrp ;

typedef struct St_def  * VsDefData;
typedef struct St_def {
        BInt4                   grpndm;
        BInt4                   grpord[MAX_DIM];
        BChar                   grpdef[MAX_NAME+1];
        BChar                   celnam[MAX_NAME+1];

        VsDefData               left;
        VsDefData               right;
} VsDef;

typedef struct St_elm * VsElmData;
typedef struct St_elm {
        BInt4                   elmndm;
        BInt4                   elmdms[MAX_DIM];
        BChar                   elmnam[MAX_NAME+1];
        BChar                   elmtyp[MAX_TYPE+1];
        BInt4                   nbytsg;
        BChar                   elmqty[MAX_NAME+1];
        BChar                   elmunt[MAX_NAME+1];
        BChar                   elmdes[MAX_DESC+1];

        VsElmData               left;
        VsElmData               right;
}VsElm;



#include "atts.h"
#include "gen.h"
#include "ex.h"
#include "pp.h"
#include "gr.h"
#include "df.h"
#include "cl.h"
#include "el.h"
#include "fm.h"
#include "vr.h"
#include "sm.h"

