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
#include "esm.h"
#include "morflow.h"


int main (
    int     argc,
    char    *argv[],
    char    *envp[]
    )
{

    int fsm_flags = ESM_TRACE; // FSM flags for trisimtest (ESM_SILENT, ESM_TRACE)
    ESM_Init (fsm_flags);

    int context_id; // shared memory context ID

    if ((context_id = ESM_Create (0,0)) == 0)
    {
        printf ("Cannot create memory context for FLOW process");
                  return 1;
    }

    TRISIMTEST (&context_id, &fsm_flags);

    if (ESM_Delete (context_id) != 0)
    {
        printf ("Cannot delete memory context for FLOW process");
                  return 1;
    }

    return 0;
}

