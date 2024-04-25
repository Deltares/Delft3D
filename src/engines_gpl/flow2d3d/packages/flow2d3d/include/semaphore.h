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
// $Id: semaphore.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/semaphore.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Semaphore built on Pthreads - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"


class Semaphore {
    public:
        Semaphore       (
            const char *    name = "<anonymous>",
            const int       initialValue = 0,
            Log *           log = NULL
            );

        ~Semaphore (
            void
            );

        void    PSem    (void);
        void    VSem    (void);

    private:

    private:
       enum {
            MAXSTRING  = 1000           // max string length in bytes
            };

        char *          name;           // for identification purposes
        int             value;          // semaphore counter
        Log *           log;            // logging facility

        int             waiting;        // number of waiting threads
        pthread_mutex_t mutex;          // semaphore mutex
        pthread_cond_t  syncv;          // synchronization cond var
    };

