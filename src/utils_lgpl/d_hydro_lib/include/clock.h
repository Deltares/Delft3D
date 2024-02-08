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
// $Id: clock.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/utils_lgpl/d_hydro_lib/include/clock.h $
//------------------------------------------------------------------------------
//  d_hydro
//  Clock Class - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  24 may 11
//-------------------------------------------------------------------------------

#pragma once

#include <stdio.h>
#if HAVE_CONFIG_H
#   include <sys/time.h>
#else
#   include <windows.h>
#endif


class Clock {
    public:
        Clock ();
        ~Clock ();

        typedef unsigned long long Timestamp;

        char *      Now     (char *);       // current epoch time as string
        Timestamp   Epoch   (void);         // current epoch time (in usec)
        Timestamp   Start   (void);         // epoch time of clock start
        Timestamp   Elapsed (void);         // elapsed time since start
        void        Set     (Timestamp);    // set clock start to specific time
        void        Reset   (void);         // reset clock start time to now

    private:
        Timestamp   starttime;              // for timer

    };


