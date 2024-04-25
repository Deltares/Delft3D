//-------------------------------------------------------------------------------
//  DelftOnline
//  Log Class -- DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  25 jun 11
//-------------------------------------------------------------------------------
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


#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include <pthread.h>

#include "dol.h"


namespace DOL {

class Log {
    public:
        Log (
            FILE *  output,
            Clock * clock,
            Verbosity level
            );

        ~Log (
            void
            );

        void
        RegisterThread (
            const char * id
            );

        void
        RenameThread (
            const char * id
            );

        void
        UnregisterThread (
            void
            );

        bool
        Write (
            Verbosity level,
            const char * format,
            ...
            );

    public:
        bool        showTime;       // when true output epoch time to the microsecond

    private:
        FILE *      output;
        Clock *     clock;
        Verbosity   level;

        pthread_key_t   thkey;      // contains key for thread-specific log data

    };

}
