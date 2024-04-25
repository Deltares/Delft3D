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
// $Id: log.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/utils_lgpl/d_hydro_lib/include/log.h $
//------------------------------------------------------------------------------
//  d_hydro
//  Log Object - Definitions
//
//  Irv.Elshoff@Deltares.NL
//  30 oct 11
//------------------------------------------------------------------------------


#pragma once

#include "d_hydro.h"


class Log {
    public:
        typedef unsigned int Mask;

        static const Mask SILENT            = 0;        // no messages
        static const Mask ALWAYS            = 1 << 1;   // a message that cannot never be supressed
        static const Mask WARN              = 1 << 2;   // warning message
        static const Mask MAJOR             = 1 << 3;   // major events in entire program
        static const Mask MINOR             = 1 << 4;   // minor events in entire program
        static const Mask DETAIL            = 1 << 5;   //
        static const Mask CONFIG_MAJOR      = 1 << 6;   // major events during initial configuration phase
        static const Mask ITER_MAJOR        = 1 << 7;   // DD iterator major events
        static const Mask DDMAPPER_MAJOR    = 1 << 8;   // DD mapper major events
        static const Mask RESERVED_9        = 1 << 9;   //
        static const Mask RESERVED_10       = 1 << 10;  //
        static const Mask RESERVED_11       = 1 << 11;  //
        static const Mask RESERVED_12       = 1 << 12;  //
        static const Mask RESERVED_13       = 1 << 13;  //
        static const Mask RESERVED_14       = 1 << 14;  //
        static const Mask RESERVED_15       = 1 << 15;  //

        static const Mask CONFIG_MINOR      = 1 << 16;  // minor events during initial configuration phase
        static const Mask ITER_MINOR        = 1 << 17;  // DD iterator minor events
        static const Mask DDMAPPER_MINOR    = 1 << 18;  // DD mapper minor events
        static const Mask DD_SENDRECV       = 1 << 19;  // DD iterator send/receive events
        static const Mask DD_SEMAPHORE      = 1 << 20;  // DD iterator semaphore events
        static const Mask RESERVED_21       = 1 << 21;  //
        static const Mask RESERVED_22       = 1 << 22;  //
        static const Mask RESERVED_23       = 1 << 23;  //
        static const Mask RESERVED_24       = 1 << 24;  //
        static const Mask RESERVED_25       = 1 << 25;  //
        static const Mask RESERVED_26       = 1 << 26;  //
        static const Mask RESERVED_27       = 1 << 27;  //
        static const Mask RESERVED_28       = 1 << 28;  //
        static const Mask RESERVED_29       = 1 << 29;  //
        static const Mask RESERVED_30       = 1 << 30;  //
        static const Mask LOG_DETAIL        = 1 << 31;  // include detailed time, node and iterator info in log messages

        static const Mask TRACE         = 0xFFFFFFFF;   // every possible event

    public:
        Log (
            FILE *  output,
            Clock * clock,
            Mask    mask = SILENT
            );

        ~Log (
            void
            );

        Mask
        GetMask (
            void
            );

        void
        SetMask (
            Mask mask
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
            Mask mask,
            const char * format,
            ...
            );

    private:
        FILE *      output;
        Clock *     clock;
        Log::Mask   mask;

        pthread_key_t   thkey;      // contains key for thread-specific log data

    };

