//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
// $Id: clock.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Clock Class - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  24 may 11
//-------------------------------------------------------------------------------


#include "clock.h"
#include <ctime>
#include <time.h>

Clock::Clock (
    void
    ) {

    this->Reset ();
    }


Clock::~Clock (
    void
    ) {

    }


Clock::Timestamp
Clock::Epoch (
    void
    ) {

#if defined (WIN32)
    //Alternative implementation (is GetSystemTime thread safe?):
	//std::time_t SysTime = std::time(nullptr);
	//struct tm *OSTime;
	//OSTime=localtime(&SysTime);
	//return ((Timestamp) OSTime->tm_sec + OSTime->tm_min * 100 + OSTime->tm_hour * 10000 + OSTime->tm_yday * 1000000);
    SYSTEMTIME tv;
    GetSystemTime(&tv);     // ToDo: Check return code for errors
    return (((((Timestamp) tv.wDay * 24 + (Timestamp) tv.wHour) * 60 + (Timestamp) tv.wMinute) * 60 + (Timestamp) tv.wSecond) * 1000000) + tv.wMilliseconds;

#else
    struct timeval  tv;

    if (gettimeofday (&tv, NULL) != 0)
        return 0;
    else
        return ((Timestamp) tv.tv_sec * 1000000) + tv.tv_usec;
#endif
    }


Clock::Timestamp
Clock::Elapsed (
    void
    ) {

    return this->Epoch () - this->starttime;
    }

Clock::Timestamp
Clock::Start (
    void
    ) {

    return this->starttime;
    }


void
Clock::Set (
    Timestamp time
    ) {

    this->starttime = time;
    }


void
Clock::Reset (
    void
    ) {

    this->starttime = this->Epoch ();
    }


char *
Clock::Now (
    char *  buffer
    ) {
    // Epoch is only used for the milliseconds
    Timestamp eTime = this->Epoch ();
	time_t ttNow = time(0);
	tm * ptmNow;
	ptmNow = localtime(&ttNow);
    sprintf (buffer, "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                        1900 + ptmNow->tm_year,
                        1 + ptmNow->tm_mon,
                        ptmNow->tm_mday,
                        ptmNow->tm_hour,
                        ptmNow->tm_min,
                        ptmNow->tm_sec,
                        (int) (eTime % 1000000)
                        );
    return buffer;
    }

