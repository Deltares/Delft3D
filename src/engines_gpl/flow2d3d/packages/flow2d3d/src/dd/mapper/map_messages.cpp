//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
// $Id: map_messages.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/map_messages.cpp $
//------------------------------------------------------------------------------
// Class: MapMess
// Message object for a Mapper Class
//
// Stef.Hummel@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//////////////////////////////////////////////////////////////////////
//
// MapMess
//

MapMess::MapMess(void)
{
    MAPDBG_FUN2("MapMess::MapMess");

    numIn  = 0;
    numOut = 0;
}


void MapMess::InitOutMess(void)
{
    MAPDBG_FUN3("MapMess::InitOutMess");

    if ( numOut != 0 )
    {
    FLOW2D3D->dd->log->Write (Log::WARN, "%d Out-Messages not processed", numOut);
    }

    numOut = 0;
}


void MapMess::PutOutMess(
    DDMesg  mess        // message
    )
{
    MAPDBG_FUN3("MapMess::PutOutMess");

    outMess[numOut] = mess;
    numOut++;
}


DDMesg * MapMess::GetOutMess(void)
{
    DDMesg * mess = NULL;   // Return value

    MAPDBG_FUN3("MapMess::GetOutMess");

    if ( numOut > 0 )
    {
        numOut--;
        mess = &outMess[numOut];
    }
    return mess;
}


void MapMess::InitInMess(void)
{
    MAPDBG_FUN3("MapMess::InitInMess");

    if ( numIn != 0 )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "%d In-Messages not processed", numIn);
    }

    numIn = 0;
}


void MapMess::PutInMess(
    DDMesg  mess        // message
    )
{
    MAPDBG_FUN3("MapMess::PutInMess");

    inMess[numIn] = mess;
    numIn++;
}


DDMesg * MapMess::GetInMess()
{
    DDMesg *  retVal=NULL;      // Return message
    int mess, moveMess;         // loop counters

    MAPDBG_FUN3("MapMess::GetInMess");

    //
    // Search for message type
    //

    for ( mess = 0 ; mess < numIn ; mess++ )
    {
        //
        // Return first message from message store.
        //

        retMess = inMess[mess];
        retVal = &retMess;

        for ( moveMess = mess + 1 ; moveMess < numIn ; moveMess++ )
        {
            inMess[moveMess-1] = inMess[moveMess];
        }
        numIn--;
    }

    return retVal;
}


MapMess::~MapMess(void)
{
    MAPDBG_FUN2("MapMess::~MapMess");
}
