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
// $Id: map_messages.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/map_messages.h $
//------------------------------------------------------------------------------
// Message object for a Mapper Class Stores in/out messages for mapper
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "flow_in_hydra.h"


#define MAX_NUM_MESSAGES    20


//
// MapMess Class
//

class MapMess
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    MapMess(void);

    void InitOutMess(void);
    void PutOutMess(
        DDMesg  mess        // out message to be stored
        );
    DDMesg * GetOutMess(void);

    void InitInMess(void);
    void PutInMess(
        DDMesg  mess        // in message to be stored
        );
    DDMesg * GetInMess(void);

    ~MapMess(void);

    ////////////////////////
    //
    // PRIVATE DATA
    //
    private:

    DDMesg  outMess[MAX_NUM_MESSAGES];   // outgoing messages
    int numOut;                          // #outgoing messages
    DDMesg  inMess[MAX_NUM_MESSAGES];    // incoming messages
    int numIn;                           // #incoming messages

    DDMesg retMess;                      // Return message

};
