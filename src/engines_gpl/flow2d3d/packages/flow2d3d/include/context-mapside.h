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
// $Id: context-mapside.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context-mapside.h $
//------------------------------------------------------------------------------
//  Class: D3dFlowContextMapSide
//  DELFT3D-FLOW context functions for mapper side
//
//  Stef.Hummel@Deltares.NL
//  Erik.deGoede@deltares.nl
//  Menno.Genseberger@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "context.h"
#include "context-flowside.h"  // FOR distributed by copy
#include "iterator.h"


//////////////////////////////////////////////////////////////////////
//
// D3dFlowContextMapSide Class
//

class D3dFlowContextMapSide : public D3dFlowContext
{

    public:


    //
    // PUBLIC FUNCTIONS
    //

    D3dFlowContextMapSide(void);

    ~D3dFlowContextMapSide(void);

    int Setup(
        Iterator *  mapper,         // mapper process
        Iterator *  flow,           // flow processes
        MemType     aMemType,
        EdgeType    aEdgeType,
        int         mStart[NR_EQ],
        int         nStart[NR_EQ],
        int         mEnd[NR_EQ],
        int         nEnd[NR_EQ]
        );

    void UpdateMapperToFlow  (UpdateHeader &updateHeader);
    void UpdateMapperFromFlow(UpdateHeader &updateHeader);

    void SendBlobToFlow(
        DDBlobID    blobID,
        int         numBytes,
        char      * bytes
        );

    int ReceiveBlobFromFlow(
        DDBlobID    blobID,
        int         numBytes,
        char      * bytes
        );

    //
    // Distributed Data Communication
    //

    void ReceiveSizesAndFlagsFromFlow(void);
    void SendMapperInfoToFlow(void);

    //
    // Data allocation
    //
    void CreateMapperStrips(void);

};
