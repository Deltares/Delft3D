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
// $Id: context-flowside.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context-flowside.h $
//------------------------------------------------------------------------------
//  Class: D3dFlowContextFlowSide
//  Additional DELFT3D-FLOW context variables used by mapper in case of distributed memory
//
//  Stef.Hummel@Deltares.NL
//  Erik.deGoede@deltares.nl
//  Menno.Genseberger@deltares.nl
//  Adri.Mourits@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "context.h"
#include "iterator.h"


//////////////////////////////////////////////////////////////////////
//
// D3dFlowContextFlowSide:
//
// Public Functions on D3dFlowContextFlowSide data
//


class D3dFlowContextFlowSide : public D3dFlowContext
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    D3dFlowContextFlowSide(void);

    ~D3dFlowContextFlowSide(void);

    int Setup(
        Iterator * flow,        // d3dflow process iterator
        Iterator * mapper,      // mapper iterator
        int        aContextID,  // esm context-ID
        MemType    aMemType     // Shared mem or distributed
        );

    int SetupForGaws(
        Iterator * flow,        // d3dflow process iterator
        Iterator * gaws,        // gaws iterator
        int        aContextID,  // esm context-ID
        MemType    aMemType     // Shared mem or distributed
        );


    //
    // Distributed Data Communication,
    // Communication with Mapper-side
    //

    void SendSizesAndFlagsToMapper(void);
    void ReceiveAndCreateMapperStrips(void);

    void UpdateFlowToMapper  (UpdateHeader &updateHeader);
    void UpdateFlowFromMapper(UpdateHeader &updateHeader);

    void SendBlobToMapper(
        DDBlobID    blobID,
        int         numBytes,
        char      * bytes
        );

    int ReceiveBlobFromMapper(
        DDBlobID    blobID,
        int         numBytes,
        char      * bytes
        );

    //
    // Distributed Data Communication,
    // Communication with Gaws-side
    //

    void ReceiveAndCreateCommPoints(void);

    void UpdateFlowToGaws  (GawsDistribGroup distribGroup);
    void UpdateFlowFromGaws(bool left_2_right, GawsDistribGroup distribGroup);

    protected:

    void SendSizesToGaws(void);

    int BufferVarOnCommPoints(
        char          * buffer,        // buffer to be filled or read
        bool            left_2_right,  // L2R (true) or bottom->top (false)
        REAL_FP       * var,           // var (2D-array) to be stored or read
        BufferAction    bufferAction   // Fill or Read buffer
    );

    int BufferVarOnCommPoints_PREC(
        char          * buffer,        // buffer to be filled or read
        bool            left_2_right,  // L2R (true) or bottom->top (false)
        REAL_PREC     * var,           // var (2D-array) to be stored or read
        BufferAction    bufferAction   // Fill or Read buffer
    );


};
