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
// $Id: hymapper.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/hymapper.cpp $
//------------------------------------------------------------------------------
//  Module: Hydra D3dFlowMapper functions
//  Functions to be called by D3D-Flow-DD Executive to creating and
//  activate a D3dFlowMapper.
//
//  Stef.Hummel@deltares.nl
//  Adri.Mourits@deltares.nl
//  6 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


void FlowDD_Mapper (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // Check/get neighbor processes

    Category * processes = LookupCategory (DD::processCategoryName);
    if (processes == NULL)
        throw new Exception (true, "Process category does not exist for mapper \"%s\"", name);

    int numNeighbors = self->NeighborCount (processes);
    if (numNeighbors != NR_CNTXTS)
        throw new Exception (true, "Mapper \"%s\" does not have two neighbor processes", name);

    Iterator * neighbors[NR_CNTXTS];

    self->RewindNeighbors (processes);
    neighbors[C_0]  = self->NextNeighbor (processes);  // left-side process
    neighbors[C_1]  = self->NextNeighbor (processes);  // right-side process

    // Determine data exchange type between Mapper and Flow

    MemType memType[NR_CNTXTS];
    for (int ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        memType[ctx] = Mem_Distributed;

    // Log results:

    char * configString = (char *) configblob->Address ();

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "Mapper \"%s\":", name);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "    config        = \"%s\"", configString);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "    numNeigh      = %d", self->NeighborCount ());
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "    left          = \"%s\", memType = %d", (neighbors[C_0] == NULL) ? "???" : neighbors[C_0]->name, memType[C_0]);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "    right         = \"%s\", memType = %d", (neighbors[C_1] == NULL) ? "???" : neighbors[C_1]->name, memType[C_1]);

    // Create and Setup Mapper and ContextData objects

    D3dFlowMapper * d3dFlowMapper = new D3dFlowMapper();

    InitMinimumBarrier ();
    self->Ready ();

    int retVal =  d3dFlowMapper->Setup(self, configString, neighbors, memType);

    if ( retVal != HY_ERR )
        retVal =  d3dFlowMapper->DoMapping();

    delete d3dFlowMapper;
    }
