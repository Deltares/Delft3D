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
// $Id: gawsbarrier.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/gawsbarrier.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  GAWS (Global ADI Wang Solver) Barrier - IMPLEMENTATION
//
//  Stef.Hummel@deltares.nl
//  Irv.Elshoff@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Iterator function


void
GawsBarrier_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // Determine Flow2D3D neighbors and setup communication with these participants

    GawsProcRelation gawsProcRelation [MAX_NUM_D3D_FLOW_PROCS];  // gaws process info
    int numD3dFlowProcesses = self->NeighborCount ();

    if (numD3dFlowProcesses > MAX_NUM_D3D_FLOW_PROCS)
        throw new Exception (true, "GAWS barrier has too many participants");

    self->RewindNeighbors ();
    for (int i = 0 ; i < numD3dFlowProcesses ; i++ ) {
        // Store relation, create and initialize DD-data communication
        gawsProcRelation[i].gawsBarrier  = self;
        gawsProcRelation[i].flowIterator = self->NextNeighbor ();
        gawsProcRelation[i].memType  = Mem_Distributed;
        }

    self->dd->log->Write (Log::ITER_MAJOR, "GAWS barrier initialized");

    self->Ready ();

    // SET UP COMMUNICATION PHASE
    // Initialize the gaws object (will also setup communication)

    self->dd->log->Write (Log::ITER_MAJOR, "GAWS barrier setting up communication");

    char * config = (char *) configblob->Address ();
    Gaws * gaws = new Gaws();
    gaws->Setup (self, config, numD3dFlowProcesses, gawsProcRelation);

    self->dd->log->Write (Log::ITER_MAJOR, "GAWS barrier starting simulation phase");

    // SIMULATION PHASE

    gaws->DoSolving();

    // WRAP UP

    self->dd->log->Write (Log::ITER_MAJOR, "GAWS barrier has terminated simulation phase");
    delete gaws;
    }
