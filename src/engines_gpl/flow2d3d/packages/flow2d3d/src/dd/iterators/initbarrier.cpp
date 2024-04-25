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
// $Id: initbarrier.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/initbarrier.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Initial inter-domain synchronization - IMPLEMENTATION
//
//  Adri.Mourits@deltares.nl
//  Irv.Elshoff@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Iterator function


void
InitBarrier_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // INITIALIZATION PHASE
    // Determine number of neighbors (= number of subdomains)

    int npart = self->NeighborCount ();
    self->dd->log->Write (Log::ITER_MAJOR, "InitBarrier starting for %d participants", npart);

    // If we're the only participant go away

    if (npart <= 1) {
        self->Ready ();
        return;
        }

    Iterator ** part = new Iterator * [npart];
    self->RewindNeighbors ();
    for (int i = 0 ; i < npart ; i++)
        part[i] = self->NextNeighbor ();

    self->Ready ();

    // SIMULATION PHASE

    self->dd->log->Write (Log::ITER_MAJOR, "InitBarrier iterator starting simulation phase");

    // First time only: Receive a dummy integer from all subdomains

    int inimesg;
    Blob * initBlob = new Blob (&inimesg, sizeof inimesg);
    for (int i = 0 ; i < npart ; i++) {
        int blobtype;
        part[i]->Receive (initBlob, &blobtype);
        if (blobtype != InitBarrier::F2IB_initFinished)
            throw new Exception (true, "Unexpected message (%d) from Flow %s to InitBarrier",blobtype,part[i]->name);
        }

    // Communicate dummy integer to all participants so they can continue

    inimesg = 1;
    for (int ipart = 0 ; ipart < npart ; ipart++)
        part[ipart]->Send (initBlob, InitBarrier::IB2F_startSimulation);

    delete initBlob;
    delete [] part;

    self->dd->log->Write (Log::ITER_MAJOR, "InitBarrier is finished");
    }


//------------------------------------------------------------------------------
//  Function called from tricom.f90


extern "C" {
void STDCALL
INITFINISHED (
    int * numdomains
    ) {

    if (*numdomains == 1) return;

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in InitFinished");

    Iterator * initbarIterator = FLOW2D3D->dd->initbar;

    // Send dummy integer to InitBarrier iterator

    int inimesg = 1;
    Blob * initBlob = new Blob (&inimesg, sizeof inimesg);
    initbarIterator->Send (initBlob, InitBarrier::F2IB_initFinished);

    // Receive a dummy integer from InitBarrier iterator
    // initBlob and inimesg are reused

    int blobtype;
    initbarIterator->Receive (initBlob, &blobtype);
    if (blobtype != InitBarrier::IB2F_startSimulation)
        throw new Exception (true, "Unexpected message (%d) in %s from InitBarrier",blobtype,self->name);

    delete initBlob;
    }
}
