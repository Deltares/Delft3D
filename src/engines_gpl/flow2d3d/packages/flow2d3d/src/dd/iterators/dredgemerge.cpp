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
// $Id: dredgemerge.cpp 945 2011-10-27 15:44:06Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/dredgemerge.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Dredge Merge - IMPLEMENTATION
//
//  ToDo: make pass functions thread safe
//
//  Adri.Mourits@Deltares.NL
//  Irv.Elshoff@Deltares.NL
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Iterator function to implement dredge merge


void
DredgeMerge_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // INITIALIZATION PHASE
    // Determine participants

    unsigned int numelements = 0;
    int npart = self->NeighborCount ();
    self->dd->log->Write (Log::ITER_MAJOR, "DredgeMerge starting for %d participants", npart);

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

    self->dd->log->Write (Log::ITER_MAJOR, "DredgeMerge iterator starting simulation phase");

    // First time only: Receive numelements from all subdomains

    int inimesg[2];
    Blob * initBlob = new Blob (inimesg, sizeof inimesg);
    for (int i = 0 ; i < npart ; i++) {
        bool firstDredgeDomain = true;
        int blobtype;
        part[i]->Receive (initBlob, &blobtype);
        if (blobtype != DredgeMerge::F2DM_init)
            throw new Exception (true, "Unexpected message (%d) from Flow %s",blobtype,part[i]->name);

        if (inimesg[0] == -1)
            part[i] = NULL; // this neighbor has nothing to do with dredge and dump, remove from list

        else {
            if (! firstDredgeDomain && numelements != inimesg[0])
                throw new Exception (true, "Inconsistent number of dredging elements received by DredgeMergeFunction");

            numelements = inimesg[0];
            firstDredgeDomain = false;
            }
        }

    // Clean up array part

    bool eltremoved = true;
    while (eltremoved) {
        int  delelt = -1;
        int  i      =  0;
        eltremoved = false;
        while (delelt == -1 && i < npart) {
            if (part[i] == NULL)
                delelt = i;
            i++;
            }
        if (delelt != -1) {
            for (i = delelt ; i < npart-1 ; i++)
                part[i] = part[i+1];

            npart--;
            eltremoved = true;
            }
        }

    // Communicate npart to (remaining) participants

    inimesg[1] = npart;
    for (int ipart = 0 ; ipart < npart ; ipart++) {
        inimesg[0] = ipart;
        part[ipart]->Send (initBlob, DredgeMerge::DM2F_init);
        }

    delete initBlob;

    // Main processing loop
    // Only when there are at least two partitions left

    if (npart >= 2) {
        int numeldomain;
        Blob * sblob  = new Blob (&numeldomain, sizeof numeldomain);

        while (true) {
            int iarr;
            int ipart;

            // Get value from each neighbor and compute the sum,
            // for each array element numelements = 0;

            for (ipart = 0 ; ipart < npart ; ipart++) {
                int blobtype;
                part[ipart]->Receive (sblob, &blobtype);
                if (blobtype != DredgeMerge::F2DM_voldred)
                    throw new Exception (true, "Unexpected message (%d) from Flow %s",blobtype,part[ipart]->name);

                if (ipart == 0)
                    numelements = numeldomain;
                else if (numeldomain != numelements)
                    throw new Exception (true, "Dredge array size (%d) from Flow %s deviates from size (%d) from Flow %s", numeldomain,part[ipart]->name, numelements,part[0]->name);
                }

            REAL_FP *mesg = new REAL_FP[numelements];
            Blob * mblob  = new Blob (mesg, sizeof(REAL_FP)* numelements);

            // Allocate the array that is going to contain the merged values

            REAL_FP *mergedvoldred = new REAL_FP[numelements];
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mergedvoldred[iarr] = 0.0;

            for (ipart = 0 ; ipart < npart ; ipart++) {
                int blobtype;
                part[ipart]->Receive (mblob, &blobtype);
                if (blobtype != DredgeMerge::F2DM_voldred)
                    throw new Exception (true, "Unexpected message (%d) from Flow %s", blobtype,part[ipart]->name);
                for (iarr = 0 ; iarr < numelements ; iarr++)
                    mergedvoldred[iarr] += mesg[iarr];
                }

            // Send merged values to all participants by reusing mesg

            for (iarr = 0 ; iarr < numelements ; iarr++)
                mesg[iarr] = mergedvoldred[iarr];
            for (ipart = 0 ; ipart < npart ; ipart++)
                part[ipart]->Send (mblob, DredgeMerge::DM2F_mergedvoldred);

            delete mblob;
            delete [] mesg;
            delete [] mergedvoldred;
            }

        delete sblob;
        }
    delete [] part;

    self->dd->log->Write (Log::ITER_MAJOR, "DredgeMerge iterator done");
    }


//------------------------------------------------------------------------------
//  Function as called from dredge.f90


extern "C" {
void STDCALL
START_DD_DREDGECOMMUNICATION (
    int     * domainnumber,
    int     * numdomains
    ) {

    Iterator * dredgemIterator = FLOW2D3D->dd->dredgem;

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in DD_DredgeCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        throw new Exception (true, "Cannot get subdomain globals pointer in DD_DredgeCommunicate");

    subdomglobals->dredgefirst = false;

    // Send numelements to DredgeDump iterator
    // Use a copy of the integer obtained from the Fortran side

    int inimesg[2] = { 1, 1 };
    Blob * initBlob = new Blob (inimesg, sizeof inimesg);
    dredgemIterator->Send (initBlob, DredgeMerge::F2DM_init);

    // Receive the number of subdomains using DredgeDump
    // initBlob and inimesg are reused

    int blobtype;
    dredgemIterator->Receive (initBlob, &blobtype);
    if (blobtype != DredgeMerge::DM2F_init)
        throw new Exception (true, "Unexpected message (%d) in %s from DredgeMerge",blobtype,self->name);

    *domainnumber = inimesg[0];
    *numdomains = inimesg[1];

    delete initBlob;

    if (*numdomains <= 1)
        subdomglobals->dredgecommunicate = false;  // No DredgeDump communication
    }
}


//------------------------------------------------------------------------------
//  Function called from dredge.f90


extern "C" {
void STDCALL
DD_DREDGECOMMUNICATE (
    REAL_FP * voldred,
    int     * numelements
    ) {

    Iterator * dredgemIterator = FLOW2D3D->dd->dredgem;

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in DD_DredgeCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        throw new Exception (true, "Cannot get subdomain globals pointer in DD_DredgeCommunicate");

    // Return immediately unless we're enabled

    if (! subdomglobals->dredgecommunicate) return;

    int mesgsize = *numelements;
    Blob * sblob = new Blob (&mesgsize, sizeof mesgsize);
    dredgemIterator->Send (sblob, DredgeMerge::F2DM_voldred);

    REAL_FP *mesg = new REAL_FP[*numelements];
    Blob * mblob = new Blob (mesg, sizeof(REAL_FP)* *numelements);

    // Copy voldred to mesg

    for (int i = 0 ; i < *numelements ; i++)
       mesg[i] = voldred[i];

    // Send mesg to dredge merge iterator

    dredgemIterator->Send (mblob, DredgeMerge::F2DM_voldred);

    // Receive the merged volumes from the dredge merge iterator; mblob and mesg are reused

    int blobtype;
    dredgemIterator->Receive (mblob,&blobtype);
    if (blobtype != DredgeMerge::DM2F_mergedvoldred)
        throw new Exception (true, "Unexpected message (%d) in %s from DredgeMerge",blobtype,self->name);

    for (int i = 0 ; i < *numelements ; i++)
       voldred[i] = mesg[i];

    delete mblob;
    delete sblob;
    delete [] mesg;
    }
}


//------------------------------------------------------------------------------
//  Function called from dimpro.f90


extern "C" {
void STDCALL
NO_DD_DREDGECOMMUNICATION (
    void
    ) {

    Iterator * dredgemIterator = FLOW2D3D->dd->dredgem;

    // Send -1 to Dredge/Merge iterator

    int inimesg[2] = { -1, -1 };
    Blob * initBlob = new Blob (inimesg, sizeof inimesg);
    dredgemIterator->Send (initBlob, DredgeMerge::F2DM_init);
    delete initBlob;
    }
}
