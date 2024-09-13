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
// $Id: rtccomm.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/rtccomm.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Inter-domain RTC module communication synchronization - IMPLEMENTATION
//
//  ToDo: make pass functions thread safe
//
//  Adri.Mourits@deltares.nl
//  Irv.Elshoff@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Iterator function to implement rtc communicate

void
Rtc_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    unsigned int numelements = 0;

    // INITIALIZATION PHASE
    // Determine RTC participants

    int npart = self->NeighborCount ();
    self->dd->log->Write (Log::ITER_MAJOR, "RtcComm starting for %d participants", npart);

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

    self->dd->log->Write (Log::MAJOR, "RtcComm iterator starting simulation phase");

    // First time only: Receive numelements from all subdomains

    int inimesg[2];
    Blob * initBlob = new Blob (inimesg, sizeof(int)*2);
    for (int i = 0 ; i < npart ; i++) {
        bool firstRtc = true;
        int blobtype;
        part[i]->Receive (initBlob, &blobtype);
        if (blobtype != RTC::F2RC_init)
            throw new Exception (true, "Unexpected message (%d) from Flow %s",blobtype,part[i]->name);

        if (inimesg[0] == -1)
            part[i] = NULL;  // This neighbor has nothing to do with RTC; remove from list
        else {
            if (! firstRtc && numelements != inimesg[0])
                throw new Exception (true, "Inconsistent number of dredging elements received by RtcFunction");

            numelements = inimesg[0];
            firstRtc = false;
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
        if (delelt != -1)  {
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
        part[ipart]->Send (initBlob, RTC::RC2F_init);
        }

    delete initBlob;

    // Number of subdomains with rtc <= 1 means this RTC iterator can stop

    if (npart <= 1) return;

    // Main processing loop

    int numeldomain;
    Blob * sblob  = new Blob (&numeldomain, sizeof(int));

    while (true) {
        int iarr;
        int ipart;
        int currentblobtype;

        // Get value from each neighbor and compute the sum,
        // for each array element

        numelements = 0;
        for (ipart = 0 ; ipart < npart ; ipart++) {
            int blobtype;
            part[ipart]->Receive (sblob, &blobtype);
            if (blobtype != RTC::F2RC_values && blobtype != RTC::F2RC_strings)
                throw new Exception (true, "Unexpected message (%d) from Flow %s",blobtype,part[ipart]->name);

            if (ipart == 0) {
                currentblobtype = blobtype;
                numelements = numeldomain;
                }
            else if (numeldomain != numelements)
                throw new Exception (true, "RTC array size (%d) from Flow %s deviates from size (%d) from Flow %s",
                                            numeldomain, part[ipart]->name, numelements, part[0]->name);
            else if (currentblobtype != blobtype)
                throw new Exception (true, "RTC blob type (%d) from Flow %s deviates from blob type (%d) from Flow %s",
                                            blobtype, part[ipart]->name, currentblobtype, part[0]->name);
            }

        if (currentblobtype ==  RTC::F2RC_values) {
            REAL_FP *mesg = new REAL_FP[numelements];
            Blob * mblob  = new Blob (mesg, sizeof(REAL_FP)* numelements);

            // Allocate the array that is going to contain the merged values

            REAL_FP *mergedvalues = new REAL_FP[numelements];
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mergedvalues[iarr] = 0.0;

            for (ipart = 0 ; ipart < npart ; ipart++) {
                int blobtype;
                part[ipart]->Receive (mblob, &blobtype);
                if (blobtype != RTC::F2RC_values)
                    throw new Exception (true, "Unexpected message (%d) from Flow %s", blobtype, part[ipart]->name);
                for (iarr = 0 ; iarr < numelements ; iarr++)
                    mergedvalues[iarr] += mesg[iarr];
                }

            // Send merged values to all participants by reusing mesg

            for (iarr = 0 ; iarr < numelements ; iarr++)
                mesg[iarr] = mergedvalues[iarr];
            for (ipart = 0 ; ipart < npart ; ipart++)
                part[ipart]->Send (mblob, RTC::RC2F_values);

            delete [] mesg;
            delete [] mergedvalues;
            delete mblob;
            }

        else if (currentblobtype == RTC::F2RC_strings) {
            char *mesg = new char[numelements];
            Blob * mblob  = new Blob (mesg, sizeof(char)*numelements);

            // Allocate the array that is going to contain the merged values

            char *mergedvalues = new char[numelements];
            for (iarr = 0 ; iarr < numelements ; iarr++)
                mergedvalues[iarr] = ' ';

            for (ipart = 0 ; ipart < npart ; ipart++) {
                int blobtype;
                part[ipart]->Receive (mblob, &blobtype);
                if (blobtype != RTC::F2RC_strings)
                    throw new Exception (true, "Unexpected message (%d) from Flow %s", blobtype, part[ipart]->name);
                for (iarr = 0 ; iarr < numelements ; iarr++)
                    if (mesg[iarr] != ' ')
                        mergedvalues[iarr] = mesg[iarr];
                }

            // Send merged values to all participants by reusing mesg

            for (iarr = 0 ; iarr < numelements ; iarr++)
                mesg[iarr] = mergedvalues[iarr];
            for (ipart = 0 ; ipart < npart ; ipart++)
                part[ipart]->Send (mblob, RTC::RC2F_strings);

            delete [] mesg;
            delete [] mergedvalues;
            delete mblob;
            }
        }

    delete [] part;

    self->dd->log->Write (Log::ITER_MAJOR, "RtcComm is finished");
    }


//------------------------------------------------------------------------------
//  Function called from rtc_comm_init.f90


extern "C" {
void STDCALL
RTCSTARTCOMMUNICATION (
    int * domainnumber,
    int * numdomains
    ) {

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in RtcStartCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        throw new Exception (true, "Cannot get subdomain globals pointer in RtcStartCommunicate");

    Iterator * rtcIterator = FLOW2D3D->dd->rtc;

    subdomglobals->rtcfirst = false;

    // Send numelements to Rtc iterator
    // Use a copy of the integer obtained from the Fortran side

    int inimesg[2] = { 1, 1 };
    Blob * initBlob = new Blob (inimesg, sizeof(int)*2);
    rtcIterator->Send (initBlob, RTC::F2RC_init);

    // Receive the number of subdomains using Rtc
    // initBlob and inimesg are reused

    int blobtype;
    rtcIterator->Receive (initBlob, &blobtype);
    if (blobtype != RTC::RC2F_init)
        throw new Exception (true, "Unexpected message (%d) in %s from Rtc",blobtype,self->name);

    *domainnumber = inimesg[0];
    *numdomains = inimesg[1];

    delete initBlob;

    if (*numdomains <= 1)
        subdomglobals->rtccommunicate = false;  // No Rtc communication
    }
}


//------------------------------------------------------------------------------
//  Function called from rtc_comm_init.f90


extern "C" {
void STDCALL
RTCCOMMUNICATE (
    REAL_FP * values,
    int     * numelements
    ) {

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in RtcCommunicate");

    SubdomGlobals *subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        throw new Exception (true, "Cannot get subdomain globals pointer in RtcCommunicate");

    // Return when rtccommunicate is explicitly switched to false

    if (! subdomglobals->rtccommunicate) return;

    Iterator * rtcIterator = FLOW2D3D->dd->rtc;

    int mesgsize = *numelements;
    Blob *sblob = new Blob (&mesgsize, sizeof(int));
    rtcIterator->Send (sblob, RTC::F2RC_values);

    REAL_FP *mesg = new REAL_FP[mesgsize];
    Blob * mblob = new Blob (mesg, sizeof(REAL_FP)*mesgsize);

    // Copy values to mesg
    for (int i = 0 ; i < mesgsize ; i++)
       mesg[i] = values[i];

    // Send mesg to rtc iterator

    rtcIterator->Send (mblob, RTC::F2RC_values);

    // Receive the merged volumes from the rtc iterator
    // mblob and mesg are reused

    int blobtype;
    rtcIterator->Receive (mblob, &blobtype);
    if (blobtype != RTC::RC2F_values)
        throw new Exception (true, "Unexpected message (%d) in %s from Rtc",blobtype,self->name);

    for (int i = 0 ; i < mesgsize ; i++)
       values[i] = mesg[i];

    delete [] mesg;
    delete mblob;
    }
}


//------------------------------------------------------------------------------
//  Function  called from rtc_comm_init.f90


extern "C" {
void STDCALL
RTCCHARCOMMUNICATE (
    char    * strings,
    int     * numelements,
    int     numchar
    ) {

    Iterator * self = IteratorSelf ();
    if (self == NULL)
        throw new Exception (true, "Cannot get iterator self in RtcCharCommunicate");

    SubdomGlobals * subdomglobals = NULL;
    if ((subdomglobals = (SubdomGlobals *) self->GetValue ()) == NULL)
        throw new Exception (true, "Cannot get subdomain globals pointer in RtcCharCommunicate");

    // Return when rtccommunicate is explicitly switched to false

    if (! subdomglobals->rtccommunicate) return;

    Iterator * rtcIterator = FLOW2D3D->dd->rtc;

    int mesgsize = *numelements * numchar;
    Blob *sblob = new Blob (&mesgsize, sizeof(int));
    rtcIterator->Send (sblob, RTC::F2RC_strings);

    char *mesg = new char[mesgsize];
    Blob *mblob = new Blob (mesg, sizeof(char)*mesgsize);
    // Copy strings to mesg
    for (int i = 0 ; i < mesgsize ; i++)
       mesg[i] = strings[i];

    // Send mesg to rtc iterator

    rtcIterator->Send (mblob, RTC::F2RC_strings);

    // Receive the merged strings from the rtc iterator
    // mblob and mesg are reused

    int blobtype;
    rtcIterator->Receive (mblob, &blobtype);
    if (blobtype != RTC::RC2F_strings)
        throw new Exception (true, "Unexpected message (%d) in %s from Rtc",blobtype,self->name);

    for (int i = 0 ; i < mesgsize ; i++)
       strings[i] = mesg[i];

    delete [] mesg;
    delete mblob;
    }
}


//------------------------------------------------------------------------------
//  Function called from rtc_comm_init.f90


extern "C" {
void STDCALL
RTCNOCOMMUNICATION (
    void
    ) {

    Iterator * rtcIterator = FLOW2D3D->dd->rtc;

    // send -1 to Rtc iterator

    int inimesg[2] = { -1, -1 };
    Blob * initBlob = new Blob (inimesg, sizeof(int)*2);
    rtcIterator->Send (initBlob, RTC::F2RC_init);
    delete initBlob;
    }
}
