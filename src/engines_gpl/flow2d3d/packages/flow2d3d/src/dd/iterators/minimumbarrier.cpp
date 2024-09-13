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
// $Id: minimumbarrier.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/minimumbarrier.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Minimum Barrier - IMPLEMENTATION
//
//  NOTE:
//  There are actually two implementations of the minimum barrier, selected
//  at run time: one using an iterator, and one using Pthreads, perhaps with
//  an MPI reduce for distributed simulations.  MPI is presently disabled
//  because it's use here may conflict with it use in the "parallel" version
//  of Flow2D3D.  The Pthreads implementation is more efficcient, and is used
//  when it can, namelijk during single-process DD simulations.  Multi-process
//  simulation always use the iterator implementation for now.
//
//  ToDo: make pass functions thread safe
//
//  Irv.Elshoff@Deltares.NL
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"

#undef WITH_MPI
#undef NO_CPP_MPI


#define INFINITY    UINT_MAX


//------------------------------------------------------------------------------
//  Barriers are by definition a global object, so it's state is stored in a
//  global structure (well, almost global, one per node).


static struct {
    bool            initialized;
    bool            usingIterator;      // false means no iterator is involved in the implementation
    Iterator *      iterator;           // when using an iterator this is its handle
    int             localpart;          // total number of local (this node) participants
    int             curpart;            // number of current (running) participants
    unsigned int    localmin;           // current local minimum
    unsigned int    globalmin;          // global minimum after reduce
    pthread_mutex_t mutex;              // lock for access to this struct
    pthread_cond_t  sync;               // for local synchronization

#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
    MPI_Comm        intracomm;          // communicator for MPI implementation
#else
    MPI::Intracomm  intracomm;          // communicator for MPI implementation
#endif
#endif

    } MinimumBarrierState = { false };  // uninitialized at the outset


typedef struct {
    unsigned int value;
    } MinimumBarrierMesg;


//------------------------------------------------------------------------------
//  Initialization routines.  InitMinimumBarrier is called by the mappers
//  during the initialzation phase.  The first (per node) initializes the
//  global state data structure.  After all mapper iterators have called
//  their ready methods, the DD master (per node) calls SetupMinimumBarrier.


void
InitMinimumBarrier (
    void
    ) {

    if (! MinimumBarrierState.initialized) {
        MinimumBarrierState.usingIterator = (FLOW2D3D->dd->role != DD::ROLE_SINGLE);
        MinimumBarrierState.localpart     = 0;
        MinimumBarrierState.curpart       = 0;
        MinimumBarrierState.localmin      = INFINITY;
        MinimumBarrierState.globalmin     = INFINITY;

        Category * barcat = LookupCategory (DD::minBarCategoryName);
        if ((long) barcat == Dictionary::NOTFOUND)
            throw new Exception (true, "Cannot find the \"%s\" category", DD::minBarCategoryName);

        Iterator * self = IteratorSelf ();
        self->RewindNeighbors (barcat);
        MinimumBarrierState.iterator = self->NextNeighbor (barcat);
        if (MinimumBarrierState.iterator == NULL)
            throw new Exception (true, "Category \"%s\" does not have any neighbors", DD::minBarCategoryName);

        if (pthread_mutex_init (&MinimumBarrierState.mutex, NULL) != 0)
            throw new Exception (true, "Cannot create mutex for minumum barrier");
        if (pthread_cond_init (&MinimumBarrierState.sync, NULL) != 0)
            throw new Exception (true, "Cannot create condition variable for minumum barrier");

        MinimumBarrierState.initialized = true;
        }

    MinimumBarrierState.localpart++;
    }


void
SetupMinimumBarrier (
    void
    ) {

    if (! MinimumBarrierState.initialized)
        throw new Exception (true, "MinimumBarrier state not initialized in SetupMinimumBarrier");

    // Setup MPI communicator

    if (! MinimumBarrierState.usingIterator) {
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
        MPI_Comm_split (MPI_COMM_WORLD, MinimumBarrierState.localpart > 0, 0, &MinimumBarrierState.intracomm);
#else
        Minibar.intracomm = MPI::COMM_WORLD.Split (MinimumBarrierState.localpart > 0, 0);
#endif
#endif
        }
    }


//------------------------------------------------------------------------------
//  Iteration function


void
MinimumBarrier_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    ) {

    // INITIALIZATION PHASE
    // Determine barrier participants

    int npart = self->NeighborCount ();
    self->dd->log->Write (Log::ITER_MAJOR, "MinimumBarrier starting for %d participants", npart);

    Iterator ** part = new Iterator * [npart];
    self->RewindNeighbors ();
    for (int i = 0 ; i < npart ; i++)
        part[i] = self->NextNeighbor ();

    self->Ready ();

    // SIMULATION PHASE

    self->dd->log->Write (Log::ITER_MAJOR, "MinimumBarrier iterator starting simulation phase");

    MinimumBarrierMesg mesg;
    Blob * mblob = new Blob (&mesg, sizeof mesg);

    while (true) {
        // Get value from each neighbor and compute running minimum
        unsigned int minvalue = INFINITY;
        int i;
        for (i = 0 ; i < npart ; i++) {
            part[i]->Receive (mblob);
            if (mesg.value < minvalue) minvalue = mesg.value;
            }

        // Send minimum to all participants
        mesg.value = minvalue;
        for (i = 0 ; i < npart ; i++)
            part[i]->Send (mblob);
        }

    delete [] part;

    self->dd->log->Write (Log::ITER_MAJOR, "MinimumBarrier is finished");
    }


//------------------------------------------------------------------------------


unsigned int
MinimumBarrier (
    unsigned int value
    ) {

    if (MinimumBarrierState.usingIterator) {
        MinimumBarrierMesg mesg;
        Blob * mblob = new Blob (&mesg, sizeof mesg);
        mesg.value = value;

        MinimumBarrierState.iterator->Send (mblob);
        MinimumBarrierState.iterator->Receive (mblob);

        unsigned int rv = mesg.value;
        delete mblob;
        return rv;
        }

    else {
        if (pthread_mutex_lock (&MinimumBarrierState.mutex) != 0)
            throw new Exception (true, "Pthreads error: pthread_mutex_lock fails in MinimumBarrier, errno=%d", errno);

        if (value < MinimumBarrierState.localmin)
            MinimumBarrierState.localmin = value;

        if (++MinimumBarrierState.curpart < MinimumBarrierState.localpart) {
            // More local participants are coming; wait for them
            if (pthread_cond_wait (&MinimumBarrierState.sync, &MinimumBarrierState.mutex) != 0)
                throw new Exception (true, "Pthreads error: pthread_cond_wait fails in MinimumBarrier, errno=%d", errno);
            }

        else {
            // This is the last local participant in this round on this MPI process.
            // Do an MPI reduce and then wake up all the other local participants

            if (FLOW2D3D->dd->role == DD::ROLE_SINGLE)
                MinimumBarrierState.globalmin = MinimumBarrierState.localmin;

            else {
#if defined (WITH_MPI)
#if defined (NO_CPP_MPI)
                MPI_Allreduce (&MinimumBarrierState.localmin, &MinimumBarrierState.globalmin, 1, MPI_UNSIGNED, MPI_MIN, MinimumBarrierState.intracomm);
#else
                MinimumBarrierState.intracomm.Allreduce (&MinimumBarrierState.localmin, &MinimumBarrierState.globalmin, 1, MPI::UNSIGNED, MPI::MIN);
#endif
#endif
                }

            MinimumBarrierState.curpart = 0;
            MinimumBarrierState.localmin = INFINITY;

            // signal all
            if (pthread_cond_broadcast (&MinimumBarrierState.sync) != 0)
                throw new Exception (true, "Pthreads error: pthread_cond_broadcast fails in MinimumBarrier, errno=%d", errno);
            }

        if (pthread_mutex_unlock (&MinimumBarrierState.mutex) != 0)
            throw new Exception (true, "Pthreads error: pthread_mutex_unlock fails in MinimumBarrier, errno=%d", errno);

        return MinimumBarrierState.globalmin;
        }
    }
