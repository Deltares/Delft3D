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
// $Id: semaphore.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/semaphore.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Semaphore built on Pthreads - IMPLEMENTATION
//
//  Semaphores are implemented using POSIX thread primitives.
//  Each semaphore has a mutex that guards its object instantiation and
//  a condition variable for synchronization using the semaphore protocol.
//  Works only in a single process, definitely not multi-node.
//  Consider a Barrier for multi-process (and multi-node).
//
//  Irv.Elshoff@Deltares.NL
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Constructors and Destructor


Semaphore::Semaphore (
    const char *    semname,
    const int       initialValue,
    Log *           log
    ) {

    this->name = new char [strlen (semname)+1];
    strcpy (this->name, semname);
    this->value = initialValue;
    this->log = log;
    this->waiting = 0;

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        throw new Exception (true, "Pthreads error in Semaphore: pthread_mutex_init fails for semaphore, errno=%d", errno);
    if (pthread_cond_init (&this->syncv, NULL) != 0)
        throw new Exception (true, "Pthreads error in Semaphore: pthread_cond_init fails for semaphore, errno=%d", errno);

    if (this->log != NULL)
        this->log->Write (Log::CONFIG_MINOR, "Semaphore \"%s\" (0x%x) created", this->name, (char *) this);
    }


Semaphore::~Semaphore (
    void
    ) {

    delete [] this->name;

    if (this->log != NULL)
        this->log->Write (Log::CONFIG_MINOR, "Semaphore \"%s\" (0x%x) destroyed", this->name, (char *) this);
    }


//------------------------------------------------------------------------------
//   P and V


void
Semaphore::PSem (
    void
    ) {

    //  Get mutex to this semaphore object

    if (pthread_mutex_lock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error in Semaphore: pthread_mutex_lock fails in PSem \"%s\", errno=%d", this->name, errno);

    //  If the semaphore's value is zero, block until some thread does a V,
    //  otherwise decrement value.

    if (this->value == 0) {
        this->log->Write (Log::DD_SEMAPHORE, "PSem \"%s\" is zero, blocking", this->name);

        this->waiting++;
        if (pthread_cond_wait (&this->syncv, &this->mutex) != 0)
            throw new Exception (true, "Pthreads error in Semaphore: pthread_cond_wait fails in PSem \"%s\", errno=%d", this->name, errno);

        this->log->Write (Log::DD_SEMAPHORE, "PSem \"%s\" has been signalled, continuing", this->name);
        }

    else {
        this->log->Write (Log::DD_SEMAPHORE, "PSem \"%s\" is positive, decrementing and continuing", this->name);
        this->value--;
        }

    //  Relinquish mutex to my element in Semaphore array

    if (pthread_mutex_unlock (&this->mutex) != 0)
       throw new Exception (true, "Pthreads error in Semaphore: pthread_mutex_unlock fails in PSem \"%s\", errno=%d", this->name, errno);
    }


void
Semaphore::VSem (
    ) {

    //  Get mutex to Semaphore object

    if (pthread_mutex_lock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error in Semaphore: pthread_mutex_lock fails in VSem \"%s\", errno=%d", this->name, errno);

    //  Signal a waiting thread if there is one, or else increment semaphore

    if (this->waiting > 0) {
        this->log->Write (Log::DD_SEMAPHORE, "VSem \"%s\" signalling a blocked thread", this->name);
        this->waiting--;
        if (pthread_cond_signal (&this->syncv) != 0)
            throw new Exception (true, "Pthreads error in Semaphore: pthread_cond_signal fails in VSem \"%s\", errno=%d", this->name, errno);
        }
    else {
        this->log->Write (Log::DD_SEMAPHORE, "VSem \"%s\" incrementing semaphore", this->name);
        this->value++;
        }

    //  Relinquish mutex to my element in Semaphore array

    if (pthread_mutex_unlock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error in Semaphore: pthread_mutex_unlock fails in VSem \"%s\", errno=%d", this->name, errno);
    }
