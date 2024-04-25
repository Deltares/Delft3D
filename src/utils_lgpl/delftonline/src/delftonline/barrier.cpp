//-------------------------------------------------------------------------------
//  DelftOnline -- Barrier for DOL Server Threads
//
//  Irv.Elshoff@Deltares.NL
//  2 jul 11
//-------------------------------------------------------------------------------
//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id$
// $HeadURL$


#include "dol.h"

namespace DOL {


Barrier::Barrier (
    int numParticipants
    ) {

    this->numParticipants = numParticipants;
    this->numArrived = 0;

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        throw new Exception (true, "Cannot create mutex for barrier: %s", strerror (errno));
    if (pthread_cond_init (&this->sync, NULL) != 0)
        throw new Exception (true, "Cannot create condition variable for barrier: %s", strerror (errno));
    }


Barrier::~Barrier (
    void
    ) {

    }


void
Barrier::Pass (
    void
    ) {



    if (pthread_mutex_lock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error: pthread_mutex_lock fails in Barrier::Pass: %s", strerror (errno));

    if (++this->numArrived < this->numParticipants) {
        // wait for more participants
        if (pthread_cond_wait (&this->sync, &this->mutex) != 0)
            throw new Exception (true, "Pthreads error: pthread_cond_wait fails in Barrier::Pass %s", strerror (errno));
        }

    else {
        // everyone has arrived; reset and signal all

        this->numArrived = 0;
        if (pthread_cond_broadcast (&this->sync) != 0)
            throw new Exception (true, "Pthreads error: pthread_cond_broadcast fails in Barrier::Pass %s", strerror (errno));
        }

    if (pthread_mutex_unlock (&this->mutex) != 0)
        throw new Exception (true, "Pthreads error: pthread_mutex_unlock fails in Barrier::Pass %s", strerror (errno));
    }

}
