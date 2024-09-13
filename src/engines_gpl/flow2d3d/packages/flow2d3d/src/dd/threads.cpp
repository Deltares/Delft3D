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
// $Id: threads.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/threads.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Threads for Domain Decomposition
//
//  Irv.Elshoff@Deltares.NL
//  4 jun 11
//------------------------------------------------------------------------------


#include "flow2d3d.h"


void
DD::InitThreads (
    void
    ) {

    static int id = this->ID_MAIN;

    // Create a thread-specific key for storing the object ID

    if (pthread_key_create (&this->thiter, NULL) != 0)
        throw new Exception (true, "Pthreads error: Cannot create thread-specific key: %s", strerror (errno));
    if (pthread_setspecific (this->thiter, (void *) &id) != 0)
        throw new Exception (true, "Pthreads error: Cannot set thread-specific key for main thread: %s", strerror (errno));

    // Get thread attributes

    if (pthread_attr_init (&this->thattr) != 0)
        throw new Exception (true, "Pthreads error: Cannot initialize thread attributes: %s", strerror (errno));

    // Set scheduling scope

    if (pthread_attr_setscope (&this->thattr, PTHREAD_SCOPE_SYSTEM) != 0)
        throw new Exception (true, "Pthreads error: Cannot set thread scope attribute: %s", strerror (errno));

    // Set stack size if an explicit value (in MB) is requested via the config tree

    size_t initsize;
    if (pthread_attr_getstacksize (&this->thattr, &initsize) != 0)
        throw new Exception (true, "Pthreads error: Cannot get initial stack size: %s", strerror (errno));

    size_t stacksize = this->config->GetIntegerAttrib ("Thread:StackSize");
    if (stacksize > 0) {
        if (stacksize > 100)
            throw new Exception (true, "Senseless thread stack size (%d > 100 MB)", stacksize);

        stacksize *= 1024*1024;
        if (pthread_attr_setstacksize (&this->thattr, stacksize) != 0)
            throw new Exception (true, "Pthreads error: Cannot set stack size to %d bytes: %s", stacksize, strerror (errno));

        size_t newsize;
        if (pthread_attr_getstacksize (&this->thattr, &newsize) != 0)
            throw new Exception (true, "Pthreads error: Cannot get new stack size: %s", strerror (errno));

        if (newsize != stacksize)
            this->log->Write (Log::ALWAYS, "Warning: Thread stack size (%d bytes) differs from requested size (%d bytes)", stacksize, newsize);
        }

    else
        stacksize = initsize;

    this->log->Write (Log::MINOR, "Thread stack size is %d bytes (%d MB)", stacksize, stacksize/ (1024*1024));
    }


int
DD::DDGetThreadID (
    void
    ) {

    int * idp;
    this->log->Write (Log::ITER_MINOR, "DDGetThreadID called");
    if ((idp = (int *) pthread_getspecific (this->thiter)) == NULL)
        throw new Exception (true, "Pthreads error: pthread_getspecific fails: %s", strerror (errno));

    return *idp;
    }
