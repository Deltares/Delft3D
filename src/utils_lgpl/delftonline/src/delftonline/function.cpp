//-------------------------------------------------------------------------------
//  DelftOnline -- Function Routines
//
//  Irv.Elshoff@Deltares.NL
//  24 may 12
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


//-------------------------------------------------------------------------------
//  Server-side Constructor/Destructor


Server::Function::Function (
    const char *    name,
    Directory *     dir,
    const char *    description,
    Callable        function,
    void *          context
    ) {

    this->pathname = new char [strlen (name) + strlen (dir->pathname) + 2];
    sprintf (this->pathname, "%s/%s", dir->pathname, name);

    this->name          = strdup (name);
    this->dir           = dir;
    this->description   = strdup (description);
    this->function      = function;
    this->context       = context;

    this->dir->functions->Append ((void *) this);
    }


Server::Function::~Function (
    void
    ) {

    this->dir->functions->Delete ((void *) this);

    free (this->name);
    free (this->description);
    }


//-------------------------------------------------------------------------------
//  Server-side API Functions


void
Server::PublishFunction (
    const char *    directory,
    const char *    name,
    const char *    description,
    Language        language,
    Callable        function,
    void *          context
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "PublishFunction");

    Function * func;
    dir->functions->Rewind ();
    while ((func = (Function *) dir->functions->Next ()) != NULL)
        if (strcmp (name, func->name) == 0)
            throw Error (true, "PublishFunction", "Function \"%s\" already exists in \"%s\"", name, dir->pathname);

    new Function (
            name,
            dir,
            description,
            function,
            context
            );

    this->log->Write (INFO, "Created function \"%s\" in \"%s\"", name, dir->pathname);
    UNLOCK
    }


void
Server::RetractFunction (
    const char *    directory,
    const char *    name
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "RetractFunction");

    Function * func;
    dir->arrays->Rewind ();
    while ((func = (Function *) dir->arrays->Next ()) != NULL)
        if (strcmp (name, func->name) == 0) {
            delete func;
            this->log->Write (INFO, "Retracted function \"%s\" in \"%s\"", name, dir->pathname);
            UNLOCK
            return;
            }

    throw Error (true, "RetractFunction", "Function \"%s\" does not exist in \"%s\"", name, dir->pathname);
    }


int
Server::CallFunction (
    Callable    funcaddr,
    void *      context,
    int         argument,
    int         language,
    int         thid
    ) {

    // Set up temporary thread
    // Find a slot in the thread table, store the ID, and attach to JVM

    int id;
    for (id = this->numThreads + 1 ; id < MaxThreads ; id++)
        if (this->thr[id]->id == THID_FREE)
            break;

    if (id >= MaxThreads)
        throw Error (true, "CallFunction", "No thread slots available");

    if (pthread_setspecific (this->thread, (void *) id) != 0)
        throw Error (true, "CallFunction", "Pthreads error: Cannot set thread-specific key, errno=%d", errno);

    this->thr[id]->id = thid;

    // Call the function

    int value;
    switch (language) {
        case C:
        case FORTRAN:
            value = (*funcaddr) (context, &argument);
            break;

        default:
            throw Error (true, "CallFunction", "Internal DOL error: Invalid language");
        }

    // Clean up temporary thread

    this->thr[id]->id = THID_FREE;
    return value;
    }


//-------------------------------------------------------------------------------
//  Server-side Utilities


Server::Function *
Server::LookupFunction (
    const char * pathname
    ) {

    if (pathname[0] != '/')
        throw Error (true, "LookupFunction", "Argument \"%s\" is not an absolute path name", pathname);

    char * pn = strdup (pathname);
    char * slash = strrchr (pn, '/');
    char * name = slash+1;
    Directory * dir;
    if (slash == pn)
        dir = this->rootDir;

    else {
        *slash = '\0';
        dir = LookupDirectory (pn);
        if (dir == NULL)
            throw Error (true, "LookupFunction", "Parent directory \"%s\" of function \"%s\" does not exist", pn, name);
        }

    dir->functions->Rewind ();
    Function * func;
    while ((func = (Function *) dir->functions->Next ()) != NULL)
        if (strcmp (name, func->name) == 0)
            break;

    free (pn);
    return func;
    }


//-------------------------------------------------------------------------------
//  Client-side Constructor/Destructor


Client::Function::Function (
    void
    ) {

    this->pathname    = NULL;
    this->description = NULL;
    this->context     = NULL;
    }


Client::Function::~Function (
    void
    ) {

    if (this->pathname    != NULL) free (this->pathname);
    if (this->description != NULL) free (this->description);
    }


//-------------------------------------------------------------------------------
//  Client-Server Marshalling Routines


size_t
Server::Function::Serialize (
    char *  buffer,
    size_t  bufsize
    ) {

    size_t size = sizeof (void *);
    if (size >= bufsize) return 0;

    void ** vp = (void **) buffer;
    *vp++ = this->context;

    char * cp = (char *) vp;

    SERIALIZE_STRING (this->pathname)
    SERIALIZE_STRING (this->description)

    return size;
    }


void
Client::Function::Unserialize (
    char *  buffer
    ) {

    void ** vp = (void **) buffer;
    this->context = *vp++;

    char * cp = (char *) vp;

    UNSERIALIZE_STRING (this->pathname)
    UNSERIALIZE_STRING (this->description)
    }

}
