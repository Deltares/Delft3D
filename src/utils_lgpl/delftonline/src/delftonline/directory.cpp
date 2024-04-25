//-------------------------------------------------------------------------------
//  DelftOnline -- Directory Routines
//
//  Irv.Elshoff@Deltares.NL
//  25 may 12
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


Server::Directory::Directory (
    const char * pathname,
    Directory * parent
    ) {

    this->pathname  = strdup (pathname);
    this->name      = strrchr (this->pathname, '/') + 1;
    this->parent    = parent;

    this->subdirs   = new LinkedList ();
    this->arrays    = new LinkedList ();
    this->elements  = new LinkedList ();
    this->functions = new LinkedList ();
    }


Server::Directory::~Directory (
    void
    ) {

    free (this->pathname);

    delete this->subdirs;
    delete this->arrays;
    delete this->elements;
    delete this->functions;
    }


//-------------------------------------------------------------------------------
//  Server-side API Functions


void
Server::CreateDirectory (
    const char * pathname
    ) {

    LOCK
    int id = this->GetThreadID ();
    const char * pn = ResolvePathName (pathname, this->thr[id]->curDir->pathname);

    Directory * parent;
    char * slash = strrchr ((char *) pn+1, '/');
    if (slash == NULL)
        parent = this->rootDir;
    else {
        *slash = '\0';
        parent = LookupDirectory (pn);
        if (parent == NULL)
            throw Error (true, "CreateDirectory", "Parent directory \"%s\" does not exist; cannot create \"%s\"", pn, pathname);

        *slash = '/';
        }

    Directory * subdir;
    parent->subdirs->Rewind ();
    while ((subdir = (Directory *) parent->subdirs->Next ()) != NULL)
        if (strcmp (pn, subdir->pathname) == 0)
            throw Error (true, "CreateDirectory", "Directory \"%s\" already exists", pn);

    Directory * dir = new Directory (pn, parent);
    parent->subdirs->Append ((void *) dir);

    this->log->Write (INFO, "Created directory \"%s\"", dir->pathname);
    UNLOCK
    }


void
Server::ChangeDirectory (
    const char * pathname
    ) {

    LOCK
    int id = this->GetThreadID ();

    const char * pn = ResolvePathName (pathname, this->thr[id]->curDir->pathname);
    Directory * dir = LookupDirectory (pn);
    if (dir == NULL)
        throw Error (true, "ChangeDirectory", "Directory \"%s\" does not exist", pn);

    this->thr[id]->curDir = dir;

    this->log->Write (INFO, "Changed to directory \"%s\"", this->thr[id]->curDir->pathname);
    UNLOCK
    }


//-------------------------------------------------------------------------------
//  Server-side Utilities


Server::Directory *
Server::GetDirectory (
    const char * directory,
    const char * moduleName
    ) {

    int id = this->GetThreadID ();

    if (directory == NULL || strcmp (directory, "") == 0)
        return this->thr[id]->curDir;

    const char * pn = ResolvePathName (directory, this->thr[id]->curDir->pathname);
    Directory * dir = this->LookupDirectory (pn);
    if (dir == NULL)
        throw Error (true, moduleName, "Directory \"%s\" does not exist", pn);

    return dir;
    }


Server::Directory *
Server::LookupDirectory (
    const char * pathname
    ) {

    if (pathname[0] != '/')
        throw Error (true, "LookupDirectory", "Argument \"%s\" is not an absolute path name", pathname);
    if (pathname[1] == '\0')
        return this->rootDir;

    char * pathnameSlash = new char [strlen (pathname) + 1];
    sprintf (pathnameSlash, "%s/", pathname+1);
    char * path = pathnameSlash;

    Directory * dir = this->rootDir;

    char * slash;
    while ((slash = strchr ((char *) path, '/')) != NULL) {
        const char * prefix = path;
        *slash = '\0';
        path = slash+1;

        dir->subdirs->Rewind ();
        Directory * subdir;
        while ((subdir = (Directory *) dir->subdirs->Next ()) != NULL)
            if (strcmp (prefix, subdir->name) == 0)
                break;

        if (subdir == NULL) {
            delete [] pathnameSlash;
            return NULL;
            }

        dir = subdir;
        }

    delete [] pathnameSlash;
    return dir;
    }


//-------------------------------------------------------------------------------
//  Client-side Constructor/Destructor


Client::Directory::Directory (
    void
    ) {

    this->pathname = NULL;

    this->subdirs.count   = 0;
    this->arrays.count    = 0;
    this->elements.count  = 0;
    this->functions.count = 0;
    }


Client::Directory::~Directory (
    void
    ) {

    if (this->pathname != NULL)
        free (this->pathname);

    for (int i = 0 ; i < this->subdirs.count ; i++)
        free (this->subdirs.name[i]);
    for (int i = 0 ; i < this->arrays.count ; i++)
        free (this->arrays.name[i]);
    for (int i = 0 ; i < this->elements.count ; i++)
        free (this->elements.name[i]);
    for (int i = 0 ; i < this->functions.count ; i++)
        free (this->functions.name[i]);
    }


//-------------------------------------------------------------------------------
//  Client-Server Marshalling Routines


int
Server::Directory::Serialize (
    char *  buffer,
    int     bufsize
    ) {

    int size = 4 * sizeof (int);
    if (size >= bufsize) return 0;

    int * ip = (int *) buffer;
    *ip++ = this->subdirs->Count ();
    *ip++ = this->arrays->Count ();
    *ip++ = this->elements->Count ();
    *ip++ = this->functions->Count ();

    char * cp = (char *) ip;

    SERIALIZE_STRING (this->pathname)

    Directory * subdir;
    this->subdirs->Rewind ();
    while ((subdir = (Directory *) this->subdirs->Next ()) != NULL)
        SERIALIZE_STRING (subdir->name)

    ArrayShape *  array;
    this->arrays->Rewind ();
    while ((array = (ArrayShape *) this->arrays->Next ()) != NULL)
        SERIALIZE_STRING (array->name)

    DataElement * elt;
    this->elements->Rewind ();
    while ((elt = (DataElement *) this->elements->Next ()) != NULL)
        SERIALIZE_STRING (elt->name)

    Function *    func;
    this->functions->Rewind ();
    while ((func = (Function *) this->functions->Next ()) != NULL)
        SERIALIZE_STRING (func->name)

    return size;
    }


void
Client::Directory::Unserialize (
    char *  buffer
    ) {

    int * ip = (int *) buffer;

    this->subdirs.count   = *ip++;
    this->arrays.count    = *ip++;
    this->elements.count  = *ip++;
    this->functions.count = *ip++;

    char * cp = (char *) ip;

    UNSERIALIZE_STRING (this->pathname)

    this->subdirs.name = new char * [this->subdirs.count];
    for (int i = 0 ; i < this->subdirs.count ; i++)
        UNSERIALIZE_STRING (this->subdirs.name[i])

    this->arrays.name = new char * [this->arrays.count];
    for (int i = 0 ; i < this->arrays.count ; i++)
        UNSERIALIZE_STRING (this->arrays.name[i])

    this->elements.name = new char * [this->elements.count];
    for (int i = 0 ; i < this->elements.count ; i++)
        UNSERIALIZE_STRING (this->elements.name[i])

    this->functions.name = new char * [this->functions.count];
    for (int i = 0 ; i < this->functions.count ; i++)
        UNSERIALIZE_STRING (this->functions.name[i])
    }

}
