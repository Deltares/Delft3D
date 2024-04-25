//-------------------------------------------------------------------------------
//  DelftOnline -- ArrayShape Routines
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


Server::ArrayShape::ArrayShape (
    const char * name,
    Directory * dir,
    int         dimensionality,
    int         dimensions[]
    ) {

    this->pathname = new char [strlen (name) + strlen (dir->pathname) + 2];
    sprintf (this->pathname, "%s/%s", dir->pathname, name);

    this->name  = strdup (name);
    this->dir   = dir;

    this->dimensionality = dimensionality;
    this->dimensions = new int [dimensionality];

    this->numelements = 1;
    for (int i = 0 ; i < dimensionality ; i++) {
        this->dimensions[i] = dimensions[i];
        this->numelements *= dimensions[i];
        }

    this->dir->arrays->Append ((void *) this);
    }


Server::ArrayShape::~ArrayShape (
    void
    ) {

    this->dir->arrays->Delete ((void *) this);

    free (this->name);
    delete [] this->dimensions;
    }


//-------------------------------------------------------------------------------
//  Server-side API Functions


void
Server::PublishArrayShape (
    const char *    directory,
    const char *    name,
    int             dimensionality,
    int             dimensions[]
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "ArrayShape");

    ArrayShape * ash;
    dir->arrays->Rewind ();
    while ((ash = (ArrayShape *) dir->arrays->Next ()) != NULL)
        if (strcmp (name, ash->name) == 0)
            throw Error (true, "PublishArrayShape", "ArrayShape \"%s\" already exists in \"%s\"", name, dir->pathname);

    new ArrayShape (
            name,
            dir,
            dimensionality,
            dimensions
            );

    this->log->Write (INFO, "Created %dD ArrayShape \"%s\" in \"%s\"", dimensionality, name, dir->pathname);
    UNLOCK
    }


void
Server::RetractArrayShape (
    const char *    directory,
    const char *    name
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "RetractArrayShape");

    ArrayShape * ash;
    dir->arrays->Rewind ();
    while ((ash = (ArrayShape *) dir->arrays->Next ()) != NULL)
        if (strcmp (name, ash->name) == 0) {
            delete ash;
            this->log->Write (INFO, "Retracted %dD ArrayShape \"%s\" in \"%s\"", ash->dimensionality, name, dir->pathname);
            UNLOCK
            return;
            }

    throw Error (true, "RetractArrayShape", "ArrayShape \"%s\" does not exist in \"%s\"", name, dir->pathname);
    }


//-------------------------------------------------------------------------------
//  Server-side Utilities


Server::ArrayShape *
Server::LookupArrayShape (
    const char * pathname
    ) {

    if (pathname[0] != '/')
        throw Error (true, "LookupArrayShape", "Argument \"%s\" is not an absolute path name", pathname);

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
            throw Error (false, "LookupArrayShape", "Parent directory \"%s\" of ArrayShape \"%s\" does not exist", pn, name);
        }

    dir->arrays->Rewind ();
    ArrayShape * ash = NULL;
    while ((ash = (ArrayShape *) dir->arrays->Next ()) != NULL)
        if (strcmp (name, ash->name) == 0)
            return ash;

    free (pn);
    throw Error (false, "LookupArrayShape", "ArrayShape \"%s\" not found", pathname);
    }


//-------------------------------------------------------------------------------
//  Client-side Constructor/Destructor


Client::ArrayShape::ArrayShape (
    void
    ) {

    this->pathname = NULL;
    this->dimensionality = 0;
    this->dimensions = NULL;
    }


Client::ArrayShape::~ArrayShape (
    void
    ) {

    if (this->pathname   != NULL) free (this->pathname);
    if (this->dimensions != NULL) delete [] this->dimensions;
    }


//-------------------------------------------------------------------------------
//  Client-Server Marshalling Routines


size_t
Server::ArrayShape::Serialize (
    char *  buffer,
    size_t  bufsize
    ) {

    size_t size = sizeof (int) * (1 + this->dimensionality);
    if (size >= bufsize) return 0;

    int * ip = (int *) buffer;

    *ip++ = this->dimensionality;

    for (int i = 0 ; i < this->dimensionality ; i++)
        *ip++ = this->dimensions[i];

    char * cp = (char *) ip;
    SERIALIZE_STRING (this->pathname)

    return size;
    }


void
Client::ArrayShape::Unserialize (
    char *  buffer
    ) {

    int * ip = (int *) buffer;

    this->dimensionality = *ip++;

    this->dimensions = new unsigned int [this->dimensionality];
    for (unsigned int i = 0 ; i < this->dimensionality ; i++)
        this->dimensions[i] = *ip++;

    char * cp = (char *) ip;
    UNSERIALIZE_STRING (this->pathname)
    }

}
