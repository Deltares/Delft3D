//-------------------------------------------------------------------------------
//  DelftOnline -- DataElement Routines
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


Server::DataElement::DataElement (
    const char *    name,
    Directory *     dir,
    const char *    description,
    const char *    units,
    const char *    definedon,
    ArrayShape *    ash,
    BaseType        basetype,
    AccessMode      inout,
    void *          address
    ) {

    this->pathname = new char [strlen (name) + strlen (dir->pathname) + 2];
    sprintf (this->pathname, "%s/%s", dir->pathname, name);

    this->name          = strdup (name);
    this->dir           = dir;
    this->description   = strdup (description);
    this->units         = strdup (units);
    this->definedon     = strdup (definedon);
    this->arrayshape    = ash;
    this->basetype      = (BaseType) basetype;
    this->inout         = (AccessMode) inout;
    this->address       = address;

    if (ash == NULL) {
        this->arrayelements = 0;
        this->size          = BaseTypeSize (basetype);
        }
    else {
        this->arrayelements = ash->numelements;
        this->size          = ash->numelements * BaseTypeSize (basetype);
        }

    this->dir->elements->Append ((void *) this);
    }


Server::DataElement::~DataElement (
    void
    ) {

    this->dir->elements->Delete ((void *) this);

    free (this->name);
    free (this->description);
    free (this->units);
    free (this->definedon);

    delete [] this->pathname;
    }


//-------------------------------------------------------------------------------
//  Server-side API Functions


void
Server::Publish (
    const char *    directory,
    const char *    name,
    const char *    description,
    const char *    units,
    const char *    definedon,
    const char *    arrayshape,
    BaseType        basetype,
    void *          address,
    AccessMode      inout
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "Publish");

    DataElement * elt;
    dir->elements->Rewind ();
    while ((elt = (DataElement *) dir->elements->Next ()) != NULL)
        if (strcmp (name, elt->name) == 0)
            throw Error (true, "Publish", "Data element \"%s\" already exists in \"%s\"", name, dir->pathname);

    ArrayShape * ash = NULL;
    if (arrayshape != NULL && strcmp (arrayshape, "") != 0) {
        const char * ashpath = ResolvePathName (arrayshape, dir->pathname);
        ash = this->LookupArrayShape (ashpath);
        if (ash == NULL)
            throw Error (true, "Publish", "ArrayShape \"%s\" does not exist; cannot publish data element \"%s\"", ashpath, name);
        }

    new DataElement (
            name,
            dir,
            description,
            units,
            definedon,
            ash,
            basetype,
            inout,
            address
            );

    this->log->Write (INFO, "Created data element \"%s\" in \"%s\"", name, dir->pathname);
    UNLOCK
    }


void
Server::Retract (
    const char *    directory,
    const char *    name
    ) {

    LOCK
    Directory * dir = GetDirectory (directory, "Retract");

    DataElement * elt;
    dir->elements->Rewind ();
    while ((elt = (DataElement *) dir->elements->Next ()) != NULL)
        if (strcmp (name, elt->name) == 0) {
            delete elt;
            this->log->Write (INFO, "Retracted data element \"%s\" in \"%s\"", name, dir->pathname);
            UNLOCK
            return;
            }

    throw Error (true, "Retract", "Data element \"%s\" does not exist in \"%s\"", name, dir->pathname);
    }


//-------------------------------------------------------------------------------
//  Server-side Utilities


Server::DataElement *
Server::LookupDataElement (
    const char * pathname
    ) {

    if (pathname[0] != '/')
        throw Error (true, "LookupDataElement", "Argument \"%s\" is not an absolute path name", pathname);

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
            throw Error (true, "Lookupv", "Parent directory \"%s\" of data element \"%s\" does not exist", pn, name);
        }

    dir->elements->Rewind ();
    DataElement * elt;
    while ((elt = (DataElement *) dir->elements->Next ()) != NULL)
        if (strcmp (name, elt->name) == 0)
            break;

    free (pn);
    return elt;
    }


//-------------------------------------------------------------------------------
//  Client-side Constructor/Destructor


Client::DataElement::DataElement (
    void
    ) {

    this->pathname    = NULL;
    this->description = NULL;
    this->units       = NULL;
    this->definedon   = NULL;
    this->arrayshape  = NULL;
    }


Client::DataElement::~DataElement (
    void
    ) {

    if (this->pathname    != NULL) free (this->pathname);
    if (this->description != NULL) free (this->description);
    if (this->units       != NULL) free (this->units);
    if (this->definedon   != NULL) free (this->definedon);
    if (this->arrayshape  != NULL) free (this->arrayshape);
    }


//-------------------------------------------------------------------------------
//  Client-Server Marshalling Routines


size_t
Server::DataElement::Serialize (
    char *  buffer,
    size_t  bufsize
    ) {

    size_t size = 4 * sizeof (int);
    if (size >= bufsize) return 0;

    unsigned int * ip = (unsigned int *) buffer;
    *ip++ = (unsigned int) this->basetype;
    *ip++ = (unsigned int) this->inout;
    *ip++ =                this->arrayelements;
    *ip++ =                this->size;

    char * cp = (char *) ip;

    SERIALIZE_STRING (this->pathname)
    SERIALIZE_STRING (this->description)
    SERIALIZE_STRING (this->units)
    SERIALIZE_STRING (this->definedon)

    if (this->arrayshape == NULL) {
        *cp++ = (char) false;
        size++;
        }

    else {
        *cp++ = (char) true;
        size++;
        size += this->arrayshape->Serialize (cp, bufsize-size);
        }

    return size;
    }


void
Client::DataElement::Unserialize (
    char *  buffer
    ) {

    unsigned int * ip = (unsigned int *) buffer;
    this->basetype      = (BaseType)   *ip++;
    this->inout         = (AccessMode) *ip++;
    this->arrayelements = *ip++;
    this->size          = *ip++;

    char * cp = (char *) ip;

    UNSERIALIZE_STRING (this->pathname)
    UNSERIALIZE_STRING (this->description)
    UNSERIALIZE_STRING (this->units)
    UNSERIALIZE_STRING (this->definedon)

    if ((bool) *cp++) {
        this->arrayshape = new ArrayShape ();
        this->arrayshape->Unserialize (cp);
        }
    }

}
