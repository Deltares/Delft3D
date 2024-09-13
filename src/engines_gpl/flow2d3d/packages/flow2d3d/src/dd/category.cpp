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
// $Id: category.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/category.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Category Class - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  25 oct 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//-------------------------------------------------------------------------------
//  Constructor and destructor


Category::Category (
    DD *         dd,
    const char * name
    ) {

    // Validate constructor arguments

    if (name == NULL || name[0] == '\0')
        throw new Exception (true, "Category does not have a name");
    if (strlen (name) >= DD::MAXSTRING)
        throw new Exception (true, "Category name is too long");
    if (dd->categoryDict->Lookup ((char *) name) != (void *) Dictionary::NOTFOUND)
        throw new Exception (true, "Duplicate category name \"%s\"", name);

    // Allocate new slot in configuration table

    if ((this->id = (int) dd->numCategories++) >= (int) DD::MAXCATEGORIES)
        throw new Exception (true, "Configuration category table is full (> %d entries)", DD::MAXCATEGORIES);

    this->dd = dd;
    this->name = new char [strlen (name)+1];
    strcpy (this->name, name);

    dd->category[id].category = this;
    strcpy (dd->category[id].name, name);

    // Add category to object dictionary

    dd->categoryDict->Insert ((char *) name, (void *) id);

    dd->log->Write (Log::ITER_MINOR, "Added category #%d \"%s\" in %s role", id, name, ROLENAME (dd->role));
    }


Category::~Category (
    void
    ) {

    delete [] this->name;
    }


//-------------------------------------------------------------------------------
//  Extra-object functions


Category *
LookupCategory (
    const char * catname
    ) {

    long catid = (long) FLOW2D3D->dd->categoryDict->Lookup (catname);
    if (catid == Dictionary::NOTFOUND)
        return NULL;
    else
        return FLOW2D3D->dd->category[catid].category;
    }
