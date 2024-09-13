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
// $Id: blob.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/blob.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Blob Class - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  31 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


Blob::Blob (
    const void * address,
    const unsigned int size
    ) {

    this->address = (void *) address;   // prefectly safe; address is private and only read once
    this->size = size;
    }


Blob::~Blob (
    void
    ) {

    }


void *
Blob::Address (
    void
    ) {

    return this->address;
    }


unsigned int
Blob::Size (
    void
    ) {

    return this->size;
    }
