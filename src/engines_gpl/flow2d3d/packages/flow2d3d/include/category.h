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
// $Id: category.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/category.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Category Class - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  25 oct 11
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"


class Category {
    public:
        Category (
            DD *         dd,
            const char * name
            );

        ~Category (
            void
            );

    public:
        int     id;         // index in DD category table
        char *  name;       // category name

    private:
        DD *    dd;         // domain decomposition object
    };


//-------------------------------------------------------------------------------


Category *
LookupCategory (
    const char * catname
    );
