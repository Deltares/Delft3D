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
// $Id: subdomGlobals.h 883 2011-10-07 16:32:16Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/subdomGlobals.h $
//------------------------------------------------------------------------------
//  Mapper subdomain globals
//
//  Adri.Mourits@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#pragma once


typedef struct {
    bool    dredgefirst;        // flag for first pass thru DredgeCommunicate
    bool    dredgecommunicate;  // flag explicitly switched to false when no
                                // dredge communication is needed (anymore) for
                                // this subdomain

    bool    rtcfirst;           // flag for first pass thru RtcCommunicate
    bool    rtccommunicate;     // flag explicitly switched to false when no
                                // rtc communication is needed (anymore) for
                                // this subdomain
    } SubdomGlobals;

