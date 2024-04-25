//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
// $Id$
// $HeadURL$
#ifndef _EX_INCLUDED
#   define COMMAND_STRING_LENGHT 512
#   define _EX_INCLUDED
    extern BVoid EX_process            ( BText, BInt4 );
    extern BVoid EX_process_with_parms ( BText, BText [] , BInt4 );
    extern BVoid EX_process_with_vars  ( BText, BText [] , BInt4 );
    extern BVoid EX_process_with_vars_and_parms (
                               BText, BText [], BText [] , BInt4 );
    extern BVoid EX_write_variables_to_pipe ( FILE *, BText [] );
#endif
