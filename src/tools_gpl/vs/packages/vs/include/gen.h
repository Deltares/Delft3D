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

#ifndef _GEN_INCLUDED
#   define _GEN_INCLUDED

#   define UNDEFVALUE 999.999
#   define MAXNUMBEROFVARS 40
#   define MAXNUMBEROFPARS 40

#   include "neftyp.h"
#   include "btps.h"

    enum INTR_TYPE    { AVERAGE, MAXIMUM, MINIMUM } ;

    extern BInt4   GEN_string_compare ( const BText, const BText );
    extern BText   GEN_tekst ( BText ) ;
    extern BVoid   GEN_close_output_stream ( BVoid );
    extern BVoid   GEN_declare_error_file ( FILE * );
    extern BVoid   GEN_display_help ( BVoid ) ;
    extern BVoid   GEN_free  ( BVoid * );
    extern BVoid   GEN_init  ( );
    extern BVoid   GEN_mallinfo( BVoid );
    extern BVoid   GEN_message_to_errorfile( BInt4, ... );
    extern BVoid   GEN_print ( FILE *, const BText, ... );
    extern BVoid * GEN_malloc ( BInt4 );
    extern FILE  * GEN_get_output_stream( BVoid );

#endif
