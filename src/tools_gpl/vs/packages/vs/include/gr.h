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
/*

 System      : Viewer/Selector

 $Header: /delft3d/libraries/vs/include/gr.h 4     9/09/05 14:44 Mooiman $

 Programmer  : Abe Hoekstra - CSO
 Part        : ViewSel

 $Log: /delft3d/libraries/vs/include/gr.h $
/*  */
/* 4     9/09/05 14:44 Mooiman */

#ifndef _GR_INCLUDED
#   ifndef MSDOS
#	ifndef __lint
	static char GR_rcsid[] = "$Id$";
#	endif
#   endif

    extern void            GR_remove_groups_from_chain(
	    			struct St_grp * );
    extern struct St_grp * GR_add_group_to_chain (
	    		    struct St_grp *,
                const BText,
			        const BText,
                const BInt4 [],
			        struct attrib * );
    extern struct St_grp * GR_find_group_in_chain (
			    struct St_grp *,
            const BText );

#   define _GR_INCLUDED
#endif
