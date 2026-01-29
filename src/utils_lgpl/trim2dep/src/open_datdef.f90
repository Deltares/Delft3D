!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
    function open_datdef(filnam, fds)
    !!--copyright-------------------------------------------------------------------
       ! Copyright (c) 2003, WL | Delft Hydraulics. All rights reserved.
    !!--disclaimer------------------------------------------------------------------
       ! This code is part of the Delft3D software system. WL|Delft Hydraulics has
       ! developed c.q. manufactured this code to its best ability and according to the
       ! state of the art. Nevertheless, there is no express or implied warranty as to
       ! this software whether tangible or intangible. In particular, there is no
       ! express or implied warranty as to the fitness for a particular purpose of this
       ! software, whether tangible or intangible. The intellectual property rights
       ! related to this software code remain with WL|Delft Hydraulics at all times.
       ! For details on the licensing agreement, we refer to the Delft3D software
       ! license and any modifications to this license, if applicable. These documents
       ! are available upon request.
    !!--version information---------------------------------------------------------
       ! $Author$
       ! $Date$
       ! $Revision$
    !!--description-----------------------------------------------------------------
       !
       !    Function: Detect the number of time steps on filnam (map-
       !              or his-file)
       ! Method used:
       !
    !!--pseudo code and references--------------------------------------------------
       ! NONE
    !!--declarations----------------------------------------------------------------
       implicit none
       !
       ! Global variables
       !
       integer :: open_datdef
       integer, intent(out) :: fds
       character(*) :: filnam
       !
       ! Local variables
       !
       integer :: len_fn
       integer, external :: crenef
       character(256) :: dat_file
       character(256) :: def_file
       character(1) :: coding
       !
    !! executable statements -------------------------------------------------------
       !
       dat_file = trim(filnam)//'.dat'
       def_file = trim(filnam)//'.def'
       !
       coding = 'N'
       open_datdef = crenef(fds, dat_file, def_file, ' ', 'r')
    end function open_datdef
