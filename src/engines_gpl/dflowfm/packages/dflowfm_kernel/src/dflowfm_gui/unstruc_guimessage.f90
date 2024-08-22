!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!----------------------------------------------------------------------
! subroutines from either net.F90 or rest.F90 that are still needed
!   without the GUI
!----------------------------------------------------------------------
!> Shows a message in a GUI dialog (Interacter only).
!! This routine is supposed to be called from the utility modules,
!! such as gridgeom, as a callback.
!!
!! NOTE: this subroutine is dflowfm's implementation of the MHCallBack::messagebox_iface interface.
subroutine unstruc_guimessage(msg)
   use unstruc_messages
   implicit none
   character(len=*) :: msg !< Message string

   call qnerror(msg, ' ', ' ')

end subroutine unstruc_guimessage
