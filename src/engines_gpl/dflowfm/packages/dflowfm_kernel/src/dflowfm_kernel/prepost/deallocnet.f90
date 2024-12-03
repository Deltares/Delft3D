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

module m_deallocnet

   implicit none

   private

   public :: deallocnet

contains

   subroutine DEALLOCNET()
      use m_netw
      use M_FLOWgeom

      integer :: p, numpx

      NUMK = 0; NUML = 0
      if (size(XK) > 0) then
         deallocate (NOD, XK, YK, KC, NMK)
         deallocate (KN, LC)
      end if
      if (size(XK0) > 0) then
         deallocate (NOD0, XK0, YK0, ZK0, KC0, NMK0)
         deallocate (KN0, LC0)
      end if
      if (size(LNN) > 0) then
         deallocate (LNN, LNE, ln2lne)
      end if
      if (NUMP > 0) then
         numpx = size(netcell)
         do p = 1, numpx
            if (allocated(netcell(p)%nod)) then
               deallocate (netcell(p)%nod, netcell(p)%lin)
            end if
         end do
         deallocate (netcell)
         nump = 0
      end if
   end subroutine DEALLOCNET

end module m_deallocnet
