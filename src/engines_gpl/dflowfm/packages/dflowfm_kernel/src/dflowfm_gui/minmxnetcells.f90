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

module m_minmxnetcells

   implicit none

contains

   subroutine MINMXNETCELLS()
      use precision, only: dp

      use m_netw
      use m_flowgeom, only: xz, yz
      use m_missing, only: dmiss
      use m_depmax
      use m_inview

      integer :: i
      integer :: k
      real(kind=dp) :: rd
      real(kind=dp) :: rmax
      real(kind=dp) :: rmin

      if (JAAUTO > 0) then
         RMIN = 1.0d30
         NODMIN = 0
         RMAX = -1.0d30
         NODMAX = 0
         do K = 1, max(NUMP, nump1d2d)
            if (INVIEW(XZ(K), YZ(K))) then
               RD = RLIN(K)
               if (rd /= dmiss) then
                  if (RD < RMIN) then
                     RMIN = RD
                     netcelMIN = K
                  end if
                  if (RD > RMAX) then
                     RMAX = RD
                     netcelMAX = K
                  end if
               end if
            end if
         end do
         VMAX = RMAX
         VMIN = RMIN
      end if

      DV = VMAX - VMIN
      do I = 1, NV
         VAL(I) = VMIN + (I - 1) * DV / (NV - 1)
      end do

      return
   end subroutine MINMXNETCELLS

end module m_minmxnetcells
