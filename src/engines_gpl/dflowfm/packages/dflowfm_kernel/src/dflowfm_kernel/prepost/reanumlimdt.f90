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

module m_reanumlimdt

   implicit none

   private

   public :: reanumlimdt

contains

   subroutine reanumlimdt()
      use m_flowgeom
      use m_GlobalParameters, only: INDTP_ALL
      use MessageHandling, only: IdLen
      use m_flow
      use m_partitioninfo
      use m_samples
      use m_find_flownode, only: find_nearest_flownodes
      use m_filez, only: oldfil, doclose

      character(len=IdLen) :: name, nams
      logical :: jawel
      integer :: mlim, k, kk, jakdtree = 1, jaoutside = 0
      integer, allocatable :: knum(:)

      if (jampi == 0) then
         name = 'prev_numlimdt.xyz'
      else
         name = 'prev_numlimdt'//'_'//trim(sdmn)//'.xyz'
      end if
      inquire (file=name, exist=jawel)
      if (jawel) then

         call oldfil(mlim, trim(name))
         call increasesam(ndx)
         kk = 0
         do k = 1, ndx
            kk = kk + 1
            read (mlim, *, end=999) xs(kk), ys(kk), zs(kk)
         end do
999      continue
         call doclose(mlim)
         allocate (knum(ndx)); knum = 0
         kk = kk - 1
         call find_nearest_flownodes(kk, xs, ys, nams, knum, jakdtree, jaoutside, INDTP_ALL)
         do k = 1, kk
            if (knum(k) > 0) then
               numlimdt(knum(k)) = zs(k)
            end if
         end do

         deallocate (xs, ys, zs, knum)
      end if
   end subroutine reanumlimdt

end module m_reanumlimdt
