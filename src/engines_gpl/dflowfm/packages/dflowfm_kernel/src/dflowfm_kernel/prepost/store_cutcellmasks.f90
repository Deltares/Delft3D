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

!> add polygon and fill cutcell mask with "kc"
   subroutine store_cutcellmasks(numk, kc, numL, Lmask, xmL, ymL)
      use m_cutcells
!      use network_data, only: kc, numk
      use m_alloc
      use unstruc_messages
      implicit none

      integer, intent(in) :: numk
      integer, dimension(numk), intent(in) :: kc
      integer, intent(in) :: numL
      integer, dimension(numL), intent(in) :: Lmask
      double precision, dimension(numL), intent(in) :: xmL, ymL

      integer :: istart, k, L, num, i, iL, iR
      integer :: numcur, numnew

      jastored = 1

      if (NPOL == 0) then
!        initialize
         call realloc(ik, NPOL + 1, keepexisting=.false., fill=0)
         ik(1) = 1
      end if

!     increase number of polygons
      NPOL = NPOL + 1

!     get startpointer
      istart = ik(NPOL)

!     count number of new data
      num = 0
      do k = 1, numk
         if (kc(k) == 1) then
            num = num + 1
         end if
      end do

!     reallocate ik
      call realloc(ik, NPOL + 1, keepexisting=.true., fill=0)

!     add to ik
      ik(NPOL + 1) = istart + num

!     reallocate jk
      call realloc(jk, ik(NPOL + 1) - 1, keepexisting=.true., fill=0)

!     add to jk
      num = 0
      do k = 1, numk
         if (kc(k) == 1) then
            jk(ik(NPOL) + num) = k
            num = num + 1
         end if
      end do

!     count number of new intersections
      num = 0
      do L = 1, numL
         if (Lmask(L) == 1) then
            num = num + 1
         end if
      end do

!     get current number of intersections
      if (allocated(idxL) .and. NPOL > 1) then
         numcur = idxL(numL + 1) - 1
      else
         numcur = 0
      end if
      numnew = numcur + num

!     reallocate idxL, jdxL, xdxL, ydxL, pdxL
      if (NPOL == 1) then
         call realloc(idxL, numL + 1, keepExisting=.true., fill=1)
      end if
      call realloc(xdxL, numnew, keepExisting=.true., fill=0d0)
      call realloc(ydxL, numnew, keepExisting=.true., fill=0d0)
      call realloc(pdxL, numnew, keepExisting=.true., fill=0)

!     shift pointers and data
      iL = idxL(numL + 1)
      do L = numL, 1, -1
         iR = iL - 1
         iL = idxL(L)
         idxL(L + 1) = idxL(L + 1) + num
         if (Lmask(L) == 1) then
            xdxL(iR + num) = xmL(L)
            ydxL(iR + num) = ymL(L)
            pdxL(iR + num) = NPOL
            num = num - 1
         end if
         do i = iL, iR
            xdxL(i + num) = xdxL(i)
            ydxL(i + num) = ydxL(i)
            pdxL(i + num) = pdxL(i)
         end do
      end do
      if (num /= 0) then
         call mess(LEVEL_ERROR, 'store_cutcellmasks: numbering error')
      end if
!     shift and add to idxL, jdxL, xdxL, ydxL, pdxL

      return
   end subroutine store_cutcellmasks
