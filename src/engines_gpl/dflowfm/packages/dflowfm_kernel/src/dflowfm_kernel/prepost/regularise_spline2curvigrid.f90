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

!>  regularise spline2curvi grid
!>     note: there is an asymmetry, but this procedure is intended for regularisation only
module m_regularise_spline2curvigrid
   use m_savegrd, only: savegrd

   implicit none

   private

   public :: regularise_spline2curvigrid

contains

   subroutine regularise_spline2curvigrid()
      use precision, only: dp
      use m_grid
      use m_spline2curvi, only: dtolLR
      use m_missing, only: dmiss
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use m_get_lr

      real(kind=dp) :: xi
      real(kind=dp) :: dhmax, dtolLR_bak

      integer :: i, j, iL, iR
      integer :: ih

      integer :: ierror

      real(kind=dp), parameter :: FAC = 1d-1 ! regularisation parameter

      call savegrd()

      ierror = 1

!  store settings
      dtolLR_bak = dtolLR

!  compute maximum mesh width and get dtolLR in the proper dimension
      dhmax = 0d0
      do i = 1, mc
         do j = 1, nc - 1
            if (xc(i, j) == DMISS .or. xc(i, j + 1) == DMISS) cycle
            dhmax = max(dhmax, dbdistance(xc(i, j), yc(i, j), xc(i, j), yc(i, j + 1), jsferic, jasfer3D, dmiss))
         end do
      end do
      dtolLR = dtolLR * dhmax

      do j = 1, nc
         i = 1
         do while (i <= mc)
            if (xc(i, j) /= DMISS .and. yc(i, j) /= DMISS) then
!           get neighboring nodes
               call get_LR(mc, xc(:, j), yc(:, j), i, iL, iR)

!           regularise grid on right hand side of this node (asymmetric)
               do ih = i + 1, iR - 1
                  xi = dble(ih - i) / dble(iR - i) * FAC
                  xc(ih, j) = (1d0 - xi) * xc(i, j) + xi * xc(iR, j)
                  yc(ih, j) = (1d0 - xi) * yc(i, j) + xi * yc(iR, j)
               end do
            else ! just advance pointer
               iR = i + 1
            end if

            i = max(iR, i + 1)
         end do
      end do

      ierror = 0
1234  continue

!  restore settings
      dtolLR = dtolLR_bak

      return
   end subroutine regularise_spline2curvigrid

end module m_regularise_spline2curvigrid
