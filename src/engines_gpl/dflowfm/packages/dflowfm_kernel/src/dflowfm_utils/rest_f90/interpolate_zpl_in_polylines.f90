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

!> linear interpolation of z-values in polylines
module m_interpolate_zpl_in_polylines

   implicit none

   private

   public :: interpolate_zpl_in_polylines

contains

   subroutine interpolate_zpl_in_polylines()
      use precision, only: dp
      use m_polygon
      use m_missing
      use geometry_module, only: dbdistance, get_startend
      use m_sferic, only: jsferic, jasfer3D
      use m_qnerror

      real(kind=dp), dimension(:), allocatable :: wfromLeft ! arc length from left

      integer, dimension(:), allocatable :: iLeft, iRight ! left and right node for interpolation, respectively

      real(kind=dp) :: wL, wR, w

      integer :: jstart, jend
      integer :: jpoint
      integer :: i, iL, iR

      integer :: ierror
      real(kind=dp), parameter :: dtol = 1d-8

      ierror = 1

      if (NPL < 2) goto 1234

!  allocate
      allocate (wfromleft(NPL))
      allocate (ileft(NPL))
      allocate (iRight(NPL))

      jpoint = 1

      do while (jpoint < NPL)

!     get subpolyline
         call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)

         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1

!     compute arc lengths from left
         wfromLeft(jstart) = 0d0
         do i = jstart, jend - 1
            wfromLeft(i + 1) = wfromLeft(i) + dbdistance(xpl(i), ypl(i), xpl(i + 1), ypl(i + 1), jsferic, jasfer3D, dmiss)
         end do

!     get left nodes for interpolation
         iL = 0
         do i = jstart, jend
            if (zpl(i) /= DMISS) then
               iL = i
            end if
            iLeft(i) = iL
         end do

!     get right nodes for interpolation
         iR = 0
         do i = jend, jstart, -1
            if (zpl(i) /= DMISS) then
               iR = i
            end if
            iRight(i) = iR
         end do

!     interpolate values
         do i = jstart, jend
            iL = iLeft(i)
            iR = iRight(i)
            wL = 0d0; if (iL > 0) wL = wfromLeft(iL)
            wR = 0d0; if (iR > 0) wR = wfromLeft(iR)

            if (iL == iR .and. iL /= 0) then
!           value prescibed
               if (i /= iL) then
                  call qnerror('interpolate_zpl_in_polylines: error', ' ', ' ')
               end if
            else if (iL > 0 .and. iR > 0) then
!           two-sided interpolation
               if (abs(wR - wL) > dtol) then
                  w = (wfromLeft(i) - wL) / (wR - wL)
                  zpl(i) = (1d0 - w) * zpl(iL) + w * zpl(iR)
               else
!              left and right node on top
                  zpl(i) = 0.5d0 * (zpl(iL) + zpl(iR))
               end if
            else if (iL > 0) then
!           one-sided interpolation
               zpl(i) = zpl(iL)
            else if (iR > 0) then
!           one-sided interpolation
               zpl(i) = zpl(iR)
            else
!           nothing to interpolate
            end if
         end do

!     proceed to next polyline, if available
         jpoint = jend + 1
      end do

      ierror = 0
1234  continue

!  deallocate
      if (allocated(wfromLeft)) deallocate (wfromLeft)
      if (allocated(iLeft)) deallocate (iLeft)
      if (allocated(iRight)) deallocate (iRight)

      return
   end subroutine interpolate_zpl_in_polylines

end module m_interpolate_zpl_in_polylines
