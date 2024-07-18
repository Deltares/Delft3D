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

!> Contains the global data for all thin dams.
!! thd is the array of cross section paths.
module m_thindams
   use m_crspath
   implicit none

   type(tcrspath), allocatable :: thd(:)
   integer :: nthd = 0

contains

!> Increases memory for thin dams
   subroutine increaseThinDams(n)
      integer, intent(inout) :: n !< Desired number of thin dams

      call increaseCRSPaths(thd, n, nthd)
   end subroutine increaseThinDams

!> Converts a set of polylines into thin dams.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
   subroutine pol_to_thindams(xpl, ypl, npl)
      use m_missing

      double precision, intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
      integer, intent(in) :: npl !< Total number of polyline points

      integer :: i, i1, i2, maxthd

      nthd = 0

      i1 = 1 ! First possible start index
      i2 = 0 ! No end index found yet.
      do i = 1, npl
         if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
               i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
               maxthd = nthd + 1
               call increaseThinDams(maxthd)
               nthd = nthd + 1
               call setCrossSectionPathPolyline(thd(nthd), xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i + 1
            cycle
         else
            i2 = i ! Advance end point by one.
         end if
      end do
   end subroutine pol_to_thindams

!> Deletes all thin dams from thd.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
   subroutine delThinDams()
      nthd = 0
      ! Do not reset thd data, just let it be overwritten later.
   end subroutine delThinDams

end module m_thindams
