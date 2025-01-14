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

module m_copyldbtopol

   implicit none

   private

   public :: copyldbtopol

contains

   subroutine COPYLDBTOPOL()
      use precision, only: dp
      use m_polygon
      use m_missing
      use m_landboundary
      use geometry_module, only: dbpinpol

      integer :: k
      integer :: mx
      integer :: in, num, isnew

      real(kind=dp), allocatable, dimension(:) :: xdum, ydum, zdum

      MX = MAXLAN
!  call increasepol(maxlan, 0)

! allocate
      allocate (xdum(maxlan), ydum(maxlan), zdum(maxlan))

! initialize
      xdum = DMISS
      ydum = DMISS
      zdum = DMISS

      num = 0
      isnew = 0
      in = -1 ! for initialization of dbpinpol
      do K = 1, MXLAN
         if (xlan(k) /= DMISS) then
            call DBPINPOL(xlan(k), ylan(k), in, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (in == 1) then
               num = num + 1
               xdum(num) = XLAN(K)
               ydum(num) = YLAN(K)
               zdum(num) = zLAN(K)
               isnew = isnew + 1
            else if (isnew > 1) then ! add one DMISS at most
               num = num + 1
               xdum(num) = DMISS
               ydum(num) = DMISS
               zdum(num) = DMISS
               isnew = 0 ! no new DMISS will be stored directly hereafter
            else if (isnew == 1 .and. num > 0) then ! do not add a single point
               num = num - 1
               isnew = 0
            end if
         else
            if (isnew > 1) then ! add one DMISS at most
               num = num + 1
               xdum(num) = DMISS
               ydum(num) = DMISS
               zdum(num) = DMISS
               isnew = 0 ! no new DMISS will be stored directly hereafter
            else if (isnew == 1 .and. num > 0) then ! do not add a single point
               num = num - 1
            end if
         end if
      end do
!  NPL = MXLAN

! copy to polygon
      if (num > 1) then
         call savepol()
!    delete original polygon
         NPL = 0

         if (NPL > 1) then
            call increasepol(npl + num + 1, 0)
            XPL(NPL + 1) = DMISS
            YPL(NPL + 1) = DMISS
            zPL(NPL + 1) = DMISS
            XPL(NPL + 2:num + 1) = xdum
            YPL(NPL + 2:num + 1) = ydum
            zPL(NPL + 2:num + 1) = zdum
            NPL = num + 1
         else
            call increasepol(num, 0)
            XPL(1:num) = xdum
            YPL(1:num) = ydum
            zPL(1:num) = zdum
            NPL = num
         end if
      end if

! deallocate
      deallocate (xdum, ydum, zdum)

      return
   end subroutine COPYLDBTOPOL

end module m_copyldbtopol
