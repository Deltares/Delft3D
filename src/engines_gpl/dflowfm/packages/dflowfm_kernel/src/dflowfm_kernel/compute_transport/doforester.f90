!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_doforester

   implicit none

   private

   public :: doforester

contains

   subroutine doforester()
      use precision, only: dp
      use m_foresterpoint2, only: foresterpoint2
      use m_flow, only: kbot, ktop, maxitverticalforestersal, ndkx, vol1, kmxn, maxitverticalforestertem
      use m_transportdata, only: constituents, numconst, isalt, itemp
      use timers, only: timon, timstrt, timstop
      use m_flowgeom, only: ndxi
      use m_turbulence, only: kmxx

      implicit none

      integer :: kk, km, kb
      real(kind=dp) :: a(kmxx), d(kmxx)

      integer(4) :: ithndl = 0

      if (timon) then
         call timstrt("doforester", ithndl)
      end if

      do kk = 1, ndxi
         kb = kbot(kk)
         km = ktop(kk) - kb + 1
         if (maxitverticalforestersal > 0) then
            call foresterpoint2(constituents, numconst, ndkx, isalt, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestersal, 1)
         end if
         if (maxitverticalforestertem > 0) then
            call foresterpoint2(constituents, numconst, ndkx, itemp, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestertem, -1)
         end if
      end do

      if (timon) then
         call timstop(ithndl)
      end if
   end subroutine doforester

end module m_doforester
