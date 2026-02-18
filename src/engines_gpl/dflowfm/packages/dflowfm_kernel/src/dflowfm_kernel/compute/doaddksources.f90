!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_doaddksources

   implicit none

   private

   public :: doaddksources

contains

   subroutine doaddksources() ! add k sources
      use precision, only: dp
      use m_flow, only: numsrc, ksrc, arsrc, qsrc, vol1, turkinws
      use m_flowtimes, only: dts
      implicit none

      integer :: n, k, kk, kk2
      real(kind=dp) :: qsrck, dvoli, dtol = 1.0e-4_dp

      do n = 1, numsrc
         if (ksrc(2, n) == 0 .and. ksrc(5, n) == 0) then
            cycle ! due to initialisation
         end if

         if (arsrc(n) == 0) then
            cycle
         end if
         kk = ksrc(1, n) ! 2D pressure cell nr FROM
         kk2 = ksrc(4, n) ! 2D pressure cell nr TO
         qsrck = qsrc(n)

         if (kk > 0) then ! FROM Point
            k = ksrc(2, n)
            dvoli = 1.0_dp / max(vol1(k), dtol)
            if (qsrck > 0) then ! FROM k to k2
               turkinws(k) = turkinws(k) - dts * qsrck * dvoli * turkinws(k)
            else if (qsrck < 0) then ! FROM k2 to k
               turkinws(k) = turkinws(k) - dts * qsrck * dvoli * 0.5_dp * (qsrck / arsrc(n))**2
            end if
         end if

         if (kk2 > 0) then ! TO Point
            k = ksrc(5, n)
            dvoli = 1.0_dp / max(vol1(k), dtol)
            if (qsrck > 0) then
               turkinws(k) = turkinws(k) + dts * qsrck * dvoli * 0.5_dp * (qsrck / arsrc(n))**2
            else if (qsrck < 0) then
               turkinws(k) = turkinws(k) + dts * qsrck * dvoli * turkinws(k)
            end if
         end if

      end do
   end subroutine doaddksources

end module m_doaddksources
