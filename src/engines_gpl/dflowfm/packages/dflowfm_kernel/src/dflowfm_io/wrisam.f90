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
module m_wrisam
   use m_wriarcsam, only: wriarcsam
   use m_wriarc, only: wriarc

   implicit none
contains
   subroutine WRISAM(MSAM)
      use precision, only: dp
      use M_SAMPLES, only: ns, zs, xs, ys
      use M_ARCINFO, only: mca, nca, x0, y0, dxa, dya, maxsamarc, d
      use m_readyy, only: readyy
      use m_qnerror, only: qnerror
      use M_MISSING, only: DMISS
      use m_filez, only: doclose

      integer :: msam, KMOD

      real(kind=dp) :: af
      integer :: i

      call READYY('Writing Samples File', 0.0_dp)

      if (MCA * NCA == NS) then
         call wriarcsam(MSAM, ZS, MCA, NCA, MCA, NCA, X0, Y0, DXA, DYA, DMISS)
         goto 1234
      else if (mca * nca > maxsamarc) then
         call wriarc(MSAM, D, mca, nca, mca, nca, X0, Y0, DXA, DYA, DMISS)
         goto 1234
      end if

      KMOD = max(1, NS / 100)
      do I = 1, NS
         if (mod(I, KMOD) == 0) then
            AF = real(I, kind=dp) / real(NS, kind=dp)
            call READYY('Writing Samples File', AF)
         end if
         ! if (xs(i) > 179.87d0) xs(i) = xs(i) - 360d0
         if (abs(zs(i)) < 1.0e6_dp) then
            write (MSAM, '(3(F16.7))') XS(I), YS(I), ZS(I)
         else if (abs(zs(i)) < 1.0e16_dp) then
            write (MSAM, "(2F16.7, ' ', F26.7)") XS(I), YS(I), ZS(I)
         else
            call qnerror('wrisam: format error', ' ', ' ')
         end if
      end do

1234  continue
      call DOCLOSE(MSAM)
      call READYY('Writing Samples File', -1.0_dp)

      return
   end subroutine WRISAM
end module m_wrisam
