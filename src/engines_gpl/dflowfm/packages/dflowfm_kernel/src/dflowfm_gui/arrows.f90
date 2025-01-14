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

module m_arrows

   implicit none

contains

   subroutine ARROWS(X0, Y0, UR, VR, PSI0, VFAC)
      use precision, only: dp
      use m_movabs
      use m_lnabs
      implicit none
      real(kind=dp) :: alfa
      real(kind=dp) :: csa
      integer :: i
      real(kind=dp) :: psi0
      real(kind=dp) :: sna
      real(kind=dp) :: ur
      real(kind=dp) :: vfac
      real(kind=dp) :: vr
      real(kind=dp) :: x0
      real(kind=dp) :: xlen
      real(kind=dp) :: y0
      real(kind=dp) :: X(3), Y(3), XR(3), YR(3)
      data X(1)/0.8d0/, X(2)/1d0/, X(3)/0.8d0/, &
         Y(1)/-0.1d0/, Y(2)/0d0/, Y(3)/0.1d0/

      if (UR == 0 .and. VR == 0) return
      ALFA = atan2(VR, UR) + PSI0
      CSA = cos(ALFA)
      SNA = sin(ALFA)
      XLEN = sqrt(UR * UR + VR * VR)

      do I = 1, 3
         XR(I) = X0 + VFAC * XLEN * (X(I) * CSA - Y(I) * SNA)
         YR(I) = Y0 + VFAC * XLEN * (Y(I) * CSA + X(I) * SNA)
      end do

      call MOVABS(X0, Y0)
      call LNABS(XR(2), YR(2))
      call LNABS(XR(1), YR(1))

      call MOVABS(XR(2), YR(2))
      call LNABS(XR(3), YR(3))
      return
   end

end module m_arrows
