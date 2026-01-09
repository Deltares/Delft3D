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

!> compute sample Hessians
module m_comp_samplehessian

   implicit none

contains

   subroutine comp_sampleHessian(ierror)
      use precision, only: dp
      use m_comp_samplegradi, only: comp_samplegradi
      use m_samples, only: xs, ys, mxsam, mysam
      use m_samples_refine, only: zss, ihesstat, ihesstat_ok
      use m_missing, only: dmiss
      use m_readyy, only: readyy
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer, intent(out) :: ierror !< error (1) or not (0)

      real(kind=dp), dimension(2, 2) :: UU, VV ! for SVD: H = USV'
      real(kind=dp), dimension(2) :: S ! for SVD: H = USV'

      real(kind=dp), dimension(2) :: gradiL, gradiR ! sample gradients at i-edges
      real(kind=dp), dimension(2) :: gradjL, gradjR ! sample gradients at j-edges
      real(kind=dp), dimension(2) :: SniL, sniR ! i-edge surface vector (for divergence)
      real(kind=dp), dimension(2) :: SnjL, snjR ! j-edge surface vector (for divergence)
      real(kind=dp) :: dareaiL, dareaiR ! contribution to control volume area (for divergence)
      real(kind=dp) :: dareajL, dareajR ! contribution to control volume area (for divergence)

      real(kind=dp) :: area ! control volume area of divergence operator (for Laplacian)

      real(kind=dp) :: zxx, zxy, zyx, zyy ! second order partial derivatives
      real(kind=dp) :: zx, zy ! first order partial derivatives

      real(kind=dp) :: af, dum, Dh

      integer :: i, j, k, ip, ihasridge

!  compute sample mesh width
      Dh = min(dbdistance(xs(1), ys(1), xs(2), ys(2), jsferic, jasfer3D, dmiss), dbdistance(xs(1), ys(1), xs(1 + MXSAM), ys(1 + MXSAM), jsferic, jasfer3D, dmiss))

      ierror = 1

      if (MXSAM < 3 .or. MYSAM < 3) then
         goto 1234
      end if

      zss(4, 1:MXSAM, 1:MYSAM) = 0.0_dp
      zss(5, 1:MXSAM, 1:MYSAM) = DMISS

      call readyy('Computing sample Hessians', 0.0_dp)
      do i = 2, MXSAM - 1
         af = real(i - 2, kind=dp) / real(max(MXSAM - 3, 1), kind=dp)
         call readyy('Computing sample Hessians', af)
         do j = 2, MYSAM - 1
!         if ( i.eq.614 .and. j.eq.154 )
            ip = i + (j - 1) * MXSAM
            if (abs(xs(ip) - 87270) < 1.0e-8_dp .and. abs(ys(ip) - 415570) < 1.0e-8_dp) then
               continue
            end if
            zxx = 0.0_dp
            zxy = 0.0_dp
            zyx = 0.0_dp
            zyy = 0.0_dp
            UU = 0.0_dp
            VV = 0.0_dp
            zx = 0.0_dp
            zy = 0.0_dp
            S = 0.0_dp
            k = 0
            ihasridge = 0
            do
               call comp_samplegradi(0, i, j, gradiR, SniR, dareaiR, dum)
               if (gradiR(1) == DMISS .or. gradiR(1) == DMISS) then
                  exit
               end if

               call comp_samplegradi(0, i - 1, j, gradiL, SniL, dum, dareaiL)
               if (gradiL(1) == DMISS .or. gradiL(1) == DMISS) then
                  exit
               end if

               call comp_samplegradi(1, i, j, gradjR, SnjR, dareajR, dum)
               if (gradjR(1) == DMISS .or. gradjR(1) == DMISS) then
                  exit
               end if

               call comp_samplegradi(1, i, j - 1, gradjL, SnjL, dum, dareajL)
               if (gradjL(1) == DMISS .or. gradjL(1) == DMISS) then
                  exit
               end if

               area = dareaiL + dareaiR + dareajL + dareajR
               zxx = (gradiR(1) * SniR(1) - gradiL(1) * SniL(1) + gradjR(1) * SnjR(1) - gradjL(1) * SnjL(1)) / area
               zxy = (gradiR(1) * SniR(2) - gradiL(1) * SniL(2) + gradjR(1) * SnjR(2) - gradjL(1) * SnjL(2)) / area
               zyx = (gradiR(2) * SniR(1) - gradiL(2) * SniL(1) + gradjR(2) * SnjR(1) - gradjL(2) * SnjL(1)) / area
               zyy = (gradiR(2) * SniR(2) - gradiL(2) * SniL(2) + gradjR(2) * SnjR(2) - gradjL(2) * SnjL(2)) / area

               zx = (0.5_dp * (zss(1, i + 1, j) + zss(1, i, j)) * SniR(1) &
                     - 0.5_dp * (zss(1, i - 1, j) + zss(1, i, j)) * SniL(1) &
                     + 0.5_dp * (zss(1, i, j + 1) + zss(1, i, j)) * SnjR(1) &
                     - 0.5_dp * (zss(1, i, j - 1) + zss(1, i, j)) * SnjL(1)) / area
               zy = (0.5_dp * (zss(1, i + 1, j) + zss(1, i, j)) * SniR(2) &
                     - 0.5_dp * (zss(1, i - 1, j) + zss(1, i, j)) * SniL(2) &
                     + 0.5_dp * (zss(1, i, j + 1) + zss(1, i, j)) * SnjR(2) &
                     - 0.5_dp * (zss(1, i, j - 1) + zss(1, i, j)) * SnjL(2)) / area

!           Eigendecompostion
               VV(1, 1) = zxx
               VV(1, 2) = zxy
               VV(2, 1) = zyx
               VV(2, 2) = zyy
               !call jacobi

!!           checks
!            if ( abs(zxy-zyx).gt.1d-8 ) then
!               continue
!            end if
!
!!           Eigendecomposition: V = U
!            VV = UU
!
!            if ( abs(UU(1,1)*S(1)*VV(1,1) + UU(1,2)*S(2)*VV(1,2) - zxx).gt.1d-8 ) then
!               continue
!            end if
!
!            if ( abs(UU(1,1)*S(1)*VV(2,1) + UU(1,2)*S(2)*VV(2,2) - zxy).gt.1d-8 ) then
!               continue
!            end if
!
!            if ( abs(UU(2,1)*S(1)*VV(1,1) + UU(2,2)*S(2)*VV(1,2) - zyx).gt.1d-8 ) then
!               continue
!            end if
!
!            if ( abs(UU(2,1)*S(1)*VV(2,1) + UU(2,2)*S(2)*VV(2,2) - zyy).gt.1d-8 ) then
!               continue
!            end if

               if (abs(S(1)) > abs(S(2))) then
                  k = 1
               else
                  k = 2
               end if

               zss(2, i, j) = UU(1, k) ! maximum change direction vector
               zss(3, i, j) = UU(2, k) ! maximum change direction vector
               zss(4, i, j) = S(k) * area ! maximum change singular value times control volume area

!           ridge detection
               dum = UU(1, k) * zx + UU(2, k) * zy

               zss(5, i, j) = -dum / (S(k) + 1.0e-8_dp) ! ridge distance in maximum change direction

               exit
            end do

         end do
      end do

      call readyy('Compute sample Hessians', -1.0_dp)

      iHesstat = iHesstat_OK

      ierror = 0

!  error handling
1234  continue

      return
   end subroutine comp_sampleHessian

end module m_comp_samplehessian
