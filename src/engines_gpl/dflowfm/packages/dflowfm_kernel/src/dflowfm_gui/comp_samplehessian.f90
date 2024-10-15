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

!> compute sample Hessians
module m_comp_samplehessian

implicit none

contains

subroutine comp_sampleHessian(ierror)
   use m_comp_samplegradi
   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D
   use m_readyy
   
   implicit none

   integer, intent(out) :: ierror !< error (1) or not (0)

   double precision, dimension(2, 2) :: UU, VV ! for SVD: H = USV'
   double precision, dimension(2) :: S ! for SVD: H = USV'

   double precision, dimension(2) :: gradiL, gradiR ! sample gradients at i-edges
   double precision, dimension(2) :: gradjL, gradjR ! sample gradients at j-edges
   double precision, dimension(2) :: SniL, sniR ! i-edge surface vector (for divergence)
   double precision, dimension(2) :: SnjL, snjR ! j-edge surface vector (for divergence)
   double precision :: dareaiL, dareaiR ! contribution to control volume area (for divergence)
   double precision :: dareajL, dareajR ! contribution to control volume area (for divergence)

   double precision :: area ! control volume area of divergence operator (for Laplacian)

   double precision :: zxx, zxy, zyx, zyy ! second order partial derivatives
   double precision :: zx, zy ! first order partial derivatives

   double precision :: af, dum, Dh

   integer :: i, j, k, nrot, ip, ihasridge

!  compute sample mesh width
   Dh = min(dbdistance(xs(1), ys(1), xs(2), ys(2), jsferic, jasfer3D, dmiss), dbdistance(xs(1), ys(1), xs(1 + MXSAM), ys(1 + MXSAM), jsferic, jasfer3D, dmiss))

   ierror = 1

   if (MXSAM < 3 .or. MYSAM < 3) goto 1234

   zss(4, 1:MXSAM, 1:MYSAM) = 0d0
   zss(5, 1:MXSAM, 1:MYSAM) = DMISS

   call readyy('Computing sample Hessians', 0d0)
   do i = 2, MXSAM - 1
      af = dble(i - 2) / dble(max(MXSAM - 3, 1))
      call readyy('Computing sample Hessians', af)
      do j = 2, MYSAM - 1
!         if ( i.eq.614 .and. j.eq.154 )
         ip = i + (j - 1) * MXSAM
         if (abs(xs(ip) - 87270) < 1d-8 .and. abs(ys(ip) - 415570) < 1d-8) then
            continue
         end if
         zxx = 0d0
         zxy = 0d0
         zyx = 0d0
         zyy = 0d0
         UU = 0d0
         VV = 0d0
         zx = 0d0
         zy = 0d0
         S = 0d0
         k = 0
         ihasridge = 0
         do
            call comp_samplegradi(0, i, j, gradiR, SniR, dareaiR, dum)
            if (gradiR(1) == DMISS .or. gradiR(1) == DMISS) exit

            call comp_samplegradi(0, i - 1, j, gradiL, SniL, dum, dareaiL)
            if (gradiL(1) == DMISS .or. gradiL(1) == DMISS) exit

            call comp_samplegradi(1, i, j, gradjR, SnjR, dareajR, dum)
            if (gradjR(1) == DMISS .or. gradjR(1) == DMISS) exit

            call comp_samplegradi(1, i, j - 1, gradjL, SnjL, dum, dareajL)
            if (gradjL(1) == DMISS .or. gradjL(1) == DMISS) exit

            area = dareaiL + dareaiR + dareajL + dareajR
            zxx = (gradiR(1) * SniR(1) - gradiL(1) * SniL(1) + gradjR(1) * SnjR(1) - gradjL(1) * SnjL(1)) / area
            zxy = (gradiR(1) * SniR(2) - gradiL(1) * SniL(2) + gradjR(1) * SnjR(2) - gradjL(1) * SnjL(2)) / area
            zyx = (gradiR(2) * SniR(1) - gradiL(2) * SniL(1) + gradjR(2) * SnjR(1) - gradjL(2) * SnjL(1)) / area
            zyy = (gradiR(2) * SniR(2) - gradiL(2) * SniL(2) + gradjR(2) * SnjR(2) - gradjL(2) * SnjL(2)) / area

            zx = (0.5d0 * (zss(1, i + 1, j) + zss(1, i, j)) * SniR(1) &
                  - 0.5d0 * (zss(1, i - 1, j) + zss(1, i, j)) * SniL(1) &
                  + 0.5d0 * (zss(1, i, j + 1) + zss(1, i, j)) * SnjR(1) &
                  - 0.5d0 * (zss(1, i, j - 1) + zss(1, i, j)) * SnjL(1)) / area
            zy = (0.5d0 * (zss(1, i + 1, j) + zss(1, i, j)) * SniR(2) &
                  - 0.5d0 * (zss(1, i - 1, j) + zss(1, i, j)) * SniL(2) &
                  + 0.5d0 * (zss(1, i, j + 1) + zss(1, i, j)) * SnjR(2) &
                  - 0.5d0 * (zss(1, i, j - 1) + zss(1, i, j)) * SnjL(2)) / area

!           Eigendecompostion
            VV(1, 1) = zxx; VV(1, 2) = zxy
            VV(2, 1) = zyx; VV(2, 2) = zyy
            call jacobi(VV, 2, 2, S, UU, nrot)

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

            zss(5, i, j) = -dum / (S(k) + 1d-8) ! ridge distance in maximum change direction

            exit
         end do

      end do
   end do

   call readyy('Compute sample Hessians', -1d0)

   iHesstat = iHesstat_OK

   ierror = 0

!  error handling
1234 continue

   return
end subroutine comp_sampleHessian

end module m_comp_samplehessian
