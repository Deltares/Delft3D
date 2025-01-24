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

!>   generate grid between fixed points on a spline that itself is defined by control points
!>     in:  t(Nt) fixed points on spline
!>          x(N)  x-coordinates of spline control points
!>          y(N)  y-coordinates of spline control points
!>          imax  maximum array size (should be .ge. n, nt and kmax)
!>          N     number of spline control points
!>          Nt    number of fixed points
!>          MNfac number of grid intervals between fixed points
!>          H     significant height, where the grid should be equidistant (>0) or disable (<=0)
!>
!>     out: xh(kmax) x-coordinates of grid points
!>          yh(kmax) y-coordinates of grid points
!>          kmax     number of grid points = 1+MNfac*(NT-1)
!>          tt(imax) spline-coordinates of grid points
module m_makespl
   implicit none
contains
   subroutine MAKESPL(T, X, Y, imax, N, NT, MNFAC, XH, YH, KMAX, TT, H)
      use precision, only: dp
      use m_gridsettings
      use m_makes
      use m_makessq
      use m_getxy

      integer :: imax, n, nt, kmax, mnfac
      real(kind=dp) :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX), &
                       S2(IMAX), SSQ(IMAX), XH(IMAX), YH(IMAX), &
                       A(IMAX), SL(IMAX), SR(IMAX)
      real(kind=dp), intent(in) :: H !< for curvature adapted meshing

      real(kind=dp), dimension(IMAX), intent(out) :: TT !< spline-coordinates of grid points

      integer :: L, k1, k2, jadip, k
!     Maak interpolatie

!     Eerst splines X,Y en S aanmaken
      call MAKES(X, Y, X2, Y2, T, S, S2, imax, N, NT, H)

      KMAX = MNFAC * (NT - 1) + 1

      if (NT >= 2) then
         call MAKESSQ(S, A, SR, SL, SSQ, NT, MNFAC, IMAX)

!        Check op positief en monotoon
         do L = 1, NT - 1
            K1 = MNFAC * (L - 1) + 1
            K2 = K1 + MNFAC

            JADIP = 0
23          if (JADIP == 1) then
               do K = K1 + 1, K2 - 1
                  SSQ(K) = 0.5 * (SSQ(K - 1) + SSQ(K + 1))
               end do
            end if

            do K = K1, K2 - 1
               if (SSQ(K + 1) < SSQ(K)) then
                  JADIP = 1
                  goto 23
               end if
            end do

         end do
      else
         SSQ(1) = T(1)
      end if

!     Punten terug invullen in oorspronkelijke spline
      do K = 1, KMAX
         call GETXY(T, X, X2, Y, Y2, imax, N, NT, SSQ(K), XH(K), YH(K), TT(K), H)
      end do

      return
   end subroutine makespl
end module m_makespl
