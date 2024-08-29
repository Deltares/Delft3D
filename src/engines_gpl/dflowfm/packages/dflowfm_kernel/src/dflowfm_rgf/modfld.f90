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
module m_modfld
   implicit none
contains
!>     VELDTRANSLATIE VAN XH,YH OP BASIS X,Y RONDOM PUNT MP,NP
!!     ALS NLOC IS 1, DAN LOKAAL ORTHOGONALE TRANSLATIES
!!     INVLOEDSSFEER IN I,J IS RESP NUMP*IN EN NUMP*JN
   subroutine MODFLD(XH, YH, X, Y, &
                     mmax, nmax, MC, NC, MP, NP, &
                     NUMP, NLOC, IN, JN)
      use m_missing, only: xymis
      use m_wearelt, only: dsix
      use m_tolocl
      use m_grid_block
      use m_smeerfunctie

      integer :: mmax, nmax, mc, nc, mp, np, nump, nloc, in, jn

      double precision :: X(MMAX, NMAX), Y(MMAX, NMAX), XH(MMAX, NMAX), YH(MMAX, NMAX)
      double precision :: pi2, x0, y0, dx0, dy0, rsx, rn, fr, dx, dy, xn, yn
      integer :: m1, n1, m2, n2, ismeer, i, j

      PI2 = asin(1d0)
      X0 = X(MP, NP)
      Y0 = Y(MP, NP)
      DX0 = XH(MP, NP) - X(MP, NP)
      DY0 = YH(MP, NP) - Y(MP, NP)

      RSX = max(DSIX, sqrt(DX0 * DX0 + DY0 * DY0))
      if (IN == 1 .and. JN == 1) then
         if (NPT >= 2) then
            ISMEER = 1
            M1 = MB(3)
            M2 = MB(4)
            N1 = NB(3)
            N2 = NB(4)
         else
            ISMEER = 0
            M1 = max(1, MP - NUMP * IN)
            M2 = min(MC, MP + NUMP * IN)
            N1 = max(1, NP - NUMP * JN)
            N2 = min(NC, NP + NUMP * JN)
         end if
      else
         if (NPT >= 3) then
            ISMEER = 1
            M1 = max(MB(3), MP - 10000 * IN)
            M2 = min(MB(4), MP + 10000 * IN)
            N1 = max(NB(3), NP - 10000 * JN)
            N2 = min(NB(4), NP + 10000 * JN)
         else
            ISMEER = 0
            M1 = max(1, MP - NUMP * IN)
            M2 = min(MC, MP + NUMP * IN)
            N1 = max(1, NP - NUMP * JN)
            N2 = min(NC, NP + NUMP * JN)
         end if
      end if

      if (NLOC == 1) then
         call TOLOCL(DX0, DY0, X, Y, mmax, nmax, MP, NP, 0)
      end if
      do I = M1, M2
         do J = N1, N2
            XN = X(I, J)
            if (XN /= XYMIS) then
               YN = Y(I, J)
               if (ISMEER == 1) then
                  call SMEERFUNCTIE(I, J, MP, NP, FR, IN, JN)
                  DX = DX0 * FR
                  DY = DY0 * FR
                  if (NLOC == 1) then
                     call TOLOCL(DX, DY, X, Y, mmax, nmax, I, J, 1)
                  end if
                  XH(I, J) = XN + DX
                  YH(I, J) = YN + DY
               else
                  RN = sqrt((XN - X0)**2 + (YN - Y0)**2)
                  if (RN < RSX) then
!                    FR = (RSX - RN)/RSX
                     RN = PI2 * RN / RSX
!                    FR = COS(RN)/(1.0 + SIN(RN))
                     FR = (1 + cos(2 * RN)) / 2
                     DX = DX0 * FR
                     DY = DY0 * FR
                     if (NLOC == 1) then
                        call TOLOCL(DX, DY, X, Y, mmax, nmax, I, J, 1)
                     end if
                     XH(I, J) = XN + DX
                     YH(I, J) = YN + DY
                  end if
               end if
            end if
         end do
      end do
      return
   end subroutine modfld

end module m_modfld
