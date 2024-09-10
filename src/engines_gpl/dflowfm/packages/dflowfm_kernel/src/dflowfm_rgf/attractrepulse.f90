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

      subroutine ATTRACTREPULSE(XH, YH, X, Y, mmax, nmax, MC, NC, NUMP, JA)
         use m_missing
         use m_gridsettings
         use m_sferic
         use m_wearelt
         use geometry_module, only: dbdistance
         use m_tolocl
         use m_grid_block
         use m_smeerfunctie
         implicit none
         integer :: mmax, nmax, mc, nc, nump, ja
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX), XH(MMAX, NMAX), YH(MMAX, NMAX)
!     ATTRACTIE, REPULSIE

         integer :: M1, N1, M2, N2, IN, JN, I, J, II, JJ, ii1, ii2, jj1, jj2, JANU, numpi, numpj
         double precision :: rsx, teken, dx, dy, dxy, dxy0, x0, y0, xn, yn, rn, fr

         M1 = MB(1)
         N1 = NB(1)
         M2 = MB(2)
         N2 = NB(2)
!     IN    = MIN(1,M2-M1)
!     JN    = MIN(1,N2-N1)
         JN = min(1, M2 - M1)
         IN = min(1, N2 - N1)
         NUMPI = IN * NUMP
         NUMPJ = JN * NUMP
!     RSX   = DSIX
         RSX = dbDISTANCE(X1, Y1, X2, Y2, jsferic, jasfer3D, dmiss)
         RSX = RSX / 6
         JANU = JA
         do I = M1, M2
            do J = N1, N2
               X0 = X(I, J)
               Y0 = Y(I, J)
               if (X0 /= XYMIS) then
                  if (NPT <= 2) then
                     II1 = max(1, I - NUMPI)
                     II2 = min(I + NUMPI, MC)
                     JJ1 = max(1, J - NUMPJ)
                     JJ2 = min(J + NUMPJ, NC)
                  else
                     II1 = max(MB(3), I - NUMPI * 1000)
                     II2 = min(I + NUMPI * 1000, MB(4))
                     JJ1 = max(NB(3), J - NUMPJ * 1000)
                     JJ2 = min(J + NUMPJ * 1000, NB(4))
                  end if
                  do Ii = II1, II2
                     do Jj = JJ1, JJ2
                        XN = X(II, JJ)
                        if (XN /= XYMIS .and. .not. (II == I .and. JJ == J)) then
                           YN = Y(II, JJ)
                           if (NPT <= 2) then
                              RN = dbDISTANCE(XN, YN, X0, Y0, jsferic, jasfer3D, dmiss)
!                          RN = SQRT( (XN - X0)**2 + (YN - Y0)**2 )
                              if (RN < RSX) then
                                 FR = (RSX - RN) / RSX
                                 if (IN == 1) then
                                    TEKEN = dble(sign(1, II - I))
                                 else if (JN == 1) then
                                    TEKEN = dble(sign(1, JJ - J))
                                 end if
                                 call DXYB(X, Y, mmax, nmax, MC, &
                                           NC, II, JJ, IN, &
                                           JN, DXY0)
                                 DXY = RFAC * TEKEN * FR * JANU * DXY0
                                 if (JSFERIC == 1) DXY = RD2DG * DXY / RA
                                 DX = DXY * IN
                                 DY = DXY * JN
                                 call TOLOCL(DX, DY, X, Y, mmax, nmax, &
                                             II, JJ, 1)
                                 XH(II, JJ) = XN + DX
                                 YH(II, JJ) = YN + DY
                              end if
                           else
                              call SMEERFUNCTIE(II, JJ, I, J, FR, IN, JN)
                              if (IN == 1) then
                                 TEKEN = dble(sign(1, II - I))
                              else if (JN == 1) then
                                 TEKEN = dble(sign(1, JJ - J))
                              end if
                              call DXYB(X, Y, mmax, nmax, MC, &
                                        NC, II, JJ, JN, &
                                        IN, DXY0)
                              DXY = RFAC * TEKEN * FR * JANU * DXY0
                              if (JSFERIC == 1) DXY = RD2DG * DXY / RA
                              DX = DXY * IN
                              DY = DXY * JN
                              call TOLOCL(DX, DY, X, Y, mmax, nmax, &
                                          II, JJ, 1)
                              XH(II, JJ) = XN + DX
                              YH(II, JJ) = YN + DY
                           end if
                        end if
                     end do
                  end do
               end if
            end do
         end do
         return
      end subroutine attractrepulse
