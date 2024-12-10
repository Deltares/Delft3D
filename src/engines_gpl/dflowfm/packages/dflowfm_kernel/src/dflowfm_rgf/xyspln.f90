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
!>     SPLINE INTERPOLATIE BINNEN ALLE GROVE CELLEN
!!     LIJN 1,2 ZIJN DE VERTICALE   CELWANDEN
!!     LIJN 3,4 ZIJN DE HORIZONTALE CELWANDEN
      subroutine XYSPLN(X, Y, XR, YR, &
                        XI2, YI2, XJ2, YJ2, XRH, YRH, &
                        mmax, nmax, imax, &
                        M1, N1, M2, N2, MC, NC, &
                        MFAC, NFAC, IJYES)
         use precision, only: dp
         use m_missing
         use m_splint
         use m_readyy
         use m_get_ij
         implicit none

         integer :: mmax, nmax, imax, m1, n1, m2, n2, mc, nc, mfac, nfac
         real(kind=dp) :: X(MMAX, NMAX), XR(MMAX, NMAX), &
                          Y(MMAX, NMAX), YR(MMAX, NMAX), &
                          XI2(MMAX, NMAX), XJ2(MMAX, NMAX), &
                          YI2(MMAX, NMAX), YJ2(MMAX, NMAX), &
                          XH1(IMAX), XH21(IMAX), &
                          XH2(IMAX), XH22(IMAX), &
                          XH3(IMAX), XH23(IMAX), &
                          XH4(IMAX), XH24(IMAX), &
                          YH1(IMAX), YH21(IMAX), &
                          YH2(IMAX), YH22(IMAX), &
                          YH3(IMAX), YH23(IMAX), &
                          YH4(IMAX), YH24(IMAX), &
                          X1(IMAX), Y1(IMAX), &
                          X2(IMAX), Y2(IMAX), &
                          X3(IMAX), Y3(IMAX), &
                          X4(IMAX), Y4(IMAX), &
                          XRH(MMAX, NMAX), YRH(MMAX, NMAX)
         integer IJYES(MMAX, NMAX)

         real(kind=dp) :: af, TI, TJ
         integer :: md, nd, mfa, nfa, mfaa, nfaa, ki1, i1, i2, j1, j2, &
                    KI, LJ, LJ1, K, L, dum
         XR = dmiss
         YR = dmiss

         MD = M2 - M1
         ND = N2 - N1
         MFAA = MFAC
         NFAA = NFAC
         if (MD == 0) MFAA = 1
         if (ND == 0) NFAA = 1

         KI1 = 0
         do I1 = 1, MC - 1
            AF = 0.20d0 + 0.70d0 * dble(I1 - 1) / (MC - 1)
            call READYY(' ', AF)
            if (I1 >= M1 .and. I1 < M2) then
               MFA = MFAA
            else
               MFA = 1
            end if
            I2 = I1 + 1
            call GETIJ(X, XH1, mmax, nmax, imax, I1, I1, 1, NC)
            call GETIJ(XI2, XH21, mmax, nmax, imax, I1, I1, 1, NC)
            call GETIJ(X, XH2, mmax, nmax, imax, I2, I2, 1, NC)
            call GETIJ(XI2, XH22, mmax, nmax, imax, I2, I2, 1, NC)
            call GETIJ(Y, YH1, mmax, nmax, imax, I1, I1, 1, NC)
            call GETIJ(YI2, YH21, mmax, nmax, imax, I1, I1, 1, NC)
            call GETIJ(Y, YH2, mmax, nmax, imax, I2, I2, 1, NC)
            call GETIJ(YI2, YH22, mmax, nmax, imax, I2, I2, 1, NC)
            LJ1 = 0
            do J1 = 1, NC - 1
               if (J1 >= N1 .and. J1 < N2) then
                  NFA = NFAA
               else
                  NFA = 1
               end if
               J2 = J1 + 1
               call GETIJ(X, XH3, mmax, nmax, imax, 1, MC, J1, J1)
               call GETIJ(XJ2, XH23, mmax, nmax, imax, 1, MC, J1, J1)
               call GETIJ(X, XH4, mmax, nmax, imax, 1, MC, J2, J2)
               call GETIJ(XJ2, XH24, mmax, nmax, imax, 1, MC, J2, J2)
               call GETIJ(Y, YH3, mmax, nmax, imax, 1, MC, J1, J1)
               call GETIJ(YJ2, YH23, mmax, nmax, imax, 1, MC, J1, J1)
               call GETIJ(Y, YH4, mmax, nmax, imax, 1, MC, J2, J2)
               call GETIJ(YJ2, YH24, mmax, nmax, imax, 1, MC, J2, J2)
               if (IJYES(I1, J1) == 1) then

                  do K = 1, MFA + 1
                     TI = (I1 - 1) + dble(K - 1) / dble(MFA)
                     call SPLINT(XH3, XH23, MC, TI, X3(K))
                     call SPLINT(XH4, XH24, MC, TI, X4(K))
                     call SPLINT(YH3, YH23, MC, TI, Y3(K))
                     call SPLINT(YH4, YH24, MC, TI, Y4(K))
                  end do
                  do L = 1, NFA + 1
                     TJ = (J1 - 1) + dble(L - 1) / dble(NFA)
                     call SPLINT(XH1, XH21, NC, TJ, X1(L))
                     call SPLINT(XH2, XH22, NC, TJ, X2(L))
                     call SPLINT(YH1, YH21, NC, TJ, Y1(L))
                     call SPLINT(YH2, YH22, NC, TJ, Y2(L))
!                 als je equidistant wil interpoleren
                     if (J1 == -1) then
                        call EQDINT(XH1, imax, TJ, X1(L))
                        call EQDINT(XH2, imax, TJ, X2(L))
                        call EQDINT(YH1, imax, TJ, Y1(L))
                        call EQDINT(YH2, imax, TJ, Y2(L))
                     end if
                  end do
                  if (X1(1) == 0) then
                     DUM = 0
                  end if
                  call TRANFN(X1, X2, X3, X4, &
                              Y1, Y2, Y3, Y4, &
                              mmax, nmax, imax, &
                              MFA, NFA, XRH, YRH)
                  do K = 1, MFA + 1
                     do L = 1, NFA + 1
                        KI = KI1 + K
                        LJ = LJ1 + L
                        XR(KI, LJ) = XRH(K, L)
                        YR(KI, LJ) = YRH(K, L)
                     end do
                  end do
               end if
               LJ1 = LJ1 + NFA
            end do
            KI1 = KI1 + MFA
         end do
         return
      end subroutine XYSPLN
