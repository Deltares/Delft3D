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

!>    compute the intersection of two splines
module m_sect3r

implicit none

private

public :: sect3r

contains

      subroutine SECT3R(XI, YI, XJ, YJ, imax, CRP, &
                        NUMPI, NUMPJ, NUMCRO, TIV, TJV, XP, YP)
         use precision, only: dp
         use m_missing
         use geometry_module, only: dbdistance, cross
         use m_sferic, only: jsferic, jasfer3D
         use m_splint
         use m_spline

!     BEPAAL HET SNYPUNT VAN DE 2 SPLINES NR I EN J      USE DIMENS

         integer, intent(in) :: imax !< array size
         integer, intent(in) :: numpi !< number of control points of first spline
         integer, intent(in) :: numpj !< number of control points of second spline

         real(kind=dp), dimension(imax), intent(in) :: xi, yi !< control point coordinates of first spline
         real(kind=dp), dimension(imax), intent(in) :: xj, yj !< control point coordinates of second spline

         integer, intent(out) :: numcro !< number of intersections found

         real(kind=dp), intent(out) :: crp !< cross product (SPvdP: dimensional, so only look at sign)
         real(kind=dp), intent(out) :: tiv !< spline-coordinate of intersetion point on first spline (0 corresponds to first control point, 1 to second, and so on)
         real(kind=dp), intent(out) :: tjv !< spline-coordinate of intersetion point on second spline
         real(kind=dp), intent(out) :: xp, yp !< coordinates of intersection point

         real(kind=dp) :: ti, tj, tip, tjp, ti0, ti1, ti2, tj0, tj1, tj2, tii, tjj, &
                          tio, tjo, &
                          timx, tjmx, eps, eps2, xcr, ycr, crs, dis, &
                          xo, yo
         real(kind=dp) :: sl, sm, xi2(imax), yi2(imax), xj2(imax), yj2(imax), xc(4), yc(4)
         integer :: i, j, jo, jacros, k

         real(kind=dp) :: sdist, sdistmin

         NUMCRO = 0
         EPS = 0.0001
         EPS2 = 0.000001
         TI = -1
         TJ = -1

         TI0 = 0d0
         TJ0 = 0d0

         sdistmin = 1d99 ! used for selecting middle intersection in case of multiple intersections

         do I = 1, NUMPI - 1 ! SNIJDEN RECHTE LIJNSTUKKEN?
            do J = 1, NUMPJ - 1
               XC(1) = XI(I)
               XC(2) = XI(I + 1)
               XC(3) = XJ(J)
               XC(4) = XJ(J + 1)
               YC(1) = YI(I)
               YC(2) = YI(I + 1)
               YC(3) = YJ(J)
               YC(4) = YJ(J + 1)
               CRP = -1234d0
               call CROSS(XC(1), YC(1), XC(2), YC(2), XC(3), YC(3), XC(4), YC(4), &
                          JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
               if (JACROS == 1) then
                  if (numpi == 2) then
                     sdist = min(sdistmin, abs(SL - 0.5d0))
                  else if (numpj == 2) then
                     sdist = abs(SM - 0.5d0)
                  else
                     sdist = sdistmin
                  end if

                  if (sdist < sdistmin .or. NUMCRO == 0) then
                     sdistmin = sdist
                     TIP = TI
                     TJP = TJ
!                NUMCRO = NUMCRO + 1
                     NUMCRO = 1
                     TI0 = I - 1
                     TJ0 = J - 1
                     TI = TI0 + SL
                     TJ = TJ0 + SM
                  else
!                NUMCRO = NUMCRO + 1
                  end if

                  if (NUMCRO >= 2 .and. abs(TIP - TI) > EPS .and. abs(TJP - TJ) > EPS) then
                     return
                  end if
               end if
            end do
         end do

         if (NUMCRO == 0) then
            do I = 1, NUMPI - 1
               JO = -999
               do J = 1, NUMPJ - 1 ! ZO NIET, GRENZEN OPREKKEN
                  if (J > JO + 1) then ! NA OPREKKEN GRENZEN NIET TWEE
!              JO    = J                  ! KEER NAAST ELKAAR ZOEKEN
!             SPvdP: previous line causes asymmetries
                     XC(1) = XI(I)
                     XC(2) = XI(I + 1)
                     XC(3) = XJ(J)
                     XC(4) = XJ(J + 1)
                     YC(1) = YI(I)
                     YC(2) = YI(I + 1)
                     YC(3) = YJ(J)
                     YC(4) = YJ(J + 1)
                     SL = dmiss; SM = dmiss
                     call CROSS(XC(1), YC(1), XC(2), YC(2), XC(3), YC(3), XC(4), YC(4), &
                                JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

!             BIJ NIEUW ZOEKEN MAG JE OOK NET BUITEN DE RECHTE LIJNSTUKKEN VALLEN 20-5-2003
                     if (SL > -0.2 .and. SL < 1.2 .and. SM > -0.2 .and. SM < 1.2) then
                        NUMCRO = NUMCRO + 1
                        TI0 = I - 1
                        TJ0 = J - 1
                        TI = TI0 + SL
                        TJ = TJ0 + SM
                     end if
                  end if
               end do
            end do
         end if

         TIO = TI0 ! SPvdP: TIO and TJO uninitialized, may be typing error
         TJO = TJ0

         if (NUMCRO == 0) return

         NUMCRO = 0

         TIMX = NUMPI - 1
         TJMX = NUMPJ - 1

         SL = TI - TIO
         SM = TJ - TJO

         TI = max(0.0d0, min(TIMX, TI))
         TJ = max(0.0d0, min(TJMX, TJ))

         call SPLINE(XI, NUMPI, XI2)
         call SPLINE(YI, NUMPI, YI2)
         call SPLINE(XJ, NUMPJ, XJ2)
         call SPLINE(YJ, NUMPJ, YJ2)

         TII = 1.0
         TJJ = 1.0
         K = 0

20       continue
         K = K + 1

         if (SL > 0. .and. SL < 1.) then
            TII = 0.5 * TII
         end if
         if (SM > 0. .and. SM < 1.) then
            TJJ = 0.5 * TJJ
         end if
         TI1 = max(0.0d0, min(TIMX, TI - TII / 2))
         TI2 = max(0.0d0, min(TIMX, TI + TII / 2))
         TJ1 = max(0.0d0, min(TJMX, TJ - TJJ / 2))
         TJ2 = max(0.0d0, min(TJMX, TJ + TJJ / 2))
         TII = TI2 - TI1
         TJJ = TJ2 - TJ1

         call SPLINT(XI, XI2, NUMPI, TI1, XC(1))
         call SPLINT(YI, YI2, NUMPI, TI1, YC(1))
         call SPLINT(XI, XI2, NUMPI, TI2, XC(2))
         call SPLINT(YI, YI2, NUMPI, TI2, YC(2))
         call SPLINT(XJ, XJ2, NUMPJ, TJ1, XC(3))
         call SPLINT(YJ, YJ2, NUMPJ, TJ1, YC(3))
         call SPLINT(XJ, XJ2, NUMPJ, TJ2, XC(4))
         call SPLINT(YJ, YJ2, NUMPJ, TJ2, YC(4))

!        CALL SETCOL(14*K+26)
!        CALL MOVABS(XC(1),YC(1))
!        CALL  LNABS(XC(2),YC(2))
!        CALL MOVABS(XC(3),YC(3))
!        CALL  LNABS(XC(4),YC(4))

!        CALL RCIRC(XC(1),YC(1))
!        CALL RCIRC(XC(2),YC(2))
!        CALL RCIRC(XC(3),YC(3))
!        CALL RCIRC(XC(4),YC(4))
!        CALL TOEMAAR()

         XO = XCR
         YO = YCR
         SL = dmiss; SM = dmiss
         CRS = -1234d0
         call CROSS(XC(1), YC(1), XC(2), YC(2), XC(3), YC(3), XC(4), YC(4), &
                    JACROS, SL, SM, XCR, YCR, CRS, jsferic, dmiss)
         if (SL > -2. .and. SL < 3.0 .and. SM > -2. .and. SM < 3.0) then
            TIO = TI
            TJO = TJ

            TI = TI1 + SL * TII
            TJ = TJ1 + SM * TJJ

            TI = max(0.0d0, min(TIMX, TI))
            TJ = max(0.0d0, min(TJMX, TJ))

            if (JACROS == 1) then !ZOLANG IE NOG KRUIST WORDT UITPRODUCT BEPAALD
               NUMCRO = 1
               CRP = CRS
            end if

            if (K < 20) then
               if (abs(TI - TIO) > EPS .or. abs(TJ - TJO) > EPS) then
!                DIS = SQRT((XCR-XO)*(XCR-XO)+(YCR-YO)*(YCR-YO))
                  dis = dbdistance(xo, yo, xcr, ycr, jsferic, jasfer3D, dmiss)
                  if (DIS > EPS2) goto 20 ! NIET VERDER VERKLEINEN ALS PUNTEN AL BIJNA IDENTIEK
               end if
            end if
         end if

         if (NUMCRO == 1) then
            XP = XCR
            YP = YCR
            TIV = TI
            TJV = TJ
         end if
!     CALL TOEMAAR()

         return
      end subroutine sect3r

end module m_sect3r
