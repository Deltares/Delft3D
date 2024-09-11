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

      subroutine TEKFN(NSC, NF, JW, X, Y, N, X1, X2, Y1, Y2, NCOL, TITLE, JAUTO, JP, DAG, kp1)
         use m_flow, only: kplotfrombedorsurface
         use unstruc_colors, only: ncolblack
         use m_depmax
         use m_ktext
         use m_fbox
         implicit none
         integer, parameter :: MX=366, NX=20
         double precision :: dag
         double precision :: dxh
         double precision :: dyh
         double precision :: fmx(NX)
         integer :: i, kp
         integer, save :: ini = 0
         integer :: j
         integer :: jauto
         integer :: jp
         integer :: jw
         integer :: n
         integer :: ncol
         integer :: nf
         integer :: nsc
         integer :: kp1
         double precision :: fx1, fx2, fy1, fy2
         double precision :: x1
         double precision :: x2
         double precision :: xo(MX, NX)
         double precision :: xtx
         double precision :: y1
         double precision :: y2
         double precision :: yo(MX, NX)
         double precision :: ytx, rcx, rcy
         character TITLE * (*), TEX * 16
         double precision :: X(N), Y(N), XX(4), YY(4), ZZ(4)

         ! NSC schermnr
         ! NF  functienr
         ! JW  update assen 1 = ja, niet 1 = nee
         ! JP  teken profielen 1 = ja, 2=circ, 3 = teken isolijnen
         ! in dat geval DAG (nr van de dag) toevoegen

         kp = kp1

         if (INI == 0) then
            do J = 1, NX
               FMX(J) = 0
               do I = 1, MX
                  XO(I, J) = 0
                  YO(I, J) = 0
               end do
            end do
            INI = INI + 1
         end if

         if (N < 2) return

         Fx1 = 1.0d20
         Fx2 = -1.0d20
         do I = 1, N
            Fx1 = min(X(I), Fx1)
            Fx2 = max(X(I), Fx2)
         end do

         Fy1 = 1.0d20
         Fy2 = -1.0d20
         do I = 1, N
            Fy1 = min(y(I), Fy1)
            Fy2 = max(y(I), Fy2)
         end do

         if (JAUTO == 1) then
            X1 = Fx1
            X2 = max(Fx2, Fx1 + 1d-4)

            if (fy1 < 2d0 * y1 - y2) return
            if (fy2 > 2d0 * y2 - y1) return
            if (fx1 < 2d0 * x1 - x2) return
            if (fx2 > 2d0 * x2 - x1) return
         end if

         if (Fx1 < -1.0d6 .or. Fx2 > 1.0d6) then
            call KTEXT(TITLE, 2, 2, 60)
            call KTEXT('TOO LARGE FOR PLOTTING', 3, 3, 60)
            return
         end if
         if (Fy1 < -1.0d6 .or. Fy2 > 1.0d6) then
            call KTEXT(TITLE, 2, 2, 60)
            call KTEXT('TOO LARGE FOR PLOTTING', 3, 3, 60)
            return
         end if

         call SETWINDOW(NSC, X1, Y1, X2, Y2, DXH, DYH) ! TEKEN IN WINDOW NR ZOVEEL

         call SETCOL(NCOL)

         if (JW == 1) then
            call BOX(X1, Y1, X2, Y2)

            ! CALL IGRCHARSIZE(2.0,1.0)
            tex = ' '
            write (tex(2:10), '(F8.1)') fy2 - fy1
            call DRAWTEXT(real(X1), real(Y2 + DYH), TITLE//tex)

            if (abs(x1) < 1d3) then
               write (TEX, '(F8.3)') X1
            else
               write (TEX, '(e10.3)') X1
            end if
            call DRAWTEXT(real(X1), real(Y1 - DYH), TEX)

            if (abs(x2) < 1d3) then
               write (TEX, '(F8.3)') X2
               call DRAWTEXT(real(X2 - 6 * DXH), real(Y1 - DYH), TEX)
            end if

            call MOVABS((X1 + X2) / 2, Y1)
            call LNABS((X1 + X2) / 2, Y1 + DYH / 2)

         end if

         if (JP == 1 .or. JP == 2) then ! JA PROFIELEN
            if (JP == 1) then
               call DISPF2(X, Y, N, N, NCOL) ! HUIDIGE PROFIEL TEKENEN
            else
               rcx = 5d-3 * (x2 - x1)
               rcy = 5d-3 * (y2 - y1)
               call DISPF2cir(X, Y, N, RCx, rcy, NCOL)
               if (kp > 0 .and. kp <= n) then ! print layer value
                  if (kplotfrombedorsurface /= 1) then
                     kp = n - kp + 1
                  end if
                  call movabs(x(kp), y(kp))
                  call setcol(ncolblack) ! 31)
                  call fbox(x(kp) - 2 * rcx, y(kp) - 2 * rcy, x(kp) + 2 * rcx, y(kp) + 2 * rcy)

                  write (TEX, '(E13.5)') X(kp)
                  xtx = x(kp)
                  if (xtx > (x1 + x2) / 2) then
                     xtx = x2 - (xtx - x1)
                  end if
                  ytx = Y(kp)
                  call GTEXT(TEX, xtx, ytx, NCOL)

                  write (TEX, '(F13.5)') y(kp) - y1
                  ytx = ytx - 0.05d0 * (y2 - y1)
                  call GTEXT(TEX, xtx, ytx, NCOL)

               end if
            end if

            if (JW == 1) then ! ALLEEN BIJ PROFIELEN EN ALS WINDOW GETEKEND WORDT
               ! WRITE (TEX,'(E16.5)') FMX(NF)   ! max profile value
               xtx = X2 - 10d0 * DXH
               ytx = Y2 - DYH
               ! CALL GTEXT(TEX, xtx, ytx, 0)
               write (TEX, '(E16.5)') Fx2
               call GTEXT(TEX, xtx, ytx, NCOL)
               FMX(NF) = Fx2

               write (TEX, '(E16.5)') Fx1 !  sum(x)/dble(n)    ! ave profile value
               xtx = X1 - DXH
               ytx = Y2 - DYH
               call GTEXT(TEX, xtx, ytx, Ncol)

            end if
         else if (jp > 0) then ! ISOLIJNEN
            VMAX = 22
            VMIN = 2
            NV = 10
            DV = VMAX - VMIN
            do I = 1, NV
               VAL(I) = VMIN + (I - 1) * DV / (NV - 1)
            end do
!C        CALL ISOSCALE()
            do I = 2, N
               XX(1) = DAG - 1
               XX(2) = DAG - 1
               XX(3) = DAG
               XX(4) = DAG
               YY(1) = YO(I, NF)
               YY(2) = YO(I - 1, NF)
               YY(3) = Y(I - 1)
               YY(4) = Y(I)
               ZZ(1) = XO(I, NF)
               ZZ(2) = XO(I - 1, NF)
               ZZ(3) = X(I - 1)
               ZZ(4) = X(I)
               call ISOFIL(XX, YY, ZZ, 4, 0)
            end do
         end if

         return
      end
