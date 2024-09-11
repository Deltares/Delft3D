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

   subroutine TEKSAM(MET)

      use unstruc_colors
      use m_missing, only: DMISS
      use unstruc_opengl, only: jaopengl
      use m_samples
      use unstruc_display
      use m_arcinfo
      use m_perspx
      use m_halt2
      
      implicit none
      double precision :: RC
      double precision :: hrc
      integer :: i, KMOD
      integer :: key
      double precision :: x
      double precision :: y
      double precision :: z
      integer :: MET
!     TEKEN SAMPLES

      if (MET == 0) return

      if (MET == 4 .or. MET == 5) call SETTEXTSIZE()
      RC = 1.7d0 * RCIR
      HRC = RCIR / 2
      KMOD = max(1, NS / 100)
      key = 0

!     Fix for OpenGL rendering
      if (jaopengl == 1 .and. MET == 1) then
         MET = 7
      end if

      if (met == 5) then
         call SETCOL(KLSAM)
      else
         call minmxsam()
      end if

      do I = 1, NS

         if (mod(I, KMOD) == 0) then
            call HALT2(KEY)
            if (KEY == 1) return
         end if

         X = XS(I)
         Y = YS(I)
         Z = ZS(I)

         if (Z == DMISS) cycle ! SPvdP: structured sample data may comprise missing values

         call tek1sample(x, y, z, met, hrc, i)

      end do

      call IGRFILLPATTERN(4, 0, 0)
      call IGRCHARDIRECTION('H')
      return
   end subroutine TEKSAM

   subroutine TEKarc(MET)
      use m_arcinfo
      use unstruc_display
      use m_missing, only: DMISS
      use m_halt2

      implicit none
      double precision :: hrc, rc, x, y, z
      integer :: met, m, n, key

      if (MET == 4 .or. MET == 5) call SETTEXTSIZE()
      RC = 1.7d0 * RCIR
      HRC = RCIR / 2

      if (met == 5) then
         call SETCOL(KLSAM)
      else
         call minmxarc()
      end if

      do n = 1, nca

         call HALT2(KEY)
         if (KEY == 1) return
         do m = 1, mca

            z = d(m, n)
            if (z == dmiss) cycle
            x = x0 + dxa * (m - 1)
            y = y0 + dya * (n - 1)
            call tek1sample(x, y, z, met, hrc, m)

         end do
      end do
   end subroutine TEKarc

   subroutine tek1sample(x, y, z, met, hrc, m)
      use unstruc_colors
      use unstruc_display
      use m_arcinfo
      use m_drawthis
      use m_htext

      implicit none

      double precision :: x, y, z, hrc
      integer :: met, m, ncol
      logical, external :: inview

      if (INVIEW(X, Y)) then
         if (NDRAW(9) == 2) then
!            CALL VIEW(XS(I),YS(I),ZS(I),X0S,Y0S,VS,X,Y,ZC)
         end if
         if (MET /= 5) then
            call ISOCOL2(Z, NCOL)
         end if
         if (MET == 1 .or. MET == 2) then
            if (NDRAW(9) == 1) then
!
!               CALL MOVABS(X,Y)
!               CALL CIR(RCIR)
!!              CALL HTEXT(ZS(I),X,Y)

               call box(x - 0.5d0 * rcir, y - 0.5d0 * rcir, x + 0.5d0 * rcir, y + 0.5d0 * rcir)

               if (MET == 2) then
                  call MOVABS(X, Y)
                  call IGRFILLPATTERN(0, 0, 0)
                  call SETCOL(1)
                  call CIR(RCIR)
                  call IGRFILLPATTERN(4, 0, 0)
               end if

            end if
         else if (MET == 3) then
            call PTABS(X, Y)
         else if (MET == 4 .or. MET == 5) then
            call HTEXT(Z, X, Y)
         else if (MET == 6) then
            call MOVABS(X, Y)
            call CIR(RCIR)
            call HTEXT(Z, X + rcir, Y)
         else if (MET == 7) then
            call KREC5(X, Y, HRC, HRC)
         else if (MET == 8) then
            call HITEXT(m, X, Y)
         end if
      end if

   end subroutine tek1sample

