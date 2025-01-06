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

module m_editsplines
   use m_copyspline, only: copyspline
   use m_snap_spline, only: snap_spline
   use m_movespline, only: movespline
   use m_deleteselectedsplines, only: deleteselectedsplines
   use m_selecteditmode

   implicit none

contains

   subroutine EDITSPLINES(MODE, KEY)
      use precision, only: dp
      use m_cir
      use m_choices
      use unstruc_colors
      use M_SPLINES
      use unstruc_display, only: plotSplines
      use m_helpnow
      use m_drawthis
      use m_ktext
      use m_putget_un
      use m_okay
      use m_botlin
      use m_dispnode2
      use m_draw_nu
      use m_set_col
      use m_movabs

      integer, intent(inout) :: mode, key
      integer :: newmode

!      use rgfblock
!
      integer :: IIJ
      integer :: ja, num, numb, ncol, nwhat, nput
      real(kind=dp) :: xp, yp

      WRDKEY = 'EDIT SPLINES'
      NLEVEL = 2
      JA = 0
      NUM = 0
      NWHAT = 0
      NPUT = -1
      NUMB = 9
      NCOL = NCOLSP
      NDRAW(15) = 1

      MP = 0
      NP = 0
      call BOTLIN(0, NUMB, KEY)
!!     TEST
      call saveSplines()

10    continue
      call DRAWNU(KEY)
      call KTEXT(' Edit Splines       ', 1, 2, 15)
      !CALL KTEXT(' Click Spline Points',1,3,15)
      call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

      if (NUM /= 0) then
!        ER IS EEN KEUZE
         if (NUM == 4) then
            MODE = NWHAT
            return
         else
            if (NUM == 5 .and. NWHAT == 2) then
               mp = 0
               np = 0
            end if
            call CHOICES(NUM, NWHAT, KEY)
         end if
      else if (KEY >= 577) then ! Alt+letter switches edit mode.
         call selecteditmode(newmode, key)
         if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
         end if
      else if (KEY == 21) then
!        INS KEY
         if (NPUT == 0 .or. NPUT == -2 .or. NPUT == -3 .or. NPUT == -4 .or. NPUT == -5 .or. NPUT == -6) then
!           kijken welk punt bij deleten en bij oppakken
            call isSplinePoint(XP, YP, RCIR, MP, NP)
         end if
         if (NPUT == 0 .and. MP /= 0) then
!           punt oppakken
            call MOVABS(XP, YP)
            call SETCOL(0)
            call CIR(RCIR)
            call IGRFILLPATTERN(0, 0, 0)
            call SETCOL(NCOL)
            call CIR(RCIR)
            call IGRFILLPATTERN(4, 0, 0)
            NPUT = 1
         else if (NPUT == 1 .and. MP /= 0) then
!           punt neerzetten
            call saveSplines()
            call plotSplines(MP, MP, 0)
            call setSplinePoint(MP, NP, XP, YP)
            call plotSplines(MP, MP, NCOL)
            NPUT = 0
         else if (NPUT == -1) then
!           punt toevoegen
            call saveSplines()
            call plotSplines(MP, MP, 0)
            call insertSplinePoint(mp, np, xp, yp)
            call plotSplines(MP, MP, NCOL)
         else if (NPUT == -2 .and. MP /= 0) then
!           punt deleten
            call saveSplines()
            IIJ = 68
            call SETCOL(0)
            call MOVABS(XP, YP)
            if (MP == 1) then
               call CIR(1.4 * RCIR)
            else
               call CIR(RCIR)
            end if
            call plotSplines(MP, MP, 0)
            call delSplinePoint(mp, np)
            call plotSplines(MP, MP, NCOL)
         else if (NPUT == -3 .and. MP /= 0) then
!           hele spline deleten
            call saveSplines()
            IIJ = 68
            call SETCOL(0)
            call MOVABS(XP, YP)
            if (MP == 1) then
               call CIR(1.4 * RCIR)
            else
               call CIR(RCIR)
            end if
            call plotSplines(MP, MP, 0)
            call delSpline(mp)
            call plotSplines(MP, MP, NCOL)
         else if ((NPUT == -4 .or. NPUT == -5) .and. MP /= 0) then
!           move or copy whole spline, get spline
            call savesplines()
            call setcol(0)
            call movabs(xp, yp)
            if (mp == 1) then
               call cir(1.4 * rcir)
            else
               call cir(rcir)
            end if
            NPUT = 10 * NPUT - 1 ! -41 .or. -51
         else if (NPUT == -41 .and. MP /= 0) then
!           move whole spline, put spline
            call plotSplines(MP, MP, 0)
            call movespline(mp, np, xp, yp)
            call plotSplines(MP, MP, NCOL)
            NPUT = -4
         else if (NPUT == -51 .and. MP /= 0) then
!           copy whole spline, put spline
            call plotSplines(MP, MP, NCOL) ! plot original spline
            call copyspline(mp, np, xp, yp)
            call plotSplines(MP, MP, NCOL)
            NPUT = -5
         else if (NPUT == -6 .and. MP /= 0) then
!           snap spline to landboundary
            call plotSplines(MP, MP, 0)
            call snap_spline(MP)
            call plotSplines(MP, MP, NCOL)
            MP = 0
         end if
         call dispnode2(mp, np)
      else if (KEY == 22) then
!        ENTER KEY
         if (NPUT == -1 .and. NP >= 2) then
            MP = 0
            NP = 0
         end if
      else if (KEY == 23) then
!        ESC
         call restoreSplines()
         KEY = 3
      else if (KEY == 73 .or. KEY == 73 + 32) then
         if (NPUT /= 1) then
!           kijken welk punt dit is t.b.v insert mode (I)
            call isSplinePoint(XP, YP, RCIR, MP, NP)
            if (mp /= 0 .and. np /= 0) then
               ! Point was found, now highlight it temporarily on screen.
               call MOVABS(XP, YP)
               call SETCOL(0)
               call CIR(RCIR)
               call IGRFILLPATTERN(0, 0, 0)
               call SETCOL(NCOL)
               call CIR(RCIR)
               call IGRFILLPATTERN(4, 0, 0)
            end if
         end if
         NPUT = -1
      else if (KEY == 8) then ! Backspace KEY
!        delete all splines (within polygon if any) and stay in previous mode.
         call saveSplines()
         call deleteSelectedSplines()
         key = 3
      else if (KEY == 68 .or. KEY == 68 + 32) then
!        delete mode losse punten (D)
         NPUT = -2
      else if (KEY == 88 .or. KEY == 88 + 32) then
!        delete mode hele splines (X)
         NPUT = -3
      else if (KEY == 77 .or. KEY == 77 + 32) then
!        move whole spline (M)
         NPUT = -4
      else if (KEY == 67 .or. KEY == 67 + 32) then
!        copy whole spline (C)
         NPUT = -5
      else if (KEY == 76 .or. KEY == 76 + 32) then
!        snap whole spline to land (L)
         NPUT = -6
      else if (KEY == 82 .or. KEY == 82 + 32 .and. NPUT /= 1) then
!        replace mode, maar niet bij zetten (R)
         NPUT = 0
      else if (KEY == 98) then
!        b RINGS BELL
         call KTEXT(' B Rings Bell', 2, 6, 11)
         call OKAY(0)
      end if
!
      goto 10
!
   end subroutine editSplines

end module m_editsplines
