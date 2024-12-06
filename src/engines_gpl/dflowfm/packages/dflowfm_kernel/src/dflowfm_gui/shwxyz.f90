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

module m_shwxyz
   use m_tekgpt

   implicit none

contains

   ! NOTE: japes is disabled [AvD]
   subroutine SHWXYZ(X, Y, RD1, mmax, nmax, MC, NC, JAPERS, KEY, M, N)
      use precision, only: dp
      use m_setxor
      use m_orglocator
      use m_dispos2
      use m_disdep
      use m_cir
      use m_missing
      use unstruc_colors
      use m_locatora
      use m_helpnow
      use m_set_col
      use m_movabs
      use m_help

      implicit none

      integer :: mmax, nmax, mc, nc, japers, key, m, n
      real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX), RD1(MMAX, NMAX)
      character(len=40) :: OLDKEY

      integer :: jadraw, jonce, jplus, nlevo
      real(kind=dp) :: XL, YL, RDOL, FAC

      if (MC == 0) return
      OLDKEY = WRDKEY
      NLEVO = NLEVEL
      WRDKEY = 'TAB = DCURSOR;'
      call IMOUSECURSORHIDE()
      call SETXOR(1)
      JADRAW = 1
      JONCE = 0
      JPLUS = 0

      if (JAPERS == 1) then
         XL = (X1 + X2) / 2
         YL = (Y1 + Y2) / 2
      else
         XL = XLC
         YL = YLC
      end if

      call CLOSPT(X, Y, mmax, nmax, MC, NC, &
                  XL, YL, M, N)
      RDOL = RD1(M, N)

20    continue

      call DISPOS2(X(M, N), Y(M, N))
      call DISDEP(M, N, RD1(M, N))
      if (JADRAW == 1) then
         call TEKGPT(X, Y, mmax, nmax, MMAX, NMAX, &
                     M, N, NCOLTX, RD1)
         JADRAW = 0
      end if
      call INKEYEVENT(KEY)
      if (KEY /= 27) JONCE = 0
      if (KEY /= 45 .and. KEY /= 160 .and. &
          KEY /= 43 .and. KEY /= 162) JPLUS = 0

      call DISPOS2(X(M, N), Y(M, N))
      call DISDEP(M, N, RD1(M, N))
      call TEKGPT(X, Y, mmax, nmax, MMAX, NMAX, &
                  M, N, NCOLTX, RD1)
      JADRAW = 1
      if (KEY == 131) then
         M = max(1, M - 1)
         RDOL = RD1(M, N)
      else if (KEY == 130) then
         M = min(MC, M + 1)
         RDOL = RD1(M, N)
      else if (KEY == 128) then
         N = min(NC, N + 1)
         RDOL = RD1(M, N)
      else if (KEY == 129) then
         N = max(1, N - 1)
         RDOL = RD1(M, N)
      else if (KEY == 171) then
         call HELP(WRDKEY, 3)
      else if (KEY == 45 .or. KEY == 160) then
         if (X(M, N) /= XYMIS) then
            if (JPLUS /= -1) FAC = 1.0
            if (RD1(M, N) == DMISS) RD1(M, N) = 6.9
            RD1(M, N) = RD1(M, N) - .01 * FAC
            FAC = FAC * 1.05
            JPLUS = -1
         end if
      else if (KEY == 43 .or. KEY == 162) then
         if (X(M, N) /= XYMIS) then
            if (JPLUS /= 1) FAC = 1.0
            if (RD1(M, N) == DMISS) RD1(M, N) = 6.9
            RD1(M, N) = RD1(M, N) + .01 * FAC
            FAC = FAC * 1.05
            JPLUS = 1
         end if
      else if (KEY == 68 .or. KEY == 68 + 32) then
         RD1(M, N) = DMISS
         call SETCOL(0)
         call MOVABS(X(M, N), Y(M, N))
         call CIR(RCIR)
         call DISDEP(M, N, RD1(M, N))
      else if (KEY == 27) then
         JONCE = JONCE + 1
         if (JONCE >= 2) then
            call ORGLOCATOR(X(M, N), Y(M, N))
            call IMOUSECURSORSHOW()
            call SETXOR(0)
            NLEVEL = NLEVO
            WRDKEY = OLDKEY
            return
         end if
         RD1(M, N) = RDOL
         call DISDEP(M, N, RD1(M, N))
      else
         call ORGLOCATOR(X(M, N), Y(M, N))
         call IMOUSECURSORSHOW()
         call SETXOR(0)
         NLEVEL = NLEVO
         WRDKEY = OLDKEY
         return
      end if
      goto 20
   end subroutine shwxyz

end module m_shwxyz
