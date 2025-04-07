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

submodule(m_setcoltabfile) m_setcoltabfile_

   implicit none

contains

   module subroutine SETCOLTABFILE(FILNAM, JASECOND)
      use m_sysorlocalfil, only: sysorlocalfil
      use m_qnmessage
      use m_filemenu
      use unstruc_colors
      use m_depmax
      use m_depmax2
      use m_filez, only: doclose

      implicit none
      integer :: ierror
      integer :: iblue
      integer :: igreen
      integer :: ihue
      integer :: ired
      integer :: isat
      integer :: jahls
      integer :: jasecond
      integer :: k
      integer :: light
      integer :: minp
      integer, parameter :: mxq = 1, mxclass = 1
      character(len=*), intent(in) :: filnam
      character(:), allocatable :: FOLNAM

      FOLNAM = FILNAM
      if (FILNAM(1:5) == '*.hls') then
         MINP = 0
         call FILEMENU(MINP, FOLNAM, ierror)
      else
         k = len_trim(filnam)
         folnam(1:k) = filnam(1:k)
         call SYSORLOCALFIL(MINP, FOLNAM, 0)
      end if
      if (MINP /= 0) then
         if (index(FOLNAM, 'HLS') >= 1 .or. index(FOLNAM, 'hls') >= 1) then
            JAHLS = 1
         else if (index(FOLNAM, 'RGB') >= 1 .or. index(FOLNAM, 'rgb') >= 1) then
            JAHLS = 2
         else
            call QNMESSAGE('CHOOSE *.hls OR *.rgb FILE')
            return
         end if
         if (JASECOND == 0) then
            coltabfile = folnam
         else
            coltabfile2 = folnam
         end if

         K = 1
         read (MINP, *, end=999, ERR=888)
20       continue
         if (JAHLS == 1) then
            read (MINP, *, end=999, ERR=888) IHUE, LIGHT, ISAT
            IHUE = max(0, min(IHUE, 360))
            LIGHT = max(0, min(LIGHT, 100))
            ISAT = max(0, min(ISAT, 100))
            if (JASECOND == 0) then
               call IGRPALETTEHLS(NCOLS(K), IHUE, LIGHT, ISAT)
            else
               call IGRPALETTEHLS(NCOLS2(K), IHUE, LIGHT, ISAT)
            end if
         else if (JAHLS == 2) then
            read (MINP, *, end=999, ERR=888) IRED, IGREEN, IBLUE
            IRED = max(0, min(IRED, 255))
            IGREEN = max(0, min(IGREEN, 255))
            IBLUE = max(0, min(IBLUE, 255))
            if (JASECOND == 0) then
               call IGRPALETTERGB(NCOLS(K), IRED, IGREEN, IBLUE)
            else
               call IGRPALETTERGB(NCOLS2(K), IRED, IGREEN, IBLUE)
            end if
         end if
         K = K + 1
         goto 20
999      continue
         call doclose(MINP)
         if (JASECOND == 0) then
            NV = max(2, K - 2)
            NIE = NIS + NV + 1
         else
            NV2 = max(2, K - 2)
            NIE2 = NIS2 + NV2 + 1
         end if
         return
888      continue ! Read error in coltabfile, back to defaults.
         call doclose(MINP)
      end if
      return
   end subroutine SETCOLTABFILE

end submodule m_setcoltabfile_
