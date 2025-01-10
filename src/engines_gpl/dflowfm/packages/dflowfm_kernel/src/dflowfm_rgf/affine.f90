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

module m_affine

   implicit none

   private

   public :: affine

contains

   subroutine AFFINE(XX, YY, XG, YG, INI)
      use precision, only: dp
      use M_BITMAP
      use string_module, only: find_first_letter
      use m_qnerror
      use m_bilinxy
      use m_filez, only: oldfil, doclose, numbersonline

      integer :: ini
      logical :: jawel
      integer :: k
      integer :: minp
      real(kind=dp) :: xg4
      real(kind=dp) :: xx4
      real(kind=dp) :: yg4
      real(kind=dp) :: yy4
      character REC * 132
      real(kind=dp) :: XX, YY, XG, YG
      XX4 = XX; YY4 = YY

      if (INI == 1) then

         inquire (FILE='AFFINE'//'.xyx', EXIST=JAWEL)

         if (JAWEL) then
            call OLDFIL(MINP, 'AFFINE'//'.xyx')
            read (MINP, '(A)') REC
            if (find_first_letter(REC) == 1) then
               read (MINP, '(A)') REC
               do K = 1, 4
                  read (MINP, '(A)') REC
                  if (NUMBERSONLINE(REC) == 2) then
                     read (REC, *) XP(K), YP(K)
                  else if (NUMBERSONLINE(REC) == 4) then
                     read (REC, *) XP(K), YP(K), XB(K), YB(K)

                  end if
               end do
            else
               call QNERROR('Cannot Read AFFINE.XYX File', ' ', ' ')
            end if
            call DOCLOSE(MINP)
            call BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
            INI = 0
         else
            call QNERROR('NO AFFINE.XYX FILE FOUND', ' ', ' ')
         end if
      end if

      call BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
      XG = XG4
      YG = YG4

      return
   end

end module m_affine
