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
module m_dispos
   implicit none
contains
   subroutine DISPOS()
      use m_devices
      use m_sferic
      use m_locatora
      use m_disfor
      use m_howtoview
      use m_ktext

      integer :: ixmax, ixmin, ixy, ndec, nxy
      character POSITI * 25

      POSITI = 'X,Y:         ,         '
      if (JVIEW == 2) then
         POSITI = 'Z,Y:         ,         '
      else if (JVIEW == 3) then
         POSITI = 'X-Z:         ,         '
      end if

      if (jsferic == 1) then ! nou ja, laat maar even staan
         IXMIN = int(log10(max(1d-6, min(abs(xlc), abs(ylc)))))
         IXMax = int(log10(max(1d-6, max(abs(xlc), abs(ylc)))))

         Ixy = abs(max(ixmin, ixmax))
         NXY = IXY + 3
         NDEC = 9 - NXY
         if (NDEC >= 1) then
            XYFORM = '(F10.1)'
            write (XYFORM(6:6), '(I1)') NDEC
         else
            disFORM = '(E10.3)'
         end if
      end if

      write (POSITI(5:14), xyform) XLC
      write (POSITI(16:25), xyform) YLC
      call KTEXT(POSITI, IWS - 24, 2, 15)

      return
   end
end module m_dispos
