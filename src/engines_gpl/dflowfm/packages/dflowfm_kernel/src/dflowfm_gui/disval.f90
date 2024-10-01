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

module m_disval

implicit none

contains

      subroutine DISVAL(M, N, DEP)
         use m_devices, only: iws
         use m_drawthis
         use m_ktext

         double precision :: dep
         integer :: m
         integer :: n
         character DISTAN * 23
         if (NDRAW(14) <= 1) then
            DISTAN = 'M:     N:              '
         else if (NDRAW(14) == 2) then
            DISTAN = 'M:    N:    ZC:        '
         else if (NDRAW(14) == 3) then
            DISTAN = 'M:    N:    RES:       '
         else if (NDRAW(14) == 4) then
            DISTAN = 'M:    N:    MSM:       '
         else if (NDRAW(14) == 5) then
            DISTAN = 'M:    N:    NSM:       '
         else if (NDRAW(14) == 6) then
            DISTAN = 'M:    N:    MCU:       '
         else if (NDRAW(14) == 7) then
            DISTAN = 'M:    N:    NCU:       '
         else if (NDRAW(14) == 8) then
            DISTAN = 'M:    N:    MSZ:       '
         else if (NDRAW(14) == 9) then
            DISTAN = 'M:    N:    NSZ:       '
         else if (NDRAW(14) == 10) then
            DISTAN = 'M:    N:    ASP:       '
         else if (NDRAW(14) == 11) then
            DISTAN = 'M:    N:               '
         else if (NDRAW(14) == 12) then
            DISTAN = 'M:    N:    DEP:       '
         else if (NDRAW(11) == 1) then
            DISTAN = 'M:    N:    CNM:       '
         else if (NDRAW(11) == 2) then
            DISTAN = 'M:    N:    CRM:       '
         else if (NDRAW(11) == 3) then
            DISTAN = 'M:    N:    CRN:       '
         end if

         if (M == 0) then
            DISTAN = 'NO POINT FOUND         '
         else
            write (DISTAN(3:6), '(I4)') M
            write (DISTAN(10:13), '(I4)') N
            if (NDRAW(14) >= 2 .and. NDRAW(14) <= 10) then
               write (DISTAN(16:23), '(F8.3)') DEP
            else if (NDRAW(14) == 11) then
               write (DISTAN(17:23), '(F7.1)') DEP
            else if (NDRAW(11) >= 1 .and. NDRAW(11) <= 3) then
               write (DISTAN(17:23), '(F7.2)') DEP
            end if
         end if

         call KTEXT(DISTAN, IWS - 22, 4, 15)

         return
      end

end module m_disval
