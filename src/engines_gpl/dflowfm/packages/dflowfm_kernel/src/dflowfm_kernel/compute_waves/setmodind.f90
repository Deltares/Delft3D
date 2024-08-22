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

   subroutine setmodind(rouwav, modind)
      implicit none
      integer, intent(out) :: modind
      character(len=4), intent(in) :: rouwav

      modind = 0 ! safety
      if (rouwav == 'FR84') then
         modind = 1
      elseif (rouwav == 'MS90') then
         modind = 2
      elseif (rouwav == 'HT91') then
         modind = 3
      elseif (rouwav == 'GM79') then
         modind = 4
      elseif (rouwav == 'DS88') then
         modind = 5
      elseif (rouwav == 'BK67') then
         modind = 6
      elseif (rouwav == 'CJ85') then
         modind = 7
      elseif (rouwav == 'OY88') then
         modind = 8
      elseif (rouwav == 'VR04') then
         modind = 9
      elseif (rouwav == 'RU03') then
         modind = 10
      end if
   end subroutine setmodind
