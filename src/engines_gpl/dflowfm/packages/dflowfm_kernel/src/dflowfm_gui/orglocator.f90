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

      subroutine ORGLOCATOR(XL, YL)
         use m_devices
         implicit none
         integer :: jashow
         integer :: jmouse
         integer :: ml
         integer :: nl
         double precision :: xa
         double precision :: xl
         double precision :: xlc
         double precision :: ya
         double precision :: yl
         double precision :: ylc
!     INITIATE CURSOR LOCATION
         common / LOCATORA / XLC, YLC, XA, YA, JMOUSE, JASHOW

         if (XL == 0 .and. YL == 0) then
            ML = NPX / 2
            NL = NPY / 2
            call TOWOR(ML, NL, XLC, YLC)
         else
            XLC = XL
            YLC = YL
         end if

         call IMOUSECURSORXYG(real(XLC), real(YLC))
         return
      end
