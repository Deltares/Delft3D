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
module m_disdis
   implicit none
contains
      subroutine DISDIS()

         use m_devices
         use geometry_module, only: dbdistance
         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D
         use m_locatora
         use m_disfor
         use m_ktext
         
         double precision :: dis
!     -------------------------------
!     write distance
!     -------------------------------
         character DISTAN * 25

         DISTAN = 'DIS:'
         DIS = dbdistance(xa, ya, xlc, ylc, jsferic, jasfer3D, dmiss)
         write (DISTAN(6:), '(F17.5)') min(DIS, 1d9)
         call KTEXT(DISTAN, IWS - 24, 3, 15)

         !   checkdislin()

         return
      end
end module m_disdis
