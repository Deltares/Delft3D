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

module m_statistics
   implicit none
   double precision :: avedif !< for now only, cum dif with analytic sol
   double precision :: sqadif !< for now only, cum dif with analytic sol
   double precision :: rmsdif !< for now only, cum dif with analytic sol
   double precision :: dmxdif !< for now only, cum dif with analytic sol
   integer :: numdif

   double precision :: cumavedif !< for now only, cum dif with analytic sol
   double precision :: cumrmsdif !< for now only, cum dif with analytic sol
   double precision :: cumdmxdif !< for now only, cum dif with analytic sol
   integer :: numcum, npdf
   double precision, allocatable :: xpdf(:), ypdf(:)
contains
   subroutine reset_statistics()
      avedif = 0d0 ! for now only, cum dif with analytic sol
      sqadif = 0d0
      rmsdif = 0d0 ! for now only, cum dif with analytic sol
      dmxdif = 0d0 ! for now only, cum dif with analytic sol
      numdif = 0

      cumavedif = 0d0 ! for now only, cum dif with analytic sol
      cumrmsdif = 0d0 ! for now only, cum dif with analytic sol
      cumdmxdif = 0d0 ! for now only, cum dif with analytic sol
      numcum = 0
      npdf = 0
   end subroutine reset_statistics
end module m_statistics
