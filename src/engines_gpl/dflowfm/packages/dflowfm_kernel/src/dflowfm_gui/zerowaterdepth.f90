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

module m_zerowaterdepth

   implicit none

contains

   !subroutine wricir()
   !use m_sferic
   !use m_flow
   !use m_flowgeom
   !implicit none
   !integer :: mout, k
   !real(kind=dp) :: phi, r0
   !
   !return
   !call inisferic()
   !call newfil(mout,'circ250.ldb')
   !write(mout,'(a)') 'L001'
   !write(mout,'(a)') '360 2'
   !r0 = 125000d0
   !do k = 0,360
   !   phi = dg2rd*k
   !   write(mout,*) r0*cos(phi), r0*sin(phi), r0, r0
   !enddo
   !call doclose(mout)
   !
   !end subroutine wricir
   subroutine zerowaterdepth() ! restart without water
      use m_flow
      use m_flowgeom
      implicit none
      s0 = bl
      s1 = bl
      u0 = 0d0
      u1 = 0d0
   end subroutine zerowaterdepth

end module m_zerowaterdepth
