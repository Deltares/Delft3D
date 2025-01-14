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

module m_heatu
   use m_qsun_nominal
   use m_heatun

   implicit none

contains

   subroutine heatu(timhr)
      use precision, only: dp
      use m_flow
      use m_flowgeom
      use m_sferic
      use m_get_kbot_ktop
      implicit none

      real(kind=dp) :: timhr

      real(kind=dp) :: qsnom
      integer :: n, kb, kt

      heatsrc0 = 0d0 ! array of heat sources zero

      if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! map output zero
         if (jatem == 3) then
            Qtotmap = 0d0
         else if (jatem == 5) then
            Qtotmap = 0d0
            Qsunmap = 0d0
            Qevamap = 0d0
            Qconmap = 0d0
            Qlongmap = 0d0
            Qfrevamap = 0d0
            Qfrconmap = 0d0
         end if
      end if

      call qsun_nominal(anglon, anglat, timhr, qsnom) ! for models not in spherical coordinates do this just once

!epshstem     = 0.001d0
!chktempdep   = 0.0d0
!Soiltempthick = 0.2d0

!$OMP PARALLEL DO   &
!$OMP PRIVATE(n,kb,kt)
      do n = 1, ndxi
         if (nd(n)%lnx == 0) cycle ! The need for this statement makes Santa Claus unhappy
         if (hs(n) < epshstem) cycle
         call heatun(n, timhr, qsnom)
         if (hs(n) < chktempdep) then
            call getkbotktop(n, kb, kt)
            heatsrc0(kb:kt) = heatsrc0(kb:kt) * hs(n) / chktempdep
         end if
      end do
!$OMP END PARALLEL DO

   end subroutine heatu

end module m_heatu
