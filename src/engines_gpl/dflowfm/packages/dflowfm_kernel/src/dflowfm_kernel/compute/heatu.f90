!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

   implicit none

contains

   !> Update the heatfluxes
   subroutine heatu(time_in_hours)
      use precision, only: dp
      use m_flow, only: qtotmap, qsunmap, qevamap, qconmap, qlongmap, qfrevamap, qfrconmap, jamapheatflux, jahisheatflux, &
                        jatem, hs, epshstem, chktempdep
      use m_flowgeom, only: ndxi, nd
      use m_sferic, only: anglon, anglat
      use m_wind, only: heatsrc0
      use m_qsun_nominal, only: calculate_nominal_solar_radiation
      use m_get_kbot_ktop, only: getkbotktop
      use m_heatun, only: heatun

      real(kind=dp), intent(in) :: time_in_hours !< Current model time in hours

      real(kind=dp) :: nominal_solar_radiation
      integer :: n, kb, kt

      heatsrc0(:) = 0.0_dp ! 2D or 3D heat source per cell, only set at timeuser (Km3/s)

      if (jamapheatflux > 0 .or. jahisheatflux > 0) then
         if (jatem == 3) then
            qtotmap(:) = 0.0_dp
         else if (jatem == 5) then
            qtotmap(:) = 0.0_dp
            qsunmap(:) = 0.0_dp
            qevamap(:) = 0.0_dp
            qconmap(:) = 0.0_dp
            qlongmap(:) = 0.0_dp
            qfrevamap(:) = 0.0_dp
            qfrconmap(:) = 0.0_dp
         end if
      end if

      nominal_solar_radiation = calculate_nominal_solar_radiation(anglon, anglat, time_in_hours) ! for models not in spherical coordinates do this just once

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE(n,kb,kt)
      do n = 1, ndxi
         if (nd(n)%lnx == 0) then
            cycle
         end if
         if (hs(n) < epshstem) then
            cycle
         end if
         call heatun(n, time_in_hours, nominal_solar_radiation)
         if (hs(n) < chktempdep) then
            call getkbotktop(n, kb, kt)
            heatsrc0(kb:kt) = heatsrc0(kb:kt) * hs(n) / chktempdep
         end if
      end do
      !$OMP END PARALLEL DO

   end subroutine heatu

end module m_heatu
