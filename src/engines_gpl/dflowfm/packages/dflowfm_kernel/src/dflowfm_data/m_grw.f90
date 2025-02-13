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

module m_grw
   use precision, only: dp
   use m_hydrology_data
   integer :: jagrw !< include ground water
   real(kind=dp), allocatable :: sgrw0(:) !< ground water level start
   real(kind=dp), allocatable :: sgrw1(:) !< ground water level end of timestep
   real(kind=dp), allocatable :: pgrw(:) !< pressure and plotting of sgrw
   real(kind=dp), allocatable, target :: h_unsat(:) !< initial height unsaturated zone
   real(kind=dp), allocatable :: bgrw(:) !< initial height unsaturated zone

   !  TODO: UNST-3763: Use named parameter constant for jaintercept2D
   integer :: jaintercept2D !< 1 = uniform, 2 = spatially variable
   real(kind=dp) :: Hinterceptionlayer !< (DEPRECATED) thickness of interception layer in  (m) only if infiltrationmodel == 1
   real(kind=dp) :: Conductivity !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
   real(kind=dp) :: Unsatfac !< reduction factor for conductivity in unsaturated zone

   real(kind=dp) :: h_aquiferuni !< uniform height of carrying layer
   real(kind=dp) :: h_unsatini !< initial level groundwater is bedlevel - h_unsatini
   real(kind=dp) :: sgrwini !< initial level groundwater. If specified, h_unsatini wiil not be used
   real(kind=dp) :: bgrwuni !< initial level groundwater. If specified, h_unsatini wiil not be used
   real(kind=dp) :: h_capillair !< Capillary rising height (m)
   real(kind=dp) :: h_transfer !< uniform thickness (numerical) transfer zone grw <-> openw

   real(kind=dp) :: porosgrw !< porosity of soil = Vair / (Vsoil+Vair)  , or,
   !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
   !< e.g.
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_grw() instead.
   subroutine default_grw()
      jagrw = 0 !< include ground water
      jaintercept2D = 0 !< 1 = uniform, 2 = spatially variable
      !Hinterceptionlayer          !< thickness of interception layer in  (m) only if infiltrationmodel == 1
      Conductivity = 0.0_dp !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
      Unsatfac = 1.0_dp !< reduction factor for conductivity in unsaturated zone

      h_aquiferuni = 20.0_dp !< uniform height of carrying layer
      h_unsatini = 0.2_dp !< initial level groundwater is bedlevel - h_unsatini
      sgrwini = -999.0_dp !< initial level groundwater. If specified, h_unsatini wiil not be used
      bgrwuni = -999.0_dp !< initial level groundwater. If specified, h_unsatini wiil not be used
      h_capillair = 0.5_dp !< Capillary rising height (m)
      h_transfer = 0.1_dp !< uniform thickness (numerical) transfer zone grw <-> openw

      porosgrw = 0.25_dp !< porosity of soil = Vair / (Vsoil+Vair)  , or,
      !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
      ! Remaining of variables is handled in reset_grw()
      call reset_grw()
   end subroutine default_grw

!> Resets only groundwater variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_grw() instead.
   subroutine reset_grw()
   end subroutine reset_grw

end module m_grw
