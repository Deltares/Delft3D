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
   use m_hydrology_data
   integer :: jagrw !< include ground water
   double precision, allocatable :: sgrw0(:) !< ground water level start
   double precision, allocatable :: sgrw1(:) !< ground water level end of timestep
   double precision, allocatable :: pgrw(:) !< pressure and plotting of sgrw
   double precision, allocatable :: h_unsat(:) !< initial height unsaturated zone
   double precision, allocatable :: bgrw(:) !< initial height unsaturated zone

   !  TODO: UNST-3763: Use named parameter constant for jaintercept2D
   integer :: jaintercept2D !< 1 = uniform, 2 = spatially variable
   double precision :: Hinterceptionlayer !< (DEPRECATED) thickness of interception layer in  (m) only if infiltrationmodel == 1
   double precision :: Conductivity !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
   double precision :: Unsatfac !< reduction factor for conductivity in unsaturated zone

   double precision :: h_aquiferuni !< uniform height of carrying layer
   double precision :: h_unsatini !< initial level groundwater is bedlevel - h_unsatini
   double precision :: sgrwini !< initial level groundwater. If specified, h_unsatini wiil not be used
   double precision :: bgrwuni !< initial level groundwater. If specified, h_unsatini wiil not be used
   double precision :: h_capillair !< Capillary rising height (m)
   double precision :: h_transfer !< uniform thickness (numerical) transfer zone grw <-> openw

   double precision :: porosgrw !< porosity of soil = Vair / (Vsoil+Vair)  , or,
   !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
   !< e.g.
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_grw() instead.
   subroutine default_grw()
      jagrw = 0 !< include ground water
      jaintercept2D = 0 !< 1 = uniform, 2 = spatially variable
      !Hinterceptionlayer          !< thickness of interception layer in  (m) only if infiltrationmodel == 1
      Conductivity = 0d-4 !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
      Unsatfac = 1.0d0 !< reduction factor for conductivity in unsaturated zone

      h_aquiferuni = 20d0 !< uniform height of carrying layer
      h_unsatini = 0.2 !< initial level groundwater is bedlevel - h_unsatini
      sgrwini = -999d0 !< initial level groundwater. If specified, h_unsatini wiil not be used
      bgrwuni = -999d0 !< initial level groundwater. If specified, h_unsatini wiil not be used
      h_capillair = 0.5 !< Capillary rising height (m)
      h_transfer = 0.1d0 !< uniform thickness (numerical) transfer zone grw <-> openw

      porosgrw = 0.25d0 !< porosity of soil = Vair / (Vsoil+Vair)  , or,
      !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
      ! Remaining of variables is handled in reset_grw()
      call reset_grw()
   end subroutine default_grw

!> Resets only groundwater variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_grw() instead.
   subroutine reset_grw()
   end subroutine reset_grw

end module m_grw
