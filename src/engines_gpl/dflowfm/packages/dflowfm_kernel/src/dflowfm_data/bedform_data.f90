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

module m_bedform_data
   !
   ! Contains variables and parameters related to bedform predictor module
   !
   use precision

   implicit none

   public bedformpar_type

   type bedformpar_type

      character(256) :: flbdfh !< File specifying Bedform-height
      character(256) :: flnmD50 !< File specifying 50-percentile of sediment diameters
      character(256) :: flnmD90 !< File specifying 90-percentile of sediment diameters

      logical :: lfbedfrm !< Logical flag for bedform computation
      logical :: lfbedfrmout !< Logical flag for bedform output
      logical :: lfbedfrmrou !< Logical flag for bedform roughness computation
      logical :: lfbedfrmCFL !< Logical flag for CFL-violation check
      logical :: lfbedfrmADV !< Logical flag for ADVection
      logical :: lfbdfmor !< Logical flag for morphological time scale
      logical :: spatial_bedform !< sediment diameters are spatial varying (even if no sediment simulated)

      integer :: bedformheighttype !< Type number of duneheight predictor
      integer :: bedformlengthtype !< Type number of duneheight predictor
      integer :: bdfrpt !< Type number of rksd (duinruwheidshoogte) predictor
      integer :: bdfrlxtype !< Type number of length scale bedformL_H

      real(fp) :: bdfC_Hn !< Bedform migration non-linearity parameter
      real(fp) :: bdfC_Hp !< Bedform migration speed gamma power
      real(fp) :: bdfGmin !< Minimum value of gamma
      real(fp) :: bdfHmax !< Maximum water level
      real(fp) :: bdfL_Hc !< Multiplication factor
      real(fp) :: bdfL_Hp !< Length scale phi power
      real(fp) :: bdfPmax !< Maximum value of phi
      real(fp) :: bedformL_H !< Lengthscale associated with the time depedendant adjustment of dunes
      real(fp) :: bedformT_H !< Timescale associated with the time depedendant adjustment of dunes
      real(fp) :: bdfuni !< uniform height for bedform
      real(fp) :: thetacdune !< critical shear stress for dunes

      real(fp), dimension(:), pointer :: bedformD50 !< 50-percentile of sediment diameters (if no sediment simulated)
      real(fp), dimension(:), pointer :: bedformD90 !< 90-percentile of sediment diameters (if no sediment simulated)
      real(fp), dimension(:), pointer :: hdpar !< Coefficients for duneheight (/a, b, c/) on basis of power relation a*(h**b) of (epsilon, 0) in Sieben04MPM
      real(fp), dimension(:), pointer :: ldpar !< Coefficients for dunelength on basis of power relation
      real(fp), dimension(:), pointer :: cdpar !< Coefficients to calculate the bedform celerity (a * u ** b)
      real(fp), dimension(:), pointer :: kdpar !< Coefficients for rksd on basis of power relation
      real(fp), dimension(:), pointer :: duneheight !< Dune heights
      real(fp), dimension(:), pointer :: duneheightequi !< Equilibrium dune heights
      real(fp), dimension(:), pointer :: dunelength !< Dune lengths
      real(fp), dimension(:), pointer :: qbedformx !< Bedform celerity x direction
      real(fp), dimension(:), pointer :: qbedformy !< Bedform celerity y direction
      real(fp), dimension(:), pointer :: ubedform !< Bedform celerity magnitude

      real(fp), dimension(:), pointer :: rksr !< Ripple roughness height in zeta point
      real(fp), dimension(:), pointer :: rksmr !< Mega-ripple roughness height in zeta point
      real(fp), dimension(:), pointer :: rksd !< Dune roughness height in zeta point

   end type bedformpar_type

end module m_bedform_data
