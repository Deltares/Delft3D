!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.!
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

module m_nearfield_data
   use iso_c_binding, only: c_bool
   use precision_basics, only: dp

   implicit none(type, external)

   public

   ! constants
   integer, parameter :: NEARFIELD_DISABLED = 0 !< If nearfield_mode is NEARFIELD_DISABLED (default), then nearfield/COSUMO is disabled
   integer, parameter :: NEARFIELD_ENABLED = 1 !< After call addNearfieldData, nearfield_mode is set to NEARFIELD_ENABLED. This ensures:
   !< - addNearfieldData does not need to be called again, until the data is updated and passes the pointers again.
   !< - that subroutine setNFEntrainmentMomentum can be called if flag NearFieldEntrainmentMomentum is switched on
   integer, parameter :: NEARFIELD_UPDATED = 2 !< nearfield_mode is set to NEARFIELD_UPDATED, everytime DIMR passes a data pointer
   !< from cosumo_bmi to D-Flow FM. This is the trigger to call addNearfieldData in
   !< set_external_forcings.
   integer, parameter :: NF_IX = 1 !< Column 1 in COSUMO data          : x-coordinate
   integer, parameter :: NF_IY = 2 !< Column 2 in COSUMO data          : y-coordinate
   integer, parameter :: NF_IZ = 3 !< Column 3 in COSUMO data          : z-coordinate
   integer, parameter :: NF_IS = 4 !< Column 4 in COSUMO data          : S-factor: if S changes along the sink traject, then entrainment appears
   integer, parameter :: NF_IH = 5 !< Column 5 in COSUMO data          : half the plume height
   integer, parameter :: NF_IW = 6 !< Column 6 in COSUMO data          : half the plume width
   integer, parameter :: NF_IUMAG = 7 !< Column 7 in COSUMO data, OPTIONAL: momentum magnitude
   integer, parameter :: NF_IUDIR = 8 !< Column 8 in COSUMO data, OPTIONAL: momentum direction: 0: to North, 90: to East
   integer, parameter :: NUM_TRACK = 1000 !< If there is only one source point, a track is created perpendicular to "last sink -> source",
   !< using NF_IW. NUM_TRACK points are defined on this track to detect all cells containing source points.
   integer, parameter :: CONST_OPERATOR_UNDEFINED = 0
   integer, parameter :: CONST_OPERATOR_EXCESS = 1
   integer, parameter :: CONST_OPERATOR_ABSOLUTE = 2
   
   ! integers
   integer :: nearfield_mode !< Switch to enable/disable COSUMO
   integer :: nf_num_dif !< Number of diffusers     as obtained from COSUMO_BMI
   integer :: nf_numconst !< Number of constituents  as obtained from COSUMO_BMI
   integer :: nf_numintake !< Number of intake points as obtained from COSUMO_BMI
   integer :: nf_numsour !< Number of source points as obtained from COSUMO_BMI
   integer :: nf_numsink !< Number of sink points   as obtained from COSUMO_BMI
   integer :: nf_namlen !< Length of character strings in nf_const_operator as obtained from COSUMO_BMI
   integer :: nf_sour_track_max !< Maximum (over all diffusers) of all source track flow cells
   integer :: nf_intake_cnt_max !< Maximum (over all diffusers) of all number of intake points
   integer :: nf_entr_max !< Maximum (over all diffusers) of all entrainment points (coupled sink source points)
   
   ! Pointers to data inside COSUMO_BMI, allocated in COSUMO_BMI
   real(kind=dp), dimension(:), pointer :: nf_q_source !< Qsource
   real(kind=dp), dimension(:), pointer :: nf_q_intake !< Qintake
   real(kind=dp), dimension(:, :), pointer :: nf_const !< Constituent values
   !< DIM 1: diffuser
   !< DIM 2: constituent
   real(kind=dp), dimension(:, :, :), pointer :: nf_intake !< Intake
   !< DIM 1: diffuser
   !< DIM 2: intake point id
   !< DIM 3: X, Y, Z
   real(kind=dp), dimension(:, :, :), pointer :: nf_sink !< Sinks
   !< DIM 1: diffuser
   !< DIM 2: sink point id
   !< DIM 3: X, Y, Z, S, H, B
   real(kind=dp), dimension(:, :, :), pointer :: nf_sour !< Sources
   !< DIM 1: diffuser
   !< DIM 2: source point id
   !< DIM 3: X, Y, Z, S, H, B, Umag, Udir
   character(:), dimension(:), pointer :: nf_const_operator !< Constituent operator
   logical(kind=c_bool), dimension(:), pointer :: nf_src_mom !< true: Umag and Udir in nf_sour are filled
   
   ! NearField arrays on FM domain, allocated in FM
   integer, dimension(:, :), allocatable :: nf_sink_n !<    Flow cell index of sink   points of all diffusers
   integer, dimension(:, :), allocatable :: nf_sour_n !<    Flow cell index of source points of all diffusers
   integer, dimension(:, :), allocatable :: nf_intake_n !<    Flow cell index of intake points of all diffusers
   integer, dimension(:, :), allocatable :: nf_intake_nk !< 3D index of nf_intake_n
   integer, dimension(:), allocatable :: nf_numintake_idif !< nf_numintake per diffuser
   integer, dimension(:), allocatable :: nf_entr_start !< Start index in src arrays of entrainment points (coupled sink source)        of all diffusers
   integer, dimension(:), allocatable :: nf_entr_end !< End   index in src arrays of entrainment points (coupled sink source)        of all diffusers
   integer, dimension(:, :), allocatable :: nf_sinkid !< 3D index of nf_sink_n for each src point (from nf_entr_start to nf_entr_end) of all diffusers
   real(kind=dp), dimension(:, :), allocatable :: nf_sour_wght !< Fraction * 1000.0       of each source point of all diffusers
   real(kind=dp), dimension(:), allocatable :: nf_sour_wght_sum !< Sum of all source weights for each diffuser
   real(kind=dp), dimension(:, :), allocatable :: nf_intake_wght !< Fraction * nf_numintake of each intake point of all diffusers
   real(kind=dp), dimension(:, :), allocatable :: nf_intake_z !< Z coordinate            of each intake point of all diffusers

   type :: intake_location_t
      integer :: index_2d
      integer :: index_3d
      real(kind=dp) :: z_coordinate
      integer :: weight_count
   end type intake_location_t

end module m_nearfield_data
