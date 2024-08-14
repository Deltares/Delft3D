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
module m_lateral
   use precision_basics, only: dp
   implicit none
   private

   public reset_lateral
   public default_lateral
   public initialize_lateraldata
   public dealloc_lateraldata
   public average_concentrations_for_laterals
   public add_lateral_load_and_sink
   public get_lateral_discharge
   public get_lateral_volume_per_layer
   public reset_outgoing_lat_concentration
   public finish_outgoing_lat_concentration
   public distribute_lateral_discharge
   !!
   !! Laterals
   !!
   integer, parameter, public :: ILATTP_ALL = 0 !< Type code for laterals that apply to both 2D and 1D nodes.
   integer, parameter, public :: ILATTP_1D = 1 !< Type code for laterals that only apply to 1D nodes.
   integer, parameter, public :: ILATTP_2D = 2 !< Type code for laterals that only apply to 2D nodes.

   integer, target, public :: numlatsg !< [-] nr of lateral discharge providers  {"rank": 0}
   integer, public :: num_layers !< first dimension of qplat and qqlat array, 1 for 2D, kmx for 3D. 
   ! I see 3 occurences where QPLAT is being allocated. Investigate if this is necessary.
   ! QPLAT is allocated 1) in fm_external_forcings_init_old, module subroutine init_new (will be removed)
   !                    2) in fm_external_forcings_init, module subroutine init_new
   !                    3) in test_laterals (unit test)
   ! so yes, 3 is necessary for now.
   real(kind=dp), allocatable, target, public :: qplat(:, :) !< [m3/s] Lateral discharge of provider {"shape": ["num_layers", "numlatsg"]}
   real(kind=dp), allocatable, target, public :: qqlat(:, :, :) !< [m3/s] Lateral discharge at xz,yz {"location": "face", "shape": ["num_layers","numlatsg", "ndx"]}
   real(kind=dp), allocatable, target, public :: balat(:) !< [m2] total area of all cells in provider numlatsg {"shape": ["numlatsg"]}
   character(len=128), allocatable, public :: lat_ids(:) !< id of laterals {"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qplatCum(:) !< [m3/s] Cumulative lateral discharge of provider {"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qplatCumPre(:) !< [m3/s] Cumulative lateral discharge of provider at previous history output time{"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qplatAve(:) !< [m3/s] Average lateral discharge of provider during the past history output interal {"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qLatReal(:) !< [m3/s] Realized lateral discharge {"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qLatRealCum(:) !< [m3/s] Cumulative realized lateral discharge {"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qLatRealCumPre(:) !< [m3/s] Cumulative realized lateral discharge at previous history output time{"shape": ["numlatsg"]}
   real(kind=dp), allocatable, target, public :: qLatRealAve(:) !< [m3/s] Average realized lateral discharge during the past history output interal{"shape": ["numlatsg"]}

   !! Lateral lookup tables: n1/n2latsg(ilat) = n1/n2, nnlat(n1:n2) = { flow node nrs affected by lateral ilat }
   integer, public :: nlatnd !< lateral nodes dimension, counter of nnlat(:)
   integer, allocatable, target, public :: n1latsg(:) !< [-] first  nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
   integer, allocatable, target, public :: n2latsg(:) !< [-] second nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
   integer, allocatable, target, public :: nnlat(:) !< [-] for each lateral node, flow node number == pointer to qplat/balat {"shape": ["nlatnd"]}
   integer, allocatable, target, public :: kclat(:) !< [-] for each cell: 0 when not accepting lateral discharge (e.g. pipe) {"location": "face", "shape": ["ndx"]}

   !! Lateral geometry variables
   integer, public :: nNodesLat !< [-] Total number of geom nodes for all laterals.
   integer, allocatable, target, public :: nodeCountLat(:) !< [-] Count of nodes per lateral.
   real(kind=dp), allocatable, target, public :: geomXLat(:) !< [m] x coordinates of laterals.
   real(kind=dp), allocatable, target, public :: geomYLat(:) !< [m] y coordinates of laterals.
   logical, public :: model_has_laterals_across_partitions = .false.

   real(kind=dp), allocatable, target, dimension(:, :, :), public :: outgoing_lat_concentration !< Average concentration per lateral discharge location.
   real(kind=dp), allocatable, target, dimension(:, :, :), public :: incoming_lat_concentration !< Concentration of the inflowing water at the lateral discharge location.
   real(kind=dp), allocatable, target, dimension(:, :), public :: lateral_volume_per_layer !< Total water volume per layer, for each lateral (kmx,numlatsg).

   integer, allocatable, target, dimension(:), public :: apply_transport !< Flag to apply transport for laterals (0 means only water and no substances are transported).
   logical, public :: apply_transport_is_used

   !> Reset the defaults for laterals
   interface default_lateral
      module subroutine default_lateral()
      end subroutine default_lateral
   end interface default_lateral

   !> Reset the counters for lateral data.
   interface reset_lateral
      module subroutine reset_lateral()
      end subroutine reset_lateral
   end interface reset_lateral

   !> allocate the arrays for laterals on 3d/BMI
   interface initialize_lateraldata
      module subroutine initialize_lateraldata(numconst)
         integer, intent(in) :: numconst !< number of constitiuents
      end subroutine initialize_lateraldata
   end interface initialize_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   interface dealloc_lateraldata
      module subroutine dealloc_lateraldata()
      end subroutine dealloc_lateraldata
   end interface dealloc_lateraldata

   !> At the start of an update, the outgoing_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In average_concentrations_for_laterals, the concentrations*timestep are aggregated in outgoing_lat_concentration.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   interface average_concentrations_for_laterals
      module subroutine average_concentrations_for_laterals(numconst, kmx, kmxn, cell_volume, constituents, dt)
         integer, intent(in) :: numconst !< Number or constituents.
         integer, intent(in) :: kmx !< Number of layers (0 means 2D computation).
         integer, dimension(:), intent(in) :: kmxn !< Maximum number of vertical cells per base node n.
         real(kind=dp), dimension(:), intent(in) :: cell_volume !< Volume of water in computational cells.
         real(kind=dp), dimension(:, :), intent(in) :: constituents !< Concentrations of constituents.
         real(kind=dp), intent(in) :: dt !< Timestep in seconds.
      end subroutine average_concentrations_for_laterals
   end interface average_concentrations_for_laterals

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   interface reset_outgoing_lat_concentration
      module subroutine reset_outgoing_lat_concentration()
      end subroutine reset_outgoing_lat_concentration
   end interface reset_outgoing_lat_concentration

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   interface finish_outgoing_lat_concentration
      module subroutine finish_outgoing_lat_concentration(time_interval)
         real(kind=dp), intent(in) :: time_interval
      end subroutine finish_outgoing_lat_concentration
   end interface finish_outgoing_lat_concentration

   !> Add lateral input contribution to the load being transported
   interface add_lateral_load_and_sink
      module subroutine add_lateral_load_and_sink(transport_load, transport_sink, discharge_in, discharge_out, cell_volume, dtol)
         real(kind=dp), dimension(:, :), intent(inout) :: transport_load !< Load being transported into domain
         real(kind=dp), dimension(:, :), intent(inout) :: transport_sink !< Load being transported out
         real(kind=dp), dimension(:, :, :), intent(in) :: discharge_in !< Lateral discharge going into domain (source)
         real(kind=dp), dimension(:, :, :), intent(in) :: discharge_out !< Lateral discharge going out (sink)
         real(kind=dp), dimension(:), intent(in) :: cell_volume !< Volume of water in computational cells [m3]
         real(kind=dp), intent(in) :: dtol !< cut off value for vol1, to prevent division by zero
      end subroutine add_lateral_load_and_sink
   end interface add_lateral_load_and_sink

   !> Calculate lateral discharges at each of the active grid cells, both source (lateral_discharge_in) and sink (lateral_discharge_out).
   interface get_lateral_discharge
      module subroutine get_lateral_discharge(lateral_discharge_in, lateral_discharge_out, cell_volume)
         real(kind=dp), dimension(:, :, :), intent(inout) :: lateral_discharge_in !< Lateral discharge flowing into the model (source)
         real(kind=dp), dimension(:, :, :), intent(inout) :: lateral_discharge_out !< Lateral discharge extracted out of the model (sink)
         real(kind=dp), dimension(:), intent(in) :: cell_volume !< cell_volume !< Volume of water in computational cells [m3]
      end subroutine get_lateral_discharge
   end interface get_lateral_discharge

   !> Compute water volume per layer in each lateral
   interface get_lateral_volume_per_layer
      module subroutine get_lateral_volume_per_layer(lateral_volume_per_layer)
         real(kind=dp), dimension(:, :), intent(out) :: lateral_volume_per_layer !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
      end subroutine get_lateral_volume_per_layer
   end interface get_lateral_volume_per_layer

   !> Distributes provided lateral discharge across flow nodes. 
   !> Input is lateral discharge per layer per lateral, output is per layer per lateral per cell.
   interface distribute_lateral_discharge
      module subroutine distribute_lateral_discharge(provided_lateral_discharge, lateral_discharge_per_layer_lateral_cell)
         real(kind=dp), dimension(:, :), intent(in) :: provided_lateral_discharge !< Provided lateral discharge per
                                                                                  !! layer, retrieved from BMI
         real(kind=dp), dimension(:, :, :), intent(out) :: lateral_discharge_per_layer_lateral_cell !< Real lateral discharge
                                                                                                    !! per layer per lateral per cell
      end subroutine distribute_lateral_discharge
   end interface distribute_lateral_discharge
end module m_lateral

