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
submodule(m_laterals) m_laterals_implementation

   implicit none

contains

   !> Reset the defaults for laterals
   module subroutine default_lateral()
      call reset_lateral()
   end subroutine default_lateral

   !> Reset the counters for lateral data.
   module subroutine reset_lateral()
      numlatsg = 0 !< [] nr of lateral discharge providers
      nlatnd = 0 !< lateral nodes dimension, counter of nnlat(:)
   end subroutine reset_lateral

   !> allocate the arrays for laterals on 3d/BMI
   module subroutine initialize_lateraldata(num_const)
      use m_flow, only: kmx
      use m_alloc, only: realloc

      integer, intent(in) :: num_const !< number of constitiuents

      integer :: i ! loop counter

      apply_transport_is_used = .false.
      if (allocated(apply_transport)) then
         do i = 1, numlatsg
            if (apply_transport(i) == 1) then
               apply_transport_is_used = .true.
               ! No need to look further
               exit
            end if
         end do
      end if

      num_layers = max(1, kmx)
      call realloc(incoming_lat_concentration, [num_layers, num_const, numlatsg])
      incoming_lat_concentration = 0._dp
      call realloc(outgoing_lat_concentration, [num_layers, num_const, numlatsg])
      call realloc(lateral_volume_per_layer, [num_layers, numlatsg])
      call realloc(qqlat, [num_layers, nlatnd], fill=0._dp)

   end subroutine initialize_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   module subroutine dealloc_lateraldata()

      if (allocated(incoming_lat_concentration)) then
         deallocate (incoming_lat_concentration)
         deallocate (outgoing_lat_concentration)
         deallocate (lateral_volume_per_layer)
         deallocate (qqlat)
      end if

   end subroutine dealloc_lateraldata

   !> At the start of an update, the outgoing_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !! In average_concentrations_for_laterals, the concentrations*timestep are aggregated in outgoing_lat_concentration.
   !! While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine average_concentrations_for_laterals(num_const, kmx, kmxn, cell_volume, constituents, dt)

      use m_alloc, only: aerr
      use m_get_kbot_ktop, only: getkbotktop

      integer, intent(in) :: num_const !< Number or constituents.
      integer, intent(in) :: kmx !< Number of layers (0 means 2D computation).
      integer, dimension(:), intent(in) :: kmxn !< Maximum number of vertical cells per base node n.
      real(kind=dp), dimension(:), intent(in) :: cell_volume !< Volume of water in computational cells [m3].
      real(kind=dp), dimension(:, :), intent(in) :: constituents !< Concentrations of constituents.
      real(kind=dp), intent(in) :: dt !< Timestep in seconds.

      integer :: i_lat, i_node, i_const, k, k1, kt, kb
      integer :: i_layer
      integer :: iostat

      real(kind=dp), allocatable, dimension(:) :: total_volume !< Placeholder of total lateral-volume per layer 
      real(kind=dp), allocatable, dimension(:, :, :) :: total_time_weighted_quantity !< Placeholder of accumulated quantity (mass and/or temperature) weighted by time, for each layer, lateral and constituent

      allocate (total_volume(num_layers), stat=iostat)
      call aerr('total_volume', iostat, num_layers, 'average_concentrations_for_laterals')
      allocate (total_time_weighted_quantity(num_layers, num_const, numlatsg), stat=iostat)
      call aerr('total_time_weighted_quantity', iostat, num_layers*num_const*numlatsg, 'average_concentrations_for_laterals')

      do i_lat = 1, numlatsg
         total_volume(:) = 0.0_dp
         total_time_weighted_quantity(:,:,:) = 0.0_dp
         do k1 = n1latsg(i_lat), n2latsg(i_lat)
            i_node = nnlat(k1)
            if (i_node > 0) then
               if (kmx < 1) then
                  total_volume = total_volume + cell_volume(i_node)
                  do i_const = 1, num_const
                     total_time_weighted_quantity(1, i_const, i_lat) = total_time_weighted_quantity(1, i_const, i_lat) + &
                                                                       dt * cell_volume(i_node) * constituents(i_const, i_node)
                  end do
               else
                  i_layer = kmx - kmxn(i_node) + 1 ! initialize i_layer to the index of first active bottom layer of base node(i_node)
                  call getkbotktop(i_node, kb, kt)
                  do k = kb, kt ! loop over active layers under base node(i_node)
                     total_volume(i_layer) = total_volume(i_layer) + cell_volume(k)
                     do i_const = 1, num_const
                        total_time_weighted_quantity(i_layer, i_const, i_lat) = total_time_weighted_quantity(i_layer, i_const, i_lat) + &
                                                                     dt * cell_volume(k) * constituents(i_const, k)
                     end do
                     i_layer = i_layer + 1
                  end do
               end if
            end if
         end do
         do i_layer = 1, num_layers
            if (total_volume(i_layer) > 0) then
               outgoing_lat_concentration(i_layer, :, i_lat) = outgoing_lat_concentration(i_layer, :, i_lat) + & 
                                                               total_time_weighted_quantity(i_layer, :, i_lat) / total_volume(i_layer)
            else
               outgoing_lat_concentration(i_layer, :, i_lat) = 0.0_dp
            end if
         end do
      end do

   end subroutine average_concentrations_for_laterals

   ! Add lateral input contribution to the load being transported
   module subroutine add_lateral_load_and_sink(transport_load, transport_sink, cell_volume, dtol)
      use m_transportdata, only: numconst
      real(kind=dp), dimension(:, :), intent(inout) :: transport_load !< Load being transported into domain. 
                                                                      !< Sign-convention: positive means load being transported into model.
      real(kind=dp), dimension(:, :), intent(inout) :: transport_sink !< Load being transported out. 
                                                                      !< Sign-convention: positive means load being transported out.
      real(kind=dp), dimension(:), intent(in) :: cell_volume !< Volume of water in computational cells [m3]
      real(kind=dp), intent(in) :: dtol !< cut off value for cell_volume, to prevent division by zero

      real(kind=dp) :: delta_cell_volume, qlat
      integer :: i_const, i_lateral, i_cell, k1, i_layer

      do i_layer = 1, num_layers
         do i_const = 1, numconst
            do i_lateral = 1, numlatsg
               do k1 = n1latsg(i_lateral), n2latsg(i_lateral)
                  i_cell = nnlat(k1)
                  delta_cell_volume = 1._dp / max(cell_volume(i_cell), dtol)
                  ! Transport_load is added to RHS of transport equation, sink is added to diagonal:
                  ! only multiply transport_load with concentration.
                  qlat = qqlat(i_layer, k1)
                  if (comparereal(qlat, 0._dp, eps10) > 0) then
                     transport_load(i_const, i_cell) = transport_load(i_const, i_cell) &
                                                     + delta_cell_volume * qlat * incoming_lat_concentration(1, i_const, i_lateral)
                  else
                  ! Sink sign-convention: positive means flux going out of model, hence the negative sign here
                     transport_sink(i_const, i_cell) = transport_sink(i_const, i_cell) - delta_cell_volume * qlat
                  end if
               end do
            end do
         end do
      end do
   end subroutine add_lateral_load_and_sink

   !> Compute water volume per layer in each lateral.
   !! The water volume in a lateral means the sum of water volumes in all
   !! grid cells belonging to the lateral (per layer).
   module subroutine get_lateral_volume_per_layer(lateral_volume_per_layer)

      use m_flow, only: vol1, kmx, kmxn
      use m_get_kbot_ktop, only: getkbotktop

      real(kind=dp), dimension(:, :), intent(out) :: lateral_volume_per_layer !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)

      integer :: i_node, i_lateral, i_layer, i_nnlat, i_vol1, index_vol1_bottom_layer, index_vol1_top_layer, index_active_bottom_layer

      lateral_volume_per_layer = 0._dp
      do i_lateral = 1, numlatsg
         do i_nnlat = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_nnlat)
            if (kmx > 0) then
               call getkbotktop(i_node, index_vol1_bottom_layer, index_vol1_top_layer)
               index_active_bottom_layer = kmx - kmxn(i_node) + 1
               i_layer = index_active_bottom_layer
               do i_vol1 = index_vol1_bottom_layer, index_vol1_top_layer
                  lateral_volume_per_layer(i_layer, i_lateral) = lateral_volume_per_layer(i_layer, i_lateral) + vol1(i_vol1)
                  i_layer = i_layer + 1
               end do
            else
               lateral_volume_per_layer(1, i_lateral) = lateral_volume_per_layer(1, i_lateral) + vol1(i_node)
            end if
         end do
      end do

   end subroutine get_lateral_volume_per_layer

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !!In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !! While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine reset_outgoing_lat_concentration()
      outgoing_lat_concentration = 0._dp
   end subroutine reset_outgoing_lat_concentration

   !> At the start of the update, the outgoing_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !! In  average_concentrations_for_laterals in outgoing_lat_concentration the concentrations*timestep are aggregated.
   !! While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine finish_outgoing_lat_concentration(time_interval)
      real(kind=dp), intent(in) :: time_interval
      outgoing_lat_concentration = outgoing_lat_concentration / time_interval
   end subroutine finish_outgoing_lat_concentration

   !> Distributes provided lateral discharge across flow nodes.
   !! Input is lateral discharge per layer per lateral, output is per layer per lateral per cell.
   module subroutine distribute_lateral_discharge(provided_lateral_discharge, lateral_discharge_per_layer_lateral_cell)

      use m_flow, only: vol1, kmx, kmxn
      use m_get_kbot_ktop, only: getkbotktop

      real(kind=dp), dimension(:, :), intent(in) :: provided_lateral_discharge !< Provided lateral discharge per layer
      real(kind=dp), dimension(:, :), intent(out) :: lateral_discharge_per_layer_lateral_cell !< Real lateral discharge per layer
                                                                                                 !! per lateral, per cell
      integer :: i_lateral, i_layer, i_nnlat, i_node, i_flownode
      integer :: i_node_bottom_layer, i_node_top_layer, i_active_bottom_layer

      lateral_discharge_per_layer_lateral_cell(:, :) = 0.0_dp
      do i_lateral = 1, numlatsg
         if (apply_transport(i_lateral) > 0) then
            do i_nnlat = n1latsg(i_lateral), n2latsg(i_lateral)
               i_node = nnlat(i_nnlat)
               call getkbotktop(i_node, i_node_bottom_layer, i_node_top_layer)
               i_active_bottom_layer = kmx - kmxn(i_node) + 1
               i_layer = max(i_active_bottom_layer, 1)
               do i_flownode = i_node_bottom_layer, i_node_top_layer
                  if (comparereal(lateral_volume_per_layer(i_layer, i_lateral), 0.0_dp, eps10) /= 0) then ! Avoid division by 0
                     lateral_discharge_per_layer_lateral_cell(i_layer, i_nnlat) = &
                        provided_lateral_discharge(i_layer, i_lateral) * (vol1(i_flownode) / lateral_volume_per_layer(i_layer, i_lateral))
                     i_layer = i_layer + 1
                  end if
               end do
            end do
         end if
      end do
   end subroutine distribute_lateral_discharge
end submodule m_laterals_implementation
