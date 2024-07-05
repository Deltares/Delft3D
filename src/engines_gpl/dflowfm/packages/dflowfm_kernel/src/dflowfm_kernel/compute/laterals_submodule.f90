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
submodule (m_lateral) m_lateral_implementation

implicit none

   contains

   !> Reset the defaults for laterals
   module subroutine default_lateral()
      call reset_lateral()
   end subroutine default_lateral

   !> Reset the counters for lateral data.
   module subroutine reset_lateral()
      numlatsg = 0  !< [] nr of lateral discharge providers
      nlatnd   = 0  !< lateral nodes dimension, counter of nnlat(:)
   end subroutine reset_lateral

   !> allocate the arrays for laterals on 3d/BMI
   module subroutine initialize_lateraldata(numconst)
      use m_flow, only: kmx
      use m_alloc
   
      integer, intent(in)    :: numconst  !< number of constitiuents
      
      integer :: i           ! loop counter
      integer :: num_layers  ! Number of layers

      apply_transport_is_used = .false.
      if (allocated(apply_transport)) then
         do i = 1, numlatsg
            if (apply_transport(i)==1) then
               apply_transport_is_used = .true.
               ! No need to look further
               exit
            end if
         end do
      end if

      num_layers = max(1, kmx)
      call realloc(incoming_lat_concentration, [num_layers, numconst, numlatsg])
      incoming_lat_concentration = 0._dp
      call realloc(outgoing_lat_concentration, [num_layers, numconst, numlatsg])
      call realloc(lateral_volume_per_layer,[num_layers,numlatsg])
      call realloc(lateral_center_position_per_layer,[num_layers,numlatsg])
      
   end subroutine initialize_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   module subroutine dealloc_lateraldata()
   
      if (allocated(incoming_lat_concentration)) then
         deallocate(incoming_lat_concentration)
         deallocate(outgoing_lat_concentration)
         deallocate(lateral_volume_per_layer)
         deallocate(lateral_center_position_per_layer)
      end if
   

   end subroutine dealloc_lateraldata

   !> At the start of an update, the outgoing_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In average_concentrations_for_laterals, the concentrations*timestep are aggregated in outgoing_lat_concentration.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine average_concentrations_for_laterals(numconst, kmx, kmxn, cell_volume, constituents, dt)

      use m_alloc

      integer,                       intent(in) :: numconst       !< Number or constituents.
      integer,                       intent(in) :: kmx            !< Number of layers (0 means 2D computation).
      integer, dimension(:),         intent(in) :: kmxn           !< Maximum number of vertical cells per base node n.
      real(kind=dp), dimension(:),   intent(in) :: cell_volume    !< Volume of water in computational cells. 
      real(kind=dp), dimension(:,:), intent(in) :: constituents   !< Concentrations of constituents.
      real(kind=dp),                 intent(in) :: dt             !< Timestep in seconds.

      integer :: ilat, i_node, iconst, k, k1, kt, kb
      integer :: num_layers, i_layer
      integer :: iostat
      
      real(kind=dp), dimension(:), allocatable :: total_volume

      num_layers = max(1, kmx)
      
      allocate(total_volume(num_layers), stat=iostat)
      call aerr('total_volume',iostat,num_layers,'average_concentrations_for_laterals')
      
      do ilat = 1, numlatsg
         total_volume = 0.0_dp
         do k1 = n1latsg(ilat), n2latsg(ilat)
            i_node = nnlat(k1)
            if (i_node > 0) then
               if (kmx < 1) then
                  total_volume = total_volume + cell_volume(i_node)
                  do iconst = 1, numconst
                     outgoing_lat_concentration(1, iconst, ilat) = outgoing_lat_concentration(1, iconst, ilat) + &
                                                                   dt * cell_volume(i_node) * constituents(iconst, i_node)
                  end do
               else
                  i_layer = kmx - kmxn(i_node) + 1 ! initialize i_layer to the index of first active bottom layer of base node(i_node)
                  call getkbotktop(i_node, kb, kt)
                  do k = kb, kt ! loop over active layers under base node(i_node)
                     total_volume(i_layer) = total_volume(i_layer) + cell_volume(k)
                     do iconst = 1, numconst
                        outgoing_lat_concentration(i_layer, iconst, ilat) = outgoing_lat_concentration(i_layer, iconst, ilat) + &
                                                                            dt * cell_volume(k) * constituents(iconst, k)
                     end do
                     i_layer = i_layer + 1
                  end do
               end if
            end if
         end do
         do i_layer = 1, num_layers
            if (total_volume(i_layer) > 0) then
               outgoing_lat_concentration(i_layer, :, ilat) = outgoing_lat_concentration(i_layer, :, ilat) / total_volume(i_layer)
            else
               outgoing_lat_concentration(i_layer, :, ilat) = 0.0_dp
            end if
         end do
      end do
      
      deallocate(total_volume)

   end subroutine average_concentrations_for_laterals
   
   !> Calculate lateral discharges at each of the active grid cells, both source (lateral_discharge_in) and sink (lateral_discharge_out). 
   module subroutine get_lateral_discharge(lateral_discharge_in,lateral_discharge_out, cell_volume)
      use m_flow, only: hs
      use m_flowparameters, only: epshu
      use m_flowtimes, only: dts
      use m_partitioninfo, only: is_ghost_node

      real(kind=dp), dimension(:,:), intent(inout) :: lateral_discharge_in   !< Lateral discharge flowing into the model (source)
      real(kind=dp), dimension(:,:), intent(inout) :: lateral_discharge_out  !< Lateral discharge extracted out of the model (sink)
      real(kind=dp), dimension(:),   intent(in)    :: cell_volume            !< [m3] total volume at end of timestep {"location": "face", "shape": ["ndx"]}
      
      integer :: k1, i_cell, i_lateral
      real(kind=dp) :: qlat
      
      if (numlatsg > 0) then
         lateral_discharge_in=0._dp
         lateral_discharge_out=0._dp
         do i_lateral = 1,numlatsg
            if (apply_transport(i_lateral) ==1) then 
               do k1=n1latsg(i_lateral),n2latsg(i_lateral)
                  ! loop over all elements of the lateral that are inside the current domain
                  i_cell = nnlat(k1)
                  qlat = qplat(1,i_lateral) * cell_volume(i_cell)
                  if (qlat > 0) then
                     if (.not. is_ghost_node(i_cell)) then 
                        lateral_discharge_in(i_lateral,i_cell) = lateral_discharge_in(i_lateral,i_cell) + qlat
                     end if
                  else if (hs(i_cell) > epshu) then
                     qlat = - min(0.5_dp*cell_volume(i_cell)/dts , -qlat) ! this is required to conserve mass
                     if (.not. is_ghost_node(i_cell)) then
                        lateral_discharge_out(i_lateral,i_cell) = lateral_discharge_out(i_lateral,i_cell) - qlat
                     end if
                  end if
               end do
            end if
         end do
      end if
   end subroutine get_lateral_discharge
   
   
   ! Add lateral input contribution to the load being transported
   module subroutine add_lateral_load_and_sink(transport_load,transport_sink,discharge_in,discharge_out,cell_volume,dtol)
      use m_transportdata, only: numconst
      real(kind=dp), dimension(:,:), intent(inout) :: transport_load  !< Load being transported into domain
      real(kind=dp), dimension(:,:), intent(inout) :: transport_sink  !< Load being transported out
      real(kind=dp), dimension(:,:), intent(in   ) :: discharge_in    !< Lateral discharge going into domain (source)
      real(kind=dp), dimension(:,:), intent(in   ) :: discharge_out   !< Lateral discharge going out (sink)
      real(kind=dp), dimension(:),   intent(in)    :: cell_volume     !< [m3] total volume at end of timestep {"location": "face", "shape": ["ndx"]}
      real(kind=dp), intent(in)                    :: dtol            !< cut off value for cell_volume, to prevent division by zero

      real(kind=dp) :: delta_cell_volume
      integer :: i_const, i_lateral, i_cell, k1
      
      do i_const = 1,numconst   
         do i_lateral = 1,numlatsg
            do k1=n1latsg(i_lateral),n2latsg(i_lateral)
               i_cell = nnlat(k1)
               delta_cell_volume = 1._dp/max(cell_volume(i_cell),dtol)
               ! transport_load is added to RHS of transport equation, sink is added to diagonal:
               ! only multiply transport_load with concentration
               transport_load(i_const,i_cell) = transport_load(i_const,i_cell) + delta_cell_volume* discharge_in(i_lateral,i_cell) * incoming_lat_concentration(1,i_const,i_lateral)
               transport_sink(i_const,i_cell) = transport_sink(i_const,i_cell) + delta_cell_volume * discharge_out(i_lateral,i_cell)
            end do
         end do
      end do   
   end subroutine add_lateral_load_and_sink
      
   !> Compute water volume per layer in each lateral.
   !! The water volume in a lateral means the sum of water volumes in all
   !! grid cells belonging to the lateral (per layer).
   module subroutine get_lateral_volume_per_layer(lateral_volume_per_layer)
   
      use m_flow, only: vol1, kmx, kmxn
      
      real(kind=dp), dimension(:,:), intent(out) :: lateral_volume_per_layer  !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
      
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

   !> Calculate the average cell center positions for each layer for all laterals.
   module subroutine get_lateral_layer_positions(lateral_center_position_per_layer, cell_center_position)
   
      use m_flow, only: cell_center_position, kmx, kmxn
      
      real(kind=dp), dimension(:,:), intent(out) :: lateral_center_position_per_layer  !< Lateral center position of each layer, 
                                                                                       !< dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg).
      real(kind=dp), dimension(:),   intent(in)  :: cell_center_position               !< Vertical cell center positions.
      
      integer, allocatable, target, dimension(:) :: active_cell_count    !< Help array for counting the active cells.
      integer :: i_node, i_lateral, i_layer, i_nnlat, index_bottom_layer, index_top_layer, index_active_bottom_layer
      
      if (kmx == 0) then
         return
      end if
      allocate(active_cell_count(kmx))
      
      lateral_center_position_per_layer = 0.0_dp
      do i_lateral = 1, numlatsg

         ! Totalize the cell center positions and count the number of active cells per layer.
         active_cell_count = 0
         do i_nnlat = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_nnlat)
            call getkbotktop(i_node, index_bottom_layer, index_top_layer)
            index_active_bottom_layer = kmx - kmxn(i_node) + 1

            call accumulate_active_cell_positions(lateral_center_position_per_layer(index_active_bottom_layer :, i_lateral), &
                                               cell_center_position(index_bottom_layer : index_top_layer), &
                                               active_cell_count(index_active_bottom_layer : ))
         end do
         
         ! Use the totalized center positions to calculate the average center positions.
         do i_layer = 1, kmx
            if (active_cell_count(i_layer) > 0) then
               lateral_center_position_per_layer(i_layer, i_lateral) = &
                              lateral_center_position_per_layer(i_layer, i_lateral)/active_cell_count(i_layer)
            else
               lateral_center_position_per_layer(i_layer, i_lateral) = huge(1.0_dp)
            end if
         end do
      
      end do
      deallocate(active_cell_count)
      
   end subroutine get_lateral_layer_positions

   !> Add the cell center positions of 1 node to the lateral_center_position_per_layer and update the active_cell_count.
   subroutine accumulate_active_cell_positions(lateral_center_position_per_layer, cell_center_position, active_cell_count)
      real(kind=dp), dimension(:), intent(out)   :: lateral_center_position_per_layer  !< Cumulative cell center positions for 1 lateral.
      real(kind=dp), dimension(:), intent(in)    :: cell_center_position               !< Cell center positions per layer for the current cell.
      integer,       dimension(:), intent(inout) :: active_cell_count                  !< Number of active cells per layer.

      integer :: i

      do i = 1, size(cell_center_position)
         active_cell_count(i) = active_cell_count(i) + 1
         lateral_center_position_per_layer(i) = lateral_center_position_per_layer(i) + &
                                                cell_center_position(i)
      end do

   end subroutine accumulate_active_cell_positions
            

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine reset_outgoing_lat_concentration()
      outgoing_lat_concentration = 0._dp
   end subroutine reset_outgoing_lat_concentration
      
   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine finish_outgoing_lat_concentration(time_interval)
      real(kind=dp), intent(in) :: time_interval
      outgoing_lat_concentration = outgoing_lat_concentration/time_interval
   end subroutine finish_outgoing_lat_concentration

   !> Distributes lateral discharge per layer, that is retrieved from BMI, to per layer per cell
   module subroutine distribute_lateral_discharge_per_layer_per_cell(provided_lateral_discharge_per_layer, &
                                                                     lateral_discharge_per_layer_per_cell)

      use m_flow,             only: vol1, kmx, kmxn
      use precision_basics,   only: comparereal
      use m_GlobalParameters, only: flow1d_eps10

      real(kind=dp), dimension(:,:), intent(in   ) :: provided_lateral_discharge_per_layer !< Provided lateral discharge per
                                                                                           !! layer, which is retrieved from BMI
      real(kind=dp), dimension(:,:), intent(  out) :: lateral_discharge_per_layer_per_cell !< Real lateral discharge per layer
                                                                                           !! per cell

      integer :: i_lateral, i_layer, i_nnlat, i_node, i_flownode
      integer :: i_node_bottom_layer, i_node_top_layer, i_active_bottom_layer


      lateral_discharge_per_layer_per_cell(:,:) = 0.0_dp

      do i_lateral = 1,numlatsg
         do i_nnlat = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_nnlat)
            call getkbotktop(i_node, i_node_bottom_layer, i_node_top_layer)
            i_active_bottom_layer = kmx - kmxn(i_node) + 1
            i_layer = i_active_bottom_layer
            do i_flownode = i_node_bottom_layer, i_node_top_layer
               if (comparereal(lateral_volume_per_layer(i_layer, i_lateral), 0.0_dp, flow1d_eps10) /= 0) then ! Avoid division by 0
                  lateral_discharge_per_layer_per_cell(i_layer, i_flownode) = vol1(i_flownode) &
                                                                              / lateral_volume_per_layer(i_layer, i_lateral) &
                                                                              * provided_lateral_discharge_per_layer(i_layer, i_lateral)
                  i_layer = i_layer + 1
               end if
            end do
         end do
      end do
   end subroutine distribute_lateral_discharge_per_layer_per_cell
end submodule m_lateral_implementation
