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

   end subroutine initialize_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   module subroutine dealloc_lateraldata()
   
      if (allocated(incoming_lat_concentration)) then
         deallocate(incoming_lat_concentration, outgoing_lat_concentration, lateral_volume_per_layer)
      end if
   

   end subroutine dealloc_lateraldata

   !> At the start of an update, the outgoing_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In average_concentrations_for_laterals, the concentrations*timestep are aggregated in outgoing_lat_concentration.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine average_concentrations_for_laterals(numconst, kmx, cell_volume, constituents, dt)

      integer,                       intent(in) :: numconst       !< Number or constituents.
      integer,                       intent(in) :: kmx            !< Number of layers (0 means 2D computation).
      real(kind=dp), dimension(:),   intent(in) :: cell_volume    !< Volume of water in computational cells. 
      real(kind=dp), dimension(:,:), intent(in) :: constituents   !< Concentrations of constituents.
      real(kind=dp),                 intent(in) :: dt             !< Timestep in seconds

      integer :: ilat, n, iconst, k, k1, kt, kb
      
      real(kind=dp) :: total_volume

      do ilat = 1, numlatsg
         total_volume = 0_dp
         do iconst = 1, numconst
            do k1 = n1latsg(ilat), n2latsg(ilat)
               n = nnlat(k1)
               if (n > 0) then
                  if (kmx < 1) then 
                     k = n
                  else
                     ! For now we only use the top layer
                     call getkbotktop(n, kb, kt)
                     k = kt
                  end if
                  total_volume = total_volume + cell_volume(k)
                  outgoing_lat_concentration(1,iconst,ilat) =  outgoing_lat_concentration(1,iconst,ilat) + &
                                                                 dt*cell_volume(k)*constituents(iconst,k)
               end if
            end do
         end do
         outgoing_lat_concentration(:,:,ilat)= outgoing_lat_concentration(:,:,ilat) / total_volume
      end do
   
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
   
end submodule m_lateral_implementation
