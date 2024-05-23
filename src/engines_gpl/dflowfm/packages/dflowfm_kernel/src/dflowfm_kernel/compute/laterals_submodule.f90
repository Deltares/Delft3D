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
         numlatsg = 0           !< [] nr of lateral discharge providers
      nlatnd   = 0           !< lateral nodes dimension, counter of nnlat(:)
   end subroutine reset_lateral

   !> allocate the arrays for laterals on 3d/BMI
   module subroutine initialize_lateraldata(numconst)
      use m_flow, only: kmx
      use m_alloc
   
      integer, intent(in) :: numconst        !< number of constitiuents
      
      integer :: i
      integer :: num_layers !< Number of layers

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
      call realloc(incoming_lat_concentration, (/num_layers, numconst, numlatsg/))
      call realloc(outgoing_lat_concentration, (/num_layers, numconst, numlatsg/))

   end subroutine initialize_lateraldata

   !> deallocate the arrays for laterals on 3d/BMI
   module subroutine dealloc_lateraldata()
   
      if (allocated(incoming_lat_concentration)) then
         deallocate(incoming_lat_concentration, outgoing_lat_concentration)
      endif
   

   end subroutine dealloc_lateraldata

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine average_concentrations_for_laterals(numconst, kmx, bottom_area, constituents, dt)

      integer,                       intent(in) :: numconst       !< Number or constituents.
      integer,                       intent(in) :: kmx            !< Number of layers (0 means 2d computation).
      real(kind=dp), dimension(:),   intent(in) :: bottom_area    !< Cell area.
      real(kind=dp), dimension(:,:), intent(in) :: constituents   !< concentrations.
      real(kind=dp),                 intent(in) :: dt             !< timestep in seconds

      integer :: ilat
      integer :: n, iconst, k, k1, kt, kb

      do ilat = 1, numlatsg
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
                  endif
                  outgoing_lat_concentration(1, iconst, ilat) =  outgoing_lat_concentration(1, iconst, ilat) + &
                                                                 dt * bottom_area(n) * constituents(iconst, k)/balat(ilat)
               endif
            enddo
         enddo
      enddo

   end subroutine average_concentrations_for_laterals

   !> Compute water volume per layer in each lateral
   module subroutine get_lateral_volume_per_layer(lateral_volume_per_layer)
   
      use m_flow, only: vol1, kmx, kmxn
      
      real(kind=dp), dimension(:,:), intent(out)   :: lateral_volume_per_layer                !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
      
      integer :: i_node, i_lateral, i_layer, i_nnlat, i_vol1, index_vol1_bottom_layer, index_vol1_top_layer, index_active_bottom_layer
      
      lateral_volume_per_layer = 0d0
      do i_lateral = 1, numlatsg
         do i_nnlat = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_nnlat)
            call getkbotktop(i_node, index_vol1_bottom_layer, index_vol1_top_layer)
            index_active_bottom_layer = kmx - kmxn(i_node) + 1
            i_layer = index_active_bottom_layer
            do i_vol1 = index_vol1_bottom_layer, index_vol1_top_layer
               lateral_volume_per_layer(i_layer, i_lateral) = lateral_volume_per_layer(i_layer, i_lateral) + vol1(i_vol1)
               i_layer = i_layer + 1
            enddo
         enddo
      enddo
      
   end subroutine get_lateral_volume_per_layer

   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine reset_outgoing_lat_concentration()
      outgoing_lat_concentration = 0d0
   end subroutine reset_outgoing_lat_concentration
      
   !> At the start of the update, the out_going_lat_concentration must be set to 0 (reset_outgoing_lat_concentration).
   !> In  average_concentrations_for_laterals in out_going_lat_concentration the concentrations*timestep are aggregated.
   !> While in finish_outgoing_lat_concentration, the average over time is actually computed.
   module subroutine finish_outgoing_lat_concentration(time_interval)
      real(kind=dp), intent(in) :: time_interval
      outgoing_lat_concentration = outgoing_lat_concentration/time_interval
   end subroutine finish_outgoing_lat_concentration
   
end submodule m_lateral_implementation
