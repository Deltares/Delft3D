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
   
      use m_alloc
   
      integer, intent(in) :: numconst        !< number of constitiuents
      
      integer :: i

      apply_transport_is_used = .false.
      do i = 1, numlatsg
         if (apply_transport(i)==1) then
            apply_transport_is_used = .true.
            ! No need to look further
            exit
         end if
      end do
      call realloc(incoming_lat_concentration, (/1, numconst, numlatsg/))
      call realloc(outgoing_lat_concentration, (/1, numconst, numlatsg/))

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

      integer                         , intent(in)    :: numconst       !< Number or constituents.
      integer                         , intent(in)    :: kmx            !< Number of layers (0 means 2d computation).
      double precision, dimension(:)  , intent(in)    :: bottom_area    !< Cell area.
      double precision, dimension(:,:), intent(in)    :: constituents   !< concentrations.
      double precision,                 intent(in)    :: dt             !< timestep in seconds

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
      double precision, intent(in   )  :: time_interval
      outgoing_lat_concentration = outgoing_lat_concentration/time_interval
   end subroutine finish_outgoing_lat_concentration
   
end submodule m_lateral_implementation
