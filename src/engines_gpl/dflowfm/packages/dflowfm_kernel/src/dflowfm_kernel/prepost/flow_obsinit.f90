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

 !> Initializes all administration encessary for writing output to his-files.
 !! That is: snap observation stations to flow cells, cross sections to flow links.
 !! And bookkeeping for time series output on structures.
 subroutine flow_obsinit()
    use m_observations, only : init_valobs
    use unstruc_model,  only : md_delete_observation_points_outside_grid
    use m_wind
    use m_structures
    use fm_external_forcings, only: allocatewindarrays
    use m_obs_on_flowgeom, only: obs_on_flowgeom
    
    implicit none
 
    call crosssections_on_flowgeom()
    call runupgauges_on_flowgeom()

    if (jawind == 1 ) then ! was needed here if jawind was set 1 by windext
       call allocatewindarrays()
    endif 

    call obs_on_flowgeom(0)
    
    if ( md_delete_observation_points_outside_grid ) then
       call delete_static_observation_points_outside_grid()
    end if


!   for the following, it is assumed that the moving observation stations have been initialized (in flow_initexternalforcings)
    call init_valobs()   ! (re)initialize work array and set pointers for observation stations

    call updateValuesOnObservationStations() ! and fill first value

    call init_structure_hisvalues()
   
    contains
    
    !> delete_static_observation_points_outside_grid
   subroutine delete_static_observation_points_outside_grid()
      use m_observations, only : kobs, numobs, deleteObservation, purgeObservations
      use MessageHandling
   
      integer :: point
      integer :: number_of_deleted_points
      
      number_of_deleted_points = 0
      do point = 1, numobs
         if ( kobs(point) == 0 ) then
            call deleteObservation(point)
            number_of_deleted_points = number_of_deleted_points + 1
         end if
      end do
      
      if (number_of_deleted_points > 0) then 
         call purgeObservations()
         write(msgbuf, '(a,i0)') 'Number of deleted observation points outside of the grid is ', number_of_deleted_points
         call msg_flush()
     end if
   
   end subroutine delete_static_observation_points_outside_grid

 end subroutine flow_obsinit
