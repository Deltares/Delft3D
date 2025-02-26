module m_flow1d_reader
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!-------------------------------------------------------------------------------
  
   implicit none
   
   private
   
   public read_1d_attributes
   
   contains
    
   subroutine read_1d_attributes(filenames, network)

      use string_module
      use m_globalParameters
      use messageHandling
      use m_readCrossSections
      use m_read_roughness
      use m_network
      use m_readstructures
      use m_readObservationPoints
      use m_readStorageNodes
      use properties
      use timers
   
      implicit none
      
      ! Variables
      type(t_filenames), intent(inout):: filenames
      type(t_network), intent(inout)  :: network
      
      integer                         :: maxErrorLevel
      logical                         :: success
      
      integer                         :: timerReadCsDefs    = 0
      integer                         :: timerReadCsLocs    = 0
      integer                         :: timerReadStorgNodes= 0
      integer                         :: timerReadRoughness = 0

      timon = .true.

      success = .true.
     
      ! Read roughnessFile file
      call timstrt('ReadRoughness', timerReadRoughness)
      call SetMessage(LEVEL_INFO, 'Reading Roughness ...')

      call roughness_reader(network, filenames%roughness)
     
      call SetMessage(LEVEL_INFO, 'Reading Roughness Done')
      call timstop(timerReadRoughness)

      ! Read cross section definitions
      call timstrt('ReadCsDefs', timerReadCsDefs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions ...')
     
      call readCrossSectionDefinitions(network, filenames%cross_section_definitions)
      
      if (network%CSDefinitions%Count < 1) then
         call SetMessage(LEVEL_WARN, 'No Cross_Section Definitions Found')
      endif
     
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions Done')
      call timstop(timerReadCsDefs)
      
      ! Read cross section locations
      call timstrt('ReadCsLocs', timerReadCsLocs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations ...')

      call readCrossSectionLocationFile(network, filenames%cross_section_locations)
         
     if (network%crs%Count < 1) then
        allocate(network%crs%cross(0))
        call SetMessage(LEVEL_WARN, 'No Cross Sections Found')
     endif

     call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations Done')
     call timstop(timerReadCsLocs)
  
     if (len_trim(filenames%storage_nodes) > 0) then ! if a storage node file is specified
        call timstrt('ReadStorageNodes', timerReadStorgNodes)
        call SetMessage(LEVEL_INFO, 'Reading Storage Nodes ...')
        
        ! Read storage nodes file
        call readStorageNodes(network, filenames%storage_nodes)
         
        call SetMessage(LEVEL_INFO, 'Reading Storage Nodes Done')
        call timstop(timerReadStorgNodes)
     end if
     
     ! Stop in case of errors
     maxErrorLevel = getMaxErrorLevel()
     if (maxErrorLevel >= LEVEL_ERROR) then
        call SetMessage(LEVEL_FATAL, 'Error(s) while reading 1D attribute files')
     endif
     
     call SetMessage(LEVEL_INFO, 'All 1D-Reading Done')

   end subroutine read_1d_attributes
 
end module m_flow1d_reader
    
    
   
