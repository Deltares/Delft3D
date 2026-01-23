subroutine grids_and_gridmaps (n_swan_grids, sr, mode)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use wave_data
   !
   implicit none
!
! local parameters
!
   integer, parameter :: nswmax=10
!
! Global variables
!
   integer                             :: n_swan_grids     ! number of SWAN grids
   integer                             :: mode
   type(swan_type)                     :: sr
!
! Local variables
!
   integer                           :: i
   integer                           :: j
   integer                           :: i_swan
   integer                           :: lun              ! file unit
   integer                           :: partitionlocation
   logical                           :: exists
   logical                           :: netcdf_files     ! .true.: Flow data is read from  NetCDF file(s)
                                                         !         Wave data is written to NetCDF file(s)
                                                         !         Mapping between grids is done using an external program e.g. ESMF_RegridWeightGen
                                                         !         Therefore all grids are written to a temporary NetCDF file in a specific format
   character (256) ,dimension(nswmax):: swangrid
   character (256)                   :: grid_name        ! name of grid
   character (4)                     :: grid_file_type   ! type of grid file (SWAN/FLOW/COM/TRIM)
   character (6)                     :: xy_loc           ! location of xy coords (CORNER/CENTER)
   type(grid)    ,pointer            :: f,s              ! local pointers to grids
   type(grid_map),pointer            :: f2s,s2f          ! local pointers to gridmaps
!
!! executable statements -------------------------------------------------------
!
   if (sr%flowgridfile /= ' ') then
      netcdf_files = .true.
   else
      netcdf_files = .false.
   endif

! Find out number of SWAN grids
   i_swan=1
   inquire(file='swangrids', exist=exists)
   if (exists) then
      open(newunit=lun, file='swangrids')
      do
         read(lun,'(a)')swangrid(i_swan)
         if (trim(swangrid(i_swan)).eq.'end') exit
         i_swan=i_swan+1
      enddo
      n_swan_grids=i_swan-1
      close(lun)
   elseif (sr%nnest>0) then
      do i_swan=1,sr%nnest
         swangrid(i_swan)=sr%dom(i_swan)%curlif
      enddo
   else
      write(*,'(a)') '*** ERROR: File swangrids not available or error reading mdw file'
      call wavestop(1, '*** ERROR: File swangrids not available or error reading mdw file')
   endif

   ! Allocate pointer arrays grid structures
   call Init_Grids(n_swan_grids)
   
   do i=1,n_swan_grids
      grid_name = swangrid(i)
      grid_file_type ='FLOW'
      xy_loc         ='CORNER'
      call Alloc_and_get_grid(i, swan_grids(i),grid_name,grid_file_type,xy_loc, sr%flowLinkConnectivity)
      call write_bnd_loc(i,swan_grids(i))
   enddo
end subroutine grids_and_gridmaps
