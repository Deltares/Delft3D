subroutine grids_and_gridmaps (n_swan_grids, n_flow_grids, sr, mode)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
   integer                             :: n_flow_grids     ! number of FLOW grids
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
   ! Find out number of FLOW domains
   !
   write(*,'(a)') '  Initialising grids and grid mappings'
   if (mode == stand_alone) then
      !
      ! If flow results are used
      !
      if (sr%useflowdata .or. sr%swwav) then
         n_flow_grids = num_subdomains
      else
         n_flow_grids = 0
      endif
   else
      n_flow_grids=num_subdomains
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
   call Init_Grids (n_swan_grids, n_flow_grids)
   
   do i=1,n_swan_grids
      grid_name = swangrid(i)
      grid_file_type ='FLOW'
      xy_loc         ='CORNER'
      call Alloc_and_get_grid(i, swan_grids(i),grid_name,grid_file_type,xy_loc, sr%flowLinkConnectivity)
      call write_bnd_loc(i,swan_grids(i))
      if (netcdf_files) then
         call write_wave_grid_netcdf(i, swan_grids(i), grid_name, flow2swan_maps(i,1)%r_tmp_filename)
      endif
   enddo

   do i=1,n_flow_grids
      if (.not. flow_data_initialized) then
         write(*,'(a)') '*** ERROR: FLOW data (runid(s)) is not initialized.'
         call wavestop(1, '*** ERROR: FLOW data (runid(s)) is not initialized.')
      endif
      if (netcdf_files) then
         if (n_flow_grids == 1) then
            grid_name = sr%flowgridfile
         else
            partitionlocation = index(sr%flowgridfile, '_com.nc')
            write(grid_name,'(a,a,i4.4,a)') sr%flowgridfile(:partitionlocation-1), '_', i-1, trim(sr%flowgridfile(partitionlocation:))
         endif
         grid_file_type = 'NC'
         xy_loc         = 'CENTER'
         call Alloc_and_get_grid(i, flow_grids(i),grid_name,grid_file_type,xy_loc, sr%flowLinkConnectivity, flow2swan_maps(1,i)%p_tmp_filename)
      else
         write(grid_name,'(a,a)')'com-',trim(runids(i))
         grid_file_type ='COM'
         xy_loc         ='CENTER'
         call Alloc_and_get_grid(i, flow_grids(i),grid_name,grid_file_type,xy_loc, sr%flowLinkConnectivity)
      endif
   enddo

   do i=1,n_swan_grids
      do j=2,n_flow_grids
         flow2swan_maps(i,j)%r_tmp_filename = flow2swan_maps(i,1)%r_tmp_filename
      enddo
   enddo
   do i=2,n_swan_grids
      do j=1,n_flow_grids
         flow2swan_maps(i,j)%p_tmp_filename = flow2swan_maps(1,j)%p_tmp_filename
      enddo
   enddo

   
   do i=1,n_swan_grids
      s=>swan_grids(i)
      do j=1,n_flow_grids
         f=>flow_grids(j)
         s2f=>swan2flow_maps(i,j)
         f2s=>flow2swan_maps(i,j)
         s2f%msurpnts = sr%msurpnts
         f2s%msurpnts = sr%msurpnts
         call make_grid_map(j, i, f, s, f2s, netcdf_files)
         !
         ! Mapping from (structured) SWAN to FLOW is never externally
         !
         call make_grid_map(i, j, s, f, s2f, .false.)
      enddo
   enddo
   
end subroutine grids_and_gridmaps
