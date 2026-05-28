subroutine get_flow_fields (i_flow, i_swan, sif, fg, sg, f2s, wavedata, sr, flowVelocityType)
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
! Global variables
!
   integer                          :: i_flow
   integer                          :: i_swan
   integer                          :: flowVelocityType
   type(input_fields)               :: sif              ! input fields defined on swan grid
   type(grid)                       :: fg               ! flow grid
   type(grid)                       :: sg               ! swan grid
   type(grid_map)                   :: f2s              ! flow to swn grid mapper
   integer, dimension(:,:), pointer :: covered
   type(wave_data_type)             :: wavedata
   type(swan_type)                  :: sr               ! swan input structure
!
! Local variables
!
   integer            :: i
   integer            :: j
   integer            :: iprint       = 0
   real               :: alpb         = 0.0
   real               :: dummy        = -999.0
   real               :: maxval
   logical            :: clbot        = .true.
   character(256)     :: mudfilnam    = ' '
   type(input_fields) :: fif                    ! input fields defined on flow grid

   interface
      subroutine grmap_esmf(i1, f1, n1, f2, mmax, nmax, f2s, f2g)
         use swan_flow_grid_maps
         integer                   , intent(in)  :: i1
         integer                   , intent(in)  :: n1
         integer                   , intent(in)  :: mmax
         integer                   , intent(in)  :: nmax
         real   , dimension(n1)    , intent(in)  :: f1
         real   , dimension(mmax,nmax)           :: f2
         type(grid_map)            , intent(in)  :: f2s
         type(grid)                              :: f2g  ! f2 grid
      end subroutine grmap_esmf

      subroutine get_var_netcdf(i_flow, wavetime, varname, vararr, mmax, nmax, basename, &
                              & lastvalidflowfield, kmax, flowVelocityType)
         use wave_data         
         integer                      , intent(in)  :: i_flow
         type(wave_time_type)                       :: wavetime
         character(*)                 , intent(in)  :: varname
         real   , dimension(mmax,nmax), intent(out) :: vararr
         integer                      , intent(in)  :: mmax
         integer                      , intent(in)  :: nmax
         character(*)                               :: basename
         integer                                    :: lastvalidflowfield
         integer, optional            , intent(in)  :: kmax
         integer, optional            , intent(in)  :: flowVelocityType
      end subroutine
   end interface
   !
   !! executable statements -------------------------------------------------------
   !
   ! Allocate memory swan input fields defined on flow grid
   !
   call alloc_input_fields(fg, fif, wavedata%mode)
   !
   if (sr%dom(i_swan)%qextnd(q_bath)>0) then
      if (sr%flowgridfile == ' ') then
         !
         ! Read depth from com-file
         !
         call get_dep (fif%dps, fif%mmax, fif%nmax, &
                     & fg%grid_name)
         !
         ! Map depth to SWAN grid
         !
         call map_flow_to_swan(fif%dps, sif%dps)
      else
         !
         ! Read depth from netcdf-file
         !
         call get_var_netcdf (i_flow, wavedata%time , 'dps', &
                            & fif%dps, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         !
         ! Map depth to SWAN grid
         !
         call map_flow_to_swan(fif%dps, sif%dps)
         !
      endif
   endif
   !
   ! Read polygons fixed structures
   !
   call dam_cod (fg%x, fg%y, fg%kcs, fg%mmax, fg%nmax)
   !
   if (sr%dom(i_swan)%qextnd(q_wl)>0) then
      if (sr%flowgridfile == ' ') then
         !
         ! Read water level from com-file
         !
         call get_lev (wavedata%time , &
                     & fif%s1, fif%mmax, fif%nmax, &
                     & fg%grid_name)
         !
         ! Map water level to SWAN grid
         !
         call map_flow_to_swan(fif%s1, sif%s1)
      else
         !
         ! Read water level from netcdf-file
         !
         call get_var_netcdf (i_flow, wavedata%time , 's1', &
                            & fif%s1, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         !
         ! Map water level to SWAN grid
         !
         call map_flow_to_swan(fif%s1, sif%s1)
      endif
   endif
   !
   if (sr%dom(i_swan)%qextnd(q_cur)>0) then
      if (sr%flowgridfile == ' ') then
         !
         ! Read velocity from com-file
         !
         call get_cur (wavedata%time, &
                     & fif%kfu, fif%kfv, fif%u1, fif%v1, fif%mmax, fif%nmax, &
                     & fg%kmax, fg%grid_name, fg%layer_model, flowVelocityType, &
                     & fif%dps, fif%s1)
         !
         ! Convert to Cartesian, cell centres
         !
         call flow2wav (fif%u1  , fif%v1 , &
                      & fg%alfas, fg%guu , fg%gvv, fg%mmax, fg%nmax, fg%kcs, &
                      & fif%kfu , fif%kfv, alpb  , clbot  )
         !
         ! Map velocity to SWAN grid
         ! NOTE: mapping procedure only updates the part of SWAN grid covered by current FLOW domain
         !
         call map_flow_to_swan(fif%u1, sif%u1)
         call map_flow_to_swan(fif%v1, sif%v1)
      else
         !
         ! Read velocity components from netcdf-file
         !
         if (fg%kmax == 1) then
            call get_var_netcdf (i_flow, wavedata%time , 'u1', &
                               & fif%u1, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            call get_var_netcdf (i_flow, wavedata%time , 'v1', &
                               & fif%v1, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         else
            call get_var_netcdf (i_flow, wavedata%time , 'u1', &
                               & fif%u1, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield, fg%kmax,flowVelocityType)
            call get_var_netcdf (i_flow, wavedata%time , 'v1', &
                               & fif%u1, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield, fg%kmax,flowVelocityType)                      
         endif                   
         !
         ! Map velocity components to SWAN grid
         !
         call map_flow_to_swan(fif%u1, sif%u1)
         call map_flow_to_swan(fif%v1, sif%v1)
      endif
   endif
   !
   if (sr%dom(i_swan)%qextnd(q_wind) >= 1) then
      if (sr%flowgridfile == ' ') then
         !
         ! Read wind from com-file
         !
         call get_wind (wavedata%time, &
                      & fif%windu, fif%windv, fif%mmax, fif%nmax, &
                      & fg%grid_name)
         !
         ! Map wind to SWAN grid
         !
         call map_flow_to_swan(fif%windu, sif%windu)
         call map_flow_to_swan(fif%windv, sif%windv)
      else
         !
         ! Read wind components from netcdf-file
         !
         call get_var_netcdf (i_flow, wavedata%time , 'windx', &
                            & fif%windu, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         call get_var_netcdf (i_flow, wavedata%time , 'windy', &
                            & fif%windv, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         !
         ! Map wind components to SWAN grid
         !
         call map_flow_to_swan(fif%windu, sif%windu)
         call map_flow_to_swan(fif%windv, sif%windv)
      endif
   endif
   !
if (sr%swveg .and. sr%dom(1)%qextnd(q_veg) >= 1) then
      if (sr%flowgridfile == ' ') then
         !
         ! There is no vegetation on the Delf3D4-FLOW com file
         !
         write(*,'(a)') "ERROR: trying to read vegetation from Delft3D4-FLOW com-file. Not implemented yet."
         call wavestop(1, "ERROR: trying to read vegetation from Delft3D4-FLOW com-file. Not implemented yet.")
      else
         !
         ! Read vegetation parameters from netcdf-file
         !
         call get_var_netcdf (i_flow, wavedata%time , 'rnveg', &
                            & fif%veg, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         call get_var_netcdf (i_flow, wavedata%time , 'diaveg', &
                            & fif%diaveg, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         call get_var_netcdf (i_flow, wavedata%time , 'veg_stemheight', &
                            & fif%veg_stemheight, fif%mmax, fif%nmax, &
                            & sr%flowgridfile, wavedata%output%lastvalidflowfield)
         !
         ! Map vegetation components to SWAN grid
         !
         call map_flow_to_swan(fif%veg, sif%veg)
         call map_flow_to_swan(fif%diaveg, sif%diaveg)
         call map_flow_to_swan(fif%veg_stemheight, sif%veg_stemheight)
         ! It seems that SWAN only accepts constant values for diaveg and veg_stemheight
         !
         maxval = -1.0e10
         do i=1, fif%mmax
            do j=1, fif%nmax
               maxval = max(maxval, fif%diaveg(i,j))
            enddo
         enddo
         sr%veg_diamtr = maxval
         maxval = -1.0e10
         do i=1, fif%mmax
            do j=1, fif%nmax
               maxval = max(maxval, fif%veg_stemheight(i,j))
            enddo
         enddo
         sr%veg_height = maxval
      endif
   endif
   if (wavedata%mode == flow_mud_online) then
      write(*,'(4x,a)') 'Mud:'
      write(mudfilnam,'(a,a)')'com-',trim(mudids(1))
      !
      ! Read mud parameters needed by SWAN
      !
      call get_params  (dummy, sr%rhomud, mudfilnam)
      call get_visc (wavedata%time , sr%viscmud, fif%mmax, fif%nmax, mudfilnam)
      !
      ! Read depth from mud-com-file
      ! ASSUMPTIONS:
      ! - Only one mud domain
      ! - Mud grid is identical to the grid of the only water domain
      !
      call get_dep (fif%dpsmud, fif%mmax, fif%nmax, &
                  & mudfilnam)
      !
      ! Map depth to SWAN grid
      !
      call map_flow_to_swan(fif%dpsmud, sif%dpsmud)
      !
      ! Read mud level from mud-com-file
      !
      call get_lev (wavedata%time , &
                  & fif%s1mud, fif%mmax, fif%nmax, &
                  & mudfilnam)
      !
      ! Map mud level to SWAN grid
      !
      call map_flow_to_swan(fif%s1mud, sif%s1mud)
   endif
   !
   ! Deallocate memory swan input fields defined on flow grid
   !
   call dealloc_input_fields(fif, wavedata%mode)

contains

   subroutine map_flow_to_swan(src, dst)
      real, dimension(fif%mmax, fif%nmax), intent(in)    :: src
      real, dimension(sif%mmax, sif%nmax), intent(inout) :: dst

      if (f2s%ext_mapper) then
         call grmap_esmf(f2s%provider_index, src, fif%npts, dst, sif%mmax, sif%nmax, f2s, sg)
      else
         call grmap(src, fif%npts, dst, sif%npts, f2s%ref_table, &
                  & f2s%weight_table, f2s%n_surr_points, iprint)
      endif
   end subroutine map_flow_to_swan
end subroutine get_flow_fields
