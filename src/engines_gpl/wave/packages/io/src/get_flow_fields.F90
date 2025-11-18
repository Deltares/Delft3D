module m_get_flow_fields
   implicit none
   private
   public :: get_flow_fields
contains
   subroutine get_flow_fields(i_flow, i_swan, sif, fg, sg, f2s, wavedata, sr, flowVelocityType, precice_state)
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
      use m_wave_precice_state_t, only: wave_precice_state_t
      implicit none
!
! Global variables
!
      integer :: i_flow
      integer :: i_swan
      integer :: flowVelocityType
      type(input_fields) :: sif ! input fields defined on swan grid
      type(grid) :: fg ! flow grid
      type(grid) :: sg ! swan grid
      type(grid_map) :: f2s ! flow to swn grid mapper
      integer, dimension(:, :), pointer :: covered
      type(wave_data_type) :: wavedata
      type(swan_type) :: sr ! swan input structure
      type(wave_precice_state_t), intent(in) :: precice_state
!
! Local variables
!
      integer :: i, j, precice_index
      integer :: iprint = 0
      real :: alpb = 0.0
      real :: dummy = -999.0
      real :: maxval
      logical :: clbot = .true.
      character(256) :: mudfilnam = ' '
      type(input_fields) :: fif ! input fields defined on flow grid

      interface
         subroutine grmap_esmf(i1, f1, n1, f2, mmax, nmax, f2s, f2g)
            use swan_flow_grid_maps
            integer, intent(in) :: i1
            integer, intent(in) :: n1
            integer, intent(in) :: mmax
            integer, intent(in) :: nmax
            real, dimension(n1), intent(in) :: f1
            real, dimension(mmax, nmax) :: f2
            type(grid_map), intent(in) :: f2s
            type(grid) :: f2g ! f2 grid
         end subroutine grmap_esmf

         subroutine get_var_netcdf(i_flow, wavetime, varname, vararr, mmax, nmax, basename, &
                                 & lastvalidflowfield, kmax, flowVelocityType)
            use wave_data
            integer, intent(in) :: i_flow
            type(wave_time_type) :: wavetime
            character(*), intent(in) :: varname
            real, dimension(mmax, nmax), intent(out) :: vararr
            integer, intent(in) :: mmax
            integer, intent(in) :: nmax
            character(*) :: basename
            integer :: lastvalidflowfield
            integer, optional, intent(in) :: kmax
            integer, optional, intent(in) :: flowVelocityType
         end subroutine
      end interface
      !
   !! executable statements -------------------------------------------------------
      !
      ! Allocate memory swan input fields defined on flow grid
      !
      call alloc_input_fields(fg, fif, wavedata%mode)
      !
      if (sr%dom(i_swan)%qextnd(q_bath) > 0) then
         if (sr%flowgridfile == ' ') then
            !
            ! Read depth from com-file (Delft3d4)
            !
            call get_dep(fif%dps, fif%mmax, fif%nmax, &
                        & fg%grid_name)
            !
            ! Map depth to SWAN grid
            !
            call grmap(fif%dps, fif%npts, &
                      & sif%dps, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
         else
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
            call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%bed_levels_name, sif%dps)
#else
            !
            ! Read depth from netcdf-file
            !
            call get_var_netcdf(i_flow, wavedata%time, 'dps', &
                               & fif%dps, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            !
            ! Map depth to SWAN grid, using ESMF_Regrid weights
            !
            call grmap_esmf(i_flow, fif%dps, fif%npts, &
                           & sif%dps, sif%mmax, sif%nmax, &
                           & f2s, sg)
            !
#endif
         end if
      end if
      !
      ! Read polygons fixed structures
      !
      call dam_cod(fg%x, fg%y, fg%kcs, fg%mmax, fg%nmax)
      !
      if (sr%dom(i_swan)%qextnd(q_wl) > 0) then
         if (sr%flowgridfile == ' ') then
            !
            ! Read water level from com-file (Delft3d4)
            !
            call get_lev(wavedata%time, &
                        & fif%s1, fif%mmax, fif%nmax, &
                        & fg%grid_name)
            !
            ! Map water level to SWAN grid
            !
            call grmap(fif%s1, fif%npts, &
                      & sif%s1, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
         else
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
            call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%water_levels_name, sif%s1)
#else
            !
            ! Read water level from netcdf-file
            !
            call get_var_netcdf(i_flow, wavedata%time, 's1', &
                               & fif%s1, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            !
            ! Map water level to SWAN grid, using ESMF_Regrid weights
            !
            call grmap_esmf(i_flow, fif%s1, fif%npts, &
                           & sif%s1, sif%mmax, sif%nmax, &
                           & f2s, sg)
#endif
         end if
      end if
      !
      if (sr%dom(i_swan)%qextnd(q_cur) > 0) then
         if (sr%flowgridfile == ' ') then
            !
            ! Read velocity from com-file
            !
            call get_cur(wavedata%time, &
                        & fif%kfu, fif%kfv, fif%u1, fif%v1, fif%mmax, fif%nmax, &
                        & fg%kmax, fg%grid_name, fg%layer_model, flowVelocityType, &
                        & fif%dps, fif%s1)
            !
            ! Convert to Cartesian, cell centres
            !
            call flow2wav(fif%u1, fif%v1, &
                         & fg%alfas, fg%guu, fg%gvv, fg%mmax, fg%nmax, fg%kcs, &
                         & fif%kfu, fif%kfv, alpb, clbot)
            !
            ! Map velocity to SWAN grid
            ! NOTE: mapping procedure only updates the part of SWAN grid covered by current FLOW domain
            !
            call grmap(fif%u1, fif%npts, &
                      & sif%u1, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
            call grmap(fif%v1, fif%npts, &
                      & sif%v1, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
         else
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%flow_velocity_name, sif%u1, sif%v1)
#else
            !
            ! Read velocity components from netcdf-file
            !
            if (fg%kmax == 1) then
               call get_var_netcdf(i_flow, wavedata%time, 'u1', &
                                  & fif%u1, fif%mmax, fif%nmax, &
                                  & sr%flowgridfile, wavedata%output%lastvalidflowfield)
               call get_var_netcdf(i_flow, wavedata%time, 'v1', &
                                  & fif%v1, fif%mmax, fif%nmax, &
                                  & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            else
               call get_var_netcdf(i_flow, wavedata%time, 'u1', &
                                  & fif%u1, fif%mmax, fif%nmax, &
                                  & sr%flowgridfile, wavedata%output%lastvalidflowfield, fg%kmax, flowVelocityType)
               call get_var_netcdf(i_flow, wavedata%time, 'v1', &
                                  & fif%u1, fif%mmax, fif%nmax, &
                                  & sr%flowgridfile, wavedata%output%lastvalidflowfield, fg%kmax, flowVelocityType)
            end if
            !
            ! Map velocity components to SWAN grid, using ESMF_Regrid weights
            !
            call grmap_esmf(i_flow, fif%u1, fif%npts, &
                           & sif%u1, sif%mmax, sif%nmax, &
                           & f2s, sg)
            call grmap_esmf(i_flow, fif%v1, fif%npts, &
                           & sif%v1, sif%mmax, sif%nmax, &
                           & f2s, sg)
#endif
         end if
      end if
      !
      if (sr%dom(i_swan)%qextnd(q_wind) >= 1) then
         if (sr%flowgridfile == ' ') then
            !
            ! Read wind from com-file
            !
            call get_wind(wavedata%time, &
                         & fif%windu, fif%windv, fif%mmax, fif%nmax, &
                         & fg%grid_name)
            !
            ! Map wind to SWAN grid
            !
            call grmap(fif%windu, fif%npts, &
                      & sif%windu, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
            call grmap(fif%windv, fif%npts, &
                      & sif%windv, sif%npts, &
                      & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                      & iprint)
         else
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%wind_velocity_name, sif%windu, sif%windv)
#else
            !
            ! Read wind components from netcdf-file
            !
            call get_var_netcdf(i_flow, wavedata%time, 'windx', &
                               & fif%windu, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            call get_var_netcdf(i_flow, wavedata%time, 'windy', &
                               & fif%windv, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            !
            ! Map wind components to SWAN grid, using ESMF_Regrid weights
            !
            call grmap_esmf(i_flow, fif%windu, fif%npts, &
                           & sif%windu, sif%mmax, sif%nmax, &
                           & f2s, sg)
            call grmap_esmf(i_flow, fif%windv, fif%npts, &
                           & sif%windv, sif%mmax, sif%nmax, &
                           & f2s, sg)
#endif
         end if
      end if
      !
      if (sr%swveg .and. sr%dom(1)%qextnd(q_veg) >= 1) then
         if (sr%flowgridfile == ' ') then
            !
            ! There is no vegetation on the Delf3D4-FLOW com file
            !
            write (*, '(a)') "ERROR: trying to read vegetation from Delft3D4-FLOW com-file. Not implemented yet."
            call wavestop(1, "ERROR: trying to read vegetation from Delft3D4-FLOW com-file. Not implemented yet.")
         else
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
            call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_stem_density_name, sif%veg)
            call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_diameter_name, sif%diaveg)
            call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_height_name, sif%veg_stemheight)
#else
            !
            ! Read vegetation parameters from netcdf-file
            !
            call get_var_netcdf(i_flow, wavedata%time, 'rnveg', &
                               & fif%veg, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            call get_var_netcdf(i_flow, wavedata%time, 'diaveg', &
                               & fif%diaveg, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            call get_var_netcdf(i_flow, wavedata%time, 'veg_stemheight', &
                               & fif%veg_stemheight, fif%mmax, fif%nmax, &
                               & sr%flowgridfile, wavedata%output%lastvalidflowfield)
            !
            ! Map vegetation components to SWAN grid, using ESMF_Regrid weights
            !
            call grmap_esmf(i_flow, fif%veg, fif%npts, &
                           & sif%veg, sif%mmax, sif%nmax, &
                           & f2s, sg)
            call grmap_esmf(i_flow, fif%diaveg, fif%npts, &
                           & sif%diaveg, sif%mmax, sif%nmax, &
                           & f2s, sg)
            call grmap_esmf(i_flow, fif%veg_stemheight, fif%npts, &
                           & sif%veg_stemheight, sif%mmax, sif%nmax, &
                           & f2s, sg)
#endif
            ! It seems that SWAN only accepts constant values for diaveg and veg_stemheight
            !
            maxval = -1.0e10
            do i = 1, fif%mmax
               do j = 1, fif%nmax
                  maxval = max(maxval, fif%diaveg(i, j))
               end do
            end do
            sr%veg_diamtr = maxval
            maxval = -1.0e10
            do i = 1, fif%mmax
               do j = 1, fif%nmax
                  maxval = max(maxval, fif%veg_stemheight(i, j))
               end do
            end do
            sr%veg_height = maxval
         end if
      end if
      if (wavedata%mode == flow_mud_online) then
         write (*, '(4x,a)') 'Mud:'
         write (mudfilnam, '(a,a)') 'com-', trim(mudids(1))
         !
         ! Read mud parameters needed by SWAN
         !
         call get_params(dummy, sr%rhomud, mudfilnam)
         call get_visc(wavedata%time, sr%viscmud, fif%mmax, fif%nmax, mudfilnam)
         !
         ! Read depth from mud-com-file
         ! ASSUMPTIONS:
         ! - Only one mud domain
         ! - Mud grid is identical to the grid of the only water domain
         !
         call get_dep(fif%dpsmud, fif%mmax, fif%nmax, &
                     & mudfilnam)
         !
         ! Map depth to SWAN grid
         !
         call grmap(fif%dpsmud, fif%npts, &
                   & sif%dpsmud, sif%npts, &
                   & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                   & iprint)
         !
         ! Read mud level from mud-com-file
         !
         call get_lev(wavedata%time, &
                     & fif%s1mud, fif%mmax, fif%nmax, &
                     & mudfilnam)
         !
         ! Map mud level to SWAN grid
         !
         call grmap(fif%s1mud, fif%npts, &
                   & sif%s1mud, sif%npts, &
                   & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                   & iprint)
      end if
      !
      ! Deallocate memory swan input fields defined on flow grid
      !
      call dealloc_input_fields(fif, wavedata%mode)
   end subroutine get_flow_fields

#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
   subroutine precice_read_data(swan_grid_mask, m_max, n_max, precice_state, field_name, output_field_x, output_field_y)
      use, intrinsic :: iso_c_binding, only: c_double, c_int
      use precision, only: sp
      use m_wave_precice_state_t, only: wave_precice_state_t
      use swan_flow_grid_maps, only: input_fields, grid
      use precice, only: precicef_read_data, precicef_get_data_dimensions
      implicit none(type, external)

      integer, dimension(:, :), intent(in) :: swan_grid_mask
      integer, intent(in) :: m_max
      integer, intent(in) :: n_max
      type(wave_precice_state_t), intent(in) :: precice_state
      character(*), intent(in) :: field_name
      real(kind=sp), dimension(:, :), intent(inout) :: output_field_x
      real(kind=sp), dimension(:, :), optional, intent(inout) :: output_field_y

      integer :: n_vertices, precice_index, i, j
      integer(kind=c_int) :: data_dimension
      real(kind=c_double), dimension(:), allocatable :: data_values

      call precicef_get_data_dimensions(precice_state%swan_mesh_name, field_name, data_dimension, &
                                   len(precice_state%swan_mesh_name), len(field_name))

      if (data_dimension > 1 .and. .not. present(output_field_y)) then
         write (*, '(a)') "ERROR: trying to read vector data from PreCICE without providing both output fields."
         stop
      end if
      n_vertices = size(precice_state%vertex_ids)
      allocate (data_values(n_vertices * data_dimension))
      call precicef_read_data(precice_state%swan_mesh_name, field_name, &
                              n_vertices, precice_state%vertex_ids, 0.0_c_double, data_values, &
                              len(precice_state%swan_mesh_name), len(field_name))
      precice_index = 1
      do j = 1, n_max
         do i = 1, m_max
            if (swan_grid_mask(i, j) /= 0) then
               output_field_x(i, j) = data_values(precice_index)
               if (present(output_field_y)) then
                  output_field_y(i, j) = data_values(precice_index + 1)
               end if
               precice_index = precice_index + data_dimension
            end if
         end do
      end do
   end subroutine precice_read_data
#endif
end module m_get_flow_fields
