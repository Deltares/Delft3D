module m_get_flow_fields
   implicit none
   private
   public :: get_flow_fields
contains
   subroutine get_flow_fields(i_swan, sif, sg, wavedata, sr, flowVelocityType, precice_state)
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
      use m_get_params, only: get_params
      implicit none
!
! Global variables
!
      integer :: i_swan
      integer :: flowVelocityType
      type(input_fields) :: sif ! input fields defined on swan grid
      type(grid) :: sg ! swan grid
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
      !
   !! executable statements -------------------------------------------------------
      if (sr%dom(i_swan)%qextnd(q_bath) > 0) then
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%bed_levels_name, sif%dps)
      end if

      if (sr%dom(i_swan)%qextnd(q_wl) > 0) then
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%water_levels_name, sif%s1)
      end if

      if (sr%dom(i_swan)%qextnd(q_cur) > 0) then
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%flow_velocity_name, sif%u1, sif%v1)
      end if

      if (sr%dom(i_swan)%qextnd(q_wind) >= 1) then
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%wind_velocity_name, sif%windu, sif%windv)
      end if

      if (sr%swveg .and. sr%dom(1)%qextnd(q_veg) >= 1) then
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_stem_density_name, sif%veg)
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_diameter_name, sif%diaveg)
         call precice_read_data(sg%kcs, sif%mmax, sif%nmax, precice_state, precice_state%vegetation_height_name, sif%veg_stemheight)
         ! It seems that SWAN only accepts constant values for diaveg and veg_stemheight
         !
         maxval = -1.0e10
         do i = 1, sif%mmax
            do j = 1, sif%nmax
               maxval = max(maxval, sif%diaveg(i, j))
            end do
         end do
         sr%veg_diamtr = maxval
         maxval = -1.0e10
         do i = 1, sif%mmax
            do j = 1, sif%nmax
               maxval = max(maxval, sif%veg_stemheight(i, j))
            end do
         end do
         sr%veg_height = maxval
      end if
   end subroutine get_flow_fields

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
end module m_get_flow_fields
