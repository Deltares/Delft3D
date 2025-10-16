!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

!> Runs flow steps for a certain period (do computational flowsteps for as long as timeinterval dtrange).
module m_flow_run_sometimesteps
   use m_partitioninfo, only: my_rank, numranks
   use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double
   implicit none

   private
#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
   integer(kind=c_int), parameter :: precice_component_name_length = 2
   character(kind=c_char, len=precice_component_name_length), parameter :: precice_component_name = "fm"
   integer(kind=c_int), parameter :: precice_config_name_length = 21
   character(kind=c_char, len=precice_config_name_length), parameter :: precice_config_name = "../precice_config.xml"
   integer(kind=c_int), parameter :: mesh_name_length = 7
   character(kind=c_char, len=mesh_name_length), parameter :: mesh_name = "fm-mesh"
   integer(kind=c_int), parameter :: max_greeting_length = 34
   integer(kind=c_int), parameter :: greeting_name_length = 8, response_name_length = 8
   character(kind=c_char, len=greeting_name_length), parameter :: greeting_name = "greeting", response_name = "response"

   integer(kind=c_int), dimension(max_greeting_length) :: vertex_ids
   integer(kind=c_int) :: greeting_size
   integer(kind=c_int) :: response_size

   type :: time_steps_t
      real(kind=c_double) :: time_step
      real(kind=c_double) :: precice_max_time_step
   end type time_steps_t
#endif

   public :: flow_run_sometimesteps
#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
   public :: couple_to_greeter_dummy
#endif // defined(HAS_PRECICE_FM_GREETER_COUPLING)
contains

#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
   subroutine couple_to_greeter_dummy()
      use m_alloc, only: realloc
      use precice, only: precicef_create, precicef_get_mesh_dimensions, precicef_initialize, &
         precicef_set_vertices, precicef_read_data, precicef_get_data_dimensions, precicef_get_max_time_step_size

      real(kind=c_double) :: precice_time_step
      integer(kind=c_int) :: mesh_dimensions
      real(kind=c_double), dimension(max_greeting_length * 2) :: mesh_coordinates
      real(kind=c_double), dimension(:), allocatable :: greeting_values
      character(kind=c_char, len=:), allocatable :: converted_greeting_values
      integer(kind=c_int) :: greeting_dimension, response_dimension
      integer :: i

      !! Initialize precice
      !! precice is initialised after mpi ranks are known, however precice's official fortran bindings
      !! do not support passing mpi communicator, so we need to use MPI_COMM_WORLD in the dimr_config.xml
      !! An unofficial extension to precice fortran binding exists and can be evaluated in future
      !! https://github.com/ivan-pi/fortran-module/blob/participant/src/precice_participant_api.F90
      call precicef_create(precice_component_name, precice_config_name, my_rank, numranks, precice_component_name_length, precice_config_name_length)
      call precicef_get_mesh_dimensions(mesh_name, mesh_dimensions, mesh_name_length)
      print *, '[FM] The number of dimensions of the fm-mesh is ', mesh_dimensions

      mesh_coordinates = 0.0_c_double
      call precicef_set_vertices(mesh_name, max_greeting_length, mesh_coordinates, vertex_ids, mesh_name_length)
      call precicef_initialize()

      call precicef_get_data_dimensions(mesh_name, greeting_name, greeting_dimension, mesh_name_length, greeting_name_length)
      call precicef_get_data_dimensions(mesh_name, response_name, response_dimension, mesh_name_length, response_name_length)
      greeting_size = greeting_dimension * max_greeting_length
      response_size = response_dimension * max_greeting_length
      print *, '[FM] greeting dimension: ', greeting_dimension, ' greeting size: ', greeting_size
      print *, '[FM] response dimension: ', response_dimension, ' response size: ', response_size
      call realloc(greeting_values, greeting_size)

      call precicef_get_max_time_step_size(precice_time_step)
      print *, '[FM] max time step: ', precice_time_step
      call precicef_read_data(mesh_name, greeting_name, greeting_size, vertex_ids, precice_time_step, greeting_values, &
                              mesh_name_length, greeting_name_length)

      allocate(character(kind=c_char, len=greeting_size) :: converted_greeting_values)
      do i = 1, greeting_size
         if (int(greeting_values(i)) /= 0) then
            converted_greeting_values(i:i) = char(int(greeting_values(i)), kind=c_char)
         else
            converted_greeting_values(i:i) = ' '
         end if
      end do
      print *, '[FM] message read: ', converted_greeting_values

   end subroutine couple_to_greeter_dummy

   subroutine read_from_greeter_dummy(time_step)
      use m_alloc, only: realloc
      use precice, only: precicef_read_data

      real(kind=c_double), intent(in) :: time_step

      real(kind=c_double), dimension(:), allocatable :: greeting_values
      character(kind=c_char, len=:), allocatable :: converted_greeting_values
      integer :: i

      !! Insert calls to read from another participant using preCICE coupling here, if needed.
      call realloc(greeting_values, greeting_size)
      call precicef_read_data(mesh_name, greeting_name, greeting_size, vertex_ids, time_step, greeting_values, &
                             mesh_name_length, greeting_name_length)
      allocate(character(kind=c_char, len=greeting_size) :: converted_greeting_values)
      do i = 1, greeting_size
         if (int(greeting_values(i)) /= 0) then
            converted_greeting_values(i:i) = char(int(greeting_values(i)), kind=c_char)
         else
            converted_greeting_values(i:i) = ' '
         end if
      end do
      print *, '[FM] message read: ', trim(converted_greeting_values)
   end subroutine read_from_greeter_dummy

   subroutine advance_coupling_greeter_dummy(time_step)
      use precice, only: precicef_is_coupling_ongoing, precicef_advance

      real(kind=c_double), intent(in) :: time_step

      integer :: coupling_ongoing
      !! Insert calls to write to another participant using preCICE coupling here, if needed.
      !! Advance preCICE by the minimum of its suggested timestep and dtuser
      call precicef_is_coupling_ongoing(coupling_ongoing)
      if (coupling_ongoing /= 0) then
         !! Calling writeData(...) is forbidden if coupling is not ongoing, because the data you are trying to write will not be used anymore.
         call write_to_greeter_dummy()
         call precicef_advance(time_step)
      end if
   end subroutine advance_coupling_greeter_dummy

   function calculate_time_steps() result(time_steps)
      use precice, only: precicef_get_max_time_step_size
      use m_flowtimes, only: dts
      type(time_steps_t) :: time_steps

      call precicef_get_max_time_step_size(time_steps%precice_max_time_step)
      time_steps%time_step = min(dts, time_steps%precice_max_time_step)
   end function calculate_time_steps

   subroutine write_to_greeter_dummy()
      use m_alloc, only: realloc
      use m_flowtimes, only: time1
      use precice, only: precicef_write_data

      character(len=50) :: temp_string
      integer :: i, string_length
      real(kind=c_double), dimension(:), allocatable :: converted_response_values

      ! Create response message with current time
      write(temp_string, '(A,F12.6)') 'FM: My current time is ', time1
      ! Get actual length of the string (without trailing spaces)
      string_length = len_trim(temp_string)
      ! Ensure we don't exceed max_greeting_length
      string_length = min(string_length, max_greeting_length)

      call realloc(converted_response_values, response_size)
      converted_response_values = 0.0_c_double

      ! Direct conversion to ASCII values
      do i = 1, string_length
         converted_response_values(i) = real(iachar(temp_string(i:i)), kind=c_double)
      end do

      call precicef_write_data(mesh_name, response_name, response_size, vertex_ids, converted_response_values, &
                                mesh_name_length, response_name_length)

      print *, '[FM] Response sent: ', temp_string(1:string_length)

   end subroutine write_to_greeter_dummy

   subroutine finalize_greeter_dummy_coupling()
      use precice, only: precicef_finalize
      call precicef_finalize()
   end subroutine finalize_greeter_dummy_coupling

#endif // defined(HAS_PRECICE_FM_GREETER_COUPLING)

   subroutine flow_run_sometimesteps(dtrange, iresult) ! do computational flowsteps for as long as timeinterval dtrange
      use m_flow_init_usertimestep, only: flow_init_usertimestep
      use m_flow_finalize_usertimestep, only: flow_finalize_usertimestep
      use precision, only: dp
      use m_flow_single_timestep, only: flow_single_timestep
      use m_flowtimes, only: time1, tstop_user, time_user
      use dfm_error, only: dfm_genericerror, dfm_noerr
      use m_laterals, only: reset_outgoing_lat_concentration, finish_outgoing_lat_concentration, apply_transport_is_used, &
                            qqlat, qplat, get_lateral_volume_per_layer, &
                            lateral_volume_per_layer, distribute_lateral_discharge

      real(kind=dp), intent(in) :: dtrange
      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.
      integer :: key

#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
      type(time_steps_t) :: time_steps
#endif

      real(kind=dp) :: timetarget

      if (apply_transport_is_used) then
         call reset_outgoing_lat_concentration()
         call distribute_lateral_discharge(qplat, qqlat)
      end if

      iresult = DFM_GENERICERROR
      if (dtrange < 0) then
         timetarget = time1 + epsilon(1.0_dp) ! dtrange < 0 means: auto pick a *single* timestep. Enforce this with a target time *just* larger than current time.
      else
         timetarget = time1 + dtrange
      end if

      timetarget = min(timetarget, tstop_user)

      do while (time1 < timetarget) ! nb, outside flow_singletimestep, time0=time1 !
         !! INIT only in case of new user timestep
         if (time1 >= time_user) then
            call flow_init_usertimestep(iresult)

            if (iresult /= DFM_NOERR) then
               goto 888
            end if
         end if

         !! RUN actual SINGLE computational timestep
         call flow_single_timestep(key, iresult)
         if (iresult /= DFM_NOERR) then
            goto 888
         end if

#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
         time_steps = calculate_time_steps()
         call read_from_greeter_dummy(time_steps%precice_max_time_step)
         call advance_coupling_greeter_dummy(time_steps%time_step)
#endif
    !! FINALIZE only when a time_user is finished
         if (time1 >= time_user) then
            call flow_finalize_usertimestep(iresult)

            if (iresult /= DFM_NOERR) then
               goto 888
            end if
         end if

      end do
#if defined(HAS_PRECICE_FM_GREETER_COUPLING)
         call finalize_greeter_dummy_coupling()
#endif
      if (apply_transport_is_used) then
         call finish_outgoing_lat_concentration(dtrange)
         call get_lateral_volume_per_layer(lateral_volume_per_layer)
      end if

      iresult = DFM_NOERR
      return ! Return with success.

888   continue
   end subroutine flow_run_sometimesteps

end module m_flow_run_sometimesteps
