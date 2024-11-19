!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define no_warning_unused_variable(x) associate( x => x ); end associate

module bmi
   use m_update_zcgen_widths_and_heights, only: update_zcgen_widths_and_heights
   use m_write_some_final_output, only: write_some_final_output
   use iso_c_binding
   use unstruc_api
   use m_gui ! this should be removed when jaGUI = 0 by default

   use m_partitioninfo
   use m_flow
   use m_waves
   use m_ship
   use m_wind
   use m_flowgeom
   use network_data
   use fm_external_forcings_data
   use m_partitioninfo
   use m_sobekdfm
   use m_transport
   use m_xbeach_data
   use dfm_error
   use m_vegetation
   use m_sediment
   use m_integralstats
   use gridoperations
   use unstruc_model
   use m_fm_erosed, only: taucr, tetacr
   use m_heatfluxes, only: Qsunmap
   use m_longculverts
   use m_nearfield
   use m_VolumeTables, only: vltb, vltbonlinks, ndx1d
   use m_update_land_nodes
   use m_find_name, only: find_name

   implicit none

   ! Define some global constants

   ! We can't return an array of pointers with an undertimened length so
   ! we fix it to something, in this case 100.

   character(*), parameter :: PREFIX = "FM"
   !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
   integer(c_int) :: MAXNAMES = 100
   integer(c_int), bind(C, name="MAXSTRLEN") :: MAXSTRLEN = 1024
   !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
   integer(c_int), bind(C, name="MAXDIMS") :: MAXDIMS = 6
   !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

   ! TODO, get rid of these custom variables....
   real(c_double), target, allocatable, save :: uabs(:)
   real(c_double), target, allocatable, save :: froude(:)
   real(c_double), target, allocatable, save :: sed1(:)
   real(c_double), target, allocatable, save :: const_t(:, :) !< Placeholder array for returning constituents (transposed: dim(numconst,ndkx))
   real(c_double), target, allocatable, save :: tem1Surf(:)
   real(c_double), target, allocatable, save :: TcrEro(:, :)
   real(c_double), target, allocatable, save :: TcrSed(:, :)
   integer, private :: iconst

   integer(c_int), parameter :: var_count_compound = 11 ! pumps, weirs, orifices, gates, generalstructures, culverts, sourcesinks, dambreak, observations, crosssections, laterals ! TODO: AvD: temp, as long as this is not templated
contains

   subroutine main() bind(C, name="main")
      !DEC$ ATTRIBUTES DLLEXPORT :: main
      ! Somehow intel fortran compiler expects a main routine in the dll, it is required since interactor is used (and win calls)
   end subroutine main

!> \defgroup modelinformation Model Information Functions
!! \{

!> Fills a string array with the model's input variable names as "long variable names" from the CSDMS Standard Names.
!! NOTE: not implemented yet, will return a DFM_NOTIMPLEMENTED error.
   function get_input_var_names(names) bind(C, name="get_input_var_names") result(c_istat)
      type(c_ptr), dimension(:), intent(out) :: names !< Array of C-pointers, will contain pointers to C-compatible strings upon return.
      integer(c_int) :: c_istat !< Integer status code indicating success (zero) or failure (nonzero)

      integer :: i_var, var_count
      character(kind=c_char, len=1), dimension(:, :), pointer :: cnames
      character(len=MAXSTRLEN) :: fname

      c_istat = DFM_NOTIMPLEMENTED
      return

      ! NOTE: code below is working example code for returning a BMI 2.0
      ! compliant array of names.

      var_count = 1

      allocate (cnames(MAXSTRLEN, var_count))
      do i_var = 1, var_count
         fname = 'test_get_input_var_names' ! TODO: UNST-7403: implement this.
         cnames(:, i_var) = string_to_char_array(trim(fname), len(trim(fname)))
         names(i_var) = c_loc(cnames(:, i_var))
      end do

   end function get_input_var_names

!> Returns a string array of the model's output variable names as "long variable names" from the CSDMS Standard Names.
!! NOTE: not implemented yet, will return a DFM_NOTIMPLEMENTED error.
   subroutine get_output_var_names(names) bind(C, name="get_output_var_names")
      type(c_ptr), dimension(:), intent(out) :: names !< Array of C-pointers, will contain pointers to C-compatible strings upon return.
      integer(c_int) :: c_istat !< Integer status code indicating success (zero) or failure (nonzero)

      integer :: i_var, var_count
      character(kind=c_char, len=1), dimension(:, :), pointer :: cnames
      character(len=MAXSTRLEN) :: fname

      c_istat = DFM_NOTIMPLEMENTED
      return

      ! NOTE: code below is working example code for returning a BMI 2.0
      ! compliant array of names.

      var_count = 1

      allocate (cnames(MAXSTRLEN, var_count))
      do i_var = 1, var_count
         fname = 'test_get_output_var_names' ! TODO: UNST-7403: implement this.
         cnames(:, i_var) = string_to_char_array(trim(fname), len(trim(fname)))
         names(i_var) = c_loc(cnames(:, i_var))
      end do

   end subroutine get_output_var_names

!> Returns a static attribute (i.e. an attribute that does not change
!! from one model application to the next) of the model (as a string)
!! when passed any attribute name from the following list:
!! * model_name
!! * version      (e.g. 2.0.1)
!! * author_name
!! * grid_type
!! * time_step_type
!! * step_method   (explicit, implicit, semi_implicit, iterative)
   subroutine get_attribute(c_att_name, c_att_value) bind(C, name="get_attribute")
!DEC$ ATTRIBUTES DLLEXPORT :: get_attribute
      use dflowfm_version_module
      character(kind=c_char), intent(in) :: c_att_name(MAXSTRLEN) !< Attribute name as C-delimited character string.
      character(kind=c_char), intent(out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.

      character(len=strlen(c_att_name)) :: att_name
      character(len=MAXSTRLEN) :: att_value

      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      select case (att_name)
      case ('model_name')
         att_value = product_name
      case ('version')
         att_value = dflowfm_version_full
      case ('author_name')
         att_value = company
      case ('grid_type')
         att_value = 'unstructured_grid'
      case ('time_step_type')
         att_value = 'adaptive'
      case ('step_method')
         att_value = 'semi_implicit'
      end select

      c_att_value = string_to_char_array(trim(att_value), len(trim(att_value)))
   end subroutine get_attribute

!! modelinformation
!! \}

!> \defgroup modelcontrol Model Control Functions
!! \{
!!

!> The initialize() function accepts a string argument that
!! gives the name (and path) of its "main input file", called
!! a configuration file. This function should perform all tasks
!! that are to take place before entering the model's time loop.
   integer(c_int) function initialize(c_config_file) result(c_iresult) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT :: initialize

      use iso_c_binding, only: c_char

      use unstruc_model
      use unstruc_files
      use m_partitioninfo
      use check_mpi_env
#ifdef HAVE_MPI
      use mpi
#endif

      character(kind=c_char), intent(in) :: c_config_file(MAXSTRLEN)
      character(len=strlen(c_config_file)) :: config_file

      ! Extra local variables
      integer :: inerr ! number of the initialisation error
      logical :: mpi_initd

      c_iresult = 0 ! TODO: is this return value BMI-compliant?
      jampi = 0
      numranks = 1
      my_rank = 0
      ja_mpi_init_by_fm = 0
#ifdef HAVE_MPI

      ! Check if MPI is already initialized
      call mpi_initialized(mpi_initd, inerr)
      if (mpi_initd) then
         ja_mpi_init_by_fm = 0
         if (inerr == 0) then
            jampi = 1
         end if
      else
         ! Preparations for calling mpi_init:
         ! When using IntelMPI, mpi_init will cause a crash if IntelMPI is not
         ! installed. Do not call mpi_init in a sequential computation.
         ! Check this via the possible environment parameters.
         jampi = merge(1, 0, running_in_mpi_environment())

         if (jampi == 1) then
            ja_mpi_init_by_fm = 1
            call mpi_init(inerr)
            if (inerr /= 0) then
               jampi = 0
            end if
         end if
      end if

      if (jampi == 1) then
         call mpi_comm_rank(DFM_COMM_DFMWORLD, my_rank, inerr)
         call mpi_comm_size(DFM_COMM_DFMWORLD, numranks, inerr)
      end if

      if (numranks <= 1) then
         jampi = 0
      end if

      !   make domain number string as soon as possible
      write (sdmn, '(I4.4)') my_rank

#endif

      ! do this until default has changed
      jaGUI = 0

      ! Store the name
      config_file = char_array_to_string(c_config_file, strlen(c_config_file))

      ! Now we can initialize with the config_file

      ! It would be great if there would be 1 init function that takes an optional configuration file name

      ! TODO: check why these are needed to avoid a segfault
      KNX = 8
      MXB = 10
      MAXLAN = 500
      MAXPOL = MAXLAN

      !call start()
      !call resetFullFlowModel()
      !call loadmodel(config_file)
      !call init_core() ! All done in inidat()

      call INIDAT()
      call api_loadmodel(config_file)

      !PETSC must be called AFTER reading the mdu file, so the icgsolver option is known to startpetsc
#ifdef HAVE_PETSC
      call startpetsc()
#endif

      c_iresult = flowinit()

      time_user = tstart_user

      ! Just terminate if we get an error....
      ! if (inerr > 0) stop
      ! initialize = 0
   end function initialize

!> Setting up the logger.
   subroutine set_logger_c_callback(c_msg_callback) bind(C, name="set_logger_c_callback")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_logger_c_callback
      use MessageHandling
      use iso_c_binding
      type(c_funptr), value :: c_msg_callback !< Set a callback that will be called with new messages
      call set_logger(c_msg_callback)
      call mess(LEVEL_WARN, "callback initialized")
   end subroutine set_logger_c_callback

   subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string
      use iso_c_binding, only: c_char
      use dflowfm_version_module, only: dflowfm_version_full
      character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)
      character(len=MAXSTRLEN) :: name
      name = dflowfm_version_full
      c_version_string = string_to_char_array(trim(name), len(trim(name)))
   end subroutine get_version_string

!! SH: Temporary routine to be able to run both DFlowFM/Wave and 1d2d.         !!
!!     To be removed when time step handling is fixed, see issue DELFT3DFM-604 !!
!!     For now, in case of 1d2d, de-activate the time step checks              !!
   subroutine deactivate_time_step_checking() bind(C, name="deactivate_time_step_checking")
      !DEC$ ATTRIBUTES DLLEXPORT :: deactivate_time_step_checking
      use dfm_error
      do_check_bmi_timestep = .false.
   end subroutine deactivate_time_step_checking

!! USER TIMESTEP !!
!! NOTE: for greater flexibility the time step routines below are split into three
!! init_, run_ and finalize_ routines. Make sure to call all of them in proper order.

!> Initializes a 'user' timestep (outer time level, for meteo updates and I/O).
!!
!! The actual time-stepping will have to be done by dfm_init/run/finalize_computational_timestep.
!! @see dfm_init_computational_timestep
   function dfm_init_user_timestep(timetarget) result(iresult) bind(C, name="dfm_init_user_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_init_user_timestep
      use m_flowtimes, only: time_user, dt_user
      use dfm_error
      use MessageHandling
      real(c_double), intent(in) :: timetarget !< Target time. For now, this is assumed to be equal to upcoming next user time. If not, errorstatus returned.
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.
      character(len=MAXSTRLEN) :: msg

      iresult = DFM_NOERR

      if (do_check_bmi_timestep .and. timetarget - time_user /= dt_user) then ! We don't support yet a changing dt_user (may affect input/output, meteo, etc.)
         iresult = DFM_INVALIDTARGETTIME
         write (msg, '(a,g16.9,a,g16.9,a)') 'Mismatch of requested step (', timetarget - time_user, ') and the specified user timestep (', dt_user, ').'
         call mess(LEVEL_WARN, msg)
!      goto 888
      end if

      call flow_init_usertimestep(iresult)

      return

888   continue

   end function dfm_init_user_timestep

!> Runs the actual 'user' timestep (outer time level, preceded by meteo updates and to be followed by I/O).
   function dfm_run_user_timestep() result(iresult) bind(C, name="dfm_run_user_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_run_user_timestep
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.
      integer :: dummykey

      call flow_run_usertimestep(dummykey, iresult)
   end function dfm_run_user_timestep

!> Finalizes a 'user' timestep (outer time level, typically timers and I/O).
   function dfm_finalize_user_timestep() result(iresult) bind(C, name="dfm_finalize_user_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_finalize_user_timestep
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.

      call flow_finalize_usertimestep(iresult)
   end function dfm_finalize_user_timestep

!! COMPUTATIONAL TIMESTEP !!
!! NOTE: for greater flexibility the time step routines below are split into three
!! init_, run_ and finalize_ routines. Make sure to call all of them in proper order.

!> Initializes a single computational timestep.
   function dfm_init_computational_timestep(timetarget, dtpredict) result(iresult) bind(C, name="dfm_init_computational_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_init_computational_timestep
      use m_flowtimes, only: dts
      use dfm_error
      use m_sobekdfm
      use MessageHandling
      real(c_double), intent(in) :: timetarget !< Target time, resulting timestep may (will generally) be smaller. For now, this is assumed to be equal to upcoming next user time. If not, errorstatus returned.
      real(c_double), intent(out) :: dtpredict !< The predicted computational timestep, based on stability criteria. Pass this value (or smaller) on to run_computational_timestep.
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.
      character(len=MAXSTRLEN) :: msg

      iresult = DFM_NOERR

      if (do_check_bmi_timestep .and. timetarget /= time_user) then ! We don't yet support another targettime than upcoming time_user
         iresult = DFM_INVALIDTARGETTIME
         write (msg, '(a,g16.9,a,g16.9,a)') 'Mismatch of requested step (', timetarget, ') and the specified user timestep (', time_user, ').'
         call mess(LEVEL_WARN, msg)
!      goto 888
      end if

      if (time0 + dts > tstop_user) then
         dts = tstop_user - time0
      end if

      call flow_init_single_timestep(iresult)
      dtpredict = dts

      ! set logical indicating a new time step is performed for Dflow1D DflowFM 1d2d coupling
      sbkdfm_new_timestep = .true.

      return

888   continue

   end function dfm_init_computational_timestep

!> Runs the actual single computational timestep.
!!
!! Should be preceded by an init_computational_timestep, and followed by finalize_computational_timestep.
   function dfm_run_computational_timestep(dtactual) result(iresult) bind(C, name="dfm_run_computational_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_run_computational_timestep
      use m_flowtimes, only: dts
      real(c_double), intent(inout) :: dtactual !< The actual timestep to be used (may have changed upon return, if time setbacks have occurred)
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.

      integer :: dummykey

      dts = dtactual

      call flow_run_single_timestep(dummykey, iresult) ! TODO: AvD/ JN: do we need a nonzero iresult on SETBACK/other errors? YES

      dtactual = dts

   end function dfm_run_computational_timestep

!> Finalizes a single computational timestep.
!!
!! Should be called immediately after a run_computational_timestep.
   function dfm_finalize_computational_timestep() result(iresult) bind(C, name="dfm_finalize_computational_timestep")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_finalize_computational_timestep
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.

      call flow_finalize_single_timestep(iresult)
   end function dfm_finalize_computational_timestep

   integer function update(dt) bind(C, name="update")
      !DEC$ ATTRIBUTES DLLEXPORT :: update
      use messagehandling
      use iso_c_binding, only: c_double
      real(c_double), value, intent(in) :: dt

      integer :: ierr
      ! The time loop seems to be located in unstruc->flow
      ! It is important that we can simulate up to a time set from the outside
      ! We might have to set time_user or dt_user

      call flow_run_sometimesteps(dt, ierr)

      update = ierr
   end function update

   subroutine update_until(t) bind(C, name="update_until")
      use iso_c_binding, only: c_double
      real(c_double), intent(in) :: t
      no_warning_unused_variable(t)
      ! Calls update(t-tnow)
   end subroutine update_until

   integer function finalize() bind(C, name="finalize")
      !DEC$ ATTRIBUTES DLLEXPORT :: finalize
      use m_partitioninfo

      call write_some_final_output()

      if (jampi == 1) then
!        finalize before exit
         call partition_finalize()
      end if

      call flowfinalize()

      finalize = 0

   end function finalize

   ! Attributes

   subroutine get_start_time(t) bind(C, name="get_start_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
      use m_flowtimes
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t

      t = tstart_user
   end subroutine get_start_time

   subroutine get_end_time(t) bind(C, name="get_end_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time

      use m_flowtimes
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t

      t = tstop_user
   end subroutine get_end_time

   subroutine get_time_step(dt) bind(C, name="get_time_step")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step

      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: dt

      dt = dt_user
   end subroutine get_time_step

   subroutine get_current_time(t) bind(C, name="get_current_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time

      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t

      t = time1
   end subroutine get_current_time

   subroutine dfm_compute_1d2d_coefficients() bind(C, name="dfm_compute_1d2d_coefficients")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_compute_1d2d_coefficients

      call compute_1d2d_coefficients()

   end subroutine dfm_compute_1d2d_coefficients

   subroutine get_time_units(unit) bind(C, name="get_time_units")
      ! returns unit string for model time, e.g. 'days since 1970-01-01'
      character(kind=c_char), intent(in) :: unit(:)
      no_warning_unused_variable(unit)
   end subroutine get_time_units

   subroutine get_n_attributes(n) bind(C, name="get_n_attributes")
      use iso_c_binding, only: c_char, c_ptr, c_loc, c_int, c_null_char
      integer(c_int), intent(out) :: n
      n = 0
      ! expose attributes, properties, parameters that are settable by users or can be used for calibration
      ! This is not part of bmi, but it allows us to easily loop over attributes:
      ! get_n_attributes(n)
      ! do i=1,n
      !  get_attribute_name(i, name)
      !  ! dosomething with name
      ! end
   end subroutine get_n_attributes

   subroutine get_attribute_name(i, c_att_name) bind(C, name="get_attribute_name")
      use iso_c_binding, only: c_char, c_ptr, c_loc, c_int, c_null_char
      integer(c_int), intent(in) :: i
      character(kind=c_char), intent(out) :: c_att_name(MAXSTRLEN)
      character(len=MAXSTRLEN) :: name
      name = 'some attribute name'
      c_att_name = string_to_char_array(trim(name), len(trim(name)))
      ! get name of attribute i
      no_warning_unused_variable(i)
   end subroutine get_attribute_name

   subroutine get_attribute_type(c_att_name, c_type) bind(C, name="get_attribute_type")
      use iso_c_binding, only: c_char, c_ptr, c_null_char
      character(kind=c_char), intent(in) :: c_att_name(*)
      character(kind=c_char), intent(out) :: c_type(MAXSTRLEN)
      ! Use one of the following types
      ! BMI datatype        C datatype        NumPy datatype
      ! BMI_STRING          char*             S<
      ! BMI_INT             int               int16
      ! BMI_DOUBLE          double            float64

      ! The fortran name of the attribute name
      character(len=strlen(c_att_name)) :: att_name
      character(len=MAXSTRLEN) :: type_name
      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      ! Lookup the type of the variable

      type_name = 'BMI_INT'
      c_type = string_to_char_array(trim(type_name), len(trim(type_name)))
   end subroutine get_attribute_type

   subroutine get_double_attribute(c_att_name, value) bind(C, name="get_double_attribute")
      use iso_c_binding, only: c_double, c_char
      character(kind=c_char), intent(in) :: c_att_name(*)
      real(c_double), intent(out) :: value

      ! The fortran name of the attribute name
      character(len=strlen(c_att_name)) :: att_name
      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      ! Look up the value of att_name
      value = -1.0d0
   end subroutine get_double_attribute

   subroutine get_int_attribute(c_att_name, value) bind(C, name="get_int_attribute")
      use iso_c_binding, only: c_double, c_char
      character(kind=c_char), intent(in) :: c_att_name(*)
      integer(c_int), intent(out) :: value

      ! The fortran name of the attribute name
      character(len=strlen(c_att_name)) :: att_name
      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      ! Look up the value of att_name
      value = -1
   end subroutine get_int_attribute

   subroutine get_string_attribute(c_att_name, c_value) bind(C, name="get_string_attribute")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_string_attribute

      use iso_c_binding, only: c_char
      use m_flowtimes

      character(kind=c_char), intent(in) :: c_att_name(*)
      character(kind=c_char), intent(out) :: c_value(MAXSTRLEN)

      ! The fortran name of the attribute name
      character(len=strlen(c_att_name)) :: att_name
      character(len=MAXSTRLEN) :: value
      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      ! Lookup the type of the variable

      select case (att_name)
      case ("refdat")
         value = refdat
      case default
         value = ''
      end select

      c_value = string_to_char_array(trim(value), len(trim(value)))
   end subroutine get_string_attribute

! Variables
   subroutine get_var_count(c_var_count) bind(C, name="get_var_count") ! non-BMI
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_count

      use iso_c_binding, only: c_ptr, c_int

      integer(c_int), intent(out) :: c_var_count
      include "bmi_get_var_count.inc"
      ! plus extra local vars
      c_var_count = var_count + var_count_compound + numconst + 12
   end subroutine get_var_count

   subroutine get_var_name(var_index, c_var_name) bind(C, name="get_var_name")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_name

      use iso_c_binding, only: c_char, c_int

      integer(kind=c_int), value, intent(in) :: var_index
      character(kind=c_char), intent(out) :: c_var_name(MAXSTRLEN)
      integer :: iloc

      character(MAXSTRLEN) :: var_name

      include "bmi_get_var_count.inc"

      include "bmi_get_var_name.inc"

      ! Var ordering:
      ! 0:var_count-1 = autogenerated
      ! var_count:var_count+var_count_compound-1 = compound types (pumps, weirs, orifices, gates, generalstructures, culverts, sourcesinks, dambreak, observations, crosssections, laterals)
      ! var_count+var_count_compound:var_count+var_count_compound+numconst-1 = constituents (tracers, salt, etc.)
      ! var_count+var_count_compound+numconst:end = remaining hand-crafted variables

      select case (var_index)
      case (var_count + 0)
         var_name = "pumps"
      case (var_count + 1)
         var_name = "weirs"
      case (var_count + 2)
         var_name = "orifices"
      case (var_count + 3)
         var_name = "gates"
      case (var_count + 4)
         var_name = "generalstructures"
      case (var_count + 5)
         var_name = "culverts"
      case (var_count + 6)
         var_name = "sourcesinks"
      case (var_count + 7)
         var_name = "dambreak"
      case (var_count + 8)
         var_name = "observations"
      case (var_count + 9)
         var_name = "crosssections"
      case (var_count + 10)
         var_name = "laterals"
      end select

      if (var_index >= var_count + var_count_compound .and. var_index < var_count + var_count_compound + numconst) then
         var_name = const_names(var_index - var_count - var_count_compound + 1)
      end if

      iloc = var_index - (var_count + var_count_compound + numconst) + 1
      select case (iloc)
      case (1)
         var_name = "uabs"
      case (2)
         var_name = "sed1"
      case (3)
         var_name = "netelemnode"
      case (4)
         var_name = "flowelemnode"
      case (5)
         var_name = "flowelemnbs"
      case (6)
         var_name = "flowelemlns"
      case (7)
         var_name = "flowelemcontour_x"
      case (8)
         var_name = "flowelemcontour_y"
      case (9)
         var_name = "kn"
      case (10)
         var_name = "TcrEro"
      case (11)
         var_name = "TcrSed"
      case (12)
         var_name = "tem1Surf"
      end select
      c_var_name = string_to_char_array(trim(var_name), len(trim(var_name)))

   end subroutine get_var_name

   subroutine get_var_type(c_var_name, c_type) bind(C, name="get_var_type")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type

      character(kind=c_char), intent(in) :: c_var_name(*)
      character(kind=c_char), intent(out) :: c_type(MAXSTRLEN)
      character(len=MAXSTRLEN) :: type_name, var_name

      ! Use one of the following types
      ! BMI datatype        C datatype        NumPy datatype
      ! BMI_STRING          char*             S<
      ! BMI_INT             int               int16
      ! BMI_LONG            long int          int32
      ! BMI_FLOAT           float             float32
      ! BMI_DOUBLE          double            float64
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      include "bmi_get_var_type.inc"

      select case (var_name)
      case ("netelemnode")
         type_name = "int"
      case ("flowelemnode", "flowelemnbs", "flowelemlns")
         type_name = "int"
      case ("flowelemcontour_x", "flowelemcontour_y")
         type_name = "double"
      case ("TcrEro", "TcrSed", "tem1Surf")
         type_name = "double"
      case ('vltb')
         type_name = "type(t_voltable)"
      case ('vltbOnLinks')
         type_name = "type(t_voltable)"
      case ('network')
         type_name = "type(t_network)"
      end select

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         type_name = "double"
      end if

      c_type = string_to_char_array(trim(type_name), len(trim(type_name)))

   end subroutine get_var_type

!> Returns the topological location on the unstructured grid of the requested variable.
!! Possible return values are: "face", "edge", "node", or "" when location does not apply.
!! In line with UGRID conventions.
   subroutine get_var_location(c_var_name, c_location) bind(C, name="get_var_location")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_location

      character(kind=c_char), intent(in) :: c_var_name(*)
      character(kind=c_char), intent(out) :: c_location(MAXSTRLEN)
      character(len=MAXSTRLEN) :: location_name, var_name

      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      include "bmi_get_var_location.inc"

      !select case(var_name)
      !   ! list of additional non-generated quantities.
      !case("netelemnode")
      !    type_name = "int"
      !end select

      c_location = string_to_char_array(trim(location_name), len(trim(location_name)))

   end subroutine get_var_location

   subroutine get_var_units(c_var_name, unit) bind(C, name="get_var_units")
      character(kind=c_char), intent(in) :: c_var_name(:)
      character(kind=c_char), intent(out) :: unit(:)
      no_warning_unused_variable(c_var_name)
      unit = ''
   end subroutine get_var_units

!> Returns the rank of a variable, i.e., its dimensionality.
!!
!! rank = count(shape)
!! @see get_var_shape
   subroutine get_var_rank(c_var_name, rank) bind(C, name="get_var_rank")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank

      use iso_c_binding, only: c_int, c_char

      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(out) :: rank

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      include "bmi_get_var_rank.inc"

      select case (var_name)
      case ("netelemnode")
         rank = 2
      case ("flowelemnode", "flowelemnbs", "flowelemlns", "flowelemcontour_x", "flowelemcontour_y")
         rank = 2
      case ("pumps", "weirs", "orifices", "gates", "generalstructures", "culverts", "sourcesinks", "dambreak", "observations", "crosssections", "laterals") ! Compound vars: shape = [numobj, numfields_per_obj]
         rank = 2
      case ("TcrEro", "TcrSed")
         rank = 2
      case ("tem1Surf")
         rank = 1
      end select

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         rank = 1
         return
      end if
   end subroutine get_var_rank

!> Returns the shape of a variable, i.e., an array with length equal to this variables's rank.
!! NOTE: the reported shape is in C-compatible row-major order.
!!
!! count(shape) = rank
!! @see get_var_rank
   subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape

      use iso_c_binding, only: c_int, c_char, c_loc

      use m_flowgeom
      use network_data
      use m_observations_data, only: numobs, nummovobs, MAXNUMVALOBS2D, MAXNUMVALOBS3D, MAXNUMVALOBS3Dw
      use m_monitoring_crosssections, only: ncrs, maxnval
      use m_laterals, only: num_layers, numlatsg
      use unstruc_channel_flow, only: network
      use m_transport, only: NAMLEN, NUMCONST
      use m_laterals, only: numlatsg, nlatnd

      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(inout) :: shape(MAXDIMS)

      character(len=strlen(c_var_name)) :: var_name

      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      shape = (/0, 0, 0, 0, 0, 0/)

      ! NOTE: report the shape below in row-major order (so, C-style, not FORTRAN-style)
      select case (var_name)
      case ("netelemnode")
         shape(1) = nump1d2d
         shape(2) = get_net_elem_max_nodes()
         return
      case ("flowelemnode")
         shape(1) = ndx2d
         shape(2) = get_flow_elem_max_nodes()
         return
      case ("flowelemnbs", "flowelemlns")
         shape(1) = ndx
         shape(2) = get_flow_elem_max_nbs()
         return
      case ("flowelemcontour_x", "flowelemcontour_y")
         shape(1) = ndx
         shape(2) = get_flow_elem_max_contour()
         return
      case ("TcrEro", "TcrSed")
         shape(1) = ndx
         shape(2) = stmpar%lsedtot
         return
      case ("")
         shape(1) = ndx
         return

! Compounds:
      case ("pumps")
         shape(1) = npumpsg
         shape(2) = 1
         return
      case ("weirs")
         shape(1) = nweirgen + network%sts%numWeirs
         shape(2) = 2
         return
      case ("orifices")
         shape(1) = network%sts%numOrifices
         shape(2) = 2
         return
      case ("gates")
         shape(1) = ngategen
         shape(2) = 5
         return
      case ("generalstructures")
         shape(1) = ngenstru
         shape(2) = 5
      case ("culverts")
         shape(1) = network%sts%numCulverts
         shape(2) = 1
      case ("sourcesinks")
         shape(1) = numsrc
         shape(2) = 3
         return
      case ("observations")
         shape(1) = numobs + nummovobs
         shape(2) = MAXNUMVALOBS2D + MAXNUMVALOBS3D + MAXNUMVALOBS3Dw
         return
      case ("crosssections")
         shape(1) = ncrs
         shape(2) = maxnval
         return
      case ("laterals")
         shape(1) = numlatsg
         shape(2) = 1
         return
      case ('vltb')
         shape(1) = ndx1d
      case ('vltbOnLinks')
         shape(1) = 2
         shape(2) = ndx1d
      case ('network')
         shape(1) = 1

         ! Array pointers:
      case ("geometry/xcc", "geometry/ycc", "field/water_depth", "geometry/kbot", "geometry/ktop")
         shape(1) = ndx
         return
      case ("geometry/z_level", "field/velocity_x", "field/velocity_y", "field/rho")
         shape(1) = ndkx
         return
      case ("field/constituents")
         shape(1) = numconst
         shape(2) = ndkx
         return
      case ("constituents_names")
         shape(1) = NUMCONST
         shape(2) = NAMLEN
         return
      case ("runid")
         shape(1) = 1
         shape(2) = len(md_ident)
         return
      end select

      include "bmi_get_var_shape.inc"

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         shape(1) = ndkx
         return
      end if

   end subroutine get_var_shape

   subroutine get_2d_int(c_var_name, xptr) bind(C, name="get_2d_int")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_2d_int

      use iso_c_binding
      use m_flow
      use m_flowgeom
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(out) :: xptr

      integer(c_int), target, allocatable, save :: x(:, :)

      integer :: i, k
      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name

      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case ("netelemnode")
         if (allocated(x)) deallocate (x)
         ! Deallocate if already allocated.
         allocate (x(nump, get_net_elem_max_nodes()))
         ! initialize to 0
         x = -1
         do k = 1, nump
            do i = 1, netcell(k)%n
               x(k, i) = netcell(k)%nod(i)
            end do
         end do
         xptr = c_loc(x)

      case ("flowelemnode")
         if (allocated(x)) deallocate (x)
         ! Deallocate if already allocated.
         allocate (x(ndx, get_flow_elem_max_nodes()))
         ! initialize to 0
         x = -1
         do k = 1, ndx
            do i = 1, size(nd(k)%nod, 1)
               x(k, i) = nd(k)%nod(i)
            end do
         end do
         xptr = c_loc(x)
      case ("kn")
         xptr = c_loc(kn)
      end select
   end subroutine get_2d_int

!> Gets a pointer to a model variable, given its name.
!!
   subroutine get_var(c_var_name, x) bind(C, name="get_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var
      ! Return a pointer to the variable
      use iso_c_binding, only: c_double, c_char, c_loc
      use iso_c_utils
      use m_flow
      use m_flowgeom
      use m_sediment
      use m_flowparameters
      use network_data
      use m_sobekdfm
      use m_alloc
      use string_module
      use m_cell_geometry ! TODO: UNST-1705: temp, replace by m_flowgeom
      use unstruc_model
      use unstruc_channel_flow, only: network
      use m_laterals, only: numlatsg, kclat, qplatCum, qLatRealCum, qLatRealCumPre, n1latsg, n2latsg, qplat, balat, qLatRealAve, nnlat, qLatReal, qplatAve, qqlat
      use m_laterals, only: qplatCumPre
      use morphology_data_module, only: get_one_transport_parameter
      use m_get_kbot_ktop

      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout) :: x
      integer(c_int), target, allocatable, save :: xi(:, :)
      real(c_double), target, allocatable, save :: xd(:, :)

      integer :: i, k, Lf, knb, kb, kt

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      character(len=strlen(c_var_name)) :: tmp_var_name
      character(len=strlen(c_var_name)) :: varset_name, item_name, field_name !< For parsing compound variable names.

      ! Store the name
      var_name = char_array_to_string(c_var_name)

      ! Please be conservative in adding variables here. Most variables
      ! can be computed outside.
      ! You can generate extra variables here.
      select case (var_name)
      case ("uabs") ! TODO: AvD: UNST-2165, consider replacing by ucmag
         if (.not. allocated(uabs)) allocate (uabs(size(ucx)))

         do i = 1, size(ucx)
            uabs(i) = dble(sqrt(ucx(i) * ucx(i) + ucy(i) * ucy(i)))
         end do

         x = c_loc(uabs)
         return
      case ("sed1") ! sediment concentration, fraction1
         if (.not. allocated(sed1)) allocate (sed1(size(ucx)))

         if (allocated(sed)) then
            do i = 1, size(ucx)
               sed1(i) = sed(1, i)
            end do
         else
            sed1 = 0
         end if

         x = c_loc(sed1)
         return
      case ("netelemnode")
         if (allocated(xi)) deallocate (xi)
         ! Deallocate if already allocated.
         allocate (xi(get_net_elem_max_nodes(), nump1d2d))

         ! 1.... ndx2d, ndx2d+1...ndxi, ndxi+1...ndx
         ! 2d flow    ,  1d flow      , boundary

         ! initialize to 0
         xi = -1
         do k = 1, nump1d2d
            do i = 1, netcell(k)%n
               xi(i, k) = netcell(k)%nod(i)
            end do
         end do
         x = c_loc(xi)
         return
      case ("flowelemnode")
         if (allocated(xi)) deallocate (xi)
         ! Deallocate if already allocated.
         allocate (xi(get_flow_elem_max_nodes(), ndx2D))
         ! initialize to 0
         xi = -1
         do k = 1, ndx2D
            do i = 1, size(nd(k)%nod, 1)
               if (allocated(nd(k)%nod)) then
                  xi(i, k) = nd(k)%nod(i)
               end if
            end do
         end do

         x = c_loc(xi)
         return
      case ("flowelemnbs") ! Neighbouring flow node numbers for each flow node
         if (allocated(xi)) deallocate (xi)
         ! Deallocate if already allocated.
         allocate (xi(get_flow_elem_max_nbs(), ndx))
         xi = -1
         do k = 1, ndx
            do i = 1, nd(k)%lnx
               Lf = abs(nd(k)%ln(i))
               knb = ln(1, Lf)
               if (knb == k) then
                  knb = ln(2, Lf)
               end if

               xi(i, k) = knb
            end do
         end do

         x = c_loc(xi)
         return

      case ("flowelemlns") ! Neighbouring flow link numbers for each flow node
         if (allocated(xi)) deallocate (xi)
         ! Deallocate if already allocated.
         allocate (xi(get_flow_elem_max_nbs(), ndx))
         xi = -1
         do k = 1, ndx
            xi(1:nd(k)%lnx, k) = abs(nd(k)%ln(1:nd(k)%lnx))
         end do

         x = c_loc(xi)
         return

      case ("flowelemcontour_x")
         allocate (xd(get_flow_elem_max_contour(), ndx))
         xd = 0.d0 ! Set this to nan?
         do i = 1, ndx
            xd(1:size(nd(i)%x), i) = nd(i)%x
         end do
         x = c_loc(xd)

      case ("flowelemcontour_y")
         allocate (xd(get_flow_elem_max_contour(), ndx))
         xd = 0.d0 ! I would like to set nans here. but how?
         do i = 1, ndx
            xd(1:size(nd(i)%x), i) = nd(i)%y
         end do
         x = c_loc(xd)

      case ("kn")
         x = c_loc(kn)
         return

      case ("isalt")
         x = c_loc(isalt)
         return

      case ("itemp")
         x = c_loc(itemp)
         return

      case ("constituents_names")
         x = c_loc(const_names)
         return

      case ("runid")
         x = c_loc(md_ident)
         return

      case ("tem1Surf")
         if (.not. allocated(tem1Surf)) then
            allocate (tem1Surf(ndx))
         end if
         do k = 1, ndx
            call getkbotktop(k, kb, kt)
            tem1Surf(k) = tem1(kt)
         end do
         x = c_loc(tem1Surf)

      case ("TcrEro")
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrEro)) then
            allocate (TcrEro(ndx, k))
         end if
         do i = 1, k
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               call get_one_transport_parameter(TcrEro(:, i), stmpar%trapar, i, 13)
            else ! Other transport formula than Parteniades-Krone
               TcrEro(:, i) = -999
            end if
         end do
         x = c_loc(TcrEro)

      case ("TcrSed")
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrSed)) then
            allocate (TcrSed(ndx, k))
         end if
         do i = 1, k
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               call get_one_transport_parameter(TcrSed(:, i), stmpar%trapar, i, 12)
            else ! Other transport formula than Parteniades-Krone
               TcrSed(:, i) = -999
            end if
         end do
         x = c_loc(TcrSed)
      case ('vltb')
         x = c_loc(vltb)
      case ('vltbOnLinks')
         x = c_loc(vltbOnLinks)
      case ('network')
         x = c_loc(Network)
      case ('kbndz')
         x = c_loc(kbndz)
      end select

      ! Try to parse variable name as slash-separated id (e.g., 'weirs/Lith/crest_level')
      tmp_var_name = var_name
      call str_token(tmp_var_name, varset_name, DELIMS='/')
      ! Check for valid group/set name (e.g. 'observations')
      select case (varset_name)
      case ("pumps", "weirs", "orifices", "gates", "generalstructures", "culverts", "longculverts", "sourcesinks", "dambreak", "observations", "crosssections", "laterals", "geometry", "field")
         ! A valid group name, now parse the location id first...
         call str_token(tmp_var_name, item_name, DELIMS='/')
         if (len_trim(item_name) > 0) then
            ! A valid item name, now parse the field name...
            field_name = tmp_var_name(2:)

            ! field_name is allowed to be empty, call the compound getter and return directly.
            call get_compound_field(string_to_char_array(varset_name), &
                                    string_to_char_array(item_name), &
                                    string_to_char_array(field_name), &
                                    x)
            return
         end if
      case ("controllabledam")
         call str_token(tmp_var_name, field_name, DELIMS='/')

         select case (field_name)
         case ("damlevel")
            x = c_loc(zcdam(1))
         end select

         ! If we return here, the var_name was no valid triplet 'weirs/Lith/crest_level',
         ! so continue below with all default variables.
         continue

      end select

      !
      ! check automatically generated interface
      include "bmi_get_var.inc"
      ! TODO: AvD: add returns to all auto generated cases to avoid unnecessary fall-through

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         call realloc(const_t, (/ndkx, numconst/), keepExisting=.true.)
         do k = 1, ndkx
            const_t(k, iconst) = constituents(iconst, k)
         end do
         x = c_loc(const_t(:, iconst))
         return
      end if
   end subroutine get_var

   subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var
      ! Return a pointer to the variable
      use unstruc_model
      use m_partitioninfo, only: jampi
      use MessageHandling
      use iso_c_binding, only: c_double, c_char, c_bool, c_loc, c_f_pointer
      use m_laterals, only: numlatsg, qplat, qqlat, balat, qplatCum, qplatCumPre, qplatAve, qLatReal, qLatRealCum
      use m_laterals, only: qLatRealCumPre, qLatRealAve, n1latsg, n2latsg, nnlat, kclat
      use morphology_data_module, only: PARSOURCE_FIELD

      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), value, intent(in) :: xptr

      integer, external :: init_openmp

      character(kind=c_char), dimension(:), pointer :: x_0d_char_ptr => null()
      real(c_double), pointer :: x_0d_double_ptr
      real(c_double), pointer :: x_1d_double_ptr(:)
      real(c_double), pointer :: x_2d_double_ptr(:, :)
      real(c_double), pointer :: x_3d_double_ptr(:, :, :)
      integer(c_int), pointer :: x_0d_int_ptr
      integer(c_int), pointer :: x_1d_int_ptr(:)
      integer(c_int), pointer :: x_2d_int_ptr(:, :)
      integer(c_int), pointer :: x_3d_int_ptr(:, :, :)
      real(c_float), pointer :: x_0d_float_ptr
      real(c_float), pointer :: x_1d_float_ptr(:)
      real(c_float), pointer :: x_2d_float_ptr(:, :)
      real(c_float), pointer :: x_3d_float_ptr(:, :, :)
      character(kind=c_char), pointer :: x_1d_char_ptr(:)
      logical(kind=c_bool), pointer :: x_1d_logical_ptr(:)
      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      character(kind=c_char), dimension(:), pointer :: c_value => null()
      character(len=:), allocatable :: levels
      character(len=10) :: threadsString = ' '
      integer :: i, k, ipos, n, ierr

      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      no_warning_unused_variable(x_0d_char_ptr)
      no_warning_unused_variable(x_3d_int_ptr)
      no_warning_unused_variable(x_0d_float_ptr)
      no_warning_unused_variable(x_1d_float_ptr)
      no_warning_unused_variable(x_2d_float_ptr)
      no_warning_unused_variable(x_3d_float_ptr)

      include "bmi_set_var.inc"

      ! custom overrides
      select case (var_name)
      case ("threads")
         ! max #threads (component setting in dimr config, passed as string)
         call c_f_pointer(xptr, c_value, [MAXSTRLEN])
         if (associated(c_value)) then
            threadsString = ' '
            do i = 1, min(len(threadsString), MAXSTRLEN)
               if (c_value(i) == c_null_char) exit
               threadsString(i:i) = c_value(i)
            end do
            read (threadsString, '(I)', iostat=ierr) md_numthreads
            if (ierr == 0) then
               ! Activate the new OpenMP threads setting
               ierr = init_openmp(md_numthreads, jampi)
            end if
         end if
         return
      case ("zk")
         do i = 1, numk
            call update_land_nodes(i, x_1d_double_ptr(i))
         end do
         call land_change_callback()

         return
      case ("processlibrary")
         call c_f_pointer(xptr, c_value, [MAXSTRLEN])
         md_pdffile = " "
         if (associated(c_value)) then
            do i = 1, MAXSTRLEN
               if (c_value(i) == c_null_char) exit
               md_pdffile(i:i) = c_value(i)
            end do
         end if
         return
      case ("openprocessdllso")
         call c_f_pointer(xptr, c_value, [MAXSTRLEN])
         md_oplfile = " "
         if (associated(c_value)) then
            do i = 1, MAXSTRLEN
               if (c_value(i) == c_null_char) exit
               md_oplfile(i:i) = c_value(i)
            end do
         end if
         return
      case ("bloomspecies")
         call c_f_pointer(xptr, c_value, [MAXSTRLEN])
         md_blmfile = " "
         if (associated(c_value)) then
            do i = 1, MAXSTRLEN
               if (c_value(i) == c_null_char) exit
               md_blmfile(i:i) = c_value(i)
            end do
         end if
         return
      case ("verbose")
         call c_f_pointer(xptr, c_value, [MAXSTRLEN])
         levels = ''
         do i = 1, MAXSTRLEN
            if (c_value(i) == c_null_char) exit
            levels = levels//c_value(i)
         end do
         ipos = index(levels, ':')
         if (ipos > 0) then
            loglevel_StdOut = stringtolevel(levels(:ipos - 1))
            loglevel_file = stringtolevel(levels(ipos + 1:))
         else
            loglevel_StdOut = stringtolevel(levels)
            loglevel_file = stringtolevel(levels)
         end if
         call initMessaging(mdia)
         return
      case ("TcrEro")
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrEro)) then
            allocate (TcrEro(ndx, k))
         end if
         call c_f_pointer(xptr, x_2d_double_ptr, shape(TcrEro))
         TcrEro(:, :) = x_2d_double_ptr
         do i = 1, size(TcrEro, 2)
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               n = stmpar%trapar%iparfile(13, i)
               if (n > 0) then ! if spatially varying
                  if (stmpar%trapar%parfile(n)%source == PARSOURCE_FIELD) then
                     stmpar%trapar%parfile(n)%parfld(:) = TcrEro(:, i)
                  else
                     call mess(LEVEL_ERROR, 'TcrEro is forced using a time-series file and can''t be set using BMI.')
                  end if
               else ! if not spatially varying
                  if (minval(TcrEro(:, i)) == maxval(TcrEro(:, i))) then ! if provided data is uniform
                     stmpar%trapar%par(13, i) = TcrEro(1, i)
                  else
                     call mess(LEVEL_ERROR, 'TcrEro isn''t defined as spatially varying, therefore set_var must be called with a constant field.')
                  end if
               end if
            else
               call mess(LEVEL_WARN, 'TcrEro values ignored for fractions not governed by the Parteniades-Krone transport formula.')
            end if
         end do
         return
      case ("TcrSed")
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrSed)) then
            allocate (TcrSed(ndx, k))
         end if
         call c_f_pointer(xptr, x_2d_double_ptr, shape(TcrSed))
         TcrSed(:, :) = x_2d_double_ptr
         do i = 1, size(TcrSed, 2)
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               n = stmpar%trapar%iparfile(12, i)
               if (n > 0) then ! if spatially varying
                  if (stmpar%trapar%parfile(n)%source == PARSOURCE_FIELD) then
                     stmpar%trapar%parfile(n)%parfld(:) = TcrSed(:, i)
                  else
                     call mess(LEVEL_ERROR, 'TcrSed is forced using a time-series file and can''t be set using BMI.')
                  end if
               else ! if not spatially varying
                  if (minval(TcrSed(:, i)) == maxval(TcrSed(:, i))) then ! if provided data is uniform
                     stmpar%trapar%par(12, i) = TcrSed(1, i)
                  else
                     call mess(LEVEL_ERROR, 'TcrSed isn''t defined as spatially varying, therefore set_var must be called with a constant field.')
                  end if
               end if
            else
               call mess(LEVEL_WARN, 'TcrSed values ignored for fractions not governed by the Parteniades-Krone transport formula.')
            end if
         end do
         return
      case ("sourcesinks/COSUMO/nf_q_source_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         return
      case ("sourcesinks/COSUMO/nf_q_source")
         call c_f_pointer(xptr, x_1d_double_ptr, (/nf_num_dif/))
         nf_q_source => x_1d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_q_intake_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         return
      case ("sourcesinks/COSUMO/nf_q_intake")
         call c_f_pointer(xptr, x_1d_double_ptr, (/nf_num_dif/))
         nf_q_intake => x_1d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_const_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         nf_numconst = x_1d_int_ptr(2)
         return
      case ("sourcesinks/COSUMO/nf_const")
         call c_f_pointer(xptr, x_2d_double_ptr, (/nf_num_dif, nf_numconst/))
         nf_const => x_2d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_intake_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         nf_numintake = x_1d_int_ptr(2)
         i = x_1d_int_ptr(3)
         if (i /= 3) then
            call mess(LEVEL_ERROR, 'set_var::nf_intake_shape: third dimension is not equal to 3')
         end if
         return
      case ("sourcesinks/COSUMO/nf_intake")
         call c_f_pointer(xptr, x_3d_double_ptr, (/nf_num_dif, nf_numintake, 3/))
         nf_intake => x_3d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_sink_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         nf_numsink = x_1d_int_ptr(2)
         i = x_1d_int_ptr(3)
         if (i /= 6) then
            call mess(LEVEL_ERROR, 'set_var::nf_sink_shape: third dimension is not equal to 6')
         end if
         return
      case ("sourcesinks/COSUMO/nf_sink")
         call c_f_pointer(xptr, x_3d_double_ptr, (/nf_num_dif, nf_numsink, 6/))
         nf_sink => x_3d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_sour_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         nf_numsour = x_1d_int_ptr(2)
         i = x_1d_int_ptr(3)
         if (i /= 8) then
            call mess(LEVEL_ERROR, 'set_var::nf_sink_shape: third dimension is not equal to 8')
         end if
         return
      case ("sourcesinks/COSUMO/nf_sour")
         call c_f_pointer(xptr, x_3d_double_ptr, (/nf_num_dif, nf_numsour, 8/))
         nf_sour => x_3d_double_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_const_operator_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_numconst = x_1d_int_ptr(1)
         nf_namlen = x_1d_int_ptr(2)
         allocate (character(nf_namlen) :: nf_const_operator(nf_numconst))
         return
      case ("sourcesinks/COSUMO/nf_const_operator")
         call c_f_pointer(xptr, x_1d_char_ptr, (/nf_numconst * nf_namlen/))
         nf_const_operator = transfer(x_1d_char_ptr, nf_const_operator)
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      case ("sourcesinks/COSUMO/nf_src_mom_shape")
         call c_f_pointer(xptr, x_1d_int_ptr, (/6/))
         nf_num_dif = x_1d_int_ptr(1)
         return
      case ("sourcesinks/COSUMO/nf_src_mom")
         call c_f_pointer(xptr, x_1d_logical_ptr, (/nf_num_dif/))
         nf_src_mom => x_1d_logical_ptr
         ! Switch on nearfield
         nearfield_mode = NEARFIELD_UPDATED
         return
      end select

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         call c_f_pointer(xptr, x_1d_double_ptr, (/ndkx/))
         do i = 1, ndkx
            constituents(iconst, i) = x_1d_double_ptr(i)
         end do
         return
      end if
      !select case(var_name)
      !case('debugLevel')
      !        call c_f_pointer(xptr, x_1d_double_ptr, (/ 1 /))
      !        call setMessageHandling(thresholdLevel = nint(x_1d_double_ptr(1)), prefix_logging = "dflow1d")
      !end select

   end subroutine set_var

   subroutine set_var_slice(c_var_name, c_start, c_count, xptr) bind(C, name="set_var_slice")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var_slice
      ! Return a pointer to the variable
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use m_laterals, only: qplat, qqlat, balat, qplatCum, qplatCumPre, qplatAve, qLatReal, qLatRealCum
      use m_laterals, only: qLatRealCumPre, qLatRealAve, n1latsg, n2latsg, nnlat, kclat
      use morphology_data_module, only: PARSOURCE_FIELD

      integer(c_int), intent(in) :: c_start(*)
      integer(c_int), intent(in) :: c_count(*)
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), value, intent(in) :: xptr
      integer :: i, k, n

      real(c_double), pointer :: x_0d_double_ptr

      real(c_double), pointer :: x_1d_double_ptr(:)
      real(c_double), pointer :: x_2d_double_ptr(:, :)
      real(c_double), pointer :: x_3d_double_ptr(:, :, :)
      integer(c_int), pointer :: x_0d_int_ptr
      integer(c_int), pointer :: x_1d_int_ptr(:)
      integer(c_int), pointer :: x_2d_int_ptr(:, :)
      integer(c_int), pointer :: x_3d_int_ptr(:, :, :)
      real(c_float), pointer :: x_0d_float_ptr
      real(c_float), pointer :: x_1d_float_ptr(:)
      real(c_float), pointer :: x_2d_float_ptr(:, :)
      real(c_float), pointer :: x_3d_float_ptr(:, :, :)

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      no_warning_unused_variable(x_0d_double_ptr)
      no_warning_unused_variable(x_3d_double_ptr)
      no_warning_unused_variable(x_0d_int_ptr)
      no_warning_unused_variable(x_3d_int_ptr)
      no_warning_unused_variable(x_0d_float_ptr)
      no_warning_unused_variable(x_1d_float_ptr)
      no_warning_unused_variable(x_2d_float_ptr)
      no_warning_unused_variable(x_3d_float_ptr)

      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))

      include 'bmi_set_var_slice.inc'

      ! custom overrides
      select case (var_name)
      case ("ucx")
         !cell = netcell(index)
         !
         !do edgeIndex = 1, cell%n
         !    ! calculate edge angle
         !    linkX1 = XK(KN(1,edgeIndex))
         !    linkX2 = XK(KN(2,edgeIndex))
         !    linkY1 = YK(KN(1,edgeIndex))
         !    linkY2 = YK(KN(2,edgeIndex))
         !
         !    angle = atan((linkY2 - linkY1) / (linkX2 - linkX1))
         !
         !    u0(cell%lin(edgeIndex)) = value * cos(angle)
         !    u1(cell%lin(edgeIndex)) = value * cos(angle)
         !end do

         ! convert it to the velocity increment on cell interfaces

         ! update u0 - velocity at cell edges

         ! a) calculate weights based on distances to the cell edges
         ! b) project current velocity component to the cell edge tangential velocity
         ! c) add resulting velocity increment to the every cell edge velocity

         ! for every link:
         ! 1. A - angle of the link, relative to the X axis (or calculate it from the link coordinates = atan((y2-y1)/(x2-x1))
         ! 2. for Y component: V_link += Uy * sin(A)
         ! 3. for X component: V_link += Ux * cos(A)

         !ucx(index + 1) = value
         !case("ucy")
         ! convert it to the velocity increment on cell interfaces

         !ucy(index + 1) = value
      case ("unorm")
         ! u1(index + 1) = value
         u1(c_start(1) + 1:c_start(1) + c_count(1)) = x_1d_double_ptr(1:c_count(1))
         return
      case ("zk")
         do i = 1, c_count(1)
            call update_land_nodes(c_start(1) + i - 1, x_1d_double_ptr(i))
         end do
         call land_change_callback()

         !zkdropstep = value - zk(index + 1)
         !call dropland(xz(index + 1), yz(index + 1), 1)
         return

      case ("TcrEro")
         call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrEro)) then
            allocate (TcrEro(ndx, k))
         end if
         TcrEro(c_start(1) + 1:(c_start(1) + c_count(1)), c_start(2) + 1:(c_start(2) + c_count(2))) = x_2d_double_ptr
         do i = c_start(2) + 1, (c_start(2) + c_count(2))
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               n = stmpar%trapar%iparfile(13, i)
               if (n > 0) then ! if spatially varying
                  if (stmpar%trapar%parfile(n)%source == PARSOURCE_FIELD) then
                     stmpar%trapar%parfile(n)%parfld(c_start(1) + 1:(c_start(1) + c_count(1))) = TcrEro(c_start(1) + 1:(c_start(1) + c_count(1)), i)
                  else
                     call mess(LEVEL_ERROR, 'TcrEro is forced using a time-series file and can''t be set using BMI.')
                  end if
               else ! if not spatially varying: the user is not allowed to change it partially by a different value
                  call mess(LEVEL_ERROR, 'TcrEro isn''t defined as spatially varying, therefore the set_var_slice call is disabled.')
               end if
            else
               call mess(LEVEL_ERROR, 'TcrEro can only be set for fractions governed by the Parteniades-Krone transport formula.')
            end if
         end do
         return

      case ("TcrSed")
         call c_f_pointer(xptr, x_2d_double_ptr, (/c_count(1), c_count(2)/))
         k = size(stmpar%trapar%par, 2) ! equivalent to stmpar%lsedtot
         if (.not. allocated(TcrSed)) then
            allocate (TcrSed(ndx, k))
         end if
         TcrSed(c_start(1) + 1:(c_start(1) + c_count(1)), c_start(2) + 1:(c_start(2) + c_count(2))) = x_2d_double_ptr
         do i = c_start(2) + 1, (c_start(2) + c_count(2))
            if (stmpar%trapar%iform(i) == -3) then ! if transport formula is Parteniades-Krone
               n = stmpar%trapar%iparfile(12, i)
               if (n > 0) then ! if spatially varying
                  if (stmpar%trapar%parfile(n)%source == PARSOURCE_FIELD) then
                     stmpar%trapar%parfile(n)%parfld(c_start(1) + 1:(c_start(1) + c_count(1))) = TcrSed(c_start(1) + 1:(c_start(1) + c_count(1)), i)
                  else
                     call mess(LEVEL_ERROR, 'TcrSed is forced using a time-series file and can''t be set using BMI.')
                  end if
               else ! if not spatially varying: the user is not allowed to change it partially by a different value
                  call mess(LEVEL_ERROR, 'TcrSed isn''t defined as spatially varying, therefore the set_var_slice call is disabled.')
               end if
            else
               call mess(LEVEL_ERROR, 'TcrSed can only be set for fractions governed by the Parteniades-Krone transport formula.')
            end if
         end do
         return
      end select

      if (numconst > 0) then
         iconst = find_name(const_names, var_name)
      end if
      if (iconst /= 0) then
         call c_f_pointer(xptr, x_1d_double_ptr, (/c_count(1)/))
         do i = 1, c_count(1)
            constituents(iconst, c_start(1) + i) = x_1d_double_ptr(i)
         end do
         return
      end if

   end subroutine set_var_slice

   subroutine on_land_change() bind(C, name="on_land_change")
!DEC$ ATTRIBUTES DLLEXPORT :: on_land_change
      implicit none
      call land_change_callback()
   end subroutine on_land_change

   subroutine update_land(c_node_index, c_new_zk) bind(C, name="update_land")
!DEC$ ATTRIBUTES DLLEXPORT :: update_land
      implicit none
      integer(c_int), intent(in) :: c_node_index
      real(c_double), intent(in) :: c_new_zk

      call update_land_nodes(c_node_index, c_new_zk)
   end subroutine update_land

!> Adds model features to the active model.
!!
!! Currently supported model features are:
!! * thindams (not yet, WIP on UNST-1111)
!! TODO: fixedweirs, sourcesinks, hydraulic structures, ...
!!
!! The model features are typically placed based on the input polylines,
!! which are in one long rank-1 array with all polylines behind each other.
   function dfm_add_features(c_feat_name, xpli_ptr, ypli_ptr, zpli_ptr, npli_ptr, num_feat, keep_existing) result(iresult) bind(C, name="dfm_add_features")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_add_features
      use iso_c_binding, only: c_double, c_int, c_char, c_loc, c_f_pointer
      use iso_c_utils
      use unstruc_messages
      use m_polygon
      use dfm_error
      use kdtree2Factory
      use m_find_crossed_links_kdtree2

      character(kind=c_char), intent(in) :: c_feat_name(*) !< Name/type of the features set, e.g., 'thindams'
      type(c_ptr), value, intent(in) :: xpli_ptr !< Pointer (by value) to the C-compatible x-coordinates of all features's polyline (one long array).
      type(c_ptr), value, intent(in) :: ypli_ptr !< Pointer (by value) to the C-compatible y-coordinates of all features's polyline (one long array).
      type(c_ptr), value, intent(in) :: zpli_ptr !< Pointer (by value) to the C-compatible z-coordinates of all features's polyline (one long array). May be NULL if not relevant.
      type(c_ptr), value, intent(in) :: npli_ptr !< Pointer (by value) to the C-compatible nr of coordinates of all features's polyline (one long array).
      integer(c_int), intent(in) :: num_feat !< Number of features passed. npli_ptr should have this length. xpli_ptr etc. should have length sum(npli_ptr(1:numfeat))
      integer(c_int), intent(in) :: keep_existing !< Whether or not (1/0) to keep existing features of the same type.
      integer(c_int) :: iresult !< Result status, DFM_NOERR(=0) if successful.

      real(c_double), pointer :: xpli(:), ypli(:), zpli(:)
      integer(c_int), pointer :: npli(:)

      integer :: i, npli_pts, nxln
      double precision :: thdh
      logical :: with_z
      double precision, dimension(:), allocatable :: dSL
      integer, dimension(:), allocatable :: iLnx, ipol

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: feat_name

      iresult = DFM_NOERR
      thdh = 1000d0

      if (keep_existing == 1) then
         iresult = DFM_NOTIMPLEMENTED
         goto 888
      end if

      ! Store the name
      feat_name = char_array_to_string(c_feat_name)

      call c_f_pointer(xpli_ptr, xpli, shape(xpli))
      call c_f_pointer(ypli_ptr, ypli, shape(ypli))
      call c_f_pointer(zpli_ptr, zpli, shape(zpli)) ! TODO: AvD: need a safety check on NULL??
      with_z = .false. ! TODO: AvD: make generic, support z.
      call c_f_pointer(npli_ptr, npli, shape(npli))

      ! First count the summed length of all polylines
      npli_pts = 0
      do i = 1, num_feat
         npli_pts = npli_pts + npli(i)
      end do

      call increasepol(npli_pts + num_feat, keep_existing)

      ! Now copy the polylines into global m_polygon with dmiss separators
      npli_pts = 0
      do i = 1, num_feat
         xpl(npli_pts + 1:npli_pts + npli(i)) = xpli(1:npli(i))
         xpl(npli_pts + 1:npli_pts + npli(i)) = xpli(1:npli(i))
         if (with_z) then
            zpl(npli_pts + 1:npli_pts + npli(i)) = zpli(1:npli(i))
         end if

         npli_pts = npli_pts + npli(i) + 1 ! One extra for the extra dmiss separator between each two consecutive polylines.
      end do
      npl = npli_pts ! Global polygon points counter

      select case (feat_name)
         ! THINDAMS
      case ("thindams")
         iresult = DFM_NOTIMPLEMENTED
         allocate (iLnx(Lnx))
         iLnx = 0
         allocate (ipol(Lnx))
         ipol = 0
         allocate (dSL(Lnx))
         dSL = 0
         call find_crossed_links_kdtree2(treeglob, npl, xpl, ypl, 2, Lnx, 0, nxln, iLnx, ipol, dSL, iresult)
         if (iresult /= DFM_NOERR) then
            goto 888
         end if
         do i = 1, nxln
            bob(1, iLnx(i)) = thdh
            bob(2, iLnx(i)) = thdh
         end do
         deallocate (iLnx, ipol, dSL)
         ! TODO: AvD: also somehow disable the existing thin dams

      case ("fixedweirs", "sourcesinks")
         iresult = DFM_NOTIMPLEMENTED
         ! TODO: AvD: call setfixedweirs but refactor the file read of md_fxwfile over there.
         ! TODO: AvD: also somehow disable the existing fixed weirs.
         goto 888
      case default
         iresult = DFM_NOTIMPLEMENTED
         goto 888
      end select

      ! Succes
      iresult = DFM_NOERR
      return

888   continue
      ! Some error.
   end function dfm_add_features

!> Gets data for a specific field for a specific item from a set-variable of compound values.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! a single pump selected by its name, and the actual data by its field name.
!!
!! To convert the returned pointer to an actual data type in your calling application,
!! use get_compound_field_type to determine the correct data type.
   subroutine get_compound_field(c_var_name, c_item_name, c_field_name, x) bind(C, name="get_compound_field")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_compound_field
      ! Return a pointer to the compound's field variable
      use iso_c_binding, only: c_double, c_char, c_loc
      use iso_c_utils
      use fm_external_forcings_data
      use m_observations
      use m_monitoring_crosssections
      use m_strucs
      use m_structures, only: valdambreak
      use m_1d_structures
      use m_wind
      use unstruc_channel_flow, only: network
      use unstruc_messages
      use m_transport, only: NUMCONST, constituents, const_names, ISALT, ITEMP, ITRA1
      use m_update_values_on_cross_sections, only: update_values_on_cross_sections
      use string_module, only: str_tolower

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      character(kind=c_char), intent(in) :: c_item_name(*) !< Name of a single item's index/location, e.g., 'Pump01'
      character(kind=c_char), intent(in) :: c_field_name(*) !< Name of the field to get, e.g., 'capacity'
      type(c_ptr), intent(inout) :: x !< Pointer (by reference) to requested value data, NULL if not available.

      integer :: item_index
      logical :: is_in_network

      integer :: iconst
      integer :: itrac

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: item_name
      character(len=MAXSTRLEN) :: field_name
      ! Store the name and convert var and field to lowercase to make them case-insensitive.
      var_name = str_tolower(char_array_to_string(c_var_name))
      item_name = char_array_to_string(c_item_name)
      field_name = str_tolower(char_array_to_string(c_field_name))

      select case (var_name)
         ! PUMPS
      case ("pumps")
         call getStructureIndex('pumps', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("capacity")
            if (is_in_network) then
               x = get_pump_capacity_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(qpump(item_index))
            end if
            return
         end select

         ! WEIRS
      case ("weirs")
         call getStructureIndex('weirs', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("crestlevel")
            if (is_in_network) then
               x = get_crest_level_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(zcgen((item_index - 1) * 3 + 1))
            end if
            return
         case ("lat_contr_coeff")
            ! TODO: RTC: AvD: get this from weir params
            return
         end select

         ! ORIFICES
      case ("orifices")
         call getStructureIndex('orifices', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("gateloweredgelevel")
            if (is_in_network) then
               x = get_gate_lower_edge_level_c_loc(network%sts%struct(item_index))
            end if
            return
         case ("crestlevel")
            if (is_in_network) then
               x = get_crest_level_c_loc(network%sts%struct(item_index))
            end if
            return
         end select

         ! GATES
      case ("gates")
         call getStructureIndex('gates', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("crestlevel")
            x = c_loc(zcgen((item_index - 1) * 3 + 1))
            return
         case ("gateheight")
            x = c_loc(generalstruc(item_index)%gatedoorheight)
            return
         case ("gateloweredgelevel")
            x = c_loc(zcgen((item_index - 1) * 3 + 2))
            return
         case ("gateopeningwidth")
            x = c_loc(zcgen((item_index - 1) * 3 + 3))
            return
         case ("gateopeninghorizontaldirection")
            ! TODO: RTC: AvD: get this from gate/genstru params
            return
         end select

         ! GENERALSTRUCTURES
      case ("generalstructures")
         call getStructureIndex('generalstructures', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("crestlevel")
            if (is_in_network) then
               x = get_crest_level_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(zcgen((item_index - 1) * 3 + 1))
            end if
            return
         case ("gateheight")
            if (is_in_network) then
               x = get_gate_door_height_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(generalstruc(item_index)%gatedoorheight)
            end if

            return
         case ("gateloweredgelevel")
            if (is_in_network) then
               x = get_gate_lower_edge_level_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(zcgen((item_index - 1) * 3 + 2))
            end if
            return
         case ("gateopeningwidth")
            if (is_in_network) then
               x = get_gate_opening_width_c_loc(network%sts%struct(item_index))
            else
               x = c_loc(zcgen((item_index - 1) * 3 + 3))
            end if
            return
         case ("gateopeninghorizontaldirection")
            ! TODO: RTC: AvD: get this from gate/genstru params
            return
         end select

         ! CULVERTS
      case ("culverts")
         call getStructureIndex('culverts', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("valveopeningheight")
            if (is_in_network) then
               x = get_valve_opening_height_c_loc(network%sts%struct(item_index))
            end if
            return
         end select

         ! LONGCULVERTS
      case ("longculverts")
         call getStructureIndex('longculverts', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("valverelativeopening")
            x = get_valve_relative_opening_c_loc(longculverts(item_index))
            return
         end select

         ! SOURCE-SINKS
      case ("sourcesinks")
         if (item_name == "COSUMO") then
            select case (field_name)
            case ("nf_q_source")
               if (.not. associated(nf_q_source)) then
                  return
               end if
               x = c_loc(nf_q_source)
               return
            case ("nf_q_intake")
               if (.not. associated(nf_q_intake)) then
                  return
               end if
               x = c_loc(nf_q_intake)
               return
            case ("nf_const")
               if (.not. associated(nf_const)) then
                  return
               end if
               x = c_loc(nf_const)
               return
            case ("nf_intake")
               if (.not. associated(nf_intake)) then
                  return
               end if
               x = c_loc(nf_intake)
               return
            case ("nf_sink")
               if (.not. associated(nf_sink)) then
                  return
               end if
               x = c_loc(nf_sink)
               return
            case ("nf_sour")
               if (.not. associated(nf_sour)) then
                  return
               end if
               x = c_loc(nf_sour)
               return
            case ("nf_const_operator")
               if (.not. associated(nf_const_operator)) then
                  return
               end if
               x = c_loc(nf_const_operator)
               return
            case ("nf_src_mom")
               if (.not. associated(nf_src_mom)) then
                  return
               end if
               x = c_loc(nf_src_mom)
               return
            end select
            return
         end if
         call getStructureIndex('sourcesinks', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("discharge")
            x = c_loc(qstss((item_index - 1) * (NUMCONST + 1) + 1))
            return
         case ("change_in_salinity")
            if (ISALT == 0) then
               return
            end if
            x = c_loc(qstss((item_index - 1) * (NUMCONST + 1) + ISALT + 1))
            return
         case ("change_in_temperature")
            if (ITEMP == 0) then
               return
            end if
            x = c_loc(qstss((item_index - 1) * (NUMCONST + 1) + ITEMP + 1))
            return
         end select
         ! Dambreak
      case ("dambreak")
         call getStructureIndex('dambreak', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("dambreak_s1up")
            x = c_loc(waterLevelsDambreakUpStream(item_index))
            return
         case ("dambreak_s1dn")
            x = c_loc(waterLevelsDambreakDownStream(item_index))
            return
         case ("dambreak_breach_depth")
            x = c_loc(breachDepthDambreak(item_index))
            return
         case ("dambreak_breach_width")
            x = c_loc(breachWidthDambreak(item_index))
            return
         case ("dambreak_instantaneous_discharge")
            x = c_loc(valdambreak(1, item_index))
            return
         case ("dambreak_cumulative_discharge")
            x = c_loc(valdambreak(2, item_index))
            return
         end select

         ! OBSERVATION STATIONS
      case ("observations")
         call getObservationIndex(item_name, item_index)
         if (item_index <= 0) then
            return
         end if

         call updateValuesOnObservationStations()

         select case (field_name)
         case ("water_level")
            x = c_loc(valobs(item_index, IPNT_S1))
            return
         case ("water_depth")
            x = c_loc(valobs(item_index, IPNT_HS))
            return
         case ("salinity")
            x = c_loc(valobs(item_index, IPNT_SA1))
            return
         case ("temperature")
            x = c_loc(valobs(item_index, IPNT_TEM1))
            return
         case ("velocity")
            x = c_loc(valobs(item_index, IPNT_UMAG))
            return
         case ("discharge")
            x = c_loc(valobs(item_index, IPNT_QMAG))
            return
         case default
            !       assume this is a tracer
            !       get constituent number for this tracer
            iconst = find_name(const_names, field_name)

            if (iconst == 0) then
               !          tracer not found
               call mess(LEVEL_ERROR, 'get_compound_field: cannot find '//trim(var_name)//'/'//trim(item_name)//'/'//trim(field_name))
            else
               if (kmx > 1) then
                  call mess(LEVEL_ERROR, 'get_compound_field: 3D not supported for '//trim(var_name)//'/'//trim(item_name)//'/'//trim(field_name))
               else
                  !             find tracer number
                  itrac = iconst - ITRA1 + 1
                  x = c_loc(VALOBS(item_index, IPNT_TRA1 + (itrac - 1)))
               end if
            end if
            return
         end select
         ! MONITORING CROSSSECTIONS
      case ("crosssections")
         call getCrosssectionIndex(item_name, item_index)
         if (item_index <= 0) then
            return
         end if

         call update_values_on_cross_sections

         select case (field_name)
         case ("discharge")
            x = c_loc(crs(item_index)%sumvalcur(IPNT_Q1C))
            return
         case ("velocity")
            x = c_loc(crs(item_index)%sumvalcur(IPNT_U1A))
            return
         case ("water_level")
            x = c_loc(crs(item_index)%sumvalcur(IPNT_S1A))
            return
         case ("water_depth")
            x = c_loc(crs(item_index)%sumvalcur(IPNT_HUA))
            return
         case default
            ! TODO: AvD: error to warn for unimplemented feature?
            return
         end select
         ! LATERAL DISCHARGES
      case ("laterals")
         x = get_pointer_to_lateral_variable(item_name, field_name)
         ! GEOMETRY
      case ("geometry")
         select case (item_name)
         case ("xcc")
            x = c_loc(xzw)
            return
         case ("ycc")
            x = c_loc(yzw)
            return
         case ("z_level")
            x = c_loc(zws)
            return
         case ("kbot")
            x = c_loc(kbot)
            return
         case ("ktop")
            x = c_loc(ktop)
            return
         end select
         ! FIELDS
      case ("field")
         select case (item_name)
         case ("water_depth")
            x = c_loc(hs)
            return
         case ("velocity_x")
            x = c_loc(ucx)
            return
         case ("velocity_y")
            x = c_loc(ucy)
            return
         case ("rho")
            x = c_loc(rho)
            return
         case ("constituents")
            x = c_loc(constituents)
            return
         end select
      end select ! var_name

      ! check automatically generated interface
      ! TODO: AvD: include "bmi_get_compound_field.inc"

   end subroutine get_compound_field

   !> Returns the c_ptr for a variable on a lateral location
   function get_pointer_to_lateral_variable(item_name, field_name) result(c_lateral_pointer)
      use m_laterals, only: qplat, nnlat, n1latsg, outgoing_lat_concentration, incoming_lat_concentration, apply_transport, &
                            lateral_volume_per_layer, num_layers
      use m_flow, only: s1
      use string_module, only: str_token

      implicit none
      character(len=MAXSTRLEN), intent(in) :: item_name
      character(len=MAXSTRLEN), intent(in) :: field_name
      type(c_ptr) :: c_lateral_pointer

      integer :: item_index, k1, constituent_index
      character(len=MAXSTRLEN) :: constituent_name, direction_string
      call getLateralIndex(item_name, item_index)
      if (item_index <= 0) then
         return
      end if

      select case (field_name)
      case ("water_discharge")
         if (apply_transport(item_index) == 1 .or. kmx == 0) then
            c_lateral_pointer = c_loc(qplat(1:num_layers, item_index))
         else
            c_lateral_pointer = c_loc(qplat(kmx, item_index))
         end if
         return
      case ("water_level")
         ! NOTE: Return the "point-value", not an area-averaged water level (in case of lateral polygons).
         k1 = nnlat(n1latsg(item_index))
         if (k1 > 0) then
            c_lateral_pointer = c_loc(s1(k1))
         else
            c_lateral_pointer = c_null_ptr
         end if
         return
      case ("water_volume")
         if (apply_transport(item_index) == 1) then
            c_lateral_pointer = c_loc(lateral_volume_per_layer(:, item_index))
         else
            ! Not defined for apply__transport == 0
            c_lateral_pointer = c_null_ptr
         end if
         return
      end select

      if (apply_transport(item_index) == 0) then
         ! No constituent support for non "apply_transport" items
         c_lateral_pointer = c_null_ptr
         return
      end if

      constituent_name = field_name
      call str_token(constituent_name, direction_string, DELIMS='/')
      constituent_name = constituent_name(2:)
      ! Find constituent index
      select case (constituent_name)
      case ('water_salinity')
         constituent_index = ISALT
      case ('water_temperature')
         constituent_index = ITEMP
      case default
         constituent_index = find_name(const_names, constituent_name)
         if (iconst == 0) then
            !        tracer not found
            c_lateral_pointer = c_null_ptr
            return
         end if
      end select

      ! Use the correct array (outgoing or incoming)
      select case (direction_string)
      case ('outgoing')
         c_lateral_pointer = c_loc(outgoing_lat_concentration(:, constituent_index, item_index))
      case ('incoming')
         c_lateral_pointer = c_loc(incoming_lat_concentration(:, constituent_index, item_index))
      case default
         c_lateral_pointer = c_null_ptr
      end select
      return
   end function get_pointer_to_lateral_variable

!> Sets the value for a specific field for a specific item in a set-variable of compound values.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! a single pump selected by its name, and the actual data by its field name.
!!
!! The input value enters as a generic pointer, and will be converted to the required data type, e.g., double.
!! If necessary, use get_compound_field_type and get_compound_field_shape to determine which input is expected.
   subroutine set_compound_field(c_var_name, c_item_name, c_field_name, xptr) bind(C, name="set_compound_field")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_compound_field
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use iso_c_utils
      use unstruc_messages
      use m_strucs
      use m_1d_structures
      use m_wind
      use unstruc_channel_flow, only: network
      use m_General_Structure, only: update_widths
      use m_transport, only: NUMCONST, ISALT, ITEMP
      use m_laterals, only: qplat

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      character(kind=c_char), intent(in) :: c_item_name(*) !< Name of a single item's index/location, e.g., 'Pump01'
      character(kind=c_char), intent(in) :: c_field_name(*) !< Name of the field to get, e.g., 'capacity'
      type(c_ptr), value, intent(in) :: xptr !< Pointer (by value) to the C-compatible value data to be set.

      real(c_double), pointer :: x_0d_double_ptr
      type(c_ptr) :: fieldptr ! c_ptr to the structure's parameter

      integer :: item_index
      logical :: is_in_network

      integer :: iostat

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: item_name
      character(len=MAXSTRLEN) :: field_name
      ! Store the name
      var_name = char_array_to_string(c_var_name)
      item_name = char_array_to_string(c_item_name)
      field_name = char_array_to_string(c_field_name)
      ! Debugging printing only: guess that it's a scalar double value, for now.
      call c_f_pointer(xptr, x_0d_double_ptr)
      write (msgbuf, '(6a,f20.6,a)', iostat=iostat) 'set_compound_field for ', trim(var_name), '(', trim(item_name), ')::', trim(field_name), ', will be set to value = ', x_0d_double_ptr, '.'
      call dbg_flush()
      ! TODO: AvD: include "bmi_set_compound_field.inc"
      select case (var_name)
         ! PUMPS
      case ("pumps")
         call getStructureIndex('pumps', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         if (network%sts%struct(item_index)%pump%nrstages > 0) then
            call mess(LEVEL_ERROR, 'set_compound_field for '''//trim(var_name)//'/'//trim(item_name)//'/'//trim(field_name)//''' : a staged pump cannot be controlled by RTC.')
         end if

         select case (field_name)
         case ("capacity")
            if (is_in_network) then
               fieldptr = get_pump_capacity_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               qpump(item_index) = x_0d_double_ptr
            end if
            return
         end select

         ! WEIRS
      case ("weirs")
         call getStructureIndex('weirs', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("crest_level", "CrestLevel", "crestLevel")
            if (is_in_network) then
               fieldptr = get_crest_level_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               zcgen((item_index - 1) * 3 + 1) = x_0d_double_ptr
            end if
            return
         case ("lat_contr_coeff")
            ! TODO: RTC: AvD: set this in weir params
            return
         end select
         call update_zcgen_widths_and_heights()

         ! ORIFICES
      case ("orifices")
         call getStructureIndex('orifices', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("gateLowerEdgeLevel")
            if (is_in_network) then
               fieldptr = get_gate_lower_edge_level_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            end if
            return
         end select
         call update_widths(network%sts%struct(item_index)%generalst, network%sts%struct(item_index)%numlinks, network%sts%struct(item_index)%linknumbers, wu, .true.)

         ! GATES
      case ("gates")
         call getStructureIndex('gates', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("sill_level", "CrestLevel")
            call c_f_pointer(xptr, x_0d_double_ptr)
            zcgen((item_index - 1) * 3 + 1) = x_0d_double_ptr
            return
         case ("door_height", "GateHeight")
            call c_f_pointer(xptr, x_0d_double_ptr)
            generalstruc(item_index)%gatedoorheight = x_0d_double_ptr ! Not time-controlled, set directly in generalstruc.
            return
         case ("lower_edge_level", "GateLowerEdgeLevel")
            call c_f_pointer(xptr, x_0d_double_ptr)
            zcgen((item_index - 1) * 3 + 2) = x_0d_double_ptr
            return
         case ("opening_width", "GateOpeningWidth")
            call c_f_pointer(xptr, x_0d_double_ptr)
            zcgen((item_index - 1) * 3 + 3) = x_0d_double_ptr
            return
         case ("horizontal_opening_direction", "GateOpeningHorizontalDirection")
            ! TODO: RTC: AvD: set this once it's used
            return
         end select
         call update_zcgen_widths_and_heights()

         ! GENERAL STRUCTURES
      case ("generalstructures")
         call getStructureIndex('generalstructures', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if

         select case (field_name)
         case ("CrestLevel", "crestLevel")
            if (is_in_network) then
               fieldptr = get_crest_level_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               zcgen((item_index - 1) * 3 + 1) = x_0d_double_ptr
            end if
            return
         case ("GateHeight", "gateHeight")
            if (is_in_network) then
               fieldptr = get_gate_door_height_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               generalstruc(item_index)%gatedoorheight = x_0d_double_ptr ! Not time-controlled, set directly in generalstruc.
            end if

            return
         case ("GateLowerEdgeLevel", "gateLowerEdgeLevel")
            if (is_in_network) then
               fieldptr = get_gate_lower_edge_level_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               zcgen((item_index - 1) * 3 + 2) = x_0d_double_ptr
            end if
            return
         case ("GateOpeningWidth", "gateOpeningWidth")
            if (is_in_network) then
               fieldptr = get_gate_opening_width_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            else
               call c_f_pointer(xptr, x_0d_double_ptr)
               zcgen((item_index - 1) * 3 + 3) = x_0d_double_ptr
            end if
            return
         case ("GateOpeningHorizontalDirection", "gateOpeningHorizontalDirection")
            ! TODO: RTC: AvD: get this from gate/genstru params
            return
         end select
         call update_zcgen_widths_and_heights()

         ! CULVERTS
      case ("culverts")
         call getStructureIndex('culverts', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("valveOpeningHeight")
            if (is_in_network) then
               fieldptr = get_valve_opening_height_c_loc(network%sts%struct(item_index))
               fieldptr = xptr ! Set the scalar value of the structure's field pointed being to.
            end if
            return
         end select

         ! SOURCE-SINKS
      case ("sourcesinks")
         call getStructureIndex('sourcesinks', item_name, item_index, is_in_network)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("discharge")
            call c_f_pointer(xptr, x_0d_double_ptr)
            qstss((item_index - 1) * (NUMCONST + 1) + 1) = x_0d_double_ptr
            return
         case ("change_in_salinity")
            if (ISALT == 0) then
               return
            end if
            call c_f_pointer(xptr, x_0d_double_ptr)
            qstss((item_index - 1) * (NUMCONST + 1) + ISALT + 1) = x_0d_double_ptr
            return
         case ("change_in_temperature")
            if (ITEMP == 0) then
               return
            end if
            call c_f_pointer(xptr, x_0d_double_ptr)
            qstss((item_index - 1) * (NUMCONST + 1) + ITEMP + 1) = x_0d_double_ptr
            return
         end select

         ! LATERAL DISCHARGES
      case ("laterals")
         call getLateralIndex(item_name, item_index)
         if (item_index <= 0) then
            return
         end if
         select case (field_name)
         case ("water_discharge")
            call c_f_pointer(xptr, x_0d_double_ptr)
            ! Using max(1,kmx) is a temporary solution for now.
            qplat(max(1, kmx), item_index) = x_0d_double_ptr
            return
         end select

         ! NOTE: observations and crosssections are read-only!
      end select
   end subroutine set_compound_field

!> Gets the name for a specific field in a compound variable.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! each pump has the same list of fields, each with a name, for example 'capacity'.
!! Use the returned field name to call other get_compound_field_* routines.
   subroutine get_compound_field_name(c_var_name, c_field_index, c_field_name) bind(C, name="get_compound_field_name")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_compound_field_name
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use iso_c_utils

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      integer(kind=c_int), intent(in) :: c_field_index !< Index of the field in the compound variable's field list
      character(kind=c_char), intent(out) :: c_field_name(MAXSTRLEN) !< Returned name of the field to , e.g., 'capacity'

      integer :: field_index

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: field_name
      ! Store the name
      var_name = char_array_to_string(c_var_name)

      field_index = c_field_index

      select case (var_name)
         ! PUMPS
      case ("pumps")
         select case (field_index)
         case (1)
            field_name = "capacity"
         end select
         ! WEIRS
      case ("weirs")
         select case (field_index)
         case (1)
            field_name = "crestLevel"
         case (2)
            field_name = "lat_contr_coeff"
         end select
         ! ORIFICES
      case ("orifices")
         select case (field_index)
         case (1)
            field_name = "gateLowerEdgeLevel"
         case (2)
            field_name = "CrestLevel"
         end select
         ! GATES
      case ("gates")
         select case (field_index)
         case (1)
            field_name = "CrestLevel"
         case (2)
            field_name = "GateHeight"
         case (3)
            field_name = "GateLowerEdgeLevel"
         case (4)
            field_name = "GateOpeningWidth"
         case (5)
            field_name = "GateOpeningHorizontalDirection"
         end select

         ! GENERALSTUCTURES
      case ("generalstructures")
         select case (field_index)
         case (1)
            field_name = "crestLevel"
         case (2)
            field_name = "gateHeight"
         case (3)
            field_name = "gateLowerEdgeLevel"
         case (4)
            field_name = "gateOpeningWidth"
         case (5)
            field_name = "gateOpeningHorizontalDirection"
         end select

         ! CULVERTS
      case ("culverts")
         select case (field_index)
         case (1)
            field_name = "valveOpeningHeight"
         end select
         ! SOURCE-SINKS
      case ("sourcesinks")
         select case (field_index)
         case (1)
            field_name = "discharge"
         case (2)
            field_name = "change_in_salinity"
         case (3)
            field_name = "change_in_temperature"
         end select
         ! OBSERVATION STATIONS
      case ("observations")
         select case (field_index)
         case (1)
            field_name = "water_level"
         case (2)
            field_name = "water_depth"
         case (3)
            field_name = "salinity"
         case default
            return
         end select
         ! MONITORING CROSSSECTIONS
      case ("crosssections")
         select case (field_index)
         case (1)
            field_name = "discharge"
         case (2)
            field_name = "velocity"
         case (3)
            field_name = "water_level"
         case (4)
            field_name = "water_depth"
         case default
            ! TODO: AvD: error to warn for unimplemented feature?
            return
         end select
      case ("laterals")
         select case (field_index)
         case (1)
            field_name = "water_discharge"
         case (2)
            field_name = "water_level"
         case default
            return
         end select
      end select ! var_name

      c_field_name = string_to_char_array(trim(field_name))
   end subroutine get_compound_field_name

!> Gets the type for a specific field in a compound variable.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! each pump has the same list of fields, each with a type, for example 'double'.
!! Use the returned field type to prepare for calls to the set/get_compound_field routines.
   subroutine get_compound_field_type(c_var_name, c_field_name, c_type_name) bind(C, name="get_compound_field_type")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_compound_field_type
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use iso_c_utils

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      character(kind=c_char), intent(in) :: c_field_name(*) !< Name of the field, e.g., 'capacity'
      character(kind=c_char), intent(out) :: c_type_name(MAXSTRLEN) !< Returned type of the field, e.g., 'double'

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: field_name
      character(len=MAXSTRLEN) :: type_name
      ! Store the name
      var_name = char_array_to_string(c_var_name)
      field_name = char_array_to_string(c_field_name)

      type_name = "double"

      c_type_name = string_to_char_array(trim(type_name))

   end subroutine get_compound_field_type

!> Gets the rank for a specific field in a compound variable.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! each pump has the same list of fields, each with a rank, typically 0 (scalar).
!! Use the returned field rank to prepare for calls to the set/get_compound_field routines.
   subroutine get_compound_field_rank(c_var_name, c_field_name, rank) bind(C, name="get_compound_field_rank")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_compound_field_rank
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use iso_c_utils

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      character(kind=c_char), intent(in) :: c_field_name(*) !< Name of the field, e.g., 'capacity'
      integer(kind=c_int), intent(out) :: rank !< Returned rank of the field, e.g., 1

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: field_name

      ! Store the name
      var_name = char_array_to_string(c_var_name)
      field_name = char_array_to_string(c_field_name)

      rank = 0 ! for all scalar vars/fields now (unless pumps start to have multiple stages)

   end subroutine get_compound_field_rank

!> Gets the shape for a specific field in a compound variable.
!!
!! For example, all pumping stations in a model may be collected in a single compound variable set, named 'pumps',
!! each pump has the same list of fields, each with a shape, which is now only for completeness: shape(1:rank) = 0.
!! Use the returned field shape to prepare for calls to the set/get_compound_field routines.
   subroutine get_compound_field_shape(c_var_name, c_field_name, shape) bind(C, name="get_compound_field_shape")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_compound_field_shape
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use iso_c_utils

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of the set variable, e.g., 'pumps'
      character(kind=c_char), intent(in) :: c_field_name(*) !< Name of the field, e.g., 'capacity'
      integer(kind=c_int), intent(inout) :: shape(MAXDIMS) !< Returned shape of the field, e.g., [1]

      ! The fortran name of the attribute name
      character(len=MAXSTRLEN) :: var_name
      character(len=MAXSTRLEN) :: field_name

      ! Store the name
      var_name = char_array_to_string(c_var_name)
      field_name = char_array_to_string(c_field_name)

      shape = 0 ! All fields now still scalar: rank=0, shape irrelevant.

   end subroutine get_compound_field_shape

   subroutine get_1d_double(c_var_name, x) bind(C, name="get_1d_double")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_1d_double

      use iso_c_binding, only: c_double, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(inout) :: x

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call get_var(c_var_name, x)
      end select

   end subroutine get_1d_double

   subroutine get_1d_int(c_var_name, x) bind(C, name="get_1d_int")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_1d_int

      use iso_c_binding, only: c_int, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(inout) :: x

      character(len=strlen(c_var_name)) :: var_name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call get_var(c_var_name, x)
      end select

   end subroutine get_1d_int

   subroutine get_2d_double(c_var_name, xptr) bind(C, name="get_2d_double")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_2d_double

      use iso_c_binding, only: c_double, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(inout) :: xptr

      real(c_double), target, allocatable, save :: x(:, :)

      integer :: i
      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      if (allocated(x)) deallocate (x)
      select case (var_name)
      case ("flowelemcontour_x")
         allocate (x(ndx, get_flow_elem_max_contour()))
         x = 0 ! Set this to nan?
         do i = 1, ndx
            x(i, 1:size(nd(i)%x)) = nd(i)%x
         end do
         xptr = c_loc(x)
      case ("flowelemcontour_y")
         allocate (x(ndx, get_flow_elem_max_contour()))
         x = 0 ! I would like to set nans here. but how?
         do i = 1, ndx
            x(i, 1:size(nd(i)%x)) = nd(i)%y
         end do
         xptr = c_loc(x)
      end select
   end subroutine get_2d_double

   subroutine set_1d_double_at_index(c_var_name, index, value) bind(C, name="set_1d_double_at_index")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_1d_double_at_index

      use m_flow
      use network_data
      use m_flowparameters
      use iso_c_binding, only: c_double, c_char, c_int

      character(kind=c_char), intent(in) :: c_var_name(*) !< Name of a BMI-available variable (should be associated with a 1d double array)
      integer(c_int), value, intent(in) :: index !< Position in array where to set the value. 0-based, pass by value.
      real(c_double), value, intent(in) :: value !< The new value to set in the array.

      character(len=strlen(c_var_name)) :: var_name

      type(tface) :: cell
      integer :: edgeIndex
      double precision :: linkX1, linkX2, linkY1, linkY2
      double precision :: angle

      real(c_double), target :: valuet
      type(c_ptr) :: xptr
      integer(c_int) :: size1(1)

      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case ("s1")
         s1(index + 1) = value
      case ("ucx")
         cell = netcell(index)

         do edgeIndex = 1, cell%n
            ! calculate edge angle
            linkX1 = XK(KN(1, edgeIndex))
            linkX2 = XK(KN(2, edgeIndex))
            linkY1 = YK(KN(1, edgeIndex))
            linkY2 = YK(KN(2, edgeIndex))

            angle = atan((linkY2 - linkY1) / (linkX2 - linkX1))

            u0(cell%lin(edgeIndex)) = value * cos(angle)
            u1(cell%lin(edgeIndex)) = value * cos(angle)
         end do

         ! convert it to the velocity increment on cell interfaces

         ! update u0 - velocity at cell edges

         ! a) calculate weights based on distances to the cell edges
         ! b) project current velocity component to the cell edge tangential velocity
         ! c) add resulting velocity increment to the every cell edge velocity

         ! for every link:
         ! 1. A - angle of the link, relative to the X axis (or calculate it from the link coordinates = atan((y2-y1)/(x2-x1))
         ! 2. for Y component: V_link += Uy * sin(A)
         ! 3. for X component: V_link += Ux * cos(A)

         !ucx(index + 1) = value
      case ("ucy")
         ! convert it to the velocity increment on cell interfaces

         !ucy(index + 1) = value
      case ("unorm")
         u1(index + 1) = value
      case ("zk")
         call update_land_nodes(index + 1, value)
         call land_change_callback()

         !zkdropstep = value - zk(index + 1)
         !call dropland(xz(index + 1), yz(index + 1), 1)
      case ("fstuw")
         fstuw(index + 1) = value
      case ("froer")
         froer(index + 1) = value
      case default
         ! Fall back to any of the auto-generated BMI vars in set_var_slice (with slice count just 1)
         size1(1) = 1
         valuet = value
         xptr = c_loc(valuet) ! To pass on this subroutine argument to set_var_slice as a C pointer
         call set_var_slice(c_var_name, (/index/), size1, xptr)
      end select

   end subroutine set_1d_double_at_index

   subroutine set_1d_double(c_var_name, x) bind(C, name="set_1d_double")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_1d_double

      use iso_c_binding, only: c_double, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(in) :: x

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call set_var(c_var_name, x)
      end select

   end subroutine set_1d_double

   subroutine set_2d_double(c_var_name, x) bind(C, name="set_2d_double")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_2d_double

      use iso_c_binding, only: c_double, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(in) :: x

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call set_var(c_var_name, x)
      end select

   end subroutine set_2d_double

   subroutine set_1d_int(c_var_name, x) bind(C, name="set_1d_int")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_1d_int

      use iso_c_binding, only: c_int, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(in) :: x

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call set_var(c_var_name, x)
      end select

   end subroutine set_1d_int

   subroutine set_2d_int(c_var_name, x) bind(C, name="set_2d_int")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_2d_int

      use iso_c_binding, only: c_int, c_char, c_loc
      use m_flow
      use network_data
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(in) :: x

      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      select case (var_name)
      case default
         call set_var(c_var_name, x)
      end select

   end subroutine set_2d_int

! TODO:
! grid routines

! Utility functions, move these to interop module
! Make functions pure so they can be used as input arguments.
   integer(c_int) pure function strlen(char_array)
      character(c_char), intent(in) :: char_array(MAXSTRLEN)
      integer :: i
      strlen = 0
      do i = 1, size(char_array)
         if (char_array(i) == c_null_char) then
            strlen = i - 1
            exit
         end if
      end do
   end function strlen

   pure function char_array_to_string(char_array, length)
      integer(c_int), intent(in) :: length
      character(c_char), intent(in) :: char_array(length)
      character(len=length) :: char_array_to_string
      integer :: i
      do i = 1, length
         char_array_to_string(i:i) = char_array(i)
      end do
   end function char_array_to_string

   pure function string_to_char_array(string, length)
      integer(c_int), intent(in) :: length
      character(len=length), intent(in) :: string
      character(kind=c_char, len=1) :: string_to_char_array(length + 1)
      integer :: i
      do i = 1, length
         string_to_char_array(i) = string(i:i)
      end do
      string_to_char_array(length + 1) = c_null_char
   end function string_to_char_array

!  TODO move to network_data...
   integer function get_net_elem_max_nodes()
      use network_data
      integer :: k
      ! Determine max nr. of vertices in NetElems (netcells)
      get_net_elem_max_nodes = 0
      do k = 1, nump
         get_net_elem_max_nodes = max(get_net_elem_max_nodes, netcell(k)%n)
      end do
   end function get_net_elem_max_nodes

   integer function get_flow_elem_max_nodes()
      use network_data
      integer :: k
      ! Determine max nr. of flow nodes
      get_flow_elem_max_nodes = 0
      do k = 1, ndx
         get_flow_elem_max_nodes = max(get_flow_elem_max_nodes, size(nd(k)%nod))
      end do
   end function get_flow_elem_max_nodes

   integer function get_flow_elem_max_contour()
      use network_data
      integer :: k
      ! Determine max nr. of flow contour points (TODO: nodes+1?)
      get_flow_elem_max_contour = 0
      do k = 1, ndx
         get_flow_elem_max_contour = max(get_flow_elem_max_contour, size(nd(k)%x))
      end do
   end function get_flow_elem_max_contour

!> Determine max nr. of neighbouring flow nodes,
!! i.e., the current maximum number of flow links per flow node.
   integer function get_flow_elem_max_nbs()
      use m_flowgeom

      integer :: k
      get_flow_elem_max_nbs = 0
      do k = 1, ndx
         get_flow_elem_max_nbs = max(get_flow_elem_max_nbs, nd(k)%lnx)
      end do
   end function get_flow_elem_max_nbs

! Geometry dll functions

!LC: TODO REMOVE
!subroutine triang(cptr_sx, cptr_sy, cptr_sv, c_numS, cptr_dx, cptr_dy, c_numD, cptr_res) bind(C, name="triang")
!    !DEC$ ATTRIBUTES DLLEXPORT :: triang
!    use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
!    use unstruc_model
!    use m_samples
!    use m_sferic, only: jsferic
!    use m_missing, only: dmiss, JINS
!    use m_polygon, only: NPL, xpl, ypl, zpl
!    use m_ec_basic_interpolation, only: triinterp2
!
!    implicit none
!
!    ! parameters
!    type(c_ptr), intent(in)                 :: cptr_sx      ! samples x, y, values
!    type(c_ptr), intent(in)                 :: cptr_sy
!    type(c_ptr), intent(in)                 :: cptr_sv
!    integer(c_int), intent(in)              :: c_numS       ! num samples
!    type(c_ptr), intent(in)                 :: cptr_dx      ! destinations x, y
!    type(c_ptr), intent(in)                 :: cptr_dy
!    integer(c_int), intent(in)              :: c_numD       ! num destination points
!    type(c_ptr), intent(inout)              :: cptr_res     ! return values (ptr to double array)
!
!    ! local variables
!    integer                                 :: numS
!    integer                                 :: numD
!    integer                                 :: jdla = 1
!    real(c_double), pointer                 :: ptr(:)
!    real(c_double), pointer                 :: dx(:)
!    real(c_double), pointer                 :: dy(:)
!    real(c_double), pointer                 :: dRes(:)
!
!    numS = c_numS
!    numD = c_numD
!
!    ! (re)allocate sample arrays
!    if (allocated(XS)) then
!        deallocate(XS,YS,ZS)
!    end if
!    allocate(XS(numS), YS(numS), ZS(numS))
!
!    ! copy ptr's to fortran arrays
!    call c_f_pointer(cptr_sx, ptr, (/numS/))
!    XS(:) = ptr
!
!    call c_f_pointer(cptr_sy, ptr, (/numS/))
!    YS(:) = ptr
!
!    call c_f_pointer(cptr_sv, ptr, (/numS/))
!    ZS(:) = ptr
!
!    call c_f_pointer(cptr_dx, dx, (/numD/))
!    call c_f_pointer(cptr_dy, dy, (/numD/))
!    call c_f_pointer(cptr_res, dRes, (/numD/))
!
!    ! set max stuff
!    NS = numS
!    NSMAX = numS
!
!    ! assign 'missing value' to all elements of dRes
!    dRes = DMISS
!
!    ! call triangulate
!    call triinterp2(dx, dy, dRes, numD, jdla, XS, YS, ZS, NS, dmiss, jsferic, jins, NPL, MXSAM, MYSAM, xpl, ypl, zpl)
!
!end subroutine triang

!LC: TODO REMOVE
!subroutine averaging(cptr_sx, cptr_sy, cptr_sv, c_nums, cptr_cx, cptr_cy, cptr_cxx, cptr_cyy, cptr_cnp, c_numc, c_n6, cptr_res, cptr_meth, cptr_nmin, cptr_csize) bind(C, name="averaging")
!    !DEC$ ATTRIBUTES DLLEXPORT :: averaging
!    use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
!    use unstruc_model
!    use m_sferic, only: jsferic, jasfer3D
!    use m_missing
!    use m_ec_interpolationsettings
!    use kdtree2Factory
!    use m_polygon, only: NPL, xpl, ypl, zpl
!    use m_ec_basic_interpolation, only: AVERAGING2
!
!    implicit none
!
!    ! parameters
!    type(c_ptr),    intent(in)                 :: cptr_sx      ! samples x, y, values
!    type(c_ptr),    intent(in)                 :: cptr_sy
!    type(c_ptr),    intent(in)                 :: cptr_sv
!    integer(c_int), intent(in)                 :: c_nums       ! number of samples
!    type(c_ptr),    intent(in)                 :: cptr_cx      ! destination cell center x, y
!    type(c_ptr),    intent(in)                 :: cptr_cy
!    type(c_ptr),    intent(in)                 :: cptr_cxx     ! destination cell corner x, y
!    type(c_ptr),    intent(in)                 :: cptr_cyy
!    type(c_ptr),    intent(in)                 :: cptr_cnp     ! destination cell corner array lengths
!    integer(c_int), intent(in)                 :: c_numc       ! number of destination cells
!    integer(c_int), intent(in)                 :: c_n6         ! max. cell corner array length
!    type(c_ptr),    intent(inout)              :: cptr_res     ! return values (ptr to double array)
!    integer(c_int), intent(in)                 :: cptr_meth    ! averaging method
!    integer(c_int), intent(in)                 :: cptr_nmin    ! minimum nr of samples for avaraging
!    real(c_double), intent(in)                 :: cptr_csize   ! relative search cell size
!
!    ! local variables
!    real(c_double), pointer                 :: sx(:)
!    real(c_double), pointer                 :: sy(:)
!    real(c_double), pointer                 :: svtmp(:)
!    integer                                 :: nums
!    real(c_double), pointer                 :: cx(:)
!    real(c_double), pointer                 :: cy(:)
!    real(c_double), pointer                 :: cxtmp(:)
!    real(c_double), pointer                 :: cytmp(:)
!    integer, pointer                        :: cnp(:)
!    integer                                 :: numc
!    integer                                 :: n6
!    real(c_double), pointer                 :: res(:)
!    double precision, allocatable           :: sv(:,:)
!    integer, allocatable                    :: ipsam(:)
!    double precision, allocatable           :: cz(:,:)
!    double precision, allocatable           :: cxx(:,:)
!    double precision, allocatable           :: cyy(:,:)
!    integer                                 :: meth
!    integer                                 :: nmin
!    double precision                        :: csize
!    integer                                 :: i, j, k, IAVtmp, NUMMINtmp, INTTYPEtmp, ierr
!    double precision                        :: RCELtmp
!
!    ! cache interpolation settings
!    IAVtmp = IAV
!    NUMMINtmp = NUMMIN
!    INTTYPEtmp = INTERPOLATIONTYPE
!    RCELtmp = RCEL
!
!    ! assign ranges and settings
!    nums = c_nums
!    numc = c_numc
!    n6 = c_n6
!    meth = cptr_meth
!    nmin = cptr_nmin
!    csize = cptr_csize
!
!    !assign pointers
!    call c_f_pointer(cptr_sx, sx, (/nums/))
!    call c_f_pointer(cptr_sy, sy, (/nums/))
!    call c_f_pointer(cptr_sv, svtmp, (/nums/))
!    call c_f_pointer(cptr_cx, cx, (/numc/))
!    call c_f_pointer(cptr_cy, cy, (/numc/))
!    call c_f_pointer(cptr_cxx, cxtmp, (/n6*numc/))
!    call c_f_pointer(cptr_cyy, cytmp, (/n6*numc/))
!    call c_f_pointer(cptr_cnp, cnp, (/numc/))
!    call c_f_pointer(cptr_res, res, (/numc/))
!
!    !allocate & copy to 2d arrays
!    allocate(sv(1, nums), ipsam(nums), cz(1,numc), cxx(n6, numc), cyy(n6, numc))
!
!    sv(1,:) = svtmp(:)
!    ipsam(:) = 1
!    k = 1
!    do i = 1, numc
!       cz(1,i) = DMISS
!       do j=1,n6
!          cxx(j, i) = cxtmp(k)
!          cyy(j, i) = cytmp(k)
!          k = k + 1
!       end do
!    end do
!
!    if(meth > 0 .and. meth < 8) then
!       IAV = meth
!    else
!       goto 1234
!    end if
!
!    if(nmin > 0) then
!       NUMMIN = nmin
!    else
!       goto 1234
!    end if
!
!    if(csize > 0 .and. csize < 10) then
!       RCEL = csize
!    else
!       goto 1234
!    end if
!
!    INTERPOLATIONTYPE = 2
!
!    call build_kdtree(treeglob, nums, sx, sy, ierr, jsferic, dmiss)
!    call averaging2(1, nums, sx, sy, sv, ipsam, cx, cy, cz, numc, cxx, cyy, n6, cnp, 1, &
!                    dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl)
!
!    call delete_kdtree2(treeglob)
!
!    !copy values back
!    res(:) = cz(1,:)
!
!1234 continue
!
!    !unroll & cleanup
!    IAV = IAVtmp
!    NUMMIN = NUMMINtmp
!    INTERPOLATIONTYPE = INTTYPEtmp
!    RCEL = RCELtmp
!    deallocate(sv, ipsam, cz, cxx, cyy)
!
!end subroutine averaging

! Further custom api functions

   subroutine find_cells(c_net_file, c_numCells, c_maxPerCell, cptr_netElemNode) bind(C, name="find_cells")
      !DEC$ ATTRIBUTES DLLEXPORT :: find_cells

      use iso_c_binding, only: c_int, c_char, c_ptr
      use network_data
      use unstruc_files
      use m_netw

      character(kind=c_char), intent(in) :: c_net_file(MAXSTRLEN)
      integer(c_int), intent(out) :: c_numCells
      integer(c_int), intent(out) :: c_maxPerCell
      type(c_ptr), intent(inout) :: cptr_netElemNode ! return values (ptr to 2d int array)

      integer, pointer :: netElemNode(:)
      character(len=strlen(c_net_file)) :: net_file
      integer :: istat
      integer :: maxNodes, ci, ni, i

      call resetFullFlowModel()
      call INIDAT()

      ! read grid filename from c-ptr
      net_file = char_array_to_string(c_net_file, strlen(c_net_file))

      ! read grid from netcdf
      call loadNetwork(net_file, istat, 0)

      nump = 0

      ! find cells
      call FINDCELLS(0)

      maxNodes = 0 ! find max num of vertices per cell
      do ci = 1, nump ! nump = num cells
         maxNodes = max(maxNodes, netcell(ci)%n)
      end do

      ! allocate 1d array (for 2d data)
      allocate (netElemNode(nump * maxNodes))

      ! fill NetElemNode array
      i = 1
      do ci = 1, nump
         do ni = 1, maxNodes
            if (ni <= netcell(ci)%n) then
               netElemNode(i) = netcell(ci)%nod(ni)
            else
               netElemNode(i) = -1
            end if
            i = i + 1
         end do
      end do

      ! copy to c variables
      cptr_netElemNode = c_loc(netElemNode(1)) ! TODO: Fedor/Tiemen: please review whether this still works (was fix for GNU Fortran "Error: Argument 'netelemnode' to 'c_loc' at (1) must be an associated scalar POINTER")
      c_numCells = nump
      c_maxPerCell = maxNodes

   end subroutine find_cells

!> snap polylines to mesh
   subroutine get_snapped_feature(c_feature_type, c_Nin, cptr_xin, cptr_yin, c_Nout, cptr_xout, cptr_yout, cptr_feature_ids, c_ierror) bind(C, name="get_snapped_feature")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_snapped_feature
      use iso_c_binding, only: c_int, c_double, c_char, c_ptr, c_f_pointer
      use m_snappol
      use m_missing
      use m_sferic
      use geometry_module, only: comp_breach_point
      use m_alloc
      implicit none

      character(kind=c_char), intent(in) :: c_feature_type(MAXSTRLEN) !< feature type ('thindam')
      integer(c_int), intent(in) :: c_Nin !< input feature array length
      type(c_ptr), intent(in) :: cptr_xin, cptr_yin !< input feature coordinates

      integer(c_int), intent(out) :: c_Nout !< output array length
      type(c_ptr), intent(inout) :: cptr_xout, cptr_yout !< output feature coordinates
      type(c_ptr), intent(inout) :: cptr_feature_ids !< output feature ids

      integer(c_int), intent(out) :: c_ierror !< ierror (1) or not (0)

      character(len=strlen(c_feature_type)) :: feature_type

      real(c_double), pointer :: ptr(:) ! temporary pointer

      double precision, dimension(:), target, allocatable, save :: xout, yout !< memory leak
      integer, dimension(:), target, allocatable, save :: feature_ids !< memory leak
      integer, dimension(:), target, allocatable :: dummy_ids !< temporary storage for snappnt
      double precision, dimension(:), allocatable :: xintemp, yintemp
      integer :: ntemp
      double precision, dimension(:), allocatable :: xin, yin

      ! Dambreak
      integer :: startIndex, i, noutSnapped, lstart, oldSize
      double precision, dimension(:), target, allocatable :: xSnapped, ySnapped
      double precision, allocatable, dimension(:, :) :: xSnappedLinks, ySnappedLinks
      double precision :: start_location_x, start_location_y, x_breach, y_breach

      c_ierror = 1

      !     read feature type from c-ptr
      feature_type = char_array_to_string(c_feature_type, strlen(c_feature_type))

      !     allocate
      allocate (xin(c_Nin), yin(c_Nin))

      !     copy pointers to fortran array
      call c_f_pointer(cptr_xin, ptr, (/c_Nin/))
      xin(:) = ptr
      call c_f_pointer(cptr_yin, ptr, (/c_Nin/))
      yin(:) = ptr

      ! xin, yin arrays store the coordinates of the feature and are terminated with a dmiss value.
      ! The last valid coordinate is thus stored at index size(xin)-1
      select case (feature_type)
      case ("thindam", "roofs")
         call snappol(c_Nin, xin, yin, DMISS, 1, c_Nout, xout, yout, feature_ids, c_ierror)
      case ("fixedweir", "crosssection", "gate", "weir", "pump")
         call snappol(c_Nin, xin, yin, DMISS, 2, c_Nout, xout, yout, feature_ids, c_ierror)
      case ("obspoint")
         call snappnt(c_Nin, xin, yin, DMISS, c_Nout, xout, yout, feature_ids, c_ierror)
      case ("flownode")
         call snappnt(c_Nin, xin, yin, DMISS, c_Nout, xout, yout, dummy_ids, c_ierror, kout=feature_ids)
      case ("sourcesink")
         startIndex = 1
         i = 1
         ntemp = 0
         oldSize = 0
         if (allocated(xintemp)) deallocate (xintemp)
         if (allocated(yintemp)) deallocate (yintemp)
         do while (i <= size(xin))
            if (xin(i) == dmiss) then
               if (allocated(xintemp)) oldSize = size(xintemp)
               if (i - startIndex > 1) then
                  ! it is a polyline with at least 2 vertexses
                  ntemp = oldSize + 3
                  call realloc(xintemp, ntemp, keepExisting=.true.)
                  call realloc(yintemp, ntemp, keepExisting=.true.)
                  xintemp(oldSize + 1) = xin(startIndex)
                  yintemp(oldSize + 1) = yin(startIndex)
                  xintemp(oldSize + 2) = xin(i - 1)
                  yintemp(oldSize + 2) = yin(i - 1)
                  xintemp(oldSize + 3) = dmiss
                  yintemp(oldSize + 3) = dmiss
               elseif (i - startIndex == 1) then
                  ! just one point
                  ntemp = oldSize + 2
                  call realloc(xintemp, ntemp, keepExisting=.true.)
                  call realloc(yintemp, ntemp, keepExisting=.true.)
                  xintemp(oldSize + 1) = xin(startIndex)
                  yintemp(oldSize + 1) = yin(startIndex)
                  xintemp(oldSize + 2) = dmiss
                  yintemp(oldSize + 2) = dmiss
               else if (i - startIndex < 0) then
                  startIndex = i + 1
                  continue
               end if
               ! the next start index
               startIndex = i + 1
            end if
            i = i + 1
         end do
         if (ntemp > 0) then
            call snappnt(ntemp, xintemp, yintemp, DMISS, c_Nout, xout, yout, feature_ids, c_ierror)
         end if
         ! re-map feature_ids array
         ntemp = 1 ! first feature Index
         do i = 1, size(xout)
            if (xintemp(i) == dmiss) then
               ! Input was a separator value
               ntemp = ntemp + 1 ! hereafter starts a new feature Index
               feature_ids(i) = 0 ! 0 means: separator
            else
               ! Input was an actual sourcesink start or endpoint. Check snapped value now.
               if (xout(i) == dmiss) then
                  ! Input point lies outside of grid.
                  feature_ids(i) = 0 ! 0 means: non-snapped.
               else
                  ! Input was snapped to a grid cell.
                  feature_ids(i) = ntemp ! ntemp is: current feature Index
               end if
            end if
         end do
      case ("dambreak")
         ! Extract polygon and the breach point coordinates
         startIndex = 1
         c_Nout = 0
         i = 1
         oldSize = 0
         if (allocated(xout)) deallocate (xout)
         if (allocated(yout)) deallocate (yout)
         if (allocated(feature_ids)) deallocate (feature_ids)
         do while (i <= size(xin))
            if (xin(i) == dmiss) then
               ntemp = i - startIndex + 1
               ! Deallocation of any previous result
               noutSnapped = 0
               if (allocated(xintemp)) deallocate (xintemp)
               if (allocated(yintemp)) deallocate (yintemp)
               if (allocated(xSnapped)) deallocate (xSnapped)
               if (allocated(ySnapped)) deallocate (ySnapped)
               if (allocated(xSnappedLinks)) deallocate (xSnappedLinks)
               if (allocated(ySnappedLinks)) deallocate (ySnappedLinks)
               ! Allocation and assignment: polyline, dimiss, breach point
               allocate (xintemp(ntemp))
               allocate (yintemp(ntemp))
               xintemp = xin(startIndex:i - 1)
               yintemp = yin(startIndex:i - 1)
               start_location_x = xin(i + 1)
               start_location_y = yin(i + 1)
               ! Determine the flow links intersected by the input polygon, save the coordinates in xSnapped, ySnapped
               call snappol(ntemp, xintemp, yintemp, dmiss, 2, noutSnapped, xSnapped, ySnapped, feature_ids, c_ierror)
               ! Project the breach point into the input polygon, determines the flow link where the breach is starting and gives back the coordinates of the middle point (x_breach, y_breach)
               ! note: default values for jsferic, jasfer3D and dmiss
               allocate (xSnappedLinks(size(xSnapped) - 1, 2))
               allocate (ySnappedLinks(size(ySnapped) - 1, 2))
               xSnappedLinks(:, 1) = xSnapped(1:size(xSnapped) - 1)
               ySnappedLinks(:, 1) = ySnapped(1:size(ySnapped) - 1)
               xSnappedLinks(:, 2) = xSnapped(2:size(xSnapped))
               ySnappedLinks(:, 2) = ySnapped(2:size(ySnapped))
               call comp_breach_point(start_location_x, start_location_y, xintemp, yintemp, ntemp, xSnappedLinks, ySnappedLinks, lstart, x_breach, y_breach, jsferic, jasfer3D, dmiss)
               ! Save the results (snapped line, dmiss, snapped point, dmiss)
               if (allocated(xout)) oldSize = size(xout)
               c_Nout = c_Nout + noutSnapped + 2
               call realloc(xout, c_Nout, keepExisting=.true.)
               call realloc(yout, c_Nout, keepExisting=.true.)
               xout(oldSize + 1:oldSize + noutSnapped) = xSnapped
               yout(oldSize + 1:oldSize + noutSnapped) = ySnapped
               xout(oldSize + noutSnapped + 1) = x_breach
               yout(oldSize + noutSnapped + 1) = y_breach
               xout(oldSize + noutSnapped + 2) = dmiss
               yout(oldSize + noutSnapped + 2) = dmiss
               call realloc(feature_ids, c_Nout, keepExisting=.true.)
               feature_ids(oldSize + noutSnapped + 1) = feature_ids(c_Nout - 3) + 1
               feature_ids(oldSize + noutSnapped + 2) = 0
               startIndex = i + 2
               i = startIndex
            end if
            i = i + 1
         end do
      case default
         call snapbnd(feature_type, c_Nin, xin, yin, dmiss, c_Nout, xout, yout, feature_ids, c_ierror)
      end select
      if (c_ierror /= 0 .or. c_Nout == 0) goto 1234

      cptr_xout = c_loc(xout)
      cptr_yout = c_loc(yout)
      cptr_feature_ids = c_loc(feature_ids)

      c_ierror = 0
1234  continue

      if (allocated(xin)) deallocate (xin)
      if (allocated(yin)) deallocate (yin)

      return
   end subroutine get_snapped_feature

   subroutine write_netgeom(c_net_file) bind(C, name="write_netgeom")
      !DEC$ ATTRIBUTES DLLEXPORT :: write_netgeom

      use iso_c_binding, only: c_int, c_char, c_ptr
      use network_data
      use unstruc_netcdf

      character(kind=c_char), intent(in) :: c_net_file(MAXSTRLEN)
      character(len=strlen(c_net_file)) :: netgeom_file

      ! read grid filename from c-ptr
      netgeom_file = char_array_to_string(c_net_file, strlen(c_net_file))

      if (netstat /= NETSTAT_OK) then
         call findcells(0)
      end if
      call unc_write_net(netgeom_file, janetcell=1, janetbnd=0, jaidomain=0, jaiglobal_s=0, iconventions=2)
   end subroutine write_netgeom

   subroutine write_partition_metis(c_netfile_in, c_netfile_out, c_npart, c_jacontiguous) bind(C, name="write_partition_metis")
      !DEC$ ATTRIBUTES DLLEXPORT :: write_partition_metis

      use iso_c_binding, only: c_int, c_char, c_ptr
      use network_data
      use unstruc_netcdf
      use m_partitioninfo
      use m_netw
      use m_commandline_option
      use unstruc_model, only: md_pmethod

      character(kind=c_char), intent(in) :: c_netfile_in(MAXSTRLEN)
      character(kind=c_char), intent(in) :: c_netfile_out(MAXSTRLEN)
      integer(c_int), intent(in) :: c_npart
      integer(c_int), intent(in) :: c_jacontiguous
      character(MAXSTRLEN) :: netfile_in
      character(MAXSTRLEN) :: netfile_out
      integer :: npart
      integer :: jacontiguous
      integer :: istat
      integer :: ierror

      netfile_in = char_array_to_string(c_netfile_in, strlen(c_netfile_in))
      numfiles = 1
      inputfiles(1) = netfile_in
      call resetFullFlowModel()
      call INIDAT()

      istat = 0

      if (netstat /= NETSTAT_OK) then
         call findcells(0)
      end if

      if (c_npart < 2) then
         return
      else
         npart = c_npart
      end if

      if (c_jacontiguous < 1) then
         jacontiguous = 0
      else
         jacontiguous = 1
      end if

      call cosphiunetcheck(1)

      call partition_METIS_to_idomain(npart, jacontiguous, md_pmethod, 0)

      ndomains = npart

      call generate_partition_pol_from_idomain(ierror)

      netfile_out = char_array_to_string(c_netfile_out, strlen(c_netfile_out))

      if (ndomains > 1) then
         call partition_write_domains(netfile_out, 6, 1, 0, md_partugrid)
      end if

   end subroutine write_partition_metis

! SPvdP: 1D, dry points not accounted for
   subroutine write_partition_pol(c_netfile_in, c_netfile_out, c_polfile) bind(C, name="write_partition_pol")
      !DEC$ ATTRIBUTES DLLEXPORT :: write_partition_pol

      use iso_c_binding, only: c_int, c_char, c_ptr
      use network_data
      use unstruc_netcdf
      use m_partitioninfo
      use m_polygon
      use unstruc_files
      use m_netw
      use m_commandline_option
      use m_reapol

      character(kind=c_char), intent(in) :: c_netfile_in(MAXSTRLEN)
      character(kind=c_char), intent(in) :: c_netfile_out(MAXSTRLEN)
      character(kind=c_char), intent(in) :: c_polfile(MAXSTRLEN)
      character(MAXSTRLEN) :: netfile_in
      character(MAXSTRLEN) :: netfile_out
      character(MAXSTRLEN) :: polfile
      integer :: minp, istat

      netfile_in = char_array_to_string(c_netfile_in, strlen(c_netfile_in))
      numfiles = 1
      inputfiles(1) = netfile_in
      call resetFullFlowModel()
      call INIDAT()

      istat = 0

      if (netstat /= NETSTAT_OK) then
         call findcells(0)
      end if

      call cosphiunetcheck(1)

      polfile = char_array_to_string(c_polfile, strlen(c_polfile))

      call newfil(minp, polfile)
      call reapol(minp, 0)

      if (npl > 1) then ! use polygons
         call generate_partitioning_from_pol()
      end if

      netfile_out = char_array_to_string(c_netfile_out, strlen(c_netfile_out))

      if (ndomains > 1) then
         call partition_write_domains(netfile_out, 6, 1, 0, md_partugrid)
      end if

   end subroutine write_partition_pol

!> Get the flow links indexes crossed by a polyline
!!
!! numberOfInputVertices        :: size of input vertices
!! c_xVerticesCoordinates       :: pointer to x array of coordinates
!! c_yVerticesCoordinates       :: pointer to y array of coordinates
!! numberOfOutputIndexes        :: number of intersected flow links
!! c_indexes                    :: flow links indexes
   function get_snapped_flow_links_indexes(numberOfInputVertices, c_xVerticesCoordinates, c_yVerticesCoordinates, startIndex, numberOfOutputIndexes, c_indexes) result(ierr) bind(C, name="get_snapped_flow_links_indexes")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_snapped_flow_links_indexes

      use m_flowgeom
      use gridoperations
      use array_module
      use m_missing
      use m_crosspoly

      implicit none
      integer(c_int), intent(in) :: numberOfInputVertices
      type(c_ptr), intent(in) :: c_xVerticesCoordinates, c_yVerticesCoordinates
      integer(c_int), intent(out) :: numberOfOutputIndexes
      type(c_ptr), intent(inout) :: c_indexes
      integer, intent(in) :: startIndex
      !return error code
      integer :: ierr
      !locals
      double precision :: xa, ya, xb, yb, xm, ym, crpm, distanceStartPolygon
      double precision, pointer :: xVerticesCoordinates(:), yVerticesCoordinates(:)
      integer :: l, k1, k2, crossed, isec
      integer, allocatable, target, save :: indexes(:) !as commented above, this is a memory leak of lnx integers

      ierr = 0

      call c_f_pointer(c_xVerticesCoordinates, xVerticesCoordinates, (/numberOfInputVertices/))
      call c_f_pointer(c_yVerticesCoordinates, yVerticesCoordinates, (/numberOfInputVertices/))

      if (allocated(indexes)) then
         deallocate (indexes)
      end if

      allocate (indexes(lnx))

      numberOfOutputIndexes = 0
      do l = 1, lnx
         k1 = ln(1, l); k2 = ln(2, l)
         xa = xz(k1); ya = yz(k1)
         xb = xz(k2); yb = yz(k2)
         call crosspoly(xa, ya, xb, yb, xVerticesCoordinates, yVerticesCoordinates, numberOfInputVertices, xm, ym, crpm, crossed, isec, distanceStartPolygon)
         if (crossed == 1) then
            numberOfOutputIndexes = numberOfOutputIndexes + 1
            indexes(numberOfOutputIndexes) = l
         end if
      end do

      ierr = convert_start_index(indexes, imiss, 1, startIndex)
      c_indexes = c_loc(indexes)

   end function get_snapped_flow_links_indexes

end module bmi
