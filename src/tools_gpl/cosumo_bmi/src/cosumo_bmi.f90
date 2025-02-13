!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!
module m_cosumo_bmi_version
   implicit none
   !
   character(*), public, parameter :: program_name = 'COSUMO_BMI'
   character(*), public, parameter :: program_version = '0.01.00'
   character(*), public, parameter :: program_company = 'Deltares'
end module m_cosumo_bmi_version

module m_cosumo_bmi
   use m_nfl_data
   !
   implicit none

contains
!
!
!==============================================================================
   subroutine main() bind(C, name="main")
      !DEC$ ATTRIBUTES DLLEXPORT :: main
      ! Somehow intel fortran compiler expects a main routine in the dll, it is required since interactor is used (and win calls)
   end subroutine main
!
!
!==============================================================================
!> The initialize() function accepts a string argument that
!! gives the name (and path) of its "main input file", called
!! a configuration file. This function should perform all tasks
!! that are to take place before entering the model's time loop.
   integer(c_int) function initialize(c_config_file) result(c_iresult) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT :: initialize
      use iso_c_binding, only: c_char
      use m_nfl_data
      !
      ! Parameters
      character(kind=c_char), intent(in) :: c_config_file(MAXSTRLEN)
      !
      ! Locals
      integer :: ierr
      external :: cosumo_bmi_errorhandler
      !
      ! Body
      !
      ! Logging
      open (newunit=diafile, file="cosumo_bmi.dia", action="write", iostat=ierr)
      call SetMessageHandling(write2screen=.true., &
                              useLog=.true., &
                              lunMessages=diafile, &
                              callback=cosumo_bmi_errorhandler, &
                              thresholdLevel=LEVEL_INFO, &
                              thresholdLevel_log=LEVEL_INFO, &
                              thresholdLevel_callback=LEVEL_INFO, &
                              reset_counters=.true., &
                              prefix_logging=KERNELNAME)
      call mess(LEVEL_INFO, "Initialize...")
      !
      ! Initialization
      call nfl_data_init()
      infile = char_array_to_string(c_config_file, MAXSTRLEN)
      c_iresult = 0
   end function initialize
!
!
!==============================================================================
   integer function update(dt) bind(C, name="update")
      !DEC$ ATTRIBUTES DLLEXPORT :: update
      use iso_c_binding, only: c_double
      use m_read_cosumo_file
      use m_write_ff2nf_files
      use m_read_nf2ff_files
      !
      ! Parameters
      real(c_double), value, intent(in) :: dt
      !
      ! Body
      ! TO DO: Set these values properly
      current_time = current_time + dt
      if (skipuniqueid) then
         uniqueid = ' '
      end if
      !
      ! Phase 1: read dimensions
      call read_cosumo_file(READ_DIMENSIONS_ONLY)
      call realloc_nfl_data()
      !
      ! Phase 2: read the rest and process
      call read_cosumo_file(READ_FULL_CONTENTS)
      call write_ff2nf_files()
      call read_nf2ff_files()
      update = 0
   end function update
!
!
!==============================================================================
   integer function finalize() bind(C, name="finalize")
      !DEC$ ATTRIBUTES DLLEXPORT :: finalize
      !
      ! Locals
      integer :: ierr
      !
      ! Body
      call nfl_data_finalize()
      finalize = 0
      close (diafile, iostat=ierr)
   end function finalize
!
!
!==============================================================================
!> Return a pointer to the variable
   subroutine get_var(c_var_name, var_ptr) bind(C, name="get_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var
      use iso_c_binding, only: c_double, c_char, c_loc
      !
      ! Parameters
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout) :: var_ptr
      !
      ! Locals
      character(len=strlen(c_var_name)) :: var_name !<  The fortran name of the attribute name
      !
      ! Body
      !
      ! Store the name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))

      ! Please be conservative in adding variables here. Most variables
      ! can be computed outside.
      ! You can generate extra variables here.
      select case (var_name)
         !
         ! D-Flow FM ==> COSUMO_BMI
         ! When DIMR asks COSUMO_BMI for this variable and it is not associated yet, COSUMO_BMI must notify DIMR
         ! that it is able to accept that parameter by returning a dummy pointer
         !
      case ("flow_xcc")
         if (.not. associated(fm_xzw)) then
            return
         end if
         var_ptr = c_loc(fm_xzw)
         continue
      case ("flow_ycc")
         if (.not. associated(fm_yzw)) then
            return
         end if
         var_ptr = c_loc(fm_yzw)
         continue
      case ("z_level_cc")
         if (.not. associated(fm_z_level)) then
            return
         end if
         var_ptr = c_loc(fm_z_level)
         continue
      case ("kbot")
         if (.not. associated(fm_kbot)) then
            return
         end if
         var_ptr = c_loc(fm_kbot)
         continue
      case ("ktop")
         if (.not. associated(fm_ktop)) then
            return
         end if
         var_ptr = c_loc(fm_ktop)
         continue
      case ("water_depth_cc")
         if (.not. associated(fm_water_depth)) then
            return
         end if
         var_ptr = c_loc(fm_water_depth)
         continue
      case ("velocity_x_cc")
         if (.not. associated(fm_velocity_x)) then
            return
         end if
         var_ptr = c_loc(fm_velocity_x)
         continue
      case ("velocity_y_cc")
         if (.not. associated(fm_velocity_y)) then
            return
         end if
         var_ptr = c_loc(fm_velocity_y)
         continue
      case ("rho_cc")
         if (.not. associated(fm_rho)) then
            return
         end if
         var_ptr = c_loc(fm_rho)
         continue
      case ("constituents")
         if (.not. associated(fm_constituents)) then
            return
         end if
         var_ptr = c_loc(fm_constituents)
         continue
      case ("constituents_names")
         if (.not. associated(fm_namcon)) then
            return
         end if
         var_ptr = c_loc(fm_namcon)
         continue
      case ("isalt")
         if (.not. associated(fm_isalt)) then
            allocate (fm_isalt)
            fm_isalt = -999
         end if
         var_ptr = c_loc(fm_isalt)
         continue
      case ("itemp")
         if (.not. associated(fm_itemp)) then
            allocate (fm_itemp)
            fm_itemp = -999
         end if
         var_ptr = c_loc(fm_itemp)
         continue
      case ("runid")
         var_ptr = c_loc(runid)
         continue
         !
         ! COSUMO_BMI ==> D-Flow FM
         !
      case ("nf_q_source")
         if (.not. associated(nf_q_source)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_q_source)
         continue
      case ("nf_q_intake")
         if (.not. associated(nf_q_intake)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_q_intake)
         continue
      case ("nf_const")
         if (.not. associated(nf_const)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_const)
         continue
      case ("nf_intake")
         if (.not. associated(nf_intake)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_intake)
         continue
      case ("nf_sink")
         if (.not. associated(nf_sink)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_sink)
         continue
      case ("nf_sour")
         if (.not. associated(nf_sour)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_sour)
         continue
      case ("nf_const_operator")
         if (.not. associated(nf_const_operator)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_const_operator)
         continue
      case ("nf_src_mom")
         if (.not. associated(d0)) then
            ! leave var_ptr undefined. It will be obtained via get_var later on
            return
         end if
         var_ptr = c_loc(nf_src_mom)
         continue
      case default
         call mess(LEVEL_ERROR, "'get_var(", var_name, ")' not implemented")
      end select
   end subroutine get_var
!
!
!==============================================================================
!> Provides a pointer to the variable
   subroutine set_var(c_var_name, var_ptr) bind(C, name="set_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
      use string_module, only: str_tolower
      !
      ! Parameters
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), value, intent(in) :: var_ptr
      !
      ! Locals
      integer :: slen, i
      real(c_double), pointer :: var_1d_double_ptr(:)
      real(c_double), pointer :: var_2d_double_ptr(:, :)
      integer(c_int), pointer :: var_0d_int_ptr
      integer(c_int), pointer :: var_1d_int_ptr(:)
      character(kind=c_char), pointer :: var_1d_char_ptr(:)
      character(len=1024), pointer :: valuestr
      character(len=strlen(c_var_name)) :: var_name
      !
      ! Body
      !
      ! Store the name and pointer to the value
      !
      ! D-Flow FM ==> COSUMO_BMI
      ! DIMR delivers a pointer to the variable in FM
      ! DIMR will first call set_var("parameter_shape",shape_array)
      !      where shape_array is an integer(6) array containing the dimensions of "parameter"
      ! Then DIMR will call set_var("parameter",parameter_pointer)
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      call c_f_pointer(var_ptr, valuestr)
      slen = index(valuestr, c_null_char) - 1
      select case (str_tolower(var_name))
      case ("flow_xcc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndx = var_1d_int_ptr(1)
      case ("flow_xcc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndx])
         fm_xzw => var_1d_double_ptr
      case ("flow_ycc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndx = var_1d_int_ptr(1)
      case ("flow_ycc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndx])
         fm_yzw => var_1d_double_ptr
      case ("z_level_cc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndkx = var_1d_int_ptr(1)
      case ("z_level_cc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndkx])
         fm_z_level => var_1d_double_ptr
      case ("kbot_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndx = var_1d_int_ptr(1)
      case ("kbot")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [fm_ndx])
         fm_kbot => var_1d_int_ptr
      case ("ktop_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndx = var_1d_int_ptr(1)
      case ("ktop")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [fm_ndx])
         fm_ktop => var_1d_int_ptr
      case ("water_depth_cc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndx = var_1d_int_ptr(1)
      case ("water_depth_cc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndx])
         fm_water_depth => var_1d_double_ptr
      case ("velocity_x_cc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndkx = var_1d_int_ptr(1)
      case ("velocity_x_cc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndkx])
         fm_velocity_x => var_1d_double_ptr
      case ("velocity_y_cc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndkx = var_1d_int_ptr(1)
      case ("velocity_y_cc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndkx])
         fm_velocity_y => var_1d_double_ptr
      case ("rho_cc_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_ndkx = var_1d_int_ptr(1)
      case ("rho_cc")
         call c_f_pointer(var_ptr, var_1d_double_ptr, [fm_ndkx])
         fm_rho => var_1d_double_ptr
      case ("constituents_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_numconst = var_1d_int_ptr(1)
         fm_ndkx = var_1d_int_ptr(2)
      case ("constituents")
         call c_f_pointer(var_ptr, var_2d_double_ptr, [fm_numconst, fm_ndkx])
         fm_constituents => var_2d_double_ptr
      case ("constituents_names_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         fm_numconst = var_1d_int_ptr(1)
         fm_namlen = var_1d_int_ptr(2)
         !
         ! Constituent names are copied instead of referenced
         allocate (character(fm_namlen) :: fm_namcon(fm_numconst))
      case ("constituents_names")
         call c_f_pointer(var_ptr, var_1d_char_ptr, [fm_numconst * fm_namlen])
         fm_namcon = transfer(var_1d_char_ptr, fm_namcon)
      case ("isalt")
         call c_f_pointer(var_ptr, var_0d_int_ptr)
         fm_isalt => var_0d_int_ptr
      case ("itemp")
         call c_f_pointer(var_ptr, var_0d_int_ptr)
         fm_itemp => var_0d_int_ptr
      case ("runid_shape")
         call c_f_pointer(var_ptr, var_1d_int_ptr, [6])
         runid_length = var_1d_int_ptr(2)
      case ("runid")
         runid = valuestr(1:runid_length)
      case ("skipuniqueid")
         select case (str_tolower(valuestr(1:slen)))
         case ("1", "yes", "true")
            skipuniqueid = .true.
         case ("0", "no", "false")
            skipuniqueid = .false.
         end select
      case default
         call mess(LEVEL_ERROR, "'set_var(", var_name, ")' not implemented")
      end select
   end subroutine set_var
!
!
!==============================================================================
!> Returns the shape of a variable, i.e., an array with length equal to this variables's rank.
!! NOTE: the reported shape is in C-compatible row-major order.
!!
!! count(shape) = rank
!! @see get_var_rank
   subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
      use iso_c_binding, only: c_int, c_char, c_loc
      !
      ! Parameters
      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(inout) :: shape(MAXDIMS)
      !
      ! Locals
      character(len=strlen(c_var_name)) :: var_name
      !
      ! Body
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      shape = [0, 0, 0, 0, 0, 0]
      !
      ! NOTE: report the shape below in row-major order (so, C-style, not FORTRAN-style)
      select case (var_name)
      case ("nf_q_source")
         shape(1) = nf_num_dif
         return
      case ("nf_q_intake")
         shape(1) = nf_num_dif
         return
      case ("nf_const")
         shape(1) = nf_num_dif
         shape(2) = fm_numconst
         return
      case ("nf_intake")
         shape(1) = nf_num_dif
         shape(2) = nf_intake_idimMAX
         shape(3) = 3 ! X, Y, Z
         return
      case ("nf_sink")
         shape(1) = nf_num_dif
         shape(2) = nf_sink_idimMAX
         shape(3) = 6 ! X, Y, Z, S, H, B
         return
      case ("nf_sour")
         shape(1) = nf_num_dif
         shape(2) = nf_sour_idimMAX
         shape(3) = 8 ! X, Y, Z, S, H, B, Umag, Udir
         return
      case ("nf_const_operator")
         shape(1) = nf_num_dif
         shape(2) = 10 ! Length of character string
         return
      case ("nf_src_mom")
         shape(1) = nf_num_dif
         return
      end select
   end subroutine get_var_shape
!
!
!==============================================================================
!> Copied from unstruc_bmi.F90
!! Returns a static attribute (i.e. an attribute that does not change
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
      use m_cosumo_bmi_version
      !
      ! Parameters
      character(kind=c_char), intent(in) :: c_att_name(MAXSTRLEN) !< Attribute name as C-delimited character string.
      character(kind=c_char), intent(out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.
      !
      ! Locals
      character(len=strlen(c_att_name)) :: att_name
      character(len=MAXSTRLEN) :: att_value
      !
      ! Body
      ! Store the name
      att_name = char_array_to_string(c_att_name, strlen(c_att_name))

      select case (att_name)
      case ('model_name')
         att_value = program_name
      case ('version')
         att_value = program_version
      case ('author_name')
         att_value = program_company
         ! TO DO    case ('reference_time')
         !        att_value = reference_time
      end select

      c_att_value = string_to_char_array(trim(att_value), len(trim(att_value)))
   end subroutine get_attribute
!
!
!==============================================================================
   subroutine get_start_time(t) bind(C, name="get_start_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t
      t = 0.0_hp
   end subroutine get_start_time
!
!
!==============================================================================
   subroutine get_end_time(t) bind(C, name="get_end_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t
      t = -999.0_hp
   end subroutine get_end_time
!
!
!==============================================================================
   subroutine get_time_step(dt) bind(C, name="get_time_step")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: dt
      dt = -999.0_hp
   end subroutine get_time_step
!
!
!==============================================================================
   subroutine get_current_time(t) bind(C, name="get_current_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
      use iso_c_binding, only: c_double
      real(c_double), intent(out) :: t
      t = current_time
   end subroutine get_current_time
!
!
!==============================================================================
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
!
!
!==============================================================================
   pure function char_array_to_string(char_array, length)
      integer(c_int), intent(in) :: length
      character(c_char), intent(in) :: char_array(length)
      character(len=length) :: char_array_to_string
      integer :: i
      do i = 1, min(strlen(char_array), length)
         char_array_to_string(i:i) = char_array(i)
      end do
      do i = min(strlen(char_array), length) + 1, length
         char_array_to_string(i:i) = ' '
      end do
   end function char_array_to_string
!
!
!==============================================================================
   pure function string_to_char_array(string, length)
      character(len=length), intent(in) :: string
      integer(c_int), intent(in) :: length
      character(kind=c_char, len=1) :: string_to_char_array(length + 1)
      integer :: i
      do i = 1, length
         string_to_char_array(i) = string(i:i)
      end do
      string_to_char_array(length + 1) = c_null_char
   end function string_to_char_array

end module m_cosumo_bmi

!
!
!==============================================================================
subroutine cosumo_bmi_errorhandler(level)
   use MessageHandling
   integer, intent(in) :: level
   if (level >= LEVEL_ERROR) then
      call throwexception()
   end if
end subroutine cosumo_bmi_errorhandler
