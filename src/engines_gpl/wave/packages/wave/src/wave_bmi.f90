module bmi
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
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
  use iso_c_binding
  use dwaves_version_module

  implicit none

  ! Define some global constants

  ! We can't return an array of pointers with an undertimened length so
  ! we fix it to something, in this case 100.

  character(*), parameter :: PREFIX = "WAVE"
  !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
  integer(c_int) :: MAXNAMES = 100
  integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = 1024
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
  integer(c_int), BIND(C, name="MAXDIMS") :: MAXDIMS = 6
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

  integer, target :: wave_mode = -999
contains
!
!
!
!===============================================================================
!> The initialize() function accepts a string argument that
!! gives the name (and path) of its "main input file", called
!! a configuration file. This function should perform all tasks
!! that are to take place before entering the model's time loop.
integer(c_int) function initialize(c_mdw_file) result(c_iresult) bind(C, name="initialize")
  !DEC$ ATTRIBUTES DLLEXPORT :: initialize
  use iso_c_binding, only: c_char
  use wave_main, only: wave_main_init
  !
  ! Global
  character(kind=c_char),intent(in)    :: c_mdw_file(MAXSTRLEN)
  !
  ! Local
  character(len=strlen(c_mdw_file)) :: mdw_file
  integer                           :: retval
  !
  ! Body
  mdw_file   = char_array_to_string(c_mdw_file, strlen(c_mdw_file))
  retval     = wave_main_init(wave_mode, mdw_file)
  c_iresult  = retval
end function initialize
!
!
!
!===============================================================================
subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string
  use iso_c_binding, only: c_char
  !
  ! Global
  character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)
  !
  ! Local
  character(len=MAXSTRLEN) :: full_version
  !
  ! Body
  call getfullversionstring_dwaves(full_version) 
  c_version_string = string_to_char_array(trim(full_version), len(trim(full_version)))
end subroutine get_version_string
!
!

subroutine get_attribute(c_att_name, c_att_value) bind(C, name="get_attribute")
!DEC$ ATTRIBUTES DLLEXPORT :: get_attribute
   character(kind=c_char), intent(in)    :: c_att_name(MAXSTRLEN)  !< Attribute name as C-delimited character string.
   character(kind=c_char), intent(  out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.

   character(len=strlen(c_att_name)) :: att_name
   character(len=MAXSTRLEN)          :: att_value
   character(len=255) :: versionstr
   integer :: from, thru
 
   ! Store the name
   att_name = char_array_to_string(c_att_name, strlen(c_att_name))

   select case (att_name)
   case ('model_name'   )
      att_value = product_name
   case ('version')
      call getfullversionstring_dwaves(att_value)
      from = index(att_value,'Version ') + 8
      thru = index(att_value(from:len(versionstr)),',')-1
      att_value=att_value(from:from+thru-1)
   case ('author_name')
      att_value = company
   end select

   c_att_value = string_to_char_array(trim(att_value), len(trim(att_value)))
end subroutine get_attribute

!===============================================================================
integer function update(dt) bind(C, name="update")
  !DEC$ ATTRIBUTES DLLEXPORT :: update
  use precision
  use messagehandling
  use wave_main
  use iso_c_binding, only: c_double
  !
  ! Global
  real(c_double), value, intent(in) :: dt
  !
  ! Local
  integer  :: retval
  real(hp) :: stepsize
  !
  ! Body
  stepsize = real(dt,hp)
  retval   = wave_main_step(stepsize)
  update   = retval
end function update
!
!
!
!===============================================================================
subroutine update_until(t) bind(C, name="update_until")
  use iso_c_binding, only: c_double
  !
  ! Global
  real(c_double),intent(in) :: t
  !
  ! Body
  write(*,*) "ERROR:BMI_update_until not implemented yet"
  call wavestop(1, "ERROR:BMI_update_until not implemented yet")
end subroutine update_until
!
!
!
!===============================================================================
integer function finalize() bind(C, name="finalize")
  !DEC$ ATTRIBUTES DLLEXPORT :: finalize
  use wave_main
  !
  ! Local
  integer :: retval
  !
  ! Body
  retval   = wave_main_finish()
  finalize = retval
end function finalize
!
!
!
!===============================================================================
subroutine get_start_time(t) bind(C, name="get_start_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
  use iso_c_binding, only: c_double
  use precision
  use wave_main, only: wavedata
  !
  ! Global
  real(c_double), intent(out) :: t
  !
  ! Body
  if (wavedata%time%refdate == 0) then
      t = -999.0_hp
  else
      t = 0.0_hp
  endif
end subroutine get_start_time
!
!
!
!===============================================================================
subroutine get_end_time(t) bind(C, name="get_end_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
  use iso_c_binding, only: c_double
  use precision
  use wave_data, only: stand_alone, flow_online, flow_mud_online
  use swan_input, only: swan_run
  !
  ! Global
  real(c_double), intent(out) :: t
  !
  ! Body
  if (wave_mode == stand_alone) then
     !
     ! Return the last time specified in the mdw-file (in seconds)
     !
     t = swan_run%timwav(swan_run%nttide) * 60.0_hp
  elseif (wave_mode == flow_online) then
     !
     ! No time available to return
     !
     t = -1.0_hp
  elseif (wave_mode == flow_mud_online) then
     write(*,*) "ERROR:BMI_get_end_time: not implemented for mode=flow_mud_online"
     call wavestop(1, "ERROR:BMI_get_end_time: not implemented for mode=flow_mud_online")
     t = -999.0
  else
     write(*,*) "ERROR:BMI_get_end_time: Value of wave_mode not recognized"
     call wavestop(1, "ERROR:BMI_get_end_time: Value of wave_mode not recognized")
     t = -999.0
  endif
end subroutine get_end_time
!
!
!
!===============================================================================
subroutine get_time_step(dt) bind(C, name="get_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
  use iso_c_binding, only: c_double
  use precision
  use wave_data, only: stand_alone, flow_online, flow_mud_online
  use swan_input, only: swan_run
  !
  ! Global
  real(c_double), intent(out) :: dt
  !
  ! Body
  if (wave_mode == stand_alone) then
     !
     ! Return the last time specified in the mdw-file (in seconds), minus the start time: zero
     !
     dt = swan_run%timwav(swan_run%nttide) * 60.0_hp - 0.0_hp
  elseif (wave_mode == flow_online) then
     !
     ! No timestep available to return
     !
     dt = -1.0_hp
  elseif (wave_mode == flow_mud_online) then
     write(*,*) "ERROR:BMI_get_time_step: not implemented for mode=flow_mud_online"
     call wavestop(1, "ERROR:BMI_get_time_step: not implemented for mode=flow_mud_online")
     dt = -999.0
  else
     write(*,*) "ERROR:BMI_get_time_step: Value of wave_mode not recognized"
     call wavestop(1, "ERROR:BMI_get_time_step: Value of wave_mode not recognized")
     dt = -999.0
  endif
end subroutine get_time_step
!
!
!
!===============================================================================
subroutine get_current_time(t) bind(C, name="get_current_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
  use iso_c_binding, only: c_double
  use precision
  use wave_main, only: wavedata
  !
  ! Global
  real(c_double), intent(out) :: t
  !
  ! Body
  t = wavedata%time%timsec
end subroutine get_current_time
!
!
!
!===============================================================================
subroutine get_var(c_var_name, x) bind(C, name="get_var")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var
  ! Return a pointer to the variable
  use iso_c_binding, only: c_double, c_char, c_loc
  use wave_mpi, only: engine_comm_world
  !
  ! Global
  character(kind=c_char), intent(in) :: c_var_name(*)
  type(c_ptr), intent(inout) :: x
  !
  ! Local
  character(len=strlen(c_var_name)) :: var_name
  !
  ! Body
  var_name = char_array_to_string(c_var_name, strlen(c_var_name))
  !
  ! Please be conservative in adding variables here. Most variables 
  ! can be computed outside.
  ! You can generate extra variables here. 
  select case(var_name)
  case("mode")
     x = c_loc(wave_mode)
     return
  case("engine_comm_world")
     x = c_loc(engine_comm_world)
     return
  end select
  !
  ! The case statement above should be replaced by an
  ! automatically generated interface:
  !include "bmi_get_var.inc"
end subroutine get_var
!
!
!
!===============================================================================
subroutine set_var(c_key, c_value) bind(C, name="set_var")
  !DEC$ ATTRIBUTES DLLEXPORT :: set_var
  ! Set a key-value pair
  use iso_c_binding, only: c_char
  !
  ! Global
  character(kind=c_char), intent(in) :: c_key(*)
  character(kind=c_char), intent(in) :: c_value(*)
  !
  ! Local
  character(len=strlen(c_key))   :: f_key
  character(len=strlen(c_value)) :: f_value
  !
  ! Body
  f_key = char_array_to_string(c_key, strlen(c_key))
  if (f_key /= "mode") then
      write(*,'(2a)') "ERROR:BMI_set_var: unrecognized key:", trim(f_key)
      call wavestop(1, "ERROR:BMI_set_var: unrecognized key:"//trim(f_key))
  endif
  f_value = char_array_to_string(c_value, strlen(c_value))
  select case(f_value)
  case("stand-alone")
      wave_mode = 0
  case("online with DflowFM", "online with Delft3D-FLOW")
      wave_mode = 1
  case default
      write(*,'(2a)') "ERROR:BMI_set_var: unrecognized value:", trim(f_value)
  end select
end subroutine set_var
!
!
!
!!!===============================================================================
!!subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
!!  !DEC$ ATTRIBUTES DLLEXPORT :: set_var
!!  ! Return a pointer to the variable
!!  use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
!!  !
!!  ! Global
!!  character(kind=c_char), intent(in) :: c_var_name(*)
!!  type(c_ptr), value, intent(in) :: xptr
!!  !
!!  ! Local
!!  character(kind=c_char), pointer   :: x_char_ptr(:)
!!  character(len=strlen(c_var_name)) :: var_name ! The fortran name of the attribute name
!!  character(len=MAXSTRLEN)          :: var_value
!!  !
!!  ! Body
!!  var_name = char_array_to_string(c_var_name, strlen(c_var_name))
!!  if (var_name /= "mode") then
!!      write(*,'(2a)') "ERROR:BMI_set_var: unrecognized var_name:", trim(var_name)
!!      call wavestop(1, "ERROR:BMI_set_var: unrecognized var_name:"//trim(var_name))
!!  endif
!!  call c_f_pointer(xptr, x_char_ptr, [MAXSTRLEN]) 
!!  var_value = char_array_to_string(x_char_ptr, MAXSTRLEN)
!!  select case(var_value)
!!  case("stand-alone")
!!      wave_mode = 0
!!  case("online with DflowFM", "online with Delft3D-FLOW")
!!      wave_mode = 1
!!  case default
!!      write(*,'(2a)') "ERROR:BMI_set_var: unrecognized value:", trim(var_value)
!!  end select
!!end subroutine set_var
!!!
!
!
!===============================================================================
! Utility functions, move these to interop module
! Make functions pure so they can be used as input arguments.
integer(c_int) pure function strlen(char_array)
  !
  ! Global
  character(c_char), intent(in) :: char_array(MAXSTRLEN)
  !
  ! Local
  integer :: inull, i
  !
  ! Body
  strlen = 0
  do i = 1, size(char_array)
    if (char_array(i) .eq. C_NULL_CHAR) then
      strlen = i-1
      exit
    end if
  end do
end function strlen
!
!
!
!===============================================================================
pure function char_array_to_string(char_array, length)
  !
  ! Global
  integer(c_int), intent(in) :: length
  character(c_char),intent(in) :: char_array(length)
  !
  ! Return value
  character(len=length) :: char_array_to_string
  !
  ! Local
  integer :: i
  !
  ! Body
  do i = 1, length
    char_array_to_string(i:i) = char_array(i)
  enddo
end function char_array_to_string
!
!
!
!===============================================================================
pure function string_to_char_array(string, length)
  !
  ! Global
  integer(c_int),intent(in) :: length
  character(len=length), intent(in) :: string
  !
  ! Return value
  character(kind=c_char,len=1) :: string_to_char_array(length+1)
  !
  ! Local
  integer :: i
  !
  ! Body
  do i = 1, length
    string_to_char_array(i) = string(i:i)
  enddo
  string_to_char_array(length+1) = C_NULL_CHAR
end function string_to_char_array





end module bmi
