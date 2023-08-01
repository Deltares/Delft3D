!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module bmi
  use m_delwaq2_main
  use m_delwaq1
  use m_getidentification
  use iso_c_binding
  implicit none

  ! Define some global constants
  character(*), parameter :: PREFIX = "WAQ"
  !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
  integer(c_int) :: MAXNAMES = 100
  integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = 1024
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
  integer(c_int), BIND(C, name="MAXDIMS") :: MAXDIMS = 6
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

contains

  integer(c_int) function set_var(c_key, xptr) bind(C, name="set_var")
    !DEC$ ATTRIBUTES DLLEXPORT::set_var
    use iso_c_binding, only: c_char, c_ptr
    use iso_c_utils
    use delwaq2_global_data
    use dhcommand
    implicit none
    character(kind=c_char),intent(in)    :: c_key(MAXSTRLEN)
    type(c_ptr), value    ,intent(in)    :: xptr
    !
    character(kind=c_char),dimension(:), pointer :: c_value => null()
    character(MAXSTRLEN)                         :: key_given
    character(MAXSTRLEN)                         :: value_given
    integer                                      :: argc
    integer                                      :: argnew
    integer                                      :: iarg
    integer                                      :: errorcode
    integer                                      :: i

    ! Store the key and value
    key_given = char_array_to_string(c_key)
    call c_f_pointer(xptr, c_value,[MAXSTRLEN])
    value_given = " "
    if (associated(c_value)) then
       do i=1,MAXSTRLEN
          if (c_value(i) == c_null_char) exit
          value_given(i:i) = c_value(i)
       enddo
    endif
    !
    argnew = 2
    if (value_given(1:1) .eq. ' ')  argnew = 1
    if (key_given(1:1) .eq. ' ')    argnew = 0
    !
    if (argnew .gt. 0) then
       ! Add new arguments to argv
       if ( allocated( argv_tmp ) ) deallocate( argv_tmp )
       if ( allocated( argv )) then
          argc = size(argv, 1)
          allocate (argv_tmp(argc))
          do iarg = 1, argc
             argv_tmp(iarg) = argv(iarg)
          end do
          deallocate( argv )
       else
          argc = 0
       end if
       allocate( argv(argc+argnew) )
       do iarg = 1, argc
          argv(iarg) = argv_tmp(iarg)
       end do
       argv(argc+1) = key_given
       if(argnew.eq.2) then
          argv(argc+2) = value_given
       endif
    endif
    set_var = 0
  end function set_var

  ! Control

  !> The initialize() function accepts a string argument that
  !! gives the name (and path) of its "main input file", called
  !! a configuration file. This function should perform all tasks
  !! that are to take place before entering the model's time loop.
  integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
    !DEC$ ATTRIBUTES DLLEXPORT::initialize
    use iso_c_binding, only: c_char
    use iso_c_utils
    use delwaq2_global_data
    use dhcommand
    use m_actions

    implicit none
    character(kind=c_char),intent(in)    :: c_config_file(MAXSTRLEN)
    character(len=strlen(c_config_file)) :: runid_given
    integer                          :: argc
    integer                          :: iarg
    integer                          :: errorcode


    ! Store the name
    runid_given = char_array_to_string(c_config_file)

    ! Add runid_given before the current arguments list
    if ( allocated( argv_tmp ) ) deallocate( argv_tmp )
    if ( allocated( argv )) then
       argc = size(argv, 1)
       allocate (argv_tmp(argc))
       do iarg = 1, argc
          argv_tmp(iarg) = argv(iarg)
       end do
       deallocate( argv )
    else
       argc = 0
    end if
    allocate( argv(argc+2) )
    argv(1) = 'delwaq.dll' ! argument 0 is the executable name on the command line
    argv(2) = runid_given
    do iarg = 1, argc
       argv(iarg+2) = argv_tmp(iarg)
    end do
    argc = argc + 2

    call delwaq1(argc, argv, errorcode)
    if (errorcode==0) then
       call delwaq2_global_data_initialize(runid_given)
       call dlwqmain( ACTION_INITIALISATION, argc, argv, dlwqd )
       call delwaq2_global_data_copy( dlwqd )
       initialize = 0
    else
       initialize = 1
    endif
  end function initialize

  subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string
    use iso_c_binding, only: c_char
    use iso_c_utils

    character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)
    character(len=MAXSTRLEN) :: name
    character(len=120)       :: idstr

    call getidentification(idstr)
    name = trim(idstr)
    c_version_string = string_to_char_array(trim(name))
  end subroutine get_version_string

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
    use delwaq_version_module
    use iso_c_binding, only: c_char
    use iso_c_utils
    character(kind=c_char), intent(in)    :: c_att_name(MAXSTRLEN)  !< Attribute name as C-delimited character string.
    character(kind=c_char), intent(  out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.

    character(len=strlen(c_att_name)) :: att_name
    character(len=MAXSTRLEN)          :: att_value

    ! Store the name
    att_name = char_array_to_string(c_att_name)

    select case (att_name)
    case ('model_name')
       att_value = component_name
    case ('version')
       att_value = major_minor_buildnr
    case ('author_name')
       att_value = company
    case default
       att_value = 'unknown attribute'
    end select

    c_att_value = string_to_char_array(trim(att_value))
end subroutine get_attribute

  integer function update(dt) bind(C, name="update")
    !DEC$ ATTRIBUTES DLLEXPORT :: update
    use delwaq2_global_data
    use messagehandling
    use iso_c_binding, only: c_double
    use m_actions
    use m_sysi

    implicit none

    real(c_double), value, intent(in) :: dt
    integer :: update_steps, step
    character(len=20), dimension(0) :: argv_dummy

    update_steps = nint(dt * dlwqd%tscale) / idt
    if(intsrt == 2) then
       ! Correct update_steps for delwaq scheme 2, which does a double time step every call
       update_steps = (update_steps + 1) / 2
    end if
    do step = 1, update_steps
      call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    enddo
    update = 0
  end function update

  integer function finalize() bind(C, name="finalize")
    !DEC$ ATTRIBUTES DLLEXPORT :: finalize
    use delwaq2_global_data
    use m_actions

    implicit none
    character(len=20), dimension(0) :: argv_dummy
    integer :: ierr


    call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    call dlwqmain( ACTION_FINALISATION, 0, argv_dummy, dlwqd )
    call delwaq2_global_data_finalize

    finalize = 0
  end function finalize


  subroutine get_start_time(t) bind(C, name="get_start_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    use m_sysi
    implicit none
    real(c_double), intent(out) :: t

    t = real(dlwqd%otime,8) + real(itstrt,8) / real(dlwqd%tscale,8)
  end subroutine get_start_time


  subroutine get_end_time(t) bind(C, name="get_end_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    use m_sysi
    implicit none
    real(c_double), intent(out) :: t

    t = real(dlwqd%otime,8) + real(itstop,8) / real(dlwqd%tscale,8)
  end subroutine get_end_time


  subroutine get_time_step(dt) bind(C, name="get_time_step")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    use m_sysi
    implicit none
    real(c_double), intent(out) :: dt

    dt = real(idt,8) / real(dlwqd%tscale,8)
  end subroutine get_time_step


  subroutine get_current_time(t) bind(C, name="get_current_time")
    !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
    use delwaq2_global_data
    use iso_c_binding, only: c_double
    use m_sysi
    implicit none
    real(c_double), intent(out) :: t
    integer current

    t = real(dlwqd%otime,8) + real(dlwqd%itime,8) / real(dlwqd%tscale,8)
  end subroutine get_current_time

end module bmi
