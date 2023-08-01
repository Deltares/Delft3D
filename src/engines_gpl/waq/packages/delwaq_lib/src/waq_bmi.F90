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

module waq_bmi
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

   !
   !
   !==============================================================================
   subroutine main() bind(C, name="main")
      !DEC$ ATTRIBUTES DLLEXPORT :: main
      ! Somehow intel fortran compiler expects a main routine in the dll.
   end subroutine main

 
  ! Control

  !> The initialize() function accepts a string argument that
  !! gives the name (and path) of its "main input file", called
  !! a configuration file. This function should perform all tasks
  !! that are to take place before entering the model's time loop.
  integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
    !DEC$ ATTRIBUTES DLLEXPORT :: initialize
    use iso_c_binding, only: c_char
    use delwaq2_global_data
    use m_waq_bmi_data 
    use MessageHandling

    implicit none
    character(kind=c_char),intent(in)    :: c_config_file(MAXSTRLEN)
    character(len=strlen(c_config_file)) :: runid_given
    integer                              :: argc
    integer                              :: iarg
    integer                              :: errorcode
    
    external :: waq_bmi_errorhandler ! MDK 01-08-2023: the arguments are incorrect, so for the moment I put it external to get it to compile

    
    call SetMessageHandling(write2screen = .true. , &
                        useLog = .true., &
                        callback = waq_bmi_errorhandler, &
                        thresholdLevel = LEVEL_INFO, &
                        thresholdLevel_log = LEVEL_INFO, &
                        thresholdLevel_callback = LEVEL_INFO, &
                        reset_counters = .true., &
                        prefix_logging = KERNELNAME)

    ! Store the name
    runid_given = char_array_to_string(c_config_file, MAXSTRLEN)

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

    call mess(LEVEL_INFO, "Initialize...")
    !call delwaq1(argc, argv, errorcode)
    if (errorcode==0) then
       !call delwaq2_global_data_initialize(runid_given)
       !call dlwqmain( ACTION_INITIALISATION, argc, argv, dlwqd )
       !call delwaq2_global_data_copy( dlwqd )
       ! MDK 31-07-2023 why do we do this copy step? What does it achieve?

       call waq_bmi_data_init()
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
    use iso_c_binding, only: c_double
    use m_actions
    use m_sysi
    use m_waq_bmi_data

    implicit none

    real(c_double), value, intent(in) :: dt
    integer :: update_steps, step
    character(len=20), dimension(0) :: argv_dummy

    update_steps = nint(dt * dlwqd%tscale) / idt
    if(intsrt == 2) then
      ! MDK 31-07-2023 this can be removed after when clean up the integration schemes, planned for september 2023
       ! Correct update_steps for delwaq scheme 2, which does a double time step every call
       update_steps = (update_steps + 1) / 2
    end if
    call realloc_waq_bmi_data()
    do step = 1, update_steps
      call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    enddo
    update = 0
  end function update

  integer function finalize() bind(C, name="finalize")
    !DEC$ ATTRIBUTES DLLEXPORT :: finalize
    use delwaq2_global_data
    use m_actions
    use m_waq_bmi_data

    implicit none
    character(len=20), dimension(0) :: argv_dummy
    integer :: ierr


    call dlwqmain( ACTION_SINGLESTEP, 0, argv_dummy, dlwqd )
    call dlwqmain( ACTION_FINALISATION, 0, argv_dummy, dlwqd )
    call delwaq2_global_data_finalize
    call waq_bmi_data_finalize()

    finalize = 0
  end function finalize


  !
!
!==============================================================================
!> Return a pointer to the variable
subroutine get_var(c_var_name, var_ptr) bind(C, name="get_var")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var
   use iso_c_binding, only: c_double, c_char, c_loc
   use iso_c_utils, only: strlen
   use m_waq_bmi_data
   !
   ! Parameters
   character(kind=c_char), intent(in)    :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
   type(c_ptr)           , intent(inout) :: var_ptr
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
   select case(var_name)
   !
   ! D-Flow FM ==> WAQ_BMI
   ! When DIMR asks WAQ_BMI for this variable and it is not associated yet, WAQ_BMI must notify DIMR
   ! that it is able to accept that parameter by returning a dummy pointer
   !
   case("flow_xcc")
       if (.not.associated(fm_xzw)) then
           return
       endif
       var_ptr = c_loc(fm_xzw)
       continue
   case("flow_ycc")
       if (.not.associated(fm_yzw)) then
           return
       endif
       var_ptr = c_loc(fm_yzw)
       continue
   case("z_level_cc")
       if (.not.associated(fm_z_level)) then
           return
       endif
       var_ptr = c_loc(fm_z_level)
       continue
   case("kbot")
       if (.not.associated(fm_kbot)) then
           return
       endif
       var_ptr = c_loc(fm_kbot)
       continue
   case("ktop")
       if (.not.associated(fm_ktop)) then
           return
       endif
       var_ptr = c_loc(fm_ktop)
       continue
   case("water_depth_cc")
       if (.not.associated(fm_water_depth)) then
           return
       endif
       var_ptr = c_loc(fm_water_depth)
       continue
   case("velocity_x_cc")
       if (.not.associated(fm_velocity_x)) then
           return
       endif
       var_ptr = c_loc(fm_velocity_x)
       continue
   case("velocity_y_cc")
       if (.not.associated(fm_velocity_y)) then
           return
       endif
       var_ptr = c_loc(fm_velocity_y)
       continue
   case("rho_cc")
       if (.not.associated(fm_rho)) then
           return
       endif
       var_ptr = c_loc(fm_rho)
       continue
   case("constituents")
       if (.not.associated(fm_constituents)) then
           return
       endif
       var_ptr = c_loc(fm_constituents)
       continue
   case("constituents_names")
       if (.not.associated(fm_namcon)) then
           return
       endif
       var_ptr = c_loc(fm_namcon)
       continue
   case("isalt")
       if (.not.associated(fm_isalt)) then
           allocate(fm_isalt)
           fm_isalt = -999
       endif
       var_ptr = c_loc(fm_isalt)
       continue
   case("itemp")
       if (.not.associated(fm_itemp)) then
           allocate(fm_itemp)
           fm_itemp = -999
       endif
       var_ptr = c_loc(fm_itemp)
       continue
   case("runid")
       var_ptr = c_loc(runid)
       continue
   ! !
   ! ! WAQ_BMI ==> D-Flow FM 
   ! ! MDK 01-08-2023: currently not implemented. Should we want to have a two-way coupling
   ! ! we can do it here.  
   ! !  
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
   use string_module
   use MessageHandling
   use m_waq_bmi_data
   
   ! MDK 31-07-2023 from old version
      use iso_c_utils
      use delwaq2_global_data
      use dhcommand
      implicit none
      
      character(MAXSTRLEN)                         :: value_given
      integer                                      :: argc
      integer                                      :: argnew
      integer                                      :: iarg
      integer                                      :: errorcode
      integer                                      :: i
   ! end MDK 31-07-2023 from old version

   !
   ! Parameters
   character(kind=c_char), intent(in) :: c_var_name(*)
   type(c_ptr), value, intent(in)     :: var_ptr
   !
   ! Locals
   integer                            :: slen
   real(c_double)        , pointer    :: var_1d_double_ptr(:)
   real(c_double)        , pointer    :: var_2d_double_ptr(:,:)
   integer(c_int)        , pointer    :: var_0d_int_ptr
   integer(c_int)        , pointer    :: var_1d_int_ptr(:)
   character(kind=c_char), pointer    :: var_1d_char_ptr(:)
   character(len=1024)   , pointer    :: valuestr
   character(len=strlen(c_var_name))  :: var_name
   !
   ! Body
   !
   ! Store the name and pointer to the value
   !
   ! D-Flow FM ==> WAQ_BMI
   ! DIMR delivers a pointer to the variable in FM
   ! DIMR will first call set_var("parameter_shape",shape_array)
   !      where shape_array is an integer(6) array containing the dimensions of "parameter"
   ! Then DIMR will call set_var("parameter",parameter_pointer)
   var_name = char_array_to_string(c_var_name)
   call c_f_pointer(var_ptr, valuestr)
   slen = index(valuestr, c_null_char) - 1
   select case (str_tolower(var_name))
   case ("flow_xcc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndx = var_1d_int_ptr(1)
   case ("flow_xcc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndx /))
       fm_xzw => var_1d_double_ptr
   case ("flow_ycc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndx = var_1d_int_ptr(1)
   case ("flow_ycc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndx /))
       fm_yzw => var_1d_double_ptr
   case ("z_level_cc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndkx = var_1d_int_ptr(1)
   case ("z_level_cc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndkx /))
       fm_z_level => var_1d_double_ptr
   case ("kbot_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndx   = var_1d_int_ptr(1)
   case ("kbot")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ fm_ndx /))
       fm_kbot => var_1d_int_ptr
   case ("ktop_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndx   = var_1d_int_ptr(1)
   case ("ktop")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ fm_ndx /))
       fm_ktop => var_1d_int_ptr
   case ("water_depth_cc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndx = var_1d_int_ptr(1)
   case ("water_depth_cc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndx /))
       fm_water_depth => var_1d_double_ptr
   case ("velocity_x_cc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndkx = var_1d_int_ptr(1)
   case ("velocity_x_cc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndkx /))
       fm_velocity_x => var_1d_double_ptr
   case ("velocity_y_cc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndkx = var_1d_int_ptr(1)
   case ("velocity_y_cc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndkx /))
       fm_velocity_y => var_1d_double_ptr
   case ("rho_cc_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_ndkx = var_1d_int_ptr(1)
   case ("rho_cc")
       call c_f_pointer(var_ptr, var_1d_double_ptr, (/ fm_ndkx /))
       fm_rho => var_1d_double_ptr
   case ("constituents_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_numconst = var_1d_int_ptr(1)
       fm_ndkx     = var_1d_int_ptr(2)
   case ("constituents")
       call c_f_pointer(var_ptr, var_2d_double_ptr, (/ fm_numconst, fm_ndkx /))
       fm_constituents => var_2d_double_ptr
   case ("constituents_names_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       fm_numconst = var_1d_int_ptr(1)
       fm_namlen   = var_1d_int_ptr(2)
       !
       ! Constituent names are copied instead of referenced
       allocate(character(fm_namlen) :: fm_namcon(fm_numconst))
   case ("constituents_names")
       call c_f_pointer(var_ptr, var_1d_char_ptr, (/ fm_numconst * fm_namlen /))
       fm_namcon = transfer(var_1d_char_ptr,fm_namcon)
   case ("isalt")
       call c_f_pointer(var_ptr, var_0d_int_ptr)
       fm_isalt => var_0d_int_ptr
   case ("itemp")
       call c_f_pointer(var_ptr, var_0d_int_ptr)
       fm_itemp => var_0d_int_ptr
   case ("runid_shape")
       call c_f_pointer(var_ptr, var_1d_int_ptr, (/ 6 /))
       slen = var_1d_int_ptr(2)
   !case ("runid")
   !    runid = valuestr(1:slen)
   case ("skipuniqueid")
       select case (str_tolower(valuestr(1:slen)))
       case ("1", "yes", "true")
           skipuniqueid = .true.
       case ("0", "no", "false")
           skipuniqueid = .false.
       end select
   case default
       call mess(LEVEL_INFO, "'set_var(", var_name, ")' not implemented")
   end select   


   ! MDK 31-07-2023 This is from the old routine. To be seen what to do with it.
   ! (needed for running waq through dimr)
   ! Store the key and value
   !var_name = char_array_to_string(c_var_name, strlen(c_var_name))
   !call c_f_pointer(var_ptr, valuestr)
   !value_given = " "
   !if (associated(valuestr)) then
   !   do i=1,MAXSTRLEN
   !      if (valuestr(i) == c_null_char) exit
   !      value_given(i:i) = valuestr(i)
   !   enddo
   !endif
   !
   argnew = 2
   if (value_given(1:1) .eq. ' ')  argnew = 1
   if (var_name(1:1) .eq. ' ')    argnew = 0
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
      argv(argc+1) = var_name
      if(argnew.eq.2) then
         argv(argc+2) = value_given
      endif
   endif
!    set_var = 0
!  end function set_var

end subroutine set_var





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

  !==============================================================================
   pure function char_array_to_string(char_array, length)
      use iso_c_utils, only: strlen

      integer(c_int), intent(in) :: length
      character(c_char),intent(in) :: char_array(length)
      character(len=length) :: char_array_to_string
      integer :: i
      do i = 1, min(strlen(char_array), length)
         char_array_to_string(i:i) = char_array(i)
      enddo
      do i = min(strlen(char_array), length) + 1, length
         char_array_to_string(i:i) = ' '
      enddo
   end function char_array_to_string
     

  end module waq_bmi
  
  
     
   !
    !
    !==============================================================================
  ! MDK 01-08-2023: the arguments are incorrect, so for the moment I put it outside of the module to get it to compile
    subroutine waq_bmi_errorhandler(level)
        use MessageHandling
        integer, intent(in) :: level
        if (level >= LEVEL_ERROR) then
            call throwexception()
        endif
    end subroutine waq_bmi_errorhandler

