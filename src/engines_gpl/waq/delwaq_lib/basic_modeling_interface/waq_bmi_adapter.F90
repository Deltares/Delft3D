!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

!> The routines in this module are based on the BMI 2.0 standard.
!! They are a small layer on top of our generic external api (waq_external_access_layer)
!! and are an adapter layer for the BMI 2.0 standard.
!
! The authorative description of BMI 2.0 can be found here:
!   https://csdms.colorado.edu/wiki/BMI   (introduction)
!   https://bmi.readthedocs.io/en/stable/ (latest documentation)
module m_waq_bmi_adapter

    use m_waq_external_access_layer !< should only depend on external api that contains the logic
    use iso_c_binding

    implicit none

    character(*), parameter :: PREFIX = EXT_PREFIX
    !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
    integer(c_int), bind(C, name="MAXSTRLEN") :: MAXSTRLEN = EXT_MAXSTRLEN
    !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
    integer(c_int), bind(C, name="MAXDIMS") :: MAXDIMS = EXT_MAXDIMS
    !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

contains

    !> The set_var function lets the caller set a variable within DELWAQ.
    !! Currently only used to manipulate key-value pairs that could appear
    !! on the command line.
    integer(c_int) function set_var(c_key, xptr) bind(C, name="set_var")
        !DEC$ ATTRIBUTES DLLEXPORT::set_var
        character(kind=c_char), intent(in) :: c_key(MAXSTRLEN)  !< Incoming string, determines the variable to be set
        type(c_ptr), value, intent(in) :: xptr                  !< C-pointer to the actual value to be picked up by DELWAQ

        set_var = ext_set_var(c_key, xptr)
    end function set_var

    ! Control

    !>    The initialize() function accepts a string argument that
    !!    gives the name (and path) of its "main input file", called
    !!    a configuration file. This function should perform all tasks
    !!    that are to take place before entering the model's time loop.
    !!
    !!    Returns 0 if successful, 1 otherwise.
    integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
        !DEC$ ATTRIBUTES DLLEXPORT::initialize
        character(kind=c_char), intent(in) :: c_config_file(MAXSTRLEN)  !< Name of the DELWAQ input file

        initialize = ext_initialize(c_config_file)
    end function initialize

    !> Run the model calculation up to a given model time
    !!
    !! Always returns 0
    integer function update_until(tupdate) bind(C, name="update_until")
        !DEC$ ATTRIBUTES DLLEXPORT :: update
        use iso_c_binding, only: c_double

        real(c_double), value, intent(in) :: tupdate   !< Time until which the calculation is to be run.

        update_until = ext_update_until(tupdate)
    end function update_until

    !> Finish the model calculation (close files, clean up, etc.)
    !!
    !! Always returns 0
    integer function finalize() bind(C, name="finalize")

        !DEC$ ATTRIBUTES DLLEXPORT :: finalize
        finalize = ext_finalize()
    end function finalize

    !> Return the start time of the calculation
    !!
    !! Always returns 0
    integer function BMI2_get_start_time(t) bind(C, name="BMI2_get_start_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_start_time
        real(c_double), intent(out) :: t

        BMI2_get_start_time = ext_get_start_time(t)
    end function BMI2_get_start_time

    !> Return the end time of the calculation
    !!
    !! Always returns 0
    integer function BMI2_get_end_time(t) bind(C, name="BMI2_get_end_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_end_time
        real(c_double), intent(out) :: t

        BMI2_get_end_time = ext_get_end_time(t)
    end function BMI2_get_end_time

    !> Return the end time of the calculation
    !!
    !! Always returns 0
    integer function BMI2_get_time_step(dt) bind(C, name="BMI2_get_time_step")
        !DEC$ ATTRIBUTES DLLEXPORT ::BMI2_get_time_step
        real(c_double), intent(out) :: dt

        BMI2_get_time_step = ext_get_time_step(dt)
    end function BMI2_get_time_step

    !> Return the current time in the calculation
    !!
    !! Always returns 0
    integer function BMI2_get_current_time(t) bind(C, name="BMI2_get_current_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_current_time
        real(c_double), intent(out) :: t

        BMI2_get_current_time = ext_get_current_time(t)
    end function BMI2_get_current_time

    !> The get_value_ptr function returns a pointer to the actual variable in the model component,
    !! so that the caller can read the current or set a new value.
    !! The consequence is that we do not know whether a new value has been set or not.
    !! Also noteworthy: DIMR only accepts double-precision numbers.
    !!
    !! Returns 0 on success, 1 if the key was not recognised or an index was out of scope.
    integer function get_value_ptr(c_key, xptr) bind(C, name="get_value_ptr")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_var
        character(kind=c_char), intent(in) :: c_key(MAXSTRLEN)
        type(c_ptr), intent(inout) :: xptr

        get_value_ptr = ext_get_value_ptr(c_key, xptr)
    end function get_value_ptr
end module m_waq_bmi_adapter
