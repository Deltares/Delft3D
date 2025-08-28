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

!> The routines in this module are based on the "BMI"-like interface
!! used by DIMR. This is not exactly BMI 2.0 and in some cases actually
!! clashes with that standard.
!!
!! To solve the above mentioned incompatibility the BMI 2.0 routines
!! that clash with the current (october 2023) version of DIMR are
!! prefixed with "BMI2_" in their name. This is a unique substring,
!! so once we can rely on BMI 2.0 to be useable, we can simply
!! remove that.
module m_waq_bmi_api_dimr

    use m_waq_bmi_adapter !< should only depend on the bmi_api (BMI 2.0)
    use m_waq_precision

    implicit none

contains

    !> The subroutine get_version_string() returns the version of DELWAQ that is being used.
    subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string

        character(kind=c_char), intent(out) :: c_version_string(MAXSTRLEN)

        c_version_string = ext_get_version_string()
    end subroutine get_version_string

    !> Returns a static attribute (i.e. an attribute that does not change
    !! from one model application to the next) of the model (as a string)
    !! When passed any attribute name from the following list:
    !! * model_name
    !! * version      (e.g. 2.0.1)
    !! * author_name
    !! * grid_type
    !! * time_step_type
    !! * step_method   (explicit, implicit, semi_implicit, iterative)
    subroutine get_attribute(c_att_name, c_att_value) bind(C, name="get_attribute")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_attribute
        character(kind=c_char), intent(in) :: c_att_name(MAXSTRLEN)  !< Attribute name as C-delimited character string.
        character(kind=c_char), intent(out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.

        c_att_value = ext_get_attribute(c_att_name)
    end subroutine get_attribute

    !>    Run the model calculation over a given interval (dt)
    integer function update(dt) bind(C, name="update")
        !DEC$ ATTRIBUTES DLLEXPORT :: update
        use iso_c_binding, only: c_double

        real(c_double), value, intent(in) :: dt  !< Interval over which the calculation is to be done.

        real(c_double) :: t_current_time
        real(c_double) :: tupdate
        integer(kind=int_wp) :: error_code

        !! May involve one or more internal timesteps
        error_code = ext_get_current_time(t_current_time)
        tupdate = t_current_time + dt
        update = update_until(tupdate)
    end function update

    !> Return the start time of the calculation
    subroutine get_start_time(t) bind(C, name="get_start_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
        real(c_double), intent(out) :: t

        ! Note: The time frame and unit are to the discretion of the components.
        !       Due to the implementation of DIMR
        integer(kind=int_wp) :: rc

        rc = BMI2_get_start_time(t)
    end subroutine get_start_time

    !> Return the end time of the calculation
    subroutine get_end_time(t) bind(C, name="get_end_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
        real(c_double), intent(out) :: t

        integer(kind=int_wp) :: rc

        rc = BMI2_get_end_time(t)
    end subroutine get_end_time

    !> Return the end time of the calculation
    subroutine get_time_step(dt) bind(C, name="get_time_step")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
        real(c_double), intent(out) :: dt

        integer(kind=int_wp) :: rc

        rc = BMI2_get_time_step(dt)
    end subroutine get_time_step

    !> Return the current time in the calculation
    subroutine get_current_time(t) bind(C, name="get_current_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
        real(c_double), intent(out) :: t
        integer(kind=int_wp) :: current

        integer(kind=int_wp) :: rc

        rc = BMI2_get_current_time(t)
    end subroutine get_current_time

    !> Thin layer to accomdate DIMR and BMI 2.0 - calls get_var_ptr, which is a BMI function
    subroutine get_var(c_key, xptr) bind(C, name="get_var")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_var

        character(kind=c_char), intent(in) :: c_key(MAXSTRLEN)
        type(c_ptr), intent(inout) :: xptr

        integer(kind=int_wp) :: rc

        rc = get_value_ptr(c_key, xptr)
    end subroutine get_var

end module m_waq_bmi_api_dimr
