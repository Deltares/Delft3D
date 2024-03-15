!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module m_error_status
    use m_waq_precision

    implicit none

    type, public :: error_status
        integer(kind = int_wp) :: ierr = 0   !< cumulative error count
        integer(kind = int_wp) :: iwar = 0   !< cumulative warning count
        integer(kind = int_wp) :: noinfo = 0 !< cumulative info count

    contains
        procedure :: initialize => initialize_status
        procedure :: increase_error_count, increase_error_count_with
        procedure :: increase_warning_count, increase_warning_count_with
        procedure :: increase_info_count
        procedure :: sync => sync_error_status

    end type error_status

contains

    subroutine initialize_status(this, error_count, warning_count, info_count)
        !< initializes error status
        class(error_status) :: this !< the error_status instance

        integer(kind = int_wp), intent(in) :: error_count   !< cumulative error count
        integer(kind = int_wp), intent(in) :: warning_count !< cumulative warning count
        integer(kind = int_wp), intent(in) :: info_count    !< cumulative informative message count

        this%ierr = error_count
        this%iwar = warning_count
        this%noinfo = info_count

    end subroutine initialize_status

    subroutine increase_error_count(this)
        !< increases the error count by 1
        class(error_status) :: this !< the error_status instance

        this%ierr = this%ierr + 1
    end subroutine increase_error_count

    subroutine increase_error_count_with(this, count)
        !< increases the error count by count
        class(error_status) :: this !< the error_status instance
        integer(kind = int_wp) :: count !< amount to increase the error count with

        this%ierr = this%ierr + count
    end subroutine increase_error_count_with

    subroutine increase_warning_count(this)
        !< increases the warning count by 1
        class(error_status), intent(inout) :: this !< the error_status instance

        this%iwar = this%iwar + 1
    end subroutine increase_warning_count

    subroutine increase_warning_count_with(this, count)
        !< increases the warning count by count
        class(error_status) :: this !< the error_status instance
        integer(kind = int_wp) :: count !< amount to increase the warning count with

        this%iwar = this%iwar + count
    end subroutine increase_warning_count_with

    subroutine increase_info_count(this)
        !< increases the informative message count by 1
        class(error_status), intent(inout) :: this !< the error_status instance

        this%noinfo = this%noinfo + 1
    end subroutine increase_info_count

    subroutine sync_error_status(this, error_count, warning_count, info_count)
        !< synchronize the status with provided variables
        class(error_status), intent(inout) :: this !< the error_status instance
        integer(kind = int_wp), intent(inout) :: error_count   !< cumulative error count
        integer(kind = int_wp), intent(inout) :: warning_count !< cumulative warning count
        integer(kind = int_wp), intent(inout) :: info_count    !< cumulative informative message count

        error_count = this%ierr
        warning_count = this%iwar
        info_count = this%noinfo
    end subroutine sync_error_status
end module m_error_status
