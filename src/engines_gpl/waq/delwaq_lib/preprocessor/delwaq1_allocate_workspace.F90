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
module m_delwaq1_allocate_workspace
    use m_waq_precision
    use m_delwaq1_write_messages
    use m_error_status

    implicit none

contains

    !>\file
    !>                    delwaq1_allocate_workspace

    subroutine delwaq1_allocate_workspace(status)
        use m_delwaq1_data

        implicit none

        logical :: parsing_error

        type(error_status), intent(inout) :: status !< current error status

        !  allocate workspace
        imax = max_int_size
        rmax = max_real_size
        cmax = max_char_size

        allocate (iar(imax), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunrep, '(A,I6,A,I12)') " ERROR: allocating integer work array:", ierr_alloc, " with length:", imax
            status%ierr = 1
            call delwaq1_write_messages(status)
            return
        end if
        allocate (real_array(rmax), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunrep, '(A,I6,A,I12)') " ERROR: allocating real work array:", ierr_alloc, " with length:", rmax
            status%ierr = 1
            call delwaq1_write_messages(status)
            return
        end if
        allocate (char_arr(cmax), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunrep, '(A,I6,A,I12)') " ERROR: allocating character work array:", ierr_alloc, " with length:", cmax
            status%ierr = 1
            call delwaq1_write_messages(status)
            return
        end if

    end subroutine delwaq1_allocate_workspace
end module m_delwaq1_allocate_workspace
