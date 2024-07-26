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
module m_dlwqf1
    use m_waq_precision

    implicit none

contains


    !> Create a zero based row pointer and direct mapping 
    !! from iq to the matrix with fmat and tmat for from and to
    subroutine dlwqf1(num_cells, num_boundary_conditions, num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, &
            fast_solver_arr_size, ipoint, iwrk, imat, rowpnt, &
            fmat, tmat)

        use m_logger_helper, only : stop_with_error
        use timers
        implicit none

        integer(kind = int_wp), intent(in   ) :: num_cells                   !< Number of volumes
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions                   !< Number of volumes
        integer(kind = int_wp), intent(in   ) :: num_exchanges                     !< Number of exchanges
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir                    !< Number of exchanges in first direction
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir                    !< Number of exchanges in second direction
        integer(kind = int_wp), intent(in   ) :: fast_solver_arr_size                   !< Dimension of sparse matrix
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)          !< Exchange pointers (dim: 4 x num_exchanges)
        integer(kind = int_wp), intent(inout) :: iwrk(num_cells + num_boundary_conditions)     !< Workspace
        integer(kind = int_wp), intent(out  ) :: imat(fast_solver_arr_size)             !< Column indeces per row of sparse matrix
        integer(kind = int_wp), intent(out  ) :: rowpnt(0:num_cells + num_boundary_conditions) !< Row index, contains row lengths of mat (elsewhere: itrac)
        integer(kind = int_wp), intent(out  ) :: fmat(num_exchanges)               !< Location from(iq) in matrix
        integer(kind = int_wp), intent(out  ) :: tmat(num_exchanges)               !< Location to  (iq) in matrix

        ! Local declarations
        integer(kind = int_wp) :: i    !< Index from cells
        integer(kind = int_wp) :: j    !< Index to cells
        integer(kind = int_wp) :: i2   !< Index from cell
        integer(kind = int_wp) :: j2   !< Index to cell
        integer(kind = int_wp) :: ip   !< Index cell from exchanges
        integer(kind = int_wp) :: jp   !< Indeces cell to exchanges
        integer(kind = int_wp) :: iseg !< Current volume
        integer(kind = int_wp) :: iq   !< Current edge
        integer(kind = int_wp) :: iadd !< Auxiliary variable

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf1", ithandl)

        rowpnt = 0
        iwrk = 0
        imat = 0
        fmat = 0
        tmat = 0

        ! compute number of off-diagonals per row for first 2 directions
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) cycle
            if (i > 0) rowpnt(i) = rowpnt(i) + 1
            if (j > 0) rowpnt(j) = rowpnt(j) + 1
        enddo

        ! see if there is a third direction
        iadd = 0
        if (num_exchanges /= num_exchanges_u_dir + num_exchanges_v_dir) iadd = 2  !  in 3D first 2 co diagonals are the vertical

        ! accumulate to pointer start of rows
        rowpnt(0) = 0
        do iseg = 1, num_cells
            rowpnt(iseg) = rowpnt(iseg) + rowpnt(iseg - 1) + iadd
        enddo
        do iseg = num_cells + 1, num_cells + num_boundary_conditions
            rowpnt(iseg) = rowpnt(iseg - 1)
        enddo
        if (rowpnt(num_cells + num_boundary_conditions) > fast_solver_arr_size) then
            write (*, *) ' System error in fast solvers matrix.'
            write (*, *) ' fast_solver_arr_size = ', fast_solver_arr_size, ', Required = ', rowpnt(num_cells + num_boundary_conditions)
            call stop_with_error()
        endif

        ! fill the (pointers in) matrix for the first 2 directions
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) cycle
            i2 = i
            if (i < 0) i2 = num_cells - i
            j2 = j
            if (j < 0) j2 = num_cells - j
            if (i > 0) then
                ip = iwrk(i) + 1 + iadd
                if (i > 1) ip = ip + rowpnt(i - 1)
                imat(ip) = j2
                fmat(iq) = ip
                iwrk(i) = iwrk(i) + 1
            endif
            if (j > 0) then
                jp = iwrk(j) + 1 + iadd
                if (j > 1) jp = jp + rowpnt(j - 1)
                imat(jp) = i2
                tmat(iq) = jp
                iwrk(j) = iwrk(j) + 1
            endif
        enddo

        ! fill the matrix for the last direction
        do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) cycle
            i2 = i
            if (i < 0) i2 = num_cells - i
            j2 = j
            if (j < 0) j2 = num_cells - j
            if (i > 0) then
                if (j < i) then        ! first  off-diagonal element -> previous layer
                    ip = 1
                else                   ! second off-diagonal element -> next layer
                    ip = 2
                endif
                if (i > 1) ip = ip + rowpnt(I - 1)
                imat(ip) = j2
                fmat(iq) = ip
            endif
            if (j > 0) then
                if (j < i) then        ! first  off-diagonal element -> previous layer
                    jp = 2
                else                    ! second off-diagonal element -> next layer
                    jp = 1
                endif
                if (j > 1) jp = jp + rowpnt(J - 1)
                imat(jp) = i2
                tmat(iq) = jp
            endif
        enddo
        ! filling the administration of arrays has been completed
        if (timon) call timstop (ithandl)
    end subroutine dlwqf1
end module m_dlwqf1
