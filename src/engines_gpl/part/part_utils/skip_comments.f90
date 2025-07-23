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
module m_skip_comments

    implicit none

contains


    subroutine skip_comment_lines(lun, ios)
        !
        !     programmer : antoon koster
        !     function   : skip comment lines on ini-file
        !     date       : may 2004
        !
        use timers
        implicit none           !   force explicit typing
        integer :: lun, ios
        integer, parameter :: max_len_line = 200
        logical :: comment_line
        character(len = max_len_line) :: line
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("skip_comment_lines", ithndl)

        !     keep skipping comment lines
        comment_line = .true.
        do while (comment_line)
            read(lun, '(a)', iostat = ios) line
            comment_line = (line(1:1) == '*')
        enddo
        !    correct for last non-comment line
        if (.not. comment_line) then
            backspace lun
        endif

        if (timon) call timstop (ithndl)
        return
    end subroutine skip_comment_lines


end module m_skip_comments
