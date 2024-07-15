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
module m_part06

    implicit none

contains


    subroutine part06 (lun, lgrid, lgrid2, num_rows, num_columns, &
            xb, yb, nodye, nocont, xwaste, &
            ywaste, nwaste, mwaste)

        !       Deltares Software Centre

        !>\file
        !>            Determines the grid cells and relative coordinates of waste locations
        !>
        !>            The wastelocations are given by the user in global x,y coordinates.\n
        !>            This routine determines the n,m grid indices and the local x,y coordinates.\n
        !>            The local x,y coordinates are 0< .. <1 and are store in the old x,y locations

        !     System administration : Antoon Koster

        !     Created               : February 1990, by Leo Postma

        !     Modified              : August   2011, by Leo Postma, only warning if outside grid

        !     logical unit numbers  : lun    - output log file

        !     subroutines called    : part07 - searches for cell and coordinates

        use m_waq_precision
        use timers
        use grid_search_mod

        implicit none

        !     Arguments

        !     kind            function         name                   description

        integer  (int_wp), intent(in) :: lun                  !< unit number output log file
        integer  (int_wp), intent(in) :: num_rows                 !< first index hydrodynamic grid
        integer  (int_wp), intent(in) :: num_columns                 !< second index hydrodynamic grid
        integer  (int_wp), intent(in) :: lgrid (num_rows, num_columns)    !< active grid matrix
        integer  (int_wp), intent(in) :: lgrid2(num_rows, num_columns)    !< total grid matrix
        real     (real_wp), intent(in) :: xb    (num_rows * num_columns)    !< x of grid corner points
        real     (real_wp), intent(in) :: yb    (num_rows * num_columns)    !< y of grid corner points
        integer  (int_wp), intent(in) :: nodye                !< number of dye releases
        integer  (int_wp), intent(in) :: nocont               !< number of continuous release
        real     (real_wp), intent(inout) :: xwaste(nodye + nocont) !< x of wasteload location
        real     (real_wp), intent(inout) :: ywaste(nodye + nocont) !< y of wasteload location
        integer  (int_wp), intent(out) :: nwaste(nodye + nocont) !< first grid index wasteloads
        integer  (int_wp), intent(out) :: mwaste(nodye + nocont) !< second grid index wasteloads

        !     Locals

        integer  (int_wp) :: id      ! loop counter wasteloads
        integer  (int_wp) :: ierror  ! error variable of part07
        real     (real_wp) :: xnloc   ! input x coordinate for the grid search
        real     (real_wp) :: ynloc   ! input y coordinate for the grid search
        real     (real_wp) :: xmloc   ! output x coordinate for the grid search
        real     (real_wp) :: ymloc   ! output y coordinate for the grid search
        integer  (int_wp) :: nmloc   ! output n index of the wasteload point
        integer  (int_wp) :: mmloc   ! output m index of the wasteload point
        integer  (int_wp) :: noerr   ! local error accumulator

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part06", ithndl)

        !     loop over the number of wasteloads

        noerr = 0
        do id = 1, nodye + nocont
            xnloc = xwaste(id)
            ynloc = ywaste(id)
            call part07 (lgrid, lgrid2, num_rows, num_columns, xb, &
                    yb, xnloc, ynloc, nmloc, mmloc, &
                    xmloc, ymloc, ierror)
            if (ierror == 0) then
                nwaste(id) = nmloc
                mwaste(id) = mmloc
                xwaste(id) = xmloc
                ywaste(id) = ymloc
            else                          ! location invalid
                if (id > nodye) then
                    write (lun, 1010) id - nodye, xnloc, ynloc
                else
                    write (lun, 1000) id, xnloc, ynloc
                endif
                noerr = noerr + 1
            endif
        enddo

        if (noerr /= 0) write (lun, 1020) noerr

        !     end of routine

        if (timon) call timstop (ithndl)
        return

        1000 format('  Warning 4901. Dye        release', i3, ' at (x,y): (', &
                f9.2, ',', f9.2, ') not on active grid cell.')
        1010 format('  Warning 4902. Continuous release', i3, ' at (x,y): (', &
                f9.2, ',', f9.2, ') not on active grid cell.')
        1020 format('  Total number of waste load warnings = ', i3)

    end subroutine

end module m_part06
