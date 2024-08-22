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
module m_plotgr

    implicit none

contains


    subroutine plotgrp(npgrid, pg, num_rows, num_columns, lgrid, &
            lgrid2, xb, yb)

        !     Deltares Software Centre

        !>\file
        !>             Makes a pointer from the plotgrid cells to the gridmap to easy access depth
        !>
        !>             The local depth variable is also included in the plot grid results file.\n
        !>             To avoid making look-ups every time step, a back-pointer is made once here.\n
        !>             The backpointer is stored in pg(i)%nmcell(:,:).

        !     Created           : July   2003 by antoon koster

        !     Modified          : August 2011 by Leo Postma : allow for multiple plot grids

        !     Subroutines called: findcell to get a grid-cell from an x,y value

        !     Functions called  : none

        !     Logical units     : * standard output

        use m_waq_precision        ! single/double precision
        use timers
        use grid_search_mod
        use typos

        implicit none

        !     Arguments

        !     kind            function         name                description

        integer  (int_wp), intent(in) :: npgrid            !< number of plot grids
        type(PlotGrid)                   pg    (npgrid)    !< collection with plot grid information
        integer  (int_wp), intent(in) :: num_rows              !< 1st dimension of the flow grid
        integer  (int_wp), intent(in) :: num_columns              !< 2nd dimension of the flow grid
        integer  (int_wp), intent(in) :: lgrid (num_rows, num_columns) !< active grid matrix
        integer  (int_wp), intent(in) :: lgrid2(num_rows, num_columns) !< total grid matrix
        real     (real_wp), intent(in) :: xb    (num_rows * num_columns) !< x-values of the grid cell corners
        real     (real_wp), intent(in) :: yb    (num_rows * num_columns) !< y-values of the grid cell corners

        !     local scalars

        integer(int_wp) :: ig             ! loop counter over grids
        real   (real_wp) :: xpf, ypf     ! step sizes in x and y direction
        integer(int_wp) :: ix, iy      ! loop counters within the grids
        real   (sp) :: xnloc, ynloc   ! location of the middle of the plot grid cell
        integer(int_wp) :: i, j       ! loop counters around a corner point
        integer(int_wp) :: nmloc          ! linear grid cell number found

        !     progress bar ?!

        integer(int_wp)    nocell, ncols, ifreq
        real   (real_wp)    proc

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("plotgrp", ithndl)

        do ig = 1, npgrid

            xpf = (pg(ig)%xhigh - pg(ig)%xlow) / pg(ig)%mmap
            ypf = (pg(ig)%yhigh - pg(ig)%ylow) / pg(ig)%nmap
            pg(ig)%surf = xpf * ypf
            !
            nocell = 0
            ncols = 40                                      ! progress bar
            ifreq = max(real(pg(ig)%mmap * pg(ig)%nmap) / real(ncols), 1.0) + 1  ! progress bar
            do ix = 1, pg(ig)%mmap
                do iy = 1, pg(ig)%nmap
                    nocell = nocell + 1
                    proc = 100.0 * nocell / real(pg(ig)%mmap * pg(ig)%nmap)
                    if (mod(nocell, ifreq) == 0) write(*, '(a)', advance = 'no') '.'  ! progress bar

                    !           check if the center of the plot grid cell is over land or water

                    xnloc = (ix - 0.5) * xpf + pg(ig)%xlow
                    ynloc = (iy - 0.5) * ypf + pg(ig)%ylow

                    call findcell(num_rows, num_columns, xnloc, ynloc, lgrid, &
                            lgrid2, xb, yb, nmloc)

                    pg(ig)%nmcell(iy, ix) = nmloc

                    if (nmloc == 0) then

                        !             the center is over land. check if one of the corners is over land or water

                        loop1: do i = 0, 1
                            do j = 0, 1
                                xnloc = (ix - i) * xpf + pg(ig)%xlow
                                ynloc = (iy - j) * ypf + pg(ig)%ylow
                                call findcell(num_rows, num_columns, xnloc, ynloc, lgrid, &
                                        lgrid2, xb, yb, nmloc)
                                if (nmloc /= 0) exit loop1
                            enddo
                        enddo loop1
                        pg(ig)%nmcell(iy, ix) = nmloc

                    endif

                enddo
            enddo
        enddo

        if (timon) call timstop (ithndl)
        return
    end subroutine

end module m_plotgr
