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
module m_set_vertical_dispersion_length
    use m_waq_precision
    use m_string_utils

    implicit none

    private
    public :: set_vertical_dispersion_length

contains


    !> Reads SURFACE from coupling
    !! Sets dispersion length in vertical
    subroutine set_vertical_dispersion_length(num_substances_total, num_cells, num_exchanges, num_exchanges_u_dir, &
            num_exchanges_v_dir, num_exchanges_z_dir, num_spatial_parameters, &
            ipoint, volume, &
            aleng, param, &
            paname, ilflag)

        use m_logger_helper, only: stop_with_error, get_log_unit_number

        SAVE

        integer(kind = int_wp), intent(in) :: num_substances_total    !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells               !< Nr. of computational elements
        integer(kind = int_wp), intent(in) :: num_exchanges           !< Total number of exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir     !< Nr. of exchanges direction 1
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir     !< Nr. of exchanges direction 2
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir     !< Nr. of exchanges direction 3
        integer(kind = int_wp), intent(in) :: num_spatial_parameters  !< Number of parameters
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)!< 1= "From"   segment pointers
        !< 2= "To"     segment pointers
        !< 3= "From-1" segment pointers
        !< 4= "To+1"   segment pointers
        real(kind = real_wp), intent(in) :: VOLUME(num_cells)         !< Segment volumes
        real(kind = real_wp), intent(inout) :: ALENG(2, num_exchanges)      !< 1= Length to "From" surface
        !< 2= Length to "To"   surface
        !< 3 lengths in the grid
        real(kind = real_wp), intent(inout) :: PARAM(num_spatial_parameters, num_cells) !< Model parameters
        character(len = 20), intent(in) :: PANAME(*)              !< Parameter names
        integer(kind = int_wp), intent(in) :: ILFLAG              !< if 0 then 3 length values

        ! Local variables
        INTEGER(kind = int_wp) :: LCCCO, ier, ierr, ier2, lunrep, isurf, &
                nmaxa, mmaxa, nma, idummy, nmt, k, cell_i, &
                ilay, iq, ipos, ifrom, ito, layt
        LOGICAL    FIRST, LINIT, LEXI
        DATA       FIRST / .TRUE. /
        DATA       LINIT / .FALSE. /

        ! check usage w.r.t. parallel computing
        ! AM:
        ! I removed this check, as all the computations set up using
        ! the Delft3D user-interface have the SURF parameter.
        ! Even if not, then the file should be available on all
        ! nodes, as they share the directory.
        ! check number of parameters

        ! Initialisation set index pointers, read surface areas
        if (first) then
            first = .false.
            ier = 0
            call get_log_unit_number(lunrep)
            write(lunrep, *)
            write(lunrep, 2000)
            ! Set pointers in param array
            isurf = index_in_array('SURF      ', paname (:num_spatial_parameters))
            ! read surface areas
            if (isurf > 0) then
                if (ilflag == 1 .and. num_exchanges_z_dir > 0) then
                    linit = .true.
                    write(lunrep, 2040)
                end if
                inquire  (file = 'areachar.dat', exist = lexi)
                if (.not. lexi) then
                    ! it is assumed the surf parameter has been set in the input
                else
                    open (newunit = lccco, file = 'areachar.dat', form = 'UNFORMATTED', &
                            status = 'OLD', iostat = IER2)
                    if (ier2 /= 0) then
                        write (lunrep, 2010)
                        write (*, 2010)
                        ier = ier + 1
                    else
                        write(lunrep, 2030)
                        read (lccco) nmaxa, mmaxa, nma, nma, nma, idummy
                        layt = num_cells / nma
                        nmt = nma * layt
                        if (nmt /= num_cells) then
                            write (lunrep, 2050) nma, layt, nmt, num_cells
                            write (*, 2050) nma, layt, nmt, num_cells
                            ier = ier + 1
                        end if
                        if (ier == 0) then
                            read (lccco) (param(isurf, k), k = 1, nma)
                            do ilay = 2, layt
                                do cell_i = 1, nma
                                    ipos = (ilay - 1) * nma + cell_i
                                    param(isurf, ipos) = param(isurf, cell_i)
                                end do
                            end do
                        end if
                        close (lccco)
                    end if
                end if
                if (ier /= 0) then
                    call stop_with_error()
                end if
            end if
            write(lunrep, 2070)
        end if

        ! adapt the length for the third direction
        if (linit) then
            do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (ifrom > 0) then
                    if (param(isurf, ifrom) > 1.0e-15) then
                        aleng(1, iq) = volume(ifrom) / param(isurf, ifrom) / 2.
                    end if
                end if
                if (ito   > 0) then
                    if (param(isurf, ifrom) > 1.0e-15) then
                        aleng(2, iq) = volume(ito) / param(isurf, ifrom) / 2.
                    end if
                end if
            end do
        end if
        return
        ! Output formats
        2000 format (' Extra functionality set_vertical_dispersion_length')
        2010 format (' ERROR: opening file <areachar.dat> !')
        2030 format (' Surface area''s will be read from file <areachar.dat>')
        2040 format (' Dispersion length in third direction will be calculated')
        2050 format (' ERROR: File areachar.dat does not match.', &
                ' NMA = ', I8, ' LAYT= ', I8, ' NMT = ', I8, ' num_cells=', I8)
        2070 format (' End extra functionality set_vertical_dispersion_length')
    end subroutine set_vertical_dispersion_length
end module m_set_vertical_dispersion_length
