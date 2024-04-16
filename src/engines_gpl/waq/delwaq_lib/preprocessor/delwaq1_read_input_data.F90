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
module m_delwaq1_read_input_data

    implicit none

    private
    public :: delwaq1_read_input_data

contains

    subroutine delwaq1_read_input_data(status)
        !< reads delwaq input file, fills data structure,
        !< validates the data and writes it to wrk files

        use m_delwaq_statistical_process, only : setup_statistical
        use m_dlwqp1
        use m_delwaq1_data
        use m_error_status
        use inputs_block_9, only : read_block_9
        use inputs_block_8, only : read_block_8_initial_conditions
        use inputs_block_7, only : read_block_7_process_parameters
        use inputs_block_6, only : read_block_6_waste_loads_withdrawals
        use inputs_block_5, only : read_block_5_boundary_conditions
        use inputs_block_4, only : read_block_4_flow_dims_pointers
        use inputs_block_3, only : read_block_3_grid_layout
        use m_block_2_input_reader, only : read_block_2_from_input
        use m_block_1_input_reader, only : read_block_1_from_input

        ! local
        type(error_status), intent(inout) :: status !< current error status

        cchar = ' '
        ilun = 0
        ilun(1) = lun(26)
        lch(1) = lchar(26)
        lunut = lun(29)

        call read_block_1_from_input(lun, psynam, nosys, notot, nomult, &
                multp, iwidth, otime, isfact, refday, &
                output_verbose_level, status)

        if (status%ierr /= 0) then
            write (lunrep, '(A)') " ERROR: reading system names"
            call status%increase_error_count()
            return
        end if
        allocate (syname(notot + nomult), stat = ierr_alloc)
        allocate (imultp(2, nomult), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunrep, '(A,I6)') " ERROR: allocating memory for system names:", ierr_alloc
            call status%increase_error_count()
            return
        end if
        syname = psynam
        imultp = multp
        if (associated(psynam)) deallocate (psynam)
        if (associated(multp)) deallocate (multp)
        deltim = otime
        char_arr(1) = ' '
        k = 2
        icmak = cmax - 1

        nullify (nsegdmp)
        nullify (isegdmp)
        nullify (nexcraai)
        nullify (iexcraai)
        nullify (ioptraai)
        call read_block_2_from_input(lun, lchar, filtype, nrftot, nlines, &
                npoins, is_date_format, is_ddhhmmss_format, nodump, iopt, &
                noint, iwidth, is_yyddhh_format, ndmpar, ntdmps, &
                noraai, ntraaq, nosys, notot, nototp, &
                output_verbose_level, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        call read_block_3_grid_layout(lun, lchar, filtype, nrftot, nrharm, &
                ivflag, is_date_format, iwidth, is_yyddhh_format, &
                output_verbose_level, gridps, syname, status, &
                has_hydfile, nexch)

        if (.not. associated(nsegdmp)) allocate (nsegdmp(1))
        if (.not. associated(isegdmp)) allocate (isegdmp(1))
        if (.not. associated(nexcraai)) allocate (nexcraai(1))
        if (.not. associated(iexcraai)) allocate (iexcraai(1))
        if (.not. associated(ioptraai)) allocate (ioptraai(1))
        call read_block_4_flow_dims_pointers(lun, lchar, filtype, nrftot, nrharm, &
                ilflag, is_date_format, iwidth, intsrt, is_yyddhh_format, &
                output_verbose_level, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, gridps, status, &
                has_hydfile, nexch)
        if (associated(nsegdmp)) deallocate (nsegdmp)
        if (associated(isegdmp)) deallocate (isegdmp)
        if (associated(nexcraai)) deallocate (nexcraai)
        if (associated(iexcraai)) deallocate (iexcraai)
        if (associated(ioptraai)) deallocate (ioptraai)

        deltim = otime
        call read_block_5_boundary_conditions(lun, lchar, filtype, char_arr, iar, &
                real_array, nrftot, nrharm, nobnd, nosys, &
                notot, nobtyp, rmax, imax, is_date_format, &
                iwidth, intsrt, is_yyddhh_format, syname, &
                icmak, output_verbose_level, status)

        deltim = otime

        nosss = noseg + nseg2     ! increase with bottom segments
        call read_block_6_waste_loads_withdrawals(lun, lchar, filtype, icmak, char_arr(k), &
                imax, iar, rmax, real_array, notot, &
                nosss, syname, nowst, nowtyp, nrftot, &
                nrharm, is_date_format, is_yyddhh_format, iwidth, &
                output_verbose_level, chkpar, status)

        novec = 50
        inpfil%is_date_format = is_date_format
        inpfil%is_ddhhmmss_format = is_ddhhmmss_format
        inpfil%is_yyddhh_format = is_yyddhh_format
        inpfil%itfact = itfact

        nrharm(10) = 0
        deltim = otime
        call read_block_7_process_parameters(lun, lchar, filtype, inpfil, syname, &
                iwidth, output_verbose_level, gridps, constants, chkpar, &
                status)

        ! Finish and close system file ( read_block_9 can re-read it )
        write (lun(2)) (nrftot(i), i = 1, noitem)
        write (lun(2)) (nrharm(i), i = 1, noitem)
        close (lun(2))

        call read_block_8_initial_conditions(lun, lchar, filtype, nosss, notot, &
                syname, iwidth, output_verbose_level, inpfil, &
                gridps, status)

        call read_block_9(lun, lchar, filtype, char_arr, iar, &
                icmak, max_int_size, iwidth, &
                output_verbose_level, ioutps, outputs, status)

        call setup_statistical(lunrep, npos, cchar, &
                ilun, lch, &
                lstack, output_verbose_level, &
                is_date_format, is_yyddhh_format, &
                statprocesdef, allitems, &
                status)
        write (lunrep, '(//'' Messages presented in this .lst file:'')')
        write (lunrep, '( /'' Number of WARNINGS            :'',I6)') status%iwar
        write (lunrep, '(  '' Number of ERRORS during input :'',I6)') status%ierr
        write (lunrep, '(  '' '')')

        call dlwqp1(lun, lchar, &
                statprocesdef, allitems, &
                ioutps, outputs, &
                nomult, imultp, &
                constants, &
                refday, &
                status)

        deallocate (syname)
        deallocate (imultp)

    end subroutine delwaq1_read_input_data

end module m_delwaq1_read_input_data
