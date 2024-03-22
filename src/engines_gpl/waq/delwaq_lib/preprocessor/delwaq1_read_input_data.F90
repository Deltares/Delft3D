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
        use m_dlwq09
        use m_dlwq08
        use m_dlwq07
        use m_dlwq06
        use m_dlwq05
        use m_dlwq04
        use m_dlwq03
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
                ioutpt, status)

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
        car(1) = ' '
        k = 2
        icmak = cmax - 1

        nullify (nsegdmp)
        nullify (isegdmp)
        nullify (nexcraai)
        nullify (iexcraai)
        nullify (ioptraai)
        call read_block_2_from_input(lun, lchar, filtype, nrftot, nlines, &
                npoins, dtflg1, dtflg2, nodump, iopt, &
                noint, iwidth, dtflg3, ndmpar, ntdmps, &
                noraai, ntraaq, nosys, notot, nototp, &
                ioutpt, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        call dlwq03(lun, lchar, filtype, nrftot, nrharm, &
                ivflag, dtflg1, iwidth, dtflg3, &
                ioutpt, gridps, syname, status, &
                has_hydfile, nexch)

        if (.not. associated(nsegdmp)) allocate (nsegdmp(1))
        if (.not. associated(isegdmp)) allocate (isegdmp(1))
        if (.not. associated(nexcraai)) allocate (nexcraai(1))
        if (.not. associated(iexcraai)) allocate (iexcraai(1))
        if (.not. associated(ioptraai)) allocate (ioptraai(1))
        call dlwq04(lun, lchar, filtype, nrftot, nrharm, &
                ilflag, dtflg1, iwidth, intsrt, dtflg3, &
                ioutpt, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, gridps, status, &
                has_hydfile, nexch)
        if (associated(nsegdmp)) deallocate (nsegdmp)
        if (associated(isegdmp)) deallocate (isegdmp)
        if (associated(nexcraai)) deallocate (nexcraai)
        if (associated(iexcraai)) deallocate (iexcraai)
        if (associated(ioptraai)) deallocate (ioptraai)

        deltim = otime
        call dlwq05(lun, lchar, filtype, car, iar, &
                rar, nrftot, nrharm, nobnd, nosys, &
                notot, nobtyp, rmax, imax, dtflg1, &
                iwidth, intsrt, dtflg3, syname, &
                icmak, ioutpt, status)

        deltim = otime

        nosss = noseg + nseg2     ! increase with bottom segments
        call dlwq06(lun, lchar, filtype, icmak, car(k), &
                imax, iar, rmax, rar, notot, &
                nosss, syname, nowst, nowtyp, nrftot, &
                nrharm, dtflg1, dtflg3, iwidth, &
                ioutpt, chkpar, status)

        novec = 50
        inpfil%dtflg1 = dtflg1
        inpfil%dtflg2 = dtflg2
        inpfil%dtflg3 = dtflg3
        inpfil%itfact = itfact

        nrharm(10) = 0
        deltim = otime
        call dlwq07(lun, lchar, filtype, inpfil, syname, &
                iwidth, ioutpt, gridps, constants, chkpar, &
                status)

        !     Finish and close system file ( DLWQ09 can re-read it )
        write (lun(2)) (nrftot(i), i = 1, noitem)
        write (lun(2)) (nrharm(i), i = 1, noitem)
        close (lun(2))

        call dlwq08(lun, lchar, filtype, nosss, notot, &
                syname, iwidth, ioutpt, inpfil, &
                gridps, status)

        call dlwq09(lun, lchar, filtype, car, iar, &
                icmak, iimax, iwidth, &
                ioutpt, ioutps, outputs, status)

        call setup_statistical(lunrep, npos, cchar, &
                ilun, lch, &
                lstack, ioutpt, &
                dtflg1, dtflg3, &
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
