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
module m_delwaq1_write_messages
    use m_waq_precision
    use m_error_status
    implicit none

contains

    subroutine delwaq1_write_messages(status)
        use m_open_waq_files
        use m_delwaq1_data
        use m_dattim

        implicit none

        type(error_status) :: status !< current error status

        write (lunrep, '(//'' Messages presented including .lsp file:'')')
        write (lunrep, '(  '' Number of WARNINGS            :'',I6)') status%iwar
        write (lunrep, '( /'' Number of ERRORS during input :'',I6)') status%ierr
        write (*, '(  ''  Number of WARNINGS            :'',I6)') status%iwar
        write (*, '(  ''  Number of ERRORS during input :'',I6)') status%ierr
        write (*, '(  '' '')')

        if (status%ierr == 0) then
            novec = min(novec, (nosss + nobnd - 1))
            itota = 0
            itoti = 0
            itotc = 0
            call space(lunrep, .false., buffer%rbuf, buffer%ibuf, buffer%chbuf, &
                    itota, itoti, itotc)
            ! create the delwaq03.wrk file
            call open_waq_files(lun(1), lchar(1), 1, 1, ioerr)
            write (lun(1)) in
            write (lun(1)) ii
            write (lun(1)) itota, itoti, itotc
            write (lun(1)) (lun(k), k = 1, nolun)
            write (lun(1)) (lchar(k), k = 1, nolun)
            write (lun(1)) (filtype(k), k = 1, nolun)
        else
            write (lunrep, '(  '' SIMULATION PROHIBITED !!!!!!!!'')')
            call open_waq_files(lun(1), lchar(1), 1, 3, ioerr)
        end if

        call dattim(rundat)
        write (lunrep, '(2A)') ' Execution stop : ', rundat
        close (lunrep)

    end subroutine delwaq1_write_messages
end module m_delwaq1_write_messages
