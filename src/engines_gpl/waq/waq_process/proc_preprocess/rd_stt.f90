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
module m_rd_stt
    use m_waq_precision
    use m_delwaq_statistical_process, only : setup_statistical
    use m_srstop

    implicit none

contains


    subroutine rd_stt(lunrep, sttfil, statprocesdef, allitems, status)

        use dlwq_hyd_data      ! for definition and storage of data
        use processet      ! processet definitions
        use rd_token       ! tokenized reading
        use m_error_status

        implicit none

        integer(kind = int_wp), intent(inout) :: lunrep          !< logical unit of report file
        character(len = 256), intent(inout) :: sttfil          !< filename stt
        type(procespropcoll), intent(inout) :: statprocesdef   !< the statistical proces definition
        type(itempropcoll), intent(inout) :: allitems        !< all items of the proces system
        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: iostat
        integer(kind = int_wp) :: output_verbose_level
        logical :: is_date_format
        logical :: is_yyddhh_format

        ilun = 0
        lch (1) = sttfil
        open (newunit = ilun(1), file = lch(1), status = 'old', iostat = iostat)
        if(iostat /= 0) then
            write(*, *) 'Error reading file: ', trim(lch(1))
            call srstop(1)
        endif
        npos = 1000
        cchar = ';'
        output_verbose_level = 0
        is_date_format = .true.
        is_yyddhh_format = .false.

        call status%initialize(0, 0, 0)

        call setup_statistical (lunrep, npos, cchar, ilun, lch, &
                lstack, output_verbose_level, is_date_format, is_yyddhh_format, statprocesdef, allitems, &
                status)

        close(ilun(1))

    end subroutine rd_stt
end module m_rd_stt
