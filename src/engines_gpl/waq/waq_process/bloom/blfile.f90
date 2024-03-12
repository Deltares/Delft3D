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
module m_blfile
    use m_waq_precision

    implicit none

contains


    subroutine blfile (lunrep)

        use m_srstop
        use bloom_data_io

        implicit none

        integer(kind = int_wp) :: lunrep       ! Report file for error messages

        character(256) filnam       ! File name with extention
        integer(kind = int_wp) :: iost         ! I/O-status

        !  Open statement for BLOOM II input files.
        filnam = trim(runnam) // '.frm'
        open (newunit = infrm, file = filnam, iostat = iost)
        if (iost /= 0) then
            write (*, *) 'blfile: error opening .frm file'
            write (lunrep, *) 'blfile: error opening .frm file'
            call srstop(1)
        endif

        ! Open statement for BLOOM II debug file.
        filnam = trim(runnam) // '.dbg'
        open (newunit = outdbg, file = filnam, iostat = iost)
        if (iost /= 0) then
            write (*, *) 'blfile: error opening .dbg file'
            write (lunrep, *) 'blfile: error opening .dbg file'
            call srstop(1)
        endif

        return
    end

end module m_blfile
