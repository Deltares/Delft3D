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
module m_wrwrko
    use m_waq_precision

    implicit none

contains


    subroutine wrwrko (lunwro, num_output_files, output_buffer_len, ioutps, outputs, &
            num_substances_total, substdname, subunit, subdescr)

        !     Deltares Software Centre

        !>/File
        !>      write output work file

        !     Created   : Nov   1994 by Jan van Beek
        !     Modified  : Aug   2012 by Jan van Beek, use results structure, modern look and feel

        use timers         !< performance timers
        use results, only : OutputPointers, ncopt

        implicit none

        integer(kind = int_wp), intent(in) :: lunwro                 !< output work file
        integer(kind = int_wp), intent(in) :: num_output_files                  !< total number of output files
        integer(kind = int_wp), intent(in) :: output_buffer_len
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure
        integer(kind = int_wp), intent(in) :: num_substances_total                  !< total number of substances
        character(len=100), intent(in) :: substdname(num_substances_total)      !< substance standard name
        character(len=40), intent(in) :: subunit(num_substances_total)         !< substance unit
        character(len=60), intent(in) :: subdescr(num_substances_total)        !< substance description

        ! local

        real(kind = real_wp) :: versio                 !  version number output system
        integer(kind = int_wp) :: k                      !  loop counter
        integer(kind = int_wp) :: num_output_variables_extra                 !  total number of variables in output
        integer(kind = int_wp) :: ithndl = 0             ! handle for performance timer
        if (timon) call timstrt("wrwrko", ithndl)

        versio = 0.2
        num_output_variables_extra = outputs%current_size

        ! write work file

        write (lunwro) versio
        write (lunwro) num_output_files, num_output_variables_extra, output_buffer_len, ncopt
        write (lunwro) (ioutps(1, k), k = 1, num_output_files)
        write (lunwro) (ioutps(2, k), k = 1, num_output_files)
        write (lunwro) (ioutps(3, k), k = 1, num_output_files)
        write (lunwro) (ioutps(4, k), k = 1, num_output_files)
        write (lunwro) (ioutps(5, k), k = 1, num_output_files)
        write (lunwro) (ioutps(6, k), k = 1, num_output_files)
        if (num_output_variables_extra>0) then
            write (lunwro) (outputs%pointers(k), k = 1, num_output_variables_extra)
            write (lunwro) (outputs%names   (k), k = 1, num_output_variables_extra)
            write (lunwro) (outputs%std_var_name(k), k = 1, num_output_variables_extra)
            write (lunwro) (outputs%units   (k), k = 1, num_output_variables_extra)
            write (lunwro) (outputs%description  (k), k = 1, num_output_variables_extra)
        end if
        if (num_substances_total>0) then
            write (lunwro) (substdname(k), k = 1, num_substances_total)
            write (lunwro) (subunit   (k), k = 1, num_substances_total)
            write (lunwro) (subdescr  (k), k = 1, num_substances_total)
        end if

        if (timon) call timstop(ithndl)
        return
        return
    end

end module m_wrwrko
