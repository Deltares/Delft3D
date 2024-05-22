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
module m_setopp
    use m_waq_precision

    implicit none

contains


    subroutine setopp (procesdef, outputs, ioff)

        !>/File
        !>      sets processes for requested output

        use m_logger
        use timers         !< performance timers
        use processet      !< processet definitions
        use results, only : OutputPointers         !< output definitions
        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       !< all processes
        type(OutputPointers) :: outputs         !< output structure
        integer(kind = int_wp) :: ioff            !< offset to process output in waq data space

        ! local decalarations

        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        type(procesprop), pointer :: proc            ! process description
        character(len = 100) :: line            ! line buffer for output
        integer(kind = int_wp) :: ioutput         ! index output item
        integer(kind = int_wp) :: iou             ! loop counter output variable
        integer(kind = int_wp) :: ithndl = 0      ! handle for performance timer
        if (timon) call timstrt("setopp", ithndl)

        ! set process on if output is requested and input ok

        write(line, '(a)') '# locating processes for requested output'
        call write_log_message(line)
        line = ' '
        call write_log_message(line)

        nproc = procesdef%current_size

        do iou = 1, outputs%current_size

            ! is the output undefined ( pointer -1 ) or from a proces

            if (outputs%pointers(iou) == -1 .or. outputs%pointers(iou) > ioff) then
                outputs%pointers(iou) = -1
                do iproc = 1, nproc
                    proc => procesdef%procesprops(iproc)
                    if (proc%linvok) then
                        call zoekio (outputs%names(iou), proc%no_output, proc%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                        if (ioutput > 0) then
                            if (.not. proc%active) then

                                ! turn proces on

                                proc%active = .true.

                                write (line, '(5a)') ' switching [', proc%name(1:10), '] on for output [', outputs%names(iou)(1:20), ']'
                                call write_log_message(line)
                                line = ' '
                                call write_log_message(line)
                            endif

                            goto 300
                        endif

                    endif

                end do
            endif

            300 continue
        end do

        if (timon) call timstop(ithndl)
        return
    end

end module m_setopp
