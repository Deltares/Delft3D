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
module m_setopo
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine setopo (procesdef, outputs, iloc, idef, iflx, status)

        ! set output pointers

        use m_logger_helper
        use timers         !< performance timers
        use processet
        use results, only : OutputPointers
        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        type(OutputPointers) :: outputs         ! output structure
        integer(kind = int_wp) :: iloc            ! offset to local array
        integer(kind = int_wp) :: idef            ! offset to default array
        integer(kind = int_wp) :: iflx            ! offset to flux array

        type(error_status), intent(inout) :: status !< current error status

        ! local decalarations

        integer(kind = int_wp) :: num_processes_activated           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        type(procesprop), pointer :: proc            ! process description
        character(len = 100) :: line            ! line buffer for output
        integer(kind = int_wp) :: ioutput         ! index output item
        integer(kind = int_wp) :: i_input         ! index input item
        integer(kind = int_wp) :: indx            ! index
        integer(kind = int_wp) :: iou             ! loop counter output variable
        integer(kind = int_wp) :: iou2            ! loop counter output variable
        character(len = 20) :: predef(3)       ! predefined names
        integer(kind = int_wp) :: ithndl = 0      ! handle for performance timer
        if (timon) call timstrt("setopo", ithndl)

        predef(1) = 'volume'
        predef(2) = 'itime'
        predef(3) = 'idt'

        ! set "output from active processes to output param" pointers

        write(line, '(a)') '# locating requested output from active processes'
        call write_log_message(line)
        line = ' '
        call write_log_message(line)

        num_processes_activated = procesdef%current_size

        do iou = 1, outputs%current_size

            ! check of deze al eerder aan de beurt is geweest

            iou2 = index_in_array(outputs%names(iou)(:10), outputs%names(:iou - 1))
            if (iou2 > 0) then

                ! if pointer the same get out ( can they be different except for -1 ?? )

                if (outputs%pointers(iou) == outputs%pointers(iou2)) then
                    goto 300
                endif

                ! if pointer -1  set pointer equal to iou2 and get out

                if (outputs%pointers(iou) == -1) then
                    outputs%pointers(iou) = outputs%pointers(iou2)
                    goto 300
                endif
            endif

            ! if this comes from proloc

            if (outputs%pointers(iou) > iloc) then
                do iproc = 1, num_processes_activated
                    proc => procesdef%procesprops(iproc)
                    if (proc%active) then
                        call zoekio (outputs%names(iou), proc%no_output, proc%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                        if (ioutput > 0) then

                            ! is this the one which is being used ?

                            if (proc%output_item(ioutput)%ip_val == outputs%pointers(iou)) then
                                write (line, '(5a)') ' output [', outputs%names (iou)(1:20), '] from proces [', proc%name(1:10), ']'
                                call write_log_message(line)
                                goto 300
                            endif
                        endif

                    endif

                enddo
            elseif (outputs%pointers(iou) == -1) then

                ! predefined ?

                indx = index_in_array(outputs%names(iou), predef)
                if (indx == 1) then
                    write(line, '(3a)') ' output [', outputs%names (iou)(1:20), '] using delwaq volume'
                    call write_log_message(line)
                    outputs%pointers(iou) = 1
                    goto 300
                endif
                if (indx == 2) then
                    write(line, '(3a)') ' output [', outputs%names (iou)(1:20), '] using delwaq itime'
                    call write_log_message(line)
                    outputs%pointers(iou) = 2
                    goto 300
                endif
                if (indx == 3) then
                    write(line, '(3a)') ' output [', outputs%names (iou)(1:20), '] using delwaq idt'
                    call write_log_message(line)
                    outputs%pointers(iou) = 3
                    goto 300
                endif

                ! investigate wheter a default with this name is being used

                do iproc = 1, num_processes_activated
                    proc => procesdef%procesprops(iproc)
                    if (proc%active) then
                        call zoekio (outputs%names(iou), proc%no_input, proc%input_item, 20, i_input, IOTYPE_SEGMENT_INPUT)
                        if (i_input > 0) then

                            ! is it a default ?

                            if (proc%input_item(i_input)%ip_val > idef .and. &
                                    proc%input_item(i_input)%ip_val <= iflx) then
                                outputs%pointers(iou) = proc%input_item(i_input)%ip_val
                                write (line, '(5a)') ' output [', outputs%names (iou)(1:20), '] default from [', proc%name, ']'
                                call write_log_message(line)
                                goto 300
                            endif
                        endif

                    endif
                enddo

                call status%increase_warning_count()
                write (line, '(5a)') ' warning: output [', outputs%names (iou)(1:20), '] not located'
                call write_log_message(line)
            endif
            300 continue
        end do

        line = ' '
        call write_log_message(line)

        if (timon) call timstop(ithndl)
        return
    end

end module m_setopo
