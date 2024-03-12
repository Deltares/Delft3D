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
module m_proc_totals
    use m_waq_precision

    implicit none

contains


    subroutine proc_totals(lunrep, procesdef, no_ins, no_ine, no_ous, &
            no_oue, no_flu, no_sto, no_dis, no_vel)

        ! caluclate totals for the processes

        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        integer(kind = int_wp) :: no_ins          ! number of output items
        integer(kind = int_wp) :: no_ine          ! number of output items
        integer(kind = int_wp) :: no_ous          ! number of output items
        integer(kind = int_wp) :: no_oue          ! number of output items
        integer(kind = int_wp) :: no_flu          ! number of output items
        integer(kind = int_wp) :: no_sto          ! number of output items
        integer(kind = int_wp) :: no_dis          ! number of output items
        integer(kind = int_wp) :: no_vel          ! number of output items

        ! local decalarations

        type(procesprop), pointer :: proc            ! single process
        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! index processes
        integer(kind = int_wp) :: i_item          ! index io_items
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("proc_totals", ithndl)


        ! loop over the processes

        no_ins = 0
        no_ine = 0
        no_ous = 0
        no_oue = 0
        no_flu = 0
        no_sto = 0
        no_dis = 0
        no_vel = 0
        nproc = procesdef%cursize
        do iproc = 1, nproc

            proc => procesdef%procesprops(iproc)

            do i_item = 1, proc%no_input
                if (proc%input_item(i_item)%type == IOTYPE_SEGMENT_INPUT .or. &
                        proc%input_item(i_item)%type == IOTYPE_SEGMENT_WORK  .or. &
                        proc%input_item(i_item)%type == IOTYPE_SCALAR_WORK) then
                    no_ins = no_ins + 1
                else
                    no_ine = no_ine + 1
                endif
            enddo
            do i_item = 1, proc%no_output
                if (proc%output_item(i_item)%type == IOTYPE_SEGMENT_OUTPUT .or. &
                        proc%output_item(i_item)%type == IOTYPE_SEGMENT_WORK   .or. &
                        proc%output_item(i_item)%type == IOTYPE_SCALAR_WORK) then
                    no_ous = no_ous + 1
                else
                    no_oue = no_oue + 1
                endif
            enddo

            no_flu = no_flu + proc%no_fluxoutput
            no_sto = no_sto + proc%no_fluxstochi
            no_dis = no_dis + proc%no_dispstochi
            no_vel = no_vel + proc%no_velostochi

        enddo

        if (timon) call timstop(ithndl)
        return
    end

end module m_proc_totals
