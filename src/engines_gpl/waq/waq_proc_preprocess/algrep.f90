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
module m_algrep
    use m_waq_precision
    use m_string_utils
    use m_string_manipulation, only : get_trimmed_length
    use processet
    use timers       !   performance timers

    implicit none

contains


    subroutine algrep (procesdef, notyp, nocof, algtyp, algact, &
            abrtyp, cofnam, algcof, maxcof, alggrp, &
            nogrp, grpnam, grpabr, nouttyp, outtyp, &
            noutgrp, outgrp)

        ! replace the proto names from proces.def with the actual BLOOM names
        ! for the moment not the names and defaults in itemprop, consequence?

        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef     !< all processes

        integer(kind = int_wp) :: notyp         !< number of algae types
        integer(kind = int_wp) :: nocof         !< number of coefficients
        integer(kind = int_wp) :: maxcof        !< max number of coefficiets
        integer(kind = int_wp) :: algact(notyp) !< active indication
        integer(kind = int_wp) :: nogrp         !< number of groups
        integer(kind = int_wp) :: nouttyp       !< number of outputs per type
        integer(kind = int_wp) :: noutgrp       !< number of outputs per group

        character(len = *) :: algtyp(notyp)     !< type names
        character(len = *) :: abrtyp(notyp)     !< type abbrevation
        character(len = *) :: cofnam(nocof)     !< coefficient name
        character(len = *) :: alggrp(notyp)     !< group of type
        character(len = *) :: grpnam(nogrp)     !< group names
        character(len = *) :: grpabr(nogrp)     !< group abbrevation
        character(len = *) :: outtyp(nouttyp)   !< name of output per type
        character(len = *) :: outgrp(noutgrp)   !< name of output per group

        real(kind = real_wp) :: algcof(maxcof, notyp) !< coefficient values

        ! local decalarations

        type(procesprop), pointer :: proc   ! process description

        integer(kind = int_wp) :: i_input   ! index input item
        integer(kind = int_wp) :: num_processes_activated     ! number of processes
        integer(kind = int_wp) :: iproc     ! loop counter processes
        integer(kind = int_wp) :: nalg      ! number of algae types
        integer(kind = int_wp) :: ialg      ! index algae types
        integer(kind = int_wp) :: igrp      ! index algae group
        integer(kind = int_wp) :: icof      ! index coefficient
        integer(kind = int_wp) :: ilen      ! nonblank length of string
        integer(kind = int_wp) :: iout      ! index output
        integer(kind = int_wp) :: ithndl = 0

        character(len = 10) :: name1  ! name
        character(len = 10) :: name2  ! name

        real(kind = real_wp) :: rgrp  ! index algae group

        if (timon) call timstrt("algrep", ithndl)

        ! some init
        num_processes_activated = procesdef%current_size

        ! input types

        nalg = 0
        do ialg = 1, notyp
            if (algact(ialg) == 1) then
                nalg = nalg + 1

                ! types themselves

                write(name1, '(''bloomalg'',i2.2)') nalg
                call replace_process_property_names(procesdef, name1, algtyp(ialg))

                ! groep number type

                write(name1, '(''specalg'',i2.2,'' '')') nalg
                igrp = index_in_array(alggrp(ialg)(:10), grpnam)
                rgrp = igrp
                do iproc = 1, num_processes_activated
                    proc => procesdef%procesprops(iproc)
                    do i_input = 1, proc%no_input
                        if (string_equals(proc%input_item(i_input)%name(1:10), name1)) then
                            proc%input_item(i_input)%actdef = rgrp
                        endif
                    enddo
                enddo

                ! coefficients, including defaults

                do icof = 1, nocof
                    name1 = cofnam(icof)
                    name2 = cofnam(icof)
                    call get_trimmed_length(name1, ilen)
                    write(name1(ilen + 1:), '(i2.2)') nalg
                    name2(ilen - 2:) = abrtyp(ialg)

                    do iproc = 1, num_processes_activated
                        proc => procesdef%procesprops(iproc)
                        do i_input = 1, proc%no_input
                            if (string_equals(proc%input_item(i_input)%name(1:10), name1)) then
                                proc%input_item(i_input)%actdef = algcof(icof, ialg)
                            endif
                        enddo
                    enddo

                    call replace_process_property_names(procesdef, name1, name2)
                enddo
            endif
        enddo

        ! output types, also input because output can be used as input

        nalg = 0
        do ialg = 1, notyp
            if (algact(ialg) == 1) then
                nalg = nalg + 1
                do iout = 1, nouttyp
                    name1 = outtyp(iout)
                    name2 = outtyp(iout)
                    call get_trimmed_length(name1, ilen)
                    write(name1(ilen + 1:), '(i2.2)') nalg
                    name2(ilen - 2:) = abrtyp(ialg)
                    call replace_process_property_names(procesdef, name1, name2)
                enddo
            endif
        enddo

        ! output groups

        do igrp = 1, nogrp

            ! special, groups themselves

            write(name1, '(''bloomgrp'',i2.2)') igrp
            name2 = grpnam(igrp)
            call replace_process_property_names(procesdef, name1, name2)

            ! the ones from the bloom database

            do iout = 1, noutgrp
                name1 = outgrp(iout)
                name2 = outgrp(iout)
                call get_trimmed_length(name1, ilen)
                write(name1(ilen + 1:), '(i2.2)') igrp
                name2(ilen - 2:) = grpabr(igrp)
                call replace_process_property_names(procesdef, name1, name2)
            enddo
        enddo

        if (timon) call timstop(ithndl)
        return
    end

    subroutine replace_process_property_names(process_definition, original_name, new_name)
        !< searches the given process_definition for items with original_name and
        !< replaces it with the new_name
        type(procespropcoll) :: process_definition !< process definition data

        character(len = *) :: original_name !< name to search
        character(len = *) :: new_name      !< name to replace_item_names with

        integer(kind = int_wp) :: iproc
        type(procesprop), pointer :: proc

        do iproc = 1, process_definition%current_size
            proc => process_definition%procesprops(iproc)
            call replace_item_names(proc, original_name, new_name)
        enddo

    end subroutine replace_process_property_names

    subroutine replace_item_names(process_properties, name1, name2)
        !< searches the given process_properties for items with original_name and
        !< replaces it with the new_name
        type(procesprop), pointer :: process_properties  ! process description

        character(len = *) :: name1  !< name to search
        character(len = *) :: name2  !< name to replace_item_names with

        character(len = 10) :: str_to_compare

        integer(kind = int_wp) :: i_input  ! index input item
        integer(kind = int_wp) :: ioutput  ! index output item
        integer(kind = int_wp) :: istochi  ! index stochi item

        do i_input = 1, process_properties%no_input
            str_to_compare = process_properties%input_item(i_input)%name(1:10)
            if (string_equals(str_to_compare, name1)) then
                process_properties%input_item(i_input)%name = name2
            endif
        enddo
        do ioutput = 1, process_properties%no_output
            str_to_compare = process_properties%output_item(ioutput)%name(1:10)
            if(string_equals(str_to_compare, name1)) then
                process_properties%output_item(ioutput)%name = name2
            endif
        enddo
        do istochi = 1, process_properties%no_fluxstochi
            if(string_equals(process_properties%fluxstochi(istochi)%substance(1:10), name1)) then
                process_properties%fluxstochi(istochi)%substance = name2
            endif
            if(string_equals(process_properties%fluxstochi(istochi)%ioitem(1:10), name1)) then
                process_properties%fluxstochi(istochi)%ioitem = name2
            endif
        enddo
        do istochi = 1, process_properties%no_dispstochi
            if(string_equals(process_properties%dispstochi(istochi)%substance(1:10), name1)) then
                process_properties%dispstochi(istochi)%substance = name2
            endif
            if(string_equals(process_properties%dispstochi(istochi)%ioitem(1:10), name1)) then
                process_properties%dispstochi(istochi)%ioitem = name2
            endif
        enddo
        do istochi = 1, process_properties%no_velostochi
            if(string_equals(process_properties%velostochi(istochi)%substance(1:10), name1)) then
                process_properties%velostochi(istochi)%substance = name2
            endif
            if(string_equals(process_properties%velostochi(istochi)%ioitem(1:10), name1)) then
                process_properties%velostochi(istochi)%ioitem = name2
            endif
        enddo
    end subroutine replace_item_names

end module m_algrep
