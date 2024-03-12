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
module m_add_sumfrc
    use m_waq_precision

    implicit none

contains


    subroutine add_sumfrc (lunrep, procesdef, allitems, sfracs, no_act, &
            actlst, nbpr)

        ! add calculation of the sum of the fractions

        use m_srstop
        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        type(itempropcoll) :: allitems        ! all items of the proces system
        type(sfracsprop) :: sfracs          ! substance fraction properties
        integer(kind = int_wp) :: no_act          ! number of active processes
        character(len = *) :: actlst(*)       ! active processes names
        integer(kind = int_wp) :: nbpr            ! number of processes

        ! local decalarations

        type(procesprop) :: proc            ! one process definition
        type(itemprop) :: item            ! one item
        integer(kind = int_wp) :: isys            ! loop counter substances
        integer(kind = int_wp) :: iret            ! index in collection
        integer(kind = int_wp) :: ifrac           ! fraction number
        character(len = 3) :: suffix          ! suffix
        integer(kind = int_wp) :: ierr_alloc      ! error indication
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("add_sumfrc", ithndl)

        ! loop over the substances with fractions

        do isys = 1, sfracs%nsfrac

            proc%name = 'SUM_' // sfracs%name(isys)
            proc%routine = 'SUMFRC'
            proc%text = 'sum of the fractions'
            proc%swtransp = 123
            proc%type = PROCESTYPE_OUTPUT
            proc%sfrac_type = 0
            proc%no_input = 1 + sfracs%nfrac(isys)
            proc%no_output = 1 + sfracs%nfrac(isys)
            proc%no_fluxoutput = 0
            proc%no_fluxstochi = 0
            proc%no_dispstochi = 0
            proc%no_velostochi = 0
            allocate(proc%input_item(proc%no_input), proc%output_item(proc%no_output), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(lunrep, *) 'error allocating work array in routine add_sumfrc:', ierr_alloc
                write(lunrep, *) 'array length:', proc%no_input, proc%no_output
                write(*, *) 'error allocating array:', ierr_alloc
                call srstop(1)
            endif

            ! input on segments

            proc%input_item(1)%name = 'nfrac_' // sfracs%name(isys)
            proc%input_item(1)%type = iotype_segment_input
            proc%input_item(1)%actdef = sfracs%nfrac(isys)
            proc%input_item(1)%indx = 1
            proc%input_item(1)%ip_val = 0
            item%name = proc%input_item(1)%name
            iret = itempropcollfind(allitems, item)
            if (iret <= 0) then
                item%text = proc%input_item(1)%name
                item%default = sfracs%nfrac(isys)
                item%waqtype = waqtype_none
                iret = itempropcolladd(allitems, item)
            endif
            proc%input_item(1)%item => allitems%itemproppnts(iret)%pnt

            do ifrac = 1, sfracs%nfrac(isys)
                if (ifrac < 100) then
                    write(suffix, '(i2.2)') ifrac
                else
                    write(suffix, '(i3.3)') ifrac
                endif
                proc%input_item(1 + ifrac)%name = trim(sfracs%name(isys)) // suffix
                proc%input_item(1 + ifrac)%type = iotype_segment_input
                proc%input_item(1 + ifrac)%actdef = 0.0
                proc%input_item(1 + ifrac)%indx = 1 + ifrac
                proc%input_item(1 + ifrac)%ip_val = 0
                item%name = proc%input_item(1 + ifrac)%name
                iret = itempropcollfind(allitems, item)
                if (iret <= 0) then
                    item%text = proc%input_item(1 + ifrac)%name
                    item%default = 0.0
                    item%waqtype = waqtype_none
                    iret = itempropcolladd(allitems, item)
                endif
                proc%input_item(1 + ifrac)%item => allitems%itemproppnts(iret)%pnt
            enddo

            ! output

            item%name = sfracs%name(isys)
            iret = itempropcollfind(allitems, item)
            if (iret <= 0) then
                item%default = -999.
                item%text = sfracs%name(isys)
                item%waqtype = waqtype_none
                iret = itempropcolladd(allitems, item)
            endif

            proc%output_item(1)%name = sfracs%name(isys)
            proc%output_item(1)%type = iotype_segment_output
            proc%output_item(1)%item => allitems%itemproppnts(iret)%pnt
            proc%output_item(1)%indx = 1
            proc%output_item(1)%ip_val = 0

            ! output relative fractions

            do ifrac = 1, sfracs%nfrac(isys)
                if (ifrac < 100) then
                    write(suffix, '(i2.2)') ifrac
                else
                    write(suffix, '(i3.3)') ifrac
                endif
                proc%output_item(1 + ifrac)%name = 'fr' // trim(sfracs%name(isys)) // suffix
                proc%output_item(1 + ifrac)%type = iotype_segment_output
                proc%output_item(1 + ifrac)%actdef = 0.0
                proc%output_item(1 + ifrac)%indx = 1 + ifrac
                proc%output_item(1 + ifrac)%ip_val = 0
                item%name = proc%output_item(1 + ifrac)%name
                iret = itempropcollfind(allitems, item)
                if (iret <= 0) then
                    item%text = proc%output_item(1 + ifrac)%name
                    item%default = -999.
                    item%waqtype = waqtype_none
                    iret = itempropcolladd(allitems, item)
                endif
                proc%output_item(1 + ifrac)%item => allitems%itemproppnts(iret)%pnt
            enddo

            iret = procespropcolladd(procesdef, proc)
            no_act = no_act + 1
            actlst(no_act) = proc%name
            nbpr = nbpr + 1
            write(lunrep, 2000) ' adding sum process [', proc%name, ']'

        enddo

        if (timon) call timstop(ithndl)
        return
        2000 format (3a)
    end

end module m_add_sumfrc
