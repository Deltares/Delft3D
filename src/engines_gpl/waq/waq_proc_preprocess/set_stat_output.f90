module m_set_stat_output
    use m_waq_precision

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2024.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine set_stat_output(statprocesdef, num_output_files, ioutps, num_output_variables_extra, outputs)

        !     Deltares Software Centre

        !>/File
        !>      set the output of the statistical processes in output structure

        !     Created   : Aug   2012 by Jan van Beek

        use timers         !< performance timers
        use processet      !< use processet definitions
        use results, only : OutputPointers, imo3, imo4, ihi3, ihi4, ihn4, ihn3, imon, imo2, & !< use results definitions
                idm2, idmp, ihi2, ihis, ihn2, ihnf, imap, ima2, imnf, imn2
        implicit none

        ! arguments

        type(procespropcoll), intent(in) :: statprocesdef          !< the statistical proces definition
        integer(kind = int_wp), intent(in) :: num_output_files                  !< total number of output files
        integer(kind = int_wp), intent(inout) :: ioutps(7, *)            !< (old) output structure
        integer(kind = int_wp), intent(inout) :: num_output_variables_extra                 !< total number of output parameters
        type(OutputPointers), intent(inout) :: outputs                !< output structure

        ! local

        integer(kind = int_wp), allocatable :: iopoi3(:)              !  pointer in the delwaq arrays
        character(len = 20), allocatable :: ounam3(:)              !  name of the output variables
        type(arraypropcoll) :: normal_output          !  normal output list
        type(arraypropcoll) :: stat_output            !  statistical output list
        type(arrayprop) :: aarrayprop             !  one array property  to add into collection
        integer(kind = int_wp) :: ioutp                  !  index variable
        integer(kind = int_wp) :: iout                   !  index variable
        integer(kind = int_wp) :: iout1                  !  index variable
        integer(kind = int_wp) :: iout3                  !  index variable
        integer(kind = int_wp) :: nrvarx                 !  number of extra variables
        integer(kind = int_wp) :: nrvar1                 !  number of variables
        integer(kind = int_wp) :: nrvar2                 !  number of variables
        integer(kind = int_wp) :: istat                  !  loop counter statistical processes
        integer(kind = int_wp) :: iioitem                !  loop counter io items
        integer(kind = int_wp) :: iret                   !  function return code
        integer(kind = int_wp) :: ierr                   !  allocate error code

        integer(kind = int_wp) :: ithndl = 0             !  handle for performance timer
        if (timon) call timstrt("set_stat_output", ithndl)

        ! merge the statistical output with the normal output

        normal_output%current_size = 0
        normal_output%maxsize = 0
        stat_output%current_size = 0
        stat_output%maxsize = 0

        if (statprocesdef%current_size > 0) then
            do istat = 1, statprocesdef%current_size
                do iioitem = 1, statprocesdef%procesprops(istat)%no_output
                    if (statprocesdef%procesprops(istat)%output_item(iioitem)%type == iotype_segment_output) then
                        aarrayprop%name = statprocesdef%procesprops(istat)%output_item(iioitem)%name
                        if (statprocesdef%procesprops(istat)%type == procestype_stat) then
                            iret = arraypropcolladd(stat_output, aarrayprop)
                        else
                            iret = arraypropcolladd(normal_output, aarrayprop)
                        endif
                    endif
                enddo
            enddo
            do ioutp = 1, num_output_files - 2

                ! check if there are weigth variables

                if (ioutps(5, ioutp) == imo3 .or. ioutps(5, ioutp) == imo4 .or. &
                        ioutps(5, ioutp) == ihi3 .or. ioutps(5, ioutp) == ihi4 .or. &
                        ioutps(5, ioutp) == ihn3 .or. ioutps(5, ioutp) == ihn4) then
                    nrvarx = normal_output%current_size * 2
                else
                    nrvarx = normal_output%current_size
                endif

                ! add statistical vars to normal output

                if (ioutps(5, ioutp) == imon .or. ioutps(5, ioutp) == imo2 .or. &
                        ioutps(5, ioutp) == imo3 .or. ioutps(5, ioutp) == imo4 .or. &
                        ioutps(5, ioutp) == idmp .or. ioutps(5, ioutp) == idm2 .or. &
                        ioutps(5, ioutp) == ihis .or. ioutps(5, ioutp) == ihi2 .or. &
                        ioutps(5, ioutp) == ihi3 .or. ioutps(5, ioutp) == ihi4 .or. &
                        ioutps(5, ioutp) == ihnf .or. ioutps(5, ioutp) == ihn2 .or. &
                        ioutps(5, ioutp) == ihn3 .or. ioutps(5, ioutp) == ihn4 .or. &
                        ioutps(5, ioutp) == imap .or. ioutps(5, ioutp) == ima2 .or. &
                        ioutps(5, ioutp) == imnf .or. ioutps(5, ioutp) == imn2) then
                    num_output_variables_extra = num_output_variables_extra + nrvarx
                endif
            enddo
            num_output_variables_extra = num_output_variables_extra + stat_output%current_size * 3

            allocate(iopoi3(num_output_variables_extra))
            allocate(ounam3(num_output_variables_extra))

            ! handle output from statistical processes

            ! for existing active files add the parameters which are not defined on a period for
            ! all but balance file. check whether there is a weight variable.
            ! check the buffer size with routine set_output_boot_variables moved from elsewhere

            iout1 = 0
            iout3 = 0
            do ioutp = 1, num_output_files - 2

                ! check if there are weigth variables

                if (ioutps(5, ioutp) == imo3 .or. ioutps(5, ioutp) == imo4 .or. &
                        ioutps(5, ioutp) == ihi3 .or. ioutps(5, ioutp) == ihi4 .or. &
                        ioutps(5, ioutp) == ihn3 .or. ioutps(5, ioutp) == ihn4) then
                    nrvar1 = ioutps(4, ioutp) / 2
                    nrvar2 = ioutps(4, ioutp) / 2
                else
                    nrvar1 = ioutps(4, ioutp)
                    nrvar2 = 0
                endif

                ! original vars

                do iout = 1, nrvar1
                    iout1 = iout1 + 1
                    iout3 = iout3 + 1
                    ounam3(iout3) = outputs%names   (iout1)
                    iopoi3(iout3) = outputs%pointers(iout1)
                enddo

                ! add statistical vars to normal output

                if (ioutps(5, ioutp) == imon .or. ioutps(5, ioutp) == imo2 .or. &
                        ioutps(5, ioutp) == imo3 .or. ioutps(5, ioutp) == imo4 .or. &
                        ioutps(5, ioutp) == idmp .or. ioutps(5, ioutp) == idm2 .or. &
                        ioutps(5, ioutp) == ihis .or. ioutps(5, ioutp) == ihi2 .or. &
                        ioutps(5, ioutp) == ihi3 .or. ioutps(5, ioutp) == ihi4 .or. &
                        ioutps(5, ioutp) == ihnf .or. ioutps(5, ioutp) == ihn2 .or. &
                        ioutps(5, ioutp) == ihn3 .or. ioutps(5, ioutp) == ihn4 .or. &
                        ioutps(5, ioutp) == imap .or. ioutps(5, ioutp) == ima2 .or. &
                        ioutps(5, ioutp) == imnf .or. ioutps(5, ioutp) == imn2) then
                    ioutps(4, ioutp) = ioutps(4, ioutp) + normal_output%current_size
                    do iout = 1, normal_output%current_size
                        iout3 = iout3 + 1
                        ounam3(iout3) = normal_output%arrayprops(iout)%name
                        iopoi3(iout3) = -1
                    enddo
                endif

                ! weigth variables original vars

                do iout = 1, nrvar2
                    iout1 = iout1 + 1
                    iout3 = iout3 + 1
                    ounam3(iout3) = outputs%names   (iout1)
                    iopoi3(iout3) = outputs%pointers(iout1)
                enddo

                ! add weight variables for statistical vars to normal output

                if (ioutps(5, ioutp) == imo3 .or. ioutps(5, ioutp) == imo4 .or. &
                        ioutps(5, ioutp) == ihi3 .or. ioutps(5, ioutp) == ihi4 .or. &
                        ioutps(5, ioutp) == ihn3 .or. ioutps(5, ioutp) == ihn4) then
                    ioutps(4, ioutp) = ioutps(4, ioutp) + normal_output%current_size
                    do iout = 1, normal_output%current_size
                        iout3 = iout3 + 1
                        ounam3(iout3) = 'volume'
                        iopoi3(iout3) = -1
                    enddo
                endif
            enddo

            ! the output files for statistical output defined on periods (8 = a map file, 9 = a mon file)

            ioutps(4, 8) = stat_output%current_size
            do iout = 1, stat_output%current_size
                iout3 = iout3 + 1
                ounam3(iout3) = stat_output%arrayprops(iout)%name
                iopoi3(iout3) = -1
            enddo
            ioutps(4, 9) = stat_output%current_size * 2
            do iout = 1, stat_output%current_size
                iout3 = iout3 + 1
                ounam3(iout3) = stat_output%arrayprops(iout)%name
                iopoi3(iout3) = -1
            enddo
            do iout = 1, stat_output%current_size
                iout3 = iout3 + 1
                ounam3(iout3) = 'volume'
                iopoi3(iout3) = -1
            enddo

            ! put the local arrays in the output structure

            deallocate(Outputs%names, Outputs%pointers, Outputs%std_var_name, Outputs%units, Outputs%description, STAT = ierr)
            allocate(Outputs%names(num_output_variables_extra), Outputs%pointers(num_output_variables_extra), Outputs%std_var_name(num_output_variables_extra), &
                    Outputs%units(num_output_variables_extra), Outputs%description(num_output_variables_extra), STAT = ierr)
            outputs%current_size = num_output_variables_extra
            outputs%pointers(1:num_output_variables_extra) = iopoi3(1:num_output_variables_extra)
            outputs%names(1:num_output_variables_extra) = ounam3(1:num_output_variables_extra)

        endif

        if (ioutps(4, 8) == 0) ioutps(5, 8) = 0
        if (ioutps(4, 9) == 0) ioutps(5, 9) = 0

        if (timon) call timstop(ithndl)
        return
    end

end module m_set_stat_output
