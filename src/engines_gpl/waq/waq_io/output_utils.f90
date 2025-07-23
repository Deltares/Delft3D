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
module output_utils
    use m_waq_precision

    implicit none

    private
    public :: get_output_pointers, set_default_output, set_output_boot_variables

contains


    subroutine get_output_pointers(num_output_files, nrvar, nrvarm, dlwnam, iopoin, &
            nmis, num_substances_total, syname, num_constants, coname, &
            num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
            sfname, lurep)

        !! Sets the pointers for all extra vars

        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: num_output_files                  !< Number of output files
        integer(kind = int_wp), intent(in) :: nrvar (num_output_files)         !< No of output vars per file
        integer(kind = int_wp), intent(in) :: nrvarm                 !< Maximum of output variables p.p.
        integer(kind = int_wp), intent(out) :: nmis                   !< Number of missing input vars
        character(len=*), intent(in) :: dlwnam(nrvarm, num_output_files)  !< Name of input variables
        integer(kind = int_wp), intent(out) :: iopoin(nrvarm, num_output_files)   !< Number of missing input vars
        integer(kind = int_wp), intent(in) :: num_substances_total                  !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_spatial_parameters                   !< Number of parameters
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions                 !< Number of segment functions
        character(20), intent(in) :: syname(num_substances_total)         !< Names of systems
        integer(kind = int_wp), intent(in) :: num_constants                 !< Number of constants used
        integer(kind = int_wp), intent(in) :: num_time_functions                  !< Number of functions ( user )
        character(20), intent(in) :: coname(num_constants)        !< Constant names
        character(20), intent(in) :: paname(num_spatial_parameters)        !< Parameter names
        character(20), intent(in) :: funame(num_time_functions)        !< Function names
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)        !< Segment function names
        integer(kind = int_wp), intent(in) :: lurep                  !< Unit nr. report file

        character(20) varnam            ! Name of variable to be identified
        integer(kind = int_wp) :: ivarip             ! Pointer in the SSA
        integer(kind = int_wp) :: iout               ! loop variable of outputs
        integer(kind = int_wp) :: inrv               ! loop variable output number
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("get_output_pointers", ithndl)

        write(lurep, *)
        write(lurep, *) ' Determining the place of the output variables'
        write(lurep, *)

        nmis = 0

        do iout = 1, num_output_files
            do inrv = 1, nrvar(iout)
                varnam = dlwnam(inrv, iout)
                if (varnam == ' ') then
                    ivarip = 0
                else
                    call set_output_pointers (num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, syname, num_constants, &
                            num_time_functions, coname, paname, funame, sfname, &
                            varnam, ivarip, lurep)
                    if (ivarip == -1) then
                        nmis = nmis + 1
                        write(lurep, '(3a)') '   INFO:', varnam, &
                                '; NOT FOUND, delwaq will detect variables from process library'
                    endif
                endif
                iopoin(inrv, iout) = ivarip
            end do
        end do

        if (timon) call timstop(ithndl)
        return
    end subroutine get_output_pointers

    subroutine set_output_pointers(num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, syname, num_constants, &
            num_time_functions, coname, paname, funame, sfname, &
            varnam, ivarip, lurep)

        !! sets pointers for output variables
        !!     Logical unitnumbers : lurep   - report file

        use timers       !   performance timers
        use m_string_utils

        integer(kind = int_wp), intent(in) :: num_substances_total              !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_spatial_parameters               !< Number of parameters
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions             !< Number of segment functions
        character(20), intent(in) :: syname(num_substances_total)     !< Names of systems
        integer(kind = int_wp), intent(in) :: num_constants             !< Number of constants used
        integer(kind = int_wp), intent(in) :: num_time_functions              !< Number of functions ( user )
        character(20), intent(in) :: coname(num_constants)    !< Constant names
        character(20), intent(in) :: paname(num_spatial_parameters)    !< Parameter names
        character(20), intent(in) :: funame(num_time_functions)    !< Function names
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)    !< Segment function names
        character(20), intent(in) :: varnam            !< Name of variable to be identified
        integer(kind = int_wp), intent(out) :: ivarip             !< Pointer in the SSA
        integer(kind = int_wp), intent(in) :: lurep              !< Unit nr. report file

        !     Local

        integer(kind = int_wp), parameter :: nopred = 6
        integer(kind = int_wp) :: indx            !  index in array of names
        character(20) predef(3)
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("set_output_pointers", ithndl)

        predef(1) = 'volume'
        predef(2) = 'itime'
        predef(3) = 'idt'

        !     determine how VAL is modelled

        indx = index_in_array(varnam, predef)
        if (indx == 1) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ VOLUME'
            ivarip = 1
            goto 800
        endif
        if (indx == 2) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ ITIME'
            ivarip = 2
            goto 800
        endif
        if (indx == 3) then
            write(lurep, *) '       ', varnam, '; Using DELWAQ IDT'
            ivarip = 3
            goto 800
        endif

        !     as model variable ?

        indx = index_in_array(varnam, syname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using substance nr', indx
            ivarip = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + indx
            goto 800
        endif

        !     as segment function ?

        indx = index_in_array(varnam, sfname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using segment function nr', indx
            ivarip = nopred + num_constants + num_spatial_parameters + num_time_functions + indx
            goto 800
        endif

        !     as function ?

        indx = index_in_array(varnam, funame)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using function nr', indx
            ivarip = nopred + num_constants + num_spatial_parameters + indx
            goto 800
        endif

        !     as parameter ?

        indx = index_in_array(varnam, paname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using parameter nr', indx
            ivarip = nopred + num_constants + indx
            goto 800
        endif

        !     as constant ?

        indx = index_in_array(varnam, coname)
        if (indx > 0) then
            write(lurep, *) '       ', varnam, '; Using constant nr', indx
            ivarip = nopred + indx
            goto 800
        endif

        !     not found

        ivarip = -1

        800 continue

        if (timon) call timstop(ithndl)
        return
    end subroutine set_output_pointers

    subroutine set_default_output(num_output_files, nrvar, iostrt, iostop, iostep, &
            isrtou, igrdou)
        !! Sets default output behavior

        use timers       !   performance timers
        use results, only : ncopt, idmp, ihi3, imap, iba3, iba2, ibal, ima2, imo4, imo3
        use m_timer_variables          ! Timer characteristics

        integer(kind = int_wp), intent(in) :: num_output_files          !< Number of output files
        integer(kind = int_wp), intent(out) :: nrvar (num_output_files)  !< Number of extra output vars
        integer(kind = int_wp), intent(out) :: iostrt(num_output_files)  !< Output start time (scu)
        integer(kind = int_wp), intent(out) :: iostop(num_output_files)  !< Output stop time (scu)
        integer(kind = int_wp), intent(out) :: iostep(num_output_files)  !< Output step time (scu)
        integer(kind = int_wp), intent(out) :: isrtou(num_output_files)  !< Sort output indication
        integer(kind = int_wp), intent(out) :: igrdou(num_output_files)  !< Output grid indication

        integer(kind = int_wp), parameter :: igseg = 1, igmon = 2, iggrd = 3, igsub = 4
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("set_default_output", ithndl)

        ! set output system default action
        ! MONitor file
        iostrt(1) = imstrt
        iostop(1) = imstop
        iostep(1) = imstep
        nrvar (1) = 0
        isrtou(1) = imo3
        igrdou(1) = igsub

        ! GRID file
        iostrt(2) = idstrt
        iostop(2) = idstop
        iostep(2) = idstep
        nrvar (2) = 0
        isrtou(2) = idmp
        igrdou(2) = iggrd

        ! HIStory file
        iostrt(3) = ihstrt
        iostop(3) = ihstop
        iostep(3) = ihstep
        nrvar (3) = 0
        isrtou(3) = ihi3
        igrdou(3) = igsub

        ! MAP file
        iostrt(4) = idstrt
        iostop(4) = idstop
        iostep(4) = idstep
        nrvar (4) = 0
        isrtou(4) = imap
        igrdou(4) = igseg

        ! BAL file
        if (num_output_files >= 5) then
            iostrt(5) = imstrt
            iostop(5) = imstop
            iostep(5) = imstep
            nrvar (5) = 0
            if (mod(intopt, 64) >= 32) then
                isrtou(5) = iba3
            elseif (mod(intopt, 32) >= 16) then
                isrtou(5) = iba2
            else
                isrtou(5) = ibal
            endif
            igrdou(5) = igsub
        endif

        ! NEFIS HIS file
        if (num_output_files >= 6) then
            iostrt(6) = ihstrt
            iostop(6) = ihstop
            iostep(6) = ihstep
            nrvar (6) = 0
            isrtou(6) = 0
            igrdou(6) = igsub
        endif

        ! NEFIS MAP file
        if (num_output_files >= 7) then
            iostrt(7) = idstrt
            iostop(7) = idstop
            iostep(7) = idstep
            nrvar (7) = 0
            isrtou(7) = 0
            igrdou(7) = igseg
        endif

        ! STATistical output MAP
        if (num_output_files >= 8) then
            iostrt(8) = itstop
            iostop(8) = itstop
            iostep(8) = idt
            nrvar (8) = 0
            isrtou(8) = ima2
            igrdou(8) = igseg
        endif

        ! STATistical output MON
        if (num_output_files >= 9) then
            iostrt(9) = itstop
            iostop(9) = itstop
            iostep(9) = idt
            nrvar (9) = 0
            isrtou(9) = imo4
            igrdou(9) = igsub
        endif

        if (timon) call timstop(ithndl)
        return
    end subroutine set_default_output

    subroutine set_output_boot_variables(num_output_files, nrvar, igrdou, isrtou, num_cells, &
            num_monitoring_points, num_cells_u_dir, num_cells_v_dir, num_output_variables_extra, output_buffer_len, &
            ndmpar, num_substances_total, char_arr_buffer_len, num_transects)
        !! Sets the boot variables for OUTPUT system

        use timers       !   performance timers
        use results

        integer(kind = int_wp), intent(in) :: num_output_files          !< Number of output files
        integer(kind = int_wp), intent(in) :: nrvar (num_output_files)  !< Number variables per output file
        integer(kind = int_wp), intent(in) :: igrdou(num_output_files)  !< Output grid indication
        integer(kind = int_wp), intent(in) :: isrtou(num_output_files)  !< Sort output indication
        integer(kind = int_wp), intent(in) :: num_cells
        integer(kind = int_wp), intent(in) :: num_monitoring_points
        integer(kind = int_wp), intent(in) :: num_cells_u_dir
        integer(kind = int_wp), intent(in) :: num_cells_v_dir
        integer(kind = int_wp), intent(out) :: num_output_variables_extra         !< Total number of output variables
        integer(kind = int_wp), intent(out) :: output_buffer_len
        integer(kind = int_wp), intent(in) :: ndmpar         !< number of dump areas
        integer(kind = int_wp), intent(in) :: num_substances_total
        integer(kind = int_wp), intent(out) :: char_arr_buffer_len
        integer(kind = int_wp), intent(in) :: num_transects         !< Number of transects

        integer(kind = int_wp), parameter :: igseg = 1, igmon = 2, iggrd = 3, igsub = 4
        integer(kind = int_wp) :: nocel        !  size of the NEFIS cell
        integer(kind = int_wp) :: nbufou       !  help variable for length output buffer
        integer(kind = int_wp) :: ncbufo       !  help variable for length character buffer
        integer(kind = int_wp) :: iout         !  loop variable
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("set_output_boot_variables", ithndl)

        ! Loop over the output files
        num_output_variables_extra = 0
        output_buffer_len = 0
        do iout = 1, num_output_files
            num_output_variables_extra = num_output_variables_extra + nrvar(iout)

            ! Grid
            select case (igrdou(iout))
            case (igseg)
                nocel = num_cells
            case (igmon)
                nocel = num_monitoring_points
            case (iggrd)
                nocel = num_cells_u_dir * num_cells_v_dir
            case (igsub)
                nocel = ndmpar
            end select

            ! Calculate outputbuffer size for this file, for some (NEFIS,SUB)
            ! also a character buffer size
            ncbufo = 0

            select case (isrtou(iout))
            case (ihnf, imnf)
                ! NEFIS file, extra array with length NOCEL needed
                ! substance names and output names in char buffer.
                nbufou = nocel * (nrvar(iout) + 1)
                ncbufo = num_substances_total + nrvar(iout)
            case (ihn2, imn2)
                ! NEFIS file, extra array with length NOCEL needed
                nbufou = nocel * (nrvar(iout) + 1)
            case (imo3)
                ! On subarea's substances also in buffer, only the
                ! first half of the nrvar are real output vars.
                ! substance names and output names in char buffer.
                nbufou = nocel * (num_substances_total + nrvar(iout) / 2)
                ncbufo = num_substances_total + nrvar(iout) / 2
            case (ihi3)
                ! On subarea's substances also in buffer, only the
                ! first half of the nrvar are real output vars.
                ! substance names and output names in char buffer.
                ! also output for transects
                nbufou = (nocel + num_transects) * (num_substances_total + nrvar(iout) / 2)
                ncbufo = num_substances_total + nrvar(iout) / 2
            case (ihn3)
                ! NEFIS file, extra array with length NOCEL needed
                ! On subarea's substances also in buffer, only the
                ! first half of the nrvar are real output vars.
                ! substance names and output names in char buffer.
                ! also output for transects
                nbufou = (nocel + num_transects) * (num_substances_total + nrvar(iout) / 2 + 1)
                ncbufo = num_substances_total + nrvar(iout) / 2
            case (imo4, ihi4)
                ! On subarea's only the first half of the nrvar are
                ! real output vars.
                nbufou = nocel * (nrvar(iout) / 2)
            case (ihn4)
                ! NEFIS file, extra array with length NOCEL needed
                ! On subarea's only the first half of the nrvar are
                ! real output vars.
                nbufou = nocel * (nrvar(iout) / 2 + 1)
            case default
                ! Rest, normal
                nbufou = nocel * nrvar(iout)
            end select

            ! Buffer is as big as the largest needed
            output_buffer_len = max (output_buffer_len, nbufou)
            char_arr_buffer_len = max (char_arr_buffer_len, ncbufo)
        end do

        if (timon) call timstop(ithndl)

    end subroutine set_output_boot_variables

end module output_utils
