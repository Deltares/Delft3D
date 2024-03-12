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
module m_prsort
    use m_waq_precision
    use m_string_utils
    use m_valpoi
    use m_error_status

    implicit none

contains


    subroutine prsort(lurep, ProcesDef, notot, nopa, nosfun, &
            syname, nocons, nofun, constants, paname, &
            funame, sfname, status)

        ! sort processes according to input - output relation, simpel linear sort at the moment

        use dlwq_hyd_data
        use ProcesSet
        use timers       !   performance timers

        implicit none

        ! decalaration of arguments

        integer(kind = int_wp) :: lurep           ! unit number report file
        type(ProcesPropColl) :: ProcesDef       ! all processes
        integer(kind = int_wp) :: notot           ! number of substances
        integer(kind = int_wp) :: nopa            ! number of parameters
        integer(kind = int_wp) :: nosfun          ! number of segment functions
        character(len = *) :: syname(*)       ! substance name
        integer(kind = int_wp) :: nocons          ! number of constants
        integer(kind = int_wp) :: nofun           ! number of functions
        type(t_dlwq_item), intent(inout) :: constants       !< delwaq constants list
        character(len = *) :: paname(*)       ! parameter names
        character(len = *) :: funame(*)       ! function names
        character(len = *) :: sfname(*)       ! segment function names

        type(error_status), intent(inout) :: status !< current error status

        ! local declaration

        type(ProcesProp) :: aProces         ! array with proces properties
        integer(kind = int_wp) :: iproc
        integer(kind = int_wp) :: iproc1
        integer(kind = int_wp) :: iproc2
        integer(kind = int_wp) :: nproc
        integer(kind = int_wp) :: i_in, i_out
        integer(kind = int_wp) :: i_flx
        integer(kind = int_wp) :: ifound
        integer(kind = int_wp) :: new_rank
        integer(kind = int_wp) :: i_lowest_rank
        integer(kind = int_wp) :: nloop
        character(len = 20) :: valnam
        integer(kind = int_wp) :: ivalip
        character(len = 100) :: line
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("prsort", ithndl)

        ! loop over the processes

        nproc = ProcesDef%cursize
        i_lowest_rank = 1
        nloop = 0

        do

            if (i_lowest_rank == nproc .or. nloop > nproc) exit

            iproc1 = i_lowest_rank
            i_lowest_rank = nproc
            nloop = nloop + 1
            do iproc = iproc1, ProcesDef%cursize

                ! check if output is used by previous processes

                new_rank = iproc
                do iproc2 = 1, iproc - 1

                    do i_out = 1, ProcesDef%ProcesProps(iproc)%no_output
                        do i_in = 1, ProcesDef%ProcesProps(iproc2)%no_input
                            if (string_equals(ProcesDef%ProcesProps(iproc)%output_item(i_out)%name, &
                                    ProcesDef%ProcesProps(iproc2)%input_item(i_in)%name)) then
                                ! see if it not specified in the input, then the process needs to be moved

                                valnam = ProcesDef%ProcesProps(iproc)%output_item(i_out)%name
                                call valpoi (notot, nopa, nosfun, syname, nocons, &
                                        nofun, constants, paname, funame, sfname, &
                                        valnam, ivalip, line)

                                if (ivalip == -1) then
                                    new_rank = iproc2
                                    goto 10
                                endif

                            endif
                        enddo
                    enddo

                    ! also check fluxes

                    do i_flx = 1, ProcesDef%ProcesProps(iproc)%no_fluxoutput
                        do i_in = 1, ProcesDef%ProcesProps(iproc2)%no_input
                            if (string_equals(ProcesDef%ProcesProps(iproc)%fluxoutput(i_flx)%name, &
                                    ProcesDef%ProcesProps(iproc2)%input_item(i_in)%name)) then

                                ! see if it not specified in the input, then the process needs to be moved

                                valnam = ProcesDef%ProcesProps(iproc)%fluxoutput(i_flx)%name
                                call valpoi (notot, nopa, nosfun, syname, nocons, &
                                        nofun, constants, paname, funame, sfname, &
                                        valnam, ivalip, line)

                                if (ivalip == -1) then
                                    new_rank = iproc2
                                    goto 10
                                endif

                            endif
                        enddo
                    enddo

                enddo
                10       continue

                ! insert process at new position

                if (new_rank < iproc) then
                    i_lowest_rank = min(i_lowest_rank, new_rank)
                    aProces = ProcesDef%ProcesProps(iproc)
                    do iproc2 = iproc, new_rank + 1, -1
                        ProcesDef%ProcesProps(iproc2) = ProcesDef%ProcesProps(iproc2 - 1)
                    enddo
                    ProcesDef%ProcesProps(new_rank) = aProces
                endif

            enddo
        enddo

        ! check if there is conflict, report it but allow it to continue, to be done
        ! this is tricky because the user has no means to influence the final order

        if (nloop > nproc) then
            write(lurep, '(a)') ' WARNING: circular input output relation detected in process library'
            call status%increase_warning_count()
        endif

        if (timon) call timstop(ithndl)
        return
    end

end module m_prsort
