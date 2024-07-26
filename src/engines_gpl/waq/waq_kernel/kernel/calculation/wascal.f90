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
module m_wascal
    use m_waq_precision

    implicit none

contains


    subroutine wascal (num_waste_loads, num_substances_total, num_substances_transported, num_cells, syname, &
            conc, itime, num_waste_load_types, wastid, wstnam, &
            wsttyp, iwaste, iwtype, waste)
        !
        !     Deltares
        !
        !     CREATED:            : Jan van Beek
        !
        !     FUNCTION            : calls the user defined wasteload routines
        !

        ! global declarations

        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use delwaq_loads, only : wasteloads
        use delwaq_user_wasteloads
        use timers
        implicit none

        ! arguments declarations

        integer(kind = int_wp) :: num_waste_loads
        integer(kind = int_wp) :: num_substances_total
        integer(kind = int_wp) :: num_substances_transported
        integer(kind = int_wp) :: num_cells
        character(len = 20) :: syname(num_substances_total)
        real(kind = real_wp) :: conc(num_substances_total, num_cells)
        integer(kind = int_wp) :: itime
        integer(kind = int_wp) :: num_waste_load_types
        character(len = 20) :: wastid(num_waste_loads)
        character(len = 40) :: wstnam(num_waste_loads)
        character(len = 20) :: wsttyp(num_waste_load_types)
        integer(kind = int_wp) :: iwaste(num_waste_loads)
        integer(kind = int_wp) :: iwtype(num_waste_loads)
        real(kind = real_wp) :: waste(0:num_substances_total, num_waste_loads)

        ! local declarations

        integer(kind = int_wp), save :: ifirst = 1
        integer(kind = int_wp) :: ierror
        integer(kind = int_wp) :: iwst
        integer(kind = int_wp) :: isys
        integer(kind = int_wp) :: lunrep
        character(len = 256) :: load_routine

        integer(kind = int_wp) :: ierr_alloc
        integer(kind = int_wp), save :: ithandl = 0

        if (timon) call timstrt ("wascal", ithandl)

        ! initialise dll

        call get_log_unit_number(lunrep)

        ! update the actual loads from the delwaq arrays to the wasteload structure

        if (ifirst == 1) then
            allocate(wasteloads(num_waste_loads), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(lunrep, *) 'ERROR : allocating wasteloads structure'
                write(*, *) 'ERROR : allocating wasteloads structure'
                call stop_with_error()
            endif
            do iwst = 1, num_waste_loads
                allocate(wasteloads(iwst)%loads(num_substances_total + 1))
                wasteloads(iwst)%id%id = wastid(iwst)
                wasteloads(iwst)%id%name = wstnam(iwst)
                wasteloads(iwst)%id%type = wsttyp(iwtype(iwst))
                wasteloads(iwst)%loc%segnr = iwaste(iwst)
            enddo
        endif
        do iwst = 1, num_waste_loads
            wasteloads(iwst)%flow = waste(0, iwst)
            do isys = 1, num_substances_total
                wasteloads(iwst)%loads(isys) = waste(isys, iwst)
            enddo
        enddo

        ! call routine

        call delwaq_user_wasteload (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
                itime, conc, syname)

        ! updated wasteloads to old delwaq arrays

        do iwst = 1, num_waste_loads
            waste(0, iwst) = wasteloads(iwst)%flow
            do isys = 1, num_substances_total
                waste(isys, iwst) = wasteloads(iwst)%loads(isys)
            enddo
        enddo

        ifirst = 0

        if (timon) call timstop (ithandl)
        return
    end

end module m_wascal
