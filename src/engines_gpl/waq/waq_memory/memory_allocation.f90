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

module memory_allocation
    use m_waq_precision
    use m_logger_helper, only : stop_with_error
    implicit none

    interface allocate_array
        module procedure allocate_real_wp_1d_array
        module procedure allocate_int_wp_1d_array
        module procedure allocate_dp_2d_real_array
        module procedure allocate_real_wp_2d_array
    end interface allocate_array

    private
    public :: set_admin_array_indices, set_character_array_indices, allocate_integer_arrays, allocate_real_arrays

contains

    subroutine set_admin_array_indices(logical_unit, integer_array, character_array, partition_data)
        ! sets the array pointers for the array administration array's. declares memory.

        use m_array_manipulation, only : make_pointer, memory_partition, char_type, int_type
        use m_sysn          ! system characteristics
        use m_sysi          ! timer characteristics
        ! pointers in real array workspace
        use m_sysa, only : iasize
        ! pointers in integer array workspace
        use m_sysj, only : iapoi, iatyp, iabyt, ialen, iaknd, iadm1, iadm2, iadm3, ijsize
        ! pointers in character array workspace
        use m_sysc

        integer(kind = int_wp), intent(in) :: logical_unit            !! logical unitnumber output file
        integer(kind = int_wp), intent(out) :: integer_array(:)       !! integer workspace array
        character(len = *), intent(out) :: character_array(:)         !! character workspace array
        type(memory_partition), intent(inout) :: partition_data       !! private variables for make_pointer

        character(len = 20) :: arrnam   !! name of the arrays in the pmsa
        integer(kind = int_wp) :: iiapoi, iiatyp, iiabyt, iialen, iiaknd, iiadm1, iiadm2, iiadm3, iianam, i
        character(len = *), dimension(9), parameter :: array_name_list = [ 'ARRPOI', 'ARRTYP', 'ARRBYT', 'ARRLEN', &
                'ARRKND', 'ARRDM1', 'ARRDM2', 'ARRDM3', 'ARRNAM']
        integer(kind = int_wp), dimension(9) :: arrays

        iiapoi = iasize + 1
        iiatyp = iasize + 2
        iiabyt = iasize + 3
        iialen = iasize + 4
        iiaknd = iasize + 5
        iiadm1 = iasize + 6
        iiadm2 = iasize + 7
        iiadm3 = iasize + 8

        iianam = iasize + ijsize + 1

        ! first set and declare memory for array administration
        ! directly use the array with pointers
        arrnam = 'ARRPOI'
        iapoi = make_pointer(partition_data, int_type, noarr)
        if (iapoi == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iapoi = iapoi + 1

        arrnam = 'ARRTYP'
        iatyp = make_pointer(partition_data, int_type, noarr)
        if (iatyp == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iatyp = iatyp + 1

        arrnam = 'ARRBYT'
        iabyt = make_pointer(partition_data, int_type, noarr)
        if (iabyt == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iabyt = iabyt + 1

        arrnam = 'ARRLEN'
        ialen = make_pointer(partition_data, int_type, noarr)
        if (ialen == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        ialen = ialen + 1

        arrnam = 'ARRKND'
        iaknd = make_pointer(partition_data, int_type, noarr)
        if (iaknd == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iaknd = iaknd + 1

        arrnam = 'ARRDM1'
        iadm1 = make_pointer(partition_data, int_type, noarr)
        if (iadm1 == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iadm1 = iadm1 + 1

        arrnam = 'ARRDM2'
        iadm2 = make_pointer(partition_data, int_type, noarr)
        if (iadm2 == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iadm2 = iadm2 + 1

        arrnam = 'ARRDM3'
        iadm3 = make_pointer(partition_data, int_type, noarr)
        if (iadm3 == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) arrnam
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        iadm3 = iadm3 + 1

        !        arrnam = 'ARRNAM'
        !        ianam = make_pointer(partition_data, char_type, noarr * 20)
        !        if (ianam == 0) then
        !            write(logical_unit, 2010)
        !            write(logical_unit, 2020) arrnam
        !            write(logical_unit, 2030) noarr * 20
        !            call stop_with_error()
        !        endif
        !        ianam = ianam + 1

        !        arrays = [iapoi, iatyp, iabyt, ialen, iaknd, iadm1, iadm2, iadm3, ianam]
        !        do i = 1, 8
        !            call abstract_1(logical_unit, array_name_list(i), noarr, arrays(i), partition_data, int_type)
        !        end do

        call abstract_1(logical_unit, 'ARRNAM', noarr * 20, ianam, partition_data, char_type)

        ! fill the array's themselves
        integer_array(iapoi + iiapoi - 1) = iapoi
        integer_array(iapoi + iiatyp - 1) = iatyp
        integer_array(iapoi + iiabyt - 1) = iabyt
        integer_array(iapoi + iialen - 1) = ialen
        integer_array(iapoi + iiaknd - 1) = iaknd
        integer_array(iapoi + iiadm1 - 1) = iadm1
        integer_array(iapoi + iiadm2 - 1) = iadm2
        integer_array(iapoi + iiadm3 - 1) = iadm3
        integer_array(iapoi + iianam - 1) = ianam

        integer_array(iatyp + iiapoi - 1) = int_type
        integer_array(iatyp + iiatyp - 1) = int_type
        integer_array(iatyp + iiabyt - 1) = int_type
        integer_array(iatyp + iialen - 1) = int_type
        integer_array(iatyp + iiaknd - 1) = int_type
        integer_array(iatyp + iiadm1 - 1) = int_type
        integer_array(iatyp + iiadm2 - 1) = int_type
        integer_array(iatyp + iiadm3 - 1) = int_type
        integer_array(iatyp + iianam - 1) = char_type

        integer_array(iabyt + iiapoi - 1) = 4
        integer_array(iabyt + iiatyp - 1) = 4
        integer_array(iabyt + iiabyt - 1) = 4
        integer_array(iabyt + iialen - 1) = 4
        integer_array(iabyt + iiaknd - 1) = 4
        integer_array(iabyt + iiadm1 - 1) = 4
        integer_array(iabyt + iiadm2 - 1) = 4
        integer_array(iabyt + iiadm3 - 1) = 4
        integer_array(iabyt + iianam - 1) = 20

        integer_array(ialen + iiapoi - 1) = noarr
        integer_array(ialen + iiatyp - 1) = noarr
        integer_array(ialen + iiabyt - 1) = noarr
        integer_array(ialen + iialen - 1) = noarr
        integer_array(ialen + iiaknd - 1) = noarr
        integer_array(ialen + iiadm1 - 1) = noarr
        integer_array(ialen + iiadm2 - 1) = noarr
        integer_array(ialen + iiadm3 - 1) = noarr
        integer_array(ialen + iianam - 1) = noarr

        integer_array(iaknd + iiapoi - 1) = 1
        integer_array(iaknd + iiatyp - 1) = 1
        integer_array(iaknd + iiabyt - 1) = 1
        integer_array(iaknd + iialen - 1) = 1
        integer_array(iaknd + iiaknd - 1) = 1
        integer_array(iaknd + iiadm1 - 1) = 1
        integer_array(iaknd + iiadm2 - 1) = 1
        integer_array(iaknd + iiadm3 - 1) = 1
        integer_array(iaknd + iianam - 1) = 1

        integer_array(iadm1 + iiapoi - 1) = noarr
        integer_array(iadm1 + iiatyp - 1) = noarr
        integer_array(iadm1 + iiabyt - 1) = noarr
        integer_array(iadm1 + iialen - 1) = noarr
        integer_array(iadm1 + iiaknd - 1) = noarr
        integer_array(iadm1 + iiadm1 - 1) = noarr
        integer_array(iadm1 + iiadm2 - 1) = noarr
        integer_array(iadm1 + iiadm3 - 1) = noarr
        integer_array(iadm1 + iianam - 1) = noarr

        integer_array(iadm2 + iiapoi - 1) = 1
        integer_array(iadm2 + iiatyp - 1) = 1
        integer_array(iadm2 + iiabyt - 1) = 1
        integer_array(iadm2 + iialen - 1) = 1
        integer_array(iadm2 + iiaknd - 1) = 1
        integer_array(iadm2 + iiadm1 - 1) = 1
        integer_array(iadm2 + iiadm2 - 1) = 1
        integer_array(iadm2 + iiadm3 - 1) = 1
        integer_array(iadm2 + iianam - 1) = 1

        integer_array(iadm3 + iiapoi - 1) = 1
        integer_array(iadm3 + iiatyp - 1) = 1
        integer_array(iadm3 + iiabyt - 1) = 1
        integer_array(iadm3 + iialen - 1) = 1
        integer_array(iadm3 + iiaknd - 1) = 1
        integer_array(iadm3 + iiadm1 - 1) = 1
        integer_array(iadm3 + iiadm2 - 1) = 1
        integer_array(iadm3 + iiadm3 - 1) = 1
        integer_array(iadm3 + iianam - 1) = 1

        character_array(iiapoi) = 'ARRPOI'
        character_array(iiatyp) = 'ARRTYP'
        character_array(iiabyt) = 'ARRBYT'
        character_array(iialen) = 'ARRLEN'
        character_array(iiaknd) = 'ARRKND'
        character_array(iiadm1) = 'ARRDM1'
        character_array(iiadm2) = 'ARRDM2'
        character_array(iiadm3) = 'ARRDM3'
        character_array(iianam) = 'ARRNAM'

        return
        2010 format (' ERROR  : allocating administration array')
        2020 format (' NAME   : ', a)
        2030 format (' LENGTH : ', i12)
    end subroutine set_admin_array_indices

    subroutine set_character_array_indices(logical_unit, declare_memory, arrpoi, arrtyp, arrbyt, &
            arrlen, arrknd, arrdm1, arrdm2, arrdm3, arrnam, char_arr_size, partition_data)

        ! Sets the array pointers in the SYSC common block. Gives array space of the kind C(pointer)
        ! Declares memory through C-interface if asked (routine is also called by preprocessor)

        ! module for computing the pointers into the arrays
        use m_array_manipulation, only : make_pointer, memory_partition, char_type
        ! System characteristics
        use m_sysn, only : NOTOT, NODUMP, NOBND, NOBTYP, NOWST, NOWTYP, NOCONS, NOPA, NOFUN, NOSFUN, NY, NPROC, &
                NRVART, NODISP, NOVELO, NDMPAR, NORAAI, NCBUFM, NUFIL
        ! Timer characteristics
        use m_sysi
        ! Pointers in real array workspace
        use m_sysa
        ! Pointers in integer array workspace
        use m_sysj
        ! Pointers in character array workspace
        use m_sysc

        integer(kind = int_wp), intent(in) :: logical_unit    ! logical unitnumber output file
        logical, intent(in) :: declare_memory    ! Declare memory y/n
        integer(kind = int_wp), intent(inout) :: arrpoi(:) ! Pointer in workarray/FMM reference pointer
        integer(kind = int_wp), intent(inout) :: arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrlen(:) ! Length off array
        integer(kind = int_wp), intent(inout) :: arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
        integer(kind = int_wp), intent(inout) :: arrdm1(:) ! dimension 1
        integer(kind = int_wp), intent(inout) :: arrdm2(:) ! dimension 2
        integer(kind = int_wp), intent(inout) :: arrdm3(:) ! dimension 3 ( number of grids mostly )
        character(20), intent(inout) :: arrnam(:) ! Array name
        integer(kind = int_wp), intent(inout) :: char_arr_size     ! Required array space
        type(memory_partition), intent(inout) :: partition_data ! Private variables for make_pointer

        character(len = 20) :: temp_array_name                      ! help variable for array name
        integer(kind = int_wp) :: index_name, index_monitor_name, index_solver_name, index_dump_name, index_bound_id, &
                index_bound_name, index_bound_type, index_waste_id, index_waste_name, index_waste_type, &
                index_const_name, index_param_name, index_func_name, index_segment_func, index_edit, &
                index_print_name, index_output_name, index_dump_int_name, index_velocity_name, index_dump_analysis, &
                index_react_name, index_char_buffer, index_log_unit, index_output_short_name, index_output_unit, &
                index_output_desc, index_solver_short_name, index_solver_unit, index_solver_desc
        integer(kind = int_wp) :: i_car, iartyp, iarlen, ip

        ! index_name = IASIZE + IJSIZE + 1
        index_monitor_name = iasize + ijsize + 2
        index_solver_name = iasize + ijsize + 3
        index_dump_name = iasize + ijsize + 4
        index_bound_id = iasize + ijsize + 5
        index_bound_name = iasize + ijsize + 6
        index_bound_type = iasize + ijsize + 7
        index_waste_id = iasize + ijsize + 8
        index_waste_name = iasize + ijsize + 9
        index_waste_type = iasize + ijsize + 10
        index_const_name = iasize + ijsize + 11
        index_param_name = iasize + ijsize + 12
        index_func_name = iasize + ijsize + 13
        index_segment_func = iasize + ijsize + 14
        index_edit = iasize + ijsize + 15
        index_print_name = iasize + ijsize + 16
        index_output_name = iasize + ijsize + 17
        index_dump_int_name = iasize + ijsize + 18
        index_velocity_name = iasize + ijsize + 19
        index_dump_analysis = iasize + ijsize + 20
        index_react_name = iasize + ijsize + 21
        index_char_buffer = iasize + ijsize + 22
        index_log_unit = iasize + ijsize + 23
        index_output_short_name = iasize + ijsize + 24
        index_output_unit = iasize + ijsize + 25
        index_output_desc = iasize + ijsize + 26
        index_solver_short_name = iasize + ijsize + 27
        index_solver_unit = iasize + ijsize + 28
        index_solver_desc = iasize + ijsize + 29

        ! set defaults, no name no length
        ! don't declare the first array , arrnam
        do i_car = iasize + ijsize + 1, iasize + ijsize + nr_car
            arrnam(i_car) = ' '
            arrtyp(i_car) = char_type
            arrbyt(i_car) = 4
            arrknd(i_car) = 1
            arrdm1(i_car) = 0
            arrdm2(i_car) = 1
            arrdm3(i_car) = 1
            arrlen(i_car) = 0
        enddo

        !  Set the characteristics
        arrnam(index_monitor_name) = 'MNAME '
        arrdm1(index_monitor_name) = 8

        arrnam(index_solver_name) = 'SNAME '
        arrdm1(index_solver_name) = NOTOT

        arrnam(index_dump_name) = 'DNAME '
        arrdm1(index_dump_name) = NODUMP

        arrnam(index_bound_id) = 'BNDID '
        arrdm1(index_bound_id) = NOBND

        arrnam(index_bound_name) = 'BNAME '
        arrdm1(index_bound_name) = NOBND * 2

        arrnam(index_bound_type) = 'BNTYP '
        arrdm1(index_bound_type) = NOBTYP

        arrnam(index_waste_id) = 'WASTID'
        arrdm1(index_waste_id) = NOWST

        arrnam(index_waste_name) = 'WNAME '
        arrdm1(index_waste_name) = NOWST * 2

        arrnam(index_waste_type) = 'WTYPE '
        arrdm1(index_waste_type) = NOWTYP

        arrnam(index_const_name) = 'CONAM '
        arrdm1(index_const_name) = NOCONS

        arrnam(index_param_name) = 'PANAM '
        arrdm1(index_param_name) = NOPA

        arrnam(index_func_name) = 'FUNAM '
        arrdm1(index_func_name) = NOFUN

        arrnam(index_segment_func) = 'SFNAM '
        arrdm1(index_segment_func) = NOSFUN

        arrnam(index_edit) = 'CGRID '
        arrdm1(index_edit) = NY * 6

        arrnam(index_print_name) = 'PRNAM '
        arrdm1(index_print_name) = NPROC

        arrnam(index_output_name) = 'OUNAM '
        arrdm1(index_output_name) = NRVART

        arrnam(index_dump_int_name) = 'DINAM '
        arrdm1(index_dump_int_name) = NODISP

        arrnam(index_velocity_name) = 'VENAM '
        arrdm1(index_velocity_name) = NOVELO

        arrnam(index_dump_analysis) = 'DANAM '
        arrdm1(index_dump_analysis) = NDMPAR

        arrnam(index_react_name) = 'RANAM '
        arrdm1(index_react_name) = NORAAI

        arrnam(index_char_buffer) = 'CBUFF '
        arrdm1(index_char_buffer) = NCBUFM

        arrnam(index_log_unit) = 'LUNT  '
        arrdm1(index_log_unit) = NUFIL * 10

        arrnam(index_output_short_name) = 'OUSNM '
        arrdm1(index_output_short_name) = NRVART * 5

        arrnam(index_output_unit) = 'OUUNI '
        arrdm1(index_output_unit) = NRVART * 2

        arrnam(index_output_desc) = 'OUDSC '
        arrdm1(index_output_desc) = NRVART * 3

        arrnam(index_solver_short_name) = 'OSSNM '
        arrdm1(index_solver_short_name) = NOTOT * 5

        arrnam(index_solver_unit) = 'OSUNI '
        arrdm1(index_solver_unit) = NOTOT * 2

        arrnam(index_solver_desc) = 'OSDSC '
        arrdm1(index_solver_desc) = NOTOT * 3

        ! the total array length
        if (.not. declare_memory) then
            write (328, '(/a/a/)') "  => CHARACTER arrays 4-byte words <=", &
                    "  nr array name            array size"
        endif

        char_arr_size = 0
        do i_car = iasize + ijsize + 1, iasize + ijsize + nr_car
            arrlen(i_car) = arrdm1(i_car) * arrdm2(i_car) * arrdm3(i_car) * 5
            if (.not. declare_memory) write (328, 2040) i_car - iasize - ijsize, arrnam(i_car), arrlen(i_car)
            char_arr_size = char_arr_size + arrlen(i_car)
            if (char_arr_size < 0) then
                write(logical_unit, 2005)
                call stop_with_error()
            endif
            arrlen(i_car) = arrlen(i_car) * 4
        enddo

        ! raaien behind the dump areas
        arrlen(index_dump_analysis) = arrlen(index_dump_analysis) + arrlen(index_react_name)
        arrlen(index_react_name) = 0

        ! Allocate array's, set pointers in common block
        ! Don't declare the first array , arrnam
        if (declare_memory) then
            do i_car = iasize + ijsize + 2, iasize + ijsize + nr_car
                iartyp = arrtyp(i_car)
                iarlen = arrlen(i_car)
                temp_array_name = arrnam(i_car)
                if (iarlen > 0) then
                    ip = make_pointer(partition_data, iartyp, iarlen)
                    if (ip <= 0) then
                        write(logical_unit, 2010) temp_array_name
                        call stop_with_error()
                    endif
                else
                    ip = 0
                endif

                ! Add one extra because of the shift between ibuf(0) and integer_array(1)
                ip = ip + 1
                ip_car(i_car - iasize - ijsize) = ip
                arrpoi(i_car) = ip
            enddo
        endif
        if (.not. declare_memory) write (328, '(/5x,a20,i12)') "Total (4 byte words)", char_arr_size

        ! raaien behind the dump areas
        ip_car(index_react_name - iasize - ijsize) = ip_car(index_dump_analysis - iasize - ijsize) + ndmpar * 20
        arrpoi(index_react_name) = arrpoi(index_dump_analysis) + ndmpar * 20
        char_arr_size = char_arr_size + ndmpar

        close (328)

        2005 format (' ERROR  : character array is too big. Unable to create pointer. ')
        2010 format (' ERROR  : allocating character array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)

    end subroutine set_character_array_indices

    subroutine allocate_integer_arrays (logical_unit, declare_memory, arrpoi, arrtyp, arrbyt, &
            arrlen, arrknd, arrdm1, arrdm2, arrdm3, &
            arrnam, int_arr_size, partition_data)

        !! Allocates all integer arrays of DelwaQ
        !!  This routine:
        !!      - Sets the array pointers in the SYSJ common block
        !!      - Gives array space of the kind integer_array(pointer), for output
        !!      - Declares memory through C-interface if asked for
        !!      - Also has a new partition_data allocating arrays in the variable_declaration module

        ! module with the more recently added arrays
        use variable_declaration
        !! module for computing the pointers into the arrays
        use m_array_manipulation, only : make_pointer, memory_partition, int_type
        ! System characteristics
        use m_sysn, only : NOSEG, NSEG2, NOSYS, NOTOT, NOQ, NOQ4, NODUMP, NOBND, NOBTYP, NOWST, NOWTYP, NOCONS, NOPA, &
                NOFUN, NOSFUN, NY, NPROC, NRVART, NODISP, NOVELO, NDMPAR, NORAAI, NCBUFM, NUFIL, NIHARM, NOITEM, &
                nx, nipmsa, noutp, ifiopk, npoins, nogrid, ntdmpq, ntraaq, newisp, nomat, insize, ntdmps, nmax, mmax, &
                novar, nrref, ndspn, nveln, nothrd
        ! Timer characteristics
        use m_sysi
        ! Pointers in real array workspace
        use m_sysa
        ! Pointers in integer array workspace
        use m_sysj, only : ipror, iprvpt, iprdon, ip_jar, idpnw, idpnt, ivpnt, ivpnw, NR_JAR
        ! Pointers in character array workspace
        use m_sysc
        use omp_lib

        integer(kind = int_wp), intent(in) :: logical_unit    ! logical unitnumber output file
        logical, intent(in) :: declare_memory    ! Declare memory y/n
        integer(kind = int_wp), intent(inout) :: arrpoi(:) ! Pointer in workarray/FMM reference pointer
        integer(kind = int_wp), intent(inout) :: arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrlen(:) ! Length off array
        integer(kind = int_wp), intent(inout) :: arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
        integer(kind = int_wp), intent(inout) :: arrdm1(:) ! dimension 1
        integer(kind = int_wp), intent(inout) :: arrdm2(:) ! dimension 2
        integer(kind = int_wp), intent(inout) :: arrdm3(:) ! dimension 3 ( number of grids mostly )
        character(len = 20), intent(inout) :: arrnam(:) ! Array name
        type(memory_partition), intent(inout) :: partition_data ! Private variables for make_pointer
        integer(kind = int_wp), intent(inout) :: int_arr_size     ! Required array space

        ! Local declarations
        logical :: fluxco                            ! if .true. then flux correction
        logical :: steady                            ! if .true. then steady state computation
        logical :: iterat                            ! if .true. then iterative solution
        logical :: delmat                            ! if .true. then direct Gauss solver
        logical :: f_solv                            ! if .true. then GMRES Krilov solver
        logical :: balans                            ! if .true. then balances to be computed
        character(len = 20) :: temp_array_name                            ! help variable for array name
        integer(kind = int_wp) :: noth                              ! number of available thread for parallel processing
        integer(kind = int_wp) :: ierr                              ! error indicator
        integer(kind = int_wp) :: jstart                            ! lower limit Flow arrays method 19 and 20
        integer(kind = int_wp) :: nmmaxj                            ! upper limit Flow arrays method 19 and 20
        integer(kind = int_wp) :: nr_jar_new                        ! counter for newly allocated arrays
        integer(kind = int_wp) :: iibulk, iilp, iigrid, iinsva, iiiflu, iiipms, iiipss, iiimod, iiiout, iiiopo, &
                iiknmr, iiktim, iiqdmp, iisdmp, iipdmp, iioraa, inqraa, iiqraa, iinisp, iintyp, iiwork, ijtrac, &
                iimat, iiwrk, iisysn, iisysi, iikfu, iikfv, iikcs, iikfs, iilgra, iikbnd, iipgrd, iipndt, iipvar, &
                iiptyp, iivarr, iividx, iivtda, iivdag, iivtag, iivagg, iivset, iignos, iigref, iigseg, &
                iidmpb, iiapoi, iiatyp, iiabyt, iialen, iiaknd, iiadm1, iiadm2, iiadm3, iixpnt, iidump, iibpnt, &
                iiwast, iidpnw, iidpnt, iivpnw, iivpnt, iinrha, iinrh2, iinrft, i_jar

        integer(kind = int_wp) :: iartyp, iarlen, ip

        iiapoi = iasize + 1
        iiatyp = iasize + 2
        iiabyt = iasize + 3
        iialen = iasize + 4
        iiaknd = iasize + 5
        iiadm1 = iasize + 6
        iiadm2 = iasize + 7
        iiadm3 = iasize + 8
        iixpnt = iasize + 9
        iidump = iasize + 10
        iibpnt = iasize + 11
        iiwast = iasize + 12
        iidpnw = iasize + 13
        iidpnt = iasize + 14
        iivpnw = iasize + 15
        iivpnt = iasize + 16
        iinrha = iasize + 17
        iinrh2 = iasize + 18
        iinrft = iasize + 19
        iibulk = iasize + 20
        iilp = iasize + 21
        iigrid = iasize + 22
        iinsva = iasize + 23
        iiiflu = iasize + 24
        iiipms = iasize + 25
        iiipss = iasize + 26
        iiimod = iasize + 27
        iiiout = iasize + 28
        iiiopo = iasize + 29
        iiknmr = iasize + 30
        iiktim = iasize + 31
        iiqdmp = iasize + 32
        iisdmp = iasize + 33
        iipdmp = iasize + 34
        iioraa = iasize + 35
        inqraa = iasize + 36
        iiqraa = iasize + 37
        iinisp = iasize + 38
        iintyp = iasize + 39
        iiwork = iasize + 40
        ijtrac = iasize + 41
        iimat = iasize + 42
        iiwrk = iasize + 43
        iisysn = iasize + 44
        iisysi = iasize + 45
        iikfu = iasize + 46
        iikfv = iasize + 47
        iikcs = iasize + 48
        iikfs = iasize + 49
        iilgra = iasize + 50
        iikbnd = iasize + 51
        iipgrd = iasize + 52
        iipndt = iasize + 53
        iipvar = iasize + 54
        iiptyp = iasize + 55
        iivarr = iasize + 56
        iividx = iasize + 57
        iivtda = iasize + 58
        iivdag = iasize + 59
        iivtag = iasize + 60
        iivagg = iasize + 61
        iivset = iasize + 62
        iignos = iasize + 63
        iigref = iasize + 64
        iigseg = iasize + 65
        ipror = iasize + 66
        iprvpt = iasize + 67
        iprdon = iasize + 68
        iidmpb = iasize + 69

        ! Some logicals
        fluxco = intsrt ==  5 .or. intsrt == 12 .or. intsrt == 14 .or. &
                intsrt == 24
        steady = intsrt ==  6 .or. intsrt ==  7 .or. intsrt ==  8 .or. &
                intsrt ==  9 .or. intsrt == 17 .or. intsrt == 18
        iterat = intsrt ==  8 .or. intsrt ==  9
        delmat = intsrt ==  6 .or. intsrt ==  7
        f_solv = intsrt == 15 .or. intsrt == 16 .or. intsrt == 17 .or. &
                intsrt == 18 .or. intsrt == 21 .or. intsrt == 22
        balans = btest(intopt, 3)

        ! Set defaults, no name no length
        DO I_JAR = 9 + IASIZE, NR_JAR + IASIZE
            arrnam(I_JAR) = ' '
            ARRTYP(I_JAR) = int_type
            ARRBYT(I_JAR) = 4
            arrknd(I_JAR) = 0
            arrdm1(I_JAR) = 0
            arrdm2(I_JAR) = 0
            arrdm3(I_JAR) = 0
            ARRLEN(I_JAR) = 0
        ENDDO

        arrnam(IIXPNT) = 'IXPOIN'
        arrknd(IIXPNT) = 3
        arrdm1(IIXPNT) = 4
        arrdm2(IIXPNT) = NOQ + NOQ4
        arrdm3(IIXPNT) = 1

        arrnam(IIDUMP) = 'IIDUMP'
        arrknd(IIDUMP) = 1
        arrdm1(IIDUMP) = NODUMP
        arrdm2(IIDUMP) = 1
        arrdm3(IIDUMP) = 1

        arrnam(IIBPNT) = 'IBPNT '
        arrknd(IIBPNT) = 2
        arrdm1(IIBPNT) = 4
        arrdm2(IIBPNT) = NOBND
        arrdm3(IIBPNT) = 1

        arrnam(IIWAST) = 'IWAST '
        arrknd(IIWAST) = 1
        arrdm1(IIWAST) = NOWST
        arrdm2(IIWAST) = 1
        arrdm3(IIWAST) = 1

        arrnam(IIDPNW) = 'IDPNEW'
        arrknd(IIDPNW) = 1
        arrdm1(IIDPNW) = NOSYS
        arrdm2(IIDPNW) = 1
        arrdm3(IIDPNW) = 1

        arrnam(IIDPNT) = 'IDPNT '
        arrknd(IIDPNT) = 1
        arrdm1(IIDPNT) = NOSYS
        arrdm2(IIDPNT) = 1
        arrdm3(IIDPNT) = 1

        arrnam(IIVPNW) = 'IVPNEW'
        arrknd(IIVPNW) = 1
        arrdm1(IIVPNW) = NOSYS
        arrdm2(IIVPNW) = 1
        arrdm3(IIVPNW) = 1

        arrnam(IIVPNT) = 'IVPNT '
        arrknd(IIVPNT) = 1
        arrdm1(IIVPNT) = NOSYS
        arrdm2(IIVPNT) = 1
        arrdm3(IIVPNT) = 1

        arrnam(IINRHA) = 'IHARM '
        arrknd(IINRHA) = 1
        arrdm1(IINRHA) = NIHARM
        arrdm2(IINRHA) = 1
        arrdm3(IINRHA) = 1

        arrnam(IINRH2) = 'NRHARM'
        arrknd(IINRH2) = 1
        arrdm1(IINRH2) = NOITEM
        arrdm2(IINRH2) = 1
        arrdm3(IINRH2) = 1

        arrnam(IINRFT) = 'NRFTOT'
        arrknd(IINRFT) = 1
        arrdm1(IINRFT) = NOITEM
        arrdm2(IINRFT) = 1
        arrdm3(IINRFT) = 1

        NPOINS = NPOINS + 1           ! Try to avoid problem with the debugger

        arrnam(IIBULK) = 'IPOINT'
        arrknd(IIBULK) = 1
        arrdm1(IIBULK) = NPOINS
        arrdm2(IIBULK) = 1
        arrdm3(IIBULK) = 1

        arrnam(IILP) = 'IP    '
        arrknd(IILP) = 1
        arrdm1(IILP) = 8
        arrdm2(IILP) = 1
        arrdm3(IILP) = 1

        arrnam(IIGRID) = 'LGRID '
        arrknd(IIGRID) = 1
        arrdm1(IIGRID) = NX
        arrdm2(IIGRID) = NY
        arrdm3(IIGRID) = 1

        arrnam(IINSVA) = 'NSVAR '
        arrknd(IINSVA) = 1
        arrdm1(IINSVA) = NPROC
        arrdm2(IINSVA) = 1
        arrdm3(IINSVA) = 1

        arrnam(IIIFLU) = 'IFLUX '
        arrknd(IIIFLU) = 1
        arrdm1(IIIFLU) = NPROC
        arrdm2(IIIFLU) = 1
        arrdm3(IIIFLU) = 1

        arrnam(IIIPMS) = 'IPMSA '
        arrknd(IIIPMS) = 1
        arrdm1(IIIPMS) = NIPMSA
        arrdm2(IIIPMS) = 1
        arrdm3(IIIPMS) = 1

        arrnam(IIIPSS) = 'IPSSA '
        arrknd(IIIPSS) = 1
        arrdm1(IIIPSS) = NIPMSA
        arrdm2(IIIPSS) = 1
        arrdm3(IIIPSS) = 1

        arrnam(IIIMOD) = 'IMODU '
        arrknd(IIIMOD) = 1
        arrdm1(IIIMOD) = NPROC
        arrdm2(IIIMOD) = 1
        arrdm3(IIIMOD) = 1

        arrnam(IIIOUT) = 'IOUTPS'
        arrknd(IIIOUT) = 2
        arrdm1(IIIOUT) = 7
        arrdm2(IIIOUT) = NOUTP
        arrdm3(IIIOUT) = 1

        arrnam(IIIOPO) = 'IOPOIN'
        arrknd(IIIOPO) = 1
        arrdm1(IIIOPO) = NRVART
        arrdm2(IIIOPO) = 1
        arrdm3(IIIOPO) = 1

        arrnam(IIKNMR) = 'IKNMRK'
        arrknd(IIKNMR) = 3
        arrdm1(IIKNMR) = NOSEG + NSEG2
        IF (IFIOPK == 0) THEN
            arrdm2(IIKNMR) = 1
        ELSEIF (IFIOPK == 1) THEN
            arrdm2(IIKNMR) = 3
        ELSE
            arrdm2(IIKNMR) = 4
        ENDIF
        arrdm3(IIKNMR) = NOGRID

        arrnam(IIKTIM) = 'IKTIM '
        arrknd(IIKTIM) = 1
        if (ifiopk == 0) then
            arrdm1(iiktim) = 0
        elseif (ifiopk == 1) then
            arrdm1(iiktim) = 0
        else
            arrdm1(iiktim) = 3
        endif
        arrdm2(IIKTIM) = 1
        arrdm3(IIKTIM) = 1

        arrnam(IIQDMP) = 'IQDMP '
        arrknd(IIQDMP) = 1
        arrdm1(IIQDMP) = NOQ + NOQ4
        arrdm2(IIQDMP) = 1
        arrdm3(IIQDMP) = 1

        arrnam(IISDMP) = 'ISDMP '
        arrknd(IISDMP) = 1
        arrdm1(IISDMP) = NOSEG + NSEG2
        arrdm2(IISDMP) = 1
        arrdm3(IISDMP) = 1

        arrnam(IIPDMP) = 'IPDMP '
        arrknd(IIPDMP) = 1
        arrdm1(IIPDMP) = 2 * NDMPAR + NTDMPQ + NTDMPS
        arrdm2(IIPDMP) = 1
        arrdm3(IIPDMP) = 1

        arrnam(IIORAA) = 'IORAAI'
        arrknd(IIORAA) = 1
        arrdm1(IIORAA) = NORAAI
        arrdm2(IIORAA) = 1
        arrdm3(IIORAA) = 1

        arrnam(INQRAA) = 'NQRAAI'
        arrknd(INQRAA) = 1
        arrdm1(INQRAA) = NORAAI
        arrdm2(INQRAA) = 1
        arrdm3(INQRAA) = 1

        arrnam(IIQRAA) = 'IQRAAI'
        arrknd(IIQRAA) = 1
        arrdm1(IIQRAA) = NTRAAQ
        arrdm2(IIQRAA) = 1
        arrdm3(IIQRAA) = 1

        arrnam(IINISP) = 'INWISP'
        arrknd(IINISP) = 1
        arrdm1(IINISP) = NEWISP
        arrdm2(IINISP) = 1
        arrdm3(IINISP) = 1

        arrnam(IINTYP) = 'INTYPE'
        arrknd(IINTYP) = 1
        arrdm1(IINTYP) = NOBND + NOWST
        arrdm2(IINTYP) = 1
        arrdm3(IINTYP) = 1

        arrnam(IIWORK) = 'IWORK '
        arrknd(IIWORK) = 1
        arrdm1(IIWORK) = MAX(NOBND * NOSYS, NOWST * (NOTOT + 2))
        arrdm2(IIWORK) = 1
        arrdm3(IIWORK) = 1

        IF (F_SOLV) THEN
            arrnam(IJTRAC) = 'ITRACE'
            arrknd(IJTRAC) = 1
            arrdm1(IJTRAC) = NOSEG + NSEG2 + NOBND
            arrdm2(IJTRAC) = 1
            arrdm3(IJTRAC) = 1

            arrnam(IIMAT) = 'IMATRX'
            arrknd(IIMAT) = 1
            arrdm1(IIMAT) = NOMAT
            arrdm2(IIMAT) = 1
            arrdm3(IIMAT) = 1

            arrnam(IIWRK) = 'IWRK  '
            arrknd(IIWRK) = 1
            arrdm1(IIWRK) = NOSEG + NSEG2 + NOBND
            arrdm2(IIWRK) = 1
            arrdm3(IIWRK) = 1
        ENDIF

        arrnam(IISYSN) = 'ISYSN '
        arrknd(IISYSN) = 1
        arrdm1(IISYSN) = INSIZE
        arrdm2(IISYSN) = 1
        arrdm3(IISYSN) = 1

        arrnam(IISYSI) = 'ISYSI '
        arrknd(IISYSI) = 1
        arrdm1(IISYSI) = IISIZE
        arrdm2(IISYSI) = 1
        arrdm3(IISYSI) = 1

        if (nmax * mmax > 0) then

            arrnam(IILGRA) = 'LGRACT'
            arrknd(IILGRA) = 1
            arrdm1(IILGRA) = MMAX * NMAX
            arrdm2(IILGRA) = 1
            arrdm3(IILGRA) = 1

        ENDIF

        arrnam(IIPGRD) = 'PROGRD'
        arrknd(IIPGRD) = 1
        arrdm1(IIPGRD) = NPROC
        arrdm2(IIPGRD) = 1
        arrdm3(IIPGRD) = 1

        arrnam(IIPNDT) = 'PRONDT'
        arrknd(IIPNDT) = 1
        arrdm1(IIPNDT) = NPROC
        arrdm2(IIPNDT) = 1
        arrdm3(IIPNDT) = 1

        arrnam(IIPVAR) = 'PRVVAR'
        arrknd(IIPVAR) = 1
        arrdm1(IIPVAR) = NIPMSA
        arrdm2(IIPVAR) = 1
        arrdm3(IIPVAR) = 1

        arrnam(IIPTYP) = 'PRVTYP'
        arrknd(IIPTYP) = 1
        arrdm1(IIPTYP) = NIPMSA
        arrdm2(IIPTYP) = 1
        arrdm3(IIPTYP) = 1

        arrnam(IIVARR) = 'VARARR'
        arrknd(IIVARR) = 1
        arrdm1(IIVARR) = NOVAR
        arrdm2(IIVARR) = 1
        arrdm3(IIVARR) = 1

        arrnam(IIVIDX) = 'VARIDX'
        arrknd(IIVIDX) = 1
        arrdm1(IIVIDX) = NOVAR
        arrdm2(IIVIDX) = 1
        arrdm3(IIVIDX) = 1

        arrnam(IIVTDA) = 'VARTDA'
        arrknd(IIVTDA) = 1
        arrdm1(IIVTDA) = NOVAR
        arrdm2(IIVTDA) = 1
        arrdm3(IIVTDA) = 1

        arrnam(IIVDAG) = 'VARDAG'
        arrknd(IIVDAG) = 1
        arrdm1(IIVDAG) = NOVAR
        arrdm2(IIVDAG) = 1
        arrdm3(IIVDAG) = 1

        arrnam(IIVTAG) = 'VARTAG'
        arrknd(IIVTAG) = 1
        arrdm1(IIVTAG) = NOVAR
        arrdm2(IIVTAG) = 1
        arrdm3(IIVTAG) = 1

        arrnam(IIVAGG) = 'VARAGG'
        arrknd(IIVAGG) = 1
        arrdm1(IIVAGG) = NOVAR
        arrdm2(IIVAGG) = 1
        arrdm3(IIVAGG) = 1

        arrnam(IIVSET) = 'VGRSET'
        arrknd(IIVSET) = 2
        arrdm1(IIVSET) = NOVAR
        arrdm2(IIVSET) = NOGRID
        arrdm3(IIVSET) = 1

        arrnam(IIGNOS) = 'GRDNOS'
        arrknd(IIGNOS) = 1
        arrdm1(IIGNOS) = NOGRID
        arrdm2(IIGNOS) = 1
        arrdm3(IIGNOS) = 1

        arrnam(IIGREF) = 'GRDREF'
        arrknd(IIGREF) = 1
        arrdm1(IIGREF) = NOGRID
        arrdm2(IIGREF) = 1
        arrdm3(IIGREF) = 1

        arrnam(IIGSEG) = 'GRDSEG'
        arrknd(IIGSEG) = 2
        arrdm1(IIGSEG) = NOSEG + NSEG2
        arrdm2(IIGSEG) = NOGRID
        arrdm3(IIGSEG) = 1

        arrnam(ipror) = 'PROREF'
        arrknd(ipror) = 1
        arrdm1(ipror) = nrref
        arrdm2(ipror) = nproc
        arrdm3(ipror) = 1

        arrnam(iprvpt) = 'PROPNT'
        arrknd(iprvpt) = 1
        arrdm1(iprvpt) = nproc
        arrdm2(iprvpt) = 1
        arrdm3(iprvpt) = 1

        arrnam(iprdon) = 'PRODON'
        arrknd(iprdon) = 1
        arrdm1(iprdon) = nproc
        arrdm2(iprdon) = 1
        arrdm3(iprdon) = 1

        arrnam(IIDMPB) = 'DMPBAL'
        arrknd(IIDMPB) = 1
        arrdm1(IIDMPB) = NDMPAR
        arrdm2(IIDMPB) = 1
        arrdm3(IIDMPB) = 1

        ! the total array length
        if (.not. declare_memory) then
            write (328, '(/a/a/)') "  ==> INTEGER arrays 4-byte words <==", &
                    "  nr array name            array size"
        endif

        int_arr_size = 0
        do i_jar = iasize + 1, iasize + nr_jar
            arrlen(i_jar) = arrdm1(i_jar) * arrdm2(i_jar) * arrdm3(i_jar)
            if (.not. declare_memory) write (328, 2040) i_jar - iasize, arrnam(i_jar), arrlen(i_jar)
            int_arr_size = int_arr_size + arrlen(i_jar)
            if (int_arr_size < 0) then
                write(logical_unit, 2005)
                call stop_with_error()
            endif
        enddo

        ! Declare memory
        if (declare_memory) then
            do i_jar = iasize + 9, iasize + nr_jar
                iartyp = arrtyp(i_jar)
                iarlen = arrlen(i_jar)
                temp_array_name = arrnam(i_jar)
                if (iarlen > 0) then
                    ip = make_pointer(partition_data, iartyp, iarlen)
                    if (ip <= 0) then
                        write(logical_unit, 2010) temp_array_name
                        call stop_with_error()
                    endif
                else
                    ip = 0
                endif

                ! Add one extra because of the shift between IBUF(0) and integer_array(1)
                ip = ip + 1
                ip_jar(i_jar - iasize) = ip
                arrpoi(i_jar) = ip
            enddo
        endif

        ! Reset new disp and velo pointers if array's are the same
        if (ndspn == 0) then
            idpnw = idpnt
            arrpoi(iidpnw) = arrpoi(iidpnt)
            arrlen(iidpnw) = arrlen(iidpnt)
            arrknd(iidpnw) = arrknd(iidpnt)
            arrdm1(iidpnw) = arrdm1(iidpnt)
            arrdm2(iidpnw) = arrdm2(iidpnt)
            arrdm3(iidpnw) = arrdm3(iidpnt)
        endif
        if (nveln == 0) then
            ivpnw = ivpnt
            arrpoi(iivpnw) = arrpoi(iivpnt)
            arrlen(iivpnw) = arrlen(iivpnt)
            arrknd(iivpnw) = arrknd(iivpnt)
            arrdm1(iivpnw) = arrdm1(iivpnt)
            arrdm2(iivpnw) = arrdm2(iivpnt)
            arrdm3(iivpnw) = arrdm3(iivpnt)
        endif

        ! New array declarations
        ierr = 0

        nr_jar_new = nr_jar
        int_arr_size = int_arr_size + (noseg + nseg2) * nogrid
        nr_jar_new = nr_jar_new + 1
        if (declare_memory) allocate (iknmkv(noseg + nseg2, nogrid), stat = ierr)

        if (ierr /= 0) then
            write(logical_unit, 2010) "iknmkv              "
            call stop_with_error() ;
        endif
        if (.not. declare_memory) write (328, 2040) nr_jar_new, "iknmkv              ", (noseg + nseg2) * nogrid

        int_arr_size = int_arr_size + nowst
        nr_jar_new = nr_jar_new + 1                                    ! iwstkind
        if (declare_memory) allocate (iwstkind(nowst), stat = ierr)
        if (ierr /= 0) then
            write(logical_unit, 2010) "iwstkind            "
            call stop_with_error()
        endif
        if (.not. declare_memory) write (328, 2040) nr_jar_new, "iwstkind            ", nowst

        if (nmax * mmax > 0) then
            call allocate_array(logical_unit, nr_jar_new, declare_memory, cellpnt, "cellpnt", noseg, noseg, int_arr_size)
            call allocate_array(logical_unit, nr_jar_new, declare_memory, flowpnt, "flowpnt", noq, noq, int_arr_size)
        endif
        if (f_solv) then
            noth = OMP_GET_MAX_THREADS()

            ! rowpnt
            int_arr_size = int_arr_size + noseg + nobnd + 1
            nr_jar_new = nr_jar_new + 1
            if (declare_memory) allocate (rowpnt (0:noseg + nobnd), stat = ierr)
            if (ierr /= 0) then
                write(logical_unit, 2010) "rowpnt              "
                call stop_with_error()
            endif
            if (.not. declare_memory) write (328, 2040) nr_jar_new, "rowpnt              ", noseg + nobnd + 1

            call allocate_array(logical_unit, nr_jar_new, declare_memory, fmat, "fmat", noq, noq, int_arr_size)
            call allocate_array(logical_unit, nr_jar_new, declare_memory, tmat, "tmat", noq, noq, int_arr_size)

            ! iexseg
            int_arr_size = int_arr_size + (noseg + nobnd) * noth
            nr_jar_new = nr_jar_new + 1
            if (declare_memory) allocate (iexseg (noseg + nobnd, noth), stat = ierr)
            if (ierr /= 0) then
                write(logical_unit, 2010) "iexseg              "
                call stop_with_error()
            endif
            if (.not. declare_memory) write (328, 2040) nr_jar_new, "iexseg              ", (noseg + nobnd) * noth
        endif
        if (intsrt == 24) then
            call allocate_array(logical_unit, nr_jar_new, declare_memory, ibas, "ibas", noseg, noseg, int_arr_size)
            call allocate_array(logical_unit, nr_jar_new, declare_memory, ibaf, "ibaf", noq, noq, int_arr_size)
            call allocate_array(logical_unit, nr_jar_new, declare_memory, iords, "iords", noseg, noseg, int_arr_size)
            call allocate_array(logical_unit, nr_jar_new, declare_memory, iordf, "iordf", noq, noq, int_arr_size)

            ! nvert
            int_arr_size = int_arr_size + 2 * noseg
            nr_jar_new = nr_jar_new + 1
            if (declare_memory) allocate (nvert(2, noseg), stat = ierr)
            if (ierr /= 0) then
                write(logical_unit, 2010) "nvert               "
                call stop_with_error()
            endif
            if (.not. declare_memory) write (328, 2040) nr_jar_new, "nvert               ", 2 * noseg

            call allocate_array(logical_unit, nr_jar_new, declare_memory, ivert, "ivert", noseg, noseg, int_arr_size)
        endif

        call allocate_array(logical_unit, nr_jar_new, declare_memory, isegcol, "isegcol", &
                noseg + nseg2, noseg + nseg2, int_arr_size)

        if (.not. declare_memory) write (328, '(/5x,a20,i12)') "Total (4 byte words)", int_arr_size

        2005 format (' ERROR  : integer array is too big. Unable to create pointer. ')
        2010 format (' ERROR  : allocating integer array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)

    end subroutine allocate_integer_arrays

    subroutine allocate_real_arrays(logical_unit, declare_memory, arrpoi, arrtyp, arrbyt, &
            arrlen, arrknd, arrdm1, arrdm2, arrdm3, arrnam, real_arr_size, partition_data)

        !! file Define the real arrays as partition_data of the one overall array Sets the array pointers in the
        !! SYSA common block. Gives array space of the kind A(pointer) Declares memory through C-interface if asked
        !! (routine is also called by preprocessor)

        ! module with the more recently added arrays
        use variable_declaration
        ! module for computing the pointers into the arrays
        use m_array_manipulation, only : make_pointer, memory_partition, real_type
        ! System characteristics
        use m_sysn, only : nolay, nosys, notot, nothrd, noseg, nseg2, nogrid, noq4, noq, nobnd, nowst, nocons, nopa, &
                nofun, nosfun, ndspn, nodisp, nveln, novelo, nharms, nlines, jtrack, nomat, ndmpar, noloc, nodef, &
                nflux, ndmps, nbufmx, ndspx, nvelx, nlocx, ndmpq, noraai, newrsp, noq3, mmax, nmax, novec
        ! Timer characteristics
        use m_sysi
        ! Pointers in real array workspace
        use m_sysa
        use m_cli_utils, only : get_command_argument_by_name
        use omp_lib

        integer(kind = int_wp), intent(in) :: logical_unit    ! logical unitnumber output file
        logical, intent(in) :: declare_memory    ! Declare memory y/n
        integer(kind = int_wp), intent(inout) :: arrpoi(:) ! Pointer in workarray/FMM reference pointer
        integer(kind = int_wp), intent(inout) :: arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrlen(:) ! Length off array
        integer(kind = int_wp), intent(inout) :: arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
        integer(kind = int_wp), intent(inout) :: arrdm1(:) ! dimension 1
        integer(kind = int_wp), intent(inout) :: arrdm2(:) ! dimension 2
        integer(kind = int_wp), intent(inout) :: arrdm3(:) ! dimension 3 ( number of grids mostly )
        character(len = 20), intent(inout) :: arrnam(:) ! Array name
        integer(kind = int_wp), intent(inout) :: real_arr_size     ! Required array space
        type(memory_partition), intent(inout) :: partition_data ! Private variables for make_pointer

        integer(kind = int_wp) :: i_rar                             ! loop counter
        integer(kind = int_wp) :: nr_rar                            ! number of real ::arrays
        integer(kind = int_wp) :: nohor                             ! number of computational volumes in 1 layer
        integer(kind = int_wp) :: nsubs                             ! nr of substances for array space declaration
        logical :: fluxco                            ! if .true. then flux correction
        logical :: steady                            ! if .true. then steady state computation
        logical :: delmat                            ! if .true. then direct Gauss solver
        logical :: f_solv                            ! if .true. then GMRES Krilov solver
        logical :: balans                            ! if .true. then balances to be computed
        character(len = 20) :: temp_array_name                            ! help variable for array name
        integer(kind = int_wp) :: iartyp                            ! help variable for array type
        integer(kind = int_wp) :: iarlen                            ! help variable for array length
        integer(kind = int_wp) :: ip                                ! help variable for array pointer
        integer(kind = int_wp) :: noth                              ! number of available thread for parallel processing
        integer(kind = int_wp) :: ierr                              ! error indicator
        integer(kind = int_wp) :: jstart                            ! lower limit Flow arrays method 19 and 20
        integer(kind = int_wp) :: nmmaxj                            ! upper limit Flow arrays method 19 and 20

        ! optional number of threads from delwaq2 commandline arguments
        integer(kind = int_wp) :: nothreadsarg, num_to_file

        integer(kind = int_wp) :: ith, j                      ! iteration variables (NUMA initialisation)

        integer :: iivol = 1, iiarea = 2, iiflow = 3, iileng = 4, iidisp = 5, &
                iiconc = 6, iimass = 7, iiderv = 8, iiboun = 9, iibset = 10, &
                iibsav = 11, iiwste = 12, iicons = 13, iiparm = 14, iifunc = 15, &
                iisfun = 16, iidnew = 17, iidiff = 18, iivnew = 19, iivelo = 20, &
                iiharm = 21, iifarr = 22, iimas2 = 23, iitimr = 24, iivol2 = 25, &

                iismas = 32, iiploc = 33, iidefa = 34, iiflux = 35, &
                iistoc = 36, iiflxd = 37, iiflxi = 38, iiriob = 39, iidspx = 40, &
                iivelx = 41, iilocx = 42, iidsto = 43, iivsto = 44, iidmpq = 45, &
                iidmps = 46, iitrra = 47, iinrsp = 48, iivoll = 49, &
                iir1 = 51, iiqxk = 52, iiqyk = 53, iiqzk = 54, iidifx = 55, &
                iidify = 56, iidifz = 57, iivola = 58, iivolb = 59, iiguv = 60, &
                iigvu = 61, iiaak = 63, iibbk = 64, iicck = 65, &
                iibd3x = 66, iibddx = 67, iibdx = 68, iibu3x = 69, iibuux = 70, &
                iibux = 71, iiwrk1 = 72, iiwrk2 = 73, iiaakl = 74, iibbkl = 75, &
                iicckl = 76, iiddkl = 77, iiwdmp = 78

        ! How many threads ?

        ! The '-threads [N]' argument for delwaq2 will turn on parallelism, and override
        ! any setting of the number of threads in the input file.
        ! No value or zero for [N] will use the maximum number of available threads
        nothreadsarg = 0
        if (get_command_argument_by_name('-threads', nothreadsarg)) then
            nothrd = nothreadsarg
        else
            if (get_command_argument_by_name('-nothreads', nothreadsarg)) then
                nothrd = nothreadsarg
            end if
        end if

        if (nothrd > 0) call OMP_SET_NUM_THREADS(nothrd)
        noth = OMP_GET_MAX_THREADS()
        write (logical_unit, 2020) noth
        if (declare_memory) write (6, 2030) noth

        ! Some logicals
        fluxco = intsrt ==  5 .or. intsrt == 12 .or. intsrt == 14 .or. &
                intsrt == 24
        steady = intsrt ==  6 .or. intsrt ==  7 .or. intsrt ==  8 .or. &
                intsrt ==  9 .or. intsrt == 17 .or. intsrt == 18
        delmat = intsrt ==  6 .or. intsrt ==  7
        f_solv = intsrt == 15 .or. intsrt == 16 .or. intsrt == 17 .or. &
                intsrt == 18 .or. intsrt == 21 .or. intsrt == 22
        balans = btest(intopt, 3)

        ! Set defaults, no name no length
        ! total number of arrays
        nr_rar = iasize
        do i_rar = 1, nr_rar
            arrnam(i_rar) = ' '
            arrtyp(i_rar) = real_type
            arrbyt(i_rar) = 4
            arrknd(i_rar) = 0
            arrdm1(i_rar) = 0
            arrdm2(i_rar) = 0
            arrdm3(i_rar) = 0
            arrlen(i_rar) = 0
        enddo

        arrnam(iivol) = 'VOLUME'
        arrknd(iivol) = 2
        arrdm1(iivol) = 1
        arrdm2(iivol) = noseg + nseg2
        arrdm3(iivol) = nogrid

        arrnam(iiarea) = 'AREA  '
        arrknd(iiarea) = 2
        arrdm1(iiarea) = 1
        arrdm2(iiarea) = noq + noq4
        arrdm3(iiarea) = 1

        arrnam(iiflow) = 'FLOW  '
        arrknd(iiflow) = 2
        arrdm1(iiflow) = 1
        arrdm2(iiflow) = noq + noq4
        arrdm3(iiflow) = 1

        arrnam(iileng) = 'LENG  '
        if (ilflag == 0) then
            arrknd(iileng) = 1
            arrdm1(iileng) = 3
            arrdm2(iileng) = 1
        else
            arrknd(iileng) = 2
            arrdm1(iileng) = 2
            arrdm2(iileng) = noq + noq4
        endif
        arrdm3(iileng) = 1

        arrnam(iidisp) = 'DISP  '
        arrknd(iidisp) = 1
        arrdm1(iidisp) = 3
        arrdm2(iidisp) = 1
        arrdm3(iidisp) = 1

        arrnam(iiconc) = 'CONC  '
        arrknd(iiconc) = 2
        if (steady) then
            arrdm1(iiconc) = notot
            arrdm2(iiconc) = noseg + nseg2
            arrdm3(iiconc) = nogrid
            nsubs = notot
        else
            arrdm1(iiconc) = notot
            arrdm2(iiconc) = noseg + nseg2
            arrdm3(iiconc) = nogrid
            nsubs = nosys
        endif

        arrnam(iimass) = 'MASS  '
        arrknd(iimass) = 2
        if (f_solv) then
            arrdm1(iimass) = notot
            arrdm2(iimass) = noseg + nseg2 + nobnd
        else
            arrdm1(iimass) = notot
            arrdm2(iimass) = noseg + nseg2
        endif
        arrdm3(iimass) = nogrid

        arrnam(iiderv) = 'DERIV '
        arrknd(iiderv) = 2
        if (f_solv .and. steady) then
            arrdm1(iiderv) = notot
            arrdm2(iiderv) = noseg + nseg2 + nobnd
        else
            arrdm1(iiderv) = notot
            arrdm2(iiderv) = noseg + nseg2
        endif
        arrdm3(iiderv) = nogrid

        arrnam(iiboun) = 'BOUND '
        arrknd(iiboun) = 2
        arrdm1(iiboun) = nsubs
        arrdm2(iiboun) = nobnd
        arrdm3(iiboun) = 1

        arrnam(iibset) = 'BSET  '
        arrknd(iibset) = 2
        arrdm1(iibset) = nsubs
        arrdm2(iibset) = nobnd
        arrdm3(iibset) = 1

        arrnam(iibsav) = 'BSAVE '
        arrknd(iibsav) = 2
        arrdm1(iibsav) = nsubs
        arrdm2(iibsav) = nobnd
        arrdm3(iibsav) = 1

        arrnam(iiwste) = 'WASTE '
        arrknd(iiwste) = 2
        arrdm1(iiwste) = notot + 2
        arrdm2(iiwste) = nowst
        arrdm3(iiwste) = 1

        arrnam(iicons) = 'CONS  '
        arrknd(iicons) = 1
        arrdm1(iicons) = nocons
        arrdm2(iicons) = 1
        arrdm3(iicons) = 1

        arrnam(iiparm) = 'PARAM '
        arrknd(iiparm) = 2
        arrdm1(iiparm) = nopa
        arrdm2(iiparm) = noseg + nseg2
        arrdm3(iiparm) = nogrid

        arrnam(iifunc) = 'FUNC  '
        arrknd(iifunc) = 1
        arrdm1(iifunc) = nofun
        arrdm2(iifunc) = 1
        arrdm3(iifunc) = 1

        arrnam(iisfun) = 'SFUNC '
        arrknd(iisfun) = 3
        arrdm1(iisfun) = noseg + nseg2
        arrdm2(iisfun) = nosfun
        arrdm3(iisfun) = nogrid

        arrnam(iidnew) = 'DISPNW'
        arrknd(iidnew) = 2
        arrdm1(iidnew) = ndspn
        arrdm2(iidnew) = noq + noq4
        arrdm3(iidnew) = 1

        arrnam(iidiff) = 'DISPER'
        arrknd(iidiff) = 2
        arrdm1(iidiff) = nodisp
        arrdm2(iidiff) = noq + noq4
        arrdm3(iidiff) = 1

        arrnam(iivnew) = 'VELONW'
        arrknd(iivnew) = 2
        arrdm1(iivnew) = nveln
        arrdm2(iivnew) = noq + noq4
        arrdm3(iivnew) = 1

        arrnam(iivelo) = 'VELO  '
        arrknd(iivelo) = 2
        arrdm1(iivelo) = novelo
        arrdm2(iivelo) = noq + noq4
        arrdm3(iivelo) = 1

        arrnam(iiharm) = 'HARMAT'
        arrknd(iiharm) = 1
        arrdm1(iiharm) = nharms
        arrdm2(iiharm) = 1
        arrdm3(iiharm) = 1

        nlines = nlines + 1       ! Try to avoid a problem with the debugger

        arrnam(iifarr) = 'FARR  '
        arrknd(iifarr) = 1
        arrdm1(iifarr) = nlines
        arrdm2(iifarr) = 1
        arrdm3(iifarr) = 1

        arrnam(iimas2) = 'MASS2 '
        arrknd(iimas2) = 2
        arrdm1(iimas2) = notot
        arrdm2(iimas2) = 5
        arrdm3(iimas2) = 1

        arrnam(iitimr) = 'TIMER '
        if (fluxco) then
            arrknd(iitimr) = 2
            arrdm1(iitimr) = notot
            arrdm2(iitimr) = noseg + nseg2
            arrdm3(iitimr) = 1
        elseif (delmat) then
            arrknd(iitimr) = 3
            arrdm1(iitimr) = noseg + nseg2
            arrdm2(iitimr) = jtrack * 2 + 1
            arrdm3(iitimr) = 1
        elseif (f_solv) then
            arrknd(iitimr) = 1
            arrdm1(iitimr) = nomat
            arrdm2(iitimr) = 1
            arrdm3(iitimr) = 1
        endif

        arrnam(iivol2) = 'VOL2  '
        if (f_solv) then
            arrknd(iivol2) = 3
            arrdm1(iivol2) = noseg + nseg2 + nobnd
            arrdm2(iivol2) = 1
            arrdm3(iivol2) = 1
        else
            arrknd(iivol2) = 3
            arrdm1(iivol2) = noseg + nseg2
            arrdm2(iivol2) = 1
            arrdm3(iivol2) = 1
        endif

        arrnam(iismas) = 'ASMASS'
        if (balans) then
            arrknd(iismas) = 4
            arrdm1(iismas) = notot
            arrdm2(iismas) = ndmpar
            arrdm3(iismas) = 6
        endif

        arrnam(iiploc) = 'LOCAL '
        arrknd(iiploc) = 2
        arrdm1(iiploc) = noloc
        arrdm2(iiploc) = noseg + nseg2
        arrdm3(iiploc) = nogrid

        arrnam(iidefa) = 'DEFAUL'
        arrknd(iidefa) = 1
        arrdm1(iidefa) = nodef
        arrdm2(iidefa) = 1
        arrdm3(iidefa) = 1

        arrnam(iiflux) = 'FLUX  '
        arrknd(iiflux) = 2
        arrdm1(iiflux) = nflux
        arrdm2(iiflux) = noseg + nseg2
        arrdm3(iiflux) = nogrid

        arrnam(iistoc) = 'STOCHI'
        arrknd(iistoc) = 4
        arrdm1(iistoc) = notot
        arrdm2(iistoc) = nflux
        arrdm3(iistoc) = 1

        arrnam(iiflxd) = 'FLXDMP'
        if (balans) then
            arrknd(iiflxd) = 3
            arrdm1(iiflxd) = ndmps
            arrdm2(iiflxd) = nflux
            arrdm3(iiflxd) = 1
        endif

        arrnam(iiflxi) = 'FLXINT'
        if (balans) then
            arrknd(iiflxi) = 3
            arrdm1(iiflxi) = ndmpar
            arrdm2(iiflxi) = nflux
            arrdm3(iiflxi) = 1
        endif

        arrnam(iiriob) = 'RIOBUF'
        arrknd(iiriob) = 1
        arrdm1(iiriob) = nbufmx
        arrdm2(iiriob) = 1
        arrdm3(iiriob) = 1

        arrnam(iidspx) = 'DISPX '
        arrknd(iidspx) = 2
        arrdm1(iidspx) = ndspx
        arrdm2(iidspx) = noq + noq4
        arrdm3(iidspx) = 1

        arrnam(iivelx) = 'VELX  '
        arrknd(iivelx) = 2
        arrdm1(iivelx) = nvelx
        arrdm2(iivelx) = noq + noq4
        arrdm3(iivelx) = 1

        arrnam(iilocx) = 'VLOCX '
        arrknd(iilocx) = 2
        arrdm1(iilocx) = nlocx
        arrdm2(iilocx) = noq + noq4
        arrdm3(iilocx) = 1

        arrnam(iidsto) = 'DSTO  '
        arrknd(iidsto) = 4
        arrdm1(iidsto) = nosys
        arrdm2(iidsto) = ndspx
        arrdm3(iidsto) = 1

        arrnam(iivsto) = 'VSTO  '
        arrknd(iivsto) = 4
        arrdm1(iivsto) = nosys
        arrdm2(iivsto) = nvelx
        arrdm3(iivsto) = 1

        arrnam(iidmpq) = 'DMPQ  '
        arrknd(iidmpq) = 4
        arrdm1(iidmpq) = nosys
        arrdm2(iidmpq) = ndmpq
        arrdm3(iidmpq) = 2

        arrnam(iidmps) = 'DMPS  '
        arrknd(iidmps) = 4
        arrdm1(iidmps) = notot
        arrdm2(iidmps) = ndmps
        arrdm3(iidmps) = 3

        arrnam(iitrra) = 'TRRAAI'
        arrknd(iitrra) = 3
        arrdm1(iitrra) = nosys
        arrdm2(iitrra) = noraai
        arrdm3(iitrra) = 1

        arrnam(iinrsp) = 'INWRSP'
        arrknd(iinrsp) = 1
        arrdm1(iinrsp) = newrsp
        arrdm2(iinrsp) = 1
        arrdm3(iinrsp) = 1

        arrnam(iivoll) = 'VOLUML'
        arrknd(iivoll) = 3
        arrdm1(iivoll) = noseg + nseg2
        arrdm2(iivoll) = 1
        arrdm3(iivoll) = 1

        arrnam(iiwdmp) = 'WSTDMP'
        arrknd(iiwdmp) = 4
        arrdm1(iiwdmp) = notot
        arrdm2(iiwdmp) = nowst
        arrdm3(iiwdmp) = 2

        ! the total array length
        if (.not. declare_memory) then
            open  (328, file = 'memory_map.out')
            write (328, '(/a/a/)') "  ==> REAL arrays in 4-byte words <==", &
                    "  nr array name            array size"
        endif

        real_arr_size = 0
        do i_rar = 1, nr_rar
            arrlen(i_rar) = arrdm1(i_rar) * arrdm2(i_rar) * arrdm3(i_rar)
            if (arrlen(i_rar) < 0) then
                write(logical_unit, 2000)
                write(logical_unit, 2010) arrnam(i_rar)
                call stop_with_error()
            endif
            if (.not. declare_memory) write (328, 2040) i_rar, arrnam(i_rar), arrlen(i_rar)
            real_arr_size = real_arr_size + arrlen(i_rar)
            if (real_arr_size < 0) then
                write(logical_unit, 2005)
                write(logical_unit, 2010) arrnam(i_rar)
                call stop_with_error()
            endif
        enddo

        ! Declare memory
        if (declare_memory) then
            do i_rar = 1, nr_rar
                iartyp = arrtyp(i_rar)
                iarlen = arrlen(i_rar)
                temp_array_name = arrnam(i_rar)
                if (iarlen > 0) then
                    ip = make_pointer(partition_data, iartyp, iarlen)
                    if (ip <= 0) then
                        write(logical_unit, 2010) temp_array_name
                        call stop_with_error()
                    endif
                else
                    ip = 0
                endif

                ! Add one extra because of the shift between rbuf(0) and a(1)
                ip = ip + 1
                ip_rar(i_rar) = ip
                arrpoi(i_rar) = ip
            enddo
        endif

        ! Reset new disp and velo pointers if array's are the same
        if (ndspn == 0) then
            idnew = idiff
            arrknd(iidnew) = arrknd(iidiff)
            arrdm1(iidnew) = arrdm1(iidiff)
            arrdm2(iidnew) = arrdm2(iidiff)
            arrdm3(iidnew) = arrdm3(iidiff)
            arrlen(iidnew) = arrlen(iidiff)
            arrpoi(iidnew) = arrpoi(iidiff)
        endif
        if (nveln == 0) then
            ivnew = ivelo
            arrknd(iivnew) = arrknd(iivelo)
            arrdm1(iivnew) = arrdm1(iivelo)
            arrdm2(iivnew) = arrdm2(iivelo)
            arrdm3(iivnew) = arrdm3(iivelo)
            arrlen(iivnew) = arrlen(iivelo)
            arrpoi(iivnew) = arrpoi(iivelo)
        endif

        ! New array declarations
        ! (Make sure there is no allocated memory from a possible previous run.
        ! This is a problem if DELWAQ is used as a library)
        if (declare_memory) then
            if (allocated(surface)) deallocate(surface)
            if (allocated(rhs)) deallocate(rhs)
            if (allocated(arhs)) deallocate(arhs)
            if (allocated(adiag)) deallocate(adiag)
            if (allocated(acodia)) deallocate(acodia)
            if (allocated(bcodia)) deallocate(bcodia)
            if (allocated(cell_x)) deallocate(cell_x)
            if (allocated(cell_y)) deallocate(cell_y)
            if (allocated(mixlen)) deallocate(mixlen)
            if (allocated(gm_rhs)) deallocate(gm_rhs)
            if (allocated(gm_sol)) deallocate(gm_sol)
            if (allocated(gm_work)) deallocate(gm_work)
            if (allocated(gm_hess)) deallocate(gm_hess)
            if (allocated(gm_amat)) deallocate(gm_amat)
            if (allocated(gm_diag)) deallocate(gm_diag)
            if (allocated(gm_diac)) deallocate(gm_diac)
            if (allocated(gm_trid)) deallocate(gm_trid)
            if (allocated(flowtot)) deallocate(flowtot)
            if (allocated(disptot)) deallocate(disptot)
            if (allocated(theta)) deallocate(theta)
            if (allocated(thetaseg)) deallocate(thetaseg)
            if (allocated(flux)) deallocate(flux)
            if (allocated(lim)) deallocate(lim)
            if (allocated(maxi)) deallocate(maxi)
            if (allocated(mini)) deallocate(mini)
            if (allocated(l1)) deallocate(l1)
            if (allocated(l2)) deallocate(l2)
            if (allocated(m1)) deallocate(m1)
            if (allocated(m2)) deallocate(m2)
            if (allocated(n1)) deallocate(n1)
            if (allocated(n2)) deallocate(n2)
        endif

        ierr = 0
        call allocate_array(logical_unit, nr_rar, declare_memory, surface, "surface", noseg + nseg2, &
                noseg + nseg2, real_arr_size)
        call allocate_array(logical_unit, nr_rar, declare_memory, wdrawal, "wdrawal", noseg + nseg2, &
                noseg + nseg2, real_arr_size)

        if (delmat) then
            num_to_file = nosys * noseg
            call allocate_array(logical_unit, nr_rar, declare_memory, rhs, "rhs", nosys, noseg, num_to_file, real_arr_size)
        endif

        if (intsrt == 11 .or. intsrt == 12 .or. &
                intsrt == 13 .or. intsrt == 14 .or. &
                intsrt == 24) then

            num_to_file = notot * (noseg + nseg2) * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, arhs, "arhs", notot, noseg + nseg2, &
                    num_to_file, real_arr_size)

            num_to_file = notot * (noseg + nseg2) * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, adiag, "adiag", notot, &
                    noseg + nseg2, num_to_file, real_arr_size)

            num_to_file = notot * max((noq3 + noq4), 1) * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, acodia, "acodia", notot, &
                    max(noq3 + noq4, 1), num_to_file, real_arr_size)

            num_to_file = notot * max((noq3 + noq4), 1) * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, bcodia, "bcodia", notot, &
                    max(noq3 + noq4, 1), num_to_file, real_arr_size)
        endif

        if (nmax * mmax > 0) then
            num_to_file = nmax * mmax
            call allocate_array(logical_unit, nr_rar, declare_memory, cell_x, "cell_x", nmax, mmax, num_to_file, real_arr_size)

            num_to_file = nmax * mmax
            call allocate_array(logical_unit, nr_rar, declare_memory, cell_y, "cell_y", nmax, mmax, num_to_file, real_arr_size)
        endif

        if (f_solv) then
            call allocate_array(logical_unit, nr_rar, declare_memory, mixlen, "mixlen", noq, noq, real_arr_size)

            num_to_file = (noseg + nobnd) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_rhs, "gm_rhs", noseg + nobnd, &
                    noth, num_to_file, real_arr_size)

            num_to_file = (noseg + nobnd) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_sol, "gm_sol", noseg + nobnd, &
                    noth, num_to_file, real_arr_size)

            num_to_file = (noseg + nobnd) * (novec + 5) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_work, "gm_work", &
                    (noseg + nobnd) * (novec + 5), noth, num_to_file, real_arr_size)

            num_to_file = (novec + 1) * (novec + 2) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_hess, "gm_hess", &
                    (novec + 1) * (novec + 2), noth, num_to_file, real_arr_size)

            num_to_file = nomat * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_amat, "gm_amat", nomat, noth, &
                    num_to_file, real_arr_size)

            num_to_file = (noseg + nobnd) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_diag, "gm_diag", noseg + nobnd, &
                    noth, num_to_file, real_arr_size)

            num_to_file = (noseg + nobnd) * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_diac, "gm_diac", noseg + nobnd, &
                    noth, num_to_file, real_arr_size)

            num_to_file = 6 * nolay * noth * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, gm_trid, "gm_trid", 6 * nolay, noth, &
                    num_to_file, real_arr_size)

            num_to_file = noq * noth
            call allocate_array(logical_unit, nr_rar, declare_memory, flowtot, "flowtot", noq, noth, num_to_file, real_arr_size)

            num_to_file = noq * noth
            call allocate_array(logical_unit, nr_rar, declare_memory, disptot, "disptot", noq, noth, num_to_file, real_arr_size)

            ! Note: trick for making sure that memory near the processor is assigned
            !       to the array. This has to do with the NUMA characteristics.
            if (declare_memory) then
                !$omp parallel
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_rhs, 1)
                        gm_rhs(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do

                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_sol, 1)
                        gm_sol(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_work, 1)
                        gm_work(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_hess, 1)
                        gm_hess(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_amat, 1)
                        gm_amat(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_diag, 1)
                        gm_diag(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_diac, 1)
                        gm_diac(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(gm_trid, 1)
                        gm_trid(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(flowtot, 1)
                        flowtot(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp do private(j)
                do ith = 1, noth
                    do j = 1, size(disptot, 1)
                        disptot(j, ith) = 0.0
                    enddo
                enddo
                !$omp end do
                !$omp end parallel
            endif ! declare_memory

            if (intsrt == 21) then
                num_to_file = noq * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, theta, "theta", noq, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, thetaseg, "thetaseg", noseg, &
                        noth, num_to_file, real_arr_size)

                num_to_file = noq * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, flux, "flux", noq, noth, num_to_file, real_arr_size)

                num_to_file = noq * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, lim, "lim", noq, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, maxi, "maxi", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, mini, "mini", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, l1, "l1", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, l2, "l2", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, m1, "m1", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, m2, "m2", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, n1, "n1", noseg, noth, num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, n2, "n2", noseg, noth, num_to_file, real_arr_size)

                if (declare_memory) then
                    !$omp parallel
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(theta, 1)
                            theta(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(thetaseg, 1)
                            thetaseg(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(flux, 1)
                            flux(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(lim, 1)
                            lim(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(maxi, 1)
                            maxi(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(mini, 1)
                            mini(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(l1, 1)
                            l1(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(l2, 1)
                            l2(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(m1, 1)
                            m1(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(m2, 1)
                            m2(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(n1, 1)
                            n1(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(n2, 1)
                            n2(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp end parallel
                endif ! declare_memory

            endif
            if (intsrt == 22) then
                num_to_file = noq * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, theta, "theta", noq, noth, &
                        num_to_file, real_arr_size)

                num_to_file = noseg * noth
                call allocate_array(logical_unit, nr_rar, declare_memory, thetaseg, "thetaseg", noseg, &
                        noth, num_to_file, real_arr_size)

                if (declare_memory) then
                    !$omp parallel
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(theta, 1)
                            theta(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp do private(j)
                    do ith = 1, noth
                        do j = 1, size(thetaseg, 1)
                            thetaseg(j, ith) = 0.0
                        enddo
                    enddo
                    !$omp end do
                    !$omp end parallel
                endif ! declare_memory
            endif
        endif
        if (intsrt == 24) then
            num_to_file = 3 * noseg * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, dwork, "dwork", 3, noseg, num_to_file, real_arr_size)

            ! volint
            real_arr_size = real_arr_size + noseg * 2
            nr_rar = nr_rar + 1
            if (declare_memory) allocate (volint  (noseg), stat = ierr)
            if (ierr /= 0) then
                write(logical_unit, 2010) "volint              "
                call stop_with_error()
            endif
            if (.not. declare_memory) write (328, 2040) nr_rar, "volint              ", noseg * 2

            num_to_file = notot * (noseg + nseg2) * 2
            call allocate_array(logical_unit, nr_rar, declare_memory, dconc2, "dconc2", notot, &
                    noseg + nseg2, num_to_file, real_arr_size)

        endif
        if (.not. declare_memory) write (328, '(/5x,a20,i12)') "Total (4 byte words)", real_arr_size

        return

        2000 format (' ERROR  : sub array of real array is too big. Unable to create pointer. ')
        2005 format (' ERROR  : real array is too big. Unable to create pointer. ')
        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2020 format (/' Parallel processing with ', i3, ' processor(s)'/)
        2030 format ('  Parallel processing with ', i3, ' processor(s)')
        2040 format (i4, 1x, a20, i12)

    end subroutine allocate_real_arrays

    subroutine allocate_real_wp_2d_array(logical_unit, nr_rar, declare_memory, arr, arr_name, dim1, dim2, num_to_file, &
            total_num)
        integer(kind = int_wp), intent(in) :: logical_unit, num_to_file
        character(len = *), intent(in) :: arr_name
        logical, intent(in) :: declare_memory
        integer(kind = int_wp), intent(in) :: dim1, dim2
        integer(kind = int_wp), intent(inout) :: nr_rar
        integer(kind = int_wp), intent(inout) :: total_num
        real(kind = real_wp), allocatable, intent(inout) :: arr(:, :)
        integer(kind = int_wp) :: ierr = 0

        total_num = total_num + num_to_file
        nr_rar = nr_rar + 1
        if (declare_memory) allocate (arr(dim1, dim2), stat = ierr)
        if (ierr /= 0) then
            write(logical_unit, 2010) trim(arr_name) // "             "
            call stop_with_error()
        endif
        if (.not. declare_memory) write (328, 2040) nr_rar, trim(arr_name) // "             ", num_to_file

        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)
    end subroutine allocate_real_wp_2d_array

    subroutine allocate_dp_2d_real_array(logical_unit, nr_rar, declare_memory, arr, arr_name, dim1, dim2, num_to_file, &
            total_num)
        integer(kind = int_wp), intent(in) :: logical_unit, num_to_file
        character(len = *), intent(in) :: arr_name
        logical, intent(in) :: declare_memory
        integer(kind = int_wp), intent(in) :: dim1, dim2
        integer(kind = int_wp), intent(inout) :: total_num
        real(kind = dp), allocatable, intent(inout) :: arr(:, :)
        integer(kind = int_wp), intent(inout) :: nr_rar

        integer(kind = int_wp) :: ierr = 0

        total_num = total_num + num_to_file
        nr_rar = nr_rar + 1
        if (declare_memory) allocate (arr(dim1, dim2), stat = ierr)
        if (ierr /= 0) then
            write(logical_unit, 2010) trim(arr_name) // "             "
            call stop_with_error()
        endif
        if (.not. declare_memory) write (328, 2040) nr_rar, trim(arr_name) // "             ", num_to_file

        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)
    end subroutine allocate_dp_2d_real_array

    subroutine allocate_int_wp_1d_array(logical_unit, nr_rar, declare_memory, arr, arr_name, dim1, num_to_file, &
            total_num)

        integer(kind = int_wp), intent(in) :: logical_unit, num_to_file
        character(len = *), intent(in) :: arr_name
        logical, intent(in) :: declare_memory
        integer(kind = int_wp), intent(in) :: dim1
        integer(kind = int_wp), intent(inout) :: total_num
        integer(kind = int_wp), allocatable, intent(inout) :: arr(:)
        integer(kind = int_wp), intent(inout) :: nr_rar

        integer(kind = int_wp) :: ierr = 0

        total_num = total_num + num_to_file
        nr_rar = nr_rar + 1
        if (declare_memory) allocate (arr(dim1), stat = ierr)
        if (ierr /= 0) then
            write(logical_unit, 2010) trim(arr_name) // "            "
            call stop_with_error()
        endif
        if (.not. declare_memory) write (328, 2040) nr_rar, trim(arr_name) // "            ", num_to_file

        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)
    end subroutine allocate_int_wp_1d_array

    subroutine allocate_real_wp_1d_array(logical_unit, nr_rar, declare_memory, arr, arr_name, dim1, num_to_file, &
            total_num)

        integer(kind = int_wp), intent(in) :: logical_unit, num_to_file
        character(len = *), intent(in) :: arr_name
        logical, intent(in) :: declare_memory
        integer(kind = int_wp), intent(in) :: dim1
        integer(kind = int_wp), intent(inout) :: total_num
        real(kind = real_wp), allocatable, intent(inout) :: arr(:)
        integer(kind = int_wp), intent(inout) :: nr_rar

        integer(kind = int_wp) :: ierr = 0

        total_num = total_num + num_to_file
        nr_rar = nr_rar + 1
        if (declare_memory) allocate (arr(dim1), stat = ierr)
        if (ierr /= 0) then
            write(logical_unit, 2010) trim(arr_name) // "            "
            call stop_with_error()
        endif
        if (.not. declare_memory) write (328, 2040) nr_rar, trim(arr_name) // "            ", num_to_file

        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)
    end subroutine allocate_real_wp_1d_array

    subroutine abstract_1(logical_unit, array_name, noarr, var, partition_data, var_type)
        use m_array_manipulation, only : make_pointer, int_type, memory_partition, char_type

        integer(kind = int_wp), intent(in) :: logical_unit, var_type
        integer, intent(inout) :: var
        integer(kind = int_wp), intent(in) :: noarr
        character(len = *) :: array_name   !! name of the arrays in the pmsa
        type(memory_partition), intent(inout) :: partition_data

        if (var_type == int_type) then
            var = make_pointer(partition_data, int_type, noarr)
        else if (var_type == char_type) then
            var = make_pointer(partition_data, char_type, noarr)
        end if

        if (var == 0) then
            write(logical_unit, 2010)
            write(logical_unit, 2020) array_name
            write(logical_unit, 2030) noarr
            call stop_with_error()
        endif
        var = var + 1

        2010 format (' ERROR  : allocating administration array')
        2020 format (' NAME   : ', a)
        2030 format (' LENGTH : ', i12)
    end subroutine abstract_1

end module memory_allocation
