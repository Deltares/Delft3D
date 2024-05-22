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

module workspace

    use m_waq_precision
    use m_logger, only : terminate_execution
    USE memory_allocation, only : set_admin_array_indices, allocate_real_arrays, allocate_integer_arrays, &
            set_character_array_indices

    ! System characteristics
    use m_sysn, only : NOTOT, NOCONS, NOPA, NOFUN, NOSFUN, NODISP, NOVELO, NODEF, NOLOC, NDSPX, NVELX, NLOCX, NFLUX
    ! Timer characteristics
    use m_sysi
    ! Pointers in real array workspace
    use m_sysa
    use m_sysj, only : IAPOI, IATYP, IABYT, IALEN, IAKND, IADM1, IADM2, IADM3, IJSIZE
    ! Pointers in character array workspace
    use m_sysc
    ! module for computing the pointers into the arrays
    use m_array_manipulation, only : memory_partition

    private
    public :: set_array_indexes, initialize_variables

CONTAINS

    subroutine set_array_indexes(logical_unit, decclare_memory, real_array, int_array, charachter_array, &
            max_real_arr_size, max_int_arr_size, max_char_arr_size)
        !! sets the array pointers in the sysa, sysi and sysc common blocks.
        !! this is the only place where these common blocks are changed.
        !! warning: the order in the common block must be the same as the order in which the
        !! pointers are set.

        integer(kind = int_wp), intent(in) :: logical_unit  !! logical unitnumber output file ( monitoring output file)
        integer(kind = int_wp), intent(inout) :: max_real_arr_size     !! maximum real array space
        integer(kind = int_wp), intent(inout) :: max_int_arr_size      !! maximum integer array space
        integer(kind = int_wp), intent(inout) :: max_char_arr_size     !! maximum character array space
        logical, intent(in) :: decclare_memory       !! declare memory y/n
        real(kind = real_wp), dimension(:), allocatable, intent(inout) :: real_array   !! real workspace array
        integer(kind = int_wp), dimension(:), allocatable, intent(out) :: int_array  !! integer workspace array
        character(len = *), dimension(:), allocatable, intent(out) :: charachter_array      !! character workspace array

        integer(kind = int_wp), dimension(:), allocatable :: jnew
        character(len = len(charachter_array)), dimension(:), allocatable :: cnew
        character(len = 20), dimension(:), allocatable :: cname

        integer(kind = int_wp) :: k1, k2
        integer(kind = int64) :: itot
        type(memory_partition) :: part

        ! allocate initial space
        noarr = iasize + ijsize + icsize

        if (allocated(int_array)) deallocate(int_array)
        if (allocated(charachter_array)) deallocate(charachter_array)
        if (allocated(cname)) deallocate(cname)

        allocate(int_array(iasize + 1 + ijsize + icsize + 1 + 8 * noarr))
        allocate(charachter_array(20 * (iasize + 1 + ijsize + icsize + 1)))
        allocate(cname (iasize + 1 + ijsize + icsize + 1))

        int_array = 0
        charachter_array = ' '
        cname = ' '

        ! total number of "separate" variables
        novar = 5 + nocons + nopa + nofun + nosfun + notot + notot + &
                notot + nodisp + novelo + nodef + noloc + ndspx + &
                nvelx + nlocx + nflux

        ! sets the array pointers for the array administration array's.
        call set_admin_array_indices(logical_unit, int_array, cname, part)

        ! set the real array workspace
        call allocate_real_arrays(logical_unit, decclare_memory, int_array(iapoi:), int_array(iatyp:), &
                int_array(iabyt:), int_array(ialen:), int_array(iaknd:), int_array(iadm1:), int_array(iadm2:), &
                int_array(iadm3:), cname, itota, part)

        ! set the integer array workspace
        call allocate_integer_arrays(logical_unit, decclare_memory, int_array(iapoi:), int_array(iatyp:), &
                int_array(iabyt:), int_array(ialen:), int_array(iaknd:), int_array(iadm1:), int_array(iadm2:), &
                int_array(iadm3:), cname, itoti, part)

        ! set the character array workspace
        call set_character_array_indices(logical_unit, decclare_memory, int_array(iapoi:), int_array(iatyp:), &
                int_array(iabyt:), int_array(ialen:), int_array(iaknd:), int_array(iadm1:), int_array(iadm2:), &
                int_array(iadm3:), cname, itotc, part)

        ! messages and tests on array space
        itot = int8(itota + itoti + itotc) * 4_2
        write (logical_unit, 2000) itota, itoti, itotc, itot / 4, &
                itot / 1000000000, &
                mod(itot, 1000000000) / 1000000, &
                mod(itot, 1000000) / 1000, &
                mod(itot, 1000)
        ieflag = 0

        if (itota > max_real_arr_size .and. max_real_arr_size /= 0 .and. decclare_memory) then
            write (logical_unit, 2010) itota, max_real_arr_size
            ieflag = 1
        endif
        if (itoti > max_int_arr_size .and. max_int_arr_size /= 0 .and. decclare_memory) then
            write (logical_unit, 2020) itoti, max_int_arr_size
            ieflag = 1
        endif
        if (itotc > max_char_arr_size .and. max_char_arr_size /= 0 .and. decclare_memory) then
            write (logical_unit, 2030) itotc, max_char_arr_size
            ieflag = 1
        endif
        if (ieflag ==    1) then
            write (logical_unit, 2040)

            call terminate_execution(1)
        endif
        max_real_arr_size = itota
        max_int_arr_size = itoti
        max_char_arr_size = itotc

        ! allocate the arrays, first charachter_array then int_array then real_array for least memory requirement
        if (decclare_memory) then
            ! it was allocated at the start
            deallocate(real_array)
            allocate(cnew(part%char_pointer))
            cnew = ' '
            do k2 = 1, size(cname)
                do k1 = 1, 20
                    cnew(1 + k1 + (k2 - 1) * 20) = cname(k2)(k1:k1)
                enddo
            enddo
            deallocate(charachter_array)
            charachter_array = cnew

            allocate(jnew(part%index_pointer))
            jnew = 0
            jnew(1:size(int_array)) = int_array
            deallocate(int_array)
            int_array = jnew

            allocate(real_array(part%alpha_pointer))
            real_array = 0.0
        endif

        return

        2000 FORMAT (' total real      array space: ', I10, / &
                ' total integer   array space: ', I10, / &
                ' total character array space: ', I10, / &
                ' grand total in 4-byte words: ', I10, &
                ' = ', i3, '-GB ', i3, '-MB ', i3'-KB ', i3, '-Byte.')
        2010 FORMAT (' ERROR. Real      array space exceeded !!! ', /, &
                ' total real    array space: ', I10, ', allowed = ', I10)
        2020 FORMAT (' ERROR. Integer   array space exceeded !!! ', /, &
                ' total integer array space: ', I10, ', allowed = ', I10)
        2030 FORMAT (' ERROR. Character array space exceeded !!! ', /, &
                ' total character(len=20)  space: ', I10, ', allowed = ', I10)
        2040 FORMAT (' EXECUTION HALTED, CONSULT YOUR SYSTEM MANAGER !!!')

    end subroutine set_array_indexes

    subroutine initialize_variables(logical_unit, nocons, nopa, nofun, nosfun, &
            nosys, notot, nodisp, novelo, nodef, &
            noloc, ndspx, nvelx, nlocx, nflux, &
            nopred, novar, vararr, varidx, vartda, &
            vardag, vartag, varagg, nogrid, vgrset)
        !! initialisation of variables structur

        use m_array_manipulation, only : initialize_integer_array
        use timers

        integer(kind = int_wp) :: logical_unit, nocons, nopa, nofun, nosfun, &
                nosys, notot, nodisp, novelo, nodef, &
                noloc, ndspx, nvelx, nlocx, nflux, &
                nopred, novar, nogrid
        integer(kind = int_wp) :: vararr(novar), varidx(novar), &
                vartda(novar), vardag(novar), &
                vartag(novar), varagg(novar)
        integer(kind = int_wp) :: vgrset(novar, nogrid)

        ! just take the used array's in the right order
        integer(kind = int_wp) :: iivol = 1
        integer(kind = int_wp) :: iiarea = 2
        integer(kind = int_wp) :: iiflow = 3
        integer(kind = int_wp) :: iileng = 4
        integer(kind = int_wp) :: iidisp = 5
        integer(kind = int_wp) :: iiconc = 6
        integer(kind = int_wp) :: iimass = 7
        integer(kind = int_wp) :: iiderv = 8
        integer(kind = int_wp) :: iiboun = 9
        integer(kind = int_wp) :: iibset = 10
        integer(kind = int_wp) :: iibsav = 11
        integer(kind = int_wp) :: iiwste = 12
        integer(kind = int_wp) :: iicons = 13
        integer(kind = int_wp) :: iiparm = 14
        integer(kind = int_wp) :: iifunc = 15
        integer(kind = int_wp) :: iisfun = 16
        integer(kind = int_wp) :: iidnew = 17
        integer(kind = int_wp) :: iidiff = 18
        integer(kind = int_wp) :: iivnew = 19
        integer(kind = int_wp) :: iivelo = 20
        integer(kind = int_wp) :: iiharm = 21
        integer(kind = int_wp) :: iifarr = 22
        integer(kind = int_wp) :: iimas2 = 23
        integer(kind = int_wp) :: iitimr = 24
        integer(kind = int_wp) :: iivol2 = 25
        integer(kind = int_wp) :: iitrac = 26
        integer(kind = int_wp) :: iigwrk = 27
        integer(kind = int_wp) :: iighes = 28
        integer(kind = int_wp) :: iigsol = 29
        integer(kind = int_wp) :: iigdia = 30
        integer(kind = int_wp) :: iigtri = 31
        integer(kind = int_wp) :: iismas = 32
        integer(kind = int_wp) :: iiploc = 33
        integer(kind = int_wp) :: iidefa = 34
        integer(kind = int_wp) :: iiflux = 35
        integer(kind = int_wp) :: iistoc = 36
        integer(kind = int_wp) :: iiflxd = 37
        integer(kind = int_wp) :: iiflxi = 38
        integer(kind = int_wp) :: iiriob = 39
        integer(kind = int_wp) :: iidspx = 40
        integer(kind = int_wp) :: iivelx = 41
        integer(kind = int_wp) :: iilocx = 42
        integer(kind = int_wp) :: iidsto = 43
        integer(kind = int_wp) :: iivsto = 44
        integer(kind = int_wp) :: iidmpq = 45
        integer(kind = int_wp) :: iidmps = 46
        integer(kind = int_wp) :: iitrra = 47
        integer(kind = int_wp) :: iinrsp = 48
        integer(kind = int_wp) :: iivoll = 49
        integer(kind = int_wp) :: iivol3 = 50
        integer(kind = int_wp) :: iir1 = 51
        integer(kind = int_wp) :: iiqxk = 52
        integer(kind = int_wp) :: iiqyk = 53
        integer(kind = int_wp) :: iiqzk = 54
        integer(kind = int_wp) :: iidifx = 55
        integer(kind = int_wp) :: iidify = 56
        integer(kind = int_wp) :: iidifz = 57
        integer(kind = int_wp) :: iivola = 58
        integer(kind = int_wp) :: iivolb = 59
        integer(kind = int_wp) :: iiguv = 60
        integer(kind = int_wp) :: iigvu = 61
        integer(kind = int_wp) :: iigzz = 62
        integer(kind = int_wp) :: iiaak = 63
        integer(kind = int_wp) :: iibbk = 64
        integer(kind = int_wp) :: iicck = 65
        integer(kind = int_wp) :: iibd3x = 66
        integer(kind = int_wp) :: iibddx = 67
        integer(kind = int_wp) :: iibdx = 68
        integer(kind = int_wp) :: iibu3x = 69
        integer(kind = int_wp) :: iibuux = 70
        integer(kind = int_wp) :: iibux = 71
        integer(kind = int_wp) :: iiwrk1 = 72
        integer(kind = int_wp) :: iiwrk2 = 73
        integer(kind = int_wp) :: iiaakl = 74
        integer(kind = int_wp) :: iibbkl = 75
        integer(kind = int_wp) :: iicckl = 76
        integer(kind = int_wp) :: iiddkl = 77

        integer(kind = int_wp) :: ivvol, ivare, ivflo, ivlen, ivcns, ivpar, ivfun, ivsfu, &
                ivcnc, ivmas, ivder, ivdsp, ivvel, ivdef, ivloc, ivdsx, &
                ivvlx, ivlcx, ivflx

        integer(kind = int_wp) :: ivar, icons, ipa, ifun, isys, isfun, idsp, ivel, iloc, &
                idsx, ivlx, ilcx, idef, iflx

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("initialize_variables", ithandl)

        ivvol = 1
        ivare = ivvol + 1
        ivflo = ivare + 1
        ivlen = ivflo + 1
        ivcns = ivlen + 2
        ivpar = ivcns + nocons
        ivfun = ivpar + nopa
        ivsfu = ivfun + nofun
        ivcnc = ivsfu + nosfun
        ivmas = ivcnc + notot
        ivder = ivmas + notot
        ivdsp = ivder + notot
        ivvel = ivdsp + nodisp
        ivdef = ivvel + novelo
        ivloc = ivdef + nodef
        ivdsx = ivloc + noloc
        ivvlx = ivdsx + ndspx
        ivlcx = ivvlx + nvelx
        ivflx = ivlcx + nlocx

        call initialize_integer_array(vgrset, novar * nogrid)

        ivar = 1
        vgrset(ivar, 1) = 1
        ivar = ivar + 1
        vgrset(ivar, 1) = 1

        ! flow
        ivar = ivar + 1
        vgrset(ivar, 1) = 1

        ! length , two length
        ivar = ivar + 1
        vgrset(ivar, 1) = 1
        ivar = ivar + 1
        vgrset(ivar, 1) = 1

        ! cons
        do icons = 1, nocons
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! param
        do ipa = 1, nopa
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! func
        do ifun = 1, nofun
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! seg func
        do isfun = 1, nosfun
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! conc
        do isys = 1, nosys
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo
        do isys = nosys + 1, notot
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! mass
        do isys = 1, notot
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        do isys = 1, notot
            ivar = ivar + 1
        enddo

        ! disp
        do idsp = 1, nodisp
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! velo
        do ivel = 1, novelo
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        ! default
        do idef = 1, nodef
            ivar = ivar + 1
            vgrset(ivar, 1) = 1
        enddo

        do iloc = 1, noloc
            ivar = ivar + 1
        enddo

        ! dspx
        do idsx = 1, ndspx
            ivar = ivar + 1
        enddo

        ! velx
        do ivlx = 1, nvelx
            ivar = ivar + 1
        enddo

        ! locx
        do ilcx = 1, nlocx
            ivar = ivar + 1
        enddo

        ! flux
        do iflx = 1, nflux
            ivar = ivar + 1
        enddo

        if (timon) call timstop (ithandl)

    end subroutine initialize_variables

end module workspace
