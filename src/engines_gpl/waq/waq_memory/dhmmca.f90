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

module dhmmca_mod
    use m_waq_precision
    use m_srstop

contains
    subroutine dhmmca (lunrep, l_decl, arrpoi, arrtyp, arrbyt, &
            arrlen, arrknd, arrdm1, arrdm2, arrdm3, &
            arrnam, itotc, part)

        !     Deltares Software Centre

        !     Function            : Sets the array pointers in the SYSC common
        !                           block. Gives array space of the kind C(pointer)
        !                           Declares memory through C-interface if asked
        !                           (routine is also called by preprocessor)

        !     Created             : Feb. 1997 by Jan van Beek
        !     Modified            : May  2010 by Leo Postma
        !                           Routine brought in line with real and integer version

        !     Files               : LUNREP - monitoring output file

        !     Routines            : SRSTOP, stops execution (on error)

        use m_array_manipulation, only : make_pointer, memory_partition, char_type ! module for computing the pointers into the arrays
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace

        implicit none

        !     Parameters          :

        !     kind     function         name        description

        integer(kind = int_wp), intent(in) :: lunrep    ! logical unitnumber output file
        logical, intent(in) :: l_decl    ! Declare memory y/n
        integer(kind = int_wp), intent(inout) :: arrpoi(:) ! Pointer in workarray/FMM reference pointer
        integer(kind = int_wp), intent(inout) :: arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
        integer(kind = int_wp), intent(inout) :: arrlen(:) ! Length off array
        integer(kind = int_wp), intent(inout) :: arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
        integer(kind = int_wp), intent(inout) :: arrdm1(:) ! dimension 1
        integer(kind = int_wp), intent(inout) :: arrdm2(:) ! dimension 2
        integer(kind = int_wp), intent(inout) :: arrdm3(:) ! dimension 3 ( number of grids mostly )
        character(20), intent(inout) :: arrnam(:) ! Array name
        integer(kind = int_wp), intent(inout) :: itotc     ! Required array space
        type(memory_partition), intent(inout) :: part ! Private variables for make_pointer


        !     Local declarations

        character*20 :: namarr                      ! help variable for array name

        integer(kind = int_wp) :: iianam, iimnam, iisnam, iidnam, iibnid, &
                iibnam, iibtyp, iiwsid, iiwnam, iiwtyp, &
                iicnam, iipnam, iifnam, iisfna, iiedit, &
                iiprna, iionam, iidina, iivnam, iidana, &
                iirnam, iicbuf, iilunt, iiosnm, iiouni, &
                iiodsc, iissnm, iisuni, iisdsc
        integer(kind = int_wp) :: i_car, iartyp, iarlen, ip
        !
        IIANAM = IASIZE + IJSIZE + 1
        IIMNAM = IASIZE + IJSIZE + 2
        IISNAM = IASIZE + IJSIZE + 3
        IIDNAM = IASIZE + IJSIZE + 4
        IIBNID = IASIZE + IJSIZE + 5
        IIBNAM = IASIZE + IJSIZE + 6
        IIBTYP = IASIZE + IJSIZE + 7
        IIWSID = IASIZE + IJSIZE + 8
        IIWNAM = IASIZE + IJSIZE + 9
        IIWTYP = IASIZE + IJSIZE + 10
        IICNAM = IASIZE + IJSIZE + 11
        IIPNAM = IASIZE + IJSIZE + 12
        IIFNAM = IASIZE + IJSIZE + 13
        IISFNA = IASIZE + IJSIZE + 14
        IIEDIT = IASIZE + IJSIZE + 15
        IIPRNA = IASIZE + IJSIZE + 16
        IIONAM = IASIZE + IJSIZE + 17
        IIDINA = IASIZE + IJSIZE + 18
        IIVNAM = IASIZE + IJSIZE + 19
        IIDANA = IASIZE + IJSIZE + 20
        IIRNAM = IASIZE + IJSIZE + 21
        IICBUF = IASIZE + IJSIZE + 22
        IILUNT = IASIZE + IJSIZE + 23
        IIOSNM = IASIZE + IJSIZE + 24
        IIOUNI = IASIZE + IJSIZE + 25
        IIODSC = IASIZE + IJSIZE + 26
        IISSNM = IASIZE + IJSIZE + 27
        IISUNI = IASIZE + IJSIZE + 28
        IISDSC = IASIZE + IJSIZE + 29
        !
        !     Set defaults, no name no length
        !     Don't declare the first array , ARRNAM
        !
        DO I_CAR = IASIZE + IJSIZE + 1, IASIZE + IJSIZE + NR_CAR
            ARRNAM(I_CAR) = ' '
            ARRTYP(I_CAR) = char_type
            ARRBYT(I_CAR) = 4
            ARRKND(I_CAR) = 1
            ARRDM1(I_CAR) = 0
            ARRDM2(I_CAR) = 1
            ARRDM3(I_CAR) = 1
            ARRLEN(I_CAR) = 0
        ENDDO
        !
        !     Set the characteristics
        !
        ARRNAM(IIMNAM) = 'MNAME '
        ARRDM1(IIMNAM) = 8
        !
        ARRNAM(IISNAM) = 'SNAME '
        ARRDM1(IISNAM) = NOTOT
        !
        ARRNAM(IIDNAM) = 'DNAME '
        ARRDM1(IIDNAM) = NODUMP
        !
        ARRNAM(IIBNID) = 'BNDID '
        ARRDM1(IIBNID) = NOBND
        !
        ARRNAM(IIBNAM) = 'BNAME '
        ARRDM1(IIBNAM) = NOBND * 2
        !
        ARRNAM(IIBTYP) = 'BNTYP '
        ARRDM1(IIBTYP) = NOBTYP
        !
        ARRNAM(IIWSID) = 'WASTID'
        ARRDM1(IIWSID) = NOWST
        !
        ARRNAM(IIWNAM) = 'WNAME '
        ARRDM1(IIWNAM) = NOWST * 2
        !
        ARRNAM(IIWTYP) = 'WTYPE '
        ARRDM1(IIWTYP) = NOWTYP
        !
        ARRNAM(IICNAM) = 'CONAM '
        ARRDM1(IICNAM) = NOCONS
        !
        ARRNAM(IIPNAM) = 'PANAM '
        ARRDM1(IIPNAM) = NOPA
        !
        ARRNAM(IIFNAM) = 'FUNAM '
        ARRDM1(IIFNAM) = NOFUN
        !
        ARRNAM(IISFNA) = 'SFNAM '
        ARRDM1(IISFNA) = NOSFUN
        !
        ARRNAM(IIEDIT) = 'CGRID '
        ARRDM1(IIEDIT) = NY * 6
        !
        ARRNAM(IIPRNA) = 'PRNAM '
        ARRDM1(IIPRNA) = NPROC
        !
        ARRNAM(IIONAM) = 'OUNAM '
        ARRDM1(IIONAM) = NRVART
        !
        ARRNAM(IIDINA) = 'DINAM '
        ARRDM1(IIDINA) = NODISP
        !
        ARRNAM(IIVNAM) = 'VENAM '
        ARRDM1(IIVNAM) = NOVELO
        !
        ARRNAM(IIDANA) = 'DANAM '
        ARRDM1(IIDANA) = NDMPAR
        !
        ARRNAM(IIRNAM) = 'RANAM '
        ARRDM1(IIRNAM) = NORAAI
        !
        ARRNAM(IICBUF) = 'CBUFF '
        ARRDM1(IICBUF) = NCBUFM
        !
        ARRNAM(IILUNT) = 'LUNT  '
        ARRDM1(IILUNT) = NUFIL * 10

        ARRNAM(IIOSNM) = 'OUSNM '
        ARRDM1(IIOSNM) = NRVART * 5

        ARRNAM(IIOUNI) = 'OUUNI '
        ARRDM1(IIOUNI) = NRVART * 2

        ARRNAM(IIODSC) = 'OUDSC '
        ARRDM1(IIODSC) = NRVART * 3

        ARRNAM(IISSNM) = 'OSSNM '
        ARRDM1(IISSNM) = NOTOT * 5

        ARRNAM(IISUNI) = 'OSUNI '
        ARRDM1(IISUNI) = NOTOT * 2

        ARRNAM(IISDSC) = 'OSDSC '
        ARRDM1(IISDSC) = NOTOT * 3

        !     the total array length

        if (.not. l_decl) then
            write (328, '(/a/a/)') "  => CHARACTER arrays 4-byte words <=", &
                    "  nr array name            array size"
        endif

        !     Variables are 'CHARACTER(20), we count characters in 4 byte words, FMM in 1

        itotc = 0
        do i_car = iasize + ijsize + 1, iasize + ijsize + nr_car
            arrlen(i_car) = arrdm1(i_car) * arrdm2(i_car) * arrdm3(i_car) * 5
            if (.not. l_decl) write (328, 2040) i_car - iasize - ijsize, arrnam(i_car), arrlen(i_car)
            itotc = itotc + arrlen(i_car)
            if (itotc < 0) then
                write(lunrep, 2005)
                call srstop(1)
            endif
            arrlen(i_car) = arrlen(i_car) * 4
        enddo
        !     jvb                                     ??lp
        !     raaien behind the dump areas
        !
        ARRLEN(IIDANA) = ARRLEN(IIDANA) + ARRLEN(IIRNAM)
        ARRLEN(IIRNAM) = 0

        !     Allocate array's, set pointers in common block
        !     Don't declare the first array , ARRNAM

        if (l_decl) then
            do i_car = iasize + ijsize + 2, iasize + ijsize + nr_car
                iartyp = arrtyp(i_car)
                iarlen = arrlen(i_car)
                namarr = arrnam(i_car)
                if (iarlen > 0) then
                    ip = make_pointer(part, iartyp, iarlen)
                    if (ip <= 0) then
                        write(lunrep, 2010) namarr
                        call srstop(1)
                    endif
                    !jvb           itotc = itotc + arrlen(i_car)
                else
                    ip = 0
                endif

                !           Add one extra because of the shift between ibuf(0) and j(1)

                ip = ip + 1
                ip_car(i_car - iasize - ijsize) = ip
                arrpoi(i_car) = ip
            enddo
        endif
        if (.not. l_decl) write (328, '(/5x,a20,i12)') "Total (4 byte words)", itotc
        !     jvb                                     ??lp
        !     raaien behind the dump areas
        !
        IP_CAR(IIRNAM - IASIZE - IJSIZE) = IP_CAR(IIDANA - IASIZE - IJSIZE) + &
                NDMPAR * 20
        ARRPOI(IIRNAM) = ARRPOI(IIDANA) + NDMPAR * 20
        ITOTC = ITOTC + NDMPAR
        !     jvb
        !
        close (328)
        return

        2005 format (' ERROR  : character array is too big. Unable to create pointer. ')
        2010 format (' ERROR  : allocating character array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)

    end subroutine
end module dhmmca_mod
