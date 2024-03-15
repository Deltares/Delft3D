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
module m_wq_processes_pmsa_size
    use m_waq_precision

    implicit none

contains


    subroutine wq_processes_pmsa_size (lunrep, noseg, noq, isizea)

        use m_srstop
        use m_array_manipulation, only : make_pointer, memory_partition, real_type
        use processes_input
        use processes_pointers

        implicit none

        !     Parameters          :

        !     kind     function         name        description

        integer(kind = int_wp), intent(in) :: lunrep     ! logical unitnumber output file
        integer(kind = int_wp), intent(in) :: noseg      ! number of segments
        integer(kind = int_wp), intent(in) :: noq        ! number of exchanges
        integer(kind = int_wp), intent(inout) :: isizea     ! Required array space

        !     Local declarations

        type(memory_partition) :: part      ! Private variables for make_pointer
        integer(kind = int_wp) :: i_rar                              ! loop counter
        integer(kind = int_wp) :: nr_rar                             ! number of real arrays
        character*20    namarr                            ! help variable for array name
        integer(kind = int_wp) :: iartyp                             ! help variable for array type
        integer(kind = int_wp) :: iarlen                             ! help variable for array length
        integer(kind = int_wp) :: ip                                 ! help variable for array pointer
        integer(kind = int_wp) :: ip_rar(78)                         ! help array to fill the common block / SYSA /
        integer(kind = int_wp) :: ierr                               ! error indicator

        nr_rar = 78                   ! total number of arrays
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
        arrdm2(iivol) = noseg
        arrdm3(iivol) = 1

        arrnam(iiarea) = 'AREA  '
        arrknd(iiarea) = 2
        arrdm1(iiarea) = 1
        arrdm2(iiarea) = noq
        arrdm3(iiarea) = 1

        arrnam(iiflow) = 'FLOW  '
        arrknd(iiflow) = 2
        arrdm1(iiflow) = 1
        arrdm2(iiflow) = noq
        arrdm3(iiflow) = 1

        arrnam(iileng) = 'LENG  '
        arrknd(iileng) = 2
        arrdm1(iileng) = 2
        arrdm2(iileng) = noq
        arrdm3(iileng) = 1

        arrnam(iiconc) = 'CONC  '
        arrknd(iiconc) = 2
        arrdm1(iiconc) = notot
        arrdm2(iiconc) = noseg
        arrdm3(iiconc) = 1

        arrnam(iicons) = 'CONS  '
        arrknd(iicons) = 1
        arrdm1(iicons) = nocons
        arrdm2(iicons) = 1
        arrdm3(iicons) = 1

        arrnam(iiparm) = 'PARAM '
        arrknd(iiparm) = 2
        arrdm1(iiparm) = nopa
        arrdm2(iiparm) = noseg
        arrdm3(iiparm) = 1

        arrnam(iifunc) = 'FUNC  '
        arrknd(iifunc) = 1
        arrdm1(iifunc) = nofun
        arrdm2(iifunc) = 1
        arrdm3(iifunc) = 1

        arrnam(iisfun) = 'SFUNC '
        arrknd(iisfun) = 3
        arrdm1(iisfun) = noseg
        arrdm2(iisfun) = nosfun
        arrdm3(iisfun) = 1

        arrnam(iiploc) = 'LOCAL '
        arrknd(iiploc) = 2
        arrdm1(iiploc) = noloc
        arrdm2(iiploc) = noseg
        arrdm3(iiploc) = 1

        arrnam(iidefa) = 'DEFAUL'
        arrknd(iidefa) = 1
        arrdm1(iidefa) = nodef
        arrdm2(iidefa) = 1
        arrdm3(iidefa) = 1

        arrnam(iiflux) = 'FLUX  '
        arrknd(iiflux) = 2
        arrdm1(iiflux) = nflux
        arrdm2(iiflux) = noseg
        arrdm3(iiflux) = 1

        arrnam(iidspx) = 'DISPX '
        arrknd(iidspx) = 2
        arrdm1(iidspx) = ndspx
        arrdm2(iidspx) = noq
        arrdm3(iidspx) = 1

        arrnam(iivelx) = 'VELX  '
        arrknd(iivelx) = 2
        arrdm1(iivelx) = nvelx
        arrdm2(iivelx) = noq
        arrdm3(iivelx) = 1

        arrnam(iilocx) = 'VLOCX '
        arrknd(iilocx) = 2
        arrdm1(iilocx) = nlocx
        arrdm2(iilocx) = noq
        arrdm3(iilocx) = 1

        isizea = 1 ! a(1) is 'dump' location
        write (lunrep, '(/a/)') "  Size of PMSA arrays in 4-byte words"
        write (lunrep, '(a)') "  nr array name            array size"
        write (lunrep, '(a)') "  -----------------------------------"
        do i_rar = 1, nr_rar
            arrlen(i_rar) = arrdm1(i_rar) * arrdm2(i_rar) * arrdm3(i_rar)
            isizea = isizea + arrlen(i_rar)
            if (arrnam(i_rar)/=' ') then
                write (lunrep, 2040) i_rar, arrnam(i_rar), arrlen(i_rar)
            endif
            if (isizea < 0) then
                write(lunrep, 2005)
                call srstop(1)
            endif
        enddo

        !     Declare memory

        do i_rar = 1, nr_rar
            iartyp = arrtyp(i_rar)
            iarlen = arrlen(i_rar)
            namarr = arrnam(i_rar)
            if (iarlen > 0) then
                ip = make_pointer(part, iartyp, iarlen)
                if (ip <= 0) then
                    write(lunrep, 2010) namarr
                    call srstop(1)
                endif
            else
                ip = 0
            endif

            !         Add one extra because of the shift between rbuf(0) and a(1)

            ip = ip + 1
            ip_rar(i_rar) = ip
            arrpoi(i_rar) = ip
        enddo

        write (lunrep, '(a)') "  -----------------------------------"
        write (lunrep, '(5x,a20,i12/)') "Total (4 byte words)", isizea
        write (lunrep, '(5x,"Memory",i3,"-GB ",i3,"-MB ",i3,"-KB ",i3,"-B"//)') &
                isizea / 1000000000, mod(isizea, 1000000000) / 1000000, &
                mod(isizea, 1000000) / 1000, mod(isizea, 1000)

        return

        2005 format (' ERROR  : real array is too big. Unable to create pointer. ')
        2010 format (' ERROR  : allocating real array. Name   : ', A)
        2040 format (i4, 1x, a20, i12)

    end subroutine

end module m_wq_processes_pmsa_size
