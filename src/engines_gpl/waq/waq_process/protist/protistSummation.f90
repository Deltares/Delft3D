module m_protistsummation
    use m_waq_precision

    implicit none

contains

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

    subroutine phprot (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Composition of phytoplankton by summing algae fractions and NPP - PROTIST

        !
        !     Description of the module :
        !
        !     Sum the contributions of the five groups of algae and protozoa
        !     to each oputput parameter.
        !
        !     Note:
        !     The ordering of the input variables is such that the variables that contribute
        !     to a certain output can be listed individually. It is also possible to apply a
        !     conversion factor for the total output.
        !
        !     Logical Units : -
        !
        !     Modules called : -
        !
        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        integer(kind = int_wp), allocatable :: ipnt(:)          ! Local work array for the pointering
        integer(kind = int_wp) :: ioffset, ioffsetoutput        ! Offsets for input and start of output
        integer(kind = int_wp) :: ipointLength
        integer(kind = int_wp) :: iseg, isum, iinpt
        integer(kind = int_wp) :: nrsums
        integer(kind = int_wp), allocatable :: nrinputs(:)

        real(kind = dp) :: total
        real(kind = dp), allocatable :: conversionfactor(:)

        nrsums = nint(pmsa(ipoint(1)))
        allocate(nrinputs(nrsums), conversionfactor(nrsums))

        ioffset = 1
        do isum = 1, nrsums
            nrinputs(isum) = nint(pmsa(ipoint(ioffset + 1)))
            conversionfactor(isum) = pmsa(ipoint(ioffset + 2))
            ioffset = ioffset + 2 + nrinputs(isum)
        enddo
        ioffsetoutput = ioffset
        ipointLength = ioffsetoutput + nrsums
        allocate (ipnt(ipointLength))
        ipnt(1:ipointLength) = ipoint(1:ipointLength)

        do iseg = 1, noseg
            ioffset = 3
            do isum = 1, nrsums
                total = 0.0d0
                do iinpt = 1, nrinputs(isum)
                    total = total + pmsa(ipnt(ioffset + iinpt))
                end do
                pmsa(ipnt(ioffsetoutput + isum)) = conversionfactor(isum) * total
                ioffset = ioffset + 2 + nrinputs(isum)
            enddo
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)
        enddo

        deallocate(nrinputs, conversionfactor, ipnt)

    end subroutine phprot

end module m_protistsummation
