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
module m_blinit
    use m_waq_precision

    implicit none

contains


    subroutine blinit

        use bloom_data_dim
        use bloom_data_matrix
        use bloom_data_phyt
        use bloom_data_sumou
        use bloom_data_size

        implicit none

        !     Local variables
        integer(kind = int_wp) :: i, j                ! indexes

        !     Convert BLOOM II specific units to DLWQWQ specific units
        !
        ! Assuming that concentration units in the calling program are g/m3,
        ! where as BLOOM II uses mg/m3, it is necessary to convert
        ! 1.  the specific extinction coefficients
        ! 2.  the carbon to chlorophyll ratio (and hence the dry weight to
        !     chlorophyll ratio)
        ! of all phytoplankton types.
        ! The specific extinction coefficient of detritus.
        ! The base and top levels of the growth and mortality constraints.
        do i = 1, nuspec
            chltoc(i) = chltoc(i) * 1.0d-3
            chlr(i) = chltoc(i) * ctodry(i)
            ekx(i) = ekx(i) * 1000.0d0
        enddo
        biobas = biobas * 1.0d-3
        toplev = toplev * 1.0d-3

        ! Set A-matrix. Copy nutrient rows from AA (stochiometry matrix).
        ! Copy the extinction rows.
        ! Note: in steady state version of BLOOM the A matrix is updated each
        ! call of subroutine SETABC. This is not necessary now; the section
        ! in SETABC is skipped in the the dynamic version of the model.
        do j = 1, nuspec
            do i = 1, nunuco
                a(i, j) = aa(i, j)
            enddo
        enddo
        do j = 1, nuspec
            do i = nufili, nuabco
                a(i, j) = ekx(j)
            enddo
        enddo

        idump = 0

        return
    end

end module m_blinit
