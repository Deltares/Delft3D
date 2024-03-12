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
module m_dectra
    use m_waq_precision

    implicit none

contains


    subroutine dectra (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)

        !>\file
        !>       General decayable tracer proces

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                             ----
        ! conctr  R*4 1 I concentration tracer ( 1st order decay )            g/m3
        ! decayr  R*4 1 I decay rate tracer                                    1/d
        ! fdecay  R*4 1 O flux first order decay on tracer                  g/m3/d

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(2), increm(2), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        integer(kind = int_wp) :: ipnt(2), iflux, iseg
        real(kind = real_wp) :: conctr, decayr, fdecay

        ipnt = ipoint
        iflux = 0

        do iseg = 1, noseg
            if (btest(iknmrk(iseg), 0)) then
                conctr = pmsa(ipnt(1))
                decayr = pmsa(ipnt(2))

                !           Calculate decay
                fdecay = decayr * conctr
                fl(1 + iflux) = fdecay
            end if
            iflux = iflux + noflux
            ipnt = ipnt + increm
        end do
        !
        return
    end

end module m_dectra
