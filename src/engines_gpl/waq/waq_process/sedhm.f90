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
module m_sedhm
    use m_waq_precision

    implicit none

contains


    subroutine sedhm  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

        !>\file
        !>       Sedimentation flux and velocity of adsorbed heavy metals

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! FL1-5   R*4 1 I  flux from a layer                               [gX/m2/d]
        ! Q1-5    R*4 1 I  fraction substance the layer                    [gOMV/gX]
        ! DEPTH   R*4 1 I  depth                                                 [m]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(27), increm(27), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4
        integer(kind = int_wp) :: ipnt(27)

        integer(kind = int_wp) :: iflux, iseg, ikmrk2, iq, ifrom
        real(kind = real_wp) :: fl1, fl2, fl3, fl1s2, fl2s2, fl3s2, fl4, fl5
        real(kind = real_wp) :: q1, q2, q3, q4, q5, depth
        real(kind = real_wp) :: fhmim1, fhmim2, fhmim3, fhmpoc, fhmphy
        real(kind = real_wp) :: vsim1, vsim2, vsim3, vspoc, vsphy

        ipnt = ipoint
        iflux = 0

        do iseg = 1, noseg
            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then

                    fl1 = pmsa(ipnt(1))
                    fl2 = pmsa(ipnt(2))
                    fl3 = pmsa(ipnt(3))
                    fl1s2 = pmsa(ipnt(4))
                    fl2s2 = pmsa(ipnt(5))
                    fl3s2 = pmsa(ipnt(6))
                    fl4 = pmsa(ipnt(7))
                    fl5 = pmsa(ipnt(8))
                    q1 = pmsa(ipnt(9))
                    q2 = pmsa(ipnt(10))
                    q3 = pmsa(ipnt(11))
                    q4 = pmsa(ipnt(12))
                    q5 = pmsa(ipnt(13))
                    depth = pmsa(ipnt(14))

                    !***********************************************************************
                    !**** Processes connected to the BURIAL and DIGGING
                    !***********************************************************************

                    !.....Sedimentation HM to S1/S2
                    pmsa(ipnt(25)) = fl1 * q1 + fl2 * q2 + fl3 * q3 + fl4 * q4 + fl5 * q5
                    fl (1 + iflux) = pmsa(ipnt(25)) / depth

                    pmsa(ipnt(26)) = fl1s2 * q1 + fl2s2 * q2 + fl3s2 * q3
                    fl (2 + iflux) = pmsa(ipnt(26)) / depth

                endif
            endif

            iflux = iflux + noflux
            ipnt = ipnt + increm

        end do

        !.....Reset pointers
        ipnt = ipoint

        !.....Exchangeloop over horizontal direction
        do iq = 1, noq1 + noq2

            !........VxSedHM op nul
            pmsa(ipnt(27)) = 0.0
            ipnt(27) = ipnt(27) + increm(27)

        end do

        !.....Entery point in PMSA for VxSedIM1, 2 en 3, VxSedPOC en VxSedPhyt
        ipnt(20) = ipnt(20) + (noq1 + noq2) * increm(20)
        ipnt(21) = ipnt(21) + (noq1 + noq2) * increm(21)
        ipnt(22) = ipnt(22) + (noq1 + noq2) * increm(22)
        ipnt(23) = ipnt(23) + (noq1 + noq2) * increm(23)

        !.....Exchange loop over the vertical direction
        do iq = noq1 + noq2 + 1, noq1 + noq2 + noq3

            ifrom = iexpnt(1, iq)

            if (ifrom > 0) then
                fhmim1 = pmsa(ipnt(15) + (ifrom - 1) * increm(15))
                fhmim2 = pmsa(ipnt(16) + (ifrom - 1) * increm(16))
                fhmim3 = pmsa(ipnt(17) + (ifrom - 1) * increm(17))
                fhmpoc = pmsa(ipnt(18) + (ifrom - 1) * increm(18))
                fhmphy = pmsa(ipnt(19) + (ifrom - 1) * increm(19))
                vsim1 = pmsa(ipnt(20))
                vsim2 = pmsa(ipnt(21))
                vsim3 = pmsa(ipnt(22))
                vspoc = pmsa(ipnt(23))
                vsphy = pmsa(ipnt(24))

                !...........Calculate VxSedHM
                pmsa(ipnt(27)) = fhmim1 * vsim1 + fhmim2 * vsim2 + fhmim3 * vsim3 + &
                        fhmpoc * vspoc + fhmphy * vsphy
            endif

            !........Exchangepointers increment
            ipnt(20) = ipnt(20) + increm(20)
            ipnt(21) = ipnt(21) + increm(21)
            ipnt(22) = ipnt(22) + increm(22)
            ipnt(23) = ipnt(23) + increm(23)
            ipnt(24) = ipnt(24) + increm(24)
            ipnt(27) = ipnt(27) + increm(27)

        end do

        return
    end

end module m_sedhm
