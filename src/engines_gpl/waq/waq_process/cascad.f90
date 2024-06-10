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
module m_cascad
    use m_waq_precision

    implicit none

contains


    subroutine cascad (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_extract_waq_attribute

        !>\file
        !>       Sedimentation routine used for IMx

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Group of up to five substances that are subject to linear transformations:
        !        Substance 1 can be transformed to substances 2, 3, 4 and 5.
        !        Substance 2 can be transformed to substances 3, 4 and 5.
        !        Substance 3 can be transformed to substances 4 and 5.
        !        Substance 4 can be transformed to substance 5.
        !        All five substances can also decay.
        !        This makes it possible to define a fairly general, beit linear,
        !        transformation process.
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! C1      R*4 1 I  Concentration substance 1                       [gX/m3]
        ! C2      R*4 1 I  Concentration substance 2                       [gX/m3]
        ! C3      R*4 1 I  Concentration substance 3                       [gX/m3]
        ! C4      R*4 1 I  Concentration substance 4                       [gX/m3]
        ! C5      R*4 1 I  Concentration substance 5                       [gX/m3]
        ! DECAY1  R*4 1 I  Decay rate of substance 1                         [1/d]
        ! DECAY2  R*4 1 I  Decay rate of substance 2                         [1/d]
        ! DECAY3  R*4 1 I  Decay rate of substance 3                         [1/d]
        ! DECAY4  R*4 1 I  Decay rate of substance 4                         [1/d]
        ! DECAY5  R*4 1 I  Decay rate of substance 5                         [1/d]
        ! TRC1C2  R*4 1 I  Transformation rate of substance 1 into C2        [1/d]
        ! TRC1C3  R*4 1 I  Transformation rate of substance 1 into C3        [1/d]
        ! TRC1C4  R*4 1 I  Transformation rate of substance 1 into C4        [1/d]
        ! TRC1C5  R*4 1 I  Transformation rate of substance 1 into C5        [1/d]
        ! TRC2C3  R*4 1 I  Transformation rate of substance 2 into C3        [1/d]
        ! TRC2C4  R*4 1 I  Transformation rate of substance 2 into C4        [1/d]
        ! TRC2C5  R*4 1 I  Transformation rate of substance 2 into C5        [1/d]
        ! TRC3C4  R*4 1 I  Transformation rate of substance 3 into C4        [1/d]
        ! TRC3C5  R*4 1 I  Transformation rate of substance 3 into C5        [1/d]
        ! TRC4C5  R*4 1 I  Transformation rate of substance 4 into C5        [1/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(20), increm(20), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        integer(kind = int_wp) :: iseg, iflux, ikmrk1

        real(kind = real_wp) :: c1, c2, c3, c4, c5
        real(kind = real_wp) :: decay1, decay2, decay3, decay4, decay5
        real(kind = real_wp) :: trc1c2, trc1c3, trc1c4, trc1c5
        real(kind = real_wp) :: trc2c3, trc2c4, trc2c5
        real(kind = real_wp) :: trc3c4, trc3c5
        real(kind = real_wp) :: trc4c5

        integer(kind = int_wp) :: ipnt(20)

        ipnt = ipoint(1:20)
        iflux = 0

        do iseg = 1, noseg

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                c1 = pmsa(ipnt(1))
                c2 = pmsa(ipnt(2))
                c3 = pmsa(ipnt(3))
                c4 = pmsa(ipnt(4))
                c5 = pmsa(ipnt(5))
                decay1 = pmsa(ipnt(6))
                decay2 = pmsa(ipnt(7))
                decay3 = pmsa(ipnt(8))
                decay4 = pmsa(ipnt(9))
                decay5 = pmsa(ipnt(10))
                trc1c2 = pmsa(ipnt(11))
                trc1c3 = pmsa(ipnt(12))
                trc1c4 = pmsa(ipnt(13))
                trc1c5 = pmsa(ipnt(14))
                trc2c3 = pmsa(ipnt(15))
                trc2c4 = pmsa(ipnt(16))
                trc2c5 = pmsa(ipnt(17))
                trc3c4 = pmsa(ipnt(18))
                trc3c5 = pmsa(ipnt(19))
                trc4c5 = pmsa(ipnt(20))

                !
                ! All processes considered here are linear ...
                !
                fl(iflux + 1) = decay1 * c1
                fl(iflux + 2) = decay2 * c2
                fl(iflux + 3) = decay3 * c3
                fl(iflux + 4) = decay4 * c4
                fl(iflux + 5) = decay5 * c5
                fl(iflux + 6) = trc1c2 * c1
                fl(iflux + 7) = trc1c3 * c1
                fl(iflux + 8) = trc1c4 * c1
                fl(iflux + 9) = trc1c5 * c1
                fl(iflux + 10) = trc2c3 * c2
                fl(iflux + 11) = trc2c4 * c2
                fl(iflux + 12) = trc2c5 * c2
                fl(iflux + 13) = trc3c4 * c3
                fl(iflux + 14) = trc3c5 * c3
                fl(iflux + 15) = trc4c5 * c4
            endif

            iflux = iflux + noflux
            ipnt = ipnt + increm(1:20)
        enddo
    end subroutine

end module m_cascad
