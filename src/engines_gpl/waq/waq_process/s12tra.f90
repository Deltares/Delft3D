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
module m_s12tra
    use m_waq_precision

    implicit none

contains


    subroutine s12tra (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute


        !>\file
        !>       Generic module to process resuspension, burial, digging S1 & S2

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(23), increm(23), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip(23), iflux, iseg, ikmrk2
        real(kind = real_wp) :: fracs1, scals1, fracs2, scals2, fress1, fress2, &
                fburs1, fburs2, fdigs1, fdigs2, swds1, swds2, &
                depth, switch, fracs3, scals3, b1, b2, d1, d2, r1, r2

        ip = ipoint
        !
        iflux = 0
        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==0).or.(ikmrk2==3)) then
                    !
                    fracs1 = process_space_real(ip(1))
                    scals1 = process_space_real(ip(2))
                    fracs2 = process_space_real(ip(3))
                    scals2 = process_space_real(ip(4))
                    fracs3 = process_space_real(ip(5))
                    scals3 = process_space_real(ip(6))
                    fress1 = process_space_real(ip(7))
                    fress2 = process_space_real(ip(8))
                    fburs1 = process_space_real(ip(9))
                    fburs2 = process_space_real(ip(10))
                    fdigs1 = process_space_real(ip(11))
                    fdigs2 = process_space_real(ip(12))
                    swds1 = process_space_real(ip(13))
                    swds2 = process_space_real(ip(14))
                    depth = process_space_real(ip(15))
                    switch = process_space_real(ip(16))

                    !***********************************************************************
                    !**** Processes connected to the BURIAL and DIGGING
                    !***********************************************************************

                    !     RESUSPENSION
                    R1 = 0.0
                    R2 = 0.0
                    IF (FRACS1 * SCALS1 >= 0.0) R1 = FRESS1 * FRACS1 * SCALS1
                    IF (FRACS2 * SCALS2 >= 0.0) R2 = FRESS2 * FRACS2 * SCALS2

                    !     BURIAL
                    B1 = 0.0
                    B2 = 0.0
                    IF (FRACS1 * SCALS1 >= 0.0) B1 = FBURS1 * FRACS1 * SCALS1
                    IF (FRACS2 * SCALS2 >= 0.0) B2 = FBURS2 * FRACS2 * SCALS2

                    !     DIGGING
                    D1 = 0.0
                    D2 = 0.0
                    IF ((SWDS1 < 0.5) .AND. (FRACS1 * SCALS1 >= 0.0)) THEN
                        D1 = FDIGS1 * FRACS1 * SCALS1
                    ELSEIF (FRACS2 * SCALS2 >= 0.0) THEN
                        D1 = FDIGS1 * FRACS2 * SCALS2
                    ENDIF
                    IF ((SWDS2 < 0.5) .AND. (FRACS2 * SCALS2 >= 0.0)) THEN
                        D2 = FDIGS2 * FRACS2 * SCALS2
                    ELSEIF (FRACS3 * SCALS3 >= 0.0) THEN
                        D2 = FDIGS2 * FRACS3 * SCALS3
                    ENDIF

                    !     Store results

                    process_space_real(IP(17)) = R1
                    process_space_real(IP(18)) = R2
                    IF (ABS(SWITCH)<0.5) THEN
                        !       NO SWITCH
                        process_space_real(IP(19)) = B1
                        process_space_real(IP(20)) = 0.0
                    ELSE
                        !       SWITCH
                        process_space_real(IP(19)) = 0.0
                        process_space_real(IP(20)) = B1
                    ENDIF
                    process_space_real(IP(21)) = B2
                    process_space_real(IP(22)) = D1
                    process_space_real(IP(23)) = D2

                    FL(1 + IFLUX) = R1 / DEPTH
                    FL(2 + IFLUX) = R2 / DEPTH
                    IF (ABS(SWITCH)<0.5) THEN
                        !       NO SWITCH
                        FL(3 + IFLUX) = B1 / DEPTH
                        FL(4 + IFLUX) = 0.0
                    ELSE
                        !       SWITCH
                        FL(3 + IFLUX) = 0.0
                        FL(4 + IFLUX) = B1 / DEPTH
                    ENDIF
                    FL(5 + IFLUX) = B2 / DEPTH
                    FL(6 + IFLUX) = D1 / DEPTH
                    FL(7 + IFLUX) = D2 / DEPTH

                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP = IP + INCREM
            !
        end do
        !
        RETURN
        !
    END

end module m_s12tra
