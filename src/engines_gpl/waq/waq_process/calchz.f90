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
module m_calchz
    use m_waq_precision

    implicit none

contains


    subroutine calchz (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_extract_waq_attribute

        !>\file
        !>       Calculate chezy coefficient using roughness and depth

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATE CHEZY COEFFICIENT USING ROUGHNESS AND DEPTH
        !
        !        AVERAGED MODELS
        !
        ! Name    T   L I/O  Description                              Units
        ! ----    --- -  -   -------------------                      ----
        ! CHZ     R   1  L   Chezy coefficient                         [sqrt(m)/s]
        ! DEPTH   R   1  I   Water depth                                       [m]
        ! ROUGH   R   1  I   Bottom roughness                                  [m]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        !     Local declarations, constants in source
        !
        REAL(kind = real_wp) :: ROUGH, DEPTH, TOTDEP, CHZ, ONESIX, &
                MANCOF
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IKMRK2, &
                ICHZTP, IP4, IP5, IP6, ISEG

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)

        ONESIX = 1.0 / 6.0
        ! you need this for maninng

        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                ! 0-inactive cell  1-active cell
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    ! place in layers   0-depth integerated (2D) 1-top 2-between 3-bottom
                    !

                    ROUGH = PMSA(IP1)
                    MANCOF = PMSA(IP2)
                    DEPTH = PMSA(IP3)
                    TOTDEP = PMSA(IP4)
                    ICHZTP = NINT(PMSA(IP5))

                    IF (ICHZTP==1) THEN
                        !       Shear stress by flow according to White/Colebrook - protect against very small depth
                        CHZ = 18. * LOG10 (12. * (TOTDEP + ROUGH) / ROUGH)
                    ELSE IF (ICHZTP==2) THEN
                        !       Chezy according to Manning
                        CHZ = (TOTDEP ** ONESIX) / MANCOF
                    END IF
                    CHZ = MAX(CHZ, 1.0)

                    PMSA (IP6) = CHZ
                    !
                ENDIF
            ENDIF
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            !
        end do

        RETURN
    END

end module m_calchz
