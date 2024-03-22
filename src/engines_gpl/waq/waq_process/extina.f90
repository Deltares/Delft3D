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
module m_extina
    use m_waq_precision

    implicit none

contains


    subroutine extina (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Extinction of light by algae and POC

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: NALG, ISWFIX, NIPALG, IFLUX, ISEG, &
                IALG, IP, IFIX
        REAL(kind = dp) :: EXTALG, EXTCF, BIOMAS, DEPTH, SDMIX
        !
        NALG = NINT(PMSA(IPOINT(1)))
        ISWFIX = NINT(PMSA(IPOINT(2)))
        IF (ISWFIX == 1) THEN
            NIPALG = 4
        ELSE
            NIPALG = 2
        ENDIF
        IFLUX = 0

        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                EXTALG = 0.0
                DEPTH = PMSA (IPOINT(3) + (ISEG - 1) * INCREM(3))
                !
                !     Loop over algae

                DO IALG = 1, NALG

                    IP = 3 + IALG
                    EXTCF = PMSA (IPOINT(IP) + (ISEG - 1) * INCREM(IP))

                    IP = 3 + NALG + IALG
                    BIOMAS = PMSA (IPOINT(IP) + (ISEG - 1) * INCREM(IP))

                    IF (ISWFIX == 1) THEN
                        IP = 3 + 2 * NALG + IALG
                        IFIX = NINT(PMSA (IPOINT(IP) + (ISEG - 1) * INCREM(IP)))
                        IF (IFIX < 0) THEN

                            ! Rooted algae, inlclude only if sdmix positive

                            IP = 3 + 3 * NALG + IALG
                            SDMIX = PMSA (IPOINT(IP) + (ISEG - 1) * INCREM(IP))
                            IF (SDMIX > 1E-10) THEN
                                BIOMAS = BIOMAS / DEPTH
                            ELSE
                                BIOMAS = 0.0
                            ENDIF
                        ENDIF
                    ENDIF

                    IF (BIOMAS > 0.0) &
                            EXTALG = EXTALG + BIOMAS * EXTCF

                end do

                IP = 3 + NIPALG * NALG + 1
                PMSA (IPOINT(IP) + (ISEG - 1) * INCREM(IP)) = EXTALG

            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            !
        end do
        !
        RETURN

    END

end module m_extina
