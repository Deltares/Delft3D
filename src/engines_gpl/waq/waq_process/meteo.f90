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
module m_waqmeteo
    use m_waq_precision

    implicit none

contains


    subroutine meteo  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Process meteo from various meteo-stations

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! Y       R*4 8 I     dependent value pairs
        ! X       R*4 8 I     independent value pairs
        ! VALUE   R*4 1 I     independent value
        ! RESULT  R*4 1 I     resulting dependent value
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        INTEGER(kind = int_wp) :: MAXSTA, MAXVAR, IP, NP, ISEG, i
        !
        !     aantal meteo stations
        !
        PARAMETER (MAXSTA = 10)
        !
        !     aantal variabelen per station
        !
        PARAMETER (MAXVAR = 7)

        !
        !     aantal ongebonden variabelen
        !
        PARAMETER (NP = 5)

        real(kind = real_wp), DIMENSION(MAXSTA) :: RAD, VWIND, DIR, HUM, &
                TEMP, PRES, SUN

        real(kind = real_wp), DIMENSION(MAXSTA) :: X, Y

        DIMENSION IP((MAXSTA + 1) * MAXVAR + MAXSTA * 2 + NP)
        real(kind = real_wp), DIMENSION(MAXSTA) :: DIST, WFAC

        integer(kind = int_wp) :: icalcsw, inear
        real(kind = real_wp) :: scale, nostat, xseg, yseg, sum, sum2, min

        DO I = 1, (MAXSTA + 1) * MAXVAR + MAXSTA * 2 + NP
            IP(I) = IPOINT(I)
        end do
        !
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     waarden per station
                !
                DO I = 1, MAXSTA
                    RAD(I) = process_space_real(IP((I - 1) * MAXVAR + 1))
                    VWIND(I) = process_space_real(IP((I - 1) * MAXVAR + 2))
                    DIR(I) = process_space_real(IP((I - 1) * MAXVAR + 3))
                    HUM(I) = process_space_real(IP((I - 1) * MAXVAR + 4))
                    TEMP(I) = process_space_real(IP((I - 1) * MAXVAR + 5))
                    PRES(I) = process_space_real(IP((I - 1) * MAXVAR + 6))
                    SUN(I) = process_space_real(IP((I - 1) * MAXVAR + 7))
                end do
                !
                !     coordinaten van de stations

                DO I = 1, MAXSTA
                    X(I) = process_space_real(IP(MAXSTA * MAXVAR + (I - 1) * 2 + 1))
                    Y(I) = process_space_real(IP(MAXSTA * MAXVAR + (I - 1) * 2 + 2))
                end do
                !
                !     overige parameters
                !
                SCALE = process_space_real(IP(MAXSTA * (MAXVAR + 2) + 1))
                NOSTAT = process_space_real(IP(MAXSTA * (MAXVAR + 2) + 2))
                ICALCSW = NINT(process_space_real(IP(MAXSTA * (MAXVAR + 2) + 3)))
                XSEG = process_space_real(IP(MAXSTA * (MAXVAR + 2) + 4))
                YSEG = process_space_real(IP(MAXSTA * (MAXVAR + 2) + 5))

                !*******************************************************************************
                !**** RESULT Calculated meteo parameters based on 1-MAXSTA meteo stations.
                !*****       Option 1 looks up the nearest meteo station for each segment
                !*****       Option 2 calculates the distance weighted average of NoStations
                !*****                for each segment
                !***********************************************************************

                !     Bereken Distance Cell to all stations (in meters)
                MIN = -1.0
                SUM = 0.0
                SUM2 = 0.0
                IF (NOSTAT > MAXSTA) THEN
                    NOSTAT = MAXSTA
                ENDIF

                IF (NOSTAT < 1) THEN
                    NOSTAT = 1
                ENDIF

                !
                DO I = 1, NOSTAT
                    DIST(I) = SQRT ((XSEG - X(I) * SCALE) * (XSEG - X(I) * SCALE) + &
                            (YSEG - Y(I) * SCALE) * (YSEG - Y(I) * SCALE))
                    !
                    dist(i) = 1. / max(dist(i), 1.0)
                    SUM = SUM + DIST(I)
                    SUM2 = SUM2 + DIST(I) * DIST(I)

                    !
                    IF (MIN < 0.0) THEN
                        MIN = DIST(I)
                        INEAR = I
                    ELSEIF (DIST(I) < MIN) THEN
                        MIN = DIST(I)
                        INEAR = I
                    ENDIF
                    !
                end do


                !
                !     optie 1:  nearest station
                !
                IF  (ICALCSW == 1) THEN
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 1)) = RAD(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 2)) = VWIND(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 3)) = DIR(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 4)) = HUM(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 5)) = TEMP(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 6)) = PRES(INEAR)
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 7)) = SUN(INEAR)
                    !
                    !     optie 2a: dist weighted lineair
                    !
                ELSE
                    !         optie 2 lineair inv dist
                    IF (ICALCSW == 2) THEN
                        DO I = 1, NOSTAT
                            WFAC(I) = DIST(I) / SUM
                        end do
                        !
                        !         optie 2b: inv dist kwadratisch
                    ELSEIF (ICALCSW == 3) THEN
                        DO I = 1, NOSTAT
                            WFAC(I) = DIST(I) * DIST(I) / SUM2
                        end do

                    ENDIF

                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 1)) = 0.0
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 2)) = 0.0
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 4)) = 0.0
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 5)) = 0.0
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 6)) = 0.0
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 7)) = 0.0
                    DO I = 1, NOSTAT
                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 1)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 1)) + WFAC(I) * RAD(I)

                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 2)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 2)) + WFAC(I) * VWIND(I)

                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 4)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 4)) + WFAC(I) * HUM(I)

                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 5)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 5)) + WFAC(I) * TEMP(I)

                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 6)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 6)) + WFAC(I) * PRES(I)

                        process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 7)) = &
                                process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 7)) + WFAC(I) * SUN(I)

                    end do
                    !
                    !         wind ricthing niet middelen
                    !
                    process_space_real(IP(MAXSTA * (MAXVAR + 2) + NP + 3)) = DIR(INEAR)

                ENDIF

            ENDIF

            !
            DO I = 1, (MAXSTA + 1) * MAXVAR + MAXSTA * 2 + NP
                IP(I) = IP(I) + INCREM (I)
            end do

            !
        end do
        !
        RETURN
        !
    END

end module m_waqmeteo
