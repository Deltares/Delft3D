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
module m_dayrad
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DAYRAD (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_evaluate_waq_attribute


        !***********************************************************************
        !
        !     Function : Computes irradiance over the day from daily average irradiance
        !                from "Zonnestraling in Nederland",
        !                C.A.Velds, Thieme/KNMI, 1992, 1st imp., ISBN 90-5210-140-X
        !
        !***********************************************************************

        IMPLICIT NONE

        !     arguments

        REAL(kind = real_wp) :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
        REAL(kind = real_wp) :: FL(*)              ! in/out flux array
        INTEGER(kind = int_wp) :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
        INTEGER(kind = int_wp) :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
        INTEGER(kind = int_wp) :: NOSEG              ! in     number of segments
        INTEGER(kind = int_wp) :: NOFLUX             ! in     total number of fluxes (increment in FL array)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)        ! in     exchange pointer table
        INTEGER(kind = int_wp) :: IKNMRK(*)          ! in     segment features array
        INTEGER(kind = int_wp) :: NOQ1               ! in     number of exchanges in first direction
        INTEGER(kind = int_wp) :: NOQ2               ! in     number of exchanges in second direction
        INTEGER(kind = int_wp) :: NOQ3               ! in     number of exchanges in third direction
        INTEGER(kind = int_wp) :: NOQ4               ! in     number of exchanges in fourth direction

        !     from PMSA array

        REAL(kind = real_wp) :: RADSURF            ! 1  in  irradiation at the water surface            (W/m2)
        REAL(kind = real_wp) :: TIME               ! 2  in  DELWAQ time                                  (scu)
        DOUBLE PRECISION :: LATITUDE           ! 3  in  latitude of study area                   (degrees)
        REAL(kind = real_wp) :: REFDAY             ! 4  in  daynumber of reference day simulation          (d)
        REAL(kind = real_wp) :: AUXSYS             ! 5  in  ratio between days and system clock        (scu/d)
        REAL(kind = real_wp) :: DAYRADSURF         ! 6  out actual irradiance over the day              (W/m2)
        DOUBLE PRECISION :: RADDAY             ! 7  out actual irradiance                           (W/m2)
        DOUBLE PRECISION :: RADTIME            ! 8  out actual irradiance                           (W/m2)

        !     local decalrations

        DOUBLE PRECISION, PARAMETER :: SIN50M = -1.454389765D-2
        DOUBLE PRECISION, PARAMETER :: E = 1.721420632D-2
        DOUBLE PRECISION, PARAMETER :: PI = 3.141592654D0
        DOUBLE PRECISION, PARAMETER :: I0 = 1367.D0
        DOUBLE PRECISION :: DAYNR
        DOUBLE PRECISION :: HOUR
        DOUBLE PRECISION :: RDIST
        DOUBLE PRECISION :: OMEGA
        DOUBLE PRECISION :: DECLIN
        DOUBLE PRECISION :: OMEGA0
        DOUBLE PRECISION :: SIN_DECLIN
        DOUBLE PRECISION :: SIN_LATITU
        DOUBLE PRECISION :: SIN_OMEGA0
        DOUBLE PRECISION :: COS_DECLIN
        DOUBLE PRECISION :: COS_LATITU
        DOUBLE PRECISION :: COS_OMEGA
        LOGICAL :: VARFLG
        INTEGER(kind = int_wp) :: ISEG
        INTEGER(kind = int_wp) :: IKMRK1
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8

        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)

        !
        VARFLG = .TRUE.
        IF (IN2 == 0 .AND. IN3 == 0 .AND. IN4 == 0 .AND. &
                IN5 == 0) THEN
            !
            VARFLG = .FALSE.
            !
            TIME = PMSA(IP2)
            !        Conversion Latitude to rads
            LATITUDE = PMSA(IP3) / 360 * 2 * PI
            REFDAY = PMSA(IP4)
            AUXSYS = PMSA(IP5)

            !        Conversion time to daynumbers relative to refday
            DAYNR = MOD (TIME / AUXSYS + REFDAY, 365.)
            HOUR = MOD (TIME / AUXSYS + REFDAY, 1.) * 24.
            RDIST = 1.D0 + .033 * COS(E * DAYNR)
            OMEGA = ABS(12.D0 - HOUR) * PI / 12.D0

            DECLIN = 6.918D-3 - &
                    3.99912D-1 * DCOS (E * DAYNR) - &
                    6.758D-3 * DCOS (2.0D0 * E * DAYNR) - &
                    2.697D-3 * DCOS (3.0D0 * E * DAYNR) + &
                    7.0257D-2 * DSIN (E * DAYNR) + &
                    9.07D-4 * DSIN (2.0D0 * E * DAYNR) + &
                    1.480D-3 * DSIN (3.0D0 * E * DAYNR)

            !        compute actual irradiance

            OMEGA0 = ACOS(-TAN(DECLIN) * TAN(LATITUDE))
            SIN_DECLIN = SIN(DECLIN)
            SIN_LATITU = SIN(LATITUDE)
            SIN_OMEGA0 = SIN(OMEGA0)
            COS_DECLIN = COS(DECLIN)
            COS_LATITU = COS(LATITUDE)
            COS_OMEGA = COS(OMEGA)
            RADTIME = I0 * RDIST * (SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * COS_OMEGA)
            RADTIME = MAX(0.0D0, RADTIME)
            RADDAY = I0 / PI * RDIST * (OMEGA0 * SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * SIN_OMEGA0)
        ENDIF
        !
        DO ISEG = 1, NOSEG
            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

            RADSURF = PMSA(IP1)

            IF (VARFLG) THEN
                !
                TIME = PMSA(IP2)
                !              Conversion Latitude to rads
                LATITUDE = PMSA(IP3) / 360 * 2 * PI
                REFDAY = PMSA(IP4)
                AUXSYS = PMSA(IP5)

                !              Conversion time to daynumbers relative to refday
                DAYNR = MOD (TIME / AUXSYS + REFDAY, 365.)
                HOUR = MOD (TIME / AUXSYS + REFDAY, 1.) * 24.
                RDIST = 1.D0 + .033 * COS(E * DAYNR)
                OMEGA = ABS(12.D0 - HOUR) * PI / 12.D0
                OMEGA0 = ACOS(-TAN(DECLIN) * TAN(LATITUDE))

                DECLIN = 6.918D-3 - &
                        3.99912D-1 * DCOS (E * DAYNR) - &
                        6.758D-3 * DCOS (2.0D0 * E * DAYNR) - &
                        2.697D-3 * DCOS (3.0D0 * E * DAYNR) + &
                        7.0257D-2 * DSIN (E * DAYNR) + &
                        9.07D-4 * DSIN (2.0D0 * E * DAYNR) + &
                        1.480D-3 * DSIN (3.0D0 * E * DAYNR)

                !              compute actual irradiance

                OMEGA0 = ACOS(-TAN(DECLIN) * TAN(LATITUDE))
                SIN_DECLIN = SIN(DECLIN)
                SIN_LATITU = SIN(LATITUDE)
                SIN_OMEGA0 = SIN(OMEGA0)
                COS_DECLIN = COS(DECLIN)
                COS_LATITU = COS(LATITUDE)
                COS_OMEGA = COS(OMEGA)
                RADTIME = I0 * RDIST * (SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * COS_OMEGA)
                RADTIME = MAX(0.0D0, RADTIME)
                RADDAY = I0 / PI * RDIST * (OMEGA0 * SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * SIN_OMEGA0)
            ENDIF
            !
            DAYRADSURF = RADTIME * RADSURF / RADDAY

            PMSA (IP6) = DAYRADSURF
            PMSA (IP7) = RADTIME
            PMSA (IP8) = RADDAY
            !
            !        ENDIF
            !
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            !
        ENDDO

        RETURN
        !
    END

end module m_dayrad
