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
module m_temper
    use m_waq_precision

    implicit none

contains


    subroutine temper (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

        !>\file
        !>       Exchange of excess temperature at the surface (Sweers)

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

        REAL(kind = real_wp) :: MTEMP              ! 1  in  Modelled temperature                                [oC]
        REAL(kind = real_wp) :: TMPNAT             ! 2  in  natural temperature of ambient water                [oC]
        REAL(kind = real_wp) :: DEPTH              ! 3  in  actual depth of the water column                     [m]
        REAL(kind = real_wp) :: VWIND              ! 4  in  wind speed at 10 m above surface                   [m/s]
        REAL(kind = real_wp) :: CP                 ! 5  in  specific heat (default 4183.0)                 [J/kg/oC]
        REAL(kind = real_wp) :: DELT               ! 6  in  DELWAQ process time step                             [d]
        INTEGER(kind = int_wp) :: ISWTMP             ! 7  in  DELWAQ process time step                             [d]
        REAL(kind = real_wp) :: FACTRC             ! 8  in  Factor on rate constant                              [d]
        REAL(kind = real_wp) :: ZEROFL             ! 9  in  Zeroth-order flux                                 [oC/d]
        INTEGER(kind = int_wp) :: SWTEMPDF           !10  in  switch temperature increase on tidal flats           (-)
        INTEGER(kind = int_wp) :: SWEMERSION         !11  in  switch indicating submersion(0) or emersion(1)       (-)
        REAL(kind = real_wp) :: LOCSEDDEPT         !12  in  Sediment layer depth to bottom of segment            (m)
        REAL(kind = real_wp) :: THSEDDT            !13  in  thickness sed. layer subjected to temp. change       (m)
        REAL(kind = real_wp) :: RAD                !14  in  actual irradiance over the day                    (W/m2)
        REAL(kind = real_wp) :: RADMAX             !15  in  maximal irradiance                                (W/m2)
        REAL(kind = real_wp) :: RTRADMAX           !16  in  max. rate temp. increase tidal flats              (oC/d)
        REAL(kind = real_wp) :: DELTRADMAX         !17  in  max. temp. increase tidal flats radiation           (oC)
        REAL(kind = real_wp) :: DELTEV             !18  in  temperature decrease evaporation tidal flats        (oC)
        REAL(kind = real_wp) :: DELTRAD            !19  i/o temperature increase previous step                  (oC)
        REAL(kind = real_wp) :: WEXCH              !20  out Rate constant for surplus temperature exchange       [d]
        REAL(kind = real_wp) :: TTEMP              !21  out Total temperature                                   [oC]
        REAL(kind = real_wp) :: ETEMP              !22  out EXCESS! temperature                                 [oC]

        !     fluxes

        REAL(kind = real_wp) :: WFLUX              ! 1      excess temperature flux                           [oC/d]

        !     local decalrations

        REAL(kind = real_wp), PARAMETER :: P1 = 0.00158       ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P2 = 0.018         ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P3 = 1.12          ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P4 = 0.049         ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P5 = 4.48          ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P6 = 2.05          ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: P7 = 3.5           ! coefficient in heat exchange
        REAL(kind = real_wp), PARAMETER :: C1 = 1000.0        ! coefficient in density of water
        REAL(kind = real_wp), PARAMETER :: C2 = 0.088         ! coefficient in density of water
        REAL(kind = real_wp), PARAMETER :: C3 = 86400.        ! conversion, seconds in one day
        REAL(kind = real_wp) :: RHOW               ! density of the water                                    [kg/m3]
        REAL(kind = real_wp) :: HCAPAC             ! Heat capacity of water                                [J/m3/oC]
        REAL(kind = real_wp) :: FWIND              ! wind factor in heat exchange
        REAL(kind = real_wp) :: TREQ               ! equilibrium temperature increse due to solar radiation     [oC]
        REAL(kind = real_wp) :: RTRAD              ! rate of temperature increase due to solar radiation      [oC/d]

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        IP20 = IPOINT(20)
        IP21 = IPOINT(21)
        IP22 = IPOINT(22)
        IP23 = IPOINT(23)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            MTEMP = PMSA(IP1)
            TMPNAT = PMSA(IP2)
            ISWTMP = NINT(PMSA(IP7))

            !        What is the modelled temperature

            IF (ISWTMP == 0) THEN
                TTEMP = MTEMP
                ETEMP = TTEMP - TMPNAT
            ELSE
                ETEMP = MTEMP
                TTEMP = ETEMP + TMPNAT
            ENDIF
            WEXCH = 0.0
            WFLUX = 0.0
            DELTRAD = 0.0

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !           Heat exchange only for top layer segments
                !
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF (IKMRK2==0 .OR. IKMRK2==1) THEN
                    !
                    DEPTH = PMSA(IP3)
                    VWIND = PMSA(IP4)
                    CP = PMSA(IP5)
                    DELT = PMSA(IP6)
                    FACTRC = PMSA(IP8)
                    ZEROFL = PMSA(IP9)

                    RHOW = C1 - C2 * TTEMP
                    HCAPAC = CP * RHOW
                    !
                    !                   wind function source E&Z deviates from Sweers
                    !
                    FWIND = 0.75 * (P7 + P6 * VWIND)
                    !
                    !                   heat exchange coefficient (W/m^2/C) = J/m^2/C/s
                    !
                    WEXCH = P5 + P4 * TTEMP + FWIND * &
                            (P3 + P2 * TTEMP + P1 * TTEMP * TTEMP)
                    !
                    !              heat exchange coefficient = 1/d
                    !
                    WEXCH = WEXCH / DEPTH / HCAPAC * C3 * FACTRC
                    !
                    WFLUX = - WEXCH * ETEMP + ZEROFL
                    !
                    IF (ETEMP > 0.0) THEN
                        !
                        !                 Limitation of FL(1) to amount of excess temperature present
                        !
                        WFLUX = MAX (- ETEMP / DELT, WFLUX)
                    ENDIF
                ENDIF
            ENDIF

            !        Temperature increase due to emersion

            SWTEMPDF = NINT(PMSA(IP10))

            IF (SWTEMPDF == 1) THEN

                SWEMERSION = NINT(PMSA(IP11))

                IF (SWEMERSION == 1) THEN

                    LOCSEDDEPT = PMSA(IP12)
                    THSEDDT = PMSA(IP13)

                    IF (LOCSEDDEPT <= THSEDDT) THEN

                        DELT = PMSA(IP6)
                        RAD = PMSA(IP14)
                        RADMAX = PMSA(IP15)
                        RTRADMAX = PMSA(IP16)
                        DELTRADMAX = PMSA(IP17)
                        DELTEV = PMSA(IP18)
                        DELTRAD = PMSA(IP19)

                        TREQ = DELTRADMAX * RAD / RADMAX
                        RTRAD = RTRADMAX * RAD / RADMAX
                        DELTRAD = MIN(DELT * RTRAD + DELTRAD, TREQ)

                        TTEMP = TMPNAT + DELTRAD - DELTEV

                    ENDIF

                ENDIF

            ENDIF
            !
            !        Output flux, temp, surtemp, heat exchage and temperature increase due to radiation
            !
            FL(1 + IFLUX) = WFLUX
            PMSA (IP20) = WEXCH
            PMSA (IP21) = TTEMP
            PMSA (IP22) = ETEMP
            PMSA (IP23) = DELTRAD
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            IP13 = IP13 + INCREM (13)
            IP14 = IP14 + INCREM (14)
            IP15 = IP15 + INCREM (15)
            IP16 = IP16 + INCREM (16)
            IP17 = IP17 + INCREM (17)
            IP18 = IP18 + INCREM (18)
            IP19 = IP19 + INCREM (19)
            IP20 = IP20 + INCREM (20)
            IP21 = IP21 + INCREM (21)
            IP22 = IP22 + INCREM (22)
            IP23 = IP23 + INCREM (23)
            !
        end do
        !
        RETURN
        !
    END

end module m_temper
