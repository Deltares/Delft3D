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
module m_rear
    use m_waq_precision

    implicit none

contains


    subroutine rear   (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_zerome
        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use m_evaluate_waq_attribute

        !>\file
        !>       Reaeration of carbon dioxide and oxygen

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! DEPTH   R*4 1 I actual depth of the water column                     [m]
        ! FCOVER  R*4 1 I fraction of water surface covered <0-1>              [-]
        ! FL (1)  R*4 1 O reaeration flux                                 [g/m3/d]
        ! HCRT    R*4 1 I critical water depth/velocity                        [m]
        ! IFREAR  I*4 1 I switch for the rearation formula                     [-]
        ! MAXRRC  R*4 1 I maximum wat trf. coef. for temp. lim.              [m/d]
        ! MINRRC  R*4 1 I minimum reaeration rate                            [m/d]
        ! O2      R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! OXSAT   R*4 1 L saturation concentration of dissolved oxygen      [g/m3]
        ! RAIN    R*4 1 L rainfall rate                                     [mm/h]
        ! REARTC  R*4 1 L reaeration temperatuur coefficient                   [-]
        ! REARKL  R*4 1 L reaeration transfer coefficient                    [m/d]
        ! REARRC  R*4 1 L reaeration rate                                    [1/d]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! VELOC   R*4 1 I streamflow velocity                                [m/s]
        ! VWIND   R*4 1 I wind velocity                                      [m/s]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: LUNREP
        INTEGER(kind = int_wp) :: IFREAR, IKMRK1, IKMRK2, ISEG, IFLUX
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24, IP25, IP26, IP27
        REAL(kind = real_wp) :: SAL, B_ENHA, SC, SC20, KLREAR, TOTDEP, &
                REARTC, REARRC, HCRT, VELOC, VWIND, TEMP, &
                OXSAT, O2, TEMP20, TMPCF, DEPTH, FL1, &
                FCOVER, MAXRRC, REARKL, SATPERC, MINRRC, DELT, RAIN
        REAL(kind = real_wp) :: A, B1, B2, C1, &
                C2, D1, D2, D3, D4, &
                D1Os, D2Os, D3Os, D4Os, &
                D1Cs, D2Cs, D3Cs, D4Cs, &
                D1Hs, D2Hs, D3Hs, D4Hs

        !   PBo3: hard coded original coefficients -> user defined input
        !     Parameters for Schmidt number calculation (Warninkhoff and Guerin)
        !     A1-D1=oxygen Wannikhof (seawater)
        !     A2-D2=oxygen Gerin and Wannikhof (both fresh)
        !     A3-D3=CO2 Wannikhof (seawater)
        !     A4-D4=CO2 Gerin and Wannikhof (both fresh)
        !     A5-D5= reserved for CH4 seawater
        !     A6-D6=CH4 freshwater

        !      PARAMETER ( A1 = 1953.4     ,
        !    +            B1 =  128.00    ,
        !    +            C1 =    3.9918  ,
        !    +            D1 =    0.050091,
        !    +            A2 = 1800.6     ,
        !    +            B2 =  120.10    ,
        !    +            C2 =    3.7818  ,
        !    +            D2 =    0.047608,
        !    +            A3 = 2073.1     ,
        !    +            B3 =  125.62    ,
        !    +            C3 =    3.6276  ,
        !    +            D3 =    0.043219,
        !    +            A4 = 1911.1     ,
        !    +            B4 =  118.11    ,
        !    +            C4 =    3.4527  ,
        !    +            D4 =    0.04132 ,
        !    +            A6 = 1897.8     ,
        !    +            B6 =  114 .28   ,
        !    +            C6 =    3.2902  ,
        !    +            D6 =    0.039061 )

        !   PBo3: hard coded coefficients for salt water options Wannikhof
        !     Parameters for Schmidt number calculation (Wanninkhoff and Guerin)
        !     D1-4Os = oxygen Wannikhof (seawater)
        !     D1-4Cs = CO2 Wannikhof (seawater)
        !     D1-4Hs = dummy values for CH4 seawater

        PARAMETER (D1Os = 1953.4, &
                D2Os = 128.00, &
                D3Os = 3.9918, &
                D4Os = 0.050091, &
                D1Cs = 2073.1, &
                D2Cs = 125.62, &
                D3Cs = 3.6276, &
                D4Cs = 0.043219, &
                D1Hs = 2073.1, &
                D2Hs = 125.62, &
                D3Hs = 3.6276, &
                D4Hs = 0.043219)

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
        IP24 = IPOINT(24)
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)

        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN

                !         Compute saturation percentage for all layers

                O2 = PMSA(IP1)
                OXSAT = PMSA(IP10)
                SATPERC = O2 / OXSAT * 100
                PMSA (IP27) = SATPERC

                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==1)) THEN
                    !
                    DEPTH = PMSA(IP2)
                    TEMP = PMSA(IP3)
                    VELOC = PMSA(IP4)
                    VWIND = PMSA(IP5)
                    IFREAR = PMSA(IP6) + 0.5
                    REARKL = PMSA(IP7)
                    REARTC = PMSA(IP8)
                    DELT = PMSA(IP9)
                    SAL = PMSA(IP11)
                    TOTDEP = PMSA(IP12)
                    FCOVER = PMSA(IP13)
                    MAXRRC = PMSA(IP14)
                    MINRRC = PMSA(IP15)
                    RAIN = PMSA(IP16)
                    A = PMSA(IP17)
                    B1 = PMSA(IP18)
                    B2 = PMSA(IP19)
                    C1 = PMSA(IP20)
                    C2 = PMSA(IP21)
                    D1 = PMSA(IP22)
                    D2 = PMSA(IP23)
                    D3 = PMSA(IP24)
                    D4 = PMSA(IP25)

                    IF (DEPTH   < 1E-30) CALL ZEROME ('DEPTH in REAR')

                    !     JvG, 1 May 2002
                    !     Current formulation was not valid for layered schematisations
                    !     Correct by using the methodology as follows:
                    !     a) compute surface transfer coefficient in m/day per method
                    !     b) compute flux by multiplying with (surface) deficit
                    !     c) convert to volumetric flux by using the (surface) layer thickness

                    IF (IFREAR == 0) THEN
                        !
                        !         0. Unscaled user input coefficient in 1/day
                        !
                        REARRC = REARKL * TOTDEP

                    ELSEIF (IFREAR == 1) THEN
                        !
                        !         1. User input coefficient in m/day
                        !
                        REARRC = REARKL

                    ELSEIF (IFREAR == 2) THEN
                        !
                        !         2. Churchill [1962]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 5.026 * (VELOC**0.969) / (TOTDEP**0.673)

                    ELSEIF (IFREAR == 3) THEN
                        !
                        !         3. O'Connor - Dobbins [1958]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 3.863 * (VELOC**0.5) / (TOTDEP**0.5)

                    ELSEIF (IFREAR == 4) THEN
                        !
                        !         4. Scaled version of O'Connor - Dobbins [1958]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 3.863 * (VELOC**0.5) / (TOTDEP**0.5) * REARKL

                    ELSEIF (IFREAR == 5) THEN
                        !
                        !         5. Owens - Edwards - Gibb [1964]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 5.322 * (VELOC**0.67) / (TOTDEP**0.85)

                    ELSEIF (IFREAR == 6) THEN
                        !
                        !         6. Langbien - Durum [1967]
                        !
                        REARRC = 0.0
                        IF (VELOC  > 1E-30) &
                                REARRC = 11.23 * VELOC / (TOTDEP**0.333)

                    ELSEIF (IFREAR == 7) THEN
                        !
                        !         7. Van Pagee[1978] and Delvigne [1980]
                        !
                        IF (VELOC  > 1E-30) THEN
                            REARRC = (REARKL * 0.065 * VWIND**2 + &
                                    3.86 * SQRT(VELOC / TOTDEP))
                        ELSE
                            REARRC = REARKL * 0.065 * VWIND**2
                        ENDIF

                    ELSEIF (IFREAR == 8) THEN
                        !
                        !         8. Thackston - Krenkel [1966]
                        !
                        CALL get_log_unit_number(LUNREP)
                        WRITE (LUNREP, *) &
                                ' Reaeration formula 8 has not been implemented'
                        WRITE (*, *) ' Reaeration formula 8 has not been implemented'
                        CALL stop_with_error()

                    ELSEIF (IFREAR == 9) THEN
                        !
                        !         9. DBS
                        !
                        REARRC = (0.30 + REARKL * 0.028 * VWIND**2)

                    ELSEIF (IFREAR == 10) THEN
                        !
                        !        10. Wanninkhof Oxygen
                        !
                        IF (SAL > 5.0) THEN
                            SC = D1Os - D2Os * TEMP + D3Os * TEMP**2 - D4Os * TEMP**3
                            SC20 = D1Os - D2Os * 20.0 + D3Os * 20.0**2 - D4Os * 20.0**3
                        ELSE
                            SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                            SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3
                        ENDIF
                        KLREAR = 0.31 * VWIND**2 * (SC / SC20)**(-0.5) * 24. / 100.
                        REARRC = KLREAR

                    ELSEIF (IFREAR == 11) THEN
                        !
                        !        10. Wanninkhof CO2
                        !
                        IF (SAL > 5.0) THEN
                            SC = D1Cs - D2Cs * TEMP + D3Cs * TEMP**2 - D4Cs * TEMP**3
                            SC20 = D1Cs - D2Cs * 20.0 + D3Cs * 20.0**2 - D4Cs * 20.0**3
                        ELSE
                            SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                            SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3
                        ENDIF
                        B_ENHA = 2.5 * (.5246 + 1.6256E-2 * TEMP + 4.9946E-4 * TEMP**2)
                        KLREAR = (B_ENHA + 0.31 * VWIND**2) * (SC / SC20)**(-0.5) * 24. / 100.
                        REARRC = KLREAR

                    ELSEIF (IFREAR == 12) THEN

                        !     Note this option is not included in the process documentation!
                        !
                        !         12. Hybride formulation using O'Connor - Dobbins [1958]
                        !             and Owens - Edwards - Gibb [1964]
                        !
                        REARRC = 0.0
                        HCRT = 3.93 / 5.32 * TOTDEP**0.35
                        IF (VELOC  > 1E-30) THEN
                            IF (VELOC < HCRT**6) THEN
                                REARRC = 3.93 * (VELOC**0.5) / (TOTDEP**0.5)
                            ELSE
                                REARRC = 5.32 * (VELOC**0.67) / (TOTDEP**0.85)
                            ENDIF
                        ENDIF
                        REARRC = MAX(MINRRC, REARRC)

                    ELSEIF (IFREAR == 13) THEN
                        !
                        !        13. Guerin O2  - only fresh water
                        !            Guerin CO2 - only fresh water
                        !            Guerin CH4 - only fresh water
                        !
                        SC = D1 - D2 * TEMP + D3 * TEMP**2 - D4 * TEMP**3
                        SC20 = D1 - D2 * 20.0 + D3 * 20.0**2 - D4 * 20.0**3

                        KLREAR = (A * EXP(B1 * VWIND**B2) + C1 * Rain**C2) * (SC / SC20)**(-0.67)
                        REARRC = KLREAR

                        REARTC = 1.0
                        !
                    ELSE
                        CALL get_log_unit_number(LUNREP)
                        WRITE (LUNREP, *) ' Invalid option for reaeration formula'
                        WRITE (*, *) ' Invalid option for reaeration formula'
                        CALL stop_with_error()
                    ENDIF

                    PMSA (IP26) = REARRC / DEPTH

                    !     Calculation of rearation flux ( M.L-1.DAY)
                    !     negatieve zuurstof wordt 0 gemaakt i.v.m. deficiet berekening!
                    !     Wanninkhof, don't use temperature dependency

                    O2 = MAX (O2, 0.0)
                    IF (IFREAR == 10 .OR. IFREAR == 11) THEN
                        FL1 = REARRC * (OXSAT - O2) / DEPTH
                    ELSE
                        TEMP20 = TEMP - 20.0
                        TMPCF = REARTC ** TEMP20
                        IF (REARRC<=MAXRRC) THEN
                            REARRC = REARRC * TMPCF
                        ENDIF
                        FL1 = MIN(1.0 / DELT, REARRC * (1 - FCOVER) / DEPTH) * (OXSAT - O2)
                    ENDIF

                    !     Limitation of FL(1) to amount of oxygen present
                    IF (FL1 < 0.0) THEN
                        FL1 = MAX (-1. * O2 / DELT, FL1)
                    ENDIF
                    FL(1 + IFLUX) = FL1

                ENDIF
            ENDIF
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
            IP24 = IP24 + INCREM (24)
            IP25 = IP25 + INCREM (25)
            IP26 = IP26 + INCREM (26)
            IP27 = IP27 + INCREM (27)
            !
        end do
        !
        RETURN
        !
    END

end module m_rear
