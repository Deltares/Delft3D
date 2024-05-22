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
module m_methox
    use m_waq_precision

    implicit none

contains


    subroutine methox (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger_helper

        !>\file
        !>       Oxidation of methane (New and generic)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Methane oxidation kinetics composed of zeroth order and
        !        MM-kinetics for methane and dissolved oxygen or sulphate.
        !        Process is valid for overlying water as well as sediment.
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CCH4    R*4 1 I concentration of dissolved methane               [gC/m3]
        ! CHFUNC  R*4 1 L function for CH4 effect on the oxidation rate        [-]
        ! COX     R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! COXC    R*4 1 I critical oxygen conc. for methane oxidation       [g/m3]
        ! CRTEMP  R*4 1 I critical temperature for nitrification              [oC]
        ! CSU     R*4 1 I concentration of sulphate                        [gS/m3]
        ! CSUC    R*4 1 I critical sulphate conc. for methane oxidation    [gS/m3]
        ! DELT    R*4 1 I timestep for processes                               [d]
        ! FL (1)  R*4 1 O methane oxidation flux with DO                 [gC/m3/d]
        ! FL (2)  R*4 1 O methane oxidation flux with sulphate           [gC/m3/d]
        ! K0OXI1  R*4 1 I zeroth order methane oxidation rate for DO     [gC/m3/d]
        ! K0OXI2  R*4 1 I zeroth order methane oxid. rate for sulphate   [gC/m3/d]
        ! KOXI1   R*4 1 I MM methane oxidation rate for DO               [gC/m3/d]
        ! KOXI2   R*4 1 I MM methane oxidation rate for sulphate         [gC/m3/d]
        ! KSCH4   R*4 1 I half saturation constant for methane             [gC/m3]
        ! KSOX    R*4 1 I half saturation constant for oxygen               [g/m3]
        ! KSSU    R*4 1 I half saturation constant for sulphate            [gS/m3]
        ! KTOXI1  R*4 1 I temperature coefficient for oxidation with DO        [-]
        ! KTOXI2  R*4 1 I temperature coefficient for oxidation with sulphate  [-]
        ! KSRadFr R*4 1 I half saturation for light inhibition for fraction of
        !                 surface radiance                                     (-)
        ! KSRadSh R*4 1 I shape factor for light inhibition function           (-)
        ! OXFUNC  R*4 1 O function for DO effect on the oxidation rate         [-]
        ! POROS   R*4 1 L porosity                                             [-]
        ! RadInh  R*4 1 I irradiation level with 100% inhibition            (W/m2)
        ! Rad     R*4 1 I irradiation at the segment upper-boundary         (W/m2)
        ! SUFUNC  R*4 1 O function for sulphate effect on the oxidation rate   [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMPC   R*4 1 L ambient temperature correction function              [-]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24, IP25
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19, IN20, &
                IN21, IN22, IN23, IN24, IN25
        INTEGER(kind = int_wp) :: IFLUX, ISEG, ILUMON
        !
        REAL(kind = real_wp) :: CCH4, COX, CSU
        REAL(kind = real_wp) :: K0OXI1, K0OXI2, KOXI1, KOXI2, KSCH4, KSOX, &
                KSSU, COXC, CSUC, CHFUNC, OXFUNC, SUFUNC, &
                LIFUNC
        REAL(kind = real_wp) :: POROS, CRTEMP, KTOXI1, KTOXI2, TEMP, TEMPC, &
                TEMP20, KSRADFR, KSRADSH
        REAL(kind = real_wp) :: DELT, RAD, RADINH
        REAL(kind = real_wp) :: COX_RATIO
        REAL(kind = real_wp) :: CSU_RATIO
        REAL(kind = real_wp) :: FLCOX
        REAL(kind = real_wp) :: FLCSU
        LOGICAL  FIRST
        SAVE     FIRST
        DATA     FIRST /.TRUE./
        !
        CALL get_log_unit_number(ILUMON)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)
        IN11 = INCREM(11)
        IN12 = INCREM(12)
        IN13 = INCREM(13)
        IN14 = INCREM(14)
        IN15 = INCREM(15)
        IN16 = INCREM(16)
        IN17 = INCREM(17)
        IN18 = INCREM(18)
        IN19 = INCREM(19)
        IN20 = INCREM(20)
        IN21 = INCREM(21)
        IN22 = INCREM(22)
        IN23 = INCREM(23)
        IN24 = INCREM(24)
        IN25 = INCREM(25)
        !
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
        !
        !     -----Warnings-----
        !
        IF (FIRST) THEN
            IF (PMSA(IP7)<= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsMet should be greater than zero'
            ELSEIF (PMSA(IP8)<= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsOxMet should be greater than zero'
            ELSEIF (PMSA(IP13) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsSuMet should be greater than zero'
            ELSEIF (PMSA(IP17) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : Poros should be greater', &
                        'than zero'
            ENDIF
            FIRST = .FALSE.
        ENDIF
        !
        IFLUX = 0

        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CCH4 = MAX (0.0, PMSA(IP1))
                COX = MAX (0.0, PMSA(IP2))
                CSU = MAX (0.0, PMSA(IP3))
                K0OXI1 = PMSA(IP4)
                KOXI1 = PMSA(IP5)
                KTOXI1 = PMSA(IP6)
                KSCH4 = PMSA(IP7)
                KSOX = PMSA(IP8)
                COXC = PMSA(IP9)
                K0OXI2 = PMSA(IP10)
                KOXI2 = PMSA(IP11)
                KTOXI2 = PMSA(IP12)
                KSSU = PMSA(IP13)
                CSUC = PMSA(IP14)
                TEMP = PMSA(IP15)
                CRTEMP = PMSA(IP16)
                POROS = PMSA(IP17)
                DELT = PMSA(IP18)
                RADINH = PMSA(IP19)
                RAD = PMSA(IP20)
                KSRADFr = PMSA(IP21)
                KSRADSh = PMSA(IP22)
                !
                !           Set the rates according to CRTEMP, COXC and CSUC
                !
                IF (TEMP < CRTEMP .OR. &
                        COX <= 0.0 .OR. COX <= (COXC * POROS)) THEN
                    KOXI1 = 0.0
                ENDIF
                IF (COX <= 0.0 .OR. COX > (COXC * POROS)) THEN
                    K0OXI1 = 0.0
                ENDIF
                !
                IF (TEMP < CRTEMP .OR. &
                        CSU <= 0.0 .OR. CSU <= (CSUC * POROS)) THEN
                    KOXI2 = 0.0
                ENDIF
                IF (CSU <= 0.0 .OR. CSU > (CSUC * POROS)) THEN
                    K0OXI2 = 0.0
                ENDIF
                !
                IF (COX > (COXC * POROS)) THEN
                    KOXI2 = 0.0
                    K0OXI2 = 0.0
                ENDIF
                !
                !           Calculate both methane oxidation fluxes
                !
                TEMP20 = TEMP - 20.0
                TEMPC = KTOXI1 ** TEMP20
                !
                IF ((KSCH4 * POROS + CCH4) > 0.0) THEN
                    CHFUNC = CCH4 / (KSCH4 * POROS + CCH4)
                ELSE
                    CHFUNC = 0.0
                ENDIF
                !
                IF ((KSOX * POROS + COX) > 0.0) THEN
                    OXFUNC = COX / (KSOX * POROS + COX)
                ELSE
                    OXFUNC = 0.0
                ENDIF
                !

                IF ((KSSU * POROS + CSU) > 0.0) THEN
                    SUFUNC = CSU / (KSSU * POROS + CSU)
                ELSE
                    SUFUNC = 0.0
                ENDIF
                !
                !           Light inhibition function - analogue to salinity dependant mort function
                IF (KSRADFR >= 0.0 .AND. KSRADFR <= 1.0 &
                        .AND. RADINH > 0.1) THEN
                    LIFUNC = -1 / (1 + EXP(KSRADSh * (min(RAD / RADINH, 1.0) - KSRADFR))) + 1
                ELSE
                    LIFUNC = 0.0
                ENDIF
                !

                FLCOX = K0OXI1 + KOXI1 * TEMPC * CHFUNC * OXFUNC * (1 - LIFUNC)
                FLCSU = K0OXI2 + KOXI2 * TEMPC * CHFUNC * SUFUNC * (1 - LIFUNC)
                COX_RATIO = 5.33
                FLCOX = MIN(FLCOX, 0.5 * COX / COX_RATIO / DELT)
                FLCOX = MIN(FLCOX, 0.9 * CCH4 / DELT)
                CSU_RATIO = 2.67
                FLCSU = MIN(FLCSU, 0.9 * CSU / CSU_RATIO / DELT)
                FLCSU = MIN(FLCSU, 0.9 * CCH4 / DELT)
                FL(1 + IFLUX) = FLCOX
                FL(2 + IFLUX) = FLCSU
                !
                !           Oxygen and sulphate functions are output
                !
                PMSA(IP23) = OXFUNC
                PMSA(IP24) = SUFUNC
                PMSA(IP25) = LIFUNC
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
            IP20 = IP20 + IN20
            IP21 = IP21 + IN21
            IP22 = IP22 + IN22
            IP23 = IP23 + IN23
            IP24 = IP24 + IN24
            IP25 = IP25 + IN25
            !
        end do
        !
        RETURN
        !
    END

end module m_methox
