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
module m_nitrif
    use m_waq_precision

    implicit none

contains


    subroutine nitrif (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_write_error_message

        !>\file
        !>       Nitrification of ammonium + decay of CBOD

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        NITRIFICATION FORMULA COMPOSED OF A ZERO-ORDER TERM,
        !        AND MICHAELIS-MENTEN TERMS FOR AMMONIUM AND OXYGEN
        !
        !        ----- old version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CFL     R*4 1 I constant in O2FUNC                                   [-]
        ! CONC    R*4 1 I ammonium concentration                            [g/m3]
        ! COX     R*4 1 I critical oxygen concentratio for nitrification    [g/m3]
        ! CRTEMP  R*4 1 I critical temperature for nitrification              [oC]
        ! FL (1)  R*4 1 O nitrification flux                             [gN/m3/d]
        ! RC      R*4 1 I first order nitrification rate                     [1/d]
        ! O2FUNC  R*4 1 I function for OXY effect on the nitrification rate    [-]
        ! OOX     R*4 1 I critical concentr. dissolved oxygen               [g/m3]
        ! OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! POROS   R*4 1 L porosity                                             [-]
        ! SKEWN   R*4 1 I constant in O2FUNC                                   [-]
        ! TC      R*4 1 I temperature coefficient for nitrification            [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
        ! ZERO    R*4 1 I zeroth order nitrification rate                [gN/m3/d]
        !
        !        ----- new version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! AMFUNC  R*4 1 I function for NH4 effect on the nitrification rate    [-]
        ! CRTEMP  R*4 1 I critical temperature for nitrification              [oC]
        ! CROXY   R*4 1 I critical oxygen concentratio for nitrification    [g/m3]
        ! FL (1)  R*4 1 O nitrification flux                             [gN/m3/d]
        ! K0NIT   R*4 1 I zeroth order nitrification rate                [gN/m3/d]
        ! K0TEMP  R*4 1 I zeroth order nitrification rate below CRTEMP   [gN/m3/d]
        ! K0NOX   R*4 1 I zeroth order nitrification rate below CROXY    [gN/m3/d]
        ! KNIT    R*4 1 I MM nitrification rate                          [gN/m3/d]
        ! KSAM    R*4 1 I half saturation constant for ammonium            [gN/m3]
        ! KSOX    R*4 1 I half saturation constant for oxygen               [g/m3]
        ! NH4     R*4 1 I ammonium concentration                            [g/m3]
        ! OXFUNC  R*4 1 I function for OXY effect on the nitrification rate    [-]
        ! OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! POROS   R*4 1 L porosity                                             [-]
        ! TC      R*4 1 I temperature coefficient for nitrification            [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
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
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        REAL(kind = real_wp) :: TC, O2FUNC, COX, OOX, CFL, SKEWN, &
                ZERO, RC, CONC
        INTEGER(kind = int_wp) :: IVERSN
        REAL(kind = real_wp) :: K0NIT, K0OX, K0TEMP, KNIT, KSAM, KSOX, &
                CROXY, NH4, AMFUNC, OXFUNC
        REAL(kind = real_wp) :: POROS, CRTEMP, OXY, TEMP, TEMPC, TEMP20
        REAL(kind = real_wp) :: DELT
        REAL(kind = real_wp) :: FLNIT
        REAL(kind = real_wp) :: NOX_RATIO
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
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                IVERSN = NINT (PMSA(IP13))
                !
                !     Use new version when IVERSN = 1.0
                !
                IF (IVERSN == 1) THEN

                    K0TEMP = PMSA(IP1)
                    NH4 = MAX (0.0, PMSA(IP2))
                    KNIT = PMSA(IP3)
                    TC = PMSA(IP4)
                    OXY = MAX (0.0, PMSA(IP5))
                    KSAM = PMSA(IP6)
                    KSOX = PMSA(IP7)
                    TEMP = PMSA(IP8)
                    CRTEMP = PMSA(IP9)
                    K0OX = PMSA(IP10)
                    CROXY = PMSA(IP11)
                    POROS = PMSA(IP12)
                    DELT = PMSA(IP18)
                    !
                    !           Set the rates according to CRTEMP and CROXY
                    !
                    IF (TEMP < CRTEMP .OR. OXY <= 0.0) KNIT = 0.0
                    !
                    K0NIT = 0.0
                    !
                    IF (TEMP < CRTEMP .AND. OXY > 0.0) THEN
                        K0NIT = K0TEMP
                    ELSEIF (TEMP >= CRTEMP .AND. OXY <= 0.0) THEN
                        K0NIT = K0OX
                    ENDIF
                    !
                    IF (OXY <= (CROXY * POROS)) K0NIT = 0.0
                    !
                    !           Calculate the nitrification flux
                    !
                    TEMP20 = TEMP - 20.0
                    TEMPC = TC ** TEMP20
                    AMFUNC = NH4 / (KSAM * POROS + NH4)
                    OXFUNC = OXY / (KSOX * POROS + OXY)
                    FLNIT = K0NIT + KNIT * TEMPC * AMFUNC * OXFUNC

                    !           maximise on the availebility of DO and NH4 with safety margin 0.5/0.9

                    NOX_RATIO = 4.57
                    FLNIT = MIN(FLNIT, 0.5 * OXY / NOX_RATIO / DELT)
                    FLNIT = MIN(FLNIT, 0.9 * NH4 / DELT)
                    FL(1 + IFLUX) = FLNIT
                    !
                    !           Zuurstoffunctie als uitvoer
                    !
                    PMSA(IP19) = OXFUNC
                    !
                    !
                    !     Use TEWOR version when IVERSN = 2.0
                    !
                ELSEIF (IVERSN == 2) THEN

                    NH4 = MAX (0.0, PMSA(IP2))
                    RC = PMSA(IP14)
                    OXY = MAX (0.0, PMSA(IP5))
                    KSOX = PMSA(IP7)
                    POROS = PMSA(IP12)
                    !
                    !           Calculate the nitrification flux
                    !
                    OXFUNC = OXY / (KSOX * POROS + OXY)
                    FL(1 + IFLUX) = RC * NH4 * OXFUNC
                    !
                    !           Zuurstoffunctie als uitvoer
                    !
                    PMSA(IP19) = OXFUNC
                    !
                    !     Use old version when IVERSN = 0.0
                    !
                ELSE
                    !
                    ZERO = PMSA(IP1)
                    CONC = MAX (0.0, PMSA(IP2))
                    RC = PMSA(IP14)
                    TC = PMSA(IP4)
                    OXY = MAX (0.0, PMSA(IP5))
                    OOX = PMSA(IP15)
                    COX = PMSA(IP11)
                    TEMP = PMSA(IP8)
                    CRTEMP = PMSA(IP9)
                    CFL = PMSA(IP16)
                    SKEWN = PMSA(IP17)
                    POROS = PMSA(IP12)
                    !
                    !           Calculate oxygen function
                    !
                    IF ((OOX - COX) < 1E-20)  CALL write_error_message &
                            ('OOX - COX in NITRIF zero')
                    IF (OXY > (OOX * POROS)) THEN
                        O2FUNC = 1.0
                    ELSEIF (OXY < (COX * POROS)) THEN
                        O2FUNC = CFL
                    ELSE
                        O2FUNC = (1.0 - CFL) * (OXY - COX * POROS) / &
                                ((OOX - COX) * POROS) + CFL
                        O2FUNC = (1.0 - CFL) * ((OXY - COX * POROS) / &
                                ((OOX - COX) * POROS))**10**SKEWN + CFL
                    ENDIF
                    !
                    !           Calculate flux
                    !
                    IF (TEMP <= CRTEMP) THEN
                        FL(1 + IFLUX) = ZERO
                    ELSE
                        TEMP20 = TEMP - 20.0
                        TEMPC = TC ** TEMP20
                        FL(1 + IFLUX) = ZERO + RC * CONC * TEMPC * O2FUNC
                    ENDIF
                    !
                    !           Zuurstoffunctie als uitvoer
                    !
                    PMSA(IP19) = O2FUNC
                    !
                ENDIF
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
            !
        end do
        !
        RETURN
        !
    END

end module m_nitrif
