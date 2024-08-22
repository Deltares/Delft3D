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
module m_denwat
    use m_waq_precision

    implicit none

contains


    subroutine denwat (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message

        !>\file
        !>       Denitrification in water column

        !
        !     Description of the module :
        !
        !        ----- old version -----
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            ----
        ! CROXY   R*4 1 I critical oxy. concentration for denititrification [g/m3]
        ! CRTEMP  R*4 1 L critical temperature for denitrification            [oC]
        ! CURVA   R*4 1 L constant in oxygen function                          [-]
        ! DENR    R*4 1 I zero-order denitrification rate                [gN/m3/d]
        ! DENRC   R*4 1 I first-order denitrification rate                   [1/d]
        ! FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
        ! NO3     R*4 1 I nitrate concentration                            [gN/m3]
        ! O2FUNC  R*4 1 L oxygen inhibition function                           [-]
        ! COXDEN  R*4 1 I critical oxy. concentration for denititrification [g/m3]
        ! OOXDEN  R*4 1 I critical oxy. concentration for denititrification [g/m3]
        ! OXY     R*4 1 I concentration of dissolved oxygen                [gN/m3]
        ! POROS   R*4 1 L porosity                                             [-]
        ! TC      R*4 1 I temperature coefficient for denitrif.                [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
        !
        !        ----- new version -----
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            ----
        ! CROXY   R*4 1 I critical oxy. concentration for denititrification [g/m3]
        ! CRTEMP  R*4 1 L critical temperature for denitrification            [oC]
        ! FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
        ! KDEN    R*4 1 I MM denitrification rate                        [gN/m3/d]
        ! K0DEN   R*4 1 I zero-order denitrification rate                [gN/m3/d]
        ! K0TEMP  R*4 1 I zero-order denitrification rate below CRTEMP   [gN/m3/d]
        ! K0OX    R*4 1 I zero-order denitrification rate below CROXY    [gN/m3/d]
        ! KSOX    R*4 1 L half saturation concentration for oxygen       [gN/m3/d]
        ! KSNI    R*4 1 L half saturation concentration for nitrate      [gN/m3/d]
        ! NIFUNC  R*4 1 L MM nitrate function                                  [-]
        ! NO3     R*4 1 I nitrate concentration                            [gN/m3]
        ! OXFUNC  R*4 1 L MM oxygen inhibition function                        [-]
        ! OXY     R*4 1 I concentration of dissolved oxygen                [gN/m3]
        ! POROS   R*4 1 L porosity                                             [-]
        ! TC      R*4 1 I temperature coefficient for denitrif.                [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14, IN15, IN16, IN17
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        REAL(kind = real_wp) :: TC, DENR, DENRC, OOXDEN, COXDEN, CURVA, &
                O2FUNC, DELTOX, CURVAQ
        INTEGER(kind = int_wp) :: IVERSN
        REAL(kind = real_wp) :: KDEN, K0DEN, K0TEMP, K0OX, KSNI, KSOX, &
                CROXY, NIFUNC, OXFUNC
        REAL(kind = real_wp) :: POROS, CRTEMP, OXY, NO3, TEMP, TEMPC, &
                TEMP20
        !
        LOGICAL  TMPOPT, OXYOPT
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
        !
        !     Factors that determine temperature effect space dependent?
        !
        IF (IN4 == 0 .AND. IN8 == 0) THEN
            !
            !        NO! Compute temperature effect and switch TMPOPT off
            !
            TEMP = process_space_real(IP8)
            TC = process_space_real(IP4)
            TEMP20 = TEMP - 20.0
            TEMPC = TC ** TEMP20
            TMPOPT = .FALSE.
        ELSE
            !
            !        YES! Switch TMPOPT on
            !
            TMPOPT = .TRUE.
        ENDIF
        !
        !     Factors that determine oxygen effect space dependent?
        !     Only relevant for old version IVERSN=0
        !
        IVERSN = NINT (process_space_real(IP13))
        IF (IVERSN == 0) THEN

            IF (IN5 == 0 .AND. IN15 == 0 .AND. IN11 == 0 .AND. &
                    IN16 == 0 .AND. IN12 == 0) THEN
                !
                !        NO! Compute oxygen effect and switch OXYOPT off
                !
                OXY = process_space_real(IP5)
                POROS = process_space_real(IP12)
                OOXDEN = process_space_real(IP15)
                COXDEN = process_space_real(IP11)
                DELTOX = (COXDEN - OOXDEN) * POROS
                IF (DELTOX < 1E-20)  CALL write_error_message &
                        ('(COXDEN - OOXDEN) in DENWAT <= zero')
                IF (OXY > (COXDEN * POROS)) THEN
                    O2FUNC = 0.0
                ELSEIF (OXY < (OOXDEN * POROS)) THEN
                    O2FUNC = 1.0
                ELSE
                    CURVA = MAX(process_space_real(IP16), 1.0)
                    CURVAQ = - LOG(1.) + EXP(CURVA)
                    O2FUNC = (COXDEN * POROS - OXY) / &
                            (DELTOX + CURVAQ * (OXY - OOXDEN * POROS))
                ENDIF
                OXYOPT = .FALSE.
            ELSE
                !
                !        YES! Switch OXYOPT on
                !
                OXYOPT = .TRUE.
            ENDIF
            !
        ENDIF
        !
        !     Loop over segments
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     Use new version when IVERSN=1
                !
                IF (IVERSN == 1) THEN
                    !
                    K0TEMP = process_space_real(IP1)
                    NO3 = MAX (0.0, process_space_real(IP2))
                    KDEN = process_space_real(IP3)
                    TC = process_space_real(IP4)
                    OXY = process_space_real(IP5)
                    KSNI = process_space_real(IP6)
                    KSOX = process_space_real(IP7)
                    TEMP = process_space_real(IP8)
                    CRTEMP = process_space_real(IP9)
                    K0OX = process_space_real(IP10)
                    CROXY = process_space_real(IP11)
                    POROS = process_space_real(IP12)
                    !
                    !           Set the rates according to CRTEMP and CROXY
                    !
                    IF (TEMP < CRTEMP .OR. OXY >= (CROXY * POROS)) KDEN = 0.0
                    !
                    K0DEN = 0.0
                    !
                    IF (TEMP < CRTEMP .AND. OXY < (CROXY * POROS)) THEN
                        K0DEN = K0TEMP
                    ELSEIF (TEMP >= CRTEMP .AND. OXY <(CROXY * POROS)) THEN
                        K0DEN = K0OX
                    ENDIF
                    !
                    !           Compute space dependent temperature effect if TMPOPT on
                    !
                    IF (TMPOPT) THEN
                        TEMP20 = TEMP - 20.0
                        TEMPC = TC ** TEMP20
                    ENDIF
                    !
                    !           Calculation of denitrification flux
                    !
                    NIFUNC = NO3 / (KSNI * POROS + NO3)
                    OXFUNC = 1.0 - OXY / (KSOX * POROS + OXY)
                    IF (OXY < 0.0) OXFUNC = 1.0
                    !
                    FL(1 + IFLUX) = K0DEN + KDEN * TEMPC * NIFUNC * OXFUNC
                    !
                    !           Zuurstoffunctie als uitvoer
                    !
                    process_space_real(IP17) = OXFUNC
                    !
                ELSE
                    !
                    !     Use old version whem IVERSN=0
                    !
                    NO3 = MAX (0.0, process_space_real(IP2))
                    DENR = process_space_real(IP1)
                    TEMP = process_space_real(IP8)
                    CRTEMP = process_space_real(IP9)
                    DENRC = process_space_real(IP14)
                    !
                    !           Compute space dependent temperature effect if TMPOPT on
                    !
                    IF (TMPOPT) THEN
                        TC = process_space_real(IP4)
                        TEMP20 = TEMP - 20.0
                        TEMPC = TC ** TEMP20
                    ENDIF
                    IF (TEMP <= CRTEMP) DENRC = 0.0
                    !
                    !           Compute space dependent oxygen effect if OXYOPT on
                    !
                    IF (OXYOPT) THEN
                        OXY = process_space_real(IP5)
                        POROS = process_space_real(IP12)
                        OOXDEN = process_space_real(IP15)
                        COXDEN = process_space_real(IP11)
                        DELTOX = (COXDEN - OOXDEN) * POROS
                        IF (DELTOX < 1E-20)  CALL write_error_message &
                                ('(COXDEN - OOXDEN) in DENWAT <= zero')
                        IF (OXY > COXDEN * POROS) THEN
                            O2FUNC = 0.0
                        ELSEIF (OXY < OOXDEN * POROS) THEN
                            O2FUNC = 1.0
                        ELSE
                            CURVA = MAX(process_space_real(IP16), 1.0)
                            CURVAQ = - LOG(1.) + EXP(CURVA)
                            O2FUNC = (COXDEN * POROS - OXY) / &
                                    (DELTOX + CURVAQ * (OXY - OOXDEN * POROS))
                        ENDIF
                        !
                    ENDIF
                    !
                    !           Denitrification is assumed to take place in the water
                    !           below a certain oxygen concentration in the water
                    !           Calculation of denitrification flux ( M.L-3.t-1)
                    !           Old:
                    !           O2FUNC = MAX ( 0.0 , ( COXDEN - OXY ) / DELTOX )
                    !           O2FUNC = (COXDEN - OXY) / (DELTOX + CURVAQ*(OXY - OOXDEN))
                    !           O2FUNC = MAX ( 0.0 , O2FUNC )
                    !           O2FUNC = MIN ( 1.0 , O2FUNC )
                    !           TEMFAK = DENRC * DENTC ** TEMP20
                    !           FL( 1 + IFLUX ) = DENR + TEMFAK * NO3 * O2FUNC
                    !
                    FL(1 + IFLUX) = DENR + DENRC * TEMPC * NO3 * O2FUNC
                    !
                    !           Zuurstoffunctie als uitvoer
                    !
                    process_space_real(IP17) = O2FUNC
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
            !
        end do
        !
        RETURN
    END

end module m_denwat
