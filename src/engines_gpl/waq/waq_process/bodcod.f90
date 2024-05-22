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
module m_bodcod
    use m_waq_precision

    implicit none

contains


    subroutine bodcod (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger, only : terminate_execution, get_log_unit_number


        !>\file
        !>       Decay of BOD, COD and NBOD and associated oxygen consumption

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Decay of BOD, COD and NBOD and associated oxygen consumption
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! ISW     I*4 1 I switch oxygen consumption 0=BOD;1=COD;2=BOD+COD      [-]
        ! CBOD5   R*4 1 I carbonaceous BOD (first pool) at 5 days          [gO/m3]
        ! CBOD52  R*4 1 I carbonaceous BOD (second pool) at 5 days         [gO/m3]
        ! CBODU   R*4 1 I carbonaceous BOD (first pool) ultimate           [gO/m3]
        ! CBODU2  R*4 1 I carbonaceous BOD (second pool) ultimate          [gO/m3]
        ! CODCR   R*4 1 I COD concentration by the Cr-method               [gO/m3]
        ! CODMN   R*4 1 I COD concentration by the Mr-method               [gO/m3]
        ! CNBOD5  R*4 1 I nitrogenous BOD at 5 days                        [gO/m3]
        ! CNBODU  R*4 1 I nitrogenous BOD ultimate                         [gO/m3]
        ! RCBOD   R*4 1 I decay reaction rate BOD (first pool) at 20  C      [1/d]
        ! RCBOD2  R*4 1 I decay reaction rate BOD (second pool) at 20  C     [1/d]
        ! RCCOD   R*4 1 I decay reaction rate COD at 20  C                   [1/d]
        ! RCBODN  R*4 1 I decay reaction rate NBOD at 20  C                  [1/d]
        ! TCBOD   R*4 1 I decay temperature coefficient BOD                    [-]
        ! TCCOD   R*4 1 I decay temperature coefficient COD                    [-]
        ! TCBODN  R*4 1 I decay temperature coefficient NBOD                   [-]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! OXY     R*4 1 I oxygen concentration                             [gO/m3]
        ! COXBOD  R*4 1 I critical oxygen concentration                    [gO/m3]
        ! OOXBOD  R*4 1 I optimal  oxygen concentration                    [gO/m3]
        ! CFLBOD  R*4 1 I oxygen function level for OXY below COXBOD           [-]
        ! CRVBOD  R*4 1 I curvature oxygen function                            [-]
        ! AGEFL   R*4 1 I lower value age function                             [-]
        ! AGEFU   R*4 1 I upper value age function                             [-]
        ! AGEIL   R*4 1 I lower value age index                                [-]
        ! AGEIU   R*4 1 I upper value age index                                [-]
        ! PHYT    R*4 1 I algae concentration                              [gC/m3]
        ! PHYT5U  R*4 1 I Ratio BOD5/BODinf in algae                           [-]
        ! FPHBOD  R*4 1 I fraction algae contributing to BOD-inf               [-]
        ! OXCCF   R*4 1 I amount oxygen per carbon in mineralisation       [gO/gC]
        ! POC     R*4 1 I POC   concentration                              [gC/m3]
        ! POC5U   R*4 1 I Ratio BOD5/BODinf in POC                             [-]
        ! FPCBOD  R*4 1 I fraction POC   contributing to BOD-inf               [-]
        ! EFFCCR  R*4 1 I efficiency of Cr method for COD                      [-]
        ! EFFCMN  R*4 1 I efficiency of Mn method for COD                      [-]
        ! O2FBOD  R*4 1 O oxygen function for decay of CBOD                    [-]
        ! AGEFUN  R*4 1 O age function for decay rates CBOD and NBOD           [-]
        ! BOD5    R*4 1 O calculated carbonaceous BOD at 5 days            [gO/m3]
        ! BODU    R*4 1 O calculated carbonaceous BOD at ultimate          [gO/m3]
        ! COD     R*4 1 O calculated chemical oxygen demand COD            [gO/m3]
        ! BD5POC  R*4 1 O contribution of POC to calculated BOD5           [gO/m3]
        ! BDUPOC  R*4 1 O contribution of POC to calculated BODu           [gO/m3]
        ! BD5PHY  R*4 1 O contribution of Phyt to calculated BOD5          [gO/m3]
        ! BDUPHY  R*4 1 O contribution of Phyt to calculated BODu          [gO/m3]
        ! DBOD5   R*4 1 O decay flux of CBOD5                            [gO/m3/d]
        ! DBOD52  R*4 1 O decay flux of CBOD5_2                          [gO/m3/d]
        ! DBODU   R*4 1 O decay flux of CBODu                            [gO/m3/d]
        ! DBODU2  R*4 1 O decay flux of CBODu_2                          [gO/m3/d]
        ! DNBOD5  R*4 1 O decay flux of COD_Cr                           [gO/m3/d]
        ! DNBODU  R*4 1 O decay flux of COD_Mn                           [gO/m3/d]
        ! DCODCR  R*4 1 O decay flux of NBOD5                            [gO/m3/d]
        ! DCODMN  R*4 1 O decay flux of NBODu                            [gO/m3/d]
        ! OXYDEM  R*4 1 O oxygen consumption flux of BOD and COD         [gO/m3/d]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: LUNREP

        LOGICAL  TFACT, AFACT
        REAL(kind = real_wp) :: RAT5N, BODN, BDNPHY, AMCCF, BDNPOC

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
        IN26 = INCREM(26)
        IN27 = INCREM(27)
        IN28 = INCREM(28)
        IN29 = INCREM(29)
        IN30 = INCREM(30)
        IN31 = INCREM(31)
        IN32 = INCREM(32)
        IN33 = INCREM(33)
        IN34 = INCREM(34)
        IN35 = INCREM(35)
        IN36 = INCREM(36)
        IN37 = INCREM(37)
        IN38 = INCREM(38)
        IN39 = INCREM(39)
        IN40 = INCREM(40)
        IN41 = INCREM(41)
        IN42 = INCREM(42)
        IN43 = INCREM(43)
        IN44 = INCREM(44)
        IN45 = INCREM(45)
        IN46 = INCREM(46)
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
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)
        IP28 = IPOINT(28)
        IP29 = IPOINT(29)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)
        IP33 = IPOINT(33)
        IP34 = IPOINT(34)
        IP35 = IPOINT(35)
        IP36 = IPOINT(36)
        IP37 = IPOINT(37)
        IP38 = IPOINT(38)
        IP39 = IPOINT(39)
        IP40 = IPOINT(40)
        IP41 = IPOINT(41)
        IP42 = IPOINT(42)
        IP43 = IPOINT(43)
        IP44 = IPOINT(44)
        IP45 = IPOINT(45)
        IP46 = IPOINT(46)
        !

        IF (IN14==0 .AND. IN15==0 .AND. IN16==0 .AND.IN17==0) THEN
            TCBOD = PMSA(IP14)
            TCCOD = PMSA(IP15)
            TCBODN = PMSA(IP16)
            TEMP = PMSA(IP17)

            TFBOD = TCBOD ** (TEMP - 20.)
            TFCOD = TCCOD ** (TEMP - 20.)
            TFBODN = TCBODN** (TEMP - 20.)

            TFACT = .FALSE.

        ELSE
            TFACT = .TRUE.
        ENDIF

        AGEFL = PMSA(IP23)
        AGEFU = PMSA(IP24)
        IF (IN23==0 .AND.IN24==0 .AND.ABS(AGEFL - AGEFU)<1.0E-6) THEN
            AGEFUN = AGEFL

            AFACT = .FALSE.

        ELSE
            AFACT = .TRUE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                IF (TFACT) THEN
                    TCBOD = PMSA(IP14)
                    TCCOD = PMSA(IP15)
                    TCBODN = PMSA(IP16)
                    TEMP = PMSA(IP17)

                    TFBOD = TCBOD ** (TEMP - 20.)
                    TFCOD = TCCOD ** (TEMP - 20.)
                    TFBODN = TCBODN** (TEMP - 20.)

                ENDIF

                !

                ISW = NINT(PMSA(IP1))
                CBOD5 = MAX(0., PMSA(IP2))
                CBOD52 = MAX(0., PMSA(IP3))
                CBODU = MAX(0., PMSA(IP4))
                CBODU2 = MAX(0., PMSA(IP5))
                CODCR = MAX(0., PMSA(IP6))
                CODMN = MAX(0., PMSA(IP7))
                CNBOD5 = MAX(0., PMSA(IP8))
                CNBODU = MAX(0., PMSA(IP9))
                RCBOD = PMSA(IP10)
                RCBOD2 = PMSA(IP11)
                RCCOD = PMSA(IP12)
                RCBODN = PMSA(IP13)
                OXY = MAX(0., PMSA(IP18))
                COXBOD = PMSA(IP19)
                OOXBOD = PMSA(IP20)
                CFLBOD = PMSA(IP21)
                CRVBOD = PMSA(IP22)
                PHYT = PMSA(IP27)
                PHYT5U = PMSA(IP28)
                FPHBOD = PMSA(IP29)
                OXCCF = PMSA(IP30)
                POC = PMSA(IP31)
                POC5U = PMSA(IP32)
                FPCBOD = PMSA(IP33)
                EFFCCR = PMSA(IP34)
                EFFCMN = PMSA(IP35)
                AMCCF = PMSA(IP36)

                !     CHECK IF RC'S ARE NON ZERO
                IF (RCBOD < 1E-10) THEN
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'RCBOD: Invalid value (zero)!'
                    WRITE (*, *) 'RCBOD: Invalid value (zero)!'
                    CALL terminate_execution(1)
                ENDIF
                IF (RCBOD2 < 1E-10) THEN
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'RCBOD-2: Invalid value (zero)!'
                    CALL terminate_execution(1)
                ENDIF


                !     SUBSTANCE AGGREGATION

                RAT5U = 1. - EXP(-5. * RCBOD)
                RAT5U2 = 1. - EXP(-5. * RCBOD2)
                RAT5N = 1. - EXP(-5. * RCBODN)
                BOD5 = CBOD5 + CBOD52 + CBODU * RAT5U + CBODU2 * RAT5U2
                BODU = CBODU + CBODU2 + CBOD5 / RAT5U + CBODU2 / RAT5U2
                BODN = CNBODU + CNBOD5 / RAT5N

                BDUPHY = PHYT * FPHBOD * OXCCF
                BD5PHY = BDUPHY * PHYT5U
                BDNPHY = PHYT * FPHBOD * AMCCF

                BDUPOC = POC * FPCBOD * OXCCF
                BD5POC = BDUPOC * POC5U
                BDNPOC = POC * FPCBOD * AMCCF

                BOD5 = BOD5 + BD5PHY + BD5POC
                BODU = BODU + BDUPHY + BDUPOC
                BODN = BODN + BDNPHY + BDNPOC

                COD = CODCR / EFFCCR + CODMN / EFFCMN

                !     AGE FUNCTION

                IF (AFACT) THEN

                    AGEFL = PMSA(IP23)

                    IF ((COD<1.0E-06).OR.(BOD5<1.0E-6)) THEN
                        AGEFUN = AGEFL
                    ELSE

                        AGEFU = PMSA(IP24)
                        AGEIL = PMSA(IP25)
                        AGEIU = PMSA(IP26)

                        AGEIND = COD / BOD5

                        IF (AGEIND<=AGEIL) THEN
                            AGEFUN = AGEFU

                        ELSE
                            AGEFUN = AGEFL + (AGEFU - AGEFL) * &
                                    EXP(-((AGEIND - AGEIL) / AGEIU)**2)

                        ENDIF
                    ENDIF
                ENDIF

                !     OXYGEN FUNCTION

                IF (OXY>=OOXBOD) THEN
                    O2FBOD = 1.
                ELSEIF (OXY<=COXBOD) THEN
                    O2FBOD = CFLBOD
                ELSE
                    O2FBOD = CFLBOD + (1. - CFLBOD) * &
                            ((OXY - COXBOD) / (OOXBOD - COXBOD)) ** (10 ** CRVBOD)
                ENDIF

                !     DECAY RATES
                DBOD5 = CBOD5 * RCBOD * TFBOD * O2FBOD * AGEFUN
                DBOD52 = CBOD52 * RCBOD2 * TFBOD * O2FBOD * AGEFUN
                DBODU = CBODU * RCBOD * TFBOD * O2FBOD * AGEFUN
                DBODU2 = CBODU2 * RCBOD2 * TFBOD * O2FBOD * AGEFUN
                DNBOD5 = CNBOD5 * RCBODN * TFBODN * O2FBOD * AGEFUN
                DNBODU = CNBODU * RCBODN * TFBODN * O2FBOD * AGEFUN
                DCODCR = CODCR * RCCOD * TFCOD
                DCODMN = CODMN * RCCOD * TFCOD

                !     OXYGEN DEMAND

                IF (ISW==0) THEN
                    !       BOD
                    OXYDEM = DBOD5 + DBOD52 + DBODU + DBODU2 + DNBOD5 + DNBODU
                ELSEIF (ISW==1) THEN
                    !       COD
                    OXYDEM = DCODCR + DCODMN
                ELSEIF (ISW==2) THEN
                    !       BOD + COD
                    OXYDEM = DBOD5 + DBOD52 + DBODU + DBODU2 + DNBOD5 + DNBODU + &
                            DCODCR + DCODMN
                ELSE
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'BODCOD: Invalid option for SwOXYDem!'
                    WRITE (*, *) 'BODCOD: Invalid option for SwOXYDem!'
                    CALL terminate_execution(1)
                ENDIF

                PMSA(IP37) = O2FBOD
                PMSA(IP38) = AGEFUN
                PMSA(IP39) = BOD5
                PMSA(IP40) = BODU
                PMSA(IP41) = COD
                PMSA(IP42) = BD5POC
                PMSA(IP43) = BDUPOC
                PMSA(IP44) = BD5PHY
                PMSA(IP45) = BDUPHY
                PMSA(IP46) = BODN

                FL(1 + IFLUX) = DBOD5
                FL(2 + IFLUX) = DBOD52
                FL(3 + IFLUX) = DBODU
                FL(4 + IFLUX) = DBODU2
                FL(5 + IFLUX) = DNBOD5
                FL(6 + IFLUX) = DNBODU
                FL(7 + IFLUX) = DCODCR
                FL(8 + IFLUX) = DCODMN
                FL(9 + IFLUX) = OXYDEM
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
            IP26 = IP26 + IN26
            IP27 = IP27 + IN27
            IP28 = IP28 + IN28
            IP29 = IP29 + IN29
            IP30 = IP30 + IN30
            IP31 = IP31 + IN31
            IP32 = IP32 + IN32
            IP33 = IP33 + IN33
            IP34 = IP34 + IN34
            IP35 = IP35 + IN35
            IP36 = IP36 + IN36
            IP37 = IP37 + IN37
            IP38 = IP38 + IN38
            IP39 = IP39 + IN39
            IP40 = IP40 + IN40
            IP41 = IP41 + IN41
            IP42 = IP42 + IN42
            IP43 = IP43 + IN43
            IP44 = IP44 + IN44
            IP45 = IP45 + IN45
            IP46 = IP46 + IN46
            !
        end do
        !
        RETURN
        !
    END

end module m_bodcod
