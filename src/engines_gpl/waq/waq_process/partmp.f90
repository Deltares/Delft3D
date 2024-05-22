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
module m_partmp
    use m_waq_precision

    implicit none

contains


    subroutine partmp (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger, only : terminate_execution, write_error_message, get_log_unit_number
        use m_evaluate_waq_attribute

        !>\file
        !>       Partitioning of micropollutants

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        !        PARTITIONING OF MICROPOLLUTANTS: FREE DISSOLVED AND ADSORBED TO
        !        INORGANIC MATTER (THREE FRACTIONS), ORGANIC MATTER (SUM OF DETC
        !        OOC) AND ALGAE (3 GROUPS). NON-EQUILIBRIUM PARTITIONING
        !
        ! Name    T   L I/O   Description                              Units
        ! ----    --- -  -    -------------------                      ----
        ! AIMi    R*4 1 I  mass of inorganic SS                           [g/m3]
        ! CDIS    R*4 1 O  concentration free dissolved MP                [g/m3]
        ! CDOC    R*4 1 O  concentration adsorbed to DOC                  [g/m3]
        ! DELT    R*4 1 I  DELWAQ timestep                                   [d]
        ! DOC     R*4 1 I  dissolved organic Carbon                      [gC/m3]
        ! FACi    R*4 1 L  product of conc. sorbent and Kd                   [-]
        ! FDIS    R*4 1 L  fraction free dissolved in equilibrium            [-]
        ! FDIS2   R*4 1 O  fraction free dissolved                           [-]
        ! FDCOR   R*4 1 L  factor actual/eq. for free dissolved and DOC fr.  [-]
        ! FDOC    R*4 1 O  fraction sorbed to DOC                            [-]
        ! FIMi    R*4 1 O  fraction sorbed to IMi                            [-]
        ! FPARTE  R*4 1 L  fraction particulate in equilibrium               [-]
        ! FPARTO  R*4 1 L  fraction particulate begin of present time step   [-]
        ! FPART   R*4 1 L  fraction particulate end of present time step     [-]
        ! FPCOR   R*4 1 L  factor actual/eq. for POC-PHYT-IMx fractions      [-]
        ! FPHYT   R*4 1 O  fraction sorbed to PHYT                           [-]
        ! FPOC    R*4 1 O  fraction sorbed to POC                            [-]
        ! MP      R*4 1 L  concentration MP waterphase                    [g/m3]
        ! MPDIS   R*4 1 I  concentration MP waterphase [free+DOC]         [g/m3]
        ! MPPAR   R*4 1 I  concentration MP waterphase [IMx+POC+PHYT]     [g/m3]
        ! MPHUM   R*4 1 I  concentration MP waterphase [IMx+DOC+POC+PHYT] [g/m3]
        ! HVTADS  R*4 1 I  half life time kinetic adsorption                 [d]
        ! HVTDES  R*4 1 I  half life time kinetic adsorption                 [d]
        ! KDIMi   R*4 1 I  partition coefficient inorganic matter       [m3/gDM]
        ! KDOC    R*4 1 L  partition coefficient organic carbon          [m3/gC]
        ! KPHYT   R*4 1 I  partition coefficient phytoplankton           [m3/gC]
        ! KPOC    R*4 1 I  partition coefficient organic carbon          [m3/gC]
        ! PHYT    R*4 1 I  phytoplankton                                 [gC/m3]
        ! POC     R*4 1 I  particulate organic Carbon                    [gC/m3]
        ! POR     R*4 1 I  porosity waterphase                           [m3/m3]
        ! RATE    R*4 1 L  actual rate constant kinetic sorption           [1/d]
        ! Qy      R*4 1 O  quality of MP in adsorbens y                    [g/g]
        ! XDOC    R*4 1 I  KdDOC/KdPOC                                       [-]

        !     Logical Units : -

        IMPLICIT REAL (A-H, J-Z)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        LOGICAL  IM1OPT, IM2OPT, IM3OPT, IM4OPT, IM5OPT, IM6OPT, &
                FFFOPT, QQQOPT, WATOPT, HVTOPT, TWOFRC, QUALOPT
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24, IP25, IP26, IP27, IP28, IP29, IP30, &
                IP31, IP32, IP33, IP34, IP35, IP36, IP37, IP38, IP39, IP40, &
                IP41, IP42, IP43, IP44, IP45, IP46, IP47, IP48, IP49, IP50
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19, IN20, &
                IN21, IN22, IN23, IN24, IN25, IN26, IN27, IN28, IN29, IN30, &
                IN31, IN32, IN33, IN34, IN35, IN36, IN37, IN38, IN39, IN40, &
                IN41, IN42, IN43, IN44, IN45, IN46, IN47, IN48, IN49, IN50
        REAL(kind = real_wp) :: AIM1, KDIM1, FAC1, AIM2, KDIM2, FAC2, &
                AIM3, KDIM3, FAC3, POC, KPOC, FAC4, &
                PHYT, KPHYT, FAC5, DOC, XDOC, KDOC, &
                FAC6, HVTADS, HVTDES, MPPAR, MPDIS, DELT, &
                MP, POR, FPREC, SUMKD, FDIS, AFACT, &
                MPHUM, FPARTE, FPARTO, RATE, FPCOR, FDCOR, &
                FPART, PH, KCROHS, KCROH1, KCROH2, KCROH3, &
                OH, KSOL, CDIS, CRTOT, CRFREE, MOLWT, &
                FIM1, FIM2, FIM3, FPOC, FPHYT, FDOC, &
                QIM1, QIM2, QIM3, QPOC, QPHYT, CDOC, &
                DISS, DISHS, LKSOL, LKMES, LKMEHS, FSULF, &
                FDIS2, CDISM, QUAL, KDALL, VOLUME, IAP, &
                FAC6C
        INTEGER(kind = int_wp) :: LUNREP, IKMRK2
        INTEGER(kind = int_wp) :: IFLUX, ISEG, ISWOX, IGROUP
        LOGICAL  SEDIME
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
        IP47 = IPOINT(47)
        IP48 = IPOINT(48)
        IP49 = IPOINT(49)
        IP50 = IPOINT(50)
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
        IN47 = INCREM(47)
        IN48 = INCREM(48)
        IN49 = INCREM(49)
        IN50 = INCREM(50)
        !
        !     Check sediment switch for first segment
        !
        SEDIME = .FALSE.
        IF (PMSA(IP30) > 0.5) SEDIME = .TRUE.
        !
        !     Find metal group for first segment
        !     1 = GENERAL (ZN, CU, CD, PB, HG, NI)
        !     2 = CR
        !     3 = VA, AS
        !     4 = OMP's
        !     5 = viruses (simple partitioning)
        !
        IGROUP = NINT(PMSA(IP31))
        !
        !     OPTIMALISATION PART OUTSIDE SEGMENT LOOP
        !
        !        inorganic matter 1
        !
        IF (IN4 == 0 .AND. IN11 == 0) THEN
            AIM1 = PMSA(IP4)
            KDIM1 = PMSA(IP11) / 1000.
            FAC1 = AIM1 * KDIM1
            IM1OPT = .TRUE.
        ELSE
            IM1OPT = .FALSE.
        ENDIF
        !        inorganic matter 2
        IF (IN5 == 0 .AND. IN12 == 0) THEN
            AIM2 = PMSA(IP5)
            KDIM2 = PMSA(IP12) / 1000.
            FAC2 = AIM2 * KDIM2
            IM2OPT = .TRUE.
        ELSE
            IM2OPT = .FALSE.
        ENDIF
        !        inorganic matter 3
        IF (IN6 == 0 .AND. IN13 == 0) THEN
            AIM3 = PMSA(IP6)
            KDIM3 = PMSA(IP13) / 1000.
            FAC3 = AIM3 * KDIM3
            IM3OPT = .TRUE.
        ELSE
            IM3OPT = .FALSE.
        ENDIF
        !        POC
        IF (IN9 == 0 .AND. IN14 == 0) THEN
            POC = PMSA(IP9)
            IF (IGROUP == 4) THEN
                KPOC = 10**PMSA(IP14) / 1.E+6
            ELSE
                KPOC = PMSA(IP14) / 1000.
            ENDIF
            FAC4 = POC * KPOC
            IM4OPT = .TRUE.
        ELSE
            IM4OPT = .FALSE.
        ENDIF
        !        Phytoplankton
        IF (IN10== 0 .AND. IN15 == 0) THEN
            PHYT = PMSA(IP10)
            IF (IGROUP == 4) THEN
                KPHYT = 10**PMSA(IP15) / 1.E+6
            ELSE
                KPHYT = PMSA(IP15) / 1000.
            ENDIF
            FAC5 = PHYT * KPHYT
            IM5OPT = .TRUE.
        ELSE
            IM5OPT = .FALSE.
        ENDIF
        !        DOC and XDOC
        IF (IN7 == 0 .AND. IN8 == 0 .AND. &
                IN14 == 0) THEN
            DOC = PMSA(IP7)
            XDOC = PMSA(IP8)
            IF (IGROUP == 4) THEN
                KDOC = 10**PMSA(IP14) / 1.E+6
            ELSE
                KDOC = PMSA(IP14) / 1000.
            ENDIF
            FAC6 = DOC * XDOC * KDOC
            IM6OPT = .TRUE.
        ELSE
            IM6OPT = .FALSE.
        ENDIF
        !
        !        Kinetic sorption
        !
        IF (IN20 == 0 .AND. IN21 == 0) THEN
            HVTADS = PMSA(IP20)
            HVTDES = PMSA(IP21)
            HVTOPT = .TRUE.
        ELSE
            HVTOPT = .FALSE.
        ENDIF
        !
        !     Check for modelling of total or dis/par
        !
        IF (IN2==0.AND.IN3==0) THEN
            TWOFRC = .FALSE.
        ELSE
            TWOFRC = .TRUE.
        ENDIF

        IF (TWOFRC) THEN
            IF (IN1==0) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) &
                        'PARTMP: MP with the combination MPDIS/MPPAR is invalid'
                WRITE(*, *) &
                        'PARTMP: MP with the combination MPDIS/MPPAR is invalid'
                CALL terminate_execution(1)
            ENDIF
            IF (IN2==0) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) &
                        'PARTMP: No value for MPDIS in dis/par modelling'
                WRITE(*, *) 'PARTMP: No value for MPDIS in dis/par modelling'
                CALL terminate_execution(1)
            ENDIF
            IF (IN3==0) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) &
                        'PARTMP: No value for MPPAR in dis/par modelling'
                WRITE(*, *) 'PARTMP: No value for MPPAR in dis/par modelling'
                CALL terminate_execution(1)
            ENDIF
        ELSE
            IF (IN2>0) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) &
                        'PARTMP: Values for MPDIS and MP!'
                WRITE(*, *) 'PARTMP: Values for MPDIS and MP!'
                CALL terminate_execution(1)
            ENDIF
            IF (IN3>0) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) &
                        'PARTMP: Values for MPPAR and MP!'
                WRITE(*, *) 'PARTMP: Values for MPPAR and MP!'
                CALL terminate_execution(1)
            ENDIF
        ENDIF
        !        Compute F-values or not
        IF (IN34 == 0 .AND. IN35 == 0 .AND. &
                IN36 == 0 .AND. IN37 == 0 .AND. &
                IN38 == 0 .AND. IN39 == 0 .AND. &
                IN49 == 0 .AND. IN50 == 0) THEN
            FFFOPT = .TRUE.
        ELSE
            FFFOPT = .FALSE.
        ENDIF
        !        Compute Q-values or not
        IF (IN43 == 0 .AND. IN44 == 0 .AND. &
                IN45 == 0 .AND. IN46 == 0 .AND. &
                IN47 == 0) THEN
            QQQOPT = .TRUE.
        ELSE
            QQQOPT = .FALSE.
        ENDIF
        !        Compute Waterphase-values or not
        IF (IN33 == 0 .AND. IN41 == 0 .AND. &
                IN42 == 0 .AND. IN49 == 0 .AND. &
                IN50 == 0) THEN
            WATOPT = .TRUE.
        ELSE
            WATOPT = .FALSE.
        ENDIF
        !        Compute qual or not
        IF (IN49 == 0 .AND. IN50 == 0) THEN
            QUALOPT = .TRUE.
        ELSE
            QUALOPT = .FALSE.
        ENDIF
        DELT = PMSA(IP22)
        IF (DELT  < 1E-20)  CALL write_error_message ('DELT in PARTMP zero')
        !
        !----------------------------------------------------------------------C
        !     SEGMENT LOOP
        !----------------------------------------------------------------------C
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0.OR.IKMRK2==3).OR..NOT.SEDIME) THEN

                    MP = PMSA(IP1)
                    MPDIS = PMSA(IP2)
                    MPPAR = PMSA(IP3)
                    ISWOX = NINT(PMSA(IP23))

                    IF (TWOFRC) THEN
                        IF (IGROUP==2) THEN
                            CALL get_log_unit_number(LUNREP)
                            WRITE(LUNREP, *) &
                                    'PARTMP: Kinetic sorption for Chromium not ', &
                                    'implemented!'
                            WRITE(*, *) 'PARTMP: Kinetic sorption for Chromium not ', &
                                    'implemented!'
                            CALL terminate_execution(1)
                        ENDIF
                        MP = MPDIS + MPPAR
                    ENDIF
                    PMSA(IP48) = MP
                    POR = PMSA(IP16)
                    THICK = PMSA(IP17)
                    SURF = PMSA(IP18)
                    DMS1 = PMSA(IP19)
                    VOLUME = PMSA(IP32)

                    FL(1 + IFLUX) = 0.0
                    !
                    !     OXIC (ISWOX = 1) PARTITIONING FOR GENERAL METALS (GROUP 1)
                    !     OR CR (GROUP 2) OR VA/AS (GROUP 3) or OMP's (GROUP 4)
                    !     OR viruses (group 5)
                    !
                    IF ((ISWOX==1 .AND. IGROUP == 1) .OR. &
                            IGROUP == 2   .OR. &
                            IGROUP == 3   .OR. &
                            IGROUP == 4   .OR. &
                            IGROUP == 5) THEN
                        FPREC = 0.0

                        !          inorganic matter 1
                        IF (.NOT. IM1OPT) THEN
                            AIM1 = PMSA(IP4)
                            KDIM1 = PMSA(IP11) / 1000.
                            FAC1 = AIM1 * KDIM1
                        ENDIF
                        !          inorganic matter 2
                        IF (.NOT. IM2OPT) THEN
                            AIM2 = PMSA(IP5)
                            KDIM2 = PMSA(IP12) / 1000.
                            FAC2 = AIM2 * KDIM2
                        ENDIF
                        !          inorganic matter 3
                        IF (.NOT. IM3OPT) THEN
                            AIM3 = PMSA(IP6)
                            KDIM3 = PMSA(IP13) / 1000.
                            FAC3 = AIM3 * KDIM3
                        ENDIF
                        !          POC
                        IF (.NOT. IM4OPT) THEN
                            POC = PMSA(IP9)
                            IF (IGROUP == 4) THEN
                                KPOC = 10**PMSA(IP14) / 1.E+6
                            ELSE
                                KPOC = PMSA(IP14) / 1000.
                            ENDIF
                            FAC4 = POC * KPOC
                        ENDIF
                        !          Phytoplankton
                        IF (.NOT. IM5OPT) THEN
                            PHYT = PMSA(IP10)
                            IF (IGROUP == 4) THEN
                                KPHYT = 10**PMSA(IP15) / 1.E+6
                            ELSE
                                KPHYT = PMSA(IP15) / 1000.
                            ENDIF
                            FAC5 = PHYT * KPHYT
                        ENDIF
                        !          DOC and XDOC
                        IF (.NOT. IM6OPT) THEN
                            DOC = PMSA(IP7)
                            XDOC = PMSA(IP8)
                            IF (IGROUP == 4) THEN
                                KDOC = 10**PMSA(IP14) / 1.E+6
                            ELSE
                                KDOC = PMSA(IP14) / 1000.
                            ENDIF
                            FAC6 = DOC * XDOC * KDOC
                        ENDIF
                        !
                        IF (.NOT.SEDIME .AND. POR<1E-20) &
                                CALL write_error_message ('POR in PARTMP zero')
                        !
                        !***********************************************************************
                        !**** Processes connected to the HEAVY METAL PARTITIONING
                        !***********************************************************************
                        !
                        !       Convert DOC (g/m3) to (g/m2) FOR SEDIMENT
                        !
                        FAC6C = FAC6
                        IF (SEDIME) FAC6C = FAC6 * POR * THICK
                        !
                        !       Partitioning (free dissolved and adsorbed)
                        !
                        SUMKD = FAC1 + FAC2 + FAC3 + FAC4 + FAC5 + FAC6C

                        IF (SUMKD > 1.E-20) THEN
                            IF (SEDIME) THEN
                                IF (THICK > 1.E-20) THEN
                                    FDIS = POR / (POR + SUMKD / THICK)
                                ELSE
                                    FDIS = 1.0
                                ENDIF
                            ELSE
                                FDIS = POR / (POR + SUMKD)
                            ENDIF
                            AFACT = (1.0 - FDIS) / SUMKD
                        ELSE
                            FDIS = 1.0
                            AFACT = 0.0
                        ENDIF

                        MPHUM = AFACT * MP
                        !
                        !       Equilibrium fraction PART (excluding DOC!!!)
                        !
                        FPARTE = AFACT * (FAC1 + FAC2 + FAC3 + FAC4 + FAC5)
                        !
                        !       Kinetic sorption
                        !
                        IF (.NOT. HVTOPT) THEN
                            HVTADS = PMSA(IP20)
                            HVTDES = PMSA(IP21)
                        ENDIF
                        IF (HVTADS > 1E-20 .OR. HVTDES > 1E-20) THEN
                            IF (.NOT.TWOFRC) THEN
                                CALL get_log_unit_number(LUNREP)
                                WRITE(LUNREP, *) &
                                        'PARTMP: Kinetic sorption one MP fraction ', &
                                        'not possible!'
                                WRITE(LUNREP, *) &
                                        'Create MP-Dis & MP-Part for kinetic sorption'
                                WRITE(LUNREP, *) &
                                        'or set HLTAdsMP and HLTDesMP to zero'
                                WRITE(*, *) 'PARTMP: Kinetic sorption one MP fraction ', &
                                        'not possible!'
                                WRITE(*, *) 'Create MP-Dis & MP-Part for kinetic sorption'
                                WRITE(*, *) 'or set HLTAdsMP and HLTDesMP to zero'
                                CALL terminate_execution(1)
                            ENDIF
                            !           Let op: PART is in dit verband POC + IMx + PHYT !!!
                            !           Actual fraction PART
                            IF (MP > 1E-20) THEN
                                FPARTO = MPPAR / MP
                            ELSE
                                FPARTO = 0.0
                            ENDIF
                            RATE = -1.0
                            IF (FPARTO > FPARTE .AND. HVTDES > 1E-20) &
                                    RATE = LOG(2.) / HVTDES
                            IF (FPARTO < FPARTE .AND. HVTADS > 1E-20) &
                                    RATE = LOG(2.) / HVTADS
                            !           Fraction part at end of present time step
                            IF (RATE > 0.0 .AND. FPARTE > 1E-20 &
                                    .AND. FPARTE < 1.0) THEN
                                FPART = FPARTE - (FPARTE - FPARTO) * EXP (-RATE * DELT)
                                FPCOR = FPART / FPARTE
                                FDCOR = (1.0 - FPART) / (1.0 - FPARTE)
                            ELSE
                                FPART = FPARTE
                                FPCOR = 1.0
                                FDCOR = 1.0
                            ENDIF
                        ELSE
                            FPART = FPARTE
                            FPCOR = 1.0
                            FDCOR = 1.0
                        ENDIF
                        !
                        !       Flux from particulate to dissolved fraction
                        !
                        IF (TWOFRC) THEN
                            IF (SEDIME) THEN
                                FL(1 + IFLUX) = (FPART * MP - MPPAR) / DELT / VOLUME
                            ELSE
                                FL(1 + IFLUX) = (FPART * MP - MPPAR) / DELT
                            ENDIF
                        ELSE
                            FL(1 + IFLUX) = 0.0
                        ENDIF
                        !
                        !       CHROMIUM
                        !
                        IF (IGROUP==2) THEN
                            PH = PMSA(IP24)
                            KCROHS = PMSA(IP25)
                            MOLWT = PMSA(IP26)
                            KCROH1 = PMSA(IP27)
                            KCROH2 = PMSA(IP28)
                            KCROH3 = PMSA(IP29)

                            OH = 10.0**(-(14 - PH))
                            !
                            !-- 1.4 Determine solubility product (as a dissociation K !!).
                            !
                            KSOL = 10**(- KCROHS)
                            !
                            !-- 2.2 Determine concentration of free Cr3+ ,using fdis as calculated
                            !       under (2.1), and calculate IAP of Cr(OH)3s.
                            !
                            !       CDIS (mg/m3) corresponds with CRTOT (mol/l).
                            !       conversion = ug/l * 1.e-6 g/ug / (M  g/mol)
                            !
                            IF (POR <= 1E-10) THEN
                                CDIS = 0.0
                            ELSE
                                IF (SEDIME) THEN
                                    IF (THICK > 1.E-20) THEN
                                        CDIS = FDIS * MP / (POR * THICK)
                                    ELSE
                                        CDIS = 0.0
                                    ENDIF
                                ELSE
                                    CDIS = FDIS * MP / POR
                                ENDIF
                            ENDIF
                            CRTOT = CDIS * 1.0E-3 / MOLWT
                            !
                            !--   Crtot = Cr3+ + (CrOH 2+) + (Cr(OH)2 +) + (Cr(OH)3 0+)
                            !     Cr3+ = Crtot / (1 + K1*OH + K2*(OH)^2 + K3*(OH)^3)
                            !     (mol/l), K's of the association reaction.
                            !
                            CRFREE = CRTOT / (1 + &
                                    10**(KCROH1) * OH + &
                                    10**(KCROH2) * OH**2 + &
                                    10**(KCROH3) * OH**3)
                            !
                            IAP = CRFREE * OH**3
                            !
                            !-- 2.3 If IAP > Ksol Cr(OH)3s precipitation will take place. In this
                            !       case fdis must be recalculated.
                            !       If IAP < Ksol no precipitation will take place and the under
                            !       (2.1) calculated fdis is correct.
                            !
                            IF (IAP >= KSOL) THEN
                                !
                                !------ 2.3a Calculate Cr3+ and Crtot at present OH- concentration,
                                !            (mol/l) and convert to concentration in mg/m3 (= ug/l).
                                !
                                CRFREE = KSOL / (OH**3)
                                CRTOT = CRFREE * (1 + &
                                        10**(KCROH1) * OH + &
                                        10**(KCROH2) * OH**2 + &
                                        10**(KCROH3) * OH**3)
                                !
                                !------    Conversion = mol/l * M g/mol * 1.0e+6 ug/g
                                !
                                CRTOT = CRTOT * MOLWT * 1.0E+3
                                IF (SEDIME) THEN
                                    FPREC = 1. - (CRTOT * POR * THICK / (FDIS * MP))
                                ELSE
                                    FPREC = 1. - (CRTOT * POR / (FDIS * MP))
                                ENDIF
                            ENDIF
                        ENDIF
                        !
                        PMSA(IP40) = FPREC

                        IF (.NOT. FFFOPT) THEN
                            FIM1 = AFACT * FAC1 * FPCOR * (1. - FPREC)
                            FIM2 = AFACT * FAC2 * FPCOR * (1. - FPREC)
                            FIM3 = AFACT * FAC3 * FPCOR * (1. - FPREC)
                            FPOC = AFACT * FAC4 * FPCOR * (1. - FPREC)
                            FPHYT = AFACT * FAC5 * FPCOR * (1. - FPREC)
                            FDOC = AFACT * FAC6C * FDCOR * (1. - FPREC)
                            PMSA(IP34) = FDOC
                            PMSA(IP35) = FIM1
                            PMSA(IP36) = FIM2
                            PMSA(IP37) = FIM3
                            PMSA(IP38) = FPOC
                            PMSA(IP39) = FPHYT
                        ENDIF
                        !
                        IF (.NOT. QQQOPT) THEN
                            QIM1 = 0.0
                            QIM2 = 0.0
                            QIM3 = 0.0
                            QPOC = 0.0
                            QPHYT = 0.0
                            IF (MPHUM > 1E-20) THEN
                                IF (AIM1>1E-20) QIM1 = MPHUM * KDIM1 * FPCOR * (1. - FPREC)
                                IF (AIM2>1E-20) QIM2 = MPHUM * KDIM2 * FPCOR * (1. - FPREC)
                                IF (AIM3>1E-20) QIM3 = MPHUM * KDIM3 * FPCOR * (1. - FPREC)
                                IF (POC >1E-20) QPOC = MPHUM * KPOC * FPCOR * (1. - FPREC)
                                IF (PHYT>1E-20) QPHYT = MPHUM * KPHYT * FPCOR * (1. - FPREC)
                            ENDIF
                            PMSA(IP43) = QIM1
                            PMSA(IP44) = QIM2
                            PMSA(IP45) = QIM3
                            PMSA(IP46) = QPOC
                            PMSA(IP47) = QPHYT
                        ENDIF

                        IF (.NOT. WATOPT) THEN
                            !@      Concentration free dissolved MP and DOC waterphase
                            FDIS = FDIS * (1. - FPREC)
                            FDIS2 = FDIS * FDCOR
                            IF (POR <= 1.E-10) THEN
                                CDIS = 0.0
                                CDOC = 0.0
                            ELSE
                                IF (SEDIME) THEN
                                    IF (THICK > 1.E-20) THEN
                                        CDIS = FDIS * MP * FDCOR / (THICK * POR)
                                        CDOC = (1.0 - FPREC) * AFACT * FAC6C * MP * FDCOR / &
                                                (THICK * POR)
                                    ELSE
                                        CDIS = 0.0
                                        CDOC = 0.0
                                    ENDIF
                                ELSE
                                    CDIS = FDIS * MP * FDCOR / POR
                                    CDOC = (1.0 - FPREC) * AFACT * FAC6C * MP * FDCOR / POR
                                ENDIF
                            ENDIF
                            !@      Quality of MP adsorbens waterphase
                            FDIS = FDIS2
                            PMSA(IP33) = FDIS
                            PMSA(IP41) = CDIS
                            PMSA(IP42) = CDOC
                        ENDIF
                        !
                        !     SULFIDIC PARTITIONING (ISWOX = 0) FOR GENERAL METALS
                        !
                    ELSEIF (ISWOX==0 .AND. IGROUP == 1) THEN
                        PMSA(IP34) = 0.0
                        PMSA(IP35) = 0.0
                        PMSA(IP36) = 0.0
                        PMSA(IP37) = 0.0
                        PMSA(IP38) = 0.0
                        PMSA(IP39) = 0.0
                        PMSA(IP42) = 0.0
                        PMSA(IP43) = 0.0
                        PMSA(IP44) = 0.0
                        PMSA(IP45) = 0.0
                        PMSA(IP46) = 0.0
                        PMSA(IP47) = 0.0

                        DISS = PMSA(IP24)
                        DISHS = PMSA(IP25)
                        IF (DISS <= 1E-20) THEN
                            WRITE(*, *) 'SwPoreChWK = ', ISWOX, ' 1- oxic, 0 - sulfidic, or'
                            WRITE(*, *) 'DisSWK or DisSSx  = ', DISS, 'should not equal zero'
                            CALL write_error_message ('Fatal error in PARTMP')
                        ENDIF
                        MOLWT = PMSA(IP26)
                        LKSOL = PMSA(IP27)
                        LKMES = PMSA(IP28)
                        LKMEHS = PMSA(IP29)

                        IF (DMS1 < 1E-03)  THEN
                            FDIS = 1.0
                            IF (SEDIME) THEN
                                IF (THICK > 1.E-20) THEN
                                    CDIS = MP / (THICK * POR)
                                ELSE
                                    CDIS = 0.0
                                ENDIF
                            ELSE
                                CDIS = MP / POR
                            ENDIF
                            FSULF = 0.0
                        ELSEIF (POR < 1E-10) THEN
                            FSULF = 1.0
                            FDIS = 0.0
                            CDIS = 0.0
                        ELSE
                            CDISM = (1 / (10**LKSOL * DISS)) * &
                                    (1 + 10**LKMES * DISS + 10**LKMEHS * DISHS)
                            CDIS = CDISM * MOLWT * 1000.
                            IF (SEDIME) THEN
                                FDIS = CDIS * THICK * POR / MP
                            ELSE
                                FDIS = CDIS * POR / MP
                            ENDIF
                        ENDIF
                        FSULF = 1. - FDIS
                        FDOC = 0.0
                        CDOC = 0.0

                        PMSA(IP33) = FDIS
                        PMSA(IP40) = FSULF
                        PMSA(IP41) = CDIS

                    ELSE
                        CALL get_log_unit_number(LUNREP)

                        WRITE(LUNREP, *) 'Invalid option for partitioning!'
                        WRITE(LUNREP, *) 'SwOXIC= ', ISWOX, ' 1- oxic, 0 - anoxic'
                        WRITE(LUNREP, *) &
                                'Group = ', IGROUP, ' 1- General, 2-Cr, 3-As/Va, 4-OMP, 5-virus'

                        WRITE(*, *) 'Invalid option for partitioning!'
                        WRITE(*, *) 'SwOXIC= ', ISWOX, ' 1- oxic, 0 - anoxic'
                        WRITE(*, *) 'Group = ', IGROUP, ' 1- General, 2-Cr, 3-As/Va, 4-OMP, &
                                5-virus'
                        CALL terminate_execution(1)
                    ENDIF
                    !
                    !     Addition of former process MPQUAL
                    !
                    QUAL = 0.0
                    KDALL = 0.0
                    IF (.NOT. QUALOPT) THEN
                        IF (DMS1 >= 1E-20)  THEN
                            !
                            !           Compute in g/g
                            !
                            QUAL = MP * (1.0 - FDIS - FDOC) / DMS1
                            !
                            !           Overall partitioning coefficient
                            !
                            IF ((CDIS + CDOC) >= 1E-20) THEN
                                !                        g/g     g/m3         = m3/g
                                KDALL = QUAL / (CDIS + CDOC)
                            ELSE
                                KDALL = 0.0
                            ENDIF
                            !
                            !           Convert to mg/kg (QUAL) or to l/kg (KDALL)
                            !
                            QUAL = QUAL * 1.E6
                            KDALL = KDALL * 1.E6

                        ENDIF
                    ENDIF
                    !
                    !     Output
                    !
                    PMSA (IP49) = QUAL
                    PMSA (IP50) = KDALL

                ENDIF
            ENDIF
            !
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
            IP47 = IP47 + IN47
            IP48 = IP48 + IN48
            IP49 = IP49 + IN49
            IP50 = IP50 + IN50
            IFLUX = IFLUX + NOFLUX
            !
            !
        end do
        !
        RETURN
    END

end module m_partmp
