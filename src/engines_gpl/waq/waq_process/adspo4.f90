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
module m_adspo4
    use m_waq_precision

    implicit none

contains


    subroutine adspo4 (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_monsys
        use m_write_error_message

        !>\file
        !>       P-ad/desorption to particulate inorganic matter. 3 options for sorption formulation.

        !
        !     Description of the module :
        !     P-adsorption onto particulate inorganic matter. 3 options for
        !     sorption formulation.
        !
        !        ----- old version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! AAP     R*4 1 I AAP concentration                                [gP/m3]
        ! SWAdsP  R*4 1 I swithc for adsorption option to use                  [-]
        ! DELT    R*4 1 I DELWAQ timestep                                    [scu]
        ! EQAAP   R*4 1 L calculated equlibrium AAP concentration          [gP/m3]
        ! FL (1)  R*4 1 O adsorption/desorption flux                     [gP/m3/d]
        ! TIM     R*4 1 I TIM   concentration                             [gDM/m3]
        ! KD      R*4 1 I partition coefficent PO4-AAP             [-] or [m3/gDM]
        ! MAXADS  R*4 1 I maximum adsorption (capacity)                   [gP/gDM]
        ! PMSA(9) R*4 1 O calculated equlibrium AAP concentration          [gP/m3]
        ! PO4     R*4 1 I PO4 concentration                                [gP/m3]
        ! RCADS   R*4 1 I first order rate constatnt adsorption              [1/d]
        !
        !        ----- new version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! AAP     R*4 1 I AAP concentration                                [gP/m3]
        ! PO4     R*4 1 I PO4 concentration                                [gP/m3]
        ! EQAAP   R*4 1 - calculated equlibrium AAP concentration          [gP/m3]
        ! KDADS   R*4 1 I distribution coeff. [-], or adsorpt. constant    [m3/gP]
        ! KADS20  R*4 1 I molar adsorption constant at 20 oC    [mole(a-1).l(a-1)]
        ! KADS    R*4 1 - molar adsorption constant             [mole(a-1).l(a-1)]
        ! KSORP   R*4 1 I first order rate constant for adsorption           [1/d]
        ! FCAP    R*4 1 I adsorption capacity for Fe                      [gP/gFe]
        ! FRA     R*4 1 - correction factor for oxidised iron                  [-]
        ! FADS    R*4 1 O adsorption/desorption flux                     [gP/m3/d]
        ! CADST   R*4 1 - molar total concentration adsorption sites    [moleFe/l]
        ! CADS    R*4 1 - molar concentration free adsorption sites     [moleFe/l]
        ! IM1     R*4 1 I IM1 concentration                               [gDM/m3]
        ! IM2     R*4 1 I IM2 concentration                               [gDM/m3]
        ! IM3     R*4 1 I IM3 concentration                               [gDM/m3]
        ! TFE     R*4 1 - total Fe concentration                          [gFe/m3]
        ! FRFE1   R*4 1 I fraction of Fe in IM1                          [gFe/gDW]
        ! FRFE2   R*4 1 I fraction of Fe in IM2                          [gFe/gDW]
        ! FRFE3   R*4 1 I fraction of Fe in IM3                          [gFe/gDW]
        ! FRFEOX  R*4 1 I fraction of oxidised Fe                              [-]
        ! FIM1    R*4 1 O fraction of adsorbed P in IM1                        [-]
        ! FIM2    R*4 1 O fraction of adsorbed P in IM2                        [-]
        ! FIM3    R*4 1 O fraction of adsorbed P in IM3                        [-]
        ! PH      R*4 1 I pH                                                   [-]
        ! OH      R*4 1 - hydroxyl concentration                          [mole/l]
        ! AOH     R*4 1 I reaction constant for hydroxyl                       [-]
        ! TEMP    R*4 1 I temperature                                         [oC]
        ! TC      R*4 1 I temperature coeffcient adsorption constant           [-]
        ! OXY     R*4 1 I dissolved oxygen concentration                  [gO2/m3]
        ! CROXY   R*4 1 I critical dissolved oxygen concentration         [gO2/m3]
        ! POROS   R*4 1 I prorosity                                            [-]
        ! DELT    R*4 1 I DELWAQ timestep                                    [scu]
        ! SWADSP  R*4 1 I switch for selection of the adsorption option        [-]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT REAL (A-H, J-Z)
        IMPLICIT INTEGER (i)
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        INTEGER(kind = int_wp) :: IVERSN
        REAL(kind = real_wp) :: AAP, PO4, EQAAP, KDADS, KADS20, KADS, KSORP, &
                FCAP, FRA, FADS, CADST, CADS, IM1, IM2, &
                IM3, TFE, FRFE1, FRFE2, FRFE3, FRFEOX, FIM1, &
                FIM2, FIM3, PH, OH, AOH, TEMP, TC, &
                OXY, CROXY, POROS, DELT, SWADSP, QIM1, QIM2, &
                QIM3, EQAAPM
        INTEGER(kind = int_wp) :: NR_MES, ILUMON
        SAVE     NR_MES
        DATA     NR_MES / 0 /
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
        !
        IFLUX = 0
        !
        IVERSN = NINT(PMSA(IP23))
        !
        !     Use the old version when IVERSN=0
        !
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                IF (IVERSN == 0) THEN
                    !
                    dPAds = 0.0
                    EQAAP = 0.0
                    AtotP = 0.0
                    Kads = 0.0
                    !
                    SWAdsP = PMSA(IP1)
                    PO4 = MAX(PMSA(IP2), 0.0)
                    AAP = MAX(PMSA(IP3), 0.0)
                    IM1 = MAX(PMSA(IP4), 0.0)
                    IM2 = MAX(PMSA(IP5), 0.0)
                    IM3 = MAX(PMSA(IP6), 0.0)
                    RCADS = PMSA(IP12)
                    KD = PMSA(IP7)
                    MAXADS = PMSA(IP8)
                    DELT = PMSA(IP9)
                    !
                    TIM = IM1 + IM2 + IM3
                    QIM1 = 0.0
                    QIM2 = 0.0
                    QIM3 = 0.0
                    IF (TIM < 1E-10) THEN
                        FIM1 = 0.0
                        FIM2 = 0.0
                        FIM3 = 0.0
                    ELSE
                        FIM1 = IM1 / TIM
                        FIM2 = IM2 / TIM
                        FIM3 = IM3 / TIM
                        IF (IM1 > 1E-10) QIM1 = AAP * FIM1 / IM1
                        IF (IM2 > 1E-10) QIM2 = AAP * FIM2 / IM2
                        IF (IM3 > 1E-10) QIM3 = AAP * FIM3 / IM3
                    ENDIF
                    !
                    !------ Error messages
                    !
                    IF (KD<0.0) CALL write_error_message ('KD in ADSPO4 lower then zero')
                    IF (((ABS(KD) < 1E-20) .OR. (ABS(PO4)<1E-20)) &
                            .AND. (ABS(MAXADS) < 1E-20)) &
                            CALL write_error_message &
                                    ('(KD or PO4) and MAXADS equal zero in ADSPO4')
                    !
                    !------ (1) Instantanaeous equilibrium partitioning
                    !           SWAdsP = 0
                    !
                    IF (NINT(SWAdsP) == 0) THEN
                        dPAds = (((AAP + PO4) / (1. + KD)) - AAP) / DELT
                        EQAAP = -1.0
                    ENDIF
                    !
                    !------ (2) Kinetic Langmuir sorption
                    !           SWAdsP = 1
                    !
                    IF (NINT(SWAdsP) == 1) THEN
                        IF ((MAXADS < 1.E-10) .OR. (KD < 1.E-10)) THEN
                            EQAAP = 0.0
                            dPAds = 0.0
                        ELSE
                            EQAAP = TIM * (KD * PO4 * MAXADS) / (KD * PO4 + MAXADS)
                            dPAds = RCADS * (EQAAP - AAP)
                        ENDIF
                    ENDIF
                    !
                    !------ (3) GEM formulation for sorption
                    !           SWAdsP = 2
                    !
                    IF (NINT(SWAdsP) == 2) THEN
                        KAds20 = PMSA(IP10)
                        TCKAds = PMSA(IP11)
                        RCadsP = PMSA(IP25)
                        aOHPO4 = PMSA(IP13)
                        fr_Fe = PMSA(IP24)
                        frFeox = PMSA(IP17)
                        OXY = PMSA(IP18)
                        CrOXY = PMSA(IP19)
                        pH = PMSA(IP20)
                        Temp = PMSA(IP21)
                        poros = PMSA(IP22)
                        !
                        !--------- (3a) Eqs 6.38, 6.37 of GEM report
                        OH = 10.0**(pH - 14.0)
                        IF (OXY >= CrOXY * POROS) THEN
                            fOxSor = 1.0
                        ELSE
                            fOxSor = frFeox
                        ENDIF
                        !
                        !--------- (3b) Eqn 6.36 of GEM report : total sorption capacity (mol/l)
                        AtotP = fOxSor * fr_Fe * TIM / (56000.0 * poros)
                        !
                        !--------- (3c) Eqn 6.35 of GEM report : free sorption capacity (mol/l)
                        Afree = AtotP - (AAP / (31000.0 * poros))
                        !
                        !--------- (3d) Eqn 6.34 of GEM report : temp.correction of Kads
                        Kads = Kads20 * TCKads**(Temp - 20.0)
                        !
                        !--------- (3e) Eqn 6.33 of GEM report : equilibrium conc of Pads (gP/m3)
                        EQAAP = (AAP + PO4) * &
                                (1.0 - 1.0 / (Kads * Afree * (OH**aOHPO4) + 1.0))
                        !
                        !--------- (3f) Eqn 6.31 of GEM report : the adsorption flux (gP/m3/d)
                        dPads = RCAdsP * (EQAAP - AAP)
                        !
                    ENDIF
                    !
                    !---- Output of module
                    !
                    FL(1 + IFLUX) = dPAds
                    PMSA(IP26) = EQAAP
                    PMSA(IP27) = AtotP
                    PMSA(IP28) = Kads
                    PMSA(IP29) = FIM1
                    PMSA(IP30) = FIM2
                    PMSA(IP31) = FIM3
                    PMSA(IP32) = QIM1
                    PMSA(IP33) = QIM2
                    PMSA(IP34) = QIM3
                    !
                    !---- End active cells block
                    !
                ELSE
                    !
                    !     Use the new version when IVERSN=1
                    !
                    SWADSP = PMSA(IP1)
                    PO4 = MAX(PMSA(IP2), 0.0)
                    AAP = MAX(PMSA(IP3), 0.0)
                    IM1 = MAX(PMSA(IP4), 0.0)
                    IM2 = MAX(PMSA(IP5), 0.0)
                    IM3 = MAX(PMSA(IP6), 0.0)
                    KDADS = PMSA(IP7)
                    FCAP = PMSA(IP8)
                    DELT = PMSA(IP9)
                    KSORP = PMSA(IP12)
                    FRFE1 = PMSA(IP14)
                    FRFE2 = PMSA(IP15)
                    FRFE3 = PMSA(IP16)
                    !
                    !     Calculation of the total concentration of iron and
                    !     the fractions of adsorbed phosphate in the inorganic matter
                    !     fractions IM1-3
                    !
                    TFE = FRFE1 * IM1 + FRFE2 * IM2 + FRFE3 * IM3
                    QIM1 = 0.0
                    QIM2 = 0.0
                    QIM3 = 0.0
                    IF (TFE < 1E-10) THEN
                        FIM1 = 0.0
                        FIM2 = 0.0
                        FIM3 = 0.0
                    ELSE
                        FIM1 = FRFE1 * IM1 / TFE
                        FIM2 = FRFE2 * IM2 / TFE
                        FIM3 = FRFE3 * IM3 / TFE
                        IF (IM1 > 1E-10) QIM1 = AAP * FIM1 / IM1
                        IF (IM2 > 1E-10) QIM2 = AAP * FIM2 / IM2
                        IF (IM3 > 1E-10) QIM3 = AAP * FIM3 / IM3
                    ENDIF
                    !
                    !     Error messages
                    !
                    IF (KDADS < 0.0) CALL write_error_message ('KDADS in ADSPO4 negative')
                    IF (((ABS(KDADS) < 1E-20) .OR. (ABS(PO4) < 1E-20)) &
                            .AND. (ABS(FCAP) < 1E-20)) &
                            CALL write_error_message &
                                    ('(KDADS or PO4) and FCAP equal zero in ADSPO4')
                    !
                    !     Start the calculation of the sorption flux
                    !     Use one of three options
                    !
                    CADS = 0.0
                    CADST = 0.0
                    EQAAP = 0.0
                    FADS = 0.0
                    KADS = 0.0
                    !
                    !     SWADSP = 0 : Instantaneous equilibrium partitioning
                    !
                    IF (NINT(SWADSP) == 0) THEN
                        FADS = (((AAP + PO4) / (1.0 + KDADS)) - AAP) / DELT
                        EQAAP = -1.0
                    ENDIF
                    !
                    !     SWADSP = 1 : Langmuir sorption
                    !
                    IF (NINT(SWADSP) == 1) THEN
                        IF ((FCAP < 1E-10) .OR. (KDADS < 1E-10)) THEN
                            EQAAP = 0.0
                            FADS = 0.0
                        ELSE
                            CADST = FCAP * TFE
                            EQAAP = (KDADS * CADST * PO4) / (KDADS * PO4 + 1.0)
                            FADS = KSORP * (EQAAP - AAP)
                        ENDIF
                    ENDIF
                    !
                    !     SWADSP = 2 : pH dependent Langmuir sorption
                    !
                    IF (NINT(SWADSP) == 2) THEN
                        KADS20 = PMSA(IP10)
                        TC = PMSA(IP11)
                        AOH = PMSA(IP13)
                        FRFEOX = PMSA(IP17)
                        OXY = PMSA(IP18)
                        CROXY = PMSA(IP19)
                        PH = PMSA(IP20)
                        TEMP = PMSA(IP21)
                        POROS = PMSA(IP22)

                        IF (POROS < 1E-10) CALL write_error_message &
                                ('POROS in ADSPO4 equals zero')
                        IF (TFE < 1.E-10) THEN
                            IF (NR_MES < 25) THEN
                                CALL GETMLU(ILUMON)
                                NR_MES = NR_MES + 1
                                WRITE (ILUMON, *) 'WARNING :zero TFE in ADSPO4', &
                                        ' segment=', ISEG, ' TFE=', TFE
                            ENDIF
                            IF (NR_MES == 25) THEN
                                CALL GETMLU(ILUMON)
                                NR_MES = NR_MES + 1
                                WRITE(ILUMON, *) ' 25 WARNINGS on zero TFE'
                                WRITE(ILUMON, *) ' Further messages on algae surpressed'
                            ENDIF
                        ENDIF
                        !
                        !     Calculate pH dependency (hydroxyl) and factor for redox potential
                        !
                        OH = 10.0**(PH - 14.0)
                        IF (OXY >= (CROXY * POROS)) THEN
                            FRA = 1.0
                        ELSE
                            FRA = FRFEOX
                        ENDIF
                        !
                        !     Calculate total sorption capacity (mol/l)
                        !
                        CADST = FRA * TFE / (56000.0 * POROS)
                        !
                        !     Calculate free sorption capacity (mol/l)
                        !
                        CADS = MAX(0.0, CADST - (AAP / (31000.0 * POROS)))
                        !
                        !     Calculate temperature corrected KADS
                        !
                        KADS = KADS20 * TC**(TEMP - 20.0)
                        !
                        !     Calculate equilibrium concentration of adsorbed P (gP/m3)
                        !
                        IF (ABS(CADS) < 1.E-20) THEN
                            EQAAP = 0.0
                        ELSE
                            EQAAP = (AAP + PO4) / (1.0 + OH**AOH / (KADS * CADS))
                        ENDIF
                        !
                        !     Maximize EQAAP on equivalent CADST
                        !
                        IF (CADS < 0.0) THEN
                            EQAAPM = 0.9 * CADST * (31000.0 * POROS)
                            EQAAP = EQAAPM
                        ENDIF
                        !
                        !     Calculate the adsorption flux (gP/m3/d)
                        !
                        FADS = KSORP * (EQAAP - AAP)

                    ENDIF
                    !
                    !     Output of module
                    !
                    FL(1 + IFLUX) = FADS
                    PMSA(IP26) = EQAAP
                    PMSA(IP27) = CADST
                    PMSA(IP28) = KADS
                    PMSA(IP29) = FIM1
                    PMSA(IP30) = FIM2
                    PMSA(IP31) = FIM3
                    PMSA(IP32) = QIM1
                    PMSA(IP33) = QIM2
                    PMSA(IP34) = QIM3
                    !
                    !     End active cells block
                    !
                ENDIF
                !
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
            IP28 = IP28 + INCREM (28)
            IP29 = IP29 + INCREM (29)
            IP30 = IP30 + INCREM (30)
            IP31 = IP31 + INCREM (31)
            IP32 = IP32 + INCREM (32)
            IP33 = IP33 + INCREM (33)
            IP34 = IP34 + INCREM (34)
            !
        end do
        !
        RETURN
        !
    END

end module m_adspo4
