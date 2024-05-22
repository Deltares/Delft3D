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
module m_cselac
    use m_waq_precision

    implicit none

contains


    subroutine cselac (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger

        !>\file
        !>       Consumption of electron acceptors (new, generic!)

        !
        !     Description of the module :
        !
        !     General water quality module for DELWAQ:
        !     The electron acceptors considered are dissolved oxygen,
        !     nitrate, sulphate and carbon monoxide (org. matter).
        !     The mineralisation fluxes of orgnaic matter fractions are added.
        !     The consumption fluxes follow from the total mineralisation flux
        !     and the relative contributions of the electron acceptors.
        !     The relative consumptions of electron-acceptors O2, NO3, SO4 and
        !     carbon monoxide (organic matter) are computed on the basis of
        !     MM-kinetics for limitation and inhibition. The reduction of carbon
        !     monoxide from organic matter results in methanogenesis.
        !     The process is valid for overlying water as well as sediment.
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                 Units
        ! ----    --- -  -    -------------------                          ----
        ! CFEA    R*4 1 I particulate amorphous oxidizing iron          [gFe/m3]
        ! CNI     R*4 1 I concentration of nitrate                       [gN/m3]
        ! CNIC    R*4 1 I critical nitrate conc. for inh. methanogen.    [gN/m3]
        ! COX     R*4 1 I dissolved oxygen concentration                  [g/m3]
        ! COXC1   R*4 1 I critical diss. ox. conc. for deniftrif.         [g/m3]
        ! COXC2   R*4 1 I critical diss. ox. conc. for inh. iron. red.    [g/m3]
        ! COXC3   R*4 1 I critical diss. ox. conc. for inh. sulph. red.   [g/m3]
        ! COXC4   R*4 1 I critical diss. ox. conc. for inh. methanogen.   [g/m3]
        ! CSU     R*4 1 I concentration of sulphate                      [gS/m3]
        ! CRTEMP  R*4 1 I critical temp. for denitr., sulphate red., meth.  [oC]
        ! DELT    R*4 1 I computational timestep                             [d]
        ! FCT2    R*4 1 I reduction factor for denitr. below crit. temp.    [oC]
        ! FCT3    R*4 1 I red. factor for iron red. below crit. temp.       [oC]
        ! FCT4    R*4 1 I red. factor for sulphate red. below crit. temp.   [oC]
        ! FCT5    R*4 1 I reduction factor for methanog. below crit. temp.  [oC]
        ! FDEN    R*4 1 I reduction factor for denitr. below crit. temp.    [oC]
        ! FIRED   R*4 1 I red. factor for iron red. below crit. temp.       [oC]
        ! FSRED   R*4 1 I red. factor for sulphate red. below crit. temp.   [oC]
        ! FMET    R*4 1 I reduction factor for methanog. below crit. temp.  [oC]
        ! FL (1)  R*4 1 O mineralisation flux for oxygen consumption   [gC/m3/d]
        ! FL (2)  R*4 1 O mineralisation flux for denitrification      [gC/m3/d]
        ! FL (3)  R*4 1 O mineralisation flux for sulphate reduction   [gC/m3/d]
        ! FL (4)  R*4 1 O mineralisation flux for methanogenesis       [gC/m3/d]
        ! FOX     R*4 1 L unscaled relative contribution of DO cons.         [-]
        ! FOX20   R*4 1 L unscaled relative contribution of DO cons. at 20oC [-]
        ! FROX    R*4 1 L scaled relative contribution of DO cons.           [-]
        ! FROXC   R*4 1 O corrected fractional oxygen contribution           [-]
        ! FNI     R*4 1 L unscaled relative contribution of denitrification  [-]
        ! FNI20   R*4 1 L unscaled relative contribution of denitr. at 20 oC [-]
        ! FRNI    R*4 1 L scaled relative contribution of denitrification    [-]
        ! FRNIC   R*4 1 O corrected fractional nitrate contribution          [-]
        ! FFE20   R*4 1 L unscaled relative contribution of iron re. at 20 oC[-]
        ! FFE     R*4 1 L unscaled relative contribution of iron reduction   [-]
        ! FRFE    R*4 1 L scaled relative contribution of iron reduction     [-]
        ! FRFEC   R*4 1 O corrected fractional iron contribution             [-]
        ! FSU     R*4 1 L unscaled relative contribution of sulph. reduction [-]
        ! FSU20   R*4 1 L unscaled relative contr. of sulph. red. at 20 oC   [-]
        ! FRSU    R*4 1 L scaled relative contribution of sulph. reduction   [-]
        ! FRSUC   R*4 1 O corrected fractional sulphate contribution         [-]
        ! FCH4    R*4 1 L unscaled relative contribution of methanogenesis   [-]
        ! FCH420  R*4 1 L unscaled relative contr. of methanog. at 20 oC     [-]
        ! FRCH4   R*4 1 L scaled relative contribution of methanogenesis     [-]
        ! FRCH4C  R*4 1 O corrected fractional methane contribution          [-]
        ! FSUM    R*4 1 L sum of unscaled relative contributions             [-]
        ! FRNI    R*4 1 O scaled relative contribution of nitrate            [-]
        ! KSOX    R*4 1 I half saturation constant for DO limitation      [g/m3]
        ! KSNI    R*4 1 I half saturation constant for NI limitation     [gN/m3]
        ! KSSU    R*4 1 I half saturation constant for SU limitation     [gS/m3]
        ! KSOXI   R*4 1 I half saturation constant for inhibition by DO   [g/m3]
        ! KSNISUI R*4 1 I half saturation constant for inhibition by NI  [gN/m3]
        ! KSSUI   R*4 1 I half saturation constant for inhibition by SU  [gS/m3]
        ! KTDEN   R*4 1 I temperature coefficient for denitrifcation         [-]
        ! KTMET   R*4 1 I temperature coefficient for methanogenesis         [-]
        ! KTMIN   R*4 1 I temperature coefficient for aerobic mineralisation [-]
        ! KTIRED  R*4 1 I temperature coefficient for iron reduction         [-]
        ! KTSRED  R*4 1 I temperature coefficient for sulphate reduction     [-]
        ! POROS   R*4 1 I porosity                                           [-]
        ! RDEN    R*4 1 L proposed mineralisation rate for denitrif.   [gC/m3/d]
        ! RDMAX   R*4 1 L maximal mineralisation rate for denitrif.    [gC/m3/d]
        ! RIRED   R*4 1 L proposed mineral. rate for iron red.         [gC/m3/d]
        ! RSRED   R*4 1 L proposed mineral. rate for sulphate red.     [gC/m3/d]
        ! RIMAX   R*4 1 L maximal mineral. rate for iron red.          [gC/m3/d]
        ! RSMAX   R*4 1 L maximal mineral. rate for sulphate red.      [gC/m3/d]
        ! RMIN1   R*4 1 I mineralisation flux of POC1                  [gC/m3/d]
        ! RMIN2   R*4 1 I mineralisation flux of POC2                  [gC/m3/d]
        ! RMIN3   R*4 1 I mineralisation flux of POC3                  [gC/m3/d]
        ! RMIN4   R*4 1 I mineralisation flux of POC4                  [gC/m3/d]
        ! RMIN5   R*4 1 I mineralisation flux of POC5                  [gC/m3/d]
        ! RMIN6   R*4 1 I mineralisation flux of DOC                   [gC/m3/d]
        ! RTMIN   R*4 1 I total mineralisation flux                    [gC/m3/d]
        ! TEMP    R*4 1 I ambient temperature                               [oC]
        ! TEMPC1  R*4 1 L ambient temp. corr. function for aerobic mineral.  [-]
        ! TEMPC2  R*4 1 L ambient temp. corr. function for denitrification   [-]
        ! TEMPC3  R*4 1 L ambient temp. corr. function for iron red.         [-]
        ! TEMPC4  R*4 1 L ambient temp. corr. function for sulphate red.     [-]
        ! TEMPC5  R*4 1 L ambient temp. corr. function for methanogenesis    [-]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)            [oC]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(43), INCREM(43), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        INTEGER(kind = int_wp) :: IP(43)
        INTEGER(kind = int_wp) :: IN(43)
        INTEGER(kind = int_wp) :: IFLUX, ISEG, ILUMON
        !
        REAL(kind = real_wp) :: COX, CNI, CFEA, CSU
        REAL(kind = real_wp) :: RMIN1, RMIN2, RMIN3, RMIN4, RTMIN, RDEN, &
                RSRED, RDMAX, RSMAX, DELT, RMIN5, RMIN6
        REAL(kind = real_wp) :: FOX, FNI, FSU, FCH4, FOX20, FNI20, &
                FSU20, FCH420, FSUM, FROX, FRNI, FRSU, &
                FRCH4, FROXC, FRNIC, FRSUC, FRCH4C, FRMCH4
        REAL(kind = real_wp) :: KSOX, KSOXI, KSNI, KSNISUI, KSSU, KSSUI, &
                KTMIN, KTDEN, KTSRED, KTMET, FCT2, FCT3, &
                FCT4, FDEN, FIRED, FSRED, FMET, COXC1, &
                COXC2, COXC3, COXC4, CNIC, KSFE, KSNIFEI
        REAL(kind = real_wp) :: KTIRED, FRFEC, FCT5, RIRED, RIMAX, FFE20, &
                FFE, FRFE
        REAL(kind = real_wp) :: POROS, TEMP, TEMPC1, TEMPC2, TEMPC3, TEMPC4, &
                TEMPC5, CRTEMP, TEMP20
        REAL(kind = real_wp) :: OXC_RATIO
        REAL(kind = real_wp) :: NIC_RATIO
        REAL(kind = real_wp) :: FEC_RATIO
        REAL(kind = real_wp) :: SUC_RATIO
        REAL(kind = real_wp) :: CHC_RATIO
        REAL(kind = real_wp) :: ROMAX
        REAL(kind = real_wp) :: ROXC
        LOGICAL  FIRST, ONLY_OX
        SAVE     FIRST, ONLY_OX
        DATA     FIRST /.TRUE./
        !
        CALL get_log_unit_number(ILUMON)
        !
        IN = INCREM
        IP = IPOINT
        !
        !     -----Warnings-----
        !
        IF (FIRST) THEN
            IF (PMSA(IP(11))<= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsOxCon should be greater than zero'
            ELSEIF (PMSA(IP(12))<= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsNiDen should be greater than zero'
            ELSEIF (PMSA(IP(13))<= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsFeRed should be greater than zero'
            ELSEIF (PMSA(IP(14)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsSuRed should be greater than zero'
            ELSEIF (PMSA(IP(15)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsOxDen should be greater than zero'
            ELSEIF (PMSA(IP(16)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsNiIRdInh should be greater than zero'
            ELSEIF (PMSA(IP(17)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsNiSRdInh should be greater than zero'
            ELSEIF (PMSA(IP(18)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : half saturation constant', &
                        ' KsSuMet should be greater than zero'
            ELSEIF (PMSA(IP(35)) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : Poros, PORS1 or PORS2', &
                        ' should be greater than zero'
            ENDIF
            IF (PMSA(IP(38)) > 0.5) THEN
                ONLY_OX = .TRUE.
            ELSE
                ONLY_OX = .FALSE.
            ENDIF
            FIRST = .FALSE.
        ENDIF
        !
        !     Do the segment loop.
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !           Read input. Adjust concentrations to zero, when negative.
                !           Adjust half saturation const. to prevent division by zero.
                !
                COX = MAX (0.0, PMSA(IP(1)))
                CNI = MAX (0.0, PMSA(IP(2)))
                CFEA = MAX (0.0, PMSA(IP(3)))
                CSU = MAX (0.0, PMSA(IP(4)))
                RMIN1 = PMSA(IP(5))
                RMIN2 = PMSA(IP(6))
                RMIN3 = PMSA(IP(7))
                RMIN4 = PMSA(IP(8))
                RMIN5 = PMSA(IP(9))
                RMIN6 = PMSA(IP(10))
                KSOX = MAX (1.0E-06, PMSA(IP(11)))
                KSNI = MAX (1.0E-06, PMSA(IP(12)))
                KSFE = MAX (1.0E-06, PMSA(IP(13)))
                KSSU = MAX (1.0E-06, PMSA(IP(14)))
                KSOXI = MAX (1.0E-06, PMSA(IP(15)))
                KSNIFEI = MAX (1.0E-06, PMSA(IP(16)))
                KSNISUI = MAX (1.0E-06, PMSA(IP(17)))
                KSSUI = MAX (1.0E-06, PMSA(IP(18)))
                KTMIN = PMSA(IP(19))
                KTDEN = PMSA(IP(20))
                KTIRED = PMSA(IP(21))
                KTSRED = PMSA(IP(22))
                KTMET = PMSA(IP(23))
                FDEN = PMSA(IP(24))
                FIRED = PMSA(IP(25))
                FSRED = PMSA(IP(26))
                FMET = PMSA(IP(27))
                COXC1 = PMSA(IP(28))
                COXC2 = PMSA(IP(29))
                COXC3 = PMSA(IP(30))
                COXC4 = PMSA(IP(31))
                CNIC = PMSA(IP(32))
                CRTEMP = PMSA(IP(33))
                TEMP = PMSA(IP(34))
                POROS = PMSA(IP(35))
                DELT = PMSA(IP(36))
                FRMCH4 = PMSA(IP(37))
                !
                !           Calculate the sum of the mineralisation fluxes
                !
                RTMIN = RMIN1 + RMIN2 + RMIN3 + RMIN4 + RMIN5 + RMIN6

                IF (ONLY_OX) THEN
                    FROXC = 1.
                    FRNIC = 0.
                    FRFEC = 0.
                    FRSUC = 0.
                    FRCH4C = 0.

                ELSE
                    !
                    !           Calculation of the temperature dependency coefficients
                    !
                    IF (TEMP >= CRTEMP) THEN
                        FCT2 = 1.0
                        FCT3 = 1.0
                        FCT4 = 1.0
                        FCT5 = 1.0
                    ELSE
                        FCT2 = FDEN
                        FCT3 = FIRED
                        FCT4 = FSRED
                        FCT5 = FMET
                    ENDIF
                    !
                    TEMP20 = TEMP - 20.0
                    TEMPC1 = KTMIN ** TEMP20
                    TEMPC2 = FCT2 * KTDEN ** TEMP20
                    TEMPC3 = FCT3 * KTIRED ** TEMP20
                    TEMPC4 = FCT4 * KTSRED ** TEMP20
                    TEMPC5 = FCT5 * KTMET ** TEMP20
                    !
                    !           Calculation of the unscaled (relative) contributions
                    !
                    FOX20 = COX / (KSOX * POROS + COX)
                    !
                    FNI20 = (CNI / (KSNI * POROS + CNI)) * &
                            (1 - COX / (KSOXI * POROS + COX))
                    !
                    FFE20 = (CFEA / (KSFE * POROS + CFEA)) * &
                            (1 - CNI / (KSNIFEI * POROS + CNI))
                    !
                    FSU20 = (CSU / (KSSU * POROS + CSU)) * &
                            (1 - CNI / (KSNISUI * POROS + CNI))
                    !
                    FCH420 = 1 - CSU / (KSSUI * POROS + CSU)
                    !
                    !           Adjust for temperature dependency.
                    !
                    FOX = FOX20 * TEMPC1
                    FNI = FNI20 * TEMPC2
                    FFE = FFE20 * TEMPC3
                    FSU = FSU20 * TEMPC4
                    FCH4 = FCH420 * TEMPC5
                    !
                    !           Correction of the unscaled contributions for too high
                    !           dissolved oxygen or nitrate concentrations to allow
                    !           sulphate reduction or methanogenesis.
                    !
                    IF (COX >= (COXC1 * POROS))  FNI = 0.0
                    IF (COX >= (COXC2 * POROS))  FFE = 0.0
                    IF (COX >= (COXC3 * POROS))  FSU = 0.0
                    IF (COX >= (COXC4 * POROS) .OR. &
                            CNI >= (CNIC * POROS))  FCH4 = 0.0
                    !
                    !           Calculation of the scaled contributions
                    !
                    FSUM = FOX + FNI + FFE + FSU + FCH4
                    FROX = FOX / FSUM
                    FRNI = FNI / FSUM
                    FRFE = FFE / FSUM
                    FRSU = FSU / FSUM
                    FRCH4 = 1 - (FROX + FRNI + FRFE + FRSU)
                    !
                    !           Calculate the maximal and proposed consumption fluxes in Carbon equivalents of
                    !           the electron acceptors oxygen, nitrate and sulphate
                    !           use a safety margin 0.5 on oxygen 0.9 on the rest
                    !
                    OXC_RATIO = 2.667
                    NIC_RATIO = 0.933
                    FEC_RATIO = 18.67
                    SUC_RATIO = 1.333
                    CHC_RATIO = 0.5
                    ROMAX = COX / OXC_RATIO / DELT * 0.5
                    RDMAX = CNI / NIC_RATIO / DELT * 0.9
                    RIMAX = CFEA / FEC_RATIO / DELT * 0.9
                    RSMAX = CSU / SUC_RATIO / DELT * 0.9
                    !
                    !           Correct scaled contributions for availability of electron
                    !           acceptors nitrate, oxygen, iron and sulphate
                    !
                    RDEN = FRNI * RTMIN
                    IF (RDEN > RDMAX .AND. RDEN > 0.0) THEN
                        FRNIC = FRNI * RDMAX / RDEN
                        FROX = FROX + FRNI - FRNIC
                    ELSE
                        FRNIC = FRNI
                    ENDIF
                    !
                    ROXC = FROX * RTMIN
                    IF (ROXC > ROMAX .AND. ROXC > 0.0) THEN
                        FROXC = FROX * ROMAX / ROXC
                        FRFE = FRFE + FROX - FROXC
                    ELSE
                        FROXC = FROX
                    ENDIF
                    !
                    RIRED = FRFE * RTMIN
                    IF (RIRED > RIMAX .AND. RIRED > 0.0) THEN
                        FRFEC = FRFE * RIMAX / RIRED
                        FRSU = FRSU + FRFE - FRFEC
                    ELSE
                        FRFEC = FRFE
                    ENDIF
                    !
                    RSRED = FRSU * RTMIN
                    IF (RSRED > RSMAX .AND. RSRED > 0.0) THEN
                        FRSUC = FRSU * RSMAX / RSRED
                    ELSE
                        FRSUC = FRSU
                    ENDIF
                    !
                    FRCH4C = 1 - (FROXC + FRNIC + FRFEC + FRSUC)

                    !           endif ONLY_OX
                ENDIF
                !
                !           Calculate the electron acceptor consumption fluxes
                !
                FL(1 + IFLUX) = FROXC * RTMIN
                FL(2 + IFLUX) = FRNIC * RTMIN
                FL(3 + IFLUX) = FRFEC * RTMIN
                FL(4 + IFLUX) = FRSUC * RTMIN
                FL(5 + IFLUX) = FRCH4C * FRMCH4 * RTMIN
                FL(6 + IFLUX) = FRCH4C * (1. - FRMCH4) * RTMIN
                !
                !           The corrected scaled contributions are output
                !
                PMSA(IP(39)) = FROXC
                PMSA(IP(40)) = FRNIC
                PMSA(IP(41)) = FRFEC
                PMSA(IP(42)) = FRSUC
                PMSA(IP(43)) = FRCH4C
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP = IP + IN
            !
        end do
        !
        RETURN
        !
    END

end module m_cselac
