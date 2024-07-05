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
module m_degmp
    use m_waq_precision

    implicit none

contains


    subroutine degmp  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_extract_waq_attribute

        !>\file
        !>       Degradation of organic micropolutants (new, generic!)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Overall degradation of organic micropollutants in the
        !        water and sediment.
        !
        !         ----- old version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CRTEMP  R*4 1 I critical temperature for decay                       [xC]
        ! DEPTH   R*4 1 I actual depth of a segment                            [m]
        ! FL (1)  R*4 1 O decay flux mixing layer (x=C,N,P,Si)           [gX/m3/d]
        ! MINRC   R*4 1 I first order decay rate                             [1/d]
        ! FDIS    R*4 1 I fraction free dissolved mive                         [-]
        ! MINTCR  R*4 1 I temperature coefficient two bottom layers          [1/d]
        ! ORG     R*4 1 I amount decaying organic material in mixing layer    [gX]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! TEMPC   R*4 1 L temperature coefficient                              [-]
        ! VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
        ! ZERMIN  R*4 1 I zeroth order decay rate mixing layer           [gX/m2/d]
        !
        !         ----- new version -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! IVERSN  I   1 I option for version of process                        [-]
        ! ISWOXY  I   1 I option for oxidising or reducing condition           [-]
        ! ISWDEG  I   1 I option for fraction micropoll. that is degraded      [-]
        ! CRTEMP  R*4 1 I critical temperature for decay                      [oC]
        ! DEPTH   R*4 1 I actual depth of a water segment                      [m]
        ! FL (1)  R*4 1 O decay flux (x = C, N, P, Si)                   [gX/m3/d]
        ! FTOTR   R*4 1 - fraction of micropollutant that is gedraded          [-]
        ! FDFREE  R*4 1 I fraction free dissolved micropollutant               [-]
        ! FDDOC   R*4 1 I fraction DOC-bound micropollutant                    [-]
        ! KDEG    R*4 1 - first order degradation rate                       [1/d]
        ! KDEGO   R*4 1 I first order degradation rate at oxidising cond.    [1/d]
        ! KDEGR   R*4 1 I first order degradation rate at reducing cond.     [1/d]
        ! KTDEG   R*4 1 I temperature coefficient for decay                    [-]
        ! ORGMP   R*4 1 I concentration organic micropollutant             [gX/m3]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L ambient temperature - reference temp.(20)           [oC]
        ! TEMPC   R*4 1 L temperature coefficient                              [-]
        ! VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
        ! ZDEGMP  R*4 1 I zero order degradation rate                    [gX/m3/d]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2
        REAL(kind = real_wp) :: ZERMIN, ORG, FDIS, MINRC, MINTC
        INTEGER(kind = int_wp) :: IVERSN, ISWOXY, ISWDEG
        REAL(kind = real_wp) :: ZDEGMP, ORGMP, FDFREE, FDDOC, KDEGO, KDEGR, KTDEG, &
                KDEG, FTOTR
        REAL(kind = real_wp) :: TEMP, CRTEMP, TEMPC, TEMP20, VOLUME, DEPTH
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
        !
        !     Check sediment switch for first segment
        !
        SEDIME = .FALSE.
        IF (PMSA(IP16) > 0.5) SEDIME = .TRUE.
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0.OR.IKMRK2==3).OR..NOT.SEDIME) THEN
                    IVERSN = NINT(PMSA(IP10))
                    !
                    !     Use old version when IVERSN=0
                    !
                    IF (IVERSN == 0) THEN
                        !
                        ZERMIN = PMSA(IP1)
                        ORG = MAX (0.0, PMSA(IP2))
                        FDIS = PMSA(IP3)
                        MINRC = PMSA(IP4)
                        MINTC = PMSA(IP5)
                        TEMP = PMSA(IP6)
                        CRTEMP = PMSA(IP7)
                        VOLUME = PMSA(IP8)
                        DEPTH = PMSA(IP9)
                        !
                        !        Calculate the degradation flux
                        !
                        IF (TEMP <= CRTEMP) THEN
                            !
                            !           Only the zero order term
                            !
                            IF (SEDIME) THEN
                                FL(1 + IFLUX) = ZERMIN / DEPTH
                            ELSE
                                FL(1 + IFLUX) = ZERMIN
                            ENDIF
                            !
                        ELSE
                            !
                            !           Sum of zero and first order terms
                            !
                            TEMP20 = TEMP - 20.0
                            TEMPC = MINTC ** TEMP20
                            !
                            IF (SEDIME) THEN
                                FL(1 + IFLUX) = (ZERMIN / DEPTH + &
                                        FDIS * MINRC * TEMPC * ORG / DEPTH)
                            ELSE
                                FL(1 + IFLUX) = ZERMIN + FDIS * MINRC * TEMPC * ORG
                            ENDIF

                        ENDIF
                        !
                        !     Use new version when IVERSN=1
                        !
                    ELSE
                        !
                        ZDEGMP = PMSA(IP1)
                        ORGMP = MAX (0.0, PMSA(IP2))
                        FDFREE = PMSA(IP3)
                        KTDEG = PMSA(IP5)
                        TEMP = PMSA(IP6)
                        CRTEMP = PMSA(IP7)
                        VOLUME = PMSA(IP8)
                        DEPTH = PMSA(IP9)
                        ISWOXY = NINT(PMSA(IP11))
                        ISWDEG = NINT(PMSA(IP12))
                        FDDOC = PMSA(IP13)
                        KDEGO = PMSA(IP14)
                        KDEGR = PMSA(IP15)
                        !
                        !        Calculate the degradation flux
                        !
                        IF (TEMP <= CRTEMP) THEN
                            !
                            !           Only the zero order term
                            !
                            IF (SEDIME) THEN
                                FL(1 + IFLUX) = ZDEGMP / DEPTH
                            ELSE
                                FL(1 + IFLUX) = ZDEGMP
                            ENDIF
                            !
                        ELSE
                            !
                            !           Sum of zero and first order terms
                            !
                            TEMP20 = TEMP - 20.0
                            TEMPC = KTDEG ** TEMP20
                            !
                            !           Select rate for oxidising conditions (ISWOXY = 1)
                            !                        or reducing conditions (ISWOXY = 0)
                            !
                            KDEG = KDEGO
                            IF (ISWOXY == 0) KDEG = KDEGR
                            !
                            !           Select the fractions that are degraded,
                            !           total (ISWDEG = 0), free dissolved (ISWDEG = 1),
                            !           free dissolved plus DOC-bound (ISWDEG = 2)
                            !
                            IF (ISWDEG == 1) THEN
                                FTOTR = FDFREE
                            ELSE IF (ISWDEG == 2) THEN
                                FTOTR = FDFREE + FDDOC
                            ELSE
                                FTOTR = 1.0
                            ENDIF
                            !
                            IF (SEDIME) THEN
                                FL(1 + IFLUX) = ZDEGMP / DEPTH + &
                                        KDEG * TEMPC * FTOTR * ORGMP / DEPTH
                            ELSE
                                FL(1 + IFLUX) = ZDEGMP + KDEG * TEMPC * FTOTR * ORGMP
                            ENDIF
                            !
                        ENDIF
                        !
                    ENDIF
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
            !     IP16 wordt niet opgehoogd, want deze staat buiten segmentloop!!!
            !
        end do
        !
        RETURN
        !
    END

end module m_degmp
