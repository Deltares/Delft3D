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
module m_rfpart
    use m_waq_precision

    implicit none

contains


    subroutine rfpart (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger, only : terminate_execution, get_log_unit_number

        !>\file
        !>       Reprofunctions for HM partition coefficients

        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! ALK     R*4 1 I     alkalinity                                 [mole/m3]
        ! CCL     R*4 1 I     chloride concentration                      [gCl/m3]
        ! CECIM1  R*4 1 I     cation exchange capacity of IM1            [eq/kgDW]
        ! CECIM2  R*4 1 I     cation exchange capacity of IM2            [eq/kgDW]
        ! CECIM3  R*4 1 I     cation exchange capacity of IM3            [eq/kgDW]
        ! DOC     R*4 1 I     dissolve organic carbon concentration        [gC/m3]
        ! KPIM1   R*4 1 O     partition coefficient for IM1              [m3/kgDW]
        ! KPIM2   R*4 1 O     partition coefficient for IM2              [m3/kgDW]
        ! KPIM3   R*4 1 O     partition coefficient for IM3              [m3/kgDW]
        ! KP0     R*4 1 -     reference partition coefficient            [m3/kgDW]
        ! PH      R*4 1 I     acidity                                          [-]
        ! AC      R*4 1 I     coefficient a for reprofunction            [various]
        ! BC      R*4 1 I     coefficient b for reprofunction            [various]
        ! CC      R*4 1 I     coefficient c for reprofunction            [various]
        ! DC      R*4 1 I     coefficient d for reprofunction            [various]
        ! GC      R*4 1 I     coefficient g for reprofunction            [various]
        ! LC      R*4 1 I     coefficient l for reprofunction            [various]
        ! MC      R*4 1 I     coefficient m for reprofunction            [various]
        ! NC      R*4 1 I     coefficient n for reprofunction            [various]
        ! OC      R*4 1 I     coefficient o for reprofunction            [various]
        ! IVERSN  I*4 1 -     option parameter for reprofunction               [-]
        !                     (0=no repro, 1=Rine repro, 2=North Sea repro)
        !
        !     Logical Units : -
        !
        !     Modules called : -
        !
        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        !     declaration of arguments
        !
        INTEGER(kind = int_wp) :: NOSEG, NOFLUX, NOQ1, NOQ2, NOQ3, NOQ4
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), &
                IEXPNT(4, *), IKNMRK(*)
        REAL(kind = real_wp) :: PMSA(*), FL(*)
        !
        !     local declarations
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, &
                IP19, IP20
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        !
        INTEGER(kind = int_wp) :: IVERSN
        INTEGER(kind = int_wp) :: LUNREP
        !
        REAL(kind = real_wp) :: PH, ALK, CCL, DOC, &
                CECIM1, CECIM2, CECIM3, &
                AC, BC, CC, DC, LC, GC, &
                MC, NC, OC, &
                LOGALK, LOGCCL, LOGDOC, LOGKP0
        !
        REAL(kind = real_wp) :: KPIM1, KPIM2, KPIM3, KP0
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
        !
        IFLUX = 0
        !
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                PH = PMSA(IP1)
                ALK = PMSA(IP2)
                CCL = PMSA(IP3)
                DOC = PMSA(IP4)
                CECIM1 = PMSA(IP5)
                CECIM2 = PMSA(IP6)
                CECIM3 = PMSA(IP7)
                AC = PMSA(IP8)
                BC = PMSA(IP9)
                CC = PMSA(IP10)
                DC = PMSA(IP11)
                GC = PMSA(IP13)
                LC = PMSA(IP12)
                MC = PMSA(IP14)
                NC = PMSA(IP15)
                OC = PMSA(IP16)
                IVERSN = NINT (PMSA(IP17))
                !
                IF (ALK < 1.0) ALK = 1.0
                IF (CCL < 1.0) CCL = 1.0
                IF (DOC < 0.1) DOC = 0.1
                !
                !           Calculation of partition coefficients depending on switch
                !
                IF (IVERSN == 1) THEN

                    !              "Rhine" function

                    LOGALK = LOG10(ALK)
                    LOGCCL = LOG10(CCL)
                    LOGDOC = LOG10(DOC)
                    LOGKP0 = AC + BC * PH + CC * (PH**2) + DC * LOGALK + GC * LOGCCL + &
                            LC * LOGDOC + MC * PH * LOGALK + NC * PH * LOGALK * LOGALK + &
                            OC * PH * PH * LOGALK
                    KP0 = 10.0**LOGKP0

                    KPIM1 = KP0 * (CECIM1 / 0.0002)
                    KPIM2 = KP0 * (CECIM2 / 0.0002)
                    KPIM3 = KP0 * (CECIM3 / 0.0002)
                    !
                ELSEIF (IVERSN == 2) THEN

                    !              "North Sea" function

                    KP0 = (10.0**AC) * (10.0**(BC * PH)) * &
                            ((1800. * CCL + CC)**DC)
                    KPIM1 = KP0 * CECIM1 * 1000.
                    KPIM2 = KP0 * CECIM2 * 1000.
                    KPIM3 = KP0 * CECIM3 * 1000.
                    !
                ELSE

                    !              switch for function out of range

                    CALL get_log_unit_number(LUNREP)
                    WRITE(LUNREP, *) 'ERROR in RFPART'
                    WRITE(LUNREP, *) 'Illegal option for repro function partition coefficient'
                    WRITE(LUNREP, *) 'Option in input:', IVERSN
                    WRITE(*, *) ' ERROR in RFPART'
                    WRITE(*, *) ' Illegal option for repro function partition coefficient'
                    WRITE(*, *) ' Option in input:', IVERSN
                    CALL terminate_execution(1)

                ENDIF
                !
                !           Output of module
                !
                PMSA(IP18) = KPIM1
                PMSA(IP19) = KPIM2
                PMSA(IP20) = KPIM3
                !
                !        End active cells block
                !
            ENDIF

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
            !
        ENDDO
        !
        RETURN
        !
    END

end module m_rfpart
