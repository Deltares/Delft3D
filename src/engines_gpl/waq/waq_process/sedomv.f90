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
module m_sedomv
    use m_waq_precision

    implicit none

contains


    subroutine sedomv (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

        !>\file
        !>       Sedimentation flux and velocity of adsorbed organic micro pollutants

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE DERIV-CONTRIBUTION FOR SEDIMENTATION OF OMV
        !        CALCULATES THE SEDIMENTATION VELOCITY OF OMV (DIRECTION 3)
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! SFL1-2  R*4 1 I  sedimentaion flux carriers                  [gC/m2/d]
        ! Q1-2    R*4 1 I  quality of carrier                          [gOMV/gC]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        integer(kind = int_wp) :: iq, iseg

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
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN
                CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    SFL1 = PMSA(IP1)
                    SFL2 = PMSA(IP2)
                    Q1 = PMSA(IP3)
                    Q2 = PMSA(IP4)
                    DEPTH = PMSA(IP5)

                    !***********************************************************************
                    !**** Processes connected to the SEDIMENTATION of OMV
                    !***********************************************************************

                    !     SEDIMENTATION
                    FL(1 + IFLUX) = (SFL1 * Q1 + SFL2 * Q2) / DEPTH

                    !     SEDIMENTATION SCALED
                    PMSA(IP10) = FL(1 + IFLUX) * DEPTH

                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP10 = IP10 + INCREM (10)
            !
        end do
        !
        !.....Exchangeloop over de horizontale richting
        DO IQ = 1, NOQ1 + NOQ2

            !........VxSedOMI op nul
            PMSA(IP11) = 0.0

            IP11 = IP11 + IN11

        end do

        !.....Startwaarden VxSedPOC en VxSedPhyt
        IP8 = IP8 + (NOQ1 + NOQ2) * IN8
        IP9 = IP9 + (NOQ1 + NOQ2) * IN9

        !.....Exchangeloop over de verticale richting
        DO IQ = NOQ1 + NOQ2 + 1, NOQ1 + NOQ2 + NOQ3 + NOQ4

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            IF (IVAN > 0 .AND. INAAR > 0) THEN

                ! Zoek eerste kenmerk van- en naar-segmenten

                CALL evaluate_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL evaluate_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==3) THEN

                    ! Bodem-water uitwisseling: NUL FLUX OM OOK OUDE PDF's

                    FL(1 + (IVAN - 1) * NOFLUX) = 0.0

                    FOMPOC = PMSA(IP6 + (IVAN - 1) * IN6)
                    FOMPHY = PMSA(IP7 + (IVAN - 1) * IN7)

                    VSPOC = PMSA(IP8)
                    VSPHY = PMSA(IP9)

                    VSOMI = FOMPOC * VSPOC + FOMPHY * VSPHY
                    PMSA(IP11) = VSOMI

                ELSEIF (IKMRKV==1.AND.IKMRKN==1) THEN

                    ! Water-water uitwisseling

                    FOMPOC = PMSA(IP6 + (IVAN - 1) * IN6)
                    FOMPHY = PMSA(IP7 + (IVAN - 1) * IN7)

                    VSPOC = PMSA(IP8)
                    VSPHY = PMSA(IP9)

                    ! Berekenen VxSedOMI

                    VSOMI = FOMPOC * VSPOC + &
                            FOMPHY * VSPHY

                    !..............VxSedOMI toekennen aan de PMSA
                    PMSA(IP11) = VSOMI

                ELSE
                    PMSA(IP11) = 0.0
                ENDIF
            ELSE
                PMSA(IP11) = 0.0
            ENDIF

            !........Exchangepointers ophogen
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP11 = IP11 + IN11

        end do

        RETURN
    END

end module m_sedomv
