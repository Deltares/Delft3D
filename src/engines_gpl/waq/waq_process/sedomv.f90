!!  Copyright (C)  Stichting Deltares, 2012-2025.
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


    subroutine sedomv (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

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

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
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
        DO ISEG = 1, num_cells
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==1) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    SFL1 = process_space_real(IP1)
                    SFL2 = process_space_real(IP2)
                    Q1 = process_space_real(IP3)
                    Q2 = process_space_real(IP4)
                    DEPTH = process_space_real(IP5)

                    !***********************************************************************
                    !**** Processes connected to the SEDIMENTATION of OMV
                    !***********************************************************************

                    !     SEDIMENTATION
                    FL(1 + IFLUX) = (SFL1 * Q1 + SFL2 * Q2) / DEPTH

                    !     SEDIMENTATION SCALED
                    process_space_real(IP10) = FL(1 + IFLUX) * DEPTH

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
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

            !........VxSedOMI op nul
            process_space_real(IP11) = 0.0

            IP11 = IP11 + IN11

        end do

        !.....Startwaarden VxSedPOC en VxSedPhyt
        IP8 = IP8 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN8
        IP9 = IP9 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN9

        !.....Exchangeloop over de verticale richting
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            IF (IVAN > 0 .AND. INAAR > 0) THEN

                ! Zoek eerste kenmerk van- en naar-segmenten

                CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
                CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
                IF (IKMRKV==1.AND.IKMRKN==3) THEN

                    ! Bodem-water uitwisseling: NUL FLUX OM OOK OUDE PDF's

                    FL(1 + (IVAN - 1) * NOFLUX) = 0.0

                    FOMPOC = process_space_real(IP6 + (IVAN - 1) * IN6)
                    FOMPHY = process_space_real(IP7 + (IVAN - 1) * IN7)

                    VSPOC = process_space_real(IP8)
                    VSPHY = process_space_real(IP9)

                    VSOMI = FOMPOC * VSPOC + FOMPHY * VSPHY
                    process_space_real(IP11) = VSOMI

                ELSEIF (IKMRKV==1.AND.IKMRKN==1) THEN

                    ! Water-water uitwisseling

                    FOMPOC = process_space_real(IP6 + (IVAN - 1) * IN6)
                    FOMPHY = process_space_real(IP7 + (IVAN - 1) * IN7)

                    VSPOC = process_space_real(IP8)
                    VSPHY = process_space_real(IP9)

                    ! Berekenen VxSedOMI

                    VSOMI = FOMPOC * VSPOC + &
                            FOMPHY * VSPHY

                    !..............VxSedOMI toekennen aan de process_space_real
                    process_space_real(IP11) = VSOMI

                ELSE
                    process_space_real(IP11) = 0.0
                ENDIF
            ELSE
                process_space_real(IP11) = 0.0
            ENDIF

            !........Exchangepointers ophogen
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP11 = IP11 + IN11

        end do

        RETURN
    END

end module m_sedomv
