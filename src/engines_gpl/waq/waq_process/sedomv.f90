!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

        implicit none

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        integer(kind = int_wp) :: iq, iseg, iflux, ikmrk1, ikmrk2, ivan, inaar, ikmrkv, ikmrkn
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, ip11, ip12, ip13, ip14
        integer(kind = int_wp) :: in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12, in13, in14
        REAL(kind = real_wp) :: sfl1, sfl2, sfl1s2, sfl2s2, q1, q2, depth, fompoc, fomphy, vspoc, vsphy, vsomi


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
                    SFL1S2 = process_space_real(IP3)
                    SFL2S2 = process_space_real(IP4)
                    Q1 = process_space_real(IP5)
                    Q2 = process_space_real(IP6)
                    DEPTH = process_space_real(IP7)

                    !***********************************************************************
                    !**** Processes connected to the SEDIMENTATION of OMV
                    !***********************************************************************

                    !     SEDIMENTATION
                    FL(1 + IFLUX) = (SFL1 * Q1 + SFL2 * Q2) / DEPTH
                    FL(2 + IFLUX) = (SFL1S2 * Q1 + SFL2S2 * Q2) / DEPTH

                    !     SEDIMENTATION SCALED
                    process_space_real(IP12) = FL(1 + IFLUX) * DEPTH
                    process_space_real(IP13) = FL(2 + IFLUX) * DEPTH

                ENDIF
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
            IP12 = IP12 + INCREM (12)
            IP13 = IP13 + INCREM (13)
            !
        end do
        !
        !.....Exchangeloop over de horizontale richting
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

            !........VxSedOMI op nul
            process_space_real(IP14) = 0.0

            IP14 = IP14 + IN14

        end do

        !.....Startwaarden VxSedPOC en VxSedPhyt
        IP10 = IP10 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN10
        IP11 = IP11 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN11

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

                    FOMPOC = process_space_real(IP8 + (IVAN - 1) * IN8)
                    FOMPHY = process_space_real(IP9 + (IVAN - 1) * IN9)

                    VSPOC = process_space_real(IP10)
                    VSPHY = process_space_real(IP11)

                    VSOMI = FOMPOC * VSPOC + FOMPHY * VSPHY
                    process_space_real(IP14) = VSOMI

                ELSEIF (IKMRKV==1.AND.IKMRKN==1) THEN

                    ! Water-water uitwisseling

                    FOMPOC = process_space_real(IP8 + (IVAN - 1) * IN8)
                    FOMPHY = process_space_real(IP9 + (IVAN - 1) * IN9)

                    VSPOC = process_space_real(IP10)
                    VSPHY = process_space_real(IP11)

                    ! Berekenen VxSedOMI

                    VSOMI = FOMPOC * VSPOC + &
                            FOMPHY * VSPHY

                    !..............VxSedOMI toekennen aan de process_space_real
                    process_space_real(IP14) = VSOMI

                ELSE
                    process_space_real(IP14) = 0.0
                ENDIF
            ELSE
                process_space_real(IP14) = 0.0
            ENDIF

            !........Exchangepointers ophogen
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP14 = IP14 + IN14

        end do

        RETURN
    END

end module m_sedomv
