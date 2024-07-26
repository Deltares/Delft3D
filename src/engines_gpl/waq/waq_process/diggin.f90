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
module m_diggin
    use m_waq_precision

    implicit none

contains


    subroutine diggin (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Digging dry matter to sediment S1 and S2

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! ACTHS1  R*4 1 I  actual thickness of S1                              [m]
        ! ACTHS2  R*4 1 I  actual thickness of S2                              [m]
        ! DMS2    R*4 1 I  dry matter in S1                                    [g]
        ! DMS3    R*4 1 I  dry matter in S1                                    [g]
        ! DELT    R*4 1 I  DELWAQ timestep                                   [scu]
        ! FIXS1   R*4 1 I  fixed thickness of layer S1 (option fixed)          [m]
        ! FIXS2   R*4 1 I  fixed thickness of layer S2 (option fixed)          [m]
        ! FL (1)  R*4 1 O  digging flux S2->S1                          [gDM/m3/d]
        ! FL (2)  R*4 1 O  digging flux out system (S3) ->S2            [gDM/m3/d]
        ! IAUSYS  R*4 1 I  ratio between auxiliary and system clock unit       [-]
        ! MAXDS1  R*4 1 L  max. digging flux layer S1                   [gDM/m2/d]
        ! MAXDS2  R*4 1 L  max. digging flux layer S2                   [gDM/m2/d]
        ! SOMRES  R*4 1 I  total sedimentation flux                     [gDM/m2/d]
        ! SW      R*4 1 I  swithc for digging and digging option               [-]
        ! VDIG    R*4 1 I  first order digging rate constant                 [1/d]
        ! SURF    R*4 1 I  surface area                                       [m2]
        ! ZERDIG  R*4 1 I  zeroth order digging flux                    [gDM/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
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
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN

                    SOMRES = process_space_real(IP1)
                    ZDIGS1 = process_space_real(IP2)
                    ZDIGS2 = process_space_real(IP3)
                    ACTHS1 = process_space_real(IP4)
                    ACTHS2 = process_space_real(IP5)
                    ISW = process_space_real(IP6) + 0.5
                    FIXS1 = process_space_real(IP7)
                    FIXS2 = process_space_real(IP8)
                    RHOS1 = process_space_real(IP9)
                    RHOS2 = process_space_real(IP10)
                    PORS1 = process_space_real(IP11)
                    PORS2 = process_space_real(IP12)
                    DELT = process_space_real(IP13)
                    SURF = process_space_real(IP14)

                    !*******************************************************************************
                    !**** Processes connected to the BURIAL of dry matter
                    !***********************************************************************

                    DIGS1 = 0.0
                    DIGS2 = 0.0

                    ! --- First option (fixed layer thickness)
                    !     no maxima for digging flux in this option!
                    IF (ISW == 0) THEN
                        DIGS1 = SOMRES + &
                                ((FIXS1 - ACTHS1) * RHOS1 * (1.0 - PORS1)) / DELT

                        IF (ACTHS1 >= FIXS1) DIGS1 = SOMRES

                        DIGS2 = DIGS1 + &
                                ((FIXS2 - ACTHS2) * RHOS2 * (1.0 - PORS2)) / DELT

                        IF (ACTHS2 >= FIXS2)  DIGS2 = DIGS1

                    ENDIF

                    ! --- Second  option (variable layer thickness, with a maximum thickness)
                    !     Determine max digging flux for S1 (=amount in S2)
                    !     amount in S3 assumed infinite

                    !     Amount of dry matter in layer S2:
                    DMS2 = ACTHS2 * SURF * RHOS2 * (1 - PORS2)

                    !     Maximum upward transport towards layer S1 (=amount in layer S2)
                    MAXDS1 = DMS2 / DELT / SURF

                    IF (ISW == 1) THEN

                        DIGS1 = MIN (ZDIGS1, MAXDS1)
                        DIGS2 = ZDIGS2

                    ENDIF

                    process_space_real (IP15) = DIGS1
                    process_space_real (IP16) = DIGS2

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
            !
        end do
        !
        RETURN
        !
    END

end module m_diggin
