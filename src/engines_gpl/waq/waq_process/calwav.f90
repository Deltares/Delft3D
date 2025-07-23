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
module m_calwav
    use m_waq_precision

    implicit none

contains


    subroutine calwav (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message
        use m_extract_waq_attribute

        !>\file
        !>       Wave characteristics

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATE WAVE CHARACTERISTICS FROM EMIPRICAL FORMULAES
        !        MODEL IS VERTICALLY AVERAGED.
        !
        !        AVERAGED MODELS
        !
        ! Name    T   L I/O  Description                              Units
        ! ----    --- -  -   -------------------                      ----
        ! DEPTH   R   1  I   Water depth                                       [m]
        ! DS      R   1  L   Standaard deptch                                  [-]
        ! FETCH   R   1  I   Fetch                                             [m]
        ! FS      R   1  L   Standaard fetch                                   [-]
        ! G       R   1  I   Acceleration of gravity                        [m/s2]
        ! RHOW    R   1  I   Density of water                              [kg/m3]
        ! VWIND   R   1  I   Wind velocity at 10 m                           [m/s]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declarations, constants in source
        !
        PARAMETER (G = 9.8, &
                RHOW = 1000.0, &
                PI = 3.14159265)
        REAL(kind = real_wp) :: INIDEP
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF (IKMRK2==0 .OR. IKMRK2==3) THEN
                    !
                    VWIND = process_space_real(IP1)
                    FETCH = process_space_real(IP2)
                    DEPTH = process_space_real(IP3)
                    INIDEP = process_space_real(IP4)

                    IF (FETCH < 1E-20)  CALL write_error_message ('FETCH in CALWAVE zero')

                    !     Initialisation
                    H = 0.0
                    RL = 0.0
                    T = 0.0

                    !     Check if user wants to use value of inidepth (pos values only)
                    IF (INIDEP < 0.0) INIDEP = DEPTH

                    !     Shear stress by wind
                    IF (VWIND < 0.0001) GOTO 150

                    !     Dimensionless fetch length.
                    FS = G * FETCH / VWIND**2

                    !     Dimensionless depth.
                    DS = G * INIDEP / VWIND**2

                    !     Calculate wave height H.
                    A1 = 0.710 * (DS**0.763)
                    A2 = 0.855 * (DS**0.365)
                    A3 = 0.0150 * (FS**0.450) / TANH(A1)
                    A4 = 0.0345 * (FS**0.370) / TANH(A2)
                    HS = 0.240 * TANH(A1) * TANH(A3)
                    TS = 2.0 * PI * TANH(A2) * TANH(A4)

                    H = HS * (VWIND * VWIND) / G

                    !     Calculate wave period T
                    T = TS * VWIND / G

                    !     Initialzie variables to calculate the wave length using iteration.
                    RL0 = G * T * T / (2.0 * PI)
                    A5 = 2.0 * PI * INIDEP / RL0

                    !     Calculation of wave length RL. Iterate to find an accurate approximation.
                    RL = RL0
                    do i = 1, 15
                        RL = RL0 * TANH(A5)
                        A5 = 2.0 * PI * INIDEP / RL
                    enddo

                    150 CONTINUE
                    !
                    process_space_real (IP5) = H
                    process_space_real (IP6) = RL
                    process_space_real (IP7) = T
                    !
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
            !
        end do
        !

        RETURN
    END

end module m_calwav
