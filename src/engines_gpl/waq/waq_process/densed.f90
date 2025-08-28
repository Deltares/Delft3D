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
module m_densed
    use m_waq_precision

    implicit none

contains


    subroutine densed (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Denitrification in sediment

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            ----
        ! CRTEMP  R*4 1 I critical temperature for both processes             [xC]
        ! DEPTH   R*4 1 I depth                                                [m]
        ! DENR    R*4 1 I zeroth order denitrification rate              [gN/m2/d]
        ! DENRC   R*4 1 I firstt order denitrification rate                  [m/d]
        ! DENTC   R*4 1 I temperature coefficient for denitrif.                [-]
        ! FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
        ! NO3     R*4 1 I nitrate concentration                            [gN/m3]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! TEMPC   R*4 1 L temperatuur coefficient                              [-]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        LOGICAL  TMPOPT
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        IF (IN1 == 0 .AND. IN3 == 0 .AND. IN4 == 0 .AND. &
                IN5 == 0 .AND. IN6 == 0) THEN
            DENR = process_space_real(IP1)
            TEMP = process_space_real(IP5)
            CRTEMP = process_space_real(IP6)
            IF (TEMP <= CRTEMP) THEN
                TEMFAK = 0.0
            ELSE
                DENRC = process_space_real(IP3)
                DENTC = process_space_real(IP4)
                TEMP20 = TEMP - 20.0
                TEMFAK = DENRC * DENTC ** TEMP20
            ENDIF
            TMPOPT = .FALSE.
        ELSE
            TMPOPT = .TRUE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    IF (TMPOPT) THEN
                        DENR = process_space_real(IP1)
                        TEMP = process_space_real(IP5)
                        CRTEMP = process_space_real(IP6)
                        IF (TEMP <= CRTEMP) THEN
                            TEMFAK = 0.0
                        ELSE
                            DENRC = process_space_real(IP3)
                            DENTC = process_space_real(IP4)
                            TEMP20 = TEMP - 20.0
                            TEMFAK = DENRC * DENTC ** TEMP20
                        ENDIF
                    ENDIF
                    !
                    NO3 = MAX (0.0, process_space_real(IP2))
                    DEPTH = process_space_real(IP7)

                    !***********************************************************************
                    !**** Processes connected to the DENITRIFICATION
                    !***********************************************************************
                    !
                    !     Denitrification is assumed to take place in the sediment
                    !     Calculation of denitrification flux ( M.L-3.t-1)

                    FL(1 + IFLUX) = (DENR + TEMFAK * NO3) / DEPTH
                    !
                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            !
        end do
        !
        RETURN
        !
    END

end module m_densed
