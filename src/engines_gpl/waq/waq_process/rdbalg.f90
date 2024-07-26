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
module m_rdbalg
    use m_waq_precision

    implicit none

contains


    subroutine rdbalg (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper

        !>\file
        !>       Light efficiency function DYNAMO algae

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        ! DEPTH   R*4 1 I depth of the water column                            [
        ! EFF     R*4 1 L average light efficiency green-algea                 [
        ! ACTRAD  R*4 1 I radiation                                         [W/m
        ! SATRAD  R*4 1 I radiation growth saturation green-algea           [W/m

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        LOGICAL  LGTOPT
        integer(kind = int_wp) :: iseg
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        !
        IF (IN2 == 0 .AND. IN3 == 0 .AND. IN5 == 0) THEN
            ACTRAD = process_space_real(IP2)
            SATRAD = process_space_real(IP3)
            TFGRO = process_space_real(IP5)
            !
            !        Correct SATRAD for temperature using Temp function for growth
            !
            !        SATRAD = TFGRO * SATRAD
            SATRAD = SATRAD
            !     actuele straling / straling voor groei verzadiging
            FRAD = ACTRAD / SATRAD
            LGTOPT = .FALSE.
        ELSE
            LGTOPT = .TRUE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                IF (LGTOPT) THEN
                    ACTRAD = process_space_real(IP2)
                    SATRAD = process_space_real(IP3)
                    TFGRO = process_space_real(IP5)
                    !
                    !        Correct SATRAD for temperature using Temp function for growth
                    !
                    !        SATRAD = TFGRO * SATRAD
                    SATRAD = SATRAD
                    !     actuele straling / straling voor groei verzadiging
                    FRAD = ACTRAD / SATRAD
                ENDIF
                !
                process_space_real(IP6) = MAX(MIN(FRAD, 1.0), 0.0)
                !
                IF (SATRAD < 1E-20)  CALL write_error_message ('SATRAD in RADALG zero')

                8900 CONTINUE
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP5 = IP5 + IN5
            IP4 = IP4 + IN4
            IP6 = IP6 + IN6
            !
        end do
        !
        RETURN
        !
    END

end module m_rdbalg
