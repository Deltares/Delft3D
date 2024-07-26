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
module m_restim
    use m_waq_precision

    implicit none

contains


    subroutine restim (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Residence time per volume, for advective transport only

        !
        !     Description of the module :
        !
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----

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

        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)

        !.....Zero the workspace
        DO ISEG = 1, num_cells

            process_space_real(IP2) = 0.0

            IP2 = IP2 + IN2

        end do

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)

        !.....Exchange loop
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            !........Bepaal het van- en naar- segment
            IFROM = IEXPNT(1, IQ)
            ITO = IEXPNT(2, IQ)

            FLOW = process_space_real(IP3)

            !........Absolute flows per segment sommeren in de workspace
            IF (IFROM > 0) THEN
                process_space_real (IP2 + (IFROM - 1) * IN2) = &
                        process_space_real (IP2 + (IFROM - 1) * IN2) + ABS(FLOW)
            ENDIF
            IF (ITO  > 0)  THEN
                process_space_real (IP2 + (ITO - 1) * IN2) = &
                        process_space_real (IP2 + (ITO - 1) * IN2) + ABS(FLOW)
            ENDIF

            !........Ophogen van de exchange-pointers
            IP3 = IP3 + IN3

        end do

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)

        !.....Segmentloop
        DO ISEG = 1, num_cells

            !........Niet-actieve segmenten afhandelen
            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK)
            IF (IKMRK == 0) THEN
                process_space_real(IP4) = -999.999
                GOTO 100
            ENDIF

            VOLUME = process_space_real(IP1)
            SOMFLW = process_space_real(IP2)

            !........Oneindige verblijftijden afhandelen
            IF (SOMFLW < 1.0E-20) THEN
                process_space_real(IP4) = 1.0E7
                GOTO 100
            ENDIF

            !........Bereken de verblijftijd
            RTIME = VOLUME / (SOMFLW / 2)

            !........Toekennen aan de process_space_real
            process_space_real(IP4) = RTIME

            100    CONTINUE

            !........Ophogen van de segment-pointers
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5

        end do

        RETURN
    END

end module m_restim
