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
module m_hdisp
    use m_waq_precision

    implicit none

contains


    subroutine hdisp  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       (1D) Horizontal dispersion as velocity dependent reprofunction

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE
        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7
        REAL(kind = real_wp) :: VELOC, CHEZY, WIDTH, TOTDEP, alfaK, &
                VELOCV, CHEZYV, WIDTHV, TOTDPV, alfaKV, &
                VELOCN, CHEZYN, WIDTHN, TOTDPN, alfaKN, &
                term1, term2, g, DVAR, MAXDSP, MAXDSN, MAXDSV
        INTEGER(kind = int_wp) :: IVAN, INAAR, IQ

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)

        !.....Exchangeloop over de verticale en breedterichtingen om
        !.....ze op 0 te zetten en over de verticale richting om deze
        !.....te initialiseren
        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir
            process_space_real(IP7) = 0.0
            IP7 = IP7 + IN7
        ENDDO

        !.....Exchangeloop over horizontale lengterichting
        IP7 = IPOINT(7)

        DO IQ = 1, num_exchanges_u_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            IF (IVAN>0.OR.INAAR>0) THEN

                IF (IVAN <= 0) IVAN = INAAR
                IF (INAAR <= 0) INAAR = IVAN

                VELOCV = process_space_real(IP1 + (IVAN - 1) * IN1)
                WIDTHV = process_space_real(IP2 + (IVAN - 1) * IN2)
                CHEZYV = process_space_real(IP3 + (IVAN - 1) * IN3)
                TOTDPV = process_space_real(IP4 + (IVAN - 1) * IN4)
                alfaKV = process_space_real(IP5 + (IVAN - 1) * IN5)
                MAXDSV = process_space_real(IP6 + (IVAN - 1) * IN6)

                VELOCN = process_space_real(IP1 + (INAAR - 1) * IN1)
                WIDTHN = process_space_real(IP2 + (INAAR - 1) * IN2)
                CHEZYN = process_space_real(IP3 + (INAAR - 1) * IN3)
                TOTDPN = process_space_real(IP4 + (INAAR - 1) * IN4)
                alfaKN = process_space_real(IP5 + (INAAR - 1) * IN5)
                MAXDSN = process_space_real(IP6 + (INAAR - 1) * IN6)

                VELOC = (VELOCV + VELOCN) / 2.
                WIDTH = (WIDTHV + WIDTHN) / 2.
                CHEZY = (CHEZYV + CHEZYN) / 2.
                TOTDEP = (TOTDPV + TOTDPN) / 2.
                alfaK = (alfaKV + alfaKN) / 2.
                MAXDSP = (MAXDSV + MAXDSN) / 2.

                g = 9.81
                term1 = VELOC * WIDTH ** 2 * CHEZY
                term2 = TOTDEP * g ** 0.5
                DVAR = alfaK * term1 / term2

                !
                ! Limit the horizontal dispersion, if the value of
                ! MAXDSP on at least one side is positive
                !
                IF (MAXDSV > 0.0 .AND. MAXDSN > 0.0) THEN
                    process_space_real(IP7) = MIN(DVAR, MAXDSP)
                ELSE
                    MAXDSP = MAX(MAXDSV, MAXDSN)
                    IF (MAXDSP > 0.0) THEN
                        process_space_real(IP7) = MIN(DVAR, MAXDSP)
                    ELSE
                        process_space_real(IP7) = DVAR
                    ENDIF
                ENDIF

            ENDIF

            IP7 = IP7 + IN7

        ENDDO

        RETURN
    END

end module m_hdisp
