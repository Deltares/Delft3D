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
module m_ptewor
    use m_waq_precision

    implicit none

contains


    subroutine ptewor (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Production fluxes for TEWOR+

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Production fluxes for TEWOR
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! FBOD           I    TEWOR production flux of CBOD5            (gO2/m3/d)
        ! FBOD2          I    TEWOR production flux of CBOD5_2          (gO2/m3/d)
        ! FBOD3          I    TEWOR production flux of CBOD5_3          (gO2/m3/d)
        ! FCOD           I    TEWOR production flux of COD_Cr           (gO2/m3/d)
        ! FOXY           I    TEWOR production flux of OXY              (gO2/m3/d)
        ! FORGN          I    TEWOR production flux of Org-N             (gN/m3/d)
        ! FOON           I    TEWOR production flux of OON               (gN/m3/d)
        ! FNH4           I    TEWOR production flux of NH4               (gN/m3/d)
        ! FNO3           I    TEWOR production flux of NO3               (gN/m3/d)
        ! FECOLI         I    TEWOR production flux of EColi            (MPN/ml/d)
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     local declarations
        !
        INTEGER(kind = int_wp) :: IFLUX, ISEG, &
                IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
        REAL(kind = real_wp) :: FBOD, FBOD2, FBOD3, FCOD, FOXY, FORGN, FNH4, FNO3, &
                FECOLI, FOON

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
        !
        IFLUX = 0
        DO ISEG = 1, num_cells
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                FBOD = process_space_real(IP1)
                FBOD2 = process_space_real(IP2)
                FBOD3 = process_space_real(IP3)
                FCOD = process_space_real(IP4)
                FOXY = process_space_real(IP5)
                FORGN = process_space_real(IP6)
                FNH4 = process_space_real(IP7)
                FNO3 = process_space_real(IP8)
                FOON = process_space_real(IP9)
                FECOLI = process_space_real(IP10)

                FL(1 + IFLUX) = FBOD
                FL(2 + IFLUX) = FBOD2
                FL(3 + IFLUX) = FBOD3
                FL(4 + IFLUX) = FCOD
                FL(5 + IFLUX) = FOXY
                FL(6 + IFLUX) = FORGN
                FL(7 + IFLUX) = FNH4
                FL(8 + IFLUX) = FNO3
                FL(9 + IFLUX) = FOON

                !     Conversion from MPN/ml/d to MPN/m3/d
                FL(10 + IFLUX) = FECOLI * 1E6

            ENDIF
            !     ENDIF
            !
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
            !
        end do
        !
        RETURN
        !
    END

end module m_ptewor
