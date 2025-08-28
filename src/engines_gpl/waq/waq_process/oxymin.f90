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
module m_oxymin
    use m_waq_precision

    implicit none

contains


    subroutine oxymin (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Potential daily mimimum dissolved oxygen concentration

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE POTENTIAL MINIMUM OXYGEN CONCENTRATION
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! OXY     R*4 1 I  oxygen concentration                          [gO2/m3]
        ! GREEN   R*4 1 I  concentration green algae                      [gC/m3]
        ! PGREEN  R*4 1 I  production green algae                           [1/d]
        ! RGREEN  R*4 1 I  respiration green algae                          [1/d]
        ! DIAT    R*4 1 I  concentration diatoms                          [gC/m3]
        ! PDIAT   R*4 1 I  production diatoms                               [1/d]
        ! RDIAT   R*4 1 I  respiration diatoms                              [1/d]
        ! CMINDO  R*4 1 O  minimum oxygen concentration                  [gO2/m3]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: iseg

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !

                OXY = process_space_real(IP1)
                GREEN = process_space_real(IP2)
                PGREEN = process_space_real(IP3)
                RGREEN = process_space_real(IP4)
                DIAT = process_space_real(IP5)
                PDIAT = process_space_real(IP6)
                RDIAT = process_space_real(IP7)
                DL = process_space_real(IP8)


                !     CALCULATE MINIMUM OXYGEN FROM PRODUCTION AND RESPIRATION
                !     CORRECT FOR DAY - NIGHT SEQUENCE
                !     original tentative estimate:
                !     CMINDO = OXY - 2.67 * (PROD - DL*RESP)
                !     replaced with the minimum of two estimates, one assuming
                !     occurrence or non-occurrence of compensation by reaeration
                !
                !     first calculate total gross production and respiration
                !
                PROD = (PGREEN + RGREEN) * GREEN + (PDIAT + RDIAT) * DIAT
                RESP = RGREEN * GREEN + RDIAT * DIAT
                !
                !     estimate without compensation
                !
                CMINDO1 = OXY - 0.5 * 2.67 * RESP * (1 - DL)
                !
                !     estimate with compensation on a daily average basis
                !
                CMINDO2 = OXY - 0.5 * 2.67 * PROD * (1 - DL)
                !
                !     take the minimal value of the two estimates
                !
                CMINDO = MIN (CMINDO1, CMINDO2)

                process_space_real(IP9) = CMINDO

            ENDIF

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
            !
        end do
        !
        RETURN
        !
    END

end module m_oxymin
