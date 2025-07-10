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
module m_nlalg
    use m_waq_precision

    implicit none

contains


    subroutine nlalg  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper

        !>\file
        !>       Nutrient limiation function for DYNAMO algae

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        !
        ! DIN     R*4 1 L consumable-dissolved inorganic nitrogen          [gN/m
        ! FNUT1   R*4 1 L nutrient limitation function                         [
        ! FNUT2   R*4 1 L nutrient limitation function                         [
        ! FNUT3   R*4 1 L nutrient limitation function                         [
        ! KMDIN1  R*4 1 I half-saturation value nitrogen green-algea       [gN/m
        ! KMP1    R*4 1 I half-saturation value phosphorus green-algea     [gP/m
        ! KMSI    R*4 1 I half-saturation value silicate diatoms          [gSi/m
        ! NH4     R*4 1 I concentration of ammonium                        [gN/m
        ! NO3     R*4 1 I concentration of nitrate                         [gN/m
        ! PO4     R*4 1 I concentration of ortho phosphorus                [gP/m
        ! SI      R*4 1 I concentration of dissolved silicate               [g/m

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
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                AMOPRF = process_space_real(IP1)
                KMDIN = process_space_real(IP2)
                KMP = process_space_real(IP3)
                KMSI = process_space_real(IP4)
                NH4 = process_space_real(IP5)
                NO3 = process_space_real(IP6)
                PO4 = process_space_real(IP7)
                SI = process_space_real(IP8)

                IF (AMOPRF < 1E-20)  CALL write_error_message ('AMOPRF in NLALG zero')

                !     Calculation of available dissolved N (NO3 corrected with AMOPRF)
                DIN = NO3 / AMOPRF + NH4
                IF ((NO3 < 0.0) .OR. (NH4 < 0.0)) DIN = 0.0

                !     Nutrient limitation functions (MONOD)
                FN = DIN / (DIN + KMDIN)

                IF (PO4 < 0.0) THEN
                    FP = 0.0
                ELSE
                    FP = PO4 / (PO4 + KMP)
                ENDIF

                IF  (KMSI == -1.0) THEN
                    FS = 1.0
                ELSEIF (SI < 0.0)  THEN
                    FS = 0.0
                ELSE
                    FS = SI / (SI + KMSI)
                ENDIF

                FNUT = MIN (FN, FP, FS)

                !@    Uitvoer limiterende factoren
                process_space_real (IP9) = FN
                process_space_real (IP10) = FP
                process_space_real (IP11) = FS
                process_space_real (IP12) = FNUT

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
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            !
        end do
        !
        RETURN

    END
    !

end module m_nlalg
