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
module m_nutupt
    use m_waq_precision

    implicit none

contains


    subroutine nutupt (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Uptake of nutrients by growth of algae (DYNAMO)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        ! CONMXN  R*4 1 L conc. beschikbaar N voor opname (positief)       [gN/m3
        ! CONMXP  R*4 1 L conc. beschikbaar P voor opname (positief)       [gP/m
        ! CONMXS  R*4 1 L conc. beschikbaar Si voor opname (positief)     [gSi/m
        ! DELT    R*4 1 I timestep in the computation                         [D]
        ! FL (1)  R*4 1 O uptake of NH4                                  [gN/m3/
        ! FL( 2)  R*4 1 O uptake of NO3                                  [gN/m3/
        ! FL( 3)  R*4 1 O uptake of PO4                                  [gP/m3/
        ! FL( 4)  R*4 1 O uptake of Si                                  [gSi/m3/
        ! IAUSYS  R*4 1 I ratio SCU and AUX                                    [
        ! MXPR1N  R*4 1 L max prod rate green algea based on avail N     [gC/m3/
        ! MXPR2N  R*4 1 L max prod rate diatoms based on avail N         [gC/m3/
        ! MXPR1P  R*4 1 L max prod rate green algea based on avail P     [gC/m3/
        ! MXPR2P  R*4 1 L max prod rate diatoms based on avail P         [gC/m3/
        ! MXPR2S  R*4 1 L max prod rate diatoms based on avail Si        [gC/m3/
        ! MXPRD1  R*4 1 L max prod rate green algea based on nutrients   [g/C/m3
        ! MXPRD2  R*4 1 L max prod rate diatoms based on nutrients      [g/C/m3/
        ! MXPRD   R*4 1 L max prod rate total algea based on nutrients  [g/C/m3/
        ! NCRAT1  R*4 1 I Nitrogen-Carbon ratio in green-algea             [gN/g
        ! NCRAT2  R*4 1 I Nitrogen-Carbon ratio in diatoms                 [gN/g
        ! NH4D    R*4 1 L fraction ammonium uptake all algea                   [
        ! NO3D    R*4 1 L fraction nitrate uptake all algea                    [
        ! NH4KR   R*4 1 I below this NH4 conc. no preference NO3/NH4       [gN/m
        ! NH4N    R*4 1 L available ammonium conc. (NH4-crit.NH4)          [gN/m
        ! PCRAT1  R*4 1 I Phosphorus-Carbon ratio in green-algea           [gP/g
        ! PCRAT2  R*4 1 I Phosphorus-Carbon ratio in diatoms               [gP/g
        ! FPP1    R*4 1 L total net production of algea1               [gC/m3/d]
        ! FPP2    R*4 1 L total net production of algea2               [gC/m3/d]
        ! SICRAT  R*4 1 I Silicate-Carbon ratio in diatoms                [gSi/g
        ! XNREST  R*4 1 L XNTOT - amount ammonia available for uptake       [g/m
        ! XNTOT   R*4 1 L total DIN uptake in one timestep                  [g/m

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
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                PROD1 = process_space_real(IP1)
                NCRAT1 = process_space_real(IP2)
                PCRAT1 = process_space_real(IP3)
                PROD2 = process_space_real(IP4)
                NCRAT2 = process_space_real(IP5)
                PCRAT2 = process_space_real(IP6)
                SCRAT2 = process_space_real(IP7)
                DELT = process_space_real(IP8)
                NH4 = process_space_real(IP9)
                NO3 = process_space_real(IP10)
                NH4KR = process_space_real(IP11)

                !***********************************************************************
                !**** Processes connected to the ALGEA model
                !***********************************************************************

                !      maximum uptake of N in one day (gC/m3)
                XNTOT = (NCRAT1 * PROD1 + &
                        NCRAT2 * PROD2) * DELT

                !      check if NH4+NO3 available
                !      make sure that the mass balance is closed - the sum of
                !      NH4D and NO3D must be 1.
                IF (((NH4 + NO3) <= 0.0) .OR. (XNTOT <= 0.0)) THEN
                    NH4D = 1.0
                    NO3D = 0.0
                ELSE
                    IF (NH4 > NH4KR) THEN
                        NH4N = NH4 - NH4KR
                        IF (XNTOT <= NH4N) THEN
                            NH4D = 1.
                            NO3D = 0.
                        ELSE
                            XNREST = XNTOT - NH4 + NH4KR
                            FNH4 = NH4KR / (NO3 + NH4KR)
                            NH4D = (NH4N + FNH4 * XNREST) / XNTOT
                            NO3D = 1. - NH4D
                        ENDIF
                    ELSE
                        !          below the critical NH4 conentration distribution of
                        !          NO3 and NH4 uptake based on availability!
                        NH4D = NH4 / (NO3 + NH4)
                        NO3D = 1. - NH4D
                    ENDIF
                ENDIF
                !     uitvoer fraction adsorbed as NH4
                process_space_real (IP12) = NH4D
                process_space_real (IP13) = XNTOT

                !@    Uptake of NH4
                FL (1 + IFLUX) = (NCRAT1 * PROD1 + &
                        NCRAT2 * PROD2) * NH4D

                !@    Uptake of NO3
                FL (2 + IFLUX) = (NCRAT1 * PROD1 + &
                        NCRAT2 * PROD2) * NO3D

                !@    Uptake of PO4
                FL (3 + IFLUX) = PCRAT1 * PROD1 + &
                        PCRAT2 * PROD2

                !@    Uptake of Si
                FL (4 + IFLUX) = SCRAT2 * PROD2
                !
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
            IP13 = IP13 + INCREM (13)
            !
        end do
        !
        RETURN
        !
    END

end module m_nutupt
