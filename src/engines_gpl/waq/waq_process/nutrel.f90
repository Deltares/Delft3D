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
module m_nutrel
    use m_waq_precision

    implicit none

contains


    subroutine nutrel (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Release (nutrients/detritus) by of mortality algae DYNAMO

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        ! FL( 1)  R*4 1 O autolysis of NN4                               [gN/m3/
        ! FL( 2)  R*4 1 O production of N-det                            [gN/m3/
        ! FL( 3)  R*4 1 O autolysis of P                                 [gP/m3/
        ! FL( 4)  R*4 1 O production of P-det                            [gP/m3/
        ! FL( 5)  R*4 1 O autolysis of Si                               [gSi/m3/
        ! FL( 6)  R*4 1 O production of Si-det                         [gSiC/m3/
        ! MRT1    R*4 1 I fraction of mortality dissolved as nutrients         [
        ! MRT2    R*4 1 I fraction of mortality dissolved as nutrients         [
        ! NCRAT1  R*4 1 I Nitrogen-Carbon ratio in green-algea             [gN/g
        ! NCRAT2  R*4 1 I Nitrogen-Carbon ratio in diatoms                 [gN/g
        ! PCRAT1  R*4 1 I Phosphorus-Carbon ratio in green-algea           [gP/g
        ! PCRAT2  R*4 1 I Phosphorus-Carbon ratio in diatoms               [gP/g
        ! RESP1   R*4 1 L total respiration rate const. green-algea          [1/
        ! RESP2   R*4 1 L total respiration rate const. diatoms              [1/
        ! SCRAT3  R*4 1 I Silicate-Carbon ratio in diatoms                [gSi/g

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
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !

                MORT1 = process_space_real(IP1)
                NCRAT1 = process_space_real(IP2)
                PCRAT1 = process_space_real(IP3)
                FMRT1A = process_space_real(IP4)
                FMRT2A = process_space_real(IP5)
                MORT2 = process_space_real(IP6)
                NCRAT2 = process_space_real(IP7)
                PCRAT2 = process_space_real(IP8)
                SCRAT2 = process_space_real(IP9)
                FMRT1D = process_space_real(IP10)
                FMRT2D = process_space_real(IP11)

                !***********************************************************************
                !**** Processes connected to the ALGEA model
                !***********************************************************************

                !     Calculate fractions for carbon (different from nutrient fractions)
                !     no part of carbon to autolyse!
                FDCA = 0.0
                FDCD = 0.0
                IF (FMRT1A < 1.0) FDCA = FMRT2A / (1 - FMRT1A)
                IF (FMRT1D < 1.0) FDCD = FMRT2D / (1 - FMRT1D)

                !@    Production of DETC
                FL (1 + IFLUX) = (MORT1 * FDCA + &
                        MORT2 * FDCD)

                !@    Production of OOC
                FL (2 + IFLUX) = (MORT1 * (1.0 - FDCA) + &
                        MORT2 * (1.0 - FDCD))

                !@    Autolysis of NH4
                FL (3 + IFLUX) = (MORT1 * NCRAT1 * FMRT1A + &
                        MORT2 * NCRAT2 * FMRT1D)

                !@    Production of DETN
                FL (4 + IFLUX) = (MORT1 * NCRAT1 * FMRT2A + &
                        MORT2 * NCRAT2 * FMRT2D)

                !@    Production of OON
                FL (5 + IFLUX) = (MORT1 * NCRAT1 * (1.0 - FMRT1A - FMRT2A) + &
                        MORT2 * NCRAT2 * (1.0 - FMRT1D - FMRT2D))

                !@    Autolysis of PO4
                FL (6 + IFLUX) = (MORT1 * PCRAT1 * FMRT1A + &
                        MORT2 * PCRAT2 * FMRT1D)

                !@    Production of DETP
                FL (7 + IFLUX) = (MORT1 * PCRAT1 * FMRT2A + &
                        MORT2 * PCRAT2 * FMRT2D)

                !@    Production of OOP
                FL (8 + IFLUX) = (MORT1 * PCRAT1 * (1.0 - FMRT1A - FMRT2A) + &
                        MORT2 * PCRAT2 * (1.0 - FMRT1D - FMRT2D))

                !@    Autolysis of Si
                FL (9 + IFLUX) = MORT2 * SCRAT2 * FMRT1D

                !@    Production of Si-det
                FL (10 + IFLUX) = MORT2 * SCRAT2 * FMRT2D

                !@    Production of OOSI
                FL (11 + IFLUX) = MORT2 * SCRAT2 * (1.0 - FMRT1D - FMRT2D)

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
            !
        end do
        !
        RETURN
    END

end module m_nutrel
