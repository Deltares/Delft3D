!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_dynpro
    use m_waq_precision

    implicit none

contains


    subroutine dynpro (process_space_real, fl, ipoint, increm, num_cells, &
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

		ipnt  = ipoint
		iflux = 0
        !
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                fPPGreen   = process_space_real(ipnt(1))
                NCRatGreen = process_space_real(ipnt(2))
                PCRatGreen = process_space_real(ipnt(3))
                fPPDiat    = process_space_real(ipnt(4))
                NCRatDiat  = process_space_real(ipnt(5))
                PCRatDiat  = process_space_real(ipnt(6))
                SCRatDiat  = process_space_real(ipnt(7))
                DELT       = process_space_real(ipnt(8))
                NH4        = process_space_real(ipnt(9))
                NO3        = process_space_real(ipnt(10))
                PO4        = process_space_real(ipnt(11))
                Si         = process_space_real(ipnt(12))
				
				NH4KR      = process_space_real(ipnt(13))

								
                MORT1      = process_space_real(ipnt(14))
                NCRAT1     = process_space_real(ipnt(15))
                PCRAT1     = process_space_real(ipnt(16))
                FMRT1A     = process_space_real(ipnt(17))
                FMRT2A     = process_space_real(ipnt(18))
                MORT2      = process_space_real(ipnt(19))
                NCRAT2     = process_space_real(ipnt(20))
                PCRAT2     = process_space_real(ipnt(21))
                SCRAT2     = process_space_real(ipnt(22))
                FMRT1D     = process_space_real(ipnt(23))
                FMRT2D     = process_space_real(ipnt(24))
				
				
				ConmxN = amax1(NO3 + NH4, 0.0)
                ConmxP = amax1(PO4, 0.0)
                ConmxS = amax1(Si, 0.0)

                N_demand = (fPPDiat * NCratDiat + fPPGreen * NCRatGreen) * DELT
                P_demand = (fPPDiat * PCratDiat + fPPGreen * PCRatGreen) * DELT
                Si_demand = (fPPDiat * SCratDiat) * DELT

                N_fact = 1.0
                P_fact = 1.0
                Si_fact = 1.0
                if (N_demand  > ConmxN) N_fact = ConmxN / N_demand
                if (P_demand  > ConmxP) P_fact = ConmxP / P_demand
                if (Si_demand > ConmxS) Si_fact = ConmxS / Si_demand
                G_fact = MIN (N_fact, P_fact)

                if (Si_fact >= G_fact) then

                    fcPPGreen = G_fact * fPPGreen
                    fcPPDiat = G_fact * fPPDiat

                else

                    fcPPDiat = Si_fact * fPPDiat
                    if (G_fact == 1.0) then
                        fcPPGreen = fPPGreen
                    else
                        ConmxN = ConmxN - fcPPDiat * NCratDiat * DELT
                        ConmxP = ConmxP - fcPPDiat * PCratDiat * DELT
                        N_demand = fPPGreen * NCRatGreen * DELT
                        P_demand = fPPGreen * PCRatGreen * DELT
                        N_fact = 1.0
                        P_fact = 1.0
                        if (N_demand > ConmxN) N_fact = ConmxN / N_demand
                        if (P_demand > ConmxP) P_fact = ConmxP / P_demand
                        fcPPGreen = MIN (N_fact, P_fact) * fPPGreen
                    endif

                endif

                !     CORRECTION ON Nett primary production 1 and 2

                dcPPGreen = fcPPGreen - fPPGreen
                dcPPDiat = fcPPDiat - fPPDiat
                FL (1 + IFLUX) = dcPPGreen
                FL (2 + IFLUX) = dcPPDiat
				
                process_space_real(ipnt(25)) = fcPPGreen
                process_space_real(ipnt(26)) = fcPPDiat

				
				PROD1 = fPPGreen
				PROD2 = fPPDiat
				
				NCRAT1 = NCRatGreen
				NCRAT2 = NCRatDiat
				SCRAT2 = SCRatDiat
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
                process_space_real (ipnt(27)) = NH4D
                process_space_real (ipnt(28)) = XNTOT

                !@    Uptake of NH4
                FL (3 + IFLUX) = (NCRAT1 * PROD1 + NCRAT2 * PROD2) * NH4D

                !@    Uptake of NO3
                FL (4 + IFLUX) = (NCRAT1 * PROD1 + NCRAT2 * PROD2) * NO3D

                !@    Uptake of PO4
                FL (5 + IFLUX) = PCRAT1 * PROD1 + PCRAT2 * PROD2

                !@    Uptake of Si
                FL (6 + IFLUX) = SCRAT2 * PROD2


				


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
                FL (7 + IFLUX) = (MORT1 * FDCA + MORT2 * FDCD)

                !@    Production of OOC
                FL (8 + IFLUX) = (MORT1 * (1.0 - FDCA) + MORT2 * (1.0 - FDCD))

                !@    Autolysis of NH4
                FL (9 + IFLUX) = (MORT1 * NCRAT1 * FMRT1A + MORT2 * NCRAT2 * FMRT1D)

                !@    Production of DETN
                FL (10 + IFLUX) = (MORT1 * NCRAT1 * FMRT2A + MORT2 * NCRAT2 * FMRT2D)

                !@    Production of OON
                FL (11 + IFLUX) = (MORT1 * NCRAT1 * (1.0 - FMRT1A - FMRT2A) + MORT2 * NCRAT2 * (1.0 - FMRT1D - FMRT2D))

                !@    Autolysis of PO4
                FL (12 + IFLUX) = (MORT1 * PCRAT1 * FMRT1A + MORT2 * PCRAT2 * FMRT1D)

                !@    Production of DETP
                FL (13 + IFLUX) = (MORT1 * PCRAT1 * FMRT2A + MORT2 * PCRAT2 * FMRT2D)

                !@    Production of OOP
                FL (14 + IFLUX) = (MORT1 * PCRAT1 * (1.0 - FMRT1A - FMRT2A) + MORT2 * PCRAT2 * (1.0 - FMRT1D - FMRT2D))

                !@    Autolysis of Si
                FL (15 + IFLUX) = MORT2 * SCRAT2 * FMRT1D

                !@    Production of Si-det
                FL (16 + IFLUX) = MORT2 * SCRAT2 * FMRT2D

                !@    Production of OOSI
                FL (17 + IFLUX) = MORT2 * SCRAT2 * (1.0 - FMRT1D - FMRT2D)

            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            ipnt = ipnt + increm
        end do
        !
        RETURN
    END

end module m_dynpro
