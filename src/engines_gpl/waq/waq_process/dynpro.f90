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

   subroutine dynpro(process_space_real, fl, ipoint, increm, num_cells, &
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
      ! NCRATGREEN  R*4 1 I Nitrogen-Carbon ratio in green-algea             [gN/g
      ! NCRATDIAT  R*4 1 I Nitrogen-Carbon ratio in diatoms                 [gN/g
      ! PCRATGREEN  R*4 1 I Phosphorus-Carbon ratio in green-algea           [gP/g
      ! PCRATDIAT  R*4 1 I Phosphorus-Carbon ratio in diatoms               [gP/g
      ! RESP1   R*4 1 L total respiration rate const. green-algea          [1/
      ! RESP2   R*4 1 L total respiration rate const. diatoms              [1/
      ! SCRAT3  R*4 1 I Silicate-Carbon ratio in diatoms                [gSi/g

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------
      !
      implicit none
      !
      !     Type    Name         I/O Description

      real(kind=real_wp) :: process_space_real(*) !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: fl(*) ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: ipoint(23) ! I  Array of pointers in process_space_real to get and store the data
      integer(kind=int_wp) :: increm(23) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: num_cells ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: noflux ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: iknmrk(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: num_exchanges_u_dir ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp) :: num_exchanges_v_dir ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
      integer(kind=int_wp) :: num_exchanges_z_dir ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer(kind=int_wp) :: ipnt(23) !    Local work array for the pointering
      integer(kind=int_wp) :: iseg !    Local loop counter for computational element loop

      !integer(kind = int_wp) :: ILUMON       !    Local loop counter for computational element loop
      integer(kind=int_wp) :: iflux !    Local loop counter for computational element loop
      !
      !     Local declaration
      !
      real(kind=real_wp) :: FPPGREEN !    Gross primary production of green-algae [gC/m3/d]
      real(kind=real_wp) :: NCRATGREEN !    Nitrogen-Carbon ratio in green-algae             [gN/gC]
      real(kind=real_wp) :: PCRATGREEN !    Phosphorus-Carbon ratio in green-algae           [gP/gC]
      real(kind=real_wp) :: FPPDIAT !    Gross primary production of diatoms              [gC/m3/d]
      real(kind=real_wp) :: NCRATDIAT !    Nitrogen-Carbon ratio in diatoms                 [gN/gC]
      real(kind=real_wp) :: PCRATDIAT !    Phosphorus-Carbon ratio in diatoms               [gP/gC]
      real(kind=real_wp) :: SCRATDIAT !    Silicate-Carbon ratio in diatoms                [gSi/gC]
      real(kind=real_wp) :: DELT !    Time step of the model [d]
      real(kind=real_wp) :: NH4 !    Ammonium concentration [gN/m3]
      real(kind=real_wp) :: NO3 !    Nitrate concentration [gN/m3]
      real(kind=real_wp) :: PO4 !    Phosphate concentration [gP/m3]
      real(kind=real_wp) :: Si !    Silicate concentration [gSi/m3]
      real(kind=real_wp) :: NH4KR !    Critical NH4 concentration for uptake [gN/m3]
      real(kind=real_wp) :: MORT1 !    Mortality rate of green-algae [1/d]
      real(kind=real_wp) :: MORT2 !    Mortality rate of diatoms [1/d]
      real(kind=real_wp) :: FMRT1A !    Fecal mortality rate of green-algae [1/d]
      real(kind=real_wp) :: FMRT2A !    Fecal mortality rate of diatoms [1/d]
      real(kind=real_wp) :: FMRT1D !    fraction autolysis Greens [gC/m3/d]
      real(kind=real_wp) :: FMRT2D !    fraction to detritus by mortality Greens [gC/m3/d]
      real(kind=real_wp) :: CONMXN !    Maximum concentration of nitrogen [gN/m3]
      real(kind=real_wp) :: CONMXP !    Maximum concentration of phosphorus [gP/m3]
      real(kind=real_wp) :: CONMXS !    Maximum concentration of silicate [gSi/m3]

      real(kind=real_wp) :: N_DEMAND
      real(kind=real_wp) :: P_DEMAND
      real(kind=real_wp) :: SI_DEMAND
      real(kind=real_wp) :: N_FACT
      real(kind=real_wp) :: P_FACT
      real(kind=real_wp) :: SI_FACT
      real(kind=real_wp) :: G_FACT
      real(kind=real_wp) :: FCPPGREEN
      real(kind=real_wp) :: FCPPDIAT
      real(kind=real_wp) :: DCPPGREEN
      real(kind=real_wp) :: DCPPDIAT
      real(kind=real_wp) :: XNTOT
      real(kind=real_wp) :: NH4D
      real(kind=real_wp) :: NO3D
      real(kind=real_wp) :: NH4N
      real(kind=real_wp) :: XNREST
      real(kind=real_wp) :: FNH4
      real(kind=real_wp) :: FDCA
      real(kind=real_wp) :: FDCD

      ipnt = ipoint
      iflux = 0
      !
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            fPPGreen = process_space_real(ipnt(1))
            NCRatGreen = process_space_real(ipnt(2))
            PCRatGreen = process_space_real(ipnt(3))
            fPPDiat = process_space_real(ipnt(4))
            NCRatDiat = process_space_real(ipnt(5))
            PCRatDiat = process_space_real(ipnt(6))
            SCRatDiat = process_space_real(ipnt(7))
            DELT = process_space_real(ipnt(8))
            NH4 = process_space_real(ipnt(9))
            NO3 = process_space_real(ipnt(10))
            PO4 = process_space_real(ipnt(11))
            Si = process_space_real(ipnt(12))

            NH4KR = process_space_real(ipnt(13))

            MORT1 = process_space_real(ipnt(14))
            MORT2 = process_space_real(ipnt(15))
            FMRT1A = process_space_real(ipnt(16))
            FMRT2A = process_space_real(ipnt(17))
            FMRT1D = process_space_real(ipnt(18))
            FMRT2D = process_space_real(ipnt(19))

            ConmxN = max(NO3 + NH4, 0.0)
            ConmxP = max(PO4, 0.0)
            ConmxS = max(Si, 0.0)

            N_demand = (fPPDiat * NCratDiat + fPPGreen * NCRatGreen) * DELT
            P_demand = (fPPDiat * PCratDiat + fPPGreen * PCRatGreen) * DELT
            Si_demand = (fPPDiat * SCratDiat) * DELT

            N_fact = 1.0
            P_fact = 1.0
            Si_fact = 1.0
            if (N_demand > ConmxN) N_fact = ConmxN / N_demand
            if (P_demand > ConmxP) P_fact = ConmxP / P_demand
            if (Si_demand > ConmxS) Si_fact = ConmxS / Si_demand
            G_fact = min(N_fact, P_fact)

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
                  fcPPGreen = min(N_fact, P_fact) * fPPGreen
               end if

            end if

            !     CORRECTION ON Nett primary production 1 and 2

            dcPPGreen = fcPPGreen - fPPGreen
            dcPPDiat = fcPPDiat - fPPDiat
            FL(1 + IFLUX) = dcPPGreen
            FL(2 + IFLUX) = dcPPDiat

            process_space_real(ipnt(20)) = fcPPGreen
            process_space_real(ipnt(21)) = fcPPDiat

            NCRATGREEN = NCRatGreen
            NCRATDIAT = NCRatDiat
            SCRATDIAT = SCRatDiat
            !***********************************************************************
            !**** Processes connected to the ALGEA model
            !***********************************************************************

            !      maximum uptake of N in one day (gC/m3)
            XNTOT = (NCRATGREEN * fcPPGreen + NCRATDIAT * fcPPDiat) * DELT

            !      check if NH4+NO3 available
            !      make sure that the mass balance is closed - the sum of
            !      NH4D and NO3D must be 1.
            if (((NH4 + NO3) <= 0.0) .or. (XNTOT <= 0.0)) then
               NH4D = 1.0
               NO3D = 0.0
            else
               if (NH4 > NH4KR) then
                  NH4N = NH4 - NH4KR
                  if (XNTOT <= NH4N) then
                     NH4D = 1.0
                     NO3D = 0.0
                  else
                     XNREST = XNTOT - NH4 + NH4KR
                     FNH4 = NH4KR / (NO3 + NH4KR)
                     NH4D = (NH4N + FNH4 * XNREST) / XNTOT
                     NO3D = 1.0 - NH4D
                  end if
               else
                  !          below the critical NH4 conentration distribution of
                  !          NO3 and NH4 uptake based on availability!
                  NH4D = NH4 / (NO3 + NH4)
                  NO3D = 1.0 - NH4D
               end if
            end if
            !     uitvoer fraction adsorbed as NH4
            process_space_real(ipnt(22)) = NH4D
            process_space_real(ipnt(23)) = XNTOT

            !@    Uptake of NH4
            FL(3 + IFLUX) = (NCRATGREEN * fcPPGreen + NCRATDIAT * fcPPDiat) * NH4D

            !@    Uptake of NO3
            FL(4 + IFLUX) = (NCRATGREEN * fcPPGreen + NCRATDIAT * fcPPDiat) * NO3D

            !@    Uptake of PO4
            FL(5 + IFLUX) = PCRATGREEN * fcPPGreen + PCRATDIAT * fcPPDiat

            !@    Uptake of Si
            FL(6 + IFLUX) = SCRATDIAT * fcPPDiat

            !***********************************************************************
            !**** Processes connected to the ALGEA model
            !***********************************************************************

            !     Calculate fractions for carbon (different from nutrient fractions)
            !     no part of carbon to autolyse!
            FDCA = 0.0
            FDCD = 0.0
            if (FMRT1A < 1.0) FDCA = FMRT2A / (1 - FMRT1A)
            if (FMRT1D < 1.0) FDCD = FMRT2D / (1 - FMRT1D)

            !@    Production of DETC
            FL(7 + IFLUX) = (MORT1 * FDCA + MORT2 * FDCD)

            !@    Production of OOC
            FL(8 + IFLUX) = (MORT1 * (1.0 - FDCA) + MORT2 * (1.0 - FDCD))

            !@    Autolysis of NH4
            FL(9 + IFLUX) = (MORT1 * NCRATGREEN * FMRT1A + MORT2 * NCRATDIAT * FMRT1D)

            !@    Production of DETN
            FL(10 + IFLUX) = (MORT1 * NCRATGREEN * FMRT2A + MORT2 * NCRATDIAT * FMRT2D)

            !@    Production of OON
            FL(11 + IFLUX) = (MORT1 * NCRATGREEN * (1.0 - FMRT1A - FMRT2A) + MORT2 * NCRATDIAT * (1.0 - FMRT1D - FMRT2D))

            !@    Autolysis of PO4
            FL(12 + IFLUX) = (MORT1 * PCRATGREEN * FMRT1A + MORT2 * PCRATDIAT * FMRT1D)

            !@    Production of DETP
            FL(13 + IFLUX) = (MORT1 * PCRATGREEN * FMRT2A + MORT2 * PCRATDIAT * FMRT2D)

            !@    Production of OOP
            FL(14 + IFLUX) = (MORT1 * PCRATGREEN * (1.0 - FMRT1A - FMRT2A) + MORT2 * PCRATDIAT * (1.0 - FMRT1D - FMRT2D))

            !@    Autolysis of Si
            FL(15 + IFLUX) = MORT2 * SCRATDIAT * FMRT1D

            !@    Production of Si-det
            FL(16 + IFLUX) = MORT2 * SCRATDIAT * FMRT2D

            !@    Production of OOSI
            FL(17 + IFLUX) = MORT2 * SCRATDIAT * (1.0 - FMRT1D - FMRT2D)

         end if
         !
         IFLUX = IFLUX + NOFLUX
         ipnt = ipnt + increm
      end do
      !
      return
   end

end module m_dynpro
