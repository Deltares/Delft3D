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
module m_adspo4
   use m_waq_precision

   implicit none

contains

   subroutine adspo4(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: write_error_message, get_log_unit_number

      !>\file
      !>       P-ad/desorption to particulate inorganic matter. 3 options for sorption formulation.

      !
      !     Description of the module :
      !     P-adsorption onto particulate inorganic matter. 3 options for
      !     sorption formulation.
      !
      !        ----- old version -----
      ! Name    T   L I/O   Description                                   Units
      ! ----    --- -  -    -------------------                            ----
      ! AAP     R*4 1 I AAP concentration                                [gP/m3]
      ! SWAdsP  R*4 1 I swithc for adsorption option to use                  [-]
      ! DELT    R*4 1 I DELWAQ timestep                                    [scu]
      ! EQAAP   R*4 1 L calculated equlibrium AAP concentration          [gP/m3]
      ! FL (1)  R*4 1 O adsorption/desorption flux                     [gP/m3/d]
      ! TIM     R*4 1 I TIM   concentration                             [gDM/m3]
      ! KD      R*4 1 I partition coefficent PO4-AAP             [-] or [m3/gDM]
      ! MAXADS  R*4 1 I maximum adsorption (capacity)                   [gP/gDM]
      ! process_space_real(9) R*4 1 O calculated equlibrium AAP concentration          [gP/m3]
      ! PO4     R*4 1 I PO4 concentration                                [gP/m3]
      ! RCADS   R*4 1 I first order rate constatnt adsorption              [1/d]
      !
      !        ----- new version -----
      ! Name    T   L I/O   Description                                   Units
      ! ----    --- -  -    -------------------                            ----
      ! AAP     R*4 1 I AAP concentration                                [gP/m3]
      ! PO4     R*4 1 I PO4 concentration                                [gP/m3]
      ! EQAAP   R*4 1 - calculated equlibrium AAP concentration          [gP/m3]
      ! KDADS   R*4 1 I distribution coeff. [-], or adsorpt. constant    [m3/gP]
      ! KADS20  R*4 1 I molar adsorption constant at 20 oC    [mole(a-1).l(a-1)]
      ! KADS    R*4 1 - molar adsorption constant             [mole(a-1).l(a-1)]
      ! KSORP   R*4 1 I first order rate constant for adsorption           [1/d]
      ! FCAP    R*4 1 I adsorption capacity for Fe                      [gP/gFe]
      ! FRA     R*4 1 - correction factor for oxidised iron                  [-]
      ! FADS    R*4 1 O adsorption/desorption flux                     [gP/m3/d]
      ! CADST   R*4 1 - molar total concentration adsorption sites    [moleFe/l]
      ! CADS    R*4 1 - molar concentration free adsorption sites     [moleFe/l]
      ! IM1     R*4 1 I IM1 concentration                               [gDM/m3]
      ! IM2     R*4 1 I IM2 concentration                               [gDM/m3]
      ! IM3     R*4 1 I IM3 concentration                               [gDM/m3]
      ! TFE     R*4 1 - total Fe concentration                          [gFe/m3]
      ! FRFE1   R*4 1 I fraction of Fe in IM1                          [gFe/gDW]
      ! FRFE2   R*4 1 I fraction of Fe in IM2                          [gFe/gDW]
      ! FRFE3   R*4 1 I fraction of Fe in IM3                          [gFe/gDW]
      ! FRFEOX  R*4 1 I fraction of oxidised Fe                              [-]
      ! FIM1    R*4 1 O fraction of adsorbed P in IM1                        [-]
      ! FIM2    R*4 1 O fraction of adsorbed P in IM2                        [-]
      ! FIM3    R*4 1 O fraction of adsorbed P in IM3                        [-]
      ! PH      R*4 1 I pH                                                   [-]
      ! OH      R*4 1 - hydroxyl concentration                          [mole/l]
      ! AOH     R*4 1 I reaction constant for hydroxyl                       [-]
      ! TEMP    R*4 1 I temperature                                         [oC]
      ! TC      R*4 1 I temperature coeffcient adsorption constant           [-]
      ! OXY     R*4 1 I dissolved oxygen concentration                  [gO2/m3]
      ! CROXY   R*4 1 I critical dissolved oxygen concentration         [gO2/m3]
      ! POROS   R*4 1 I prorosity                                            [-]
      ! DELT    R*4 1 I DELWAQ timestep                                    [scu]
      ! SWADSP  R*4 1 I switch for selection of the adsorption option        [-]
      !
      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------
      !
      implicit real(A - H, J - Z)
      implicit integer(i)
      !
      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      integer(kind=int_wp) :: IVERSN
      real(kind=real_wp) :: AAP, PO4, EQAAP, KDADS, KADS20, KADS, KSORP, &
                            FCAP, FRA, FADS, CADST, CADS, IM1, IM2, &
                            IM3, TFE, FRFE1, FRFE2, FRFE3, FRFEOX, FIM1, &
                            FIM2, FIM3, PH, OH, AOH, TEMP, TC, &
                            OXY, CROXY, POROS, DELT, SWADSP, QIM1, QIM2, &
                            QIM3, EQAAPM
      integer(kind=int_wp) :: NR_MES, ILUMON
      save NR_MES
      data NR_MES/0/
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
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)
      IP29 = IPOINT(29)
      IP30 = IPOINT(30)
      IP31 = IPOINT(31)
      IP32 = IPOINT(32)
      IP33 = IPOINT(33)
      IP34 = IPOINT(34)
      !
      IFLUX = 0
      !
      IVERSN = nint(process_space_real(IP23))
      !
      !     Use the old version when IVERSN=0
      !
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            if (IVERSN == 0) then
               !
               dPAds = 0.0
               EQAAP = 0.0
               AtotP = 0.0
               Kads = 0.0
               !
               SWAdsP = process_space_real(IP1)
               PO4 = max(process_space_real(IP2), 0.0)
               AAP = max(process_space_real(IP3), 0.0)
               IM1 = max(process_space_real(IP4), 0.0)
               IM2 = max(process_space_real(IP5), 0.0)
               IM3 = max(process_space_real(IP6), 0.0)
               RCADS = process_space_real(IP12)
               KD = process_space_real(IP7)
               MAXADS = process_space_real(IP8)
               DELT = process_space_real(IP9)
               !
               TIM = IM1 + IM2 + IM3
               QIM1 = 0.0
               QIM2 = 0.0
               QIM3 = 0.0
               if (TIM < 1e-10) then
                  FIM1 = 0.0
                  FIM2 = 0.0
                  FIM3 = 0.0
               else
                  FIM1 = IM1 / TIM
                  FIM2 = IM2 / TIM
                  FIM3 = IM3 / TIM
                  if (IM1 > 1e-10) QIM1 = AAP * FIM1 / IM1
                  if (IM2 > 1e-10) QIM2 = AAP * FIM2 / IM2
                  if (IM3 > 1e-10) QIM3 = AAP * FIM3 / IM3
               end if
               !
               !------ Error messages
               !
               if (KD < 0.0) call write_error_message('KD in ADSPO4 lower then zero')
               if (((abs(KD) < 1e-20) .or. (abs(PO4) < 1e-20)) &
                   .and. (abs(MAXADS) < 1e-20)) &
                  call write_error_message &
                  ('(KD or PO4) and MAXADS equal zero in ADSPO4')
               !
               !------ (1) Instantanaeous equilibrium partitioning
               !           SWAdsP = 0
               !
               if (nint(SWAdsP) == 0) then
                  dPAds = (((AAP + PO4) / (1.+KD)) - AAP) / DELT
                  EQAAP = -1.0
               end if
               !
               !------ (2) Kinetic Langmuir sorption
               !           SWAdsP = 1
               !
               if (nint(SWAdsP) == 1) then
                  if ((MAXADS < 1.e-10) .or. (KD < 1.e-10)) then
                     EQAAP = 0.0
                     dPAds = 0.0
                  else
                     EQAAP = TIM * (KD * PO4 * MAXADS) / (KD * PO4 + MAXADS)
                     dPAds = RCADS * (EQAAP - AAP)
                  end if
               end if
               !
               !------ (3) GEM formulation for sorption
               !           SWAdsP = 2
               !
               if (nint(SWAdsP) == 2) then
                  KAds20 = process_space_real(IP10)
                  TCKAds = process_space_real(IP11)
                  RCadsP = process_space_real(IP25)
                  aOHPO4 = process_space_real(IP13)
                  fr_Fe = process_space_real(IP24)
                  frFeox = process_space_real(IP17)
                  OXY = process_space_real(IP18)
                  CrOXY = process_space_real(IP19)
                  pH = process_space_real(IP20)
                  Temp = process_space_real(IP21)
                  poros = process_space_real(IP22)
                  !
                  !--------- (3a) Eqs 6.38, 6.37 of GEM report
                  OH = 10.0**(pH - 14.0)
                  if (OXY >= CrOXY * POROS) then
                     fOxSor = 1.0
                  else
                     fOxSor = frFeox
                  end if
                  !
                  !--------- (3b) Eqn 6.36 of GEM report : total sorption capacity (mol/l)
                  AtotP = fOxSor * fr_Fe * TIM / (56000.0 * poros)
                  !
                  !--------- (3c) Eqn 6.35 of GEM report : free sorption capacity (mol/l)
                  Afree = AtotP - (AAP / (31000.0 * poros))
                  !
                  !--------- (3d) Eqn 6.34 of GEM report : temp.correction of Kads
                  Kads = Kads20 * TCKads**(Temp - 20.0)
                  !
                  !--------- (3e) Eqn 6.33 of GEM report : equilibrium conc of Pads (gP/m3)
                  EQAAP = (AAP + PO4) * &
                          (1.0 - 1.0 / (Kads * Afree * (OH**aOHPO4) + 1.0))
                  !
                  !--------- (3f) Eqn 6.31 of GEM report : the adsorption flux (gP/m3/d)
                  dPads = RCAdsP * (EQAAP - AAP)
                  !
               end if
               !
               !---- Output of module
               !
               FL(1 + IFLUX) = dPAds
               process_space_real(IP26) = EQAAP
               process_space_real(IP27) = AtotP
               process_space_real(IP28) = Kads
               process_space_real(IP29) = FIM1
               process_space_real(IP30) = FIM2
               process_space_real(IP31) = FIM3
               process_space_real(IP32) = QIM1
               process_space_real(IP33) = QIM2
               process_space_real(IP34) = QIM3
               !
               !---- End active cells block
               !
            else
               !
               !     Use the new version when IVERSN=1
               !
               SWADSP = process_space_real(IP1)
               PO4 = max(process_space_real(IP2), 0.0)
               AAP = max(process_space_real(IP3), 0.0)
               IM1 = max(process_space_real(IP4), 0.0)
               IM2 = max(process_space_real(IP5), 0.0)
               IM3 = max(process_space_real(IP6), 0.0)
               KDADS = process_space_real(IP7)
               FCAP = process_space_real(IP8)
               DELT = process_space_real(IP9)
               KSORP = process_space_real(IP12)
               FRFE1 = process_space_real(IP14)
               FRFE2 = process_space_real(IP15)
               FRFE3 = process_space_real(IP16)
               !
               !     Calculation of the total concentration of iron and
               !     the fractions of adsorbed phosphate in the inorganic matter
               !     fractions IM1-3
               !
               TFE = FRFE1 * IM1 + FRFE2 * IM2 + FRFE3 * IM3
               QIM1 = 0.0
               QIM2 = 0.0
               QIM3 = 0.0
               if (TFE < 1e-10) then
                  FIM1 = 0.0
                  FIM2 = 0.0
                  FIM3 = 0.0
               else
                  FIM1 = FRFE1 * IM1 / TFE
                  FIM2 = FRFE2 * IM2 / TFE
                  FIM3 = FRFE3 * IM3 / TFE
                  if (IM1 > 1e-10) QIM1 = AAP * FIM1 / IM1
                  if (IM2 > 1e-10) QIM2 = AAP * FIM2 / IM2
                  if (IM3 > 1e-10) QIM3 = AAP * FIM3 / IM3
               end if
               !
               !     Error messages
               !
               if (KDADS < 0.0) call write_error_message('KDADS in ADSPO4 negative')
               if (((abs(KDADS) < 1e-20) .or. (abs(PO4) < 1e-20)) &
                   .and. (abs(FCAP) < 1e-20)) &
                  call write_error_message &
                  ('(KDADS or PO4) and FCAP equal zero in ADSPO4')
               !
               !     Start the calculation of the sorption flux
               !     Use one of three options
               !
               CADS = 0.0
               CADST = 0.0
               EQAAP = 0.0
               FADS = 0.0
               KADS = 0.0
               !
               !     SWADSP = 0 : Instantaneous equilibrium partitioning
               !
               if (nint(SWADSP) == 0) then
                  FADS = (((AAP + PO4) / (1.0 + KDADS)) - AAP) / DELT
                  EQAAP = -1.0
               end if
               !
               !     SWADSP = 1 : Langmuir sorption
               !
               if (nint(SWADSP) == 1) then
                  if ((FCAP < 1e-10) .or. (KDADS < 1e-10)) then
                     EQAAP = 0.0
                     FADS = 0.0
                  else
                     CADST = FCAP * TFE
                     EQAAP = (KDADS * CADST * PO4) / (KDADS * PO4 + 1.0)
                     FADS = KSORP * (EQAAP - AAP)
                  end if
               end if
               !
               !     SWADSP = 2 : pH dependent Langmuir sorption
               !
               if (nint(SWADSP) == 2) then
                  KADS20 = process_space_real(IP10)
                  TC = process_space_real(IP11)
                  AOH = process_space_real(IP13)
                  FRFEOX = process_space_real(IP17)
                  OXY = process_space_real(IP18)
                  CROXY = process_space_real(IP19)
                  PH = process_space_real(IP20)
                  TEMP = process_space_real(IP21)
                  POROS = process_space_real(IP22)

                  if (POROS < 1e-10) call write_error_message &
                     ('POROS in ADSPO4 equals zero')
                  if (TFE < 1.e-10) then
                     if (NR_MES < 25) then
                        call get_log_unit_number(ILUMON)
                        NR_MES = NR_MES + 1
                        write (ILUMON, *) 'WARNING :zero TFE in ADSPO4', &
                           ' segment=', ISEG, ' TFE=', TFE
                     end if
                     if (NR_MES == 25) then
                        call get_log_unit_number(ILUMON)
                        NR_MES = NR_MES + 1
                        write (ILUMON, *) ' 25 WARNINGS on zero TFE'
                        write (ILUMON, *) ' Further messages on algae surpressed'
                     end if
                  end if
                  !
                  !     Calculate pH dependency (hydroxyl) and factor for redox potential
                  !
                  OH = 10.0**(PH - 14.0)
                  if (OXY >= (CROXY * POROS)) then
                     FRA = 1.0
                  else
                     FRA = FRFEOX
                  end if
                  !
                  !     Calculate total sorption capacity (mol/l)
                  !
                  CADST = FRA * TFE / (56000.0 * POROS)
                  !
                  !     Calculate free sorption capacity (mol/l)
                  !
                  CADS = max(0.0, CADST - (AAP / (31000.0 * POROS)))
                  !
                  !     Calculate temperature corrected KADS
                  !
                  KADS = KADS20 * TC**(TEMP - 20.0)
                  !
                  !     Calculate equilibrium concentration of adsorbed P (gP/m3)
                  !
                  if (abs(CADS) < 1.e-20) then
                     EQAAP = 0.0
                  else
                     EQAAP = (AAP + PO4) / (1.0 + OH**AOH / (KADS * CADS))
                  end if
                  !
                  !     Maximize EQAAP on equivalent CADST
                  !
                  if (CADS < 0.0) then
                     EQAAPM = 0.9 * CADST * (31000.0 * POROS)
                     EQAAP = EQAAPM
                  end if
                  !
                  !     Calculate the adsorption flux (gP/m3/d)
                  !
                  FADS = KSORP * (EQAAP - AAP)

               end if
               !
               !     Output of module
               !
               FL(1 + IFLUX) = FADS
               process_space_real(IP26) = EQAAP
               process_space_real(IP27) = CADST
               process_space_real(IP28) = KADS
               process_space_real(IP29) = FIM1
               process_space_real(IP30) = FIM2
               process_space_real(IP31) = FIM3
               process_space_real(IP32) = QIM1
               process_space_real(IP33) = QIM2
               process_space_real(IP34) = QIM3
               !
               !     End active cells block
               !
            end if
            !
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         IP4 = IP4 + INCREM(4)
         IP5 = IP5 + INCREM(5)
         IP6 = IP6 + INCREM(6)
         IP7 = IP7 + INCREM(7)
         IP8 = IP8 + INCREM(8)
         IP9 = IP9 + INCREM(9)
         IP10 = IP10 + INCREM(10)
         IP11 = IP11 + INCREM(11)
         IP12 = IP12 + INCREM(12)
         IP13 = IP13 + INCREM(13)
         IP14 = IP14 + INCREM(14)
         IP15 = IP15 + INCREM(15)
         IP16 = IP16 + INCREM(16)
         IP17 = IP17 + INCREM(17)
         IP18 = IP18 + INCREM(18)
         IP19 = IP19 + INCREM(19)
         IP20 = IP20 + INCREM(20)
         IP21 = IP21 + INCREM(21)
         IP22 = IP22 + INCREM(22)
         IP23 = IP23 + INCREM(23)
         IP24 = IP24 + INCREM(24)
         IP25 = IP25 + INCREM(25)
         IP26 = IP26 + INCREM(26)
         IP27 = IP27 + INCREM(27)
         IP28 = IP28 + INCREM(28)
         IP29 = IP29 + INCREM(29)
         IP30 = IP30 + INCREM(30)
         IP31 = IP31 + INCREM(31)
         IP32 = IP32 + INCREM(32)
         IP33 = IP33 + INCREM(33)
         IP34 = IP34 + INCREM(34)
         !
      end do
      !
      return
      !
   end

end module m_adspo4
