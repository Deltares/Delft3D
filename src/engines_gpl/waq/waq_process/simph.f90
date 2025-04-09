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
module m_simph
   use m_waq_precision

   implicit none

contains

   subroutine simph(process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Simple calculation of pH

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        Simple calculation of pH, TESTVERSIE
      !        Restrictions: ...
      !
      ! Name    T   L I/O   Description                                    Uni
      ! ----    --- -  -    -------------------                             --
      ! AHPLUS  R*4 1 L  activity of H+                                [mol/l]
      ! ALKA    R*4 1 I  Alkalinity as HCO3-                          [mol/m3]
      ! CO2     R*4 1 O  concentration CO2 in water                  [gCO2/m3]
      ! CARBTO  R*4 1 I  total carbonate concentration                 [gC/m3]
      ! M3TOL   R*4 1 L  conversion from 1/m3 to 1/l                    [m3/l]
      ! MC      R*4 1 L  from gC/m3 to mol C/m3 (molar weight)         [g/mol]
      ! MCO2    R*4 1 L  from gCO2 to mol CO2 (molar weight)           [g/mol]
      ! MHCO3   R*4 1 L  fron gHCO3 to mol HCO3 (molar weight)         [g/mol]
      ! K1      R*4 1 L  dissociation constant CO2-HCO3                [mol/l]
      ! K2      R*4 1 L  dissociation constant HCO3-CO3                [mol/l]
      ! PH      R*4 1 O  pH                                         [pH units]
      ! SAL     R*4 1 I  salinity                                       [g/kg]
      ! TEMP    R*4 1 I  temperature                                      [oC]
      ! TEMpK   R*4 1 L  temperature in Kelvin                             [K]
      !-----------------------------------------------------------------------

      !     Logical Units : -

      !     Modules called : -
      !

      !     Name     Type   Library
      !     ------   -----  ------------
      use m_logger_helper
      use PHYSICALCONSTS, only: CtoKelvin
      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      !     Local declarations, constants in source
      !
      parameter(MC=12.0, MCO2=44.0, &
                MHCO3=61.0, M3TOL=1.0e-3, KELVIN=real(CtoKelvin))
      integer(kind=int_wp), save :: nr_mes = 0 ! message count negative total carbonate
      integer(kind=int_wp), save :: nrmes2 = 0 ! message count negative salinity
      integer(kind=int_wp), save :: nrmes3 = 0 ! message count high salinity
      integer(kind=int_wp), save :: nrmes4 = 0 ! message count negative alkalinity
      integer(kind=int_wp), save :: nrmes5 = 0 ! message count negative H+
      !
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      !
      !     Loop over de segmenten
      !
      do ISEG = 1, num_cells
         !
         !     Eerste kenmerk actief of inactief segment
         !
         !     Alleen actieve en bodem segmenten behandelen
         !
         if (btest(IKNMRK(ISEG), 0)) then
            !
            !     Map process_space_real on local variables
            !
            SAL = process_space_real(IP1)
            CARBTOT = process_space_real(IP2)
            ALKA = process_space_real(IP3)
            TEMP = process_space_real(IP4)
            PH_MIN = process_space_real(IP5)
            PH_MAX = process_space_real(IP6)
            !
            !     Error messages

            if (CARBTOT < 1e-30) then
               call get_log_unit_number(ILUMON)
               if (NR_MES < 10) then
                  NR_MES = NR_MES + 1
                  write (ILUMON, *) 'WARNING :total carbonate <= 0', &
                     ' segment=', ISEG, ' conc=', CARBTOT
               end if
               if (NR_MES == 10) then
                  NR_MES = NR_MES + 1
                  write (ILUMON, *) ' 10 WARNINGS on total carbonate'
                  write (ILUMON, *) ' No further messages on total carbonate'
               end if
               CARBTOT = 1e-30
            end if
            if (SAL < 1e-30) then
               call get_log_unit_number(ILUMON)
               if (NRMES2 < 10) then
                  NRMES2 = NRMES2 + 1
                  write (ILUMON, *) 'WARNING :salinity <= 0', &
                     ' segment=', ISEG, ' conc=', SAL
               end if
               if (NRMES2 == 10) then
                  NRMES2 = NRMES2 + 1
                  write (ILUMON, *) ' 10 WARNINGS on salinity'
                  write (ILUMON, *) ' No further messages on salinity'
               end if
               SAL = 1e-30
            end if
            if (SAL > 50.) then
               call get_log_unit_number(ILUMON)
               if (NRMES4 < 10) then
                  NRMES4 = NRMES4 + 1
                  write (ILUMON, *) 'WARNING :salinity => 50.', &
                     ' segment=', ISEG, ' conc=', SAL
               end if
               if (NRMES4 == 10) then
                  NRMES4 = NRMES4 + 1
                  write (ILUMON, *) ' 10 WARNINGS on salinity'
                  write (ILUMON, *) ' No further messages on salinity'
               end if
               SAL = 50.
            end if
            if (ALKA < 1e-30) then
               call get_log_unit_number(ILUMON)
               if (NRMES3 < 10) then
                  NRMES3 = NRMES3 + 1
                  write (ILUMON, *) 'WARNING: alkalinity <= 0', &
                     ' segment=', ISEG, ' conc=', ALKA
               end if
               if (NRMES3 == 10) then
                  NRMES3 = NRMES3 + 1
                  write (ILUMON, *) ' 10 WARNINGS on alkalinity'
                  write (ILUMON, *) ' No further messages on alkalinity'
               end if
               ALKA = 1e-30
            end if
            if (TEMP <= -KELVIN) then
               write (ILUMON, *) ' WARNING: Temperature drops below 0 Kelvin', &
                  ' segment=', ISEG, ' Temp set to 15 oC (288.15 K)'
               TEMP = 15
            end if
            !
            !---- Procesformuleringen ---------------------------------------
            ! ********************************
            ! Dissociatieconstanten afhankelijk van temperatuur en saliniteit
            TEMPK = TEMP + KELVIN

            ! Roy et al (1993), Millero (1995)
            LNK1 = 290.9097 - 14554.21 / TEMPK - 45.0575 * log(TEMPK) + &
                   (-228.39774 + 9714.36839 / TEMPK + 34.485796 * log(TEMPK)) * &
                   SAL**0.5 + (54.20871 - 2310.48919 / TEMPK - &
                               8.19515 * log(TEMPK)) * SAL + (-3.969101 + 170.22169 / TEMPK + &
                                                              0.603627 * log(TEMPK)) * SAL**1.5 - 0.00258768 * SAL**2

            ! --- Unit of K1 and K2 [mol/kg H2O]. To convert to [mol/kg solution] a fraction is added (Millero 1995)
            LNK1 = LNK1 + log(1 - SAL * 0.001005)

            K1 = exp(LNK1)

            ! Roy et al (1993), Millero (1995)
            LNK2 = 207.6548 - 11843.79 / TEMPK - 33.6485 * log(TEMPK) + &
                   (-167.69908 + 6551.35253 / TEMPK + 25.928788 * log(TEMPK)) * &
                   SAL**0.5 + (39.75854 - 1566.13883 / TEMPK - &
                               6.171951 * log(TEMPK)) * SAL + (-2.892532 + 116.270079 / TEMPK &
                                                               + 0.45788501 * log(TEMPK)) * SAL**1.5 - 0.00613142 * SAL**2

            LNK2 = LNK2 + log(1 - SAL * 0.001005)

            K2 = exp(LNK2)

            ! ******************************
            ! Conversion of [g/m3] to [mol/kg solvent]
            ! Density seawater [kg/l]
            RHOH2O = (1000.+0.7 * SAL / (1 - SAL / 1000.) &
                      - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)) / 1000

            CARBTOT = (CARBTOT * M3TOL) / MC / RHOH2O
            ALKA = (ALKA * M3TOL) / MHCO3 / RHOH2O

            ! Oplossing vierkantsvergelijking
            A = ALKA / K1
            B = ALKA - CARBTOT
            C = K2 * (ALKA - 2 * CARBTOT)
            D = B**2 - 4 * A * C
            if (D < 0) then
               call get_log_unit_number(ILUMON)
               write (ILUMON, *) 'No solution for pH: discriminant<0'
               goto 10
            end if

            AHPLUS = (-B + sqrt(D)) / (2 * A)
            if (AHPLUS <= 0) then
               call get_log_unit_number(ILUMON)
               if (NRMES5 < 10) then
                  NRMES5 = NRMES5 + 1
                  write (ILUMON, *) 'WARNING: H+ negative: ', AHPLUS, ' in segment ', ISEG
               end if
               if (NRMES5 == 10) then
                  NRMES5 = NRMES5 + 1
                  write (ILUMON, *) ' 10 WARNINGS on H+'
                  write (ILUMON, *) ' No further messages on H+'
               end if
               goto 10
            end if
            PH = -log10(AHPLUS)
            PH = max(PH_MIN, PH)
            PH = min(PH_MAX, PH)
            AHPLUS = 10.**(-PH)

            ! --- Berekening CO2 Concentratie ----
            CO2 = (CARBTOT / (1 + K1 / AHPLUS + K1 * K2 / AHPLUS**2)) &
                  * MCO2 * RHOH2O / M3TOL

            !
            !---- Output: voorzover van toepassing --------------------
            !
            process_space_real(IP7) = PH
            process_space_real(IP8) = CO2
            !
         end if
         !
         !---- Pointers ophogen ( altijd buiten de if's op kenmerk) in de segment loop
         !
10       IP1 = IP1 + INCREM(1)
         IP2 = IP2 + INCREM(2)
         IP3 = IP3 + INCREM(3)
         IP4 = IP4 + INCREM(4)
         IP5 = IP5 + INCREM(5)
         IP6 = IP6 + INCREM(6)
         IP7 = IP7 + INCREM(7)
         IP8 = IP8 + INCREM(8)
         !
      end do
      !
      !
      return
   end

end module m_simph
