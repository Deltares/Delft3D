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
module m_trcoef
   use m_waq_precision

   implicit none

contains

   subroutine trcoef(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Gas and liquid exchange organic micro pollutants (Lyman and O'Conner)

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        Calculation of transport coefficients for air-water exhchange
      !        as a function of:
      !        sw=0: wind, veloc and molmass (Chemical estimates, Lyman)
      !        sw=1: veloc, diffusion coefficients (IMPAQT)= O'Conner formulas
      !
      ! Name    T   L I/O   Description                                   Uni
      ! ----    --- -  -    -------------------                            --
      ! SWITCH  R*4 1 I  Switch for calculation method                     [-]
      ! WIND    R*4 1 I  Windspeed                                       [m/s]
      ! VELOC   R*4 1 I  Flow velocity                                   [m/s]
      ! M       R*4 1 I  Molecuulmassa omive                           [g/mol]
      ! LDIF    R*4 1 O  molekular diffusion coefficient waterphase     [m2/d]
      ! GDIF    R*4 1 O  molekular diffusion coefficient gasphase       [m2/d]
      ! KL      R*4 1 O  mass transport coefficient waterphase           [m/d]
      ! KG      R*4 1 O  mass transport coefficient gasphase             [m/d]
      ! VL      R*4 1 LC viscosity waterphase                    [Pa/s=kg/m/s]
      ! VG      R*4 1 LC viscosity gashase                       [Pa/s=kg/m/s]
      ! RHOL    R*4 1 LC density waterphase                            [kg/m3]
      ! RHOG    R*4 1 LC density gasphase                              [kg/m3]
      ! TEMP    R*4 1 I  Temperatuur
      ! DEPTH   R*4 1 I  Diepte
      !-----------------------------------------------------------------------

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------
      use m_logger_helper
      use m_extract_waq_attribute
      use PHYSICALCONSTS, only: CtoKelvin
      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      logical WOROPT, WNDOPT, TMPOPT
      !
      !     Local declarations, constants in source
      !
      parameter(C1=18., &
                C2=5.64, &
                C3=0.969, &
                C4=0.673, &
                C5=32., &
                C6=0.526, &
                C7=0.7, &
                CRIT1=1.9, &
                CRIT2=5.0, &
                E=2.718, &
                KELVIN=real(CtoKelvin), &
                VCMIN=0.001, &
                C11=1.293, &
                C12=0.00367, &
                C13=1000.0, &
                C14=0.088, &
                C15=1.32, &
                C16=0.009, &
                C17=1.e-5, &
                C18=0.001, &
                C21=0.01, &
                C22=6.1, &
                C23=0.63, &
                C24=0.001, &
                C25=0.0463, &
                C26=0.67, &
                C27=1.e-6, &
                C28=0.0144, &
                C29=0.00341, &
                C30=2.2, &
                CRIT3=0.3)
      !
      IN2 = INCREM(2)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
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
      !
      EXP1 = exp(C6 * (CRIT2 - CRIT1))
      if (IN2 == 0) then
         WIND = process_space_real(IP2)
         if (WIND < 0.0) call write_error_message('WIND       in TRCOEF < 0')
         if (WIND >= CRIT1) then
            if (WIND < CRIT2) then
               EXP2 = exp(C6 * (WIND - CRIT1))
            else
               FAC1 = (1.+(WIND - CRIT2)**C7) * EXP1
            end if
         end if
         !     Calculate wind at watersurface from wind at 10m (m/s)
         FWIND = C21 * WIND * sqrt(C22 + C23 * WIND)
         if (FWIND < CRIT3) then
            FWIN2 = C28 * FWIND**C30
         else
            FWIN2 = C29 * FWIND
         end if
         WNDOPT = .false.
      else
         WNDOPT = .true.
      end if
      !
      if (IN4 == 0) then
         M = process_space_real(IP4)
         if (M < 1.e-30) call write_error_message('MOLMASS    in TRCOEF = 0')
         WORTL1 = sqrt(C1 / M)
         WORTL5 = sqrt(C5 / M) * C2
         WOROPT = .false.
      else
         WOROPT = .true.
      end if
      !
      if (IN5 == 0 .and. IN6 == 0 .and. IN8 == 0) then
         TEMP = process_space_real(IP8)
         !--calculation of bulk densitys of water and air:
         RHOG = C11 / (1.+C12 * TEMP)
         RHOL = C13 - C14 * TEMP
         !--calculation of viscosities of water and air:
         VG = (C15 + C16 * TEMP) * C17
         VL = C18
         LDIF = process_space_real(IP5)
         GDIF = process_space_real(IP6)
         if (GDIF < 1.e-30) call write_error_message('GAS-DIFF   in TRCOEF = 0')
         if (LDIF < 1.e-30) call write_error_message('WATER-DIFF in TRCOEF = 0')
         !     Calculate Schmidt numbers for water and gas
         SCG = VG / (RHOG * GDIF / 86400.)
         SCL = VL / (RHOL * LDIF / 86400.)
         TMPOPT = .false.
      else
         TMPOPT = .true.
      end if
      !
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
            if ((IKMRK2 == 0) .or. (IKMRK2 == 1)) then
               !
               !     Map process_space_real on local variables
               !
               ISWTCH = process_space_real(IP1) + 0.5
               !
               if (ISWTCH == 0) then
                  !
                  VELOC = process_space_real(IP3)
                  if (VELOC < 0.0) call write_error_message('VELOC      in TRCOEF < 0')
                  DEPTH = process_space_real(IP7)
                  !
                  if (WNDOPT) then
                     WIND = process_space_real(IP2)
                     if (WIND < 0.0) call write_error_message('WIND       in TRCOEF < 0')
                     if (WIND >= CRIT1) then
                        if (WIND < CRIT2) then
                           EXP2 = exp(C6 * (WIND - CRIT1))
                        else
                           FAC1 = (1.+(WIND - CRIT2)**C7) * EXP1
                        end if
                     end if
                  end if
                  if (WIND < 0.0) call write_error_message('WIND       in TRCOEF < 0')
                  !
                  if (WOROPT) then
                     M = process_space_real(IP4)
                     if (M < 1.e-30) call write_error_message('MOLMASS    in TRCOEF = 0')
                     WORTL1 = sqrt(C1 / M)
                     WORTL5 = sqrt(C5 / M) * C2
                  end if
                  !
                  !     Ensure that VELOC is >= VCMIN
                  !
                  VELOC = max(VELOC, VCMIN)
                  !
                  !     Gasphase exchange coefficient
                  !
                  KG = KELVIN * (WIND + VELOC) * WORTL1
                  !
                  !     Water exchange coefficient
                  !
                  if (WIND < CRIT1) then
                     KL = VELOC**C3 / DEPTH**C4 * WORTL5
                  elseif (WIND < CRIT2) then
                     KL = VELOC**C3 / DEPTH**C4 * WORTL5 * EXP2
                  else
                     KL = VELOC**C3 / DEPTH**C4 * WORTL5 * FAC1
                  end if
                  !
               end if

               if (ISWTCH == 1) then
                  ! --- Impact formulations (O'connor personal communication?)
                  !
                  if (WNDOPT) then
                     WIND = process_space_real(IP2)
                     if (WIND < 0.0) call write_error_message('WIND       in TRCOEF < 0')
                     !     Calculate wind at watersurface from wind at 10m (m/s)
                     FWIND = C21 * WIND * sqrt(C22 + C23 * WIND)
                     if (FWIND < CRIT3) then
                        FWIN2 = C28 * FWIND**C30
                     else
                        FWIN2 = C29 * FWIND
                     end if
                  end if
                  !
                  if (TMPOPT) then
                     TEMP = process_space_real(IP8)
                     !--calculation of bulk densitys of water and air:
                     RHOG = C11 / (1.+C12 * TEMP)
                     RHOL = C13 - C14 * TEMP
                     !--calculation of viscosities of water and air:
                     VG = (C15 + C16 * TEMP) * C17
                     VL = C18
                     ! --- Impact formulations (O'connor personal communication?)
                     LDIF = process_space_real(IP5)
                     GDIF = process_space_real(IP6)
                     if (GDIF < 1.e-30) call write_error_message('GAS-DIFF   in TRCOEF = 0')
                     if (LDIF < 1.e-30) call write_error_message('WATER-DIFF in TRCOEF = 0')
                     !     Calculate Schmidt numbers for water and gas
                     SCG = VG / (RHOG * GDIF / 86400.)
                     SCL = VL / (RHOL * LDIF / 86400.)
                  end if
                  KG = (C24 + C25 * FWIND / SCG**C26) * 86400.
                  KL = (C27 + FWIN2 / sqrt(SCL)) * 86400.
               end if
               !
               !     Output
               !
               process_space_real(IP9) = KL
               process_space_real(IP10) = KG
               !
            end if
         end if
         !
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
         !
      end do
      !
      return
      !
   end

end module m_trcoef
