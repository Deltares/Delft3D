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
module m_calsed
   use m_waq_precision

   implicit none

contains

   subroutine calsed(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: write_error_message

      !>\file
      !>       Sedimentation velocity IMx, DetC OOC, BODC, all algea = f (Temp SS Sal)

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        SEDIMENTATION VELOCITY BASED ON TEMP, SUSPENDED SOLID CONC AND
      !        SALINITY
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                            -----
      ! CRSUSP  R*4 1 I  critical susp solid conc. for flocculation     [gDM/m3]
      ! N       R*4 1 I  coefficient in sedimentation formulation            [-]
      ! SUSP    R*4 1 I  total suspended solid concentration            [gDM/m3]
      ! SEDTC   R*4 1 I  temperature coefficient for sedimentation           [-]
      ! TEMP    R*4 1 I  ambient temperature                             [gradC]
      ! V0SED   R*4 1 I  sedimentaion velocity (no temp, sal, ss influence)[m/d]
      ! SAL     R*4 1 I  salinity                                         [g/kg]
      ! MAXSAL  R*4 1 I  salinity where salinity function is at max       [g/kg]
      ! ENHFAC  R*4 1 I  enhancement factor in salinity functin              [-]
      ! SALFUN  R*4 1 I  salinity function on sedimentation velocity         [-]
      ! FLOFUN  R*4 1 I  flocculation function on sedimentation velocity     [-]
      ! TEMFUN  R*4 1 I  temperature function on sedimentation velocity      [-]
      ! VSED    R*4 1 I  sedimentaion velocity, temp, sal, ss corrected    [m/d]

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real process_space_real(*), FL(*)
      integer IPOINT(*), INCREM(*), num_cells, NOFLUX, &
         IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      !     Local
      !
      parameter(PI=3.14159265)
      integer(kind=int_wp) :: num_exchanges
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
      do ISEG = 1, num_cells
         if (btest(IKNMRK(ISEG), 0)) then

            V0SED = process_space_real(IP1)
            SUSP = max(process_space_real(IP2), 0.0)
            CRSUSP = process_space_real(IP3)
            N = process_space_real(IP4)
            TEMP = process_space_real(IP5)
            SEDTC = process_space_real(IP6)
            SAL = max(process_space_real(IP7), 0.0)
            MAXSAL = process_space_real(IP8)
            ENHFAC = process_space_real(IP9)

            if (CRSUSP < 1e-20) call write_error_message('CRSUSP in CALSED zero')

            !*******************************************************************************
            !**** Processes connected to the sedimentation VELOCITY
            !***********************************************************************

            !     Initialisatie
            FLOFUN = 1.0
            SALFUN = 1.0
            TEMFUN = 1.0

            !     Flocculatie functie

            if (SUSP / CRSUSP >= 1.e-30) then
               FLOFUN = (SUSP / CRSUSP)**N
            end if

            !     Temperatuur functie

            if (SEDTC /= 1.0) then
               TEMFUN = SEDTC**(TEMP - 20.0)
            end if

            !     Salinity functie

            if (SAL < MAXSAL) then
               SALFUN = (ENHFAC + 1.) / 2.-((ENHFAC - 1.) / 2.) * cos(PI * SAL / MAXSAL)
            elseif (MAXSAL >= 0.0) then
               SALFUN = ENHFAC
            else
               SALFUN = 1.0
            end if

            !     Bereken VSED
            VSED = V0SED * TEMFUN * SALFUN * FLOFUN

            !     Output of calculated sedimentation rate
            process_space_real(IP10) = VSED
            process_space_real(IP11) = SALFUN
            process_space_real(IP12) = FLOFUN
            !
            !     ENDIF
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
         !
      end do
      !

      num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

      IP10 = IPOINT(10)
      IN10 = INCREM(10)
      IP13 = IPOINT(13)
      IN13 = INCREM(13)

      do IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

         process_space_real(IP13) = 0.0

         IP13 = IP13 + IN13

      end do

      do IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges

         IVAN = IEXPNT(1, IQ)
         !
         !        Sedimentation velocity from segment to exchange-area
         !
         if (IVAN > 0) then
            process_space_real(IP13) = process_space_real(IP10 + (IVAN - 1) * IN10)
         end if

         IP13 = IP13 + IN13

      end do

      return
      !
   end

end module m_calsed
