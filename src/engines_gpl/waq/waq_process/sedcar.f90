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
module m_sedcar
   use m_waq_precision

   implicit none

contains

   subroutine sedcar(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !>\file
      !>       Sedimentation routine used for OOC, algae, BOD pools, bacteria etc.

      !
      !     Description of the module :
      !
      !        General water quality module for DELWAQ:
      !        SEDIMENTATION FORMULATIONS
      !        MODULE VALID FOR DETC, OOC, DIAT, AAP, BLOOM ALGAE
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                            -----
      ! CONC    R*4 1 I  concentration sedimenting material water        [gX/m3]
      ! DEPTH   R*4 1 I  DELWAQ depth                                        [m]
      ! FL (1)  R*4 1 O  sedimentation flux (water->mixinglayer)       [gX/m3/d]
      ! MINDEP  R*4 1 I  minimal depth for sedimentation                     [m]
      ! PSED    R*4 1 L  sedimentaion probability (0 - 1)                    [-]
      ! POTSED  R*4 1 L  potential sedimentation flux                  [gX/m2/d]
      ! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
      ! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
      ! TCRSED  R*4 1 I  critical sheerstress sedimentation            [kg/m/s2]
      ! VCRSED  R*4 1 I  critical velocity sedimentation                   [m/s]
      ! VSED    R*4 1 O  first order sedimentaion rate (calculated)        [m/d]
      ! ZERSED  R*4 1 I  zeroth order sedimentation flux               [gX/m2/d]

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      use m_logger_helper, only: get_log_unit_number
      use m_cli_utils, only: get_command_argument_by_name
      use m_extract_waq_attribute
      use BottomSet !  Module with definition of the waterbottom segments

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      real(kind=real_wp) :: MINDEP, MINDE2, DEPTH, DEPTH2

      logical, save :: FIRST = .true.
      real(kind=real_wp), save :: PSEDMIN
      integer(kind=int_wp) :: LUNREP
      logical :: parsing_error

      if (FIRST) then
         if (get_command_argument_by_name('-psedmin', PSEDMIN, parsing_error)) then
            call get_log_unit_number(LUNREP)
            if (.not. parsing_error) then
               write (LUNREP, *) ' option -psedmin found, value: ', PSEDMIN
            else
               write (LUNREP, *) ' ERROR: option -psedmin found but value not correct: ', PSEDMIN
            end if
         else
            PSEDMIN = 0.0
         end if
         FIRST = .false.
      end if

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

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
      IN9 = INCREM(9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)

      IFLUX = 0
      do ISEG = 1, num_cells

         !     zero output

         process_space_real(IP10) = 0.0
         process_space_real(IP11) = 0.0

         !     sedimentation towards the bottom

         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
         if (IKMRK1 == 1) then
            call extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
            if ((IKMRK2 == 0) .or. (IKMRK2 == 3)) then
               !
               CONC = max(0.0, process_space_real(IP1))
               ZERSED = process_space_real(IP2)
               VSED = max(0.0, process_space_real(IP3)) ! Avoid inadvertent source if VSED negative (Delft3D-35562)
               TAU = process_space_real(IP4)
               TCRSED = process_space_real(IP5)
               DEPTH = process_space_real(IP6)
               DELT = process_space_real(IP7)
               MINDEP = process_space_real(IP8)

               !***********************************************************************
               !**** Processes connected to the SEDIMENTATION
               !***********************************************************************

               !     Calculate sedimenation probability

               if (TAU == -1.0) then
                  PSED = 1.0
               elseif (TCRSED < 1e-20) then
                  PSED = 0.0
               else
                  !         vergelijking met critische schuifspanning
                  PSED = max(0.0, (1.0 - TAU / TCRSED))
               end if
               PSED = max(PSEDMIN, PSED)

               !     Calculate potential sedimentation fluxes
               !     No sedimentation when depth below min depth

               if (DEPTH < MINDEP) then
                  MAXSED = 0.0
                  FL(1 + IFLUX) = 0.0
               else
                  POTSED = ZERSED + (VSED * CONC) * PSED

                  !        limit sedimentation to available mass (M/L2/DAY)
                  MAXSED = min(POTSED, CONC / DELT * DEPTH)

                  !        convert sedimentation to flux in M/L3/DAY
                  FL(1 + IFLUX) = MAXSED / DEPTH
               end if

               !     Output of calculated sedimentation rate
               process_space_real(IP10) = PSED
               process_space_real(IP11) = MAXSED
               !
            end if
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         !
      end do
      !
      IP1 = IPOINT(1)
      IP6 = IPOINT(6)
      IP8 = IPOINT(8)
      IP11 = IPOINT(11)

      !.....Exchangeloop over de horizontale richting
      do IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

         process_space_real(IP12) = 0.0

         IP12 = IP12 + IN12

      end do

      IP9 = IP9 + (num_exchanges_u_dir + num_exchanges_v_dir) * IN9

      !.....Exchangeloop over de verticale richting
      do IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

         IVAN = IEXPNT(1, IQ)
         INAAR = IEXPNT(2, IQ)

         if (IVAN > 0 .and. INAAR > 0) then

            !           Zoek eerste kenmerk van- en naar-segmenten

            call extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
            call extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
            if (IKMRKV == 1 .and. IKMRKN == 3) then

               !               Bodem-water uitwisseling: NUL FLUX OM OOK OUDE PDF's
               !                                         TE KUNNEN GEBRUIKEN
               !               Snelheid behoeft niet gezet (gebeurt in TRASED)

               !               MAXSED = process_space_real (IP11+(IVAN-1)*IN11)
               !               CONC   = MAX (1E-20, process_space_real(IP1+(IVAN-1)*IN1) )
               !               process_space_real(IP12) = MAXSED/86400./CONC
               FL(1 + (IVAN - 1) * NOFLUX) = 0.0

            elseif (IKMRKV == 1 .and. IKMRKN == 1) then
               !               Water-water uitwisseling
               !rs             merk op: sedimentatie tussen waterlagen: geen taucr correctie,
               !rs             alleen conversie van 1/d naar 1/s. Ten overvloede:
               !rs             scu (s) en aux-timer (d) liggen dus vast!

               DEPTH = process_space_real(IP6 + (IVAN - 1) * IN6)
               DEPTH2 = process_space_real(IP6 + (INAAR - 1) * IN6)
               MINDEP = process_space_real(IP8 + (IVAN - 1) * IN8)
               MINDE2 = process_space_real(IP8 + (INAAR - 1) * IN8)
               if (DEPTH > MINDEP .and. DEPTH2 > MINDE2) then
                  process_space_real(IP12) = process_space_real(IP9) / 86400.
               else
                  process_space_real(IP12) = 0.0
               end if
            else
               process_space_real(IP12) = 0.0
            end if

         end if

         IP9 = IP9 + IN9
         IP12 = IP12 + IN12

      end do

      !     Handle velocity to the delwaq-g bottom

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

      do IK = 1, Coll%current_size

         IWA1 = Coll%set(IK)%fstwatsed
         IWA2 = Coll%set(IK)%lstwatsed

         do IQ = IWA1, IWA2
            IWATER = IEXPNT(1, IQ)

            CONC = max(0.0, process_space_real(IP1 + (IWATER - 1) * IN1))
            ZERSED = process_space_real(IP2 + (IWATER - 1) * IN2)
            VSED = max(0.0, process_space_real(IP3 + (IWATER - 1) * IN3))
            TAU = process_space_real(IP4 + (IWATER - 1) * IN4)
            TCRSED = process_space_real(IP5 + (IWATER - 1) * IN5)
            DEPTH = process_space_real(IP6 + (IWATER - 1) * IN6)
            DELT = process_space_real(IP7 + (IWATER - 1) * IN7)
            MINDEP = process_space_real(IP8 + (IWATER - 1) * IN8)

            !           Calculate sedimenation probability

            if (TAU == -1.0) then
               PSED = 1.0
            elseif (TCRSED < 1e-20) then
               PSED = 0.0
            else
               !               vergelijking met critische schuifspanning
               PSED = max(0.0, (1.0 - TAU / TCRSED))
            end if

            !           Bereken de potentiele sedimentatie fluxen
            !           Geen sedimentatie onder een minimale diepte

            if (DEPTH < MINDEP) then
               MAXSED = 0.0
            else
               POTSED = ZERSED + (VSED * CONC) * PSED

               !              sedimenteer maximaal de aanwezige hoeveelheid (M/L2/DAY)
               MAXSED = min(POTSED, CONC / DELT * DEPTH)

            end if

            if (CONC > 1.e-10) then
               process_space_real(IP12 + (IQ - 1) * IN12) = MAXSED / 86400./CONC
            end if

         end do

      end do
      !
      return
   end

end module m_sedcar
