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
module m_trase2
   use m_waq_precision

   implicit none

contains

   subroutine trase2(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute

      !>\file
      !>       Total of transport in sediment for 66 substances

      !
      !     Description of the module :
      !
      !        Total of TRAnsport processes in the SEDiment
      !
      ! Name    T   L I/O   Description                                    Units
      ! ----    --- -  -    -------------------                            -----

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

      !     from process_space_real array

      integer(kind=int_wp) :: SWEMERSION ! 3  in  switch indicating submersion(0) or emersion (1)
      integer(kind=int_wp) :: XTRDIF ! 4  in  extra diffusion factor in sediment during emersion (-)

      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, IP11, IP12
      integer(kind=int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, IN11, IN12
      real(kind=real_wp) :: FRDISU, FRDOCU, FRDISD, FRDOCD, &
                            FRPAR, VRESU, VSEDI, VBURI, &
                            VBTUR, VBIRR, FRDIS, VSEEP
      logical NEWBOT
      integer(kind=int_wp) :: IKMRKN, IKMRKV, IVAN, INAAR, IQ

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

      !.....Segmentloop om op nul te zetten
      !      DO 9000 ISEG = 1,num_cells
      ! 9000 CONTINUE

      !.....Exchangeloop over de horizontale richtingen om 0 te zetten
      !.....en over de vertical richting om te initialiseren
      do IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir
         process_space_real(IP11) = 0.0
         process_space_real(IP12) = 0.0
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP9 = IP9 + IN9
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12
      end do

      !.....Exchangeloop over de verticale richting

      do IQ = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

         IVAN = IEXPNT(1, IQ)
         INAAR = IEXPNT(2, IQ)

         !        Zoek eerste kenmerk van- en naar-segmenten
         if (IVAN > 0) then
            call extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
         else
            IKMRKV = -1
         end if
         if (INAAR > 0) then
            call extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)
         else
            IKMRKN = -1
         end if

         !        extra diffusion during emersion

         XTRDIF = 1.0
         if (IVAN > 0) then
            SWEMERSION = nint(process_space_real(IP3 + (IVAN - 1) * IN3))
            if (SWEMERSION == 1) then
               XTRDIF = process_space_real(IP4 + (IVAN - 1) * IN4)
            end if
         end if

         NEWBOT = .false.

         if ((IKMRKV == 1 .and. IKMRKN == 3) .or. &
             (IKMRKV == 0 .and. IKMRKN == 3)) then

            !.....WATER-SEDIMENT INTERFACE

            FRDISD = process_space_real(IP1 + (INAAR - 1) * IN1)
            FRDISU = process_space_real(IP1 + (IVAN - 1) * IN1)
            FRDOCD = process_space_real(IP2 + (INAAR - 1) * IN2)
            FRDOCU = process_space_real(IP2 + (IVAN - 1) * IN2)

            NEWBOT = .true.

         end if

         if ((IKMRKV == 3 .and. IKMRKN == 3)) then

            !.....SEDIMENT-SEDIMENT INTERFACE

            FRDISU = process_space_real(IP1 + (IVAN - 1) * IN1)
            FRDISD = process_space_real(IP1 + (INAAR - 1) * IN1)
            FRDOCU = process_space_real(IP2 + (IVAN - 1) * IN2)
            FRDOCD = process_space_real(IP2 + (INAAR - 1) * IN2)

            NEWBOT = .true.

         end if

         if (IKMRKV == 3 .and. IKMRKN == -1) then

            !.....DEEP SEDIMENT BOUNDARY

            FRDISU = process_space_real(IP1 + (IVAN - 1) * IN1)
            FRDISD = FRDISU
            FRDOCU = process_space_real(IP2 + (IVAN - 1) * IN2)
            FRDOCD = FRDOCU

            NEWBOT = .true.

         end if

         !        Delwaq-G exchange?

         if (NEWBOT) then

            VRESU = process_space_real(IP5)
            VSEDI = process_space_real(IP6)
            VBURI = process_space_real(IP7)
            VBTUR = process_space_real(IP8)
            VBIRR = process_space_real(IP9)
            VSEEP = process_space_real(IP10)

            VBIRR = VBIRR * XTRDIF

            !            Upward advection

            FRDIS = FRDISD + FRDOCD
            FRPAR = 1.0 - FRDIS
            process_space_real(IP11) = (VRESU + min(VBTUR, 0.0)) * FRPAR &
                                       + (min(VBIRR, 0.0) + min(VSEEP, 0.0)) * FRDIS

            !            Downward advection

            FRDIS = FRDISU + FRDOCU
            FRPAR = 1.0 - FRDIS
            process_space_real(IP12) = (VSEDI + VBURI + max(VBTUR, 0.0)) * FRPAR &
                                       + (max(VBIRR, 0.0) + max(VSEEP, 0.0)) * FRDIS
         end if

         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP9 = IP9 + IN9
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12

      end do

      return
   end

end module m_trase2
