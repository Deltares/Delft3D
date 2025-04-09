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
module m_clcrad
   use m_waq_precision

   implicit none

contains

   subroutine CLCRAD(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)

      !
      !     Function : Calculates the radiation at the surface and at the bottom of the
      !                active water segments
      !

      use m_logger_helper, only: stop_with_error, get_log_unit_number
      use m_extract_waq_attribute
      use BottomSet !  Module with definition of the waterbottom segments

      implicit none

      !     arguments

      real(kind=real_wp) :: process_space_real(*) ! in/out input-output array space to be adressed with IPOINT/INCREM
      real(kind=real_wp) :: FL(*) ! in/out flux array
      integer(kind=int_wp) :: IPOINT(*) ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
      integer(kind=int_wp) :: INCREM(*) ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
      integer(kind=int_wp) :: num_cells ! in     number of segments
      integer(kind=int_wp) :: NOFLUX ! in     total number of fluxes (increment in FL array)
      integer(kind=int_wp) :: IEXPNT(4, *) ! in     exchange pointer table
      integer(kind=int_wp) :: IKNMRK(*) ! in     segment features array
      integer(kind=int_wp) :: num_exchanges_u_dir ! in     number of exchanges in first direction
      integer(kind=int_wp) :: num_exchanges_v_dir ! in     number of exchanges in second direction
      integer(kind=int_wp) :: num_exchanges_z_dir ! in     number of exchanges in third direction
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! in     number of exchanges in fourth direction

      !     from process_space_real array

      real(kind=real_wp) :: EXTVL ! 1  in  total extinction coefficient visible light   (1/m)
      real(kind=real_wp) :: DEPTH ! 2  in  depth of segment                               (m)
      real(kind=real_wp) :: RADSURF ! 3  in  irradiation at the water surface            (W/m2)
      real(kind=real_wp) :: A_ENH ! 4  in  enhancement factor in radiation calculation    (-)
      real(kind=real_wp) :: SURF ! 5  in  horizontal surface                            (m2)
      integer(kind=int_wp) :: SWEMERSION ! 6  in  switch indicating submersion(0) or emersion (1)(-)
      real(kind=real_wp) :: RADBOT ! 7  loc/out 9 irradiation at the segment lower-boundary   (W/m2)

      !     local decalrations

      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
      integer(kind=int_wp) :: IP11 ! index pointers in process_space_real array
      integer(kind=int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10
      integer(kind=int_wp) :: IN11 ! increments in process_space_real array
      integer(kind=int_wp) :: LUNREP ! report file
      integer(kind=int_wp) :: ISEG ! loop counter segment loop
      integer(kind=int_wp) :: IKMRK1 ! first feature inactive(0)-active(1)-bottom(2) segment
      integer(kind=int_wp) :: IK1VN ! first feature inactive(0)-active(1)-bottom(2) VAN segment
      integer(kind=int_wp) :: IK1NR ! first feature inactive(0)-active(1)-bottom(2) NAAR segment
      integer(kind=int_wp) :: IK2VN ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) VAN segment
      integer(kind=int_wp) :: IK2NR ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) NAAR segment
      integer(kind=int_wp) :: IK ! loop counter bottom columns
      integer(kind=int_wp) :: IQ ! loop counter exchanges
      integer(kind=int_wp) :: IVAN ! segment number from
      integer(kind=int_wp) :: INAAR ! segment number to
      integer(kind=int_wp) :: IWA1 ! index first water exchange
      integer(kind=int_wp) :: IWA2 ! index last water exchange
      integer(kind=int_wp) :: ITOP ! index first bottom exhange
      integer(kind=int_wp) :: IBOT ! index last bottom exhange
      integer(kind=int_wp) :: IWATER ! segment number water segment
      integer(kind=int_wp) :: IBODEM ! segment number bottom segment
      real(kind=real_wp) :: RADTOP ! radiation at top
      real(kind=real_wp) :: TOTSURF ! cummulated surface area
      real(kind=real_wp) :: REFLEC ! Reflected fraction of incident sunlight

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

      if (IP7 /= IP9) then
         call get_log_unit_number(LUNREP)
         write (LUNREP, *) 'Error in CLCRAD: Rad/RadDay/Rad_uv should be an input too!'
         write (LUNREP, *) 'Use the correct proc_def!'
         write (*, *) 'Error in CLCRAD: Rad/RadDay/Rad_uv should be an input too!'
         write (*, *) 'Use the correct proc_def!'
         call stop_with_error()
      end if

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

      !.....2DH mode

      if (num_exchanges_z_dir == 0) then

         do ISEG = 1, num_cells

            call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

            !........Segment is inactief
            if (IKMRK1 == 0) then

               !          RadTop = RadSurf corrected for reflection
               process_space_real(IP9) = process_space_real(IP3) * (1.-process_space_real(IP8))

               !          RadBot    = RadSurf corrected for reflection
               process_space_real(IP10) = process_space_real(IP3) * (1.-process_space_real(IP8))

               !          atten
               process_space_real(IP11) = 0.0

               !........Segment is actief watersegment
            else if (IKMRK1 == 1) then

               !          RadTop    = RadSurf corrected for reflection
               process_space_real(IP9) = process_space_real(IP3) * (1.-process_space_real(IP8))

               !          RadBot    = RadSurf   * (1 - reflection) * EXP( -ExtVl    *Depth     )
               process_space_real(IP10) = process_space_real(IP3) * (1.-process_space_real(IP8)) * exp(-process_space_real(IP1) * process_space_real(IP2))

               !          atten    = ExtVl    *Depth
               process_space_real(IP11) = process_space_real(IP1) * process_space_real(IP2)

               !........Segment is actief bodemsegment
            else if (IKMRK1 == 3) then

               !          RadTop    = 0.0
               process_space_real(IP9) = 0.0

               !          RadBot    = 0.0
               process_space_real(IP10) = 0.0

               !          attenuation    = 0.0
               process_space_real(IP11) = 0.0

            end if

            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11

         end do

      else

         !.....3D MODE

         do IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            if (IVAN > 0 .and. INAAR > 0) then
               call extract_waq_attribute(1, IKNMRK(IVAN), IK1VN)
               call extract_waq_attribute(1, IKNMRK(INAAR), IK1NR)
               call extract_waq_attribute(2, IKNMRK(IVAN), IK2VN)
               call extract_waq_attribute(2, IKNMRK(INAAR), IK2NR)

               !...........Van segment = inactief
               if (IK1VN == 0) then

                  !              RadTop = RadSurf corrected for reflection
                  RADTOP = process_space_real(IP3 + (IVAN - 1) * IN3) &
                           * (1.-process_space_real(IP8 + (IVAN - 1) * IN8))
                  process_space_real(IP9 + (IVAN - 1) * IN9) = RADTOP
                  !              RadBot = RadTOP
                  process_space_real(IP10 + (IVAN - 1) * IN10) = RADTOP

                  !              atten
                  process_space_real(IP11 + (IVAN - 1) * IN11) = 0.0

                  !...........Van segment = actief water segment
               else if (IK1VN == 1) then

                  !..............Van segment = water segment met surface
                  if (IK2VN == 1) then

                     EXTVL = process_space_real(IP1 + (IVAN - 1) * IN1)
                     DEPTH = process_space_real(IP2 + (IVAN - 1) * IN2)
                     RADTOP = process_space_real(IP3 + (IVAN - 1) * IN3)
                     REFLEC = process_space_real(IP8 + (IVAN - 1) * IN8)

                     RADTOP = RADTOP * (1.-REFLEC)
                     RADBOT = RADTOP * exp(-EXTVL * DEPTH)

                     process_space_real(IP9 + (IVAN - 1) * IN9) = RADTOP
                     process_space_real(IP9 + (INAAR - 1) * IN9) = RADBOT
                     process_space_real(IP10 + (IVAN - 1) * IN10) = RADBOT
                     process_space_real(IP11 + (IVAN - 1) * IN11) = EXTVL * DEPTH

                  end if

                  !..............Van segment = water segment zonder surface of bodem
                  if (IK2VN == 2) then

                     EXTVL = process_space_real(IP1 + (IVAN - 1) * IN1)
                     DEPTH = process_space_real(IP2 + (IVAN - 1) * IN2)
                     RADTOP = process_space_real(IP9 + (IVAN - 1) * IN9)

                     RADBOT = RADTOP * exp(-EXTVL * DEPTH)

                     process_space_real(IP9 + (INAAR - 1) * IN9) = RADBOT
                     process_space_real(IP10 + (IVAN - 1) * IN10) = RADBOT
                     process_space_real(IP11 + (IVAN - 1) * IN11) = EXTVL * DEPTH

                  end if

               end if

               !...........Naar segment = inactief
               if (IK1NR == 0) then

                  !              RadTop = RadSurf
                  process_space_real(IP9 + (INAAR - 1) * IN9) = process_space_real(IP3 + (INAAR - 1) * IN3)

                  !              RadBot = Radsurf
                  process_space_real(IP10 + (INAAR - 1) * IN10) = process_space_real(IP3 + (INAAR - 1) * IN3)

                  process_space_real(IP11 + (INAAR - 1) * IN11) = 0.0

                  !...........Naar segment = actief water segment
               else if (IK1NR == 1) then

                  !...........Naar segment = water segment met bodem
                  if (IK2NR == 3) then

                     EXTVL = process_space_real(IP1 + (INAAR - 1) * IN1)
                     DEPTH = process_space_real(IP2 + (INAAR - 1) * IN2)
                     RADTOP = process_space_real(IP7 + (INAAR - 1) * IN7)

                     RADBOT = RADTOP * exp(-EXTVL * DEPTH)

                     process_space_real(IP10 + (INAAR - 1) * IN10) = RADBOT
                     process_space_real(IP11 + (INAAR - 1) * IN11) = EXTVL * DEPTH

                  end if

               end if
            end if

         end do

      end if

      !     the sediment columns

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

      do IK = 1, Coll%current_size

         IWA1 = Coll%set(IK)%fstwatsed
         IWA2 = Coll%set(IK)%lstwatsed
         ITOP = Coll%set(IK)%topsedsed
         IBOT = Coll%set(IK)%botsedsed

         !         average RAD at water-sediment interface

         RADTOP = 0.0
         TOTSURF = 0.0
         do IQ = IWA1, IWA2
            IWATER = IEXPNT(1, IQ)
            IBODEM = IEXPNT(2, IQ)
            RADSURF = process_space_real(IP3 + (IWATER - 1) * IN3)
            SURF = process_space_real(IP5 + (IWATER - 1) * IN5)
            SWEMERSION = nint(process_space_real(IP6 + (IWATER - 1) * IN6))
            RADBOT = process_space_real(IP10 + (IWATER - 1) * IN10)
            if (SWEMERSION == 1) then
               RADTOP = RADTOP + RADSURF * SURF
            else
               RADTOP = RADTOP + RADBOT * SURF
            end if
            TOTSURF = TOTSURF + SURF
         end do
         A_ENH = process_space_real(IP4 + (IWATER - 1) * IN4)
         RADTOP = RADTOP * A_ENH / TOTSURF

         !         extinction over the layers of the column

         do IQ = ITOP, IBOT
            IBODEM = IEXPNT(1, IQ)
            EXTVL = process_space_real(IP1 + (IBODEM - 1) * IN1)
            DEPTH = process_space_real(IP2 + (IBODEM - 1) * IN2)
            if (RADTOP < 1.e-10) then
               RADBOT = 0.0
            else
               RADBOT = RADTOP * exp(-EXTVL * DEPTH)
            end if
            process_space_real(IP9 + (IBODEM - 1) * IN9) = RADTOP
            process_space_real(IP10 + (IBODEM - 1) * IN10) = RADBOT
            RADTOP = RADBOT
         end do

      end do

      return
   end

end module m_clcrad
