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
module m_trsoxy
   use m_waq_precision

   implicit none

contains

   subroutine TRSOXY(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)

      !***********************************************************************
      !
      !     Function : Extra rearation flux towards sediment Drying and Flooding
      !
      !***********************************************************************

      use m_advtra
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

      integer(kind=int_wp) :: SWEMERSION ! 1  in  switch indicating submersion(0) or emersion (1)
      real(kind=real_wp) :: OXY ! 2  in  dissolved oxygen concentration
      real(kind=real_wp) :: OXYSAT ! 3  in  dissolved oxygen saturation concentration
      real(kind=real_wp) :: DEPTH ! 4  in  depth of a segment
      real(kind=real_wp) :: AUXSYS ! 5  in  auxsys conversion from system timer to day
      real(kind=real_wp) :: VDOWN ! 6  in  downward velocity
      real(kind=real_wp) :: CORFLX ! 7  out correction flux

      !     local decalrations

      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5 ! index pointer in process_space_real array
      integer(kind=int_wp) :: IP6, IP7 ! index pointer in process_space_real array
      integer(kind=int_wp) :: IN1, IN2, IN3, IN4, IN5 ! increment in process_space_real array
      integer(kind=int_wp) :: IN6, IN7 ! increment in process_space_real array
      integer(kind=int_wp) :: ISEG ! loop counter segment loop
      integer(kind=int_wp) :: IK ! loop counter bottom columns
      integer(kind=int_wp) :: IQ ! loop counter exchanges
      integer(kind=int_wp) :: IWA1 ! index first water exchange
      integer(kind=int_wp) :: IWA2 ! index last water exchange
      integer(kind=int_wp) :: IVAN ! index from segment in exchange
      integer(kind=int_wp) :: INAAR ! index to segment in exchange

      !     initialise bottom if necessary

      call MAKKO2(IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                  num_exchanges_bottom_dir)

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      !
      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)

      !     zero the output

      do ISEG = 1, num_cells
         process_space_real(IP7) = 0.0
         IP7 = IP7 + IN7
      end do
      IP7 = IPOINT(7)

      !     Loop over kolommen

      do IK = 1, Coll%current_size

         !        Select first column of exchanges for DOWNWARD advection, sediment water exchanges only

         IWA1 = Coll%set(IK)%fstwatsed
         IWA2 = Coll%set(IK)%lstwatsed

         do IQ = IWA1, IWA2

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            SWEMERSION = nint(process_space_real(IP1 + (IVAN - 1) * IN1))

            if (SWEMERSION == 1) then

               OXY = process_space_real(IP2 + (IVAN - 1) * IN2)
               OXYSAT = process_space_real(IP3 + (IVAN - 1) * IN3)
               DEPTH = process_space_real(IP4 + (INAAR - 1) * IN4)
               AUXSYS = process_space_real(IP5 + (IVAN - 1) * IN5)
               VDOWN = process_space_real(IP6 + (IQ - 1) * IN6)

               !              coorection flux is equal to saturated velocity flux minus actual velocity flux, scaled for time and volume

               CORFLX = VDOWN * (OXYSAT - OXY) * AUXSYS / DEPTH

               process_space_real(IP7 + (INAAR - 1) * IN7) = process_space_real(IP7 + (INAAR - 1) * IN7) + CORFLX
               FL(1 + (INAAR - 1) * NOFLUX) = FL(1 + (INAAR - 1) * NOFLUX) + CORFLX

            end if

         end do

      end do

      return
   end

end module m_trsoxy
