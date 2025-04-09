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
module m_dayrad
   use m_waq_precision

   implicit none

contains

   subroutine DAYRAD(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute

      !***********************************************************************
      !
      !     Function : Computes irradiance over the day from daily average irradiance
      !                from "Zonnestraling in Nederland",
      !                C.A.Velds, Thieme/KNMI, 1992, 1st imp., ISBN 90-5210-140-X
      !
      !***********************************************************************

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

      real(kind=real_wp) :: RADSURF ! 1  in  irradiation at the water surface            (W/m2)
      real(kind=real_wp) :: TIME ! 2  in  DELWAQ time                                  (scu)
      double precision :: LATITUDE ! 3  in  latitude of study area                   (degrees)
      real(kind=real_wp) :: REFDAY ! 4  in  daynumber of reference day simulation          (d)
      real(kind=real_wp) :: AUXSYS ! 5  in  ratio between days and system clock        (scu/d)
      real(kind=real_wp) :: DAYRADSURF ! 6  out actual irradiance over the day              (W/m2)
      double precision :: RADDAY ! 7  out actual irradiance                           (W/m2)
      double precision :: RADTIME ! 8  out actual irradiance                           (W/m2)

      !     local decalrations

      double precision, parameter :: SIN50M = -1.454389765d-2
      double precision, parameter :: E = 1.721420632d-2
      double precision, parameter :: PI = 3.141592654d0
      double precision, parameter :: I0 = 1367.d0
      double precision :: DAYNR
      double precision :: HOUR
      double precision :: RDIST
      double precision :: OMEGA
      double precision :: DECLIN
      double precision :: OMEGA0
      double precision :: SIN_DECLIN
      double precision :: SIN_LATITU
      double precision :: SIN_OMEGA0
      double precision :: COS_DECLIN
      double precision :: COS_LATITU
      double precision :: COS_OMEGA
      logical :: VARFLG
      integer(kind=int_wp) :: ISEG
      integer(kind=int_wp) :: IKMRK1
      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8
      integer(kind=int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)

      !
      VARFLG = .true.
      if (IN2 == 0 .and. IN3 == 0 .and. IN4 == 0 .and. &
          IN5 == 0) then
         !
         VARFLG = .false.
         !
         TIME = process_space_real(IP2)
         !        Conversion Latitude to rads
         LATITUDE = process_space_real(IP3) / 360 * 2 * PI
         REFDAY = process_space_real(IP4)
         AUXSYS = process_space_real(IP5)

         !        Conversion time to daynumbers relative to refday
         DAYNR = mod(TIME / AUXSYS + REFDAY, 365.)
         HOUR = mod(TIME / AUXSYS + REFDAY, 1.) * 24.
         RDIST = 1.d0 + .033 * cos(E * DAYNR)
         OMEGA = abs(12.d0 - HOUR) * PI / 12.d0

         DECLIN = 6.918d-3 - &
                  3.99912d-1 * cos(E * DAYNR) - &
                  6.758d-3 * cos(2.0d0 * E * DAYNR) - &
                  2.697d-3 * cos(3.0d0 * E * DAYNR) + &
                  7.0257d-2 * sin(E * DAYNR) + &
                  9.07d-4 * sin(2.0d0 * E * DAYNR) + &
                  1.480d-3 * sin(3.0d0 * E * DAYNR)

         !        compute actual irradiance

         OMEGA0 = acos(-tan(DECLIN) * tan(LATITUDE))
         SIN_DECLIN = sin(DECLIN)
         SIN_LATITU = sin(LATITUDE)
         SIN_OMEGA0 = sin(OMEGA0)
         COS_DECLIN = cos(DECLIN)
         COS_LATITU = cos(LATITUDE)
         COS_OMEGA = cos(OMEGA)
         RADTIME = I0 * RDIST * (SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * COS_OMEGA)
         RADTIME = max(0.0d0, RADTIME)
         RADDAY = I0 / PI * RDIST * (OMEGA0 * SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * SIN_OMEGA0)
      end if
      !
      do ISEG = 1, num_cells
         call extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

         RADSURF = process_space_real(IP1)

         if (VARFLG) then
            !
            TIME = process_space_real(IP2)
            !              Conversion Latitude to rads
            LATITUDE = process_space_real(IP3) / 360 * 2 * PI
            REFDAY = process_space_real(IP4)
            AUXSYS = process_space_real(IP5)

            !              Conversion time to daynumbers relative to refday
            DAYNR = mod(TIME / AUXSYS + REFDAY, 365.)
            HOUR = mod(TIME / AUXSYS + REFDAY, 1.) * 24.
            RDIST = 1.d0 + .033 * cos(E * DAYNR)
            OMEGA = abs(12.d0 - HOUR) * PI / 12.d0

            DECLIN = 6.918d-3 - &
                     3.99912d-1 * cos(E * DAYNR) - &
                     6.758d-3 * cos(2.0d0 * E * DAYNR) - &
                     2.697d-3 * cos(3.0d0 * E * DAYNR) + &
                     7.0257d-2 * sin(E * DAYNR) + &
                     9.07d-4 * sin(2.0d0 * E * DAYNR) + &
                     1.480d-3 * sin(3.0d0 * E * DAYNR)

            !              compute actual irradiance

            OMEGA0 = acos(-tan(DECLIN) * tan(LATITUDE))
            SIN_DECLIN = sin(DECLIN)
            SIN_LATITU = sin(LATITUDE)
            SIN_OMEGA0 = sin(OMEGA0)
            COS_DECLIN = cos(DECLIN)
            COS_LATITU = cos(LATITUDE)
            COS_OMEGA = cos(OMEGA)
            RADTIME = I0 * RDIST * (SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * COS_OMEGA)
            RADTIME = max(0.0d0, RADTIME)
            RADDAY = I0 / PI * RDIST * (OMEGA0 * SIN_DECLIN * SIN_LATITU + COS_DECLIN * COS_LATITU * SIN_OMEGA0)
         end if
         !
         DAYRADSURF = RADTIME * RADSURF / RADDAY

         process_space_real(IP6) = DAYRADSURF
         process_space_real(IP7) = RADTIME
         process_space_real(IP8) = RADDAY
         !
         !        ENDIF
         !
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         !
      end do

      return
      !
   end

end module m_dayrad
