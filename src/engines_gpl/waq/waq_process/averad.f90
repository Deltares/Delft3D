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
module m_averad
   use m_waq_precision

   implicit none

contains

   subroutine averad(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute
      use m_logger_helper, only: get_log_unit_number, write_error_message, stop_with_error

      implicit none

      !     arguments

      real(kind=real_wp) :: process_space_real(*) ! in/out input-output array space to be adressed with IPOINT/INCREM
      real(kind=real_wp) :: fl(*) ! in/out flux array
      integer(kind=int_wp) :: ipoint(*) ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
      integer(kind=int_wp) :: increm(*) ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
      integer(kind=int_wp) :: num_cells ! in     number of segments
      integer(kind=int_wp) :: noflux ! in     total number of fluxes (increment in FL array)
      integer(kind=int_wp) :: iexpnt(4, *) ! in     exchange pointer table
      integer(kind=int_wp) :: iknmrk(*) ! in     segment features array
      integer(kind=int_wp) :: num_exchanges_u_dir ! in     number of exchanges in first direction
      integer(kind=int_wp) :: num_exchanges_v_dir ! in     number of exchanges in second direction
      integer(kind=int_wp) :: num_exchanges_z_dir ! in     number of exchanges in third direction
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! in     number of exchanges in fourth direction

      ! from process_space_real array
      ! Note: input 4 = output 1 = SumAveRad, to make sure that Delwaq stores it as a spatial variable.

      integer(kind=int_wp) :: ip1 !<REAL RADSURF        input 1, actual irradiation at the water surface (W/m2)
      integer(kind=int_wp) :: ip2 !<REAL AveRadPeri     input 2, averaging AveRadPeri of irradiance (d)
      integer(kind=int_wp) :: ip3 !<REAL DELT           input 3, timestep for processes (d)
      integer(kind=int_wp) :: ip4 !<REAL SumAveRad      input 4, work array for summing over time (W/m2)
      integer(kind=int_wp) :: ip5 !<REAL SumAveRadT     input 5, count of time SumAveRadT (d)

      integer(kind=int_wp) :: ip6 !<REAL SumAveRad     output 1, work array for summing over time (W/m2)
      integer(kind=int_wp) :: ip7 !<REAL RadSurfAve    output 2, average irradiance over the AveRadPeri (W/m2)

      integer(kind=int_wp) :: in1, in2, in3, in4, in5, in6, in7
      integer(kind=int_wp) :: ikmrk, iseg
      integer(kind=int_wp) :: iaction, lunrep
      integer(kind=int_wp) :: attrib
      real(kind=real_wp) :: AveRadPeri, delt, SumAveRadT

      integer(kind=int_wp), parameter :: maxwarn = 50
      integer(kind=int_wp), save :: nowarn = 0
      logical, save :: first = .true.

      call get_log_unit_number(lunrep)

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)   ! Note that this is a work array - identical to ip4
      ip7 = ipoint(7)

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)
      in5 = increm(5)
      in6 = increm(6)
      in7 = increm(7)

      delt = process_space_real(ip3)
      AveRadPeri = process_space_real(ip2)
      SumAveRadT = process_space_real(ip5)

      ! Initalize the accumulative values to zero
      if (first) then
         if (in2 /= 0) then
            call write_error_message('AveRadSurf: AveRadPeri should be a constant!')
         end if
         if (AveRadPeri < delt) then
            call write_error_message('AveRadSurf: AveRadPeri should be larger than DELWAQ time step!')
         end if
         ! Initialize the accumulative values for the first time
         do iseg = 1, num_cells
            process_space_real(ip4) = 0.0 ! SumAveRad
            ! Increase the pointer
            ip4 = ip4 + in4
         end do
         ! Reset the pointer
         ip4 = ipoint(4)
         ! Reset the time accumulator
         SumAveRadT = 0.0
         first = .false.
      end if

      ! When the end of the period is reached, calculate the average, and reset the accumulative values
      ! Note: ip4 and ip6 are pointing to the same work array (this is a special feature in the
      !       DELWAQ processes library)
      if (SumAveRadT >= AveRadPeri - 0.5 * delt) then
         do iseg = 1, num_cells
            process_space_real(ip7) = process_space_real(ip4) / SumAveRadT ! RadSurfAve = SumAveRad / SumAveRadT
            process_space_real(ip4) = 0.0 ! SumAveRad = 0.0
            ! Increase the pointers
            ip4 = ip4 + in4
            ip7 = ip7 + in7
         end do
         ! Reset the pointers
         ip4 = ipoint(4)
         ip7 = ipoint(7)
         ! Reset the time accumulator
         SumAveRadT = 0.0
      end if

      ! For every time step, add the radiation to the accumulative value and increase the time accumulator
      SumAveRadT = SumAveRadT + delt
      do iseg = 1, num_cells
         process_space_real(ip6) = process_space_real(ip6) + process_space_real(ip1) * delt ! SumAveRad = SumAveRad + RadSurf * DELT
         ! Increase the pointers
         ip1 = ip1 + in1
         ip6 = ip6 + in6
      end do
      process_space_real(ip5) = SumAveRadT
   end
end module m_averad
