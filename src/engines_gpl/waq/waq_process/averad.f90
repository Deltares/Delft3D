!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

      !     from process_space_real array

      !     8 inputs, 3 outputs.

      integer(kind=int_wp) :: ip1 !<REAL RADSURF        input 1, actual irradiation at the water surface            (W/m2)
      integer(kind=int_wp) :: ip2 !<REAL AveRadTIni     input 2, starting time for average irradiance      TINIT       (d)
      integer(kind=int_wp) :: ip3 !<REAL AveRadPeri     input 3, averaging period of irradiance    PERIOD           (d)
      integer(kind=int_wp) :: ip4 !<REAL ITIME          input 4, DELWAQ time                                        (s)
      integer(kind=int_wp) :: ip5 !<REAL DELT           input 5, timestep for processes          (d)
      integer(kind=int_wp) :: ip6 !<REAL AuxSys         input 6, ratio between days and system clock   (scu/d, i.e. system clock unit/day)
      integer(kind=int_wp) :: ip7 !<REAL SumAveRad      input 7,   Work array for summing over time  (W/m2)
      integer(kind=int_wp) :: ip8 !<REAL SumAveRadT     input 8,   Count of times   TCOUNT  (d)
      integer(kind=int_wp) :: ip9 !<REAL  SumAveRad     output 9,   Work array for summing over time  (W/m2)
      integer(kind=int_wp) :: ip10 !<REAL SumAveRadT    output 10,  Count of times   TCOUNT  (d)
      integer(kind=int_wp) :: ip11 !<REAL RadSurfAve    output 11,  average irradiance over the period              (W/m2)

      integer(kind=int_wp) :: in1, in2, in3, in4, in5, &
                              in6, in7, in8, in9, in10, in11
      integer(kind=int_wp) :: ikmrk, iseg
      integer(kind=int_wp) :: iaction, lunrep
      integer(kind=int_wp) :: attrib
      real(kind=real_wp) :: tinit, period, time, delt, tcount, auxsys

      integer(kind=int_wp), parameter :: maxwarn = 50
      integer(kind=int_wp), save :: nowarn = 0

      call get_log_unit_number(lunrep)

      !     IACTION is in 3 parts. 0, 2, 3.

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)
      ip8 = ipoint(8)
      ip9 = ipoint(9)
      ip10 = ipoint(10)
      ip11 = ipoint(11)

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)
      in5 = increm(5)
      in6 = increm(6)
      in7 = increm(7)
      in8 = increm(8)
      in9 = increm(9)
      in10 = increm(10)
      in11 = increm(11)

      tinit = process_space_real(ip2)
      period = process_space_real(ip3)
      time = process_space_real(ip4)
      delt = process_space_real(ip5)
      auxsys = process_space_real(ip6)

      if (period < delt) then
         call write_error_message('AveRadSurf: Period of averaging should be larger than DELWAQ time step.')
      end if

      !
      ! TIME in second should be changed to TIME in days.
      !
      time = time / auxsys
      !
      !      Start and stop criteria are somewhat involved:
      !      - The first time for the first period is special, as this
      !        is the only time there is no previous period.
      !      - If there is a previous period, update the averages
      !        for that period and reset the accumulative values
      !        for the next
      !
      !      To formulate the ideas more clearly:
      !      - The first period is a closed interval
      !      - All other periods are half-open intervals (the last time
      !        of the previous period should not be reused.)
      !
      iaction = 0
      if (time >= tinit - 0.5 * delt) then
         iaction = 2
         if (time <= tinit + 0.5 * delt) then
            do iseg = 1, num_cells
               process_space_real(ip9) = 0.0
               process_space_real(ip10) = 0.0
               ip9 = ip9 + in9
               ip10 = ip10 + in10
            end do
            ip9 = ipoint(9)
            ip10 = ipoint(10)
         end if
      end if

      if (time >= tinit + period - 0.5 * delt .and. time <= tinit + period + 0.5 * delt) then
         iaction = 3
      end if

      if (iaction == 0) return

      do iseg = 1, num_cells
         !
         !           Keep track of the time within the current quantile specification
         !           that each segment is active
         !
         tcount = process_space_real(ip8) + delt
         process_space_real(ip10) = tcount

         process_space_real(ip9) = process_space_real(ip7) + process_space_real(ip1) * delt

         !
         !        Always do the final processing whether the segment is active at this moment or not
         !

         if (iaction == 3) then
            if (tcount > 0.0) then
               process_space_real(ip11) = process_space_real(ip9) / tcount
            else
               process_space_real(ip11) = 0.0

               if (nowarn < maxwarn) then
                  call extract_waq_attribute(3, iknmrk(iseg), attrib)
                  if (attrib /= 0) then
                     nowarn = nowarn + 1
                     write (lunrep, '(a,i0)') 'Periodic average of RadSurf could not be determined for segment ', ISEG
                     write (lunrep, '(a)') '    - division by zero. Average set to zero'

                     if (nowarn == maxwarn) then
                        write (lunrep, '(a)') '(Further messages suppressed)'
                     end if
                  end if
               end if
            end if

            !
            !           Reset for the next round
            !

            process_space_real(ip9) = 0.0
            process_space_real(ip10) = 0.0

         end if

         ip1 = ip1 + in1
         ip7 = ip7 + in7
         ip8 = ip8 + in8
         ip9 = ip9 + in9
         ip10 = ip10 + in10
         ip11 = ip11 + in11
      end do
      !
      !     Be sure to also reset the initial time, so that we can restart the
      !     averaging for the next period
      !
      if (iaction == 3) then
         process_space_real(ip2) = tinit + period
      end if

      return
   end
end module m_averad
