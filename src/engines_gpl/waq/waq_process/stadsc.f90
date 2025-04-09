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
module m_stadsc
   use m_waq_precision
   use m_logger_helper, only: get_log_unit_number

   implicit none

contains

   subroutine stadsc(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_extract_waq_attribute

      !>\file
      !>       Mean, min, max, stdev of a variable during a certain time

      !
      !     Description of the module :
      !
      ! Name    T   L I/O   Description                                  Units
      ! ----    --- -  -    -------------------                          -----
      !
      ! CONC           I    Concentration of the substance            1
      ! TSTART         I    Start of statistical period               2
      ! TSTOP          I    Stop of statistical period                3
      ! TIME           I    Time in calculation                       4
      ! DELT           I    Timestep                                  5
      !
      ! TCOUNT         O    Count of times (must be imported!)        6
      ! CMAX           O    Maximum value over the given period       7
      ! CMIN           O    Minimum value over the given period       8
      ! CMEAN          O    Mean value over the given period          9
      ! CSTDEV         O    Standard deviation over the given period 10
      !

      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit none

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      !
      integer(kind=int_wp) :: IP1, IP2, IP3, IP4, IP5, &
                              IP6, IP7, IP8, IP9, IP10, &
                              IN1, IN2, IN3, IN4, IN5, &
                              IN6, IN7, IN8, IN9, IN10
      integer(kind=int_wp) :: ISEG
      integer(kind=int_wp) :: IACTION, lunrep
      integer(kind=int_wp) :: ATTRIB
      real(kind=real_wp) :: TSTART, TSTOP, TIME, DELT, TCOUNT
      real(kind=real_wp) :: CDIFF

      integer(kind=int_wp), parameter :: MAXWARN = 50
      integer(kind=int_wp), save :: NOWARN = 0

      call get_log_unit_number(lunrep)

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

      !
      !     There are five cases, defined by the time:
      !                        TIME <  TSTART-0.5*DELT : do nothing
      !     TSTART-0.5*DELT <= TIME <  TSTART+0.5*DELT : initialise
      !     TSTART          <  TIME <  TSTOP           : accumulate
      !     TSTOP           <= TIME <  TSTOP+0.5*DELT  : finalise
      !     TSTOP+0.5*DELT  <  TIME                    : do nothing
      !
      !     (Use a safe margin)
      !
      TSTART = process_space_real(IP2)
      TSTOP = process_space_real(IP3)
      TIME = process_space_real(IP4)
      DELT = process_space_real(IP5)

      !
      !      Start and stop criteria are somewhat involved. Be careful
      !      to avoid spurious calculations (initial and final) when
      !      none is expected.
      !      Notes:
      !      - The initial value for TCOUNT must be 0.0
      !      - Time is expected to be the model time (same time frame
      !        as the start and stop times of course)
      !      - Check that the NEXT timestep will not exceed the stop time,
      !        otherwise this is the last one
      !
      IACTION = 0
      if (TIME >= TSTART - 0.5 * DELT .and. TIME <= TSTOP + 0.5 * DELT) then
         IACTION = 2
         if (TIME <= TSTART + 0.5 * DELT) then
            do ISEG = 1, num_cells
               IP6 = IPOINT(6) + (ISEG - 1) * INCREM(6)
               IP7 = IPOINT(7) + (ISEG - 1) * INCREM(7)
               IP8 = IPOINT(8) + (ISEG - 1) * INCREM(8)
               IP9 = IPOINT(9) + (ISEG - 1) * INCREM(9)
               IP10 = IPOINT(10) + (ISEG - 1) * INCREM(10)
               process_space_real(IP6) = 0.0
               process_space_real(IP7) = 0.0
               process_space_real(IP8) = 0.0
               process_space_real(IP9) = 0.0
               process_space_real(IP10) = 0.0
            end do
         end if
      end if

      if (TIME >= TSTOP - 0.5 * DELT .and. TIME <= TSTOP + 0.5 * DELT) then
         IACTION = 3
      end if

      if (IACTION == 0) return

      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10 = IPOINT(10)

      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            !           Keep track of the time within the current descriptive statistics specification
            !           that each segment is active
            !
            TCOUNT = process_space_real(IP6) + 1.0
            process_space_real(IP6) = TCOUNT
            !
            if (TCOUNT == 1.0) then
               process_space_real(IP7) = process_space_real(IP1)
               process_space_real(IP8) = process_space_real(IP1)
            else
               process_space_real(IP7) = max(process_space_real(IP7), process_space_real(IP1))
               process_space_real(IP8) = min(process_space_real(IP8), process_space_real(IP1))
            end if
            process_space_real(IP9) = process_space_real(IP9) + process_space_real(IP1)
            process_space_real(IP10) = process_space_real(IP10) + process_space_real(IP1)**2
         end if

         !
         !        Always do the final processing whether the segment is active at this moment or not
         !
         if (IACTION == 3) then
            TCOUNT = process_space_real(IP6)
            if (TCOUNT > 0.0) then
               process_space_real(IP9) = process_space_real(IP9) / TCOUNT
            else
               process_space_real(IP9) = 0.0
               process_space_real(IP10) = 0.0

               if (NOWARN < MAXWARN) then
                  call extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                  if (ATTRIB /= 0) then
                     NOWARN = NOWARN + 1
                     write (lunrep, '(a,i0)') 'Average could not be determined for segment ', ISEG
                     write (lunrep, '(a)') '    - segment not active in the given period. Average and standard deviation set to zero'

                     if (NOWARN == MAXWARN) then
                        write (lunrep, '(a)') '(Further messages suppressed)'
                     end if
                  end if
               end if
            end if
            if (TCOUNT > 1.0) then
               !
               !              If we do not have the standard deviation or the mean,
               !              then the calculation may fail (horribly). We detect
               !              whether the mean and the standard deviation are there
               !              by examining the increments.
               !              Be tolerant to possible negative values, there may be
               !              some roundoff errors!
               !
               if (IN9 /= 0 .and. IN10 /= 0) then
                  CDIFF = (process_space_real(IP10) - TCOUNT * process_space_real(IP9)**2)
                  process_space_real(IP10) = sqrt(max(CDIFF, 0.0) / (TCOUNT - 1.0))
               end if
            else
               process_space_real(IP10) = 0.0

               if (NOWARN < MAXWARN) then
                  call extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                  if (ATTRIB /= 0) then
                     NOWARN = NOWARN + 1
                     write (lunrep, '(a,i0)') 'Standard deviation could not be determined for segment ', ISEG
                     write (lunrep, '(a)') '    - not enough values. Standard deviation set to zero'

                     if (NOWARN == MAXWARN) then
                        write (lunrep, '(a)') '(Further messages suppressed)'
                     end if
                  end if
               end if
            end if
         end if

         IP1 = IP1 + IN1
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP9 = IP9 + IN9
         IP10 = IP10 + IN10

      end do

      return
   end

end module m_stadsc
