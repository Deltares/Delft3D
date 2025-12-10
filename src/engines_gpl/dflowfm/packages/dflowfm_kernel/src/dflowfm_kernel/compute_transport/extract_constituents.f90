!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module m_extract_constituents

   implicit none

   private

   public :: extract_constituents, print_extract_constituents_message

   integer, parameter :: MAX_NUMBER_OF_MESSAGES = 10 ! maximum number of warning messages
   integer, dimension(6) :: number_of_printed_messages = 0

contains

   !> print the message if limited cells are found
   subroutine print_extract_constituents_message()
      use messageHandling, only: msgbuf, msg_flush

      if (any(number_of_printed_messages > 0)) then
         msgbuf = ' '
         call msg_flush()
         write (msgbuf, '(a)') &
            'Salinity, temperature and/or suspended sediment concentration (SSC) were limited in some cells during the simulation.'
         call msg_flush()
      end if

   end subroutine print_extract_constituents_message

   !> extract constituent array and limits values if needed
   subroutine extract_constituents()
      use precision, only: dp, fp
      use m_doforester, only: doforester
      use m_flowparameters, only: jaequili, jalogtransportsolverlimiting, jasal, jasecflow, jatem, &
                                  maxitverticalforestersal, maxitverticalforestertem
      use m_flow, only: hs, kmx, kbot, ktop, ndkx, spirint, vol1
      use m_flowgeom, only: ndx, ndxi, bai_mor
      use m_flowtimes, only: dts
      use m_fm_icecover, only: freezing_temperature
      use m_get_kbot_ktop, only: getkbotktop
      use m_missing, only: dmiss
      use m_physcoef, only: salinity_max, salinity_min, use_salinity_freezing_point, backgroundsalinity, temperature_max, temperature_min
      use m_plotdots, only: numdots
      use m_sediment, only: mxgr, sed, stm_included, stmpar, ssccum, upperlimitssc
      use m_transport, only: isalt, ised1, ispir, itemp, constituents, maserrsed

      use timers, only: timon, timstop, timstrt

      integer :: iconst, grain, k, kk, cells_with_min_limit, cells_with_max_limit, kb, kt
      real(kind=dp) :: minimum_salinity_value
      real(kind=dp) :: freezing_point_temperature ! freezing point temperature [degC]
      integer(4) :: ithndl = 0

      integer, parameter :: IDX_SSC_MIN = 1 ! index of suspended sediment concentration messages for min limits
      integer, parameter :: IDX_SSC_MAX = 2 ! index of suspended sediment concentration messages for max limits
      integer, parameter :: IDX_SAL_MIN = 3 ! index of salinity messages for min limits
      integer, parameter :: IDX_SAL_MAX = 4 ! index of salinity messages for max limits
      integer, parameter :: IDX_TEMP_MIN = 5 ! index of temperature messages for min limits
      integer, parameter :: IDX_TEMP_MAX = 6 ! index of temperature messages for max limits

      if (timon) then
         call timstrt("extract_constituents", ithndl)
      end if

      if (jasecflow > 0 .and. jaequili == 0 .and. kmx == 0) then
         spirint(1:ndkx) = constituents(ispir, 1:ndkx)
      end if

      if (ised1 > 0) then
         cells_with_max_limit = 0
         cells_with_min_limit = 0
         do k = 1, ndkx
            do grain = 1, mxgr
               iconst = ised1 + grain - 1
               if (constituents(iconst, k) < 0.0_dp) then
                  cells_with_min_limit = cells_with_min_limit + 1
                  constituents(iconst, k) = 0.0_dp
               end if

               ! keep track of mass error because of concentration limitation
               if (constituents(iconst, k) > upperlimitssc) then
                  cells_with_max_limit = cells_with_max_limit + 1
                  maserrsed = maserrsed + vol1(k) * (constituents(iconst, k) - upperlimitssc)
                  constituents(iconst, k) = upperlimitssc
               end if
               if (.not. stm_included) then
                  sed(grain, k) = constituents(iconst, k)
               end if
            end do
         end do

         if (jalogtransportsolverlimiting > 0) then
            call print_message(IDX_SSC_MIN, 'Negative SSC', cells_with_min_limit)
            call print_message(IDX_SSC_MAX, 'SSC overshoots', cells_with_max_limit, max_limit=upperlimitssc)
         end if
      end if

      if (jatem > 0) then
         if (temperature_max /= dmiss) then
            cells_with_max_limit = 0
            do k = 1, ndkx
               if (constituents(itemp, k) > temperature_max) then
                  constituents(itemp, k) = temperature_max
                  cells_with_max_limit = cells_with_max_limit + 1
               end if
            end do
            call print_message(IDX_TEMP_MAX, 'Maximum temperature', cells_with_max_limit)
         end if

         cells_with_min_limit = 0

         if (use_salinity_freezing_point) then
            if (isalt > 0) then ! if salinity is modeled, use local salinity to determine freezing point
               do kk = 1, ndx
                  k = ktop(kk) ! only the top layer is checked for freezing point
                  freezing_point_temperature = real(freezing_temperature(real(constituents(isalt, k), fp)), dp)
                  if (constituents(itemp, k) < freezing_point_temperature) then
                     constituents(itemp, k) = freezing_point_temperature
                     cells_with_min_limit = cells_with_min_limit + 1
                  end if
               end do
            else ! if salinity is not modeled, use background salinity to determine freezing point
               freezing_point_temperature = real(freezing_temperature(backgroundsalinity), dp)
               do kk = 1, ndx
                  k = ktop(kk) ! only the top layer is checked for freezing point
                  if (constituents(itemp, k) < freezing_point_temperature) then
                     constituents(itemp, k) = freezing_point_temperature
                     cells_with_min_limit = cells_with_min_limit + 1
                  end if
               end do
            end if
         end if

         if (temperature_min /= dmiss) then
            do k = 1, ndkx
               if (constituents(itemp, k) < temperature_min) then
                  constituents(itemp, k) = temperature_min
                  cells_with_min_limit = cells_with_min_limit + 1
               end if
            end do
         end if

         call print_message(IDX_TEMP_MIN, 'Minimum temperature', cells_with_min_limit)
      end if

      if (jasal > 0) then
         numdots = 0
         if (salinity_max /= dmiss) then
            cells_with_max_limit = 0
            do kk = 1, ndxi
               do k = kbot(kk), ktop(kk)
                  if (constituents(isalt, k) > salinity_max) then
                     constituents(isalt, k) = salinity_max
                     cells_with_max_limit = cells_with_max_limit + 1
                  end if
               end do
            end do
            call print_message(IDX_SAL_MAX, 'Maximum salinity', cells_with_max_limit)
         end if

         cells_with_min_limit = 0
         minimum_salinity_value = huge(1.0_dp)
         do kk = 1, ndxi
            do k = kbot(kk), ktop(kk)
               if (constituents(isalt, k) < salinity_min) then
                  minimum_salinity_value = min(minimum_salinity_value, constituents(isalt, k))
                  constituents(isalt, k) = salinity_min
                  cells_with_min_limit = cells_with_min_limit + 1
               end if
            end do
         end do
         call print_message(IDX_SAL_MIN, 'Minimum salinity', cells_with_min_limit, minimum_salinity_value=minimum_salinity_value)
      end if

      if (jasal > 0 .and. maxitverticalforestersal > 0 .or. jatem > 0 .and. maxitverticalforestertem > 0) then
         call doforester()
      end if
      !
      ! When a cell become dry, keep track of the mass in the water column in ssccum array. This will be accounted
      ! for in the bottom update when the cell becomes wet again. This prevents large concentration gradients and
      ! exploding bed levels.
      if (stm_included .and. ised1 > 0) then
         do grain = 1, mxgr
            do k = 1, ndx
               if (hs(k) <= stmpar%morpar%sedthr) then
                  call getkbotktop(k, kb, kt)
                  ssccum(grain, k) = ssccum(grain, k) + sum(constituents(ISED1 + grain - 1, kb:kt) * vol1(kb:kt)) / &
                                     dts * bai_mor(k)
                  constituents(ISED1 + grain - 1, kb:kt) = 0.0_dp
                  constituents(ISED1 + grain - 1, k) = 0.0_dp
               end if
            end do
         end do
      end if

      if (timon) then
         call timstop(ithndl)
      end if

   end subroutine extract_constituents

   subroutine print_message(index, text, cells_with_limit, max_limit, minimum_salinity_value)
      use messageHandling, only: msgbuf, msg_flush
      use precision, only: dp

      integer, intent(in) :: index !< index of the message to print
      character(len=*), intent(in) :: text !< text of the message to print
      integer, intent(in) :: cells_with_limit !< cells_with_limit
      real(kind=dp), optional, intent(in) :: max_limit !< optional max limit value for the message
      real(kind=dp), optional, intent(in) :: minimum_salinity_value !< optional minimum_salinity_value

      if (cells_with_limit > 0 .and. number_of_printed_messages(index) < MAX_NUMBER_OF_MESSAGES) then
         number_of_printed_messages(index) = number_of_printed_messages(index) + 1
         if (present(max_limit)) then
            write (msgbuf, *) text, ' encountered and limited to ', max_limit, ' in ', &
               cells_with_limit, ' cell(s).'
         else
            write (msgbuf, *) text, ' encountered and limited in ', cells_with_limit, ' cell(s).'
            if (present(minimum_salinity_value)) then
               call msg_flush()
               write (msgbuf, *) 'Minimum salinity encountered = ', minimum_salinity_value
            end if
         end if
         call msg_flush()

         if (number_of_printed_messages(index) == MAX_NUMBER_OF_MESSAGES) then
            write (msgbuf, *) 'Warning limit reached: No more ', text, ' limit messages will be displayed.'
            call msg_flush()
         end if
      end if

   end subroutine print_message

end module m_extract_constituents
