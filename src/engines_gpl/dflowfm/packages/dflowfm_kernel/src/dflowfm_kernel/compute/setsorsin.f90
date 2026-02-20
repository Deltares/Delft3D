!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!
!

module m_setsorsin

   implicit none

   private

   public :: setsorsin

contains

   !> Compute and set source and sink values for the 'intake-outfall' structures.
   subroutine setsorsin()
      use precision, only: dp
      use m_flow, only: srsn, num_source_sink, source_sink_indices, source_sink_water_discharge, source_sink_discharge, kmx, source_sink_z_bot, dmiss, zws, source_sink_z_top, vol1, source_sink_extraction_warning, source_sink_constituents, qin, epshs, source_sink_name
      use m_get_kbot_ktop, only: getkbotktop
      use m_flowtimes, only: dts
      use m_transport, only: NUMCONST, constituents
      use MessageHandling, only: LEVEL_WARN, msgbuf, mess
      use m_partitioninfo, only: jampi, reduce_srsn

      integer :: n, kk, k, kb, kt, kk2, ku, numvals, L
      real(kind=dp) :: qsrck, qsrckk, dzss
      real(kind=dp) :: frac = 0.5_dp ! cell volume fraction that can at most be extracted in one step

      srsn = 0.0_dp
      do n = 1, num_source_sink
         kk = source_sink_indices(1, n) ! 2D pressure cell nr, From side, 0 = out of all, -1 = in other domain, > 0, own domain
         kk2 = source_sink_indices(4, n) ! 2D pressure cell nr, To   side, 0 = out of all, -1 = in other domain, > 0, own domain
         source_sink_water_discharge(n) = source_sink_discharge(1, n)
         if (kk > 0) then ! FROM point
            if (kmx > 0) then
               call getkbotktop(kk, kb, kt)
               if (source_sink_z_bot(1, n) == dmiss) then
                  k = kb
                  ku = kt
               else
                  do k = kb, kt
                     if (zws(k) > source_sink_z_bot(1, n) .or. k == kt) then
                        exit
                     end if
                  end do
                  if (source_sink_z_top(1, n) == dmiss) then
                     ku = k
                  else
                     do ku = kb, kt
                        if (zws(ku) > source_sink_z_top(1, n) .or. ku == kt) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = kk
               kt = kk
               ku = kk ! in 2D, volume cell nr = pressure cell nr
            end if
            source_sink_indices(2, n) = k ! store kb of src
            source_sink_indices(3, n) = ku !
            if (source_sink_water_discharge(n) > 0) then ! Reduce if flux pos

               do k = source_sink_indices(2, n), kt
                  srsn(1, n) = srsn(1, n) + vol1(k)
                  do L = 1, numconst
                     srsn(1 + L, n) = srsn(1 + L, n) + constituents(L, k) * vol1(k)
                  end do
                  source_sink_indices(3, n) = k
                  if (frac * srsn(1, n) / dts > abs(source_sink_water_discharge(n))) then
                     exit
                  end if
               end do
               if (srsn(1, n) > 0.0_dp) then
                  do L = 1, numconst
                     srsn(1 + L, n) = srsn(1 + L, n) / srsn(1, n)
                  end do
               end if
               do k = source_sink_indices(2, n), source_sink_indices(3, n)
                  !if (jasal > 0) constituents(isalt,k) = srsn(1+isalt,n)
                  !if (temperature_model /= TEMPERATURE_MODEL_NONE) constituents(itemp,k) = srsn(1+itemp,n)
                  do L = 1, numconst
                     constituents(L, k) = srsn(L + 1, n)
                  end do
               end do

            end if
         end if

         if (kk2 > 0) then ! TO point
            if (kmx > 0) then
               call getkbotktop(kk2, kb, kt)
               if (source_sink_z_bot(2, n) == dmiss) then
                  k = kb
                  ku = kt
               else
                  do k = kb, kt
                     if (zws(k) > source_sink_z_bot(2, n) .or. k == kt) then
                        exit
                     end if
                  end do
                  if (source_sink_z_top(2, n) == dmiss) then
                     ku = k
                  else
                     do ku = kb, kt
                        if (zws(ku) > source_sink_z_top(2, n) .or. ku == kt) then
                           exit
                        end if
                     end do
                  end if
               end if
            else
               k = kk2
               kt = kk2
               ku = kk2 ! in 2D, volume cell nr = pressure cell nr
            end if
            source_sink_indices(5, n) = k
            source_sink_indices(6, n) = ku
            if (source_sink_water_discharge(n) < 0) then ! Reduce if flux neg

               do k = source_sink_indices(5, n), kt
                  srsn(1 + numconst + 1, n) = srsn(1 + numconst + 1, n) + vol1(k)
                  do L = 1, numconst
                     srsn(1 + numconst + 1 + L, n) = srsn(1 + numconst + 1 + L, n) + constituents(L, k) * vol1(k)
                  end do
                  source_sink_indices(6, n) = k
                  if (frac * srsn(1 + numconst + 1, n) / dts > abs(source_sink_water_discharge(n))) then
                     exit
                  end if
               end do
               if (srsn(1 + numconst + 1, n) > 0.0_dp) then
                  do L = 1, numconst
                     srsn(1 + numconst + 1 + L, n) = srsn(1 + numconst + 1 + L, n) / srsn(1 + numconst + 1, n)
                  end do
               end if
               do k = source_sink_indices(5, n), source_sink_indices(6, n)
                  !if (jasal > 0) constituents(isalt,k) = srsn(1+numconst+1+isalt,n)
                  !if (temperature_model /= TEMPERATURE_MODEL_NONE) constituents(itemp,k) = srsn(1+numconst+1+itemp,n)
                  do L = 1, numconst
                     constituents(L, k) = srsn(1 + numconst + 1 + L, n)
                  end do
               end do

            end if
         end if

      end do

      if (jampi > 0) then
         numvals = 2 * (1 + numconst)
         call reduce_srsn(numvals, num_source_sink, srsn)
      end if

      source_sink_extraction_warning = 0
      do n = 1, num_source_sink
         source_sink_water_discharge(n) = source_sink_discharge(1, n)
         do L = 1, numconst
            source_sink_constituents(L, n) = source_sink_discharge(L + 1, n)
         end do

         kk = source_sink_indices(1, n) ! 2D pressure cell nr
         qsrck = source_sink_water_discharge(n)
         if (kk /= 0 .and. qsrck > 0) then ! Extract FROM 1
            if (frac * srsn(1, n) / dts < abs(qsrck)) then
               qsrck = frac * srsn(1, n) / dts
               source_sink_extraction_warning(n) = 1
            end if
         end if

         kk2 = source_sink_indices(4, n) ! 2D pressure cell nr
         if (kk2 /= 0 .and. qsrck < 0) then ! Extract From 2
            if (frac * srsn(1 + numconst + 1, n) / dts < abs(qsrck)) then
               qsrck = -frac * srsn(1 + numconst + 1, n) / dts
               source_sink_extraction_warning(n) = 2
            end if
         end if

         source_sink_water_discharge(n) = qsrck

         if (kk * kk2 /= 0) then ! Coupled stuff
            if (qsrck > 0) then ! FROM k to k2
               do L = 1, numconst
                  source_sink_constituents(L, n) = source_sink_constituents(L, n) + srsn(1 + L, n)
               end do
            else if (qsrck < 0) then ! FROM k2 to k
               do L = 1, numconst
                  source_sink_constituents(L, n) = source_sink_constituents(L, n) + srsn(1 + numconst + 1 + L, n)
               end do
            end if
         end if

         if (kk > 0) then ! FROM Point
            qsrckk = source_sink_water_discharge(n)
            qin(kk) = qin(kk) - qsrckk ! add to 2D pressure cell nr
            do k = source_sink_indices(2, n), source_sink_indices(3, n)
               if (kmx > 0) then
                  dzss = zws(source_sink_indices(3, n)) - zws(source_sink_indices(2, n) - 1)
                  if (dzss > epshs) then
                     qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                  else
                     qsrck = qsrckk / (source_sink_indices(3, n) - source_sink_indices(2, n) + 1)
                  end if
                  qin(k) = qin(k) - qsrck
               end if
            end do
         end if

         if (kk2 > 0) then ! TO Point
            qsrckk = source_sink_water_discharge(n)
            qin(kk2) = qin(kk2) + qsrckk ! add to 2D pressure cell nr
            do k = source_sink_indices(5, n), source_sink_indices(6, n)
               if (kmx > 0) then
                  dzss = zws(source_sink_indices(6, n)) - zws(source_sink_indices(5, n) - 1)
                  if (dzss > epshs) then
                     qsrck = qsrckk * (zws(k) - zws(k - 1)) / dzss
                  else
                     qsrck = qsrckk / (source_sink_indices(6, n) - source_sink_indices(5, n) + 1)
                  end if
                  qin(k) = qin(k) + qsrck
               end if
            end do
         end if

      end do

      do n = 1, num_source_sink
         if (source_sink_extraction_warning(n) == 1) then
            write (msgbuf, *) 'Extraction flux larger than cell volume at point 1 of : ', trim(source_sink_name(n))
            call mess(LEVEL_WARN, msgbuf)
         else if (source_sink_extraction_warning(n) == 2) then
            write (msgbuf, *) 'Extraction flux larger than cell volume at point 2 of : ', trim(source_sink_name(n))
            call mess(LEVEL_WARN, msgbuf)
         end if
      end do

   end subroutine setsorsin

end module m_setsorsin
