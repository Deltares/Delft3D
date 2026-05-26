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

!> Update the cumulative waq sink source fluxes for the just set timestep.
!!
!! Should be called at the end of each computational timestep. In the waq-output, the cumulative values should be divided
!! by ti_waq, as the cumulative values are multiplied by each timestep dts (necessary because of non-constant timestep).
!!
!! The code uses similair ways to distribute discharges over layers as the calling subroutine setsorsin. Changes in the
!! calling subroutine should also be taken over in this routine!
module m_update_waq_sink_source_fluxes

   use precision, only: dp
   implicit none

   private

   public :: update_waq_sink_source_fluxes

contains

   subroutine update_waq_sink_source_fluxes()
      use m_getkbotktopmax
      use waq
      use m_flow
      use m_flowgeom
      use m_flowtimes
      use m_source_sink, only: source_sinks, num_source_sink, source_sink_cumulative_discharge_waq, source_sink_waq_index

      integer :: k, k1, k2, isrc, ip, ilaysin, ilaysor
      integer :: kksin, kbsin, ktsin, kksor, kbsor, ktsor
      integer :: kkbsin, kktsin, kktxsin, kkbsor, kktsor, kktxsor
      real(8) :: dzss, qsrck, fsor, fsorlay
      real(8), allocatable :: fsin(:)

      do isrc = 1, num_source_sink
         if (source_sink_waq_index(isrc) >= 0) then
            ! If source_sink_waq_index < 0, then the sink source is not in the current domain
            if (waqpar%kmxnxa == 1) then
               ! 2D case
               ip = source_sink_waq_index(isrc) + 1
               if (ip > 0) then
                  source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * source_sinks%discharge(isrc)
               end if
            else
               ! 3D case
               kksin = source_sinks%indices(isrc, 1) ! 2D segment number of sink
               kbsin = source_sinks%indices(isrc, 2) ! actual kbot of sink
               ktsin = source_sinks%indices(isrc, 3) ! actual ktop of sink
               kksor = source_sinks%indices(isrc, 4) ! 2D segment number of source
               kbsor = source_sinks%indices(isrc, 5) ! actual kbot of source
               ktsor = source_sinks%indices(isrc, 6) ! actual ktop source
               if (kksin == 0 .and. kksor /= 0) then
                  ! there is only a source side
                  call getkbotktopmax(kksor, kkbsor, kktsor, kktxsor)
                  dzss = zws(ktsor) - zws(kbsor - 1)
                  do k = kbsor, ktsor
                     if (dzss > epshs) then
                        qsrck = source_sinks%discharge(isrc) * (zws(k) - zws(k - 1)) / dzss
                     else
                        qsrck = source_sinks%discharge(isrc) / (ktsor - kbsor + 1)
                     end if
                     ip = source_sink_waq_index(isrc) + waqpar%ilaggr(kktxsor - k + 1)
                     source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * qsrck
                  end do
               else if (kksin /= 0 .and. kksor == 0) then
                  ! there is only a sink side (used?)
                  call getkbotktopmax(kksin, kkbsin, kktsin, kktxsin)
                  dzss = zws(ktsin) - zws(kbsin - 1)
                  do k = kbsin, ktsin
                     if (dzss > epshs) then
                        qsrck = source_sinks%discharge(isrc) * (zws(k) - zws(k - 1)) / dzss
                     else
                        qsrck = source_sinks%discharge(isrc) / (ktsin - kbsin + 1)
                     end if
                     ip = source_sink_waq_index(isrc) + waqpar%ilaggr(kktxsin - k + 1)
                     source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * qsrck
                  end do
               else if (kksin /= 0 .and. kksor /= 0) then
                  call getkbotktopmax(kksin, kkbsin, kktsin, kktxsin)
                  call getkbotktopmax(kksor, kkbsor, kktsor, kktxsor)
                  if (kbsin == ktsin) then
                     ! sink side has only one layer
                     dzss = zws(ktsor) - zws(kbsor - 1)
                     do k = kbsor, ktsor
                        if (dzss > epshs) then
                           qsrck = source_sinks%discharge(isrc) * (zws(k) - zws(k - 1)) / dzss
                        else
                           qsrck = source_sinks%discharge(isrc) / (ktsor - kbsor + 1)
                        end if
                        ip = source_sink_waq_index(isrc) + waqpar%ilaggr(kktxsin - kbsin + 1) + waqpar%kmxnxa * (waqpar%ilaggr(kktxsor - k + 1) - 1)
                        source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * qsrck
                     end do
                  else if (kbsor == ktsor) then
                     ! sor side has only one layer
                     dzss = zws(ktsin) - zws(kbsin - 1)
                     do k = kbsin, ktsin
                        if (dzss > epshs) then
                           qsrck = source_sinks%discharge(isrc) * (zws(k) - zws(k - 1)) / dzss
                        else
                           qsrck = source_sinks%discharge(isrc) / (ktsin - kbsin + 1)
                        end if
                        ip = source_sink_waq_index(isrc) + waqpar%ilaggr(kktxsin - k + 1) + waqpar%kmxnxa * (waqpar%ilaggr(kktxsor - kbsor + 1) - 1)
                        source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * qsrck
                     end do
                  else
                     ! multiple layers on both side... it's a bit more complicated...
                     ! determine fractions on sink side
                     call realloc(fsin, kmx, keepExisting=.false., fill=0.0_dp)
                     dzss = zws(ktsin) - zws(kbsin - 1)
                     do k = kbsin, ktsin
                        ilaysin = kktxsin - k + 1
                        if (dzss > epshs) then
                           fsin(ilaysin) = (zws(k) - zws(k - 1)) / dzss
                        else
                           fsin(ilaysin) = 1.0_dp / (ktsin - kbsin + 1)
                        end if
                     end do
                     ! distribute sink side fractions over source side
                     do k1 = kbsor, ktsor
                        ilaysor = kktxsor - k1 + 1
                        dzss = zws(ktsor) - zws(kbsor - 1)
                        if (dzss > epshs) then
                           fsor = (zws(k1) - zws(k1 - 1)) / dzss
                        else
                           fsor = 1.0_dp / (ktsor - kbsor + 1)
                        end if
                        do k2 = kbsin, ktsin
                           ilaysin = kktxsin - k2 + 1
                           fsorlay = min(fsin(ilaysin), fsor)
                           fsin(ilaysin) = fsin(ilaysin) - fsorlay
                           fsor = fsor - fsorlay
                           ip = source_sink_waq_index(isrc) + waqpar%ilaggr(ilaysin) + waqpar%kmxnxa * (waqpar%ilaggr(ilaysor) - 1)
                           source_sink_cumulative_discharge_waq(ip) = source_sink_cumulative_discharge_waq(ip) + dts * fsorlay * source_sinks%discharge(isrc)
                        end do
                     end do
                  end if
               end if
            end if
         end if
      end do

   end subroutine update_waq_sink_source_fluxes

end module m_update_waq_sink_source_fluxes
