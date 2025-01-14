!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

      integer :: k, k1, k2, isrc, ip, ilaysin, ilaysor
      integer :: kksin, kbsin, ktsin, kksor, kbsor, ktsor
      integer :: kkbsin, kktsin, kktxsin, kkbsor, kktsor, kktxsor
      real(8) :: dzss, qsrck, fsor, fsorlay
      real(8), allocatable :: fsin(:)

      do isrc = 1, numsrc
         if (ksrcwaq(isrc) >= 0) then
            ! If ksrcwaq < 0, then the sink source is not in the current domain
            if (waqpar%kmxnxa == 1) then
               ! 2D case
               ip = ksrcwaq(isrc) + 1
               if (ip > 0) then
                  qsrcwaq(ip) = qsrcwaq(ip) + dts * qsrc(isrc)
               end if
            else
               ! 3D case
               kksin = ksrc(1, isrc) ! 2D segment number of sink
               kbsin = ksrc(2, isrc) ! actual kbot of sink
               ktsin = ksrc(3, isrc) ! actual ktop of sink
               kksor = ksrc(4, isrc) ! 2D segment number of source
               kbsor = ksrc(5, isrc) ! actual kbot of source
               ktsor = ksrc(6, isrc) ! actual ktop source
               if (kksin == 0 .and. kksor /= 0) then
                  ! there is only a source side
                  call getkbotktopmax(kksor, kkbsor, kktsor, kktxsor)
                  dzss = zws(ktsor) - zws(kbsor - 1)
                  do k = kbsor, ktsor
                     if (dzss > epshs) then
                        qsrck = qsrc(isrc) * (zws(k) - zws(k - 1)) / dzss
                     else
                        qsrck = qsrc(isrc) / (ktsor - kbsor + 1)
                     end if
                     ip = ksrcwaq(isrc) + waqpar%ilaggr(kktxsor - k + 1)
                     qsrcwaq(ip) = qsrcwaq(ip) + dts * qsrck
                  end do
               else if (kksin /= 0 .and. kksor == 0) then
                  ! there is only a sink side (used?)
                  call getkbotktopmax(kksin, kkbsin, kktsin, kktxsin)
                  dzss = zws(ktsin) - zws(kbsin - 1)
                  do k = kbsin, ktsin
                     if (dzss > epshs) then
                        qsrck = qsrc(isrc) * (zws(k) - zws(k - 1)) / dzss
                     else
                        qsrck = qsrc(isrc) / (ktsin - kbsin + 1)
                     end if
                     ip = ksrcwaq(isrc) + waqpar%ilaggr(kktxsin - k + 1)
                     qsrcwaq(ip) = qsrcwaq(ip) + dts * qsrck
                  end do
               else if (kksin /= 0 .and. kksor /= 0) then
                  call getkbotktopmax(kksin, kkbsin, kktsin, kktxsin)
                  call getkbotktopmax(kksor, kkbsor, kktsor, kktxsor)
                  if (kbsin == ktsin) then
                     ! sink side has only one layer
                     dzss = zws(ktsor) - zws(kbsor - 1)
                     do k = kbsor, ktsor
                        if (dzss > epshs) then
                           qsrck = qsrc(isrc) * (zws(k) - zws(k - 1)) / dzss
                        else
                           qsrck = qsrc(isrc) / (ktsor - kbsor + 1)
                        end if
                        ip = ksrcwaq(isrc) + waqpar%ilaggr(kktxsin - kbsin + 1) + waqpar%kmxnxa * (waqpar%ilaggr(kktxsor - k + 1) - 1)
                        qsrcwaq(ip) = qsrcwaq(ip) + dts * qsrck
                     end do
                  else if (kbsor == ktsor) then
                     ! sor side has only one layer
                     dzss = zws(ktsin) - zws(kbsin - 1)
                     do k = kbsin, ktsin
                        if (dzss > epshs) then
                           qsrck = qsrc(isrc) * (zws(k) - zws(k - 1)) / dzss
                        else
                           qsrck = qsrc(isrc) / (ktsin - kbsin + 1)
                        end if
                        ip = ksrcwaq(isrc) + waqpar%ilaggr(kktxsin - k + 1) + waqpar%kmxnxa * (waqpar%ilaggr(kktxsor - kbsor + 1) - 1)
                        qsrcwaq(ip) = qsrcwaq(ip) + dts * qsrck
                     end do
                  else
                     ! multiple layers on both side... it's a bit more complicated...
                     ! determine fractions on sink side
                     call realloc(fsin, kmx, keepExisting=.false., fill=0.0d0)
                     dzss = zws(ktsin) - zws(kbsin - 1)
                     do k = kbsin, ktsin
                        ilaysin = kktxsin - k + 1
                        if (dzss > epshs) then
                           fsin(ilaysin) = (zws(k) - zws(k - 1)) / dzss
                        else
                           fsin(ilaysin) = 1.0d0 / (ktsin - kbsin + 1)
                        end if
                     end do
                     ! distribute sink side fractions over source side
                     do k1 = kbsor, ktsor
                        ilaysor = kktxsor - k1 + 1
                        dzss = zws(ktsor) - zws(kbsor - 1)
                        if (dzss > epshs) then
                           fsor = (zws(k1) - zws(k1 - 1)) / dzss
                        else
                           fsor = 1.0d0 / (ktsor - kbsor + 1)
                        end if
                        do k2 = kbsin, ktsin
                           ilaysin = kktxsin - k2 + 1
                           fsorlay = min(fsin(ilaysin), fsor)
                           fsin(ilaysin) = fsin(ilaysin) - fsorlay
                           fsor = fsor - fsorlay
                           ip = ksrcwaq(isrc) + waqpar%ilaggr(ilaysin) + waqpar%kmxnxa * (waqpar%ilaggr(ilaysor) - 1)
                           qsrcwaq(ip) = qsrcwaq(ip) + dts * fsorlay * qsrc(isrc)
                        end do
                     end do
                  end if
               end if
            end if
         end if
      end do

   end subroutine update_waq_sink_source_fluxes

end module m_update_waq_sink_source_fluxes
