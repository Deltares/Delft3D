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

module m_teksorsin

   implicit none

contains

   subroutine teksorsin() ! teksrc
      use precision, only: dp
      use m_settextsizefac
      use fm_external_forcings_data
      use unstruc_display, only: klsrc
      use m_transport, only: isalt, itemp
      use m_drawthis
      use m_cirr
      use m_gtext
      use m_inview

      implicit none
      integer :: n, k, n2, ncol
      character(len=40) :: tex
      real(kind=dp) :: xp, yp

      if (ndraw(41) <= 1 .or. num_source_sink == 0) then
         return
      end if

      call IGrCharJustify('L')
      call settextsizefac(1.0_dp)

      do n = 1, num_source_sink ! teksorsin
         k = source_sink_indices(1, n)
         if (k /= 0) then
            n2 = 1
            xp = source_sink_x(n, n2)
            yp = source_sink_y(n, n2)
            if (inview(xp, yp)) then
               if (source_sink_water_discharge(n) > 0) then
                  ncol = 3
               else
                  ncol = 221
               end if
               call cirr(xp, yp, ncol)
               if (ndraw(41) == 3) then
                  call gtext(' '//trim(source_sink_name(n)), xp, yp, klsrc)
               else if (ndraw(41) == 4) then
                  write (tex, '(f10.3)') - source_sink_water_discharge(n)
                  call gtext(trim(tex)//' (m3/s)', xp, yp, klsrc)
               else if (ndraw(41) == 5 .and. isalt > 0) then
                  if (source_sink_water_discharge(n) < 0.0_dp) then
                     write (tex, '(f10.3)') source_sink_constituents(isalt, n)
                     call gtext(trim(tex)//' (ppt)', xp, yp, klsrc)
                  end if
               else if (ndraw(41) == 6 .and. itemp > 0) then
                  if (source_sink_water_discharge(n) < 0.0_dp) then
                     write (tex, '(f10.3)') source_sink_constituents(itemp, n)
                     call gtext(trim(tex)//' (degC)', xp, yp, klsrc)
                  end if
               end if
            end if
         end if
         k = source_sink_indices(4, n)
         if (k /= 0) then
            n2 = source_sink_max_xy_points(n)
            xp = source_sink_x(n, n2)
            yp = source_sink_y(n, n2)
            if (inview(xp, yp)) then
               if (source_sink_water_discharge(n) > 0) then
                  ncol = 221
               else
                  ncol = 3
               end if
               call cirr(xp, yp, ncol)
               if (ndraw(41) == 3) then
                  call gtext(' '//trim(source_sink_name(n)), xp, yp, klsrc)
               else if (ndraw(41) == 4) then
                  write (tex, '(f10.3)') source_sink_water_discharge(n)
                  call gtext(trim(tex)//' (m3/s)', xp, yp, klsrc)
               else if (ndraw(41) == 5 .and. isalt > 0) then
                  if (source_sink_water_discharge(n) > 0.0_dp) then
                     write (tex, '(f10.3)') source_sink_constituents(isalt, n)
                     call gtext(trim(tex)//' (ppt)', xp, yp, klsrc)
                  end if
               else if (ndraw(41) == 6 .and. itemp > 0) then
                  if (source_sink_water_discharge(n) > 0.0_dp) then
                     write (tex, '(f10.3)') source_sink_constituents(itemp, n)
                     call gtext(trim(tex)//' (degC)', xp, yp, klsrc)
                  end if
               end if
            end if
         end if
      end do

   end subroutine teksorsin

end module m_teksorsin
