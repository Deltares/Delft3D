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

module m_sedmor_write_stats

   implicit none

   private

   public :: sedmor_write_stats

contains

   subroutine sedmor_write_stats(tim)
      use precision, only: dp
      use m_sediment, only: stm_included, stmpar
      use m_flowparameters, only: eps10
      use m_flowtimes, only: ti_sed, ti_seds, ti_sede, tstop_user, time_sed
      use precision_basics
      use m_fm_morstatistics

      implicit none

      real(kind=dp), intent(in) :: tim
      integer :: ierr
      real(kind=dp) :: tem_dif

      if (.not. stm_included) return
      if (.not. stmpar%morpar%moroutput%morstats) return

      ierr = 1
      if (stmpar%morpar%moroutput%morstats .and. ti_sed > 0) then
         if (comparereal(tim, time_sed, eps10) >= 0) then
            call unc_write_sed(tim)
            call morstats_clearstats()
            if (comparereal(time_sed, ti_sede, eps10) == 0) then
               time_sed = tstop_user + 1
            else
               tem_dif = (tim - ti_seds) / ti_sed
               time_sed = max(ti_seds + (floor(tem_dif + 0.001d0) + 1) * ti_sed, ti_seds)

               if (comparereal(time_sed, ti_sede, eps10) == 1) then
                  ! next time_map would be beyond end of map-window, write one last map exactly at that end.
                  time_sed = ti_sede
               end if
            end if
         end if
      end if

      ierr = 0

1234  continue
      return
   end subroutine sedmor_write_stats

end module m_sedmor_write_stats
