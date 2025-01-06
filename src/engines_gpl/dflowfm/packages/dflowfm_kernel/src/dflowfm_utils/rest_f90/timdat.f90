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

module m_timdat

implicit none

private

public :: timdat

contains

subroutine timdat(julday, timsec, idatum, itijd)
!!--description-----------------------------------------------------------------
!
!    Function:  returns date and time according actual time
!               in minutes and itdate (julian day)
!               in the form yyyymmdd  hhmmss
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
!
! Global variables
!
   integer :: idatum !  Absolute date related to ITDATE and TIMSEC
   integer, intent(out) :: itijd !  Absolute time related to ITDATE and TIMSEC
   integer, intent(in) :: julday !  Description and declaration in inttim.igs
   real(fp), intent(in) :: timsec !  Description and declaration in inttim.igs
!
! Local variables
!
   integer(long) :: icurtm
   integer :: iday
   integer :: ihou
   integer :: imin
   integer :: imo
   integer :: isec
   integer :: iy
   integer :: l
   integer :: n
!
!! executable statements -------------------------------------------------------
!
   ! Define number of days, hours, minutes and seconds from TIMSEC
   !
   icurtm = nint(timsec, long)
   iday = int(icurtm / 86400_long)
   icurtm = icurtm - int(iday, long) * 86400_long
   ihou = int(icurtm) / 3600
   icurtm = icurtm - int(ihou, long) * 3600_long
   imin = int(icurtm) / 60
   icurtm = icurtm - int(imin, long) * 60_long
   isec = int(icurtm)
   !
   ! Convert julian day-number
   !
   l = julday + iday + 68569
   n = 4 * l / 146097
   l = l - (146097 * n + 3) / 4
   iy = 4000 * (l + 1) / 1461001
   l = l - 1461 * iy / 4 + 31
   imo = 80 * l / 2447
   iday = l - 2447 * imo / 80
   l = imo / 11
   imo = imo + 2 - 12 * l
   iy = 100 * (n - 49) + iy + l
   !
   ! Define integer values for time and date
   !
   itijd = ihou * 10000 + imin * 100 + isec
   idatum = abs(iy) * 10000 + imo * 100 + iday
   idatum = sign(idatum, iy)
end subroutine timdat

end module m_timdat
