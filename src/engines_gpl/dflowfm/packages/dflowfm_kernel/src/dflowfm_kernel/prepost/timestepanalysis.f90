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

module m_timestepanalysis

   implicit none

   private

   public :: timestepanalysis

contains

   subroutine timestepanalysis(dtsc_loc)
      use precision, only: dp
      use m_flow
      use m_flowtimes
      use m_partitioninfo
      use unstruc_model, only: md_ident
      use m_filez, only: newfil

      real(kind=dp), intent(in) :: dtsc_loc

      integer, save :: mout = 0

!   check if local maximum time step is also global maximum time step
      if (jampi == 1) then
         if (dtsc_loc > dtsc) then
            kkcflmx = 0
         end if
      end if

      if (kkcflmx > 0) then
         numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
      end if

      if (jatimestepanalysis == 1) then
         if (mout == 0) then
            call newfil(mout, trim(md_ident)//'.steps')
            write (mout, '(A)') 'time0/60, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx) '
         end if
         if (kkcflmx > 0) then
            if (kcflmx == 0) kcflmx = kkcflmx
            if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4) then
               write (mout, '(3F14.4,2I8,4F14.4)') time0 / 60d0, dts, dtsc, kkcflmx, kcflmx - kbot(kkcflmx) + 1, vol1(kcflmx), squ2D(kkcflmx), squ(kcflmx), sqi(kcflmx)
            else
               write (mout, '(3F14.4,2I8,4F14.4)') time0 / 60d0, dts, dtsc, kkcflmx, kcflmx - kbot(kkcflmx) + 1, vol1(kcflmx), squ(kcflmx), squ(kcflmx), sqi(kcflmx)
            end if
         else
            write (mout, '(3F14.4, I8)') time0 / 60d0, dts, dtsc, kkcflmx
         end if
      end if

      return
   end subroutine timestepanalysis

end module m_timestepanalysis
