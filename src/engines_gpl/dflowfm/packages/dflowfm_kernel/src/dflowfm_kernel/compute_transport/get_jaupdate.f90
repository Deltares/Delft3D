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

!> determine if the cells have to be updated (1) or not (0)
subroutine get_jaupdate(istep, Ndxi, Ndx, ndeltasteps, jaupdate)
   use timers

   implicit none

   integer, intent(in) :: istep !< substep number
   integer, intent(in) :: Ndxi !< number of cells, excluding virtual boundary cells
   integer, intent(in) :: Ndx !< number of cells, including virtual boundary cells
   integer, dimension(Ndx), intent(in) :: ndeltasteps !< number of substeps between updates
   integer, dimension(Ndx), intent(out) :: jaupdate !< update cell (1) or not (0)

   integer :: kk
   integer :: num

   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("get_jaupdate", ithndl)

   jaupdate = 0

   num = 0
   do kk = 1, Ndxi
      if (mod(istep + 1, ndeltasteps(kk)) == 0) then
!      if ( int((istep+1)/ndeltasteps(kk))*ndeltasteps(kk).eq.istep+1 ) then
         jaupdate(kk) = 1
         num = num + 1
      end if
   end do

!  BEGIN DEBUG
!   jaupdate = 1
!  END DEBUG

!   if ( istep.lt.nsubsteps ) then
!      write(6,"(I0,':',I0, ' ', $)") istep+1, num
!   end if

   if (timon) call timstop(ithndl)
   return
end subroutine get_jaupdate
