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

!> write matlab integer array to file
   subroutine matlab_write_int(matfile, varname, var, Ni, Nj)

      implicit none

      integer :: matfile !< matlab file unit number
      character(len=*) :: varname !< variable name
      integer :: Ni, Nj !< array sizes
      integer, dimension(Ni, Nj) :: var !< variable

      integer i, j

      write (matfile, *) trim(varname), ' = ['
      do i = 1, Ni
         do j = 1, Nj
            if (var(i, j) /= -1234) then
               write (matfile, "(I20)", advance="no") var(i, j)
            else
               write (matfile, "(' NaN')", advance="no")
            end if
         end do
         write (matfile, *)
      end do
      write (matfile, "('];')")

   end subroutine matlab_write_int
