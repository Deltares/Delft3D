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
!> Write the total number of times a cell was Courant limiting to <run_id>_numlimdt.xyz file.
module m_write_timestep_limiting_cells

implicit none

private

public :: write_timestep_limiting_cells

contains

 subroutine write_timestep_limiting_cells()
    use m_flowgeom, only: ndx, xz, yz
    use m_flow, only: numlimdt
    use unstruc_model, only: md_ident, getoutputdir

    implicit none

    integer :: file_unit, cell

    call newfil(file_unit, trim(getoutputdir())//trim(md_ident)//'_numlimdt.xyz')
    do cell = 1, ndx
       if (numlimdt(cell) > 0) then
          write (file_unit, *) xz(cell), yz(cell), numlimdt(cell)
       end if
    end do
    call doclose(file_unit)

 end subroutine write_timestep_limiting_cells

end module m_write_timestep_limiting_cells
