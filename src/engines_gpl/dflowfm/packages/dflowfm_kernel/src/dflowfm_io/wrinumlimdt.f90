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

 subroutine wrinumlimdt()
    use m_flowgeom, only: ndx, xz, yz
    use m_flow, only: numlimdt
    use unstruc_model, only: md_ident, getoutputdir
    implicit none
    integer :: mlim, k

    call newfil(mlim, trim(getoutputdir())//trim(md_ident)//'_numlimdt.xyz')
    do k = 1, ndx
       if (numlimdt(k) > 0) then
          write (mlim, *) xz(k), yz(k), numlimdt(k)
       end if
    end do
    call doclose(mlim)

 end subroutine wrinumlimdt

!> a little bit a general subroutine to write double precision (:,:) data over flow nodes
 subroutine write_dp_data_over_cells(data_name, idata_begin, idata_end, idata1, idata2, data)
    use m_flowgeom, only: ndx, xz, yz
    use unstruc_model, only: md_ident, getoutputdir

    implicit none

    character(len=*), intent(in) :: data_name
    integer, intent(in) :: idata_begin, idata_end, idata1, idata2
    double precision, dimension(idata1, idata2), intent(in) :: data

    integer :: file_unit, cell_number, index_data

    call newfil(file_unit, trim(getoutputdir())//trim(md_ident)//'_'//trim(data_name)//'.xyz')

    do cell_number = 1, min(ndx, idata2)
       write (file_unit, *) xz(cell_number), yz(cell_number), (data(index_data, cell_number), index_data=idata_begin, idata_end)
    end do

    call doclose(file_unit)

 end subroutine write_dp_data_over_cells
