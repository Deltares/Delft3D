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
 subroutine allocate_linktocenterweights() ! allocate center related linkxy weights

    use m_flowgeom
    use m_alloc

    implicit none

    integer :: ierr

    if (allocated(wcx1)) deallocate (wcx1, wcy1, wcx2, wcy2)
    if (allocated(wcL)) deallocate (wcL)

    allocate (wcx1(lnx), stat=ierr); 
    call aerr('wcx1(lnx)', ierr, lnx)
    allocate (wcy1(lnx), stat=ierr); 
    call aerr('wcy1(lnx)', ierr, lnx)
    allocate (wcx2(lnx), stat=ierr); 
    call aerr('wcx2(lnx)', ierr, lnx)
    allocate (wcy2(lnx), stat=ierr); 
    call aerr('wcy2(lnx)', ierr, lnx)
    allocate (wcL(2, Lnx), stat=ierr); 
    call aerr('wcL  (2,Lnx)', ierr, 2 * Lnx)

 end subroutine allocate_linktocenterweights
