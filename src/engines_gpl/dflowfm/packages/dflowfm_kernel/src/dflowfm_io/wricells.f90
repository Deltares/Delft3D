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
! It seems it is not used. 
module m_wricells

implicit none

private

public :: wricells

contains

 subroutine wricells(mout) ! write flow cell surrounding netnodes
    use m_netw
    use m_flowgeom

    implicit none
    integer :: mout, n, j

    write (mout, '(A,I12)') 'NR of NETNODES           = ', numk ! nump = ndx
    write (mout, '(A,I12)') 'NR of NETLINKS           = ', numL ! nump = ndx
    write (mout, '(A,I12)') 'NR of internal FLOWCELLS = ', nump ! nump = ndx
    do n = 1, nump
       write (mout, '(10I10)') netcell(n)%n, (netcell(n)%NOD(j), j=1, netcell(n)%n)
    end do

 end subroutine wricells

end module m_wricells
