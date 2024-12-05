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

 subroutine setcornertolinkorientations()
    use precision, only: dp
    use m_flowgeom
    use network_data, only: xk, yk
    use m_sferic
    use m_alloc
    use unstruc_messages
    use m_missing, only: dmiss
    use geometry_module, only: half, spher2locvec
    implicit none

    real(kind=dp) :: xL, yL

    integer :: k3, k4
    integer :: L

    integer :: ierr

    real(kind=dp), parameter :: dtol = 1d-8

    if (allocated(csbn)) deallocate (csbn)
    if (allocated(snbn)) deallocate (snbn)

    if (jsferic == 0 .or. jasfer3D == 0) return

    allocate (csbn(2, Lnx), stat=ierr); csbn = 1d0
    call aerr('csbn(2,Lnx)', ierr, 2 * Lnx)
    allocate (snbn(2, Lnx), stat=ierr); snbn = 0d0
    call aerr('snbn(2,Lnx)', ierr, 2 * Lnx)

    do L = 1, Lnx
       k3 = lncn(1, L)
       k4 = lncn(2, L)

!      compute flowlink midpoint coordinates (xL,yL)
       call half(xk(k3), yk(k3), xk(k4), yk(k4), xL, yL, jsferic, jasfer3D)

       if (yk(k3) == 90d0 .or. yk(k4) == 90d0) then
          continue
       end if

!      compute orientation w.r.t. link mid point
       call spher2locvec(xk(k3), yk(3), 1, (/xL/), (/yL/), (/1d0/), (/0d0/), csbn(1, L), snbn(1, L), jsferic, jasfer3D, dmiss)
       call spher2locvec(xk(k4), yk(4), 1, (/xL/), (/yL/), (/1d0/), (/0d0/), csbn(2, L), snbn(2, L), jsferic, jasfer3D, dmiss)
    end do

    return
 end subroutine setcornertolinkorientations
