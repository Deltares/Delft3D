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

module m_writeset

implicit none

private

public :: writeset

contains

 subroutine writeset(kk, fnam, nr, a)
    implicit none
    integer :: kk, nr
    character(len=132) :: a(100)
    character(len=*) fnam
    integer :: l, mout, k

    L = index(fnam, '_')
    write (fnam(L + 1:L + 4), '(i4.4)') nr

    call newfil(mout, fnam)
    write (mout, '(a)') '*'//a(1)
    do k = 2, kk
       ! call correctiefile(a(k))
       write (mout, '(a)') a(k)
    end do
    call doclose(mout)
 end subroutine writeset

end module m_writeset
