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

 subroutine reablu(mout) ! read bottom level u points
    use M_FLOWGEOM
    use m_qnerror
    use m_qn_read_error
    use m_set_bobs
    implicit none
    integer :: mout
    character(len=256) :: rec

    integer :: L, L1
    integer :: lnxr
    double precision :: rd
    read (mout, '(a)') rec
    L1 = index(rec, '=') + 1
    read (rec(L1:), *, err=888) lnxr
    if (lnxr /= lnx) then
       call doclose(mout)
       call qnerror('nr of flowlinks read .ne. nr of flowlinks', ' ', ' ')
       return
    end if

    do L = 1, lnx
       read (mout, *) rd, rd, blu(L)
    end do
    call doclose(mout)

    call setbobs()

    return

888 call qnreaderror('trying to read nr of flowlinks but getting', rec, mout)
    call doclose(mout)

 end subroutine reablu
