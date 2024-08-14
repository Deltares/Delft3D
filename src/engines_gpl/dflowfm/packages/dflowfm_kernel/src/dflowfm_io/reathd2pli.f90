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

 subroutine reathd2pli(mthd, mout) ! convert d3d obs file to model independent
    use m_grid
    implicit none
    integer :: mthd, mout
    character(len=132) :: rec
    character(len=1) :: uv
    integer :: m, n, m2, n2, mn, mx, nn, nx, i

10  read (mthd, '(a)', end=999) rec

    read (rec, *) m, n, m2, n2, uv

    write (mout, '(a)') 'Line'
    write (mout, '(a)') ' 2 2'

    if (index(rec, 'u') > 0 .or. index(rec, 'U') > 0) then

       nn = min(n, n2); nx = max(n, n2)
       write (mout, *) xc(m, nn - 1), yc(m, nn - 1)
       do i = nn, nx
          write (mout, *) xc(m, i), yc(m, i)
       end do

    else

       mn = min(m, m2); mx = max(m, m2)
       write (mout, *) xc(mn - 1, n), yc(mn - 1, n)
       do i = mn, mx
          write (mout, *) xc(i, n), yc(i, n)
       end do

    end if

    goto 10

999 call doclose(mthd)
    call doclose(mout)

 end subroutine reathd2pli
