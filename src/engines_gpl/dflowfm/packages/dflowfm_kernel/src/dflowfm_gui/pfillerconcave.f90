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

module m_pfillerconcave

implicit none

contains

    subroutine PFILLERconcave(X, Y, N_, NCOL, NCLR)
       use unstruc_opengl
       use m_colnow
       use m_drawthis
       use m_set_col
       use m_realpolygon

       integer :: N_
       integer :: nclr
       integer :: ncol
       double precision :: X(N_), Y(N_)
       integer :: N
       integer, parameter :: NMAX = 128
       real xr(NMAX), yr(NMAX)

       call SETCOL(NCOL)

!   safety
       N = min(N_, NMAX)

       xr(1:N) = x(1:N)
       yr(1:N) = y(1:N)

       call IGrPolygoncomplex(Xr, Yr, N)

       if (.not. InOpenGLRendering .and. (NCLR /= NCOL .or. ndraw(10) /= 0)) then
          call realPolygon(Xr, Yr, N, NCLR)
       end if

       return
    end

end module m_pfillerconcave
