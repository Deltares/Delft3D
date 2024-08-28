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

     !> Checks spline points in X and Y.
     !! Counts the number of splines and the maximum length and moves all
     !! All splines with <=1 point are reset and moved to the back.
     subroutine CHECKSPL(X, Y, mmax, nmax, MCS, NCS)
        use m_missing
        implicit none
        integer :: mmax, nmax, mcs, ncs
        double precision :: X(MMAX, NMAX), Y(MMAX, NMAX)

        integer :: numspl, numpx, numpi, numpj, i, j, k

!    5CONTINUE
        NUMSPL = 0
        NUMPX = 0
        do I = 1, MMAX - 1
           call NUMPold(X, mmax, nmax, I, NUMPI)
           if (NUMPI <= 1) then
              do K = 1, NMAX
                 X(I, K) = XYMIS
                 Y(I, K) = XYMIS
              end do
              do J = I + 1, MMAX
                 call NUMPold(X, mmax, nmax, J, NUMPJ)
                 if (NUMPJ > 1) then
                    call CHAROW(X, mmax, nmax, J, J - 1, NMAX)
                    call CHAROW(Y, mmax, nmax, J, J - 1, NMAX)
                 end if
              end do
           else if (NUMPI >= 2) then
              NUMPX = max(NUMPX, NUMPI)
              NUMSPL = NUMSPL + 1
           end if
        end do
        MCS = NUMSPL
        NCS = NUMPX
        return
     end subroutine checkspl
