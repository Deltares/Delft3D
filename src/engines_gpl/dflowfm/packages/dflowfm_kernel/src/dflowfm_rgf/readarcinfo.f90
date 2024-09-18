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

      subroutine REAdarcinfo(Marc, ja)
         use M_SFERIC
         use m_netw
         use m_grid
         use M_ARCINFO
         use M_MISSING
         use m_increase_grid
         implicit none

         integer :: Marc, JA
         integer :: i, j
         integer :: merr

         JSFERIC = 0
         JSFERTEK = 0

         MERR = 0
         MC = 0

         !     CALL READYY('Reading arcinfo',0d0)

         !     CALL READYY('Reading arcinfo',1d0)

         call reaarc(marc, 1)
         call DOCLOSE(marc)

         mc = mca; nc = nca

         call INCREASEGRID(MC, NC)

         XC = DMISS
         YC = DMISS

         do i = 1, mc
            do j = 1, nc
               if (d(I, J) /= dmiss) then
                  xc(i, j) = x0 + dxa * (i - 1)
                  yc(i, j) = y0 + dxa * (j - 1)
                  zc(i, j) = d(i, j)
               end if
            end do
         end do

         if (allocated(d)) then
            deallocate (d); mca = 0; nca = 0
         end if

!     disable grid outside selecting polygon
!      if ( NPL.gt.0 ) then
!         in = -1
!         do j=1,nc
!            do i=1,mc
!               call dbpinpol(xc(i,j),yc(i,j),in)
!               if ( in.ne.1 ) then
!                  xc(i,j) = DMISS
!                  yc(i,j) = DMISS
!               end if
!            end do
!         end do
!      end if

!      call gridtonet()
!
!      if (allocated(xc) ) then
!         deallocate(xc,yc,zc) ; mc = 0
!      endif

!      CALL READYY(' ',-1d0)

         JA = 1

      end subroutine REAdarcinfo
