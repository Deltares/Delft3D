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

      SUBROUTINE REAdarcinfo(Marc, ja)
      USE M_SFERIC
      use m_netw
      USE m_grid
      USE M_ARCINFO
      USE M_MISSING
      implicit none

      INTEGER            :: Marc, JA, in
      CHARACTER NAME2*76, TEX*3, REC*132, REC1*132


      logical jawel

      integer :: i, j, k, k0, l0
      integer :: jadoorladen, merr
      double precision XX(8), YY(8), ZZ(8)
      double precision :: af

      JSFERIC  = 0
      JSFERTEK = 0

      MERR  = 0
      MC    = 0

 !     CALL READYY('Reading arcinfo',0d0)

 !     CALL READYY('Reading arcinfo',1d0)


      call reaarc (marc,1)
      CALL DOCLOSE(marc)

      mc = mca ; nc = nca

      CALL INCREASEGRID(MC,NC)

      XC = DMISS
      YC = DMISS

      do i = 1,mc
         do j = 1,nc
            if (d(I,J) .ne. dmiss) then
               xc(i,j) =  x0 + dxa*(i-1)
               yc(i,j) =  y0 + dxa*(j-1)
               zc(i,j) =  d(i,j)
            endif
         enddo
      enddo

      if (allocated(d) ) then 
         deallocate(d) ; mca = 0 ; nca = 0
      endif

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

      END SUBROUTINE REAdarcinfo
