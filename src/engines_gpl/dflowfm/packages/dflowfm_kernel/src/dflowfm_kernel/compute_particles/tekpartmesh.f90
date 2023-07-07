!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> plot mesh admin
subroutine tekpartmesh()
   use m_particles
   use m_partmesh
   use m_partrecons
   use m_wearelt
   use unstruc_display
   use m_sferic, only: jsferic
   use geometry_module, only: Cart3Dtospher
   implicit none

   character(len=32) :: text

   double precision :: xL, yL, xL1, yL1, x, y, dfac

   integer          :: i, k, k1, k2, L

   if ( japart.ne.1 .or. ndrawpart.eq.1 ) return

!  edges
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)

      if ( jsferic.eq.0 ) then
         xL = 0.5d0*(xnode(k1)+xnode(k2))
         yL = 0.5d0*(ynode(k1)+ynode(k2))
         xL1 = xL + rcir*dnx(1,L)
         yL1 = yL + rcir*dny(1,L)
      else
         dfac = 0.1d0*w(L)
         call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2)), 0.5d0*(ynode(k1)+ynode(k2)), 0.5d0*(znode(k1)+znode(k2)), xL, yL, 0d0)
         if( edge2cell(1,L).gt.0 ) then
            call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2))+dfac*dnx(1,L), 0.5d0*(ynode(k1)+ynode(k2))+dfac*dny(1,L), 0.5d0*(znode(k1)+znode(k2))+dfac*dnz(1,L), xL1, yL1, 0d0)
         else
            call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2))-dfac*dnx(2,L), 0.5d0*(ynode(k1)+ynode(k2))-dfac*dny(2,L), 0.5d0*(znode(k1)+znode(k2))-dfac*dnz(2,L), xL1, yL1, 0d0)
         end if
      end if

      if ( jsferic.eq.0 ) then
         call movabs(xnode(k1),ynode(k1))
         call lnabs(xnode(k2),ynode(k2))

         call movabs(xL,yL)
         call lnabs(xL1,yl1)
!         call hitext(L,xL1,yL1)
         write(text,"(I0, '(', F0.1, ')')") L, qe(L)
         call drawtext(real(xL1),real(yL1),trim(text))
      else
         call Cart3Dtospher(xnode(k1),ynode(k1), znode(k1), x, y, 0d0)
         call movabs(x,y)
         call Cart3Dtospher(xnode(k2),ynode(k2), znode(k2), x, y, 0d0)
         call lnabs(x,y)

         call movabs(xL,yL)
         call lnabs(xL1,yl1)
      end if
   end do

!  cells
   if ( jsferic.eq.0 ) then
      do k=1,numcells
         call hitext(k,xzwcell(k),yzwcell(k))
      end do
   else
      do k=1,numcells
         call cart3Dtospher(xzwcell(k),yzwcell(k),zzwcell(k),x,y,0d0)
         call hitext(k,x,y)
      end do
   end if

!  particle cell numbers
   do i=1,Npart
!      call hitext(i,xpart(i)+rcir,ypart(i))
!      call hitext(kpart(i),xpart(i)+rcir,ypart(i))
      write(text,"(I0, '(', I0, ')')") iglob(i), kpart(i)
      if ( jsferic.eq.0 ) then
         call drawtext(real(xpart(i)+rcir),real(ypart(i)),trim(text))
      else
         call cart3Dtospher(xpart(i),ypart(i),zpart(i),x,y,0d0)
         call drawtext(real(x+rcir),real(y),trim(text))
      end if
   end do

   return
end subroutine tekpartmesh
