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

!> draw particles in GUI
subroutine tekpart()
   use m_particles
   use m_wearelt
   use unstruc_display
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use geometry_module, only : cart3Dtospher
   implicit none

   double precision :: x, y

   integer :: i

   if ( Npart.lt.1 .or. ndrawpart.eq.1 ) return

!  safety check
   if ( jsferic.eq.1 .and. .not.(allocated(zpart)) ) then
      call dealloc_partmesh()
      call dealloc_partfluxes()
      call dealloc_partrecons()
      call dealloc_particles()
      call dealloc_auxfluxes()
      call dealloc_partparallel()
      japart = 0
      return
   end if

   call setcol(31)

   if ( jsferic.eq.0 ) then
      do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call movabs(xpart(i),ypart(i))
         call cir(rcir)
      end do
   else
       do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call Cart3Dtospher(xpart(i),ypart(i),zpart(i),x,y,0d0)
         call movabs(x,y)
         call cir(rcir)
      end do
   end if

   return
end subroutine tekpart
