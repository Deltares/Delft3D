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

!> add particles
subroutine add_particles(Nadd, xadd, yadd, jareplace, jadomain)
   use m_particles
   use m_partmesh
   use m_alloc
   use m_wearelt
   use m_sferic, only: jsferic
   use geometry_module, only: sphertocart3D
   use m_partitioninfo

   implicit none

   integer,                           intent(in)  :: Nadd       !< number of particles to be added
   double precision, dimension(Nadd), intent(in)  :: xadd       !< x-coordinates of particles to be added
   double precision, dimension(Nadd), intent(in)  :: yadd       !< y-coordinates of particles to be added
   integer,                           intent(in)  :: jareplace  !< replace existing but disabled particles(1) or not (0)
   integer,                           intent(in)  :: jadomain   !< only place particles in own subdomain (1) or not (0)

   integer,          dimension(:),    allocatable :: kadd       !< cell numbers

   integer                                        :: i, ipoint, Nsize
   integer                                        :: ierror
   integer                                        :: Npartnew
   integer                                        :: Nreplace
   integer                                        :: n, Nloc

   double precision                               :: xn, yn, zn, dn
   integer                                        :: k, k1

   if ( japart.ne.1 ) return

!  get number of existing particles that may be replaced
   if ( jareplace.eq.1 ) then
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do
   else
      Nreplace = 0
   end if

!  get new particle cell number
   allocate(kadd(Nadd))
   kadd = 0
   call part_findcell(Nadd,xadd,yadd,kadd,ierror)

!  count particles to be added
   Nloc = 0
   do i=1,Nadd
!     get cell number
      k = kadd(i)

      if ( k.gt.0 ) then
         if ( jampi.eq.1 .and. jadomain.eq.1 ) then
!           only allow particles in own subdomain
            n = iabs(cell2nod(k))
            if ( idomain(n).ne.my_rank ) then
               kadd(i) = 0
            end if
         end if

         if ( kadd(i).gt.0 ) then
            Nloc = Nloc + 1
         end if
      end if
   end do

!  new number of particles
   Npartnew = Npart + max(Nloc-Nreplace,0)

!  get current array sizes
   if ( allocated(xpart) ) then
      Nsize = size(xpart)
   else
      Nsize = 0
   end if

!  realloc
   if ( Npartnew.gt.Nsize ) then
!     compute new array size
      Nsize = int(1.2*dble(Npartnew)) + 1
      call realloc_particles(Nsize, .true., ierror)
   end if

!!  get new particle cell number
!   allocate(kadd(Nadd))
!   kadd = 0
!   call part_findcell(Nadd,xadd,yadd,kadd,ierror)

!  fill data
   if ( jareplace.eq.1 ) then
      ipoint = 1
   else
      ipoint = Npart+1
   end if

   do i=1,Nadd
      if ( kadd(i).eq.0 ) cycle

      if ( ipoint.le.Npart ) then
         do while ( kpart(ipoint).ne.0 )
            ipoint = ipoint+1
         end do
      end if

      if ( jsferic.eq.0 ) then
         xpart(ipoint) = xadd(i)
         ypart(ipoint) = yadd(i)
      else
         call sphertocart3D(xadd(i),yadd(i),xpart(ipoint),ypart(ipoint),zpart(ipoint))



         if ( jsferic.eq.1 ) then
!           project particle on triangle
            k = kadd(i)
            if ( k.gt.0 ) then
               k1 = edge2node(1,icell2edge(jcell2edge(k)))
               xn = xnode(k1)
               yn = ynode(k1)
               zn = znode(k1)
               dn = (xpart(ipoint) - xn) * dnn(1,k) +  &
                    (ypart(ipoint) - yn) * dnn(2,k) +  &
                    (zpart(ipoint) - zn) * dnn(3,k)
               xpart(ipoint) = xpart(ipoint) - dn * dnn(1,k)
               ypart(ipoint) = ypart(ipoint) - dn * dnn(2,k)
               zpart(ipoint) = zpart(ipoint) - dn * dnn(3,k)
            end if
         end if

      end if
      kpart(ipoint) = kadd(i)
      iglob(ipoint) = Nglob + i
!      write(namepart(ipoint), "('added_particle ', I0)") i
!      Npart = Npart+1
      Npart = max(Npart,ipoint)

!     plot
      call setcol(31)
!      call movabs(xpart(ipoint),ypart(ipoint))
      call movabs(xadd(i), yadd(i))
      call cir(rcir)

!     advance pointer
      ipoint = ipoint+1
   end do

   Nglob = Nglob + Nadd

!  deallocate
   if ( allocated(kadd) ) deallocate(kadd)

   return
end subroutine add_particles
