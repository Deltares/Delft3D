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
!> update positions of particles
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine update_particles(q,s0,s1,Dt)
!   use m_flow
   use m_particles
   use m_wearelt
!   use m_flowtimes, only: dts, time1
   use m_flowgeom, only: Ndx, Lnx
   use m_partitioninfo
   use m_sferic
   use geometry_module, only: Cart3Dtospher
   use unstruc_messages
   implicit none

   double precision, dimension(Lnx), intent(in) :: q  !< fluxes
   double precision, dimension(Ndx), intent(in) :: s0 !< water levels at start of time interval
   double precision, dimension(Ndx), intent(in) :: s1 !< water levels at end of time interval
   double precision,                 intent(in) :: Dt !< time interval

   integer, dimension(1) :: numremaining ! number of remaining particles to be updated

   double precision :: xx, yy

   integer :: i, k
   integer :: iter
   integer :: ierror

   integer, parameter :: MAXITER = 1000 ! maximum number of substeps

   if ( japart.ne.1 ) return

   ierror = 1

!  reconstruct velocity field
   call reconst_vel(q, s0, s1, ierror)
   if ( ierror.ne.0 ) goto 1234

   if ( Npart.gt.0 ) then
!     set remaining time to time step
      dtremaining = Dt
!      Lpart = 0
      numzero = 0
   end if

   do iter=1,MAXITER
!     update particles in cells
      call update_particles_in_cells(numremaining(1), ierror)
      if ( ierror.ne.0 ) goto 1234

      if ( jampi.eq.1 ) then
!        reduce numremaining
         call reduce_int_max(1,numremaining)
      end if

!      write(6,*) 'my_rank=', my_rank, 'iter=', iter, 'numremaining=', numremaining(1)

!     send/receive particles from other subdomains
      call partition_update_particles()

 !     if ( Npart.gt.0 .and. kpart(1).gt.0 ) then
 !        write(6,"('my_rank=', I1, ', kpart(1)=', I5 )") my_rank, kpart(1)
 !     end if

!     BEGIN DEBUG
!      i = 1
!      if ( Npart.gt.0 .and. kpart(i).gt.0 ) then
!         write(mfile,"(F8.2, F8.2, I5)") xpart(i), ypart(i), my_rank
!      end if
!     END DEBUG

      if ( numremaining(1).eq.0 ) then
!         write(6,*) 'iter=', iter
         exit
      end if
   end do


!  check for remaining particles
   if ( numremaining(1).gt.0 ) then
 !    plot remaining particles
      do i=1,Npart
         if ( dtremaining(i).gt.0d0 .and. kpart(i).gt.0 ) then
            if ( jsferic.eq.0 ) then
               xx = xpart(i)
               yy = ypart(i)
            else
               call Cart3Dtospher(xpart(i),ypart(i),zpart(i),xx,yy,0d0)
            end if
            call cirr(xx,yy, 211)
            write(6,"(I0, ', ', I0, ':', 2E25.15, ', ', I0)") iglob(i), my_rank, xx, yy, kpart(i)
         end if
      end do

!      call qnerror('update_particles: iter>MAXITER', ' ', ' ')
      call mess(LEVEL_WARN, 'update_particles: iter>MAXITER')

      goto 1234
   end if

   ierror = 0
1234 continue

   return
end subroutine update_particles
