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

!> sort per-node link administration (nod()%lin), based on connectivity
subroutine sortlinks()
   use m_netw
   use m_inverse_map
   use unstruc_colors

   implicit none

   integer                            :: k0  ! node number

   integer                            :: k, k1, k2, knext
   integer                            :: kk
   integer                            :: L1, L2
   integer                            :: N, Ncell
   integer                            :: i1, i2

   integer                            :: numerror  ! number of nodes with errors

   numerror = 0

!   integer, allocatable, dimension(:) :: kmask(:)

!   allocate(kmask(numk))
!   kmask = 0
!
!   do k=nump_firstrun+1,nump
!      Ncell = netcell(k)%N
!      do kk=1,Ncell
!         k0 = netcell(k)%nod(kk)
 k0lp:do k0=1,numk
!         if ( kmask(k0).ne.0 ) cycle       ! already visited

         N = nmk(k0)
         do k1=1,N-1                ! loop over the links and reorder the remaining links
            L1 = nod(k0)%lin(k1)
            knext = 0               ! next link

   !        1D-links are fine: proceed  ! is .or. necessary? all 1D has lnn==0
            if ( lnn(L1).lt.1 .or. kn(3,L1).eq.1 .or. kn(3,L1).eq.3 .or. kn(3,L1).eq.4 .or. kn(3,L1).eq.5 .or. kn(3,L1).eq.6 .or. kn(3,L1).eq.7 ) cycle

   !        find the next link
   !        first try to find a link with a common cell
            i1 = min(lnn(L1), 2)
            do k2=k1+1,N            ! loop over remaining links
               L2 = nod(k0)%lin(k2)
               if ( lnn(L2).lt.1 .or. kn(3,L2).eq.1  .or. kn(3,L2).eq.3  .or. kn(3,L2).eq.4 .or. kn(3,L1).eq.5 .or. kn(3,L1).eq.6 .or. kn(3,L1).eq.7 ) cycle  ! 1D-link
               i2 = min(lnn(L2), 2)

               if ( i2.lt.1 ) then
                  continue
               end if

               if ( lne( 1,L1).eq.lne(1,L2) .or. lne( 1,L1).eq.lne(i2,L2) .or.  &
                    lne(i1,L1).eq.lne(1,L2) .or. lne(i1,L1).eq.lne(i2,L2) ) then
   !              next link found
                  knext = k2
                  exit
               end if
            end do

            if ( k1.eq.1 .and. knext.eq.N .and. N.gt.2 ) then   ! wrong direction
               knext = 0
               L2    = 0
            end if

   !        if no next link bounding a common cell is found: find next non-internal link
            if ( knext.eq.0 ) then
               if ( lnn(L1).eq.2 ) then
!                  call qnerror('sortlinks: error', ' ', ' ')
!                  goto 1234
               else
   !              find next non-internal link
                  do k2=k1+1,N
                     L2 = nod(k0)%lin(k2)
                     if ( lnn(L2).lt.2 .or. kn(3,L2).eq.1  .or. kn(3,L2).eq.3  .or. kn(3,L2).eq.4 ) then ! .or. kn(3,L1).eq.5 .or. kn(3,L1).eq.6 .or. kn(3,L1).eq.7) then  ! found
                        knext = k2
                        exit
                     end if
                  end do
               end if
            end if

            if ( knext.eq.0 ) then  ! no next link found
!              error: deactivate node
               kc(k0) = 0
               numerror = numerror + 1
               call cirr(xk(k0), yk(k0), ncolhl)
               cycle k0lp
            end if

   !        swap links
            if ( nod(k0)%lin(k1+1).ne.L2 ) then   ! fold
               nod(k0)%lin(k1+2:knext) = nod(k0)%lin(k1+1:knext-1)
               nod(k0)%lin(k1+1)       = L2
            end if

         end do

!   !     mask node
!         kmask(k0) = 1
!
!      end do
   end do k0lp

1234 continue
   if ( numerror.gt.0 ) then
      call qnerror('sortlinks: unrecoverable folds', ' ', ' ')
   end if

!   if ( allocated(kmask) ) deallocate(kmask)

end subroutine sortlinks
