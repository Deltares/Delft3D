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

!> compute change in topology functional and get the nodes and cells involved
subroutine comp_ntopo(L, jalandbound, k1, k2, kL, kR, icellL, icellR, ntopo)
   use m_netw
   use m_alloc
   use m_missing

   implicit none

   integer, intent(in)   :: L                !< link number
   integer, intent(in)   :: jalandbound      !< take land boundary into account
   integer, intent(out)  :: k1, k2, kL, kR   !< nodes involved
   integer, intent(out)  :: icellL, icellR   !< cells involved
   integer, intent(out)  :: ntopo            !< change in topology functional

   integer               :: k, kk
   integer               :: n1, n2, nL, nR
   integer               :: n1L, n1R, n2L, n2R, nL1, nL2, nR1, nR2
   integer, dimension(4) :: nopt, nnow, naft !< optimal and before and after flip

   logical               :: Lproceed

!   integer, parameter    :: IMISS = -999

   integer, external     :: nmk_opt

!  debug
!   if ( allocated(zk) ) deallocate(zk)
!   call realloc(zk, numk)
!   zk = 0d0


   ntopo = 0
   k1    = 0
   k2    = 0
   kL    = 0
   kR    = 0

!  get the begin and end node of link L
   k1 = kn(1,L)
   k2 = kn(2,L)

   if ( lnn(L).ne.2 ) goto 1234  ! inner links only

   icellL = lne(1,L)
   icellR = lne(2,L)

   if ( netcell(icellL)%N.ne.3 .or. netcell(icellR)%N.ne.3 ) goto 1234  ! triangles only

!  find the nodes that are connected to both k1 and k2
   kL = sum(netcell(icellL)%nod(1:3)) - k1 - k2
   kR = sum(netcell(icellR)%nod(1:3)) - k1 - k2

   if ( kL.lt.1 .or. kR.lt.1 ) goto 1234

!  check if right nodes were found
!  this might not be the case when the cell administration is out of date
   Lproceed = .false.
   do k=1,netcell(icellL)%N
      if ( netcell(icellL)%nod(k).eq.kL ) then
         Lproceed = .true.
         exit
      end if
   end do
   if ( .not.Lproceed ) goto 1234

   Lproceed = .false.
   do k=1,netcell(icellR)%N
      if ( netcell(icellR)%nod(k).eq.kR ) then
         Lproceed = .true.
         exit
      end if
   end do
   if ( .not.Lproceed ) goto 1234

!  compute the change in functional
   n1 = nmk(k1)- nmk_opt(k1)
   n2 = nmk(k2)- nmk_opt(k2)
   nL = nmk(kL)- nmk_opt(kL)
   nR = nmk(kR)- nmk_opt(kR)

   ntopo =  (n1-1)**2 + (n2-1)**2 + (nL+1)**2 + (nR+1)**2 -  &
            (n1**2+n2**2 +  nL**2    +  nR**2)

   if ( jalandbound.eq.1 ) then
!     take land boundary into account

      if ( lanseg_map(k1).gt.0 .and. lanseg_map(k2).gt.0 ) then
!        link is associated with a land boundary -> keep it
         ntopo = 1000
      else
         call comp_nnow(k1,k2,kL,n1L)
         call comp_nnow(k1,k2,kR,n1R)
         call comp_nnow(k2,k1,kL,n2R)
         call comp_nnow(k2,k1,kR,n2L)
         call comp_nnow(kL,k1,k2,nL)
         call comp_nnow(kR,k1,k2,nR)

         ntopo =  (n1L-1)**2 + (n1R-1)**2 + (n2L-1)**2 + (n2R-1)**2 + 2d0*((nL+1)**2 + (nR+1)**2) -  &
                  (n1L**2 + n1R**2 + n2L**2 + n2R**2 + 2d0*(nL**2 + nR**2) )

         if ( n1L.ne.n1R .or. n2L.ne.n2R ) then
            continue
         end if
      end if
   end if

1234 continue

   return
end subroutine comp_ntopo
