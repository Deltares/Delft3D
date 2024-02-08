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

   subroutine disable_higherorder_at_sorsin()
      use m_flowgeom
      use m_flowexternalforcings, only: numsrc, ksrc
      use m_partitioninfo
      use m_alloc
      implicit none

      double precision, dimension(:,:), allocatable :: dum

      integer,          dimension(:),   allocatable :: imask

      integer                                       :: ierror
      integer                                       :: i, iL, kk, kk1, kk2, LL, n

      logical                                       :: Ldisabled

      integer,                          parameter   :: jaall = 1 !< disable all flowlinks attached to flownodes with sources/sinks (1) or only links with both connected flownodes (0)

      if ( jaall.eq.1 ) then
!        disable all flowlink attached to flownodes with sources/sinks
         do n=1,numsrc
            do i=1,4,3 ! 1 and 4
!              get 2D flow nodes
               kk = ksrc(i,n)
               if ( kk.le.0 ) cycle ! 0: not in whole domain, -1: not in own subdomain, but can be in ghostregion

!              loop over all attached flow links
               do iL = 1,nd(kk)%lnx
!                 get 2D flink link
                  LL = iabs(nd(kk)%ln(iL))

!                 disable high-order reconstruction
                  klnup(:,LL) = 0
               end do
            end do
         end do
      else
!        disable only flowlinks connecting two flownodes with sources/sinks

!        alloc mask array
         call realloc(imask, Ndx, keepExisting=.false., fill=0)

!        mask flownodes with sources
         do n=1,numsrc
            imask(ksrc(1,n)) = 1
            imask(ksrc(4,n)) = 1
         end do

!        disable flowlinks
         do LL=1,Lnx
            kk1 = ln(1,LL)
            kk2 = ln(2,LL)
            if ( imask(kk1).eq.1 .and. imask(kk2).eq.1 ) then
               klnup(:,LL) = 0
            end if
         end do

!        deallocate
         if ( allocated(imask) ) deallocate(imask)
      end if

      if ( jampi.eq.1 ) then
!        source/sink could have been in ghost region
         allocate(dum(6,Lnx));

         do LL=1,Lnx
            do i=1,6
               dum(i,LL) = klnup(i,LL)
            end do
         end do

         call update_ghosts(ITYPE_U, 6, Lnx, dum, ierror)

         do LL=1,Lnx
!           check if higher-order reconstruction of this link has been disabled
            Ldisabled = .true.
            do i=1,6
               if ( dum(i,LL).ne.0d0 ) then
                  Ldisabled = .false.
                  exit
               end if
            end do

            if ( Ldisabled ) then
!              disable higher-order reconstruction
               do i=1,6
                  if ( klnup(i,LL).ne.0 ) then
                     klnup(i,LL) = 0
                  end if
               end do
            end if
         end do

         deallocate(dum)
      end if

      return
   end subroutine disable_higherorder_at_sorsin
