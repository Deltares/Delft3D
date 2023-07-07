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

! =================================================================================================
! =================================================================================================
   subroutine setuc1D ()
   use m_netw
   use m_flow
   use m_flowgeom
   implicit none
   integer :: L, LL, La, n, nx, ip, i12, k2, ja1D

   double precision :: qu_in    !< sum of Q*u over inflowing links of node n
   double precision :: q_in     !< sum of abs(Q) over inflowing links of node n
   double precision :: qu_out   !< sum of Q*u over outflowing links of node n
   double precision :: q_out    !< sum of abs(Q) over outflowing links of node n
   double precision :: uc       !< representative velocity magnitude at node n
   
   integer          :: L1       !< index of first link

   if (kmx == 0 .and. lnx1D > 0) then ! setuc
      uc1D  = 0d0
      do n  = ndx2D+1,ndxi
         nx = nd(n)%lnx
         
         ja1D = 1
         do LL = 1,nx
            L   = nd(n)%ln(LL)
            La  = iabs(L)
            if (iabs(kcu(La)) /= 1) ja1D = 0
         enddo
         
         if (ja1D == 1) then
            qu_in = 0d0
            qu_out = 0d0
            q_in = 0d0
            q_out = 0d0
            do LL = 1, nx                          ! loop over all links of the upstream node
                L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
                La  = iabs(L)
                
                if (L*u1(La) >= 0d0) then ! inflowing: positive flow to this node, or negative flow from this node
                    qu_in = qu_in + qa(La) * u1(La)
                    q_in  = q_in  + abs(qa(La))
                else ! outflowing: positive flow from this node, or negative flow to this node
                    qu_out = qu_out + qa(La) * u1(La)
                    q_out  = q_out  + abs(qa(La))
                endif
            enddo
            
            if (q_in > 0d0 .and. q_out > 0d0) then
                uc = 0.5d0 * (qu_in/q_in + qu_out/q_out)
            else ! all inflow, all outflow, or stagnant
                uc = 0d0
            endif
            
            L1 = iabs(nd(n)%ln(1))
            uc1D(n) = sign(uc, u1(L1))
            
            if (japure1D > 0) then
               do LL = 1, nx                          ! loop over all links of the upstream node
                   L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
                   La  = iabs(L)
                   
                   if (L*u1(La) < 0d0) then ! outflowing: positive flow from this node, or negative flow to this node
                       u1Du(La) = sign(uc,u1(La))
                   endif
               enddo
            endif
         endif
      enddo

      do LL = lnxi+1,lnx          ! loop over open boundary links
         if (kcu(LL) == -1) then  ! 1D boundary link
            n = Ln(1,LL)
            
            ! a 1D boundary node has just one link (the boundary link)
            ! so the sign of the node is equal to the sign of the link
            uc1D(n) = u1(LL)
            
            ! inflow
            if (u1(LL) > 0d0 .and. japure1D > 0) then
                u1Du(LL) = u1(LL)
            endif
         endif
      enddo

      if (jaPure1D > 0) then
         u1Du  = 0d0
         do L = 1,lnx
            if (qa(L) > 0 .and. abs(uc1D(ln(1,L))) > 0 ) then                               ! set upwind ucxu, ucyu  on links
               u1Du(L) = uc1D(ln(1,L))
            else if (qa(L) < 0 .and. abs(uc1D(ln(2,L))) > 0 ) then
               u1Du(L) = uc1D(ln(2,L))
            endif
         enddo
      endif

   endif

   end subroutine setuc1D

   subroutine setisnbnodisnblin()
   use m_flow
   use m_flowgeom
   use m_netw
   implicit none
   integer :: L, LL, LLL, LLLa, La, L1, L2, L1a, L2a, n, nx, ip, i12, k2, ja1D

   if (allocated (isnbnod) ) deallocate(isnbnod)
   if (allocated (isnblin) ) deallocate(isnblin)
   allocate(isnbnod(2,lnx), isnblin(2,lnx))

   if (kmx == 0 .and. lnx1D > 0) then ! setuc
      kc      = 0
      isnbnod = 0
      isnblin = 0
      do n  = ndx2D+1,ndx
         nx = nd(n)%lnx
         if (nx == 2) then
            ja1D  = 1
            do LL = 1,nx
               LLL  = nd(n)%ln(LL)
               LLLa = iabs(LLL)
               if (iabs(kcu(LLLa)) /= 1) then 
                  ja1D = 0
                  exit
               endif
            enddo
            kc(n) = ja1D
            if (ja1D == 1) then 
               L1   = nd(n)%ln(1)        ! uc1D on a node follows sign of u1 of its first link
               L1a  = iabs(L1)
               L2   = nd(n)%ln(2)        ! this is the second link
               L2a  = iabs(L2)

               if (L1 > 0) then          ! first link is incoming for node n
                  isnbnod(2,L1a) =  1    ! node is on side 2 of first link
                  if (L2 < 0) then       ! second link is outgoing 
                     isnbnod(1,L2a) =  1 ! so follows sign of node on left side
                  else 
                     isnbnod(2,L2a) = -1 ! so follows sign of node on right side
                  endif
               else                      ! first link is outgoing for node n
                  isnbnod(1,L1a) =  1    ! node has sign of first link 
                  if (L2 < 0) then       ! second link is outgoing 
                     isnbnod(1,L2a) = -1 ! so follows sign of node on left side
                  else 
                     isnbnod(2,L2a) =  1 ! so follows sign of node on left side
                  endif
               endif

            endif
         endif
      enddo

   endif 

   do L = 1,lnx
      if (isnbnod(1,L ) .ne. 0) then
          n =  ln(1,L) 
          if (nd(n)%ln(1)*nd(n)%ln(2) < 0) then
              isnblin(1,L) =  1
          else 
              isnblin(1,L) = -1
          endif
      endif
      if (isnbnod(2,L ) .ne. 0) then
          n =  ln(2,L) 
          if (nd(n)%ln(1)*nd(n)%ln(2) < 0) then
              isnblin(2,L) =  1
          else 
              isnblin(2,L) = -1
          endif
      endif
   
   enddo

   deallocate(isnbnod) ! no time now to make efficient version

   end subroutine setisnbnodisnblin
