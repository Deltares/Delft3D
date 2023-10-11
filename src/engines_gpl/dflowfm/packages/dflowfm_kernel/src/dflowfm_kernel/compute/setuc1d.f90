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

   integer, parameter     :: JACSTOT = 0 !< 0 for computing the total area
   integer, parameter     :: JACSFLW = 1 !< 1 for computing the flow area
   integer, parameter     :: CALCCONV = 0 !< don't update wu, cfuhi, etc inside getprof_1D

   integer :: L, LL, La, n, nx, ip, i12, k2, ja1D

   double precision :: q_net_in  !< [m3/s] sum of inflowing Q minus sum of outflowing Q over links of node n
   double precision :: q_in      !< [m3/s] sum of inflowing Q over links of node n
   double precision :: q_out     !< [m3/s] sum of outflowing Q over links of node n
   double precision :: qu_in     !< [m4/s2] sum of Q*u over inflowing links of node n
   double precision :: qu_out    !< [m4/s2] sum of Q*u over outflowing links of node n (u = Q/A)
   double precision :: qu2_in    !< [m5/s3] sum of Q*u**2 over inflowing links of node n
   double precision :: qu2_out   !< [m5/s3] sum of Q*u**2 over outflowing links of node n (u = Q/A)
   double precision :: uc        !< [m/s] representative velocity magnitude at node n
   
   integer          :: L1        !< index of first link
   double precision :: h         !< [m] local water depth
   double precision :: hdx       !< [m] half link length
   double precision :: u         !< [m/s] velocity
   double precision :: q         !< [m3/s] discharge
   double precision :: perim     !< [m] dummy variable for wetted perimeter
   double precision :: fxar      !< [m2] cross-sectional flow area
   double precision :: sxar      !< [m2] cross-sectional total (flow + storage) area
   double precision :: sar       !< [m2] surface area of half link
   double precision :: tsar      !< [m2] total surface area of node -- equal to a1(n)
   double precision :: fwi       !< [m] surface width of flow area
   double precision :: swi       !< [m] surface width of total (flow + storage) area
   double precision :: dzwdt     !< [m/s] water level change rate
   double precision :: am        !< [-] weight of momentum conservation versus energy conservation
   
   double precision, allocatable, dimension (:) :: a ! [m] difference between node water level and link water level, absolute
   double precision, allocatable, dimension (:) :: b ! [-] difference between node water level and link water level, relative to difference of first link
   double precision :: sum_asar   !< [m3] sum of cumulative area times a
   double precision :: sum_bsar   !< [m2] sum of cumulative area times b
   integer          :: k          !< index 1 for link start and 2 for link end
   double precision :: kin_ene    !< [m] kinetic energy height
   double precision :: kin_ene1   !< [m] kinetic energy height of first link
   double precision :: dudz       !< [1/s] change in the flow velocity due to water level change
   double precision :: dedz       !< [-] change in energy height due to water level change
   double precision :: dedz1      !< [-] change in energy height due to water level change of first link
   double precision :: dz1        !< [m] difference between node water level and link water level of first link
   double precision :: sumq       !< [m3/s] sum of discharges

   if (kmx /= 0 .or. lnx1D == 0) return
   
   uc1D  = 0d0
   do n  = ndx2D+1,ndxi
      nx = nd(n)%lnx
      
      ja1D = 1
      do LL = 1,nx
         L   = nd(n)%ln(LL)
         La  = iabs(L)
         if (iabs(kcu(La)) /= 1) ja1D = 0
      enddo
      if (ja1D == 0) cycle
      if (jaJunction1D == 0 .and. nx > 2) cycle
      
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
   enddo
   
   do LL = lnxi+1,lnx          ! loop over open boundary links
      if (kcu(LL) == -1) then  ! 1D boundary link
         n = Ln(1,LL)
         
         ! a 1D boundary node has just one link (the boundary link)
         ! so the sign of the node is equal to the sign of the link
         uc1D(n) = u1(LL)
      endif
   enddo
   
   if (jaPure1D == 1 .or. jaPure1D == 2) then
      u1Du  = 0d0
      do L = 1,lnx
         if (qa(L) > 0 .and. abs(uc1D(ln(1,L))) > 0 ) then                               ! set upwind ucxu, ucyu  on links
            u1Du(L) = uc1D(ln(1,L))
         else if (qa(L) < 0 .and. abs(uc1D(ln(2,L))) > 0 ) then
            u1Du(L) = uc1D(ln(2,L))
         endif
      enddo
      
   elseif (jaPure1D >= 3) then
      
      allocate(a(100), b(100)) ! hardcoded limit to nodes with at most 100 links!
      q1D  = 0d0
      au1D = 0d0
      sar1D = 0d0
      volu1D = 0d0
      if (jaPure1D == 3) then
         alpha_mom_1D = 0d0
         alpha_ene_1D = 0d0
      elseif (jaPure1D == 4) then
         ds1u = 0d0
      endif
      do n  = ndx2D+1,ndxi
         nx = nd(n)%lnx
         
         ja1D = 1
         do LL = 1,nx
            L   = nd(n)%ln(LL)
            La  = iabs(L)
            if (iabs(kcu(La)) /= 1) ja1D = 0
         enddo
         if (ja1D == 0) cycle
         if (jaJunction1D == 0 .and. nx > 2) cycle
         
         !if (n == 578) then
         !    write(123,'(A)')          '--- [1] ---'
         !endif
         
         ! compute total net discharge into the node
         q_net_in = 0d0
         tsar = 0d0
         do LL = 1, nx                          ! loop over all links connected to the node
             L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
             La  = iabs(L)
             
             hdx = 0.5 * dx(La)
             if (L > 0) then ! link points to node
                 k = 2
             else ! link points from node
                 k = 1
             endif
             
             h = max(0d0, s1(n)-bob(k,La)) ! cross sectional area
             call getprof_1D(La, h, sxar, swi, JACSTOT, CALCCONV, perim)
             call getprof_1D(La, h, fxar, fwi, JACSFLW, CALCCONV, perim)
             sar = swi * hdx
             wu1D(k,La) = swi
             au1D(k,La) = fxar
             sar1D(k,La) = sar
             
             tsar = tsar + sar
             volu1D(La) = volu1D(La) + fxar * hdx
             
             !if (n == 578) then
             !    write(123,'(A,I18,A,I18,A,I18)')          'link: ',LL   ,' L   : ',La   ,' k   : ',k
             !    write(123,'(A,E18.12,A,E18.12)')          'q   : ',qa(La)
             !endif
             
             q_net_in = q_net_in + dble(sign(1,L)) * qa(La)
         enddo
         
         !if (n == 578) then
         !    write(123,'(A,E18.12)')          'qnet: ',q_net_in
         !    write(123,'(A)')          '--- [2] ---'
         !endif
         
         qu_in   = 0d0
         qu_out  = 0d0
         qu2_in  = 0d0
         qu2_out = 0d0
         dzwdt   = q_net_in / tsar
         do LL = 1, nx                          ! loop over all links connected to the node
             L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
             La  = iabs(L)
             
             if (L > 0) then ! link points to node: reduce by the storage on the remainder of the link
                 sar = sar1D(2,La)
                 q = qa(La) - sar * dzwdt
                 q1D(2,La) = q
                 h = max(0d0, s1(n)-bob(2,La)) ! cross sectional area
                 call getprof_1D(La, h, fxar, fwi, JACSFLW, CALCCONV, perim)
                 au1D(2,La) = fxar
             else ! link points from node: increase by the storage on the first part of the link
                 sar = sar1D(1,La)
                 q = qa(La) + sar * dzwdt
                 q1D(1,La) = q
                 h = max(0d0, s1(n)-bob(1,La)) ! cross sectional area
                 call getprof_1D(La, h, fxar, fwi, JACSFLW, CALCCONV, perim)
                 au1D(1,La) = fxar
             endif
             
             if (jaPure1D == 3) then
                if ((L*q) > 0d0) then ! inflowing: positive flow to this node, or negative flow from this node
                    u = u1(La)
                    if ((q*u) > 0) then ! flow direction at link equal to flow direction at node
                        qu_in  = qu_in  + abs(q * u)
                        qu2_in = qu2_in + abs(q * u**2)
                    else ! flow direction at link opposite to flow direction at node, so use 0 velocity inflow
                        ! no contribution if u = 0
                    endif
                else ! outflowing: negative flow to this node, or positive flow from this node
                    u = q / fxar
                    qu_out  = qu_out  + abs(q * u)
                    qu2_out = qu2_out + abs(q * u**2)
                endif
             endif
             
             volu1D(La) = volu1D(La) + 0.5d0 * fxar * dx(La)
         enddo
         
         if (jaPure1D == 3) then
            alpha_mom_1D(n) = min(1d0, qu_in / max(1e-20,qu_out))
            alpha_ene_1D(n) = min(1d0, qu2_in/ max(1e-20,qu2_out))
            
         elseif (jaPure1D == 4) then
         
            !if (n == 578) then
            !    write(123,*) 'node: ',n
            !endif
            
            sum_asar = 0d0
            sum_bsar = 0d0
            sumq = 0d0
            do LL = 1, nx                          ! loop over all links connected to the node
                L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
                La  = iabs(L)
                
                if (L > 0) then ! link points to node
                    k = 2
                    sumq = sumq + q1D(k,La)
                else ! link points from node
                    k = 1
                    sumq = sumq - q1D(k,La)
                endif
                
                sar  = sar1D(k,La)
                fxar = au1D(k,La)
                q = q1D(k,La)
                swi = wu1D(k,La)
                
                u = q / fxar
                kin_ene = u*u / (2d0*ag)
                dudz = - swi*q / (fxar*fxar)
                dedz = 1 + (u / ag) * dudz
                
                if (LL == 1) then
                    kin_ene1 = kin_ene
                    dedz1 = dedz
                endif
                
                a(LL) = (kin_ene1 - kin_ene) / dedz
                b(LL) = dedz1 / dedz
                sum_asar = sum_asar + a(LL) * sar
                sum_bsar = sum_bsar + b(LL) * sar
                
                !if (n == 578) then
                !    write(123,'(A,I18,A,I18,A,I18)')          'link: ',LL   ,' L   : ',La   ,' k   : ',k
                !    write(123,'(A,E18.12,A,E18.12)')          's1  : ',s1(n),' bob : ',bob(k,La)
                !    write(123,'(A,E18.12,A,E18.12)')          'q   : ',q    ,' sumq: ',sumq
                !    write(123,'(A,E18.12,A,E18.12)')          'xar : ',xar  ,' u   : ',u
                !    write(123,'(A,E18.12,A,E18.12,A,E18.12)') 'swi : ',swi  ,' sar : ',sar  ,' Ekin: ',kin_ene
                !    write(123,'(A,E18.12,A,E18.12)')          'dudz: ',dudz ,' dedz: ',dedz
                !    write(123,'(A,E18.12,A,E18.12)')          'a   : ',a(LL),' b   : ',b(LL)
                !    write(123,'(A,E18.12,A,E18.12)')          'asar: ',a(LL) * sar,' bsar: ',b(LL) * sar
                !    write(123,'(A,E18.12,A,E18.12)')          'asum: ',sum_asar,' bsum: ',sum_bsar
                !endif
            enddo
            
            dz1 = - sum_asar / sum_bsar
            !if (n == 578) then
            !    write(123,'(A,E18.12,A,E18.12,A,E18.12)') 'asum: ',sum_asar,' bsum: ',sum_bsar,' dz1 : ',dz1
            !endif
            do LL = 1, nx                          ! loop over all links connected to the node
                L   = nd(n)%ln(LL)                 ! positive if link points to node, negative if links points from node
                La  = iabs(L)
                
                if (L > 0) then ! link points to node: reduce by the storage on the remainder of the link
                    k = 2
                else ! link points from node: increase by the storage on the first part of the link
                    k = 1
                endif
                
                ds1u(k,La) = a(LL) + b(LL)*dz1
                
                h = max(0d0, s1(n)+ds1u(k,La)-bob(k,La)) ! cross sectional area
                call getprof_1D(La, h, fxar, fwi, JACSFLW, CALCCONV, perim)
                au1D(k,La) = fxar
                
                !if (n == 578) then
                !    write(123,'(A,E18.12,A,E18.12)') 'ds1 : ',ds1u(k,La),' fxar : ',fxar
                !endif
                ! u = q / fxar
                ! E = ds1u + u*u/(2*ag) should be approximately equal on all links
            enddo
            
            !if (n == 578) then
            !    write(123,'(A)') '------------'
            !    write(123,'(A)') ' '
            !endif
         endif
      enddo
      deallocate(a, b)
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
