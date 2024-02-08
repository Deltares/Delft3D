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

!> find one-dimensional net cells
!>    it is assumed that kc has been allocated
!>    it is assumed that findcells has already been called (for 2d cells)
   subroutine find1dcells()
      use network_data
      use m_alloc
      use m_flowgeom, only: xz, yz, ba
      use gridoperations
      use MessageHandling
      implicit none

      integer :: K1, K2, K3, L, LNX1D, N, NC1, NC2
      integer :: i, ierr, k, kcell

      logical :: Lisnew

      integer :: ierror

      ierror = 1

      nump1d2d = nump

!     BEGIN COPY from flow_geominit
      KC    = 2                                           ! ONDERSCHEID 1d EN 2d NETNODES

      DO L  = 1, NUML
         K1 = KN(1,L) ; K2 = KN(2,L) ; K3 = KN(3,L)
         IF (K3 >= 1 .and. K3 <= 7) THEN
            KC(K1) = 1 ; KC(K2) = 1
         ENDIF
      ENDDO

      DO L = 1, NUML1D

         K1  = KN(1,L) ; K2  = KN(2,L)
         if (k1 == 0) cycle

         NC1 = 0       ; NC2 = 0
         IF ( kn(3,L) .ne. 1 .and. kn(3,L) .ne. 6 ) THEN !These link types are allowed to have no 2D cells
            IF (NMK(K1) == 1) THEN
               CALL INCELLS(XK(K1), YK(K1), NC1)       ! IS INSIDE 2D CELLS()
            ENDIF
            IF (NMK(K2) == 1) THEN
               CALL INCELLS(XK(K2), YK(K2), NC2)
            ENDIF
            IF (NC1 == 0 .and. NC2 == 0) THEN
               call mess(LEVEL_WARN, '1D2D link without valid 2D flowcell detected, discarding!')
               cycle
            endif
         endif

         if (nc1 .ne. 0 .and. nc1 == nc2) then
             ! Both net nodes inside 2D cell, but assume that the first is
             ! then the 1D net node (because unc_read_net_ugrid() always
             ! sets the 1D side of 1D2D mesh contacts as the first net link
             ! node kn(1,L)).
             nc1 = 0
         endif

         LNN(L) = 0
         IF (NC1 == 0 ) THEN
            IF ( KC(K1) == 1 .and. ( NMK(K1) > 1 .or. (kn(3,l) == 1 .or. kn(3,l) == 6) ) )  THEN! NIEUWE 1d CELL
               nump1d2d =  nump1d2d + 1
               KC(K1)   = -nump1d2d             ! MARKEREN ALS OUD       
               LNE(1,L) = -iabs(KC(K1))         ! NEW 1D CELL flag 1D links through negative lne ref
               LNN(L) = LNN(L) + 1
            else if ( KC(K1) /= 1) then
               LNE(1,L) = -iabs(KC(K1))         ! NEW 1D CELL flag 1D links through negative lne ref
               LNN(L) = LNN(L) + 1
            else
               call mess(LEVEL_WARN, '1D2D link without valid 1D branch detected, discarding!')
            endif
         ELSE
            LNE(1,L) =       NC1                ! ALREADY EXISTING 2D CELL
            LNN(L) = LNN(L) + 1
         ENDIF
         IF (NC2 == 0) THEN
            IF ( KC(K2) == 1 .and. ( NMK(K2) > 1 .or. (kn(3,l) == 1 .or. kn(3,l) == 6) ) ) THEN! NIEUWE 1d CELL
               nump1d2d =  nump1d2d + 1
               KC(K2)   = -nump1d2d
               LNE(2,L) = -iabs(KC(K2))               ! NEW 1D CELL
               LNN(L) = LNN(L) + 1                
            else if (KC(k2) /= 1) then
               LNE(2,L) = -iabs(KC(K2))               ! NEW 1D CELL
               LNN(L) = LNN(L) + 1   
            else
               call mess(LEVEL_WARN, '1D2D link without valid 1D branch detected, discarding!')
            endif
         ELSE
            LNE(2,L) =       NC2                   ! ALREADY EXISTING 2D CELL
            LNN(L) = LNN(L) + 1
         ENDIF
       ENDDO

!     END COPY from flow_geominit

!     fill 1D netcell administration and set cell centers
      call realloc(xzw, nump1d2d)
      call realloc(yzw, nump1d2d)
      call realloc(xz,  nump1d2d)
      call realloc(yz,  nump1d2d)
      call realloc(ba, nump1d2d, KeepExisting=.true., fill=0d0)   ! 1D ba's will be filled halfway through flow_geominit, just allocate and initialize 1D part here
      call increasenetcells(nump1d2d, 1.0, .true.)
      do k=nump+1,nump1d2d
         netcell(k)%N = 0
         call realloc(netcell(k)%NOD, 1, stat=ierr, keepExisting=.false., fill=0)
         call realloc(netcell(k)%LIN, 1, stat=ierr, keepExisting=.false., fill=0)
      end do

      do k=1,numk
         if ( kc(k).lt.0 ) then      ! 1d cell
            nc1 = -kc(k)             ! cell number
            N = netcell(nc1)%N
!           check if this node is new in this cell
            Lisnew = .true.
            do i=1,N
               if ( netcell(nc1)%nod(i).eq.k ) then
                  Lisnew = .false.
                  exit
               end if
            end do
            if ( Lisnew ) then   ! new node for this cell
               N = N+1
               if ( N.gt.1 ) then
                  call realloc(netcell(nc1)%NOD, N, stat=ierr, keepExisting=.true., fill=0)
                  call realloc(netcell(nc1)%LIN, N, stat=ierr, keepExisting=.true., fill=0)
               end if
               netcell(nc1)%N = N
               netcell(nc1)%nod(N) = k
            end if
         end if
      end do

!      do L=1,numL1d
!         k1 = kn(1,L)
!         k2 = kn(2,L)
!         nc1 = kc(k1)
!         nc2 = kc(k2)
!         if ( nc1.lt.0 ) then
!            kcell = -nc1
!            N = netcell(kcell)%N + 1
!            if ( N.gt.2 ) then
!               call realloc(netcell(kcell)%nod, N, stat=ierr, keepExisting=.true., fill=0)
!               call realloc(netcell(kcell)%lin, N, stat=ierr, keepExisting=.true., fill=0)
!            end if
!            netcell(kcell)%N      = N
!            netcell(kcell)%nod(N) = k2
!            netcell(kcell)%lin(N) = L
!         end if
!         if ( nc2.lt.0 ) then
!            kcell = -nc2
!            N = netcell(kcell)%N + 1
!            if ( N.gt.2 ) then
!               call realloc(netcell(kcell)%nod, N, stat=ierr, keepExisting=.true., fill=0)
!               call realloc(netcell(kcell)%lin, N, stat=ierr, keepExisting=.true., fill=0)
!            end if
!            netcell(kcell)%N      = N
!            netcell(kcell)%nod(N) = k1
!            netcell(kcell)%lin(N) = L
!         end if
!      end do

      do k=1,numk
         if ( kc(k).lt.0 ) then  ! 1d cell associated with net node k
            kcell = -kc(k)
            xzw(kcell) = xk(k)
            yzw(kcell) = yk(k)
            xz(kcell)  = xk(k)
            yz(kcell)  = yk(k)
         end if
      end do

!     safety: 1D-cells can have negative lne, which will cause problems
      if ( nump1d2d.gt.nump ) then
         netstat = NETSTAT_CELLS_DIRTY
      end if

      ierror = 0
 1234 continue

      return
   end subroutine find1dcells
