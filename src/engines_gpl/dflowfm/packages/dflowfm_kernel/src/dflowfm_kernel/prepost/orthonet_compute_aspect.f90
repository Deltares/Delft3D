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

!>  compute link-based aspect ratios
subroutine orthonet_compute_aspect(aspect)
   use m_netw
   use m_flowgeom
   use m_missing
   use m_orthosettings
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   IMPLICIT NONE

   double precision, dimension(numL)             :: aspect           !< aspect-ratios at the links

   double precision                              :: x0, y0, x1, y1, x0_bc, y0_bc
   double precision                              :: xL, yL, xR, yR
   double precision                              :: SLR, R01, dinRy
   double precision                              :: xc, yc

   double precision, allocatable, dimension(:,:) :: R ! averaged netlink length at both sides of the netlink
   double precision, allocatable, dimension(:)   :: S ! flowlink lengths

   integer                                       :: k, kk, kkm1, kkp1, kkp2
   integer                                       :: klink, klinkm1, klinkp1, klinkp2, N

   integer                                       :: k0, k1, kL, kR, L, ja

   logical,          allocatable, dimension(:)   :: Liscurvi   ! node-based curvi-like indicator

   double precision                              :: ortho1

   double precision, external                    :: dprodin
   double precision, parameter                   :: EPS=1D-4

   allocate(R(2,numL), S(numL), Liscurvi(numk))
   R = DMISS
   S = DMISS

!  compute parallel length S
   do L = 1,numL
!     nodes connected by the link
      k0 = kn(1,L)
      k1 = kn(2,L)

      if ( k0.eq.0 .or. k1.eq.0 ) then ! safety
         continue
         cycle
      end if

      x0 = xk(k0)
      y0 = yk(k0)

      x1 = xk(k1)
      y1 = yk(k1)

!     compute the link length R01
      R01 = dbdistance(x0,y0,x1,y1, jsferic, jasfer3D, dmiss)

!     find left cell center, if it exists
      kL  = lne(1,L)        ! left cell center w.r.t. link L

      if ( lnn(L).gt.0 ) then
         xL = xz(kL)
         yL = yz(kL)
      else
         xL = x0
         yL = y0
      end if

!     find right cell center, if it exists
      if (lnn(L) == 2) then
         kR = lne(2,L)
         xR = xz(kR)
         yR = yz(kR)
      else
!---------------------------------------------------------------------
!     otherwise, make ghost node by imposing boundary condition
         dinry = dprodin(x0,y0, x1,y1, x0,y0, xL,yL) / max(R01*R01, EPS)

         x0_bc = (1-dinRy) * x0 + dinRy * x1
         y0_bc = (1-dinRy) * y0 + dinRy * y1

         xR    = 2d0 * ( x0_bc ) - xL
         yR    = 2d0 * ( y0_bc ) - yL
!---------------------------------------------------------------------
      end if

      SLR = dbdistance(xL,yL,xR,yR,jsferic, jasfer3D, dmiss)
!      if ( R01.ne.0d0 ) then
!         aspect(L) = SLR/R01
!      else
!         aspect(L) = DMISS
!      end if

!     store length S (normal)
      S(L) = SLR

!     debug: plot circumcenters
!      call cirr(xL,yL,31)
!      if ( lnn(L).eq.1 ) then
!         call cirr(xR,yR,31)
!         call cirr(x0_bc, y0_bc, 191)
!         call hitext(kL, xL, yL)
!         call movabs(xL,yL)
!         call lnabs(x0_bc, y0_bc)
!      end if
   enddo
!   call confrm(' ', ja)

!---------------------------------------------------------------------
!  quads -> mimic the curvi-grid discretization

!  node-based curvi-like indicator; initialization
   Liscurvi = .true.

!  compute normal length R
   do k=1,nump
      N = netcell(k)%N
      if ( N.lt.3 ) cycle  ! safety

!     repeat for all links
      do kk=1,N

!        node-based curvi-like indicator
         if ( N.ne.4 ) Liscurvi(netcell(k)%nod(kk)) = .false.

         klink = netcell(k)%lin(kk)
         if ( lnn(klink).ne.1 .and. lnn(klink).ne.2 ) cycle

!        get the other links in the right numbering
         kkm1 = kk-1; if ( kkm1.lt.1 ) kkm1=kkm1+N
         kkp1 = kk+1; if ( kkp1.gt.N ) kkp1=kkp1-N
         kkp2 = kk+2; if ( kkp2.gt.N ) kkp2=kkp2-N

         klinkm1 = netcell(k)%lin(kkm1)
         klinkp1 = netcell(k)%lin(kkp1)
         klinkp2 = netcell(k)%lin(kkp2)

         R01 = dblinklength(klink)

         if ( R01.ne.0d0 ) then
            aspect(klink) = S(klink) / R01
         end if

!        store length R (parallel)
!         if ( N.eq.4 .and. lnn(klink).ne.1 ) then ! inner quads
         if ( N.eq.4 ) then ! quads
            R01 = 0.5d0*(dblinklength(klink  ) + dblinklength(klinkp2))
         else
            R01 = dblinklength(klink)
         end if

         if ( R(1,klink).eq.DMISS ) then  ! link visited for the first time
            R(1,klink) = R01
         else                             ! link visited for the second time
            R(2,klink) = R01
         end if
      end do
   end do

   if ( ortho_pure.eq.1d0 ) goto 1234     ! no curvi-like discretization

   ortho1 = 1d0 - ortho_pure

!  compute aspect ratio in the quadrilateral part of the mesh
   do klink=1,numL
      if ( kn(1,klink).eq.0 .or. kn(2,klink).eq.0 ) cycle   ! safety

      if ( lnn(klink).ne.2 .and. lnn(klink).ne.1 ) cycle

!     quads-only
      if ( .not.Liscurvi(kn(1,klink)) .or. .not.Liscurvi(kn(2,klink)) ) cycle

!      if ( netcell(lne(1,klink))%N.ne.4 .or. netcell(lne(min(2,lnn(klink)),klink))%N.ne.4 ) cycle ! quad-quad links only

      if ( lnn(klink).eq.1 ) then
         if (R(1,klink).ne.0d0 .and. R(1,klink).ne.DMISS ) then
            aspect(klink) = S(klink) / R(1,klink)
         else
            continue
         end if
      else
         if (R(1,klink).ne.0d0 .and. R(2,klink).ne.0d0 .and. R(1,klink).ne.DMISS .and. R(2,klink).ne.DMISS ) then
            aspect(klink) = ortho_pure * aspect(klink)+ ortho1 * S(klink) / ( 0.5d0*(R(1,klink)+R(2,klink)) )
         else
            continue
         end if
      end if
   end do

1234 continue

   deallocate(R, S, Liscurvi)

contains

!  compute link length
   double precision function dblinklength(kk)
      use m_netw
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer  :: kk !< link number

      dblinklength = dbdistance(xk(kn(1,kk)), yk(kn(1,kk)), xk(kn(2,kk)), yk(kn(2,kk)), jsferic, jasfer3D, dmiss)

   end function dblinklength


end subroutine orthonet_compute_aspect
