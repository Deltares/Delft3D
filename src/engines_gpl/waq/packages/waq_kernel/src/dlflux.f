!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dlflux

      implicit none

      contains


      subroutine dlflux ( jstart , nmmaxj , nmmax  , kmax   , nosys  ,
     &                    notot  , icx    , icy    , intsrt , icreep ,
     &                    kfu    , kfv    , kfs    , kcs    , kadu   ,
     &                    kadv   , qxk    , qyk    , qzk    , difx   ,
     &                    dify   , difz   , r0     , r1     , guv    ,
     &                    gvu    , dicww  , areau  , areav  , gsqs   ,
     &                    s0     , dps    , thick  , sigdif , sigmol ,
     &                    vicmol , dfluxx , dfluxy , fluxx  , fluxy  ,
     &                    fluxz  )

!     Deltares Software Centre

!>\File
!>         Computes fluxes for the Delwaq mass balances
!>
!>         - This routine is a small difu routine that saves the fluxes.
!>           It has been chosen not to implement the fluxes in the difu routine
!>           to let it be as close as possible resembling the FLOW difu routine.
!>         - For anticreep this philosophy has been left, because the incorporation
!>           of anti creep into this routine was considered too cumbersome.
!>         - The mass balance does not fit exactly, because difu iterates the
!>           implicit part. The r1 in the mass balance should be the r1 preceeding
!>           this one, which is the result of the last iteration. If iterations
!>           have converged sufficiently, the difference is however not big.

!     Created             : August    1996 by Erik de Goede

!     Modified            : Somewhere 2010 by Arjen Markus to incorporate Anti-Creep
!                           August    2011 by Leo Postma Fortran 90 look and feel

      use timers          ! WAQ performance timers

      implicit none

!     Arguments

!     kind        function         name                                 description

      integer(4), intent(in   ) :: jstart                             !< horizontal start of the FLOW arrays
      integer(4), intent(in   ) :: nmmaxj                             !< horizontal end of the FLOW arrays
      integer(4), intent(in   ) :: nmmax                              !< nmax*mmax, active dimension FLOW arrays
      integer(4), intent(in   ) :: kmax                               !< number of layers FLOW arrays
      integer(4), intent(in   ) :: nosys                              !< number of transported substances
      integer(4), intent(in   ) :: notot                              !< total number of substances
      integer(4), intent(in   ) :: icx                                !< increment x of ADI cake-walk
      integer(4), intent(in   ) :: icy                                !< increment y of ADI cake-walk
      integer(4), intent(in   ) :: intsrt                             !< 19 or 20 for this routine
      integer(4), intent(in   ) :: icreep                             !< if 1, anti-creep is on
      integer(4), intent(in   ) :: kfu   (jstart:nmmaxj)              !< time variable feature in implicit velocity points
      integer(4), intent(in   ) :: kfv   (jstart:nmmaxj)              !< time variable feature in explicit velocity points
      integer(4), intent(in   ) :: kfs   (jstart:nmmaxj)              !< time variable feature of cells
      integer(4), intent(in   ) :: kcs   (jstart:nmmaxj)              !< constant feature of cells
      integer(4), intent(in   ) :: kadu  (jstart:nmmaxj,  kmax)       !< allways 1: structure feature in u points
      integer(4), intent(in   ) :: kadv  (jstart:nmmaxj,  kmax)       !< allways 1: structure feature in v points
      real   (4), intent(in   ) :: qxk   (jstart:nmmaxj,  kmax)       !< flow in implicit direction
      real   (4), intent(in   ) :: qyk   (jstart:nmmaxj,  kmax)       !< flow in explicit direction
      real   (4), intent(in   ) :: qzk   (jstart:nmmaxj,0:kmax)       !< flow in vertical direction
      real   (4), intent(in   ) :: difx  (jstart:nmmaxj,  kmax)       !< diffusion implicit direction
      real   (4), intent(in   ) :: dify  (jstart:nmmaxj,  kmax)       !< diffusion in explicit direction
      real   (4), intent(in   ) :: difz  (jstart:nmmaxj,0:kmax)       !< diffusion in vertical direction
      real   (4), intent(in   ) :: r0    (jstart:nmmaxj,  kmax,notot) !< concentration at start of time step
      real   (4), intent(in   ) :: r1    (jstart:nmmaxj,  kmax,notot) !< concentration at stop of time step
      real   (4), intent(in   ) :: guv   (jstart:nmmaxj)              !< dispersion length in explicit direction
      real   (4), intent(in   ) :: gvu   (jstart:nmmaxj)              !< dispersion length in implicit direction
      real   (4), intent(in   ) :: dicww (jstart:nmmaxj,0:kmax)       !< vertical diffusion coeff. at layer interfaces
      real   (4), intent(in   ) :: areau (jstart:nmmaxj,  kmax)       !< exchange surface in implicit direction
      real   (4), intent(in   ) :: areav (jstart:nmmaxj,  kmax)       !< exchange surface in explicit direction
      real   (4), intent(in   ) :: gsqs  (jstart:nmmaxj)              !< horizontal surface area per horizontal cell
      real   (4), intent(in   ) :: s0    (jstart:nmmaxj)              !< always zero
      real   (4), intent(in   ) :: dps   (jstart:nmmaxj)              !< depth from surface to bed of column at old time level
      real   (4), intent(in   ) :: thick (kmax)                       !< relative layer thickness
      real   (4), intent(in   ) :: sigdif(nosys)                      !< Prandtl/Schmidt-numbers for substances
      real   (4), intent(in   ) :: sigmol(nosys)                      !< Molecular Prandtl/Schmidt-numbers for substances
      real   (4), intent(in   ) :: vicmol                             !< Molecular viscosity
      real   (4), intent(in   ) :: dfluxx(jstart:nmmaxj,  kmax,notot) !< Horizontal Dispersion flux if anticreep (X)
      real   (4), intent(in   ) :: dfluxy(jstart:nmmaxj,  kmax,notot) !< Horizontal Dispersion flux if anticreep (Y)
      real   (4), intent(  out) :: fluxx (jstart:nmmaxj,  kmax,nosys) !< flux in implicit direction per substance
      real   (4), intent(  out) :: fluxy (jstart:nmmaxj,  kmax,nosys) !< flux in explicit direction per substance
      real   (4), intent(  out) :: fluxz (jstart:nmmaxj,  kmax,nosys) !< flux in the vertical per substance

!     locals

      integer(4)   l, k, nm                      !  loop counters
      integer(4)   nmdd, nmd, nmu, nmuu, nmuuu   !  pointers in the implicit computational stencil
      integer(4)   ndm, num                      !  pointers in the explicit computational stencil
      real   (4)   qxu                           !  help variable for implicit flow
      real   (4)   qyv                           !  help variable for explicit flow
      real   (4)   qzw                           !  help variable for vertical flow
      integer(4)   iad1, iad2, iad3              !  cell features in implicit computational stencil
      integer(4)   j1, j2, j3                    !  implicit computational stencil
      real   (4)   flux, flux1, flux2            !  help variables for computed flux
      integer(4)   kfw                           !  determines vertical upwind or central behavior
      real   (4)   adza, adzc, ddzc              !  help variables vertical flux
      real   (4)   tsg, h0, h0i, diz1            !  curiosa for vertical diffusion

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlflux", ithandl )

!     Initialisation

      fluxx = 0.0
      fluxy = 0.0
      fluxz = 0.0

!     Advective transport in implicit-direction

      do 20 k = 1,kmax
         nmd  = -icx
         nmdd = -icx - icx
         nmu  =  icx
         nmuu =  icx + icx
         do 10 nm = 1,nmmax
            nmd  = nmd  + 1
            nmdd = nmdd + 1
            nmu  = nmu  + 1
            nmuu = nmuu + 1
            if ( kfs(nm) .eq. 1 ) then
               nmuuu= min ( nmmaxj,nm+3*icx)
               qxu = qxk(nm,k) / 6.0
               if ( qxu .gt. 0.0 ) then
                  iad1 =      kfu(nm  )*kadu(nm  ,k)
                  iad2 = iad1*kfu(nmd )*kadu(nmd ,k)
                  if ( kcs(nmu) .eq. 3 ) iad2 = 0
                  iad3 = iad2*kfu(nmdd)*kadu(nmdd,k)

                  j1 = 6*iad1 + 3*iad2+   iad3
                  j2 =        - 3*iad2- 2*iad3
                  j3 =                    iad3
                  do l=1,nosys
                     flux = ( j1 * r1(nm ,k,l) + j2 * r1(nmd ,k,l) + j3 * r1(nmdd ,k,l) ) * qxu
                     if ( kcs(nm) .ne. 0 ) fluxx(nm,k,l) = fluxx(nm,k,l) + flux
                  enddo
               else
                  iad1 =      kfu(nm  )*kadu(nm  ,k)
                  iad2 = iad1*kfu(nmu )*kadu(nmu ,k)
                  if ( kcs(nmd) .eq. 3 ) iad2 = 0
                  iad3 = iad2*kfu(nmuu)*kadu(nmuu,k)
                  j1 = 6*iad1 + 3*iad2 +   iad3
                  j2 =        - 3*iad2 - 2*iad3
                  j3 =                     iad3
                  do l=1,nosys
                     flux = ( j1 * r1(nmu,k,l) + j2 * r1(nmuu,k,l) + j3 * r1(nmuuu,k,l) ) * qxu
                     if ( kcs(nm) .ne. 0 ) fluxx(nm,k,l) = fluxx(nm,k,l) + flux
                  enddo
               endif
            endif
  10     continue
  20  continue

!     Advective transport in explicit-direction

      do 50 l = 1,nosys
         do 40 k = 1,kmax
            ndm = -icy
            num =  icy
            do 30 nm = 1,nmmax
               ndm = ndm + 1
               num = num + 1
               if ( kfs(nm) .eq. 1 ) then
                  qyv = qyk(nm,k)
                  iad1 =kfv(nm)*kadv(nm,k)
                  iad2 =iad1*kfv(num)*kadv(num,k)*kfv(ndm)*kadv(ndm,k)
                  if (kcs(num).ge.2) iad2=0
                  if ( qyv .gt. 0.0 ) then
                     flux = 0.5*qyv*((2*iad1-iad2)*r0(nm ,k,l)+ iad2*r0(num,k,l))
                  else
                     flux = 0.5*qyv*((2*iad1-iad2)*r0(num,k,l)+ iad2*r0(nm ,k,l))
                  endif
               endif
               if (kcs(nm).ne.0) fluxy(nm,k,l) = fluxy(nm,k,l) + flux
  30        continue
  40     continue
  50  continue

!     Horizontal diffusion in implicit-direction

      if ( icreep .eq. 0 .or. kmax .eq. 1 ) then
         do 70 k = 1,kmax
            nmu = icx
            do 60 nm = 1,nmmax
               nmu = nmu + 1
               if ( kfu(nm)*kadu(nm,k) .ne. 0 ) then
                  flux1 = difx(nm,k)*areau(nm,k) / (0.7*gvu(nm))
                  do l=1,nosys
                     flux2 = flux1 * ( r1(nm,k,l) - r1(nmu,k,l) )
                     if (kcs(nm).ne.0) fluxx(nm,k,l) = fluxx(nm,k,l) + flux2
                  enddo
               endif
  60        continue
  70     continue
      else
         fluxx = fluxx + dfluxx
      endif

!     Horizontal diffusion in explicit-direction

      if ( icreep .eq. 0 .or. kmax .eq. 1 ) then
         do 90 k = 1,kmax
            num = icy
            do 80 nm = 1,nmmax
               num = num + 1
               if ( kfv(nm)*kadv(nm,k) .ne. 0 ) then
                  flux1 = dify(nm,k)*areav(nm,k) / (0.7*guv(nm))
                  do l=1,nosys
                     flux2 = flux1 * ( r0(nm,k,l) - r0(num,k,l) )
                     if (kcs(nm).ne.0) fluxy(nm,k,l) = fluxy(nm,k,l) + flux2
                  enddo
               endif
  80        continue
  90     continue
      else
         fluxy = fluxy + dfluxy
      endif

!     Advection in vertical-direction

      do 110 k = 1,kmax-1
         if ( intsrt .eq. 19 ) then           ! central differences  (difu default)
            if ( k.eq.1 .or. k.eq.kmax-1 ) then
               kfw = 1
            else
               kfw = 0
            endif
         else                                 ! upwind differences   (waq scheme 20)
            kfw = 1
         endif

         do 100 nm=1,nmmax
            if ( kfs(nm) .eq. 1 ) then
               qzw = qzk(nm,k)
               if ( qzw .gt. 0.0 ) then
                  adza = 0.5*qzw*(1-kfw)
                  adzc = 0.5*qzw*(1+kfw)
               else
                  adza = 0.5*qzw*(1+kfw)
                  adzc = 0.5*qzw*(1-kfw)
               endif

!     Watch out: velocities in the vertical have opposite sign in FLOW compared to Delwaq

               do l=1,nosys
                  fluxz(nm,k,l) = fluxz(nm,k,l) - ( adzc * r1(nm,k+1,l) + adza * r1(nm,k,l) )
               enddo
            endif
 100     continue
 110  continue

!     Diffusion in vertical direction

      do 130 k = 1,kmax-1
         tsg = 0.5 * (thick(k) + thick(k+1))
         do 120 nm=1,nmmax
            if ( kfs(nm) .eq. 1 ) then
               h0  = max(0.1, s0(nm) + dps(nm) )
               h0i = 1.0 / h0
!              flux1 = difz(nm,k) * h0i / tsg
               do l=1,nosys
                  diz1 = vicmol/sigmol(l) + dicww(nm, k)/sigdif(l)
                  flux1 = gsqs(nm) * diz1 * h0i / tsg
                  flux2 = flux1 * ( r1(nm,k,l) - r1(nm,k+1,l) )
                  fluxz(nm,k,l)   = fluxz(nm,k,l)   + flux2
               enddo
            endif
 120     continue
 130  continue

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlflux
