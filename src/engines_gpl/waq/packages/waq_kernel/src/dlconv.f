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
      module m_dlconv

      implicit none

      contains


      subroutine dlconv ( noseg   , noq1    , noq2    , noq3    , noq     ,
     &                    nobnd   , nosys   , notot   , intsrt  , intopt  ,
     &                    ilflag  , novelo  , nodisp  , ivpnt   , idpnt   ,
     &                    dvol0   , dvol1   , area    , flow    , aleng   ,
     &                    velo    , disp    , disper  , conc    , bound   ,
     &                    deriv   , cellpnt , flowpnt , nmax    , mmax    ,
     &                    kmax    , lgrida  , gsqs    , guv     , gvu     ,
     &                    thick   , vol0    , vol1    , dps     , s1      ,
     &                    ddkl    , r1      , qxk     , qyk     , qzk     ,
     &                    difx    , dify    , difz    , dicuv   , dicww   ,
     &                    areau   , areav   , aakl    , bbkl    , cckl    ,
     &                    kcs     , kfu     , kfv     , kfs     )

!     Deltares Software Centre

!>\File
!>           Converts all time dependent items from Delwaq to FLOW arrays
!>
!>           The constant FLOW arrays have been initialized in flwinit. There also
!>           an extensive documentation of the FLOW administration can be found.\n
!>           Notes:
!>           - the horizantal additional velocities and diffusions are incorporated
!>             in the explicit processes term ddkl because there is no substance depending
!>             horizontal diffusion term in FLOW
!>           - The vertical additional velocities and diffusions are used to initialize
!>             the aakl, bbkl and cckl arrays. They are used for implicit vertical transport
!>             and are the only FLOW arrays suitable because they have an entry per substance.
!>           - S0 + dps is depth at old time level. S0 is set to zero, so dps contains depth.
!>           - S1 + dps is depth at new time level so S1 should contain change in depth.

!     Created             : August 1996   by Erik de Goede

!     Modified            : August 2011   by Leo Postma, also fit for 'active only' flow fields

!     Subroutines called  : none

!     Functions called    : none

!     File access         : none

      use timers          ! WAQ performance timers

      implicit none

!     kind           function         name                              description

      integer  ( 4), intent(in   ) :: noseg                           !< number of Delwaq computational volumes
      integer  ( 4), intent(in   ) :: noq1                            !< number of Delwaq exchanges in first direction
      integer  ( 4), intent(in   ) :: noq2                            !< number of Delwaq exchanges in second direction
      integer  ( 4), intent(in   ) :: noq3                            !< number of Delwaq exchanges in third direction
      integer  ( 4), intent(in   ) :: noq                             !< total number of Delwaq exchanges
      integer  ( 4), intent(in   ) :: nobnd                           !< number of Delwaq open boundary cells
      integer  ( 4), intent(in   ) :: nosys                           !< number of Delwaq transportable substances
      integer  ( 4), intent(in   ) :: notot                           !< total number of Delwaq substances
      integer  ( 4), intent(in   ) :: intsrt                          !< integration number (19 or 20 for this routine)
      integer  ( 4), intent(in   ) :: intopt                          !< integration sub-option
      integer  ( 4), intent(in   ) :: ilflag                          !< if 1 spatially varying diffusion lengthes
      integer  ( 4), intent(in   ) :: novelo                          !< nr of additional (eg. settling) velocities
      integer  ( 4), intent(in   ) :: nodisp                          !< nr of space varying diffusions
      integer  ( 4), intent(in   ) :: ivpnt  (nosys)                  !< additional velocity number per substance
      integer  ( 4), intent(in   ) :: idpnt  (nosys)                  !< space varying diffusion number per substance
      real     ( 4), intent(in   ) :: dvol0  (noseg)                  !< Delwaq volumes at start of time step
      real     ( 4), intent(in   ) :: dvol1  (noseg)                  !< Delwaq volumes at end of time step
      real     ( 4), intent(in   ) :: area   (noq)                    !< Delwaq exchange surfaces
      real     ( 4), intent(in   ) :: flow   (noq)                    !< Delwaq fluxes accross those surfaces
      real     ( 4), intent(in   ) :: aleng  (2,noq)                  !< Delwaq from-to diffusion lengthes perpendicular to the surfaces
      real     ( 4), intent(in   ) :: velo   (novelo,noq)             !< Delwaq additional velocity values
      real     ( 4), intent(in   ) :: disp   (3)                      !< Delwaq constant diffusions in 3 directions
      real     ( 4), intent(in   ) :: disper (nodisp,noq)             !< Delwaq space varying diffusion terms
      real     ( 4), intent(in   ) :: conc   (notot,noseg)            !< Delwaq concentration values
      real     ( 4), intent(in   ) :: bound  (nosys,nobnd)            !< Delwaq open boundary concentration values
      real     ( 4), intent(in   ) :: deriv  (notot,noseg)            !< Delwaq derivative (processes, dischanges etc.)
      integer  ( 4), intent(in   ) :: cellpnt(noseg)                  !< Backpointer from noseg to mnmaxk
      integer  ( 4), intent(in   ) :: flowpnt(noq)                    !< Backpointer from noq to 3*mnmaxk - mnmax
!  ===========> here the Delft3D-FLOW side starts <=============
      integer  ( 4), intent(in   ) :: nmax                            !< 1st FLOW array index (y-coordinate,v-velocity,Eta-direction)
      integer  ( 4), intent(in   ) :: mmax                            !< 2nd FLOW array index (x-coordinate,u-velocity,Ksi-direction)
      integer  ( 4), intent(in   ) :: kmax                            !< 3rd FLOW array index (z-coordinate,w-velocity,Sigma-direction)
      integer  ( 4), intent(in   ) :: lgrida (nmax,   mmax)           !< Horizontal active grid pointer
      real     ( 4), intent(in   ) :: gsqs   (nmax,-1:mmax+2)         !< Horizontal surface area
      real     ( 4), intent(in   ) :: guv    (nmax,-1:mmax+2)         !< diffusion length through v points
      real     ( 4), intent(in   ) :: gvu    (nmax,-1:mmax+2)         !< diffusion length through u points
      real     ( 4), intent(in   ) :: thick  (kmax)                   !< relative layer thickness
      real     ( 4), intent(  out) :: vol0   (nmax,-1:mmax+2,  kmax)  !< FLOW volumes at old time level
      real     ( 4), intent(  out) :: vol1   (nmax,-1:mmax+2,  kmax)  !< FLOW volumes at new time level
      real     ( 4), intent(  out) :: dps    (nmax,-1:mmax+2)         !< depth from surface to bed of column at old time level
      real     ( 4), intent(  out) :: s1     (nmax,-1:mmax+2)         !< change in depth from old to new time level
      real     ( 4), intent(  out) :: ddkl   (nmax,-1:mmax+2,  kmax, nosys)   !< derivative per substance
      real     ( 4), intent(  out) :: r1     (nmax,-1:mmax+2,  kmax, nosys)   !< concentration per substance, old time level
      real     ( 4), intent(  out) :: qxk    (nmax,-1:mmax+2,  kmax)  !< FLOW flows in the second Delwaq direction
      real     ( 4), intent(  out) :: qyk    (nmax,-1:mmax+2,  kmax)  !< FLOW flows in the first Delwaq direction
      real     ( 4), intent(  out) :: qzk    (nmax,-1:mmax+2,0:kmax)  !< Flow flows in the third Delwaq direction but upwards
      real     ( 4), intent(  out) :: difx   (nmax,-1:mmax+2,  kmax)  !< FLOW diffusion in m2/s * 0.7 in second Delwaq direction
      real     ( 4), intent(  out) :: dify   (nmax,-1:mmax+2,  kmax)  !< FLOW diffusion in m2/s * 0.7 in first Delwaq direction
      real     ( 4), intent(  out) :: difz   (nmax,-1:mmax+2,0:kmax)  !< FLOW diffusion in m4/s in third Delwaq direction <==!?
      real     ( 4), intent(  out) :: dicuv  (nmax,-1:mmax+2,  kmax)  !< strange array =(disp(1)+disp(2))/2*0.7
      real     ( 4), intent(  out) :: dicww  (nmax,-1:mmax+2,0:kmax)  !< strange array = disp(3)
      real     ( 4), intent(  out) :: areau  (nmax,-1:mmax+2,  kmax)  !< exchange surface in u-points
      real     ( 4), intent(  out) :: areav  (nmax,-1:mmax+2,  kmax)  !< exchange surface in v-points
      real     ( 4), intent(  out) :: aakl   (nmax,-1:mmax+2,  kmax, nosys)   !< numerical array 3rd direction advection diffusion
      real     ( 4), intent(  out) :: bbkl   (nmax,-1:mmax+2,  kmax, nosys)   !< numerical array 3rd direction advection diffusion
      real     ( 4), intent(  out) :: cckl   (nmax,-1:mmax+2,  kmax, nosys)   !< numerical array 3rd direction advection diffusion
      integer  ( 4), intent(in   ) :: kcs    (nmax,-1:mmax+2)         !< constant property of grid cells
      integer  ( 4), intent(  out) :: kfu    (nmax,-1:mmax+2)         !< time varying property in u points
      integer  ( 4), intent(  out) :: kfv    (nmax,-1:mmax+2)         !< time varying property in v points
      integer  ( 4), intent(  out) :: kfs    (nmax,-1:mmax+2)         !< time varying property of the grid cell

!        Locals

      integer(4) nobndl            ! number of open boundaries per layer
      integer(4) mnmax             ! size of one layer = nmax*mmax
      integer(4) iq                ! loop counter exchanges
      integer(4) iseg              ! loop counter computational volumes
      integer(4) isys              ! loop counter transported substances
      integer(4) n, m, k           ! 3 matrix indices
      real   (4) surf              ! help variable horizontal surface area
      integer(4) nup, mup          ! help variables n+1, m+1 limited to nmax, mmax
      integer(4) j, jup            ! help variables lgrid(n,m) and lgrid for nup, mup
      real   (4) addvel, adddis    ! help variables for additional velocities and diffusions
      real   (4) adza  , adzc      ! help variables for additional velocities and diffusions
      integer(4) iv, id            ! help variable index addition velocities, diffusions
      real   (4) flux              ! help variable combined additional fluxes
      real   (4) sumx, sumy        ! help variables for summation
      integer(4) ndo, mdo          ! downward equivalent of nup, mup
      integer(4) kfw               ! help variable for forward differencing or not

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlconv", ithandl )

      nobndl = nobnd/kmax
      mnmax  = nmax*mmax

!       volumes, depths, concentrations and derivatives

      dps  = 0.0
      s1   = 0.0
      vol0 = 0.0
      vol1 = 0.0
      ddkl = 0.0
      r1   = 0.0
      do iseg = 1, noseg
         n = mod( cellpnt(iseg)-1, nmax  )         + 1
         m = mod( cellpnt(iseg)-1, mnmax ) / nmax  + 1
         k =    ( cellpnt(iseg)-1 )        / mnmax + 1
         j = lgrida(n,m)
         if ( j .ne. 0 ) then
            vol0( n, m, k ) = dvol0(iseg)
            vol1( n, m, k ) = dvol1(iseg)
            surf = gsqs( n, m )
            if ( abs(surf) .gt. 1.0e-20 ) then
               dps( n, m ) = dps( n, m ) + dvol0(iseg)/surf
               s1 ( n, m ) = s1 ( n, m ) + dvol1(iseg)/surf
            endif
            do isys = 1 , nosys
               ddkl(n,m,k,isys) = deriv(isys,iseg)
               r1  (n,m,k,isys) = conc (isys,iseg)
            enddo
         endif
      enddo
      do m = 1, mmax
         do n = 1, nmax
            j   = lgrida(n,m)
            if ( j .gt. 0 ) s1( n, m ) = s1( n, m ) - dps( n, m )
            if ( j .lt. 0 ) then
               do k = 1, kmax
                  do isys = 1 , nosys
                     r1(n,m,k,isys) = bound( isys, (k-1)*nobndl - j )
                  enddo
               enddo
            endif
         enddo
      enddo

!       area, flows

      qxk   = 0.0
      qyk   = 0.0
      qzk   = 0.0
      difx  = 0.0
      dify  = 0.0
      difz  = 0.0
      dicuv = 0.0
      dicww = 0.0
      do iq = 1, noq1
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         qyk  (n,m,k) =  flow(iq)
         j   = lgrida( n  , m )
         if ( j .ne. 0 ) then
            areav(n,m,k) =  area(iq)
            dify (n,m,k) =  disp(1)*0.7
            dicuv(n,m,k) = (disp(1)+disp(2))/2.0 * 0.7
            dicww(n,m,k) =  disp(3)
         endif
         if ( btest(intopt,0) .and. abs(flow(iq)).lt.1e-10 ) dify (n,m,k) = 0.0
         if ( n .eq. nmax ) cycle
         nup = n+1
         jup = lgrida( nup, m )
         if ( j .eq. 0 .or. jup .eq. 0 ) cycle
         if ( btest(intopt,1) ) then
            if ( j .lt. 0 .or. jup .lt. 0 ) dify (n,m,k) = 0.0
         endif
         do isys = 1 , nosys
            if ( ivpnt(isys) + idpnt(isys) .eq. 0 ) cycle
            addvel = 0.0
            adddis = 0.0
            iv  = ivpnt(isys)
            if ( iv .gt. 0 ) addvel = 0.5 * velo(iv,iq)*area(iq)
            id  = idpnt(isys)
            if ( id .gt. 0 ) then
               if ( btest(intopt,1) ) then
                  if ( j .lt. 0 .or. jup .lt. 0 ) then
                     adddis = 0.
                  else
                     adddis = disper(id,iq)*area(iq)/guv(n,m)
                  endif
               endif
            endif
            flux = (addvel+adddis) * r1(n  ,m,k,isys) +
     &             (addvel-adddis) * r1(nup,m,k,isys)

            if ( j   .gt. 0 ) ddkl(n  ,m,k,isys) = ddkl(n  ,m,k,isys) - flux
            if ( jup .gt. 0 ) ddkl(nup,m,k,isys) = ddkl(nup,m,k,isys) + flux
         enddo
      enddo
      do iq = noq1 + 1, noq1+noq2
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         qxk  (n,m,k) =  flow(iq)
         j   = lgrida( n, m   )
         if ( j .ne. 0 ) then
            difx (n,m,k) =  disp(2)*0.7
            areau(n,m,k) =  area(iq)
            dicuv(n,m,k) = (disp(1)+disp(2))/2.0 * 0.7
            dicww(n,m,k) =  disp(3)
         endif
         if ( btest(intopt,0) .and. abs(flow(iq)).lt.1e-10 ) difx(n,m,k) = 0.0
         if ( m .eq. mmax ) cycle
         mup = m+1
         jup = lgrida( n, mup )
         if ( j .eq. 0 .or. jup .eq. 0 ) cycle
         if ( btest(intopt,1) ) then
            if ( j .lt. 0 .or. jup .lt. 0 ) difx (n,m,k) = 0.0
         endif
         do isys = 1 , nosys
            if ( ivpnt(isys) + idpnt(isys) .eq. 0 ) cycle
            addvel = 0.0
            adddis = 0.0
            iv  = ivpnt(isys)
            if ( iv .gt. 0 ) addvel = 0.5 * velo(iv,iq)*area(iq)
            id  = idpnt(isys)
            if ( id .gt. 0 ) then
               if ( btest(intopt,1) ) then
                  if ( j .lt. 0 .or. jup .lt. 0 ) then
                     adddis = 0.
                  else
                     adddis = disper(id,iq)*area(iq)/gvu(n,m)
                  endif
               endif
            endif
            flux = (addvel+adddis) * r1(n,m  ,k,isys) +
     &             (addvel-adddis) * r1(n,mup,k,isys)
            if ( j   .gt. 0 ) ddkl(n,m  ,k,isys) = ddkl(n,m  ,k,isys) - flux
            if ( jup .gt. 0 ) ddkl(n,mup,k,isys) = ddkl(n,mup,k,isys) + flux
         enddo
      enddo
      aakl = 0.0
      bbkl = 0.0
      cckl = 0.0
      do iq = noq1 + noq2 + 1, noq
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         qzk  (n,m,k) = -flow(iq)
         difz (n,m,k) =  disp(3) * area(iq)
         j   = lgrida( n, m )
         if ( j .gt. 0 ) then
            do isys = 1 , nosys
               addvel = 0.0
               iv  = ivpnt(isys)
               if ( iv .gt. 0 ) addvel = -0.5 * velo(iv,iq)*area(iq)
               if ( intsrt .eq. 19 ) then
                  if ( k .eq. 1 .or. k .eq. kmax-1 ) then
                    kfw = 1
                  else
                    kfw = 0
                  endif
               else
                  kfw = 1
               endif
               if ( addvel .gt. 0.0 ) then
                 adza = addvel * ( 1 - kfw )
                 adzc = addvel * ( 1 + kfw )
               else
                 adza = addvel * ( 1 + kfw )
                 adzc = addvel * ( 1 - kfw )
               endif
               adddis = 0.0
               id  = idpnt(isys)
               if ( id .gt. 0 ) adddis = disper(id,iq)*area(iq) / max( 0.1 , dps(n,m)*thick(k) )
               aakl(n,m,k+1,isys) = aakl(n,m,k+1,isys) + adza - adddis
               bbkl(n,m,k+1,isys) = bbkl(n,m,k+1,isys) + adzc + adddis
               bbkl(n,m,k  ,isys) = bbkl(n,m,k  ,isys) - adza + adddis
               cckl(n,m,k  ,isys) = cckl(n,m,k  ,isys) - adzc - adddis
            enddo
         endif
      enddo

!       variable property arrays at exchanges

      do m = 1, mmax
         do n = 1, nmax
            sumx = abs( qxk(n,m,1) ) + difx(n,m,1)
            sumy = abs( qyk(n,m,1) ) + dify(n,m,1)
            do k = 2, kmax
               sumx = sumx + abs( qxk(n,m,k) ) + difx(n,m,k)
               sumy = sumy + abs( qyk(n,m,k) ) + dify(n,m,k)
            enddo
            kfu(n,m) = 0
            if ( sumx                 .gt. 1e-10 .and.
     &           kcs(n,    m)         .gt. 0     .and.
     &           kcs(n,min(m+1,mmax)) .gt. 0           ) kfu(n,m) = 1
            kfv(n,m) = 0
            if ( sumy                 .gt. 1e-10 .and.
     &           kcs(n,    m)         .gt. 0     .and.
     &           kcs(min(n+1,nmax),m) .gt. 0           ) kfv(n,m)=1
         enddo
      enddo

!       variable property array of the cell

      do m = 1, mmax
         do n = 1, nmax
            j = lgrida( n, m )
            kfs(n,m) = 0
            mdo = max(1,m-1)
            ndo = max(1,n-1)
            if ( j .ne. 0 ) then
               kfs(n,m) = max (kfu(n,m),kfu(n,mdo),kfv(n,m),kfv(ndo,m))
            endif
         enddo
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlconv
