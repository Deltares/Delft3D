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
      module m_dlmasb

      implicit none

      contains


      subroutine dlmasb ( dt     , nmax   , mmax   , kmax   , noq1   ,
     &                    noq2   , noq3   , noq    , nosys  , notot  ,
     &                    lgrida , flowpnt, fluxx  , fluxy  , fluxz  ,
     &                    area   , guv    , gvu    , dps    , thick  ,
     &                    r0     , r1     , novelo , ivpnt  , velo   ,
     &                    nodisp , idpnt  , disper , iaflag , amass2 ,
     &                    ndmpq  , iqdmp  , dmpq   )

!     Deltares Software Centre

!>\File
!>          Updates the mass balance arrays for methods 19 and 20
!>
!>          The flux information (advection and diffusion) is already accumulated
!>          in the fluxx, fluxy and fluxz arrays in mass/second. Here the effect of
!>          additional velocities and dispersions is added, because they cannot be
!>          distinguished per substance in FLOW. There is no optional boundary
!>          treatment possible in difu, so those options are not granted.\n
!>          NOTE: although the vertical flux has opposite sign in FLOW, that effect
!>          is already corrected for in the fluxz array.

!     Created             : August 1996 by Erik de Goede

!     Modified            : August 2011 by Leo Postma     : deal also with "active only" schematisations

!     Routines called     : none

!     File input/output   : none

      use timers
      implicit none

!     Arguments

!     Kind        function         name                                  Description

      real   (4), intent(in   ) :: dt                                  !< time step in seconds
      integer(4), intent(in   ) :: nmax                                !< 1st FLOW array index (y-coordinate,v-velocity,Eta-direction)
      integer(4), intent(in   ) :: mmax                                !< 2nd FLOW array index (x-coordinate,u-velocity,Ksi-direction)
      integer(4), intent(in   ) :: kmax                                !< 3rd FLOW array index (z-coordinate,w-velocity,Sigma-direction)
      integer(4), intent(in   ) :: noq1                                !< number of Delwaq exchanges in first direction
      integer(4), intent(in   ) :: noq2                                !< number of Delwaq exchanges in second direction
      integer(4), intent(in   ) :: noq3                                !< number of Delwaq exchanges in third direction
      integer(4), intent(in   ) :: noq                                 !< total number of Delwaq exchanges
      integer(4), intent(in   ) :: nosys                               !< number of Delwaq transportable substances
      integer(4), intent(in   ) :: notot                               !< total number of Delwaq substances
      integer(4), intent(in   ) :: lgrida (nmax,   mmax)               !< Horizontal active grid pointer
      integer(4), intent(in   ) :: flowpnt(noq)                        !< Backpointer from noq to 3*mnmaxk - mnmax
      real   (4), intent(in   ) :: fluxx  (nmax,-1:mmax+2,kmax,nosys)  !< FLOW flux in m3/s in second Delwaq direction
      real   (4), intent(in   ) :: fluxy  (nmax,-1:mmax+2,kmax,nosys)  !< FLOW flux in m3/s in first Delwaq direction
      real   (4), intent(in   ) :: fluxz  (nmax,-1:mmax+2,kmax,nosys)  !< FLOW flux in m3/s in third Delwaq direction
      real   (4), intent(in   ) :: area   (noq)                        !< Delwaq exchange surfaces
      real   (4), intent(in   ) :: guv    (nmax,-1:mmax+2)             !< diffusion length through v points
      real   (4), intent(in   ) :: gvu    (nmax,-1:mmax+2)             !< diffusion length through u points
      real   (4), intent(in   ) :: thick  (kmax)                       !< relative layer thickness
      real   (4), intent(in   ) :: dps    (nmax,-1:mmax+2)             !< depth from surface to bed of column at old time level
      real   (4), intent(in   ) :: r0     (nmax,-1:mmax+2,kmax,nosys)  !< concentration per substance, old time level
      real   (4), intent(in   ) :: r1     (nmax,-1:mmax+2,kmax,nosys)  !< concentration per substance, new time level
      integer(4), intent(in   ) :: novelo                              !< nr of additional (eg. settling) velocities
      integer(4), intent(in   ) :: ivpnt  (nosys)                      !< additional velocity number per substance
      integer(4), intent(in   ) :: nodisp                              !< nr of space varying diffusions
      integer(4), intent(in   ) :: idpnt  (nosys)                      !< space varying diffusion number per substance
      real   (4), intent(in   ) :: velo   (novelo,noq)                 !< Delwaq additional velocity values
      real   (4), intent(in   ) :: disper (nodisp,noq)                 !< Delwaq space varying diffusion terms
      integer(4), intent(in   ) :: iaflag                              !< If 1, then accumulate the boundaries
      real   (4), intent(inout) :: amass2 (notot ,5)                   !< Delwaq whole area mass balance accumulators
      integer(4), intent(in   ) :: ndmpq                               !< number of dumped exchanges for mass balances
      integer(4), intent(in   ) :: iqdmp  (noq)                        !< pointer from exchange to dump location
      real   (4), intent(inout) :: dmpq   (nosys ,ndmpq,2)             !< array with mass balance information

!     Local declarations

      integer(4)    iq, l            !  loop counters exchanges and substances
      integer(4)    n, m, k          !  three indices in the FLOW cube
      integer(4)    ifrom, ito       !  help variables for from and to cell numbers
      real   (4)    dq               !  help variable for fluxes
      integer(4)    idmp             !  help variable for the number of the dump transect / area
      real   (4)    a, al            !  help variables for area and mixing length
      real   (4)    addvel, adddis   !  help variables for additional velocities and dispersions
      integer(4)    iv, id           !  help variables for the additional velocity / diffusion number
      integer(4)    mnmax            !  help variable nmax*mmax

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlmasb", ithandl )

      mnmax = nmax*mmax

!     first Delwaq direction ( v, y, n FLOW direction )

      do iq = 1, noq1
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         if ( n == nmax .or. m == mmax ) cycle
         ifrom = lgrida(n  ,m)
         ito   = lgrida(n+1,m)                                            ! n
         if ( iaflag .eq. 1 ) then
            if ( ifrom .lt. 0 .and. ito .gt. 0 ) then
               do l = 1, nosys
                  dq = fluxy(n,m,k,l)                                     ! y
                  if ( dq .gt. 0 ) then
                     amass2(l,4) = amass2(l,4) + dq*dt
                  else
                     amass2(l,5) = amass2(l,5) - dq*dt
                  endif
               enddo
            endif
            if ( ifrom .gt. 0 .and. ito .lt. 0 ) then
               do l = 1, nosys
                  dq = fluxy(n,m,k,l)
                  if ( dq .gt. 0 ) then
                     amass2(l,5) = amass2(l,5) + dq*dt
                  else
                     amass2(l,4) = amass2(l,4) - dq*dt
                  endif
               enddo
            endif
         endif
         idmp = iqdmp(iq)
         if ( idmp .gt. 0 ) then
            a  = area(iq)
            al = a / guv(n,m)                                             ! v
            do l = 1, nosys
               addvel = 0.0
               iv  = ivpnt(l)
               if ( iv .gt. 0 ) addvel = 0.5 * velo( iv, iq ) * a
               adddis = 0.0
               id  = idpnt(l)
               if ( id .gt. 0 ) adddis = disper( id, iq ) * al
               dq = fluxy(n,m,k,l) + (addvel+adddis) * r0(n  ,m,k,l) +    ! y
     &                               (addvel-adddis) * r0(n+1,m,k,l)      ! n
               if ( dq .gt. 0.0 ) then
                  dmpq(l,idmp,1) = dmpq(l,idmp,1) + dq*dt
               else
                  dmpq(l,idmp,2) = dmpq(l,idmp,2) - dq*dt
               endif
            enddo
         endif
      enddo

!     second Delwaq direction ( u, x, m FLOW direction )

      do iq = noq1+1, noq1+noq2
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         if ( n == nmax .or. m == mmax ) cycle
         ifrom = lgrida(n,m  )
         ito   = lgrida(n,m+1)                                            ! m
         if ( iaflag .eq. 1 ) then
            if ( ifrom .lt. 0 .and. ito .gt. 0 ) then
               do l = 1, nosys
                  dq = fluxx(n,m,k,l)                                     ! x
                  if ( dq .gt. 0 ) then
                     amass2(l,4) = amass2(l,4) + dq*dt
                  else
                     amass2(l,5) = amass2(l,5) - dq*dt
                  endif
               enddo
            endif
            if ( ifrom .gt. 0 .and. ito .lt. 0 ) then
               do l = 1, nosys
                  dq = fluxy(n,m,k,l)
                  if ( dq .gt. 0 ) then
                     amass2(l,5) = amass2(l,5) + dq*dt
                  else
                     amass2(l,4) = amass2(l,4) - dq*dt
                  endif
               enddo
            endif
         endif
         idmp = iqdmp(iq)
         if ( idmp .gt. 0 ) then
            a  = area(iq)
            al = a / gvu(n,m)                                             ! u
            do l = 1, nosys
               addvel = 0.0
               iv  = ivpnt(l)
               if ( iv .gt. 0 ) addvel = 0.5 * velo( iv, iq ) * a
               adddis = 0.0
               id  = idpnt(l)
               if ( id .gt. 0 ) adddis = disper( id, iq ) * al
               dq = fluxx(n,m,k,l) + (addvel+adddis) * r0(n,m  ,k,l) +    ! x
     &                               (addvel-adddis) * r0(n,m+1,k,l)      ! m
               if ( dq .gt. 0.0 ) then
                  dmpq(l,idmp,1) = dmpq(l,idmp,1) + dq*dt
               else
                  dmpq(l,idmp,2) = dmpq(l,idmp,2) - dq*dt
               endif
            enddo
         endif
      enddo

!     third Delwaq direction ( w, -z, k FLOW direction )

      do iq = noq1+noq2+1, noq1+noq2+noq3
         idmp = iqdmp(iq)
         if ( idmp .eq. 0 ) cycle
         n = mod( flowpnt(iq)-1, nmax  )         + 1
         m = mod( flowpnt(iq)-1, mnmax ) / nmax  + 1
         k =    ( flowpnt(iq)-1 )        / mnmax + 1
         a  = area(iq)
         al = a / max( 0.1, dps(n,m)*thick(k) )                        ! w
         do l = 1, nosys
            addvel = 0.0
            iv  = ivpnt(l)
            if ( iv .gt. 0 ) addvel = 0.5 * velo( iv, iq ) * a
            adddis = 0.0
            id  = idpnt(l)
            if ( id .gt. 0 ) adddis = disper( id, iq ) * al
            dq = fluxz(n,m,k,l) + (addvel+adddis) * r1(n,m,k  ,l) +    ! z
     &                            (addvel-adddis) * r1(n,m,k+1,l)      ! k
            if ( dq .gt. 0.0 ) then
               dmpq(l,idmp,1) = dmpq(l,idmp,1) + dq*dt
            else
               dmpq(l,idmp,2) = dmpq(l,idmp,2) - dq*dt
            endif
         enddo
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlmasb
