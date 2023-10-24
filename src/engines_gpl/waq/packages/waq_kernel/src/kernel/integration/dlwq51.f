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
      module m_dlwq51

      implicit none

      contains


      subroutine dlwq51 ( nosys  , notot  , noseg  , noq1   , noq2   ,
     &                    noq3   , noq    , nodisp , novelo , disp   ,
     &                    disper , velo   , volume , area   , flow   ,
     &                    aleng  , ipoint , iknmrk , idpnt  , ivpnt  ,
     &                    conc   , conc2  , bound  , iopt   , ilflag ,
     &                    idt    , iaflag , amass2 , ndmpq  , iqdmp  ,
     &                    dmpq   )

!     Deltares Software Centre

!>\file
!>       Performs flux correction in the direction of flow according to Boris and Book.
!>
!>       This routine makes for the nosys transported substaces the flux correction term after
!>       the upwind advection step is set by dlwq50/18 producing CONC2(notot,noseg) array.\n
!>       The numerical diffusion of the upwind method amounts to 0.5*v*dx - 0.5*v*v*dt. The
!>       numerical diffusion is subtracted from the prescribed diffusion. The resulting flux
!>       correction term, if negative, is applied as negative diffusion. This negative diffusion
!>       term is limited to the value that new maxima and minima would result.\n
!>       the criterion against new maxima and minima is measured compared to the old level
!>       concentration field in CONC(notot,noseg).\n
!>       This method of Boris and Book looks in the flow direction only to limit the flux
!>       correction term. Salezac's method looks around a cell, but that has disadvantages in
!>       stratified systems.\n
!>       The application is subject to the following switches:
!>       - 1) no dispersion accross open boundaries (bit 1 of iopt is 1)
!>       - 2) first order processing accross open boundaries (bit 2 of iopt is 1)
!>       Because the routine corrects the CONC2 array, the flux correction (as mass/timestep)
!>       is divided by the new volume of the cell to get concentratiosn again.\n
!>       This routine also accumulates on the fly the mass balance information for the whole area in
!>       the AMASS2(notot,5) array. This array is printed as header for every time step in the monitoring
!>       file.\n
!>       Furthermore the fluxes in and out of monitoring areas for detail balances are accumulated on
!>       the fly. Which flux needs to be accumulated in what balance is given in the IQDMP(noq) array.


!     Files               : none

!     Routines            : none

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosys                !< number of transported substances
      integer  ( 4), intent(in   ) :: notot                !< total number of substances
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      integer  ( 4), intent(in   ) :: noq1                 !< number of interfaces in direction 1
      integer  ( 4), intent(in   ) :: noq2                 !< number of interfaces in direction 2
      integer  ( 4), intent(in   ) :: noq3                 !< number of interfaces in direction 3
      integer  ( 4), intent(in   ) :: noq                  !< total number of interfaces
      integer  ( 4), intent(in   ) :: nodisp               !< number additional dispersions
      integer  ( 4), intent(in   ) :: novelo               !< number additional velocities
      real     ( 4), intent(in   ) :: disp  (3)            !< fixed dispersions in the 3 directions
      real     ( 4), intent(in   ) :: disper(nodisp,noq)   !< array with additional dispersions
      real     ( 4), intent(in   ) :: velo  (novelo,noq)   !< array with additional velocities
      real     ( 4), intent(in   ) :: volume(noseg)        !< volumes at end of time step
      real     ( 4), intent(in   ) :: area  (noq)          !< exchange areas in m2
      real     ( 4), intent(in   ) :: flow  (noq)          !< flows through the exchange areas in m3/s
      real     ( 4), intent(in   ) :: aleng (  2   ,noq)   !< mixing length to and from the exchange area
      integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
      integer  ( 4), intent(in   ) :: iknmrk(noseg)        !< feature array
      integer  ( 4), intent(in   ) :: idpnt (nosys)        !< additional dispersion number per substance
      integer  ( 4), intent(in   ) :: ivpnt (nosys)        !< additional velocity number per substance
      real     ( 4), intent(in   ) :: conc  (notot,noseg)  !< concentrations at previous time level
      real     ( 4), intent(inout) :: conc2 (notot,noseg)  !< first estimate to be flux corrected
      real     ( 4), intent(in   ) :: bound (nosys,  *  )  !< open boundary concentrations
      integer  ( 4), intent(in   ) :: iopt                 !< bit 0: 1 if no dispersion at zero flow
                                                           !< bit 1: 1 if no dispersion across boundaries
                                                           !< bit 2: 1 if lower order across boundaries
                                                           !< bit 3: 1 if mass balance output
      integer  ( 4), intent(in   ) :: ilflag               !< if 0 then only 3 constant lenght values
      integer  ( 4), intent(in   ) :: idt                  !< time step in seconds
      integer  ( 4), intent(in   ) :: iaflag               !< if 1 then accumulate mass in report array
      real     ( 4), intent(inout) :: amass2(notot, 5   )  !< report array for monitoring file
      integer  ( 4), intent(in   ) :: ndmpq                !< number of dumped exchanges for mass balances
      integer  ( 4), intent(in   ) :: iqdmp (noq)          !< pointer from echange to dump location
      real     ( 4), intent(inout) :: dmpq  (nosys,ndmpq,2)!< array with mass balance information

!     Local variables     :

      integer  ( 4) iq              ! loop counter exchanges
      integer  ( 4) isys            ! loop counter substance
      integer  ( 4) noq12           ! number of horizontal exchanges
      integer  ( 4) ifrom  , ito    ! from   and to   volume numbers
      real     ( 4) vfrom  , vto    ! from   and to   volumes
      integer  ( 4) ifrom_1, ito_1  ! from-1 and to+1 volume numbers
      real     ( 4) cfrm_1 , cto_1  ! from-1 and to+1 concentration values
      real     ( 4) e1, e2, e3      ! limiter help variable
      real     ( 4) s               ! limiter sign variable
      real     ( 4) a               ! this area
      real     ( 4) q               ! flow for this exchange
      real     ( 4) e               ! dispersion for this exchange
      real     ( 4) al              ! this length
      real     ( 4) dl              ! area / length
      real     ( 4) d               ! dispersion for this substance
      real     ( 4) v               ! flow for this substance
      real     ( 4) dq              ! total flux from and to
      integer  ( 4) ipb             ! pointer in the mass balance dump array
      real     ( 4) f1 , f2         ! correction factors central differences
      real     ( 4) dqtr, dqtot     ! balances help variables

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq51", ithandl )

!         loop accross the number of exchanges

      noq12 = noq1 + noq2
      do 60 iq = 1, noq

!         initialisations , check for transport anyhow

         ifrom   = ipoint(1,iq)
         ito     = ipoint(2,iq)
         ifrom_1 = ipoint(3,iq)
         ito_1   = ipoint(4,iq)
         if ( ifrom   .eq. 0 .or. ito   .eq. 0 ) cycle
         if ( ifrom .le. 0 .and. ito .le. 0 ) cycle

         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle   ! identified dry at start and end of timestep
         endif
         if ( ito   .gt. 0 ) then
            if ( .not. btest(iknmrk(ito  ),0) ) cycle
         endif

         a = area(iq)
         q = flow(iq)
         if ( abs(q) .lt. 10.0e-25 .and. iq .le. noq12 .and. btest(iopt,0) )  cycle
                                                           ! thin dam option, no dispersion at zero flow
!     Check if exchange is dump exchange, set IPB

         ipb = 0
         if ( btest(iopt,3) ) then
            if ( iqdmp(iq) .gt. 0 ) ipb = iqdmp(iq)
         endif

!         initialize uniform values

         if ( iq .le. noq1      ) then
            e  = disp (1)
            al = aleng(1,1)
         elseif ( iq .le. noq12 ) then
            e  = disp (2)
            al = aleng(2,1)
         else
            e  = disp (3)
            al = aleng(1,2)
         endif
         if ( iq .gt. noq12+noq3 ) e  = 0.0     ! in the bed

         if ( ilflag .eq. 1 ) al = aleng(1,iq) + aleng(2,iq)
         if ( al .lt. 1.0e-25) cycle
         if ( ilflag .eq. 1 ) then
            f1 = aleng(1,iq) / al
         else
            f1 = 0.5
         endif
         dl = a / al
         e  = e*dl                              ! in m3/s
!
         if ( ifrom .lt. 0 ) goto 20
         if ( ito   .lt. 0 ) goto 40

!         The correction step

         vfrom   = volume(ifrom)
         vto     = volume( ito )
         do isys = 1 , nosys
            v  = q
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            f2 = f1
            if ( v .lt. 0.0 ) f2 = f2 - 1.0
            d  = -f2*v + e + 0.5*v*v*idt/a/al
            d  = min( d, e )
            if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            dq = ( conc(isys,ifrom) - conc(isys,ito) )*d*idt
            if ( d .lt. 0.0 ) then
               e2 = dq
               s  = sign ( 1.0 , e2 )
               select case ( ifrom_1 )
                  case ( 1: )
                     cfrm_1 = conc2(isys,ifrom_1)
                  case (  0 )
                     if ( s .gt. 0 ) then
                        cfrm_1 = 0.0
                     else
                        cfrm_1 = 2.0*conc2(isys,ifrom)
                     endif
                  case ( :-1 )
                     cfrm_1 = bound(isys,-ifrom_1)
               end select
               select case ( ito_1 )
                  case ( 1: )
                     cto_1 = conc2(isys,ito_1)
                  case (  0 )
                     if ( s .gt. 0 ) then
                        cto_1 = 2.0*conc2(isys,ito)
                     else
                        cto_1 = 0.0
                     endif
                  case ( :-1 )
                     cto_1 = bound(isys,-ito_1)
               end select

               e1 = ( conc2(isys,ifrom) - cfrm_1 ) * vfrom
               e3 = ( cto_1  - conc2(isys,ito  ) ) * vto
               dq = s * max( 0.0 , min( s*e1 , s*e2 , s*e3 ) )
            endif

            conc2(isys,ifrom) = conc2(isys,ifrom) - dq/vfrom
            conc2(isys,ito  ) = conc2(isys,ito  ) + dq/vto

            if ( ipb .gt. 0 ) then                          ! recalculate
               if ( v .gt. 0.0 ) then                       ! transport
                  dqtr = v * conc(isys,ifrom) * idt         ! for the mass
               else                                         ! balance
                  dqtr = v * conc(isys,ito  ) * idt
               endif
               dqtot = dq + dqtr
               if ( dqtot .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dqtot
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dqtot
               endif
            endif
         enddo
         cycle

!        The 'from' element was a boundary. Note the 2 options.

   20    vto     = volume( ito )
         do isys = 1 , nosys
            v  = q
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            d = 0.0
            if ( .not. btest(iopt,1) ) then
               d = e
               if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( .not. btest(iopt,2) ) then
               f2 = f1
               if ( v .lt. 0.0 ) f2 = f2 - 1.0
               d = d -f2*v + 0.5*v*v*idt/a/al
               d = min( d, e )
            endif
            dq = d * ( bound(isys,-ifrom) - conc(isys,ito) )
            conc2(isys,ito) = conc2(isys,ito)  + dq*idt/vto

            if ( iaflag .eq. 1 ) then
               if ( dq .gt. 0 ) then
                  amass2(isys,4) = amass2(isys,4) + dq*idt
               else
                  amass2(isys,5) = amass2(isys,5) - dq*idt
               endif
            endif

            if ( ipb .gt. 0 ) then                          ! recalculate
               if ( v .gt. 0.0 ) then                       ! transport
                  dqtr = v * bound(isys,-ifrom)             ! for the mass
               else                                         ! balance
                  dqtr = v * conc (isys, ito  )
               endif
               dqtot = dq + dqtr
               if ( dqtot .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dqtot * idt
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dqtot * idt
               endif
            endif
         enddo
         cycle

!        The 'to' element was a boundary.

   40    vfrom   = volume(ifrom)
         do isys = 1 , nosys
            v  = q
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            d = 0.0
            if ( .not. btest(iopt,1) ) then
               d = e
               if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( .not. btest(iopt,2) ) then
               f2 = f1
               if ( v .lt. 0 ) f2 = f2 - 1.0
               d = -f2*v + d + 0.5*v*v*idt/a/al
               d = min( d, e )
            endif
            dq = d * ( conc(isys,ifrom) - bound(isys,-ito) )
            conc2(isys,ifrom) = conc2(isys,ifrom)  - dq*idt/vfrom

            if ( iaflag .eq. 1 ) then
               if ( dq .gt. 0 ) then
                  amass2(isys,5) = amass2(isys,5) + dq*idt
               else
                  amass2(isys,4) = amass2(isys,4) - dq*idt
               endif
            endif

            if ( ipb .gt. 0 ) then                          ! recalculate
               if ( v .gt. 0.0 ) then                       ! transport
                  dqtr = v * conc (isys, ifrom)             ! for the mass
               else                                         ! balance
                  dqtr = v * bound(isys,-ito  )
               endif
               dqtot = dq + dqtr
               if ( dqtot .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dqtot * idt
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dqtot * idt
               endif
            endif
         enddo

!       end of the loop over exchanges

   60 continue

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq51
