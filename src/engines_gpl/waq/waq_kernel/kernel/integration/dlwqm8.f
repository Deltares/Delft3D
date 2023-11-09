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
      module m_dlwqm8

      implicit none

      contains


      subroutine dlwqm8( idt    , isys   , nosys  , notot  , noseg  ,
     &                   conc   , concvt , volnew , nobnd  , bound  ,
     &                   noq    , iknmrk , ipoint , area   , aleng  ,
     &                   theta  , flowtot, iopt   , amass2 , ndmpq  ,
     &                   iqdmp  , dmpq   )

!     Deltares Software Centre

!>/File
!>                performs flux correction according to Boris and Book

!     Created   : June  2007 by Pauline van Slingerland

!     Modified  : July  2009 by Leo Postma : double precission version
!                 May   2010 by Leo Postma : Boris and Book in stead of Zalezac

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                    Description

      integer(4), intent(in   ) :: idt                    !< time step in scu's
      integer(4), intent(in   ) :: isys                   !< current active substance
      integer(4), intent(in   ) :: nosys                  !< number of active substances
      integer(4), intent(in   ) :: notot                  !< total number of substances
      integer(4), intent(in   ) :: noseg                  !< number of segments
      real   (4), intent(inout) :: conc   (notot,noseg)   !< concentrations
      real   (8), intent(inout) :: concvt (noseg)         !< first solution estimation by means of local theta method
      real   (4), intent(in   ) :: volnew (noseg)         !< segment volumes at the new time
      integer(4), intent(in   ) :: nobnd                  !< number of boundary segments
      real   (4), intent(in   ) :: bound  (nosys,nobnd)   !< boundary concentrations
      integer(4), intent(in   ) :: noq                    !< number of exchanges
      integer(4), intent(in   ) :: iknmrk(noseg)          !< feature array
      integer(4), intent(in   ) :: ipoint ( 4   ,noq  )   !< exchange pointers
      real   (4), intent(in   ) :: area   (      noq  )   !< surface areas
      real   (4), intent(in   ) :: aleng  ( 2   ,noq  )   !< from- and to lengths (dim: 2*noq)
      real   (4), intent(in   ) :: theta  (      noq  )   !< local theta coefficients
      real   (4), intent(in   ) :: flowtot(      noq  )   !< flows plus additional velos.
      integer(4), intent(in   ) :: iopt                   !< option for e.g. treatment of boundaries
      real   (4), intent(inout) :: amass2 (notot, 5   )   !< areawide mass balance array
      integer(4), intent(in   ) :: ndmpq                  !< number of dumped discharges
      integer(4), intent(in   ) :: iqdmp  (noq)           !< pointer dumped exchages
      real   (4), intent(inout) :: dmpq   (nosys,ndmpq,2) !< mass balance array for monitoring areas

!         auxiliary limiter variables

      real                      :: length                 ! length between cel midpoints
      real                      :: cio, cjo, cin, cjn     ! old and local-theta from- and to concentrations
      integer                   :: ifrom , ito            ! from- and to segement indices
      integer                   :: ifrom1, itopl1         ! from- and to segement indices
      integer                   :: iseg                   ! current volume
      integer                   :: iq                     ! current edge
      real                      :: aflux                  ! corrective flux
      real                      :: vfrom, vto             ! 'from' and 'to' new volume
      real                      :: dq, e1, e3, s          ! support variables for the limiter
      real                      :: cfrm1, ctop1           ! concentration of from-1 and to+1 cell

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqm8", ithandl )

!         loop accross the number of exchanges

      do 10 iq = 1, noq

!         initialisations , check for transport anyhow

         ifrom  = ipoint(1,iq)
         ito    = ipoint(2,iq)
         ifrom1 = ipoint(3,iq)
         itopl1 = ipoint(4,iq)
         if (   ifrom  .eq. 0 .or.  ito    .eq. 0 ) cycle
         if (   ifrom  .le. 0 .and. ito    .le. 0 ) cycle
         if (   ifrom1 .eq. 0 .or.  itopl1 .eq. 0 ) cycle   ! no flux correction with closed edges
         if ( ( ifrom  .lt. 0 .or.  ito    .lt. 0 ) .and. btest(iopt,2) ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle       ! identified dry at start and end of timestep
         endif                                              ! aggregated time step can be wet in between
         if ( ito   .gt. 0 ) then                           ! start and end, that is why a check on 1 cm3/s
            if ( .not. btest(iknmrk(ito  ),0) ) cycle       ! life is not easy
         endif

!     Compute the difference flux towards 2nd order

         if ( ifrom .gt. 0 ) then
            cio = conc  ( isys,  ifrom )
            cin = concvt(        ifrom )
         else
            cio = bound ( isys, -ifrom )
            cin = bound ( isys, -ifrom )
         endif

         if ( ito   .gt. 0 ) then
            cjo = conc  ( isys,  ito   )
            cjn = concvt(        ito   )
         else
            cjo = bound ( isys, -ito   )
            cjn = bound ( isys, -ito   )
         endif

         if ( theta(iq) < 1.0E-25 ) then ! Lax-Wendroff flux correction at `explicit' edges (theta = 0)
            length = aleng(1,iq)+aleng(2,iq)
            if ( length .gt. 1.0e-25 ) then
               if ( flowtot(iq) .gt. 0 ) then          ! flow from i to j
                  aflux = (  aleng(1,iq)/length - ( flowtot(iq)*real(idt))/(2*area(iq)*length) )*flowtot(iq)*(cjo-cio)
               else                                ! flow from j to i
                  aflux = ( -aleng(2,iq)/length - ( flowtot(iq)*real(idt))/(2*area(iq)*length) )*flowtot(iq)*(cjo-cio)
               endif
            else
               aflux = 0.0
            endif
         else                          ! central flux correction at implicit edges (theta > 0)
            if ( flowtot(iq) .gt. 0 ) then ! flow from i to j
                aflux = ( 1.0 - theta(iq) )*( flowtot(iq)*(cio+cjo)/2.0 - flowtot(iq)*cio )
     &                        + theta(iq)  *( flowtot(iq)*(cin+cjn)/2.0 - flowtot(iq)*cin )
            else ! flow from j to i
                aflux = ( 1.0 - theta(iq) )*( flowtot(iq)*(cio+cjo)/2.0 - flowtot(iq)*cjo )
     &                        + theta(iq)  *( flowtot(iq)*(cin+cjn)/2.0 - flowtot(iq)*cjn )
            endif
         endif

         if ( aflux*(cin-cjn) > 0 ) aflux = 0.0 ! antidiffusion should not behave as diffusion.
         dq     = aflux*real(idt)

!         Flux correction at the open boundaries

         if ( ifrom .lt. 0 ) then
            if ( itopl1 .le. 0 ) cycle
            vto    = volnew(ito  )
            s  = sign ( 1.0 , dq )
            e3 = (concvt(itopl1) - concvt( ito  ))*vto
            dq = s * max( 0.0 , min( s*dq , s*e3 ) )
            concvt(ito  ) = concvt(ito  ) + dq / vto
            if ( dq .gt. 0 ) then
               amass2(isys,4) = amass2(isys,4) + dq
            else
               amass2(isys,5) = amass2(isys,5) - dq
            endif
            if ( btest(iopt,3) ) then             ! balances active
               if ( iqdmp(iq) .gt. 0 ) then       ! balances to be updated
                  if ( dq .gt. 0.0 ) then
                     dmpq(isys,iqdmp(iq),1)=dmpq(isys,iqdmp(iq),1) + dq
                  else
                     dmpq(isys,iqdmp(iq),2)=dmpq(isys,iqdmp(iq),2) - dq
                  endif
               endif
            endif
            cycle
         endif
         if ( ito   .lt. 0 ) then
            if ( ifrom1 .le. 0 ) cycle
            vfrom  = volnew(ifrom)
            s  = sign ( 1.0 , dq )
            e1 = (concvt(ifrom ) - concvt(ifrom1))*vfrom
            dq = s * max( 0.0 , min( s*e1 , s*dq ) )
            concvt(ifrom) = concvt(ifrom) - dq / vfrom
            if ( dq .gt. 0 ) then
               amass2(isys,5) = amass2(isys,5) + dq
            else
               amass2(isys,4) = amass2(isys,4) - dq
            endif
            if ( btest(iopt,3) ) then             ! balances active
               if ( iqdmp(iq) .gt. 0 ) then       ! balances to be updated
                  if ( dq .gt. 0.0 ) then
                     dmpq(isys,iqdmp(iq),1)=dmpq(isys,iqdmp(iq),1) + dq
                  else
                     dmpq(isys,iqdmp(iq),2)=dmpq(isys,iqdmp(iq),2) - dq
                  endif
               endif
            endif
            cycle
         endif

!         Boris and Book for the inner area

         vfrom  = volnew(ifrom)
         vto    = volnew(ito  )
         if ( vfrom .gt. 1.0e-25 .and. vto .gt. 1.0e-25 ) then
            s  = sign ( 1.0 , dq )
            if      ( ifrom1 .gt. 0 ) then
               cfrm1 = concvt( ifrom1 )
            else if ( ifrom1 .eq. 0 ) then
               if ( s .gt. 0 ) then
                  cfrm1 = 0.0
               else
                  cfrm1 = 2.0*concvt(ifrom)
               endif
            else if ( ifrom1 .lt. 0 ) then
               cfrm1 = bound(isys,-ifrom1)
            endif
            if      ( itopl1 .gt. 0 ) then
               ctop1 = concvt( itopl1 )
            else if ( itopl1 .eq. 0 ) then
               if ( s .gt. 0 ) then
                  ctop1 = 2.0*concvt(ito  )
               else
                  ctop1 = 0.0
               endif
            else if ( itopl1 .lt. 0 ) then
               ctop1 = bound(isys,-itopl1)
            endif

            e1 = (concvt(ifrom) - cfrm1       )*vfrom
            e3 = (ctop1         - concvt(ito ))*vto
            dq = s * max( 0.0 , min( s*e1 , s*dq , s*e3 ) )

            concvt(ifrom) = concvt(ifrom) - dq/vfrom
            concvt(ito  ) = concvt(ito  ) + dq/vto

            if ( btest(iopt,3) ) then             ! balances active
               if ( iqdmp(iq) .gt. 0 ) then       ! balances to be updated
                  if ( dq .gt. 0.0 ) then
                     dmpq(isys,iqdmp(iq),1)=dmpq(isys,iqdmp(iq),1) + dq
                  else
                     dmpq(isys,iqdmp(iq),2)=dmpq(isys,iqdmp(iq),2) - dq
                  endif
               endif
            endif
         endif

   10 continue

      do iseg = 1,noseg
         if ( btest(iknmrk(iseg),0) ) then
            conc(isys,iseg) = concvt(iseg)
         else
            conc(isys,iseg) = 0.0
         endif
      enddo

      if ( timon ) call timstop ( ithandl )
      end subroutine dlwqm8

      end module m_dlwqm8
