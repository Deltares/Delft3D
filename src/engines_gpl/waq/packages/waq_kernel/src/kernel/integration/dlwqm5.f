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
      module m_dlwqm5

      implicit none

      contains


      subroutine dlwqm5( idt    , isys   , nosys  , notot  , noseg  ,
     &                   conc   , concvt , volnew , nobnd  , bound  ,
     &                   noq    , noq1   , noq2   , noq3   , ipoint ,
     &                   iknmrk , area   , aleng  , theta  , flowtot,
     &                   disptot, iopt   , amass2 , ndmpq  , iqdmp  ,
     &                   dmpq   , flux   , lim    , maxi   , mini   ,
     &                   l1     , l2     , m1     , m2     , n1     ,
     &                   n2     )

!     Deltares Software Centre

!>/File
!>            This is the Zalezac flux correction procedure
!>
!>            Procedure:
!>            - all wanted corrections are summed per computational volume
!>            - all room for change of concentrations without generating
!>              new maxima or minima are evaluted per volume
!>            - then those corrections are applied pro-rato
!>            - flux correction accross open boundaries is removed !

!     Created   :      2007 by Pauline van Slingerland

!     Function  : fills the right hand side and the initial guess

!     Modified  : July 2009 by Leo Postma : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                    Description

      integer(4), intent(in   ) :: idt                !< time step in scu's
      integer(4), intent(in   ) :: isys               !< current active substance
      integer(4), intent(in   ) :: nosys              !< number of active substances
      integer(4), intent(in   ) :: notot              !< total number of substances
      integer(4), intent(in   ) :: noseg              !< number of segments
      real   (4), intent(inout) :: conc(notot,noseg)  !< concentrations
      real   (8), intent(inout) :: concvt(noseg)      !< first solution estimation by means of local theta method
      real   (4), intent(in   ) :: volnew(noseg)      !< segment volumes at the new time
      integer(4), intent(in   ) :: nobnd              !< number of boundary segments
      real   (4), intent(in   ) :: bound(nosys,nobnd) !< boundary concentrations
      integer(4), intent(in   ) :: noq                !< number of exchanges
      integer(4), intent(in   ) :: noq1               !< number of exchanges in the first direction
      integer(4), intent(in   ) :: noq2               !< number of exchanges in the second direction
      integer(4), intent(in   ) :: noq3               !< number of exchanges in the third direction
      integer(4), intent(in   ) :: ipoint(4,noq)      !< exchange pointers
      integer(4), intent(in   ) :: iknmrk(noseg)      !< feature array
      real   (4), intent(in   ) :: area(noq)          !< surface areas
      real   (4), intent(in   ) :: aleng(2,noq)       !< from- and to lengths (dim: 2*noq)
      real   (4), intent(in   ) :: theta(noq)         !< local theta coefficients
      real   (4), intent(in   ) :: flowtot(noq)       !< flows plus additional velos.
      real   (4), intent(in   ) :: disptot(noq)       !< dispersion plus additional dipers.
      integer(4), intent(in   ) :: iopt               !< options for special treatment of boundaries etc.
      real   (4), intent(inout) :: amass2(notot,5)    !< areawide mass balance arrays
      integer(4), intent(in   ) :: ndmpq              !< number of dumped discharges
      integer(4), intent(in   ) :: iqdmp(noq)         !< pointer dumped exchages
      real   (4), intent(inout) :: dmpq(nosys,ndmpq,2)!< mass balance array per monitoring area
      real   (4)                   flux( noq )        !< flux corrections
      real   (4)                   lim ( noq )        !< limiter
      real   (4)                   maxi(noseg)        !< workspace
      real   (4)                   mini(noseg)        !< workspace
      real   (4)                   l1  (noseg)        !< workspace
      real   (4)                   l2  (noseg)        !< workspace
      real   (4)                   m1  (noseg)        !< workspace
      real   (4)                   m2  (noseg)        !< workspace
      real   (4)                   n1  (noseg)        !< workspace
      real   (4)                   n2  (noseg)        !< workspace

!    Local variables

      real                   :: length
      real                   :: cio, cjo, cin, cjn ! old and local-theta from- and to concentrations
      integer                :: ifrom, ito         ! from- and to segement indices
      integer                :: iseg               ! current segment
      integer                :: iq                 ! current edge

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqm5", ithandl )

! initialisation
      do iq = 1, noq
         flux(iq) = 0.0
         lim (iq) = 1.0
      enddo

      do iseg = 1, noseg
         maxi(iseg) = concvt(iseg)
         mini(iseg) = concvt(iseg)
         l1(iseg)   = 0.0
         l2(iseg)   = 0.0
         m1(iseg)   = 0.0
         m2(iseg)   = 0.0
         n1(iseg)   = 0.0
         n2(iseg)   = 0.0
      enddo

! compute flux corrections
      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle       ! identified dry at start and end of timestep
         endif                                              ! aggregated time step can be wet in between
         if ( ito   .gt. 0 ) then                           ! start and end, that is why a check on 1 cm3/s
            if ( .not. btest(iknmrk(ito  ),0) ) cycle       ! life is not easy
         endif

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
            if ( length .gt. 1.0E-25 ) then
               if ( flowtot(iq) .gt. 0 ) then          ! flow from i to j
                  flux(iq) = (  aleng(1,iq)/length - ( flowtot(iq)*real(idt))/(2*area(iq)*length) )*flowtot(iq)*(cjo-cio)
               else                                    ! flow from j to i
                  flux(iq) = ( -aleng(2,iq)/length - ( flowtot(iq)*real(idt))/(2*area(iq)*length) )*flowtot(iq)*(cjo-cio)
               endif
            endif
         else                          ! central flux correction at implicit edges (theta > 0)
            if ( flowtot(iq) .gt. 0 ) then ! flow from i to j
                flux(iq) = ( 1.0 - theta(iq) )*( flowtot(iq)*(cio+cjo)/2.0 - flowtot(iq)*cio )
     &                           + theta(iq)  *( flowtot(iq)*(cin+cjn)/2.0 - flowtot(iq)*cin )
            else ! flow from j to i
                flux(iq) = ( 1.0 - theta(iq) )*( flowtot(iq)*(cio+cjo)/2.0 - flowtot(iq)*cjo )
     &                           + theta(iq)  *( flowtot(iq)*(cin+cjn)/2.0 - flowtot(iq)*cjn )
            endif
         endif

         if (flux(iq)*(cin-cjn)>0)  flux(iq)=0.0 ! antidiffusion should not behave as diffusion.

      enddo

! compute limiter a la Zalesak

      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle       ! identified dry at start and end of timestep
         endif                                              ! aggregated time step can be wet in between
         if ( ito   .gt. 0 ) then                           ! start and end, that is why a check on 1 cm3/s
            if ( .not. btest(iknmrk(ito  ),0) ) cycle       ! life is not easy
         endif

         if ( ifrom .gt. 0 ) then
            if ( ito  .gt. 0 ) then
               maxi(ifrom) = max(maxi(ifrom),concvt(      ito  ))
               mini(ifrom) = min(mini(ifrom),concvt(      ito  ))
            else
               maxi(ifrom) = max(maxi(ifrom),bound(isys, -ito  ))
               mini(ifrom) = min(mini(ifrom),bound(isys, -ito  ))
            endif

            l1(ifrom) = l1(ifrom) + real(idt)*max(0.0,-flux(iq))
            l2(ifrom) = l2(ifrom) + real(idt)*max(0.0, flux(iq))
         endif

         if ( ito   .gt. 0 ) then
            if ( ifrom .gt. 0 ) then
               maxi(ito  ) = max(maxi(ito  ),concvt(      ifrom))
               mini(ito  ) = min(mini(ito  ),concvt(      ifrom))
            else
               maxi(ito  ) = max(maxi(ito  ),bound(isys, -ifrom))
               mini(ito  ) = min(mini(ito  ),bound(isys, -ifrom))
            endif

            l1(ito  ) = l1(ito  ) + real(idt)*max(0.0, flux(iq))
            l2(ito  ) = l2(ito  ) + real(idt)*max(0.0,-flux(iq))
         endif
      enddo

      do iseg = 1, noseg
         m1(iseg) = volnew(iseg) * (maxi(iseg)-concvt(iseg))
         m2(iseg) = volnew(iseg) * (concvt(iseg)-mini(iseg))
         if ( l1(iseg) .gt. 1.0E-25 ) n1(iseg) = min(1.0,m1(iseg)/l1(iseg))
         if ( l2(iseg) .gt. 1.0E-25 ) n2(iseg) = min(1.0,m2(iseg)/l2(iseg))
      enddo

      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .gt. 0 .and.  ito .gt. 0 ) then
            if ( flux(iq) .lt. 0 ) then
               lim(iq) = min(n1(ifrom),n2(ito  ))
            else
               lim(iq) = min(n1(ito  ),n2(ifrom))
            endif
         endif
      enddo

! store the result

      do iseg = 1, noseg
         if ( btest(iknmrk(iseg),0) ) then
            conc(isys,iseg) = concvt(iseg)
         else
            conc(isys,iseg) = 0.0
         endif
      enddo

! solution estimation 2: local theta FCT solution estimation
! apply limited flux correction

      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .le. 0 .and.  ito .le. 0 ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle       ! identified dry at start and end of timestep
         endif                                              ! aggregated time step can be wet in between
         if ( ito   .gt. 0 ) then                           ! start and end. Life is not easy
            if ( .not. btest(iknmrk(ito  ),0) ) cycle
         endif
         if ( ifrom .gt. 0 .and.  ito .gt. 0 ) then
            if ( volnew(ifrom) .gt. 1.0e-25 .and. volnew(ito) .gt. 1.0e-25 ) then
               conc(isys,ifrom)=conc(isys,ifrom) - lim(iq)*real(idt)*flux(iq)/volnew(ifrom)
               conc(isys, ito )=conc(isys, ito ) + lim(iq)*real(idt)*flux(iq)/volnew( ito )
               if ( iqdmp(iq) .gt. 0 ) then
                  if ( flux(iq) .gt. 0 ) then
                     dmpq(isys,iqdmp(iq),1) = dmpq(isys,iqdmp(iq),1) + real(idt)*lim(iq)*flux(iq)
                  else
                     dmpq(isys,iqdmp(iq),2) = dmpq(isys,iqdmp(iq),2) - real(idt)*lim(iq)*flux(iq)
                  endif
               endif
            endif
         endif
      enddo

      if ( timon ) call timstop ( ithandl )
      end subroutine dlwqm5

      end module m_dlwqm5
