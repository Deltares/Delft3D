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
      module m_dlwqm3

      implicit none

      contains


      subroutine dlwqm3(idt , isys  , nosys  , notot  , noseg,
     &                  conc, deriv , volold , nobnd  , bound,
     &                  noq , ipoint, flowtot, disptot, theta,
     &                  diag, iscale, rhs    , sol           )

!     Deltares - Delft Software Department

!     Created   :      2007 by Pauline van Slingerland

!     Function  : fills the right hand side and the initial guess

!     Modified  : July 2009 by Leo Postma : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                    Description

      integer(4), intent(in   ) :: idt                   ! time step in scu's
      integer(4), intent(in   ) :: isys                  ! current active substance
      integer(4), intent(in   ) :: nosys                 ! number of active substances
      integer(4), intent(in   ) :: notot                 ! total number of substances

      integer(4), intent(in   ) :: noseg                 ! number of segments
      real   (4), intent(in   ) :: conc   (notot, noseg) ! concentrations
      real   (4), intent(in   ) :: deriv  (notot, noseg) ! processes and discharges (divided by the time step idt)
      real   (4), intent(in   ) :: volold (noseg)        ! segment volumes at the previous time
      integer(4), intent(in   ) :: nobnd                 ! number of boundary segments
      real   (4), intent(in   ) :: bound  (nosys, nobnd) ! boundary concentrions

      integer(4), intent(in   ) :: noq                   ! number of exchanges
      integer(4), intent(in   ) :: ipoint (4,noq)        ! exchange pointers (dim: 4 x noq)
      real   (4), intent(in   ) :: flowtot(noq)          ! flows plus additional velos. (dim: noq)
      real   (4), intent(in   ) :: disptot(noq)          ! dispersion plus additional dipers. (dim: noq)
      real   (4), intent(in   ) :: theta  (noq)          ! variable theta coefficients

      real   (8), intent(in   ) :: diag   (noseg+nobnd)  ! diagonal of the matrix (lhs)
      integer(4), intent(in   ) :: iscale                ! 0: no diagonal scaling
                                                         ! 1: diagonal scaling
      real   (8), intent(  out) :: rhs(noseg+nobnd)      ! righthandside
      real   (8), intent(  out) :: sol(noseg+nobnd)      ! initial guess

      integer(4)                :: ifrom,ito             ! from- and to segments
      real   (4)                :: ci,cj                 ! from- and to concentrations
      real   (4)                :: fluxij                ! flux from segment i to segment j
      integer(4)                :: iseg                  ! current segment
      integer(4)                :: ibnd                  ! current boundary segment
      integer(4)                :: iq                    ! current edge

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqm3", ithandl )

! volumes, processes, and discharges
      do iseg = 1, noseg
         rhs(iseg) = volold(iseg) * conc(isys,iseg) / real(idt) + deriv(isys,iseg)
      enddo

      do ibnd = 1, nobnd
         rhs(noseg+ibnd) = bound(isys,ibnd)
      enddo

! flow and diffusion
      do iq = 1 , noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or. ito .eq. 0 ) cycle

! compute from- and to concentrations
         if ( ifrom .gt. 0 ) then
            ci = conc (isys, ifrom)
         else
            ci = bound(isys,-ifrom)
         endif
         if ( ito   .gt. 0 ) then
            cj = conc (isys, ito  )
         else
            cj = bound(isys,-ito  )
         endif

! compute flux from i to j
         if ( flowtot(iq) .gt. 0 ) then         ! flow from i to j
            fluxij = flowtot(iq)*ci - disptot(iq)*(cj-ci)
         else                                   ! flow from j to i
            fluxij = flowtot(iq)*cj - disptot(iq)*(cj-ci)
         endif

! add flux to both neighbours
         if ( ifrom .gt. 0 ) rhs(ifrom) = rhs(ifrom) - (1-theta(iq))*fluxij
         if ( ito   .gt. 0 ) rhs(ito  ) = rhs(ito  ) + (1-theta(iq))*fluxij
      enddo

! scale rhs (diagonal scaling to improve convergence of gmres)
      if ( iscale .eq. 1 ) then
         do iseg = 1, noseg+nobnd
            rhs(iseg) = rhs(iseg) / diag(iseg)
         enddo
      endif

!        zero initial guess, try previous concentration for water volumes
!        ( alternatively take zero vector ). Zero initial guess for boundaries.

      sol = 0.0
      do iseg = 1, noseg
         sol(iseg) = conc(isys,iseg) + 0.01
      enddo

      if ( timon ) call timstop ( ithandl )
      endsubroutine dlwqm3

      end module m_dlwqm3
