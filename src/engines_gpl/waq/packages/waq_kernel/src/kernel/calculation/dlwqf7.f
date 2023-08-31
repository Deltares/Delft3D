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
      module m_dlwqf7

      implicit none

      contains


      subroutine dlwqf7( isys    , nosys   , notot   , noseg   , conc    ,
     &                   concvt  , nobnd   , bound   , noq     , ipoint  ,
     &                   flowtot , disptot , amass2  , ndmpq   , iqdmp   ,
     &                   dmpq    , iknmrk  , idt     )

!     Deltares Software Centre

!>/file
!>            mass balance of transport and copy of solution in the concentration array
!>
!>            Iterative solver solves 1 substance at a time (per thread in parallel mode)\n
!>            This routine saves the double precision solution in the single precision cons array\n
!>            The tricky part is that for dry cells 0.0 is taken. In previous versions the old
!>            value remained untouched or if influenced by the iteration it got a strange value.\n
!>            Furthermore with the concentration values the mass balance aarays are updated.\n
!>            The update makes use of the pre-computed flowtot and disptot arrays for this substance.
!>            They contain the flow and disp plus additional velocity and dispersion terms.
!>            This avoids to have the logics of additional velocities and dispersions again
!>            in this routine.

!     Created                 : June 2011 by Leo Postma

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                    Description

      integer(4), intent(in   ) :: isys                  !< current transported substance
      integer(4), intent(in   ) :: nosys                 !< number of active substances
      integer(4), intent(in   ) :: notot                 !< total number of substances
      integer(4), intent(in   ) :: noseg                 !< number of computational volumes
      real   (4), intent(inout) :: conc   (notot, noseg) !< concentration vector to store results
      real   (8), intent(in   ) :: concvt (       noseg) !< newly obtained concentration from the solver
      integer(4), intent(in   ) :: nobnd                 !< number of volumes with open boundaries
      real   (4), intent(in   ) :: bound  (nosys, nobnd) !< boundary concentrations
      integer(4), intent(in   ) :: noq                   !< number of exchanges between volumes
      integer(4), intent(in   ) :: ipoint ( 4   , noq  ) !< exchange pointers
      real   (4), intent(in   ) :: flowtot( noq )        !< flows plus additional velos.
      real   (4), intent(in   ) :: disptot( noq )        !< dispersion plus additional dipers.
      real   (4), intent(inout) :: amass2 (notot, 5    ) !< mass balance array for the whole model area
      integer(4), intent(in   ) :: ndmpq                 !< number of dumped exchanges
      integer(4), intent(in   ) :: iqdmp  ( noq )        !< pointers dumped exchages
      real   (4), intent(inout) :: dmpq  (nosys,ndmpq,2) !< flux accumulation array for monitoring areas
      integer(4), intent(in   ) :: iknmrk(noseg)         !< feature array, bit zero indicates wet or not
      integer(4), intent(in   ) :: idt                   !< time step

!     Locals              :

      real   (4)                   cin, cjn              ! from- and to concentrations
      real   (4)                   fluxij                ! flux from i to j
      integer(4)                   ifrom , ito           ! from- and to volume indices
      integer(4)                   iseg                  ! current computational volume
      integer(4)                   iq                    ! current edge

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqf7", ithandl )

!         put result in the concentration array

      do iseg = 1, noseg
         if ( btest(iknmrk(iseg),0) ) then
            conc(isys,iseg) = concvt(iseg)
         else
            conc(isys,iseg) = 0.0
         endif
      enddo

!         flow and diffusion

      do iq = 1, noq

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

!             only compute where needed

         if ( ifrom .gt. 0 .and. ito .gt. 0 .and. iqdmp(iq) .eq. 0 ) cycle
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle

         if ( ifrom .gt. 0 ) then
            cin = concvt(        ifrom )
         else
            cin = bound ( isys, -ifrom )
         endif

         if ( ito   .gt. 0 ) then
            cjn = concvt(        ito   )
         else
            cjn = bound ( isys, -ito   )
         endif

         if ( flowtot(iq) .gt. 0 ) then                      ! flow from i to j
            fluxij =  real(idt) * ( (flowtot(iq)+disptot(iq))*cin - disptot(iq)*cjn )
         else                                                ! flow from j to i
            fluxij =  real(idt) * ( (flowtot(iq)-disptot(iq))*cjn + disptot(iq)*cin )
         endif

!             mass balance of the whole area

         if ( ifrom .lt. 0 ) then
            if ( fluxij .gt. 0 ) then
               amass2(isys,4) = amass2(isys,4) + fluxij
            else                                             ! amass2(*,1) masses of the substances in the model
               amass2(isys,5) = amass2(isys,5) - fluxij      ! amass2(*,2) change by processes
            endif                                            ! amass2(*,3) change by discharges
         endif                                               ! amass2(*,4) incoming boundary transport
         if ( ito   .lt. 0 ) then                            ! amass2(*,5) outgoing boundary transport
            if ( fluxij .gt. 0 ) then
               amass2(isys,5) = amass2(isys,5) + fluxij
            else
               amass2(isys,4) = amass2(isys,4) - fluxij
            endif
         endif

!             mass balance of selected monitoring areas

         if ( iqdmp(iq) .gt. 0 ) then                        ! dmpq(*,*,1) incoming transport in a monitoring area
            if ( fluxij .gt. 0 ) then                        ! dmpq(*,*,2) outgoing transport from a monitoring area
               dmpq( isys, iqdmp(iq), 1 ) = dmpq( isys, iqdmp(iq), 1 ) + fluxij
            else
               dmpq( isys, iqdmp(iq), 2 ) = dmpq( isys, iqdmp(iq), 2 ) - fluxij
            endif
         endif

      enddo

      if ( timon ) call timstop ( ithandl )
      end subroutine dlwqf7

      end module m_dlwqf7
