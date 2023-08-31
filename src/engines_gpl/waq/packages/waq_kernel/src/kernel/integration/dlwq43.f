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
      module m_dlwq43

      implicit none

      contains


!> Implicit solution of one dimension by double sweep, central discretisation of the advection term.

      subroutine dlwq43 ( nosys  , notot  , NOSEG  , noq    , nodisp ,
     &                    novelo , disp   , disper , velo   , area   ,
     &                    flow   , aleng  , ipoint , IKNMRK , idpnt  ,
     &                    ivpnt  , conc   , bound  , iopt   , ilflag ,
     &                    idt    , deriv  , iaflag , amass2 )

!> \file
!> Implicit solution of one dimension by double sweep, central discretisation of the advection term.
!> \par Description:
!> This routine performs the implicit solution of a tridiagonal system.\n
!> The implicit assumption is that the information comes in increasing row order.\n
!> For the horizontal this is equivalent with the assumption that 1D-lines in the grid have
!> consistent increasing computational volume numbers. So the 'from' 'to' pointer should
!> consistently point from a lower cell number to a higher cell number. \n
!> For the vertical this is equivalent with the assumption that the top layer is the first layer
!> with lowest computational volume numbers and that the 'from' 'to' pointer points downward,
!> then 'from' is always less than 'to'.\n
!> Rather than filling the tridiagonal matrix for a row in the grid or a column of the vertical
!> and then performing the double sweep, the sweep downwards is already done during the filling step
!> per layer. This means that only the diagonal and the upper codiagonal need to be stored
!> for the later backward sweep.\n
!> NOTE-1: First all diagonal and codiagonal entries of the first layer are resolved, then of the second
!> layer etc. So not per column, but per layer for all columns together. This allows for an ADI
!> (Alternate Direction Implicit) procedure without renumbering of the computational volumes.
!> NOTE-2: The routine is almost identical to dlwqd1.f that is only used for the vertical coordinate.

!     Deltares Software Centre

!     Function            : Makes double sweep implicit part of ADI

!     Created             : March     1988 by Leo Postma
!     Modified            : Unknown        by Jan van Beek
!                                          balances
!                         : September 2010 by Leo Postma
!                                          addition of feature array for drying and flooding
!                                          FORTRAN-90 look and feel

!     Files               : none

!     Routines            : none

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosys                !< number of transported substances
      integer  ( 4), intent(in   ) :: notot                !< total number of substances
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      integer  ( 4), intent(in   ) :: noq                  !< total number of interfaces
      integer  ( 4), intent(in   ) :: nodisp               !< number additional dispersions
      integer  ( 4), intent(in   ) :: novelo               !< number additional velocities
      real     ( 4), intent(in   ) :: disp  (3)            !< fixed dispersions in the 3 directions
      real     ( 4), intent(in   ) :: disper(nodisp,noq)   !< array with additional dispersions
      real     ( 4), intent(in   ) :: velo  (novelo,noq)   !< array with additional velocities
      real     ( 4), intent(in   ) :: area  (noq)          !< exchange areas in m2
      real     ( 4), intent(in   ) :: flow  (noq)          !< flows through the exchange areas in m3/s
      real     ( 4), intent(in   ) :: aleng (  2   ,noq)   !< mixing length to and from the exchange area
      integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
      integer  ( 4), intent(in   ) :: iknmrk(noseg)        !< feature array
      integer  ( 4), intent(in   ) :: idpnt (nosys)        !< additional dispersion number per substance
      integer  ( 4), intent(in   ) :: ivpnt (nosys)        !< additional velocity number per substance
      real     ( 4), intent(inout) :: conc  (notot,noseg)  !< masses after horizontal transport step
      real     ( 4), intent(in   ) :: bound (nosys,  *  )  !< open boundary concentrations
      integer  ( 4), intent(in   ) :: iopt                 !< bit 0: 1 if no dispersion at zero flow
                                                           !< bit 1: 1 if no dispersion across boundaries
                                                           !< bit 2: 1 if lower order across boundaries
                                                           !< bit 3: 1 if mass balance output
      integer  ( 4), intent(in   ) :: ilflag               !< if 0 then only 3 constant lenght values
      integer  ( 4), intent(in   ) :: idt                  !< time step in seconds
      real     ( 4), intent(inout) :: deriv (notot,noseg)  !< workspace containing the diagonal
      integer  ( 4), intent(in   ) :: iaflag               !< if 1 then accumulate mass in report array
      real     ( 4), intent(inout) :: amass2(notot, 5   )  !< report array for monitoring file

!         local variables

      integer  ( 4) iq                    ! loop counter exchanges
      integer  ( 4) isys                  ! loop counter substance
      integer  ( 4) ifrom, ito            ! from and to volume numbers
      real     ( 4) a                     ! this area
      real     ( 4) q                     ! flow for this exchange
      real     ( 4) e                     ! dispersion for this exchange
      real     ( 4) al                    ! this length
      real     ( 4) dl                    ! area / length
      real     ( 4) d                     ! dispersion for this substance
      real     ( 4) v                     ! flow for this substance
      real     ( 4) dq                    ! total flux from and to
      real     ( 4) f1, f2                ! linear interpolation variables
      real     ( 4) g1, g2                ! linear interpolation variables
      real     ( 4) q3, q4                ! flux help variables
      real     ( 4) diag, codiag, rhs     ! double sweep help variables

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq43", ithandl )

!         Loop over exchanges, single sweep forward

      do 60 iq = 1 , noq

!         Initialisations, check for transport anyhow

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .lt. 0 .and. ito .lt. 0 ) cycle

         a = area(iq)
         q = flow(iq)

         e  = disp (1)                                 ! there is only one direction
         al = aleng(1,1)                               ! the vertical
         if ( ilflag .eq. 1 ) then
              al = aleng(1,iq) + aleng(2,iq)
         endif
         if ( al .lt. 1.0E-25 ) cycle
         if ( ilflag .eq. 1 ) then
              f1 = aleng(2,iq) / al
              f2 = aleng(1,iq) / al
         else
              f1 = 0.5
              f2 = 0.5
         endif
         g1 = f1
         g2 = f2
         dl = a / al
         e  = e*dl
         if ( ifrom .lt. 0 .or. ito .lt. 0 ) goto 20

!         the regular case

         do isys = 1, nosys
            d  = e
            v  = q
            if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            if ( btest(iopt,0) ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0e-25 ) d = 0.0
            endif
            q3 = d*idt
            q3 = min ( q3, 1.0e+5*deriv(isys,ifrom), 1.0e+5*deriv(isys,ito) ) ! limitation to
            q4 = -q3                                                     ! avoid degradation of matrix
            if ( btest(iknmrk(ifrom),0) .and. btest(iknmrk(ito),0) ) then
               q3 = q3 + v*f1*idt
               q4 = q4 + v*f2*idt
            else
               if ( v .gt. 0.0 ) then
                  q3 = q3 + v*idt
               else
                  q4 = q4 + v*idt
               endif
            endif
!        row of the 'from' volume
            diag              = deriv(isys,ifrom) + q3                   ! first the diagonal (new volume) is in deriv
            codiag            = q4 / diag                                ! upper codiagonal scaled now (diagonal = 1.0)
            rhs               = conc (isys,ifrom) / diag                 ! right hand side also scaled
            deriv(isys,ifrom) = codiag                                   ! now the pivot is in deriv
            conc (isys,ifrom) = rhs                                      ! as said rhs is scaled
!        row of the 'to  ' segment
            deriv(isys,ito)   = deriv(isys,ito) - q4 + q3*codiag         ! lower co-diagonal immediately illiminated
            conc (isys,ito)   = conc (isys,ito)      + q3*rhs
         enddo
         cycle

!        'from' or 'to' volume is boundary

   20    do isys = 1, nosys
            d  = 0.0
            v  =  q
            if ( .not. btest(iopt,1) ) then
                d = e
                if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            if ( btest(iopt,0) ) then                                     ! this is important
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0e-25 ) d = 0.0        ! the settling velocity is
            endif                                                         ! not included in the test
            if ( btest(iopt,2) ) then
               if ( v .gt. 0 ) then
                  g1 = 1.0
                  g2 = 0.0
               else
                  g1 = 0.0
                  g2 = 1.0
               endif
            endif
            if ( ifrom .lt. 0 ) then
               deriv(isys,ito) = deriv(isys,ito) - (V*G2-D)*IDT
               conc (isys,ito) = conc (isys,ito) + (V*G1+D)*IDT*bound(isys,-ifrom)
            else
               diag              = deriv(isys,ifrom) + (V*G1+D)*IDT
               conc (isys,ifrom) = (conc(isys,ifrom) - (V*G2-D)*IDT*bound(isys,-ito)) / diag
               dq                = (V*G1+D)*conc(isys,ifrom) + (V*G2-D)*bound(isys,-ito)
               if ( iaflag .eq. 1 ) then
                  if ( dq .gt. 0.0 ) then
                     amass2(isys,5) = amass2(isys,5) + dq*idt
                  else
                     amass2(isys,4) = amass2(isys,4) - dq*idt
                  endif
               endif
               deriv(isys,ifrom) = 1.0
            endif
         enddo

!        End of loop

   60 continue

!         Loop over exchanges, single sweep backward

      do 100 iq = noq , 1 , -1
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .lt. 0 .and. ito .lt. 0 ) cycle
         if ( ifrom .lt. 0 ) goto  80
         if ( ito   .lt. 0 ) cycle

!         The regular case

         do isys = 1 , nosys
            codiag            = deriv(isys,ifrom)                         ! Deriv now contains the off-diagonal
            diag              = deriv(isys,ito  )                         ! the previous deriv contains the diagonal
            rhs               = conc (isys,ito  ) / diag                  ! scale the rhs of that row
            conc (isys,ifrom) = conc (isys,ifrom) - codiag*rhs            ! ipdate the from rhs
            conc (isys,ito  ) = rhs                                       ! set the to (the previous) conc
            deriv(isys,ifrom) = 1.0
            deriv(isys,ito  ) = 1.0
         enddo
         cycle

!        the from segment is boundary

   80    do isys = 1,nosys
            d  = 0.0
            v  =  q
            if ( .not. btest(iopt,1) ) then
                d = e
                if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a
            if ( btest(iopt,2) ) then
               if ( v .gt. 0 ) then
                  g1 = 1.0
                  g2 = 0.0
               else
                  g1 = 0.0
                  g2 = 1.0
               endif
            endif
            conc (isys,ito  ) = conc(isys,ito) / deriv(isys,ito)
            dq                = (V*G1+D)*bound(isys,-ifrom) + (V*G2-D)*conc(isys,ito)
            if ( iaflag .eq. 1 ) then
               if ( dq .gt. 0.0 ) then
                  amass2(isys,4) = amass2(isys,4) + dq*idt
               else
                  amass2(isys,5) = amass2(isys,5) - dq*idt
               endif
            endif
            deriv(isys,ito  ) = 1.0
         enddo

!        end of loop

  100 continue

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq43
