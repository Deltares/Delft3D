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
      module m_dlwq16a

      implicit none

      contains


      subroutine dlwq16a( nosys  , notot  , noseg  , noq1   , noq2   ,
     &                    noq3   , noq    , nodisp , novelo , disp   ,
     &                    disper , velo   , area   , flow   , aleng  ,
     &                    ipoint , iknmrk , idpnt  , ivpnt  , conc   ,
     &                    bound  , iopt   , ilflag , idt    , deriv  ,
     &                    iaflag , amass2 , ndmpq  , iqdmp  , dmpq   )

!     Deltares Software Centre

!>\file
!>         Makes explicit upwind derivatives for the advection diffusion equation.
!>         Special version of dlwq16 for first order upwind in space and time for emission module (25)
!>
!>         This routine makes for the nosys transported substaces the contribution of the advection and
!>         the diffusion to the DERIV(notot,noseg) array. Notot is the total number of substances,
!>         noseg is the number of computational volumes.\n
!>         This process is steered with options for:\n
!>         1) no dispersion at zero flow (typical for thin dams and drying flats) (bit 0 of iopt is 1)\n
!>         2) no dispersion accross open boundaries (bit 1 of iopt is 1)\n
!>         Besides the water flow in the array FLOW(noq), there are optional additional velocities.
!>         These options are often used in the vertical for settling velocities of particulates or
!>         floating velocities of blue-green algae. The array IVPNT tells which additional velocity applies
!>         for which substance. Their values are in VELO(novelo,noq).\n
!>         Besides the constant diffusion terms in 3 direction contained in DISP(3), there are optional
!>         additional dispersions. These options are often used in the vertical for time and space varying
!>         vertical diffusion as computed by the vertical turbulence model in the hydrodynamic model.
!>         This option may also become common for the horizontal if the Horizontal Large Scale Eddy
!>         diffusivity as computed by the hydrodynamic model will commonly be used.\n
!>         This routine also accumulates on the fly the mass balance information for the whole area in
!>         the AMASS2(notot,5) array. This array is printed as header for every time step in the monitoring
!>         file.\n
!>         Furthermore the fluxes in and out of monitoring areas for detail balances are accumulated on
!>         the fly. Which flux needs to be accumulated in what balance is given in the IQDMP(noq) array.

!     Function            : Makes explicit derivatives according to
!                           upwind differencing in space

!     Files               : lun: the monitoring file

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
      real     ( 4), intent(in   ) :: area  (noq)          !< exchange areas in m2
      real     ( 4), intent(in   ) :: flow  (noq)          !< flows through the exchange areas in m3/s
      real     ( 4), intent(in   ) :: aleng (  2   ,noq)   !< mixing length to and from the exchange area
      integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
      integer  ( 4), intent(in   ) :: iknmrk(noseg)        !< feature array
      integer  ( 4), intent(in   ) :: idpnt (nosys)        !< additional dispersion number per substance
      integer  ( 4), intent(in   ) :: ivpnt (nosys)        !< additional velocity number per substance
      real     ( 4), intent(in   ) :: conc  (notot,noseg)  !< concentrations at previous time level
      real     ( 4), intent(in   ) :: bound (nosys,  *  )  !< open boundary concentrations
      integer  ( 4), intent(in   ) :: iopt                 !< bit 0: 1 if no dispersion at zero flow
                                                           !< bit 1: 1 if no dispersion across boundaries
                                                           !< bit 3: 1 if mass balance output
      integer  ( 4), intent(in   ) :: ilflag               !< if 0 then only 3 constant lenght values
      integer  ( 4), intent(in   ) :: idt                  !< time step in seconds
      real     ( 4), intent(inout) :: deriv (notot,noseg)  !< explicit derivative in mass/s
      integer  ( 4), intent(in   ) :: iaflag               !< if 1 then accumulate mass in report array
      real     ( 4), intent(inout) :: amass2(notot, 5   )  !< report array for monitoring file
      integer  ( 4), intent(in   ) :: ndmpq                !< number of dumped exchanges for mass balances
      integer  ( 4), intent(in   ) :: iqdmp (noq)          !< pointer from echange to dump location
      real     ( 4), intent(inout) :: dmpq  (nosys,ndmpq,2)!< array with mass balance information

!     Local variables     :

      integer  ( 4) iq , k      ! loop counter exchanges
      integer  ( 4) isys        ! loop counter substance
      integer  ( 4) noq12       ! number of horizontal exchanges
      integer  ( 4) ifrom, ito  ! from and to volume numbers
      real     ( 4) a           ! this area
      real     ( 4) q           ! flow for this exchange
      real     ( 4) e           ! dispersion for this exchange
      real     ( 4) al          ! this length
      real     ( 4) dl          ! area / length
      real     ( 4) d           ! dispersion for this substance
      real     ( 4) v           ! flow for this substance
      real     ( 4) dq          ! total flux from and to
      integer  ( 4) ipb         ! pointer in the mass balance dump array

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq16", ithandl )

!         loop accross the number of exchanges

      noq12 = noq1 + noq2
      do 60 iq = 1 , noq

!         initialisations, check if transport will take place

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or.  ito .eq. 0 ) cycle
         if ( ifrom .le. 0 .and. ito .le. 0 ) cycle

         a = area(iq)
         q = flow(iq)
         if ( btest(iopt,0) .and. abs(q) .lt. 1.0e-25 )  cycle ! thin dam option, no dispersion at zero flow

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

         if ( al .gt. 1.0e-25 ) then
            dl = a / al
         else
            dl = 0.0
         endif
         e  = e*dl                              ! in m3/s
         if ( ifrom .lt. 0 ) goto 20
         if ( ito   .lt. 0 ) goto 40

!         The regular case

         do isys = 1, nosys
            d  = e
            v  = q
            if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a

!              upwinding

            dq = v
            deriv(isys,ifrom) = deriv(isys,ifrom) - dq
            deriv(isys,ito  ) = deriv(isys,ito  ) + dq

!              balances

            if ( ipb .gt. 0 ) then
               if ( dq .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dq*idt
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dq*idt
               endif
            endif
         enddo
         cycle

!        The 'from' element was a boundary. Note the 2 options.

   20    do isys = 1, nosys
            v  = q
            d  = 0.0
            if ( .not. btest(iopt,1) ) then
                d = e
                if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a

!              upwinding

            dq = v
            deriv(isys,ito  ) = deriv(isys,ito  ) + dq

!              balances

            if ( iaflag .eq. 1 ) then
               if ( dq .gt. 0.0 ) then
                    amass2(isys,4) = amass2(isys,4) + dq*idt
               else
                    amass2(isys,5) = amass2(isys,5) - dq*idt
               endif
            endif
            if ( ipb .gt. 0 ) then
               if ( dq .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dq*idt
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dq*idt
               endif
            endif
         enddo
         cycle

!        The 'to' element was a boundary.

   40    do isys = 1, nosys
            v  = q
            d  = 0.0
            if ( .not. btest(iopt,1) ) then
                d = e
                if ( idpnt(isys) .gt. 0 ) d = d + disper( idpnt(isys), iq ) * dl
            endif
            if ( ivpnt(isys) .gt. 0 ) v = v + velo  ( ivpnt(isys), iq ) * a

!              upwinding

            dq = v
            deriv(isys,ifrom) = deriv(isys,ifrom) - dq

!              balances

            if ( iaflag .eq. 1 ) then
               if ( dq .gt. 0.0 ) then
                    amass2(isys,5) = amass2(isys,5) + dq*idt
               else
                    amass2(isys,4) = amass2(isys,4) - dq*idt
               endif
            endif
            if ( ipb .gt. 0 ) then
               if ( dq .gt. 0.0 ) then
                  dmpq(isys,ipb,1) = dmpq(isys,ipb,1) + dq*idt
               else
                  dmpq(isys,ipb,2) = dmpq(isys,ipb,2) - dq*idt
               endif
            endif
         enddo

!        end of the loop over exchanges

   60 continue

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq16a
