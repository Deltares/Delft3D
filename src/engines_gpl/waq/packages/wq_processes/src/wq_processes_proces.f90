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

      subroutine wq_processes_proces ( notot  , noseg  , conc   , volume , time   , &
                                       dts    , deriv  , ndmpar , nproc  , noflux , &
                                       ipmsa  , prvnio , promnr , iflux  , increm , &
                                       flux   , flxdmp , stochi , ibflag , ipbloo , &
                                       ioffbl , amass  , nosys  , isfact , itfact , &
                                       iexpnt , iknmrk , noq1   , noq2   , noq3   , &
                                       noq4   , area   , ndspn  , idpnew , dispnw , &
                                       ndspx  , dspx   , dsto   , nveln  , ivpnew , &
                                       velonw , nvelx  , velx   , vsto   , isdmp  , &
                                       defaul , prondt , prvvar , prvtyp , vararr , &
                                       varidx , arrpoi , arrknd , arrdm1 , arrdm2 , &
                                       novar  , a      , ndmps  , pronam , prvpnt , &
                                       nodef  , surfac , flux_int)

!     Deltares Software Centre

!>\File
!>         This subroutine procesfm is a 'light' variant of PROCES, intended to be called from FM
!>
!>         Control routine of PROCES system. Process sub-system of DELWAQ waterquality modelling system.

      use processes_pointers, only: dll_opb
      use process_registration
      use timers

      implicit none


!     Arguments           :

!     Kind         Function         Name                          Description

      integer( 4), intent(in   ) :: notot                       !< Total number of substances
      integer( 4), intent(in   ) :: noseg                       !< Nr. of computational volumes
      integer( 4), intent(in   ) :: nodef                       !< Number of values in the deafult array
      integer( 4), intent(in   ) :: novar                       !<
      real   ( 4), intent(inout) :: conc  (notot,noseg)         !< Model concentrations
      real   ( 8), intent(in   ) :: volume(      noseg)         !< Segment volumes
      real   ( 8), intent(in   ) :: time                        !< Time in system clock units
      real   ( 8), intent(in   ) :: dts                         !< Time step system clock units
      real   ( 8), intent(inout) :: deriv (noseg,notot)         !< Model derivatives
      integer( 4), intent(in   ) :: ndmpar                      !< Number of dump areas
      integer( 4), intent(in   ) :: nproc                       !< Number of processes
      integer( 4), intent(in   ) :: noflux                      !< Number of fluxes
      integer( 4), intent(in   ) :: ipmsa (*)                   !< Direct pointer in DELWAQ arrays
      integer( 4), intent(in   ) :: prvnio(nproc)               !< Nr. of state variables per proces
      integer( 4), intent(in   ) :: promnr(nproc)               !< Proces module number per proces
      integer( 4), intent(in   ) :: iflux (nproc)               !< Offset in flux array per process
      integer( 4), intent(in   ) :: increm(*)                   !< Direct increment in DELWAQ arrays
      real   ( 4)                :: flux  (noflux,noseg)        !< Proces fluxes
      real   ( 8), intent(inout) :: flxdmp(2,noflux,ndmps)        !< Fluxes at dump segments
      real   ( 4), intent(in   ) :: stochi(notot ,noflux)       !< Proces stochiometry
      integer( 4), intent(in   ) :: ibflag                      !< if 1 then mass balance output
      integer( 4), intent(in   ) :: ipbloo                      !< Number of Bloom module  (if >0)
      integer( 4), intent(in   ) :: ioffbl                      !< Offset in IPMSA for Bloom
      real   ( 8), intent(inout) :: amass (notot,noseg)         !< mass array to be updated
      integer( 4), intent(in   ) :: nosys                       !< number of active substances
      integer( 4), intent(in   ) :: isfact                      !< system clock in seconds
      integer( 4), intent(in   ) :: itfact                      !< time scale factor processes
      integer( 4), intent(in   ) :: iexpnt(4,*)                 !< Exchange pointer
      integer( 4), intent(in   ) :: iknmrk(noseg)               !< Integration suboptions
      integer( 4), intent(in   ) :: noq1                        !< Number of exchanges first direction
      integer( 4), intent(in   ) :: noq2                        !< Number of exchanges second direction
      integer( 4), intent(in   ) :: noq3                        !< Number of exchanges vertical
      integer( 4), intent(in   ) :: noq4                        !< Number of exchanges in the bed
      real   ( 4), intent(in   ) :: area  (*)                   !< exchange areas
      integer( 4), intent(in   ) :: ndspn                       !< Number of new dispersion arrays
      integer( 4), intent(in   ) :: idpnew(nosys )              !< Pointer to new disp array
      real   ( 4), intent(inout) :: dispnw(ndspn ,*)            !< New dispersion array
      integer( 4), intent(in   ) :: ndspx                       !< Nr. of calculated dispersions
      real   ( 4)                :: dspx  (ndspx ,*)            !< Calculated dispersions
      real   ( 4), intent(in   ) :: dsto  (nosys,ndspx)         !< Factor for calc. dispersions
      integer( 4), intent(in   ) :: nveln                       !< Nr. of new velocity array's
      integer( 4), intent(in   ) :: ivpnew(nosys )              !< Pointer to new velo array
      real   ( 4), intent(  out) :: velonw(nveln ,*)            !< New velocity array
      integer( 4), intent(in   ) :: nvelx                       !< Nr. of calculated velocities
      real   ( 4)                :: velx  (nvelx ,*)            !< Calculated velocities
      real   ( 4), intent(in   ) :: vsto  (nosys,nvelx)         !< Factor for velocitie
      integer( 4), intent(in   ) :: isdmp (noseg)               !< pointer dumped segments
      real   ( 4), intent(inout) :: defaul(nodef)               !< Default proces parameters
      integer( 4), intent(inout) :: prondt(nproc)               !<
      integer( 4), intent(in   ) :: prvvar(*)                   !<
      integer( 4), intent(in   ) :: prvtyp(*)                   !<
      integer( 4), intent(in   ) :: vararr(novar)               !<
      integer( 4), intent(in   ) :: varidx(novar)               !<
      integer( 4), intent(in   ) :: arrpoi(78)                  !<
      integer( 4), intent(in   ) :: arrknd(78)                  !<
      integer( 4), intent(in   ) :: arrdm1(78)                  !<
      integer( 4), intent(in   ) :: arrdm2(78)                  !<
      real   ( 4), intent(in   ) :: a     (*)                   !<
      integer( 4), intent(in   ) :: ndmps                       !<
      character(10)              :: pronam(nproc)               !< Name of called module
      integer( 4), intent(in   ) :: prvpnt(nproc)               !< entry in process io pointers (cummulative of prvnio)
      real   ( 4), intent(in   ) :: surfac(noseg)               !< horizontal surface
      integer( 4), intent(in   ) :: flux_int                    !< Switch for integration of process fluxes by Delwaq (or not)
      integer( 4)                :: lunrep                      !< Logical unit number of report-file

!     Local declarations

      integer( 4)  ivar  , iarr  , iv_idx, ip_arr          !  help variables
      integer( 4)  ipndt , ndtblo                          !  help variables
      integer( 4)  nfluxp, ifracs, iproc   !  help variables
      real   ( 8)                :: dtspro    ! fractional step dts
      integer(4)                 :: ipp_dts    ! pointer in default array to process specific dts
      integer(4)                 :: ipp_delt   ! pointer in default array to process specific delt
      INTEGER ISTEP, NOQ
      integer                    :: open_shared_library
      integer, save              :: ifirst = 1
      logical                    :: lfound
      integer                    :: idummy
      real                       :: rdummy
      integer                    :: ierror
      integer                    :: ierr2
      logical                    :: l_stop
      integer(4)                 :: iflx                            ! Loop counter over fluxes
      integer(4)                 :: iseg                            ! Loop counter over segments
      integer(4)                 :: nflux1                          ! Help variable for fluxes
      integer(4)                 :: ips                             ! Help variable for dump segments
      real(8)                    :: vol                             ! Help variable volume
      real(8)                    :: ndt                             ! Help variable time step multiplier
      real(8)                    :: atfac                           ! Help variable

      save    istep
      data    istep  / 0 /

      integer(4) ithndl /0/
      if ( timon ) call timstrt ( "wq_processes_proces", ithndl )

      IFRACS = 1

      IF ( nproc .eq. 0 ) goto 9999

!     Count calls of this module
!
      istep = istep + 1
!
      noq = noq1 + noq2 + noq3 + noq4

!     BLOOM fractional step (derivs assumed zero at entry)

      if ( ipbloo .gt. 0 ) then         !     Check presence of BLOOM module for this run
         ivar   = prvvar(ioffbl)
         iarr   = vararr(ivar)
         iv_idx = varidx(ivar)
         ip_arr = arrpoi(iarr)
         ipndt  = ip_arr + iv_idx - 1
         ndtblo = nint( a(ipndt) )  ! This picks up TimMultBl from BLOOM (without checking the name!)
         prondt(ipbloo) = ndtblo

!        This timestep fractional step ?

         if ( mod(istep-1,ndtblo) .eq. 0 ) then
            flux = 0.0

!           set dts and delt, bloom itself will multiply with prondt
            dtspro     = prondt(ipbloo)*dts
            ipp_dts    = nodef - 2*nproc + ipbloo
            ipp_delt   = nodef -   nproc + ipbloo
            defaul(ipp_dts)  = dts
            defaul(ipp_delt) = dts/float(itfact)

            call onepro_wqp (ipbloo , ioffbl , prvnio , prvtyp , prvvar , vararr , &
                             varidx , arrknd , arrpoi , arrdm1 , arrdm2 ,          &
                             noseg  , a      , ipmsa  , increm ,                   &
                             noflux , iflux  , promnr , flux   , iexpnt ,          &
                             iknmrk , noq1   , noq2   , noq3   , noq4   ,          &
                             pronam , dll_opb)

            if ( ipbloo .ne. nproc ) then
               nfluxp = iflux(ipbloo+1) - iflux(ipbloo)
            else
               nfluxp = noflux - iflux(ipbloo) + 1
            endif
            if ( nfluxp .gt. 0 ) then
!              Construct derivatives for these fluxes on this grid
               call wq_processes_derivatives ( deriv            , notot       , noflux , stochi      , iflux (ipbloo) , &
                                               nfluxp           , flux        , noseg  , volume      , prondt(ipbloo) )

!              For balances store FLXDMP
               if ( ibflag .gt. 0 ) then
                  ndt = prondt(ipbloo)*dts/86400.0
                  do iseg = 1 , noseg
                     if ( isdmp(iseg) .gt. 0 ) then
                        nflux1 = iflux (ipbloo)
                        vol = volume(iseg)
                        ips = isdmp(iseg)
                        if(ips.lt.1) cycle
                        do iflx = nflux1 , nflux1 + nfluxp - 1
                           if(flux(iflx,iseg).gt.0) then
                              flxdmp(1,iflx,ips) = flxdmp(1,iflx,ips) + flux(iflx,iseg)*vol*ndt
                           else
                              flxdmp(2,iflx,ips) = flxdmp(2,iflx,ips) - flux(iflx,iseg)*vol*ndt
                           endif
                        enddo
                     endif
                  enddo
               endif

            endif

            if ( istep .eq. 1 ) then
               deriv(:,:) = 0.0d0
               if ( ibflag .gt. 0 ) flxdmp = 0.0d0
            else
!              Scale fluxes and update "processes" accumulation arrays
               atfac = 1.0/real(itfact,8)
               do iseg = 1 , noseg
                  deriv (iseg,:) = deriv(iseg,:) * atfac
               enddo

               if (flux_int == 1) then
!                 let WAQ integrate the process fluxes
                  call wq_processes_integrate_fluxes ( conc   , amass  , deriv  , volume , dts     , &
                                                       nosys  , notot  , noseg  , surfac )
               endif
            endif
         endif
      endif

!     The processes fractional step
      flux = 0.0

      do iproc = 1,nproc
!        NOT bloom
         if ( iproc .ne. ipbloo ) then
!           Check fractional step
            if ( mod( istep-1, prondt(iproc) ) .eq. 0 ) then

               ! set dts and delt for this process in the default array
               ipp_dts    = nodef - 2*nproc + iproc
               ipp_delt   = nodef -   nproc + iproc
               dtspro    = prondt(iproc)*dts
               defaul(ipp_dts)  = dtspro
               defaul(ipp_delt) = dtspro/float(itfact)

               call onepro_wqp (iproc   , prvpnt(iproc), prvnio  , prvtyp  , prvvar  , vararr  , &
                                varidx  , arrknd       , arrpoi  , arrdm1  , arrdm2  ,           &
                                noseg   , a            , ipmsa   , increm  ,                     &
                                noflux  , iflux        , promnr  , flux    , iexpnt  ,           &
                                iknmrk  , noq1         , noq2    , noq3    , noq4    ,           &
                                pronam       , dll_opb )
            endif
         endif
      enddo

!     Now update the derivatives and the dumps of the fluxes from
!     all processes together outside of the parallel region
      call twopro_wqm ( nproc  , noflux , noseg  ,             &
                        notot  , ndmps  , dts    , iflux  ,    &
                        volume , deriv  , stochi , flux   ,    &
                        prondt , ibflag , isdmp  , flxdmp , ipbloo , istep  )

!     Calculate new velocities
      if (flux_int == 2) then
         if ( nveln  .gt. 0 .and. flux_int .ne. 1 ) then
            call wq_processes_velocities ( velonw , nveln  , ivpnew ,          &
                          velx   , nvelx  , vsto   , nosys  , &
                            noq    )
         endif
      endif

!     Set fractional step
      if ( noflux .gt. 0 .and. ifracs .eq. 1 ) then

         ! no fluxes at first step of fractional step

         if ( istep .eq. 1 ) then
            deriv(:,:) = 0.0d0
            if ( ibflag .gt. 0 ) flxdmp = 0.0d0
         else

!           Scale fluxes and update "processes" accumulation arrays
            atfac = 1.0/real(itfact,8)
            do iseg = 1 , noseg
               deriv (iseg,:) = deriv(iseg,:) * atfac
            enddo

            if (flux_int == 1) then
!              let WAQ integrate the process fluxes
               if ( nveln  .gt. 0 ) then
!                 Add effect of additional flow velocities
                  call wq_processes_integrate_velocities ( nosys    , notot    , noseg    , noq      , nveln    , &
                                                           velx     , area     , volume   , iexpnt   , iknmrk   , &
                                                           ivpnew   , conc     , dts      , deriv  )
               end if

!              Integration (derivs are zeroed)
               call wq_processes_integrate_fluxes ( conc   , amass  , deriv  , volume , dts     , &
                                                    nosys  , notot  , noseg  , surfac )
            endif
         endif
      endif

 9999 continue
      if (timon) call timstop( ithndl )
      return
 2000 format ( ' ERROR: undefined kind of array in PROCES :', i8 )
      end

      subroutine onepro_wqp ( iproc , k     , prvnio, prvtyp, prvvar, vararr, &
                              varidx, arrknd, arrpoi, arrdm1, arrdm2, noseg , &
                              a     , ipmsa , increm, noflux, iflux , promnr, &
                              flux  , iexpnt, iknmrk, noq1  , noq2  , noq3  , &
                              noq4  , pronam, dll_opb)

      use timers
      use iso_c_binding
      use process_registration

      integer             iproc , k, noseg , noflux, noq1  , noq2  , noq3  , noq4
      integer             prvnio(*)      , prvtyp(*)      , &
                          prvvar(*)      , vararr(*)      , &
                          varidx(*)      , arrknd(*)      , &
                          arrpoi(*)      , arrdm1(*)      , &
                          arrdm2(*)      ,                  &
                          ipmsa (*)      , increm(*)      , &
                          iflux (*)      , promnr(*)      , &
                          iexpnt(*)      , iknmrk(*)
      real                a(*)           , flux(*)
      character*10        pronam(*)
      integer(c_intptr_t)   , intent(in   ) :: dll_opb     ! open proces library dll handle
!
!     Local
!
      integer :: ityp
      integer :: ivario
      integer :: ivar
      integer :: iarr
      integer :: iv_idx
      integer :: iarknd
      integer :: ip_arr
      integer :: idim1
      integer :: idim2
      integer :: ipflux

      integer(4) ithndl /0/
      if ( timon ) call timstrt ( "onepro_wqp", ithndl )

!     Set the variables
      do ivario = 1 , prvnio(iproc)
         ityp   = prvtyp(k+ivario-1)
         ivar   = prvvar(k+ivario-1)
         iarr   = vararr(ivar)
         iv_idx = varidx(ivar)
         iarknd = arrknd(iarr)
         ip_arr = arrpoi(iarr)
         idim1  = arrdm1(iarr)
         idim2  = arrdm2(iarr)

!        Set pointer structure
         if ( iarknd .eq. 1 ) then
            ipmsa (k+ivario-1) = ip_arr + iv_idx - 1
            increm(k+ivario-1) = 0
         elseif ( iarknd .eq. 2 ) then
            ipmsa (k+ivario-1) = ip_arr + iv_idx - 1
            increm(k+ivario-1) = idim1
         elseif ( iarknd .eq. 3 ) then
            ipmsa (k+ivario-1) = ip_arr + (iv_idx-1)*idim1
            increm(k+ivario-1) = 1
         else
            write(*,*) 'Processes: type = ', iarknd, ivario, ' - ', pronam(iproc)
         endif
!
      enddo

!     compute fluxes
      ipflux = iflux(iproc)
      call procal (a        , promnr(iproc), flux(ipflux), ipmsa(k)      , increm(k)    , &
                   noseg    , noflux       , iexpnt      , iknmrk(1)     , noq1         , &
                   noq2     , noq3         , noq4        , pronam(iproc) , prvnio(iproc), &
                   prvtyp(k), iproc        , dll_opb     )

      if (timon) call timstop( ithndl )
      return
      end

      subroutine twopro_wqm (nproc  , noflux , noseg  ,          &
                             notot  , ndmps  , dts    , iflux  , &
                             volume , deriv  , stochi , flux   , &
                             prondt , ibflag , isdmp  , flxdmp , ipbloo , istep  )

      use timers

      implicit none

!     Arguments           :

!     Kind        Function         Name   Dimensions                 Description
      integer(4), intent(in   ) :: nproc                           ! Total number of processes
      integer(4), intent(in   ) :: noflux                          ! Total number of fluxes
      integer(4), intent(in   ) :: noseg                           ! Total number of computational volumes
      integer(4), intent(in   ) :: notot                           ! Total number of substances
      integer(4), intent(in   ) :: ndmps                           ! Total number of mass balance areas
      real   (8), intent(in   ) :: dts
      integer(4), intent(in   ) :: iflux (nproc )                  ! Offset in the flux array per process
      real   (8), intent(in   ) :: volume(noseg )                  ! Computational volumes
      real   (8), intent(inout) :: deriv (noseg , notot )          ! Array with derivatives
      real   (4), intent(in   ) :: stochi(notot , noflux )         ! Stoichiometric factors per flux
      real   (4), intent(in   ) :: flux  (noflux, noseg )          ! Process fluxes
      integer(4), intent(in   ) :: prondt(nproc )                  ! Time step size of the process
      integer(4), intent(in   ) :: ibflag                          ! If > 0 then balances are required
      integer(4), intent(in   ) :: isdmp (noseg )                  ! Segment to dumped segment pointer
      real   (8), intent(inout) :: flxdmp(2,noflux, ndmps  )       ! Dumped fluxes
      integer(4), intent(in   ) :: ipbloo                          ! The BLOOM  process if any
      integer(4), intent(in   ) :: istep                           ! Time step nr.

!     Local
      integer(4)                :: iproc                           ! Loop counter over processes
      integer(4)                :: iflx                            ! Loop counter over fluxes
      integer(4)                :: iseg                            ! Loop counter over segments
      integer(4)                :: nflux1                          ! Help variable for fluxes
      integer(4)                :: ips                             ! Help variable for dump segments
      integer(4)                :: nfluxp                          ! Number of fluxes in this process
      real(8)                   :: vol                             ! Help variable volume
      real(8)                   :: ndt                             ! Help variable time step multiplier

      integer(4) ithndl /0/
      if ( timon ) call timstrt ( "twopro_wqm", ithndl )

      do iproc = 1, nproc
         if ( iproc .eq. ipbloo ) cycle
         if ( mod( istep-1, prondt(iproc) ) .ne. 0 ) cycle

!        See if this process produces fluxes
         if ( iproc .ne. nproc ) then
            nfluxp = iflux(iproc+1) - iflux(iproc)
         else
            nfluxp = noflux - iflux(iproc) + 1
         endif
         if ( nfluxp .eq. 0 ) cycle

!        Construct derivatives from these fluxes on this grid
         call wq_processes_derivatives( deriv           , notot          , noflux , stochi         , iflux (iproc), &
                                        nfluxp          , flux           , noseg  , volume         , prondt(iproc))

!        For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'
         ndt = prondt(iproc)*real(dts)/86400.0
         if ( ibflag .gt. 0 ) then
            do iseg = 1 , noseg
               if ( isdmp(iseg) .gt. 0 ) then
                  nflux1 = iflux (iproc)
                  vol = volume(iseg)
                  ips = isdmp(iseg)
                  if(ips.lt.1) cycle
                  do iflx = nflux1 , nflux1 + nfluxp - 1
                     if(flux(iflx,iseg).gt.0) then
                        flxdmp(1,iflx,ips) = flxdmp(1,iflx,ips) + flux(iflx,iseg)*vol*ndt
                     else
                        flxdmp(2,iflx,ips) = flxdmp(2,iflx,ips) - flux(iflx,iseg)*vol*ndt
                     endif
                  enddo
               endif
            enddo
         endif

      enddo

      if (timon) call timstop( ithndl )
      return
      end
