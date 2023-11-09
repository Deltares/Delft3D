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
      module m_dlwq15
      use m_wascal


      implicit none

      contains


      subroutine dlwq15 ( nosys   , notot   , noseg   , noq     , nowst   ,
     &                    nowtyp  , ndmps   , intopt  , idt     , itime   ,
     &                    iaflag  , syname  , conc    , volume  , vol2    ,
     &                    flow    , ipoint  , wastid  , wstnam  , wsttyp  ,
     &                    iwtype  , iwaste  , iwstkind, waste   , deriv   ,
     &                    iknmrk  , nopa    , paname  , param   , nosfun  ,
     &                    sfname  , segfun  , isdmp   , dmps    , amass2  ,
     &                    wstdmp  , isys    , nsys     )

!     Deltares Software Centre

!> \file
!>                          Adds waste loads to the derivative vector DERIV
!>
!>                          This routine identifies which wasteflows are -999.0 (missing value).
!>                          Those flows are replaces by the detected closure error / nr of
!>                          wasteflows it concerns. This is an inherent weakness of the approach
!>                          that all get the same value.\n
!>                          The user wasteload dll is called. The user may insert dynamic wasteload processing.\n
!>                          The normal processing takes place if iwstkind (see below) is zero:
!>                             - if wasteflow .gt. 0 then load is wasteflow * prescribed value
!>                             - if wasteflow .eq. 0 then load is prescribed value
!>                             - if wasteflow .lt. 0 then
!>                                   - withdrawal is wasteflow * model-conc if prescribed value eq 0.0\n
!>                                   - withdrawal is wasteflow * prescribed value if that ne 0.0\n
!>                             .
!>                          Normal procedures apply for the update of balances.\n
!>                          New is the steering of the wasteload processing with iwstkind:
!>                             - 0 = PRES : above described situation
!>                             - 1 = MASS : values are always used as mass also if a flow exist
!>                             - 2 = CONC : values are always used as concentration also if flow = 0
!>                             - 3 = RAIN : concentrations if flow > 0 and zero if flow < 0 (evaporation)
!>                             - 4 = WELL : concentrations if flow > 0 and model conc's if flow < 0 (to groundwater)
!>                          New also are the following location numbers:
!>                             - -1 = a load/withdrawal over the whole water surface. Values are /m2.
!>                             - -2 = a load/withdrawal along the bank lengthe of a volume. Values are /m.
!>                             - -3 = a load/withdrawal over the whole water bed. Values are /m2.
!>                             - positive flow is always directed towards the model (so influx)
!>                             - negative flow is aways directed out of the model (so outflux)
!>                             - surface and bed loads require the presence of a parameter SURF
!>                             - bank loads require the presence of a parameter LENGTH


!     Function            : Adds the wasteloads to DERIV.

!     Logical units       : none

!     Subroutines called  : wascal : the user specified wasteload dll

      use m_zoek
      use m_srstop
      use m_evaluate_waq_attribute
      use timers
      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of volumes
      integer   (4), intent(in   ) :: noq                     !< number of flows
      integer   (4), intent(in   ) :: nowst                   !< number of wastes
      integer   (4), intent(in   ) :: nowtyp                  !< number of waste types
      integer   (4), intent(in   ) :: ndmps                   !< number of dumped volumes for balances
      integer   (4), intent(in   ) :: intopt                  !< integration suboptions
      integer   (4), intent(in   ) :: idt                     !< integration time step size
      integer   (4), intent(in   ) :: itime                   !< current time
      integer   (4), intent(in   ) :: iaflag                  !< if 1 then accumulation of balances
      character(20), intent(in   ) :: syname(notot  )         !< names of the substances
      real      (4), intent(in   ) :: conc  (notot  ,noseg)   !< concentrations for withdrawals
      real      (4), intent(in   ) :: volume(noseg  )         !< volumes at start of time step
      real      (4), intent(in   ) :: vol2  (noseg  )         !< volumes at end   of time step
      real      (4), intent(in   ) :: flow  (noq    )         !< flows between comp. volumes
      integer   (4), intent(in   ) :: ipoint(4,noq  )         !< from-to pointer
      character(20), intent(in   ) :: wastid(nowst  )         !< IDs   of the wasteloads
      character(40), intent(in   ) :: wstnam(nowst  )         !< names of the wasteloads
      character(40), intent(in   ) :: wsttyp(nowst  )         !< types of the wasteloads
      integer   (4), intent(in   ) :: iwtype(nowst  )         !< type numbers of the wasteloads
      integer   (4), intent(in   ) :: iwaste(nowst  )         !< volume numbers of the waste locations
      integer   (4), intent(in   ) :: iwstkind(nowst)         !< treatment of the flow-conc combination
      real      (4), intent(inout) :: waste (0:notot,nowst)   !< waste masses/concs per system clock
                                                              !< zero-th element is 'flow'
      real      (4), intent(inout) :: deriv (notot  ,noseg)   !< derivatives to be updated
      integer   (4), intent(in   ) :: iknmrk(noseg  )         !< feature array
      integer   (4), intent(in   ) :: nopa                    !< nr of parameters
      character(20), intent(in   ) :: paname(nopa   )         !< names of the parameters
      real      (4), intent(in   ) :: param (nopa   ,noseg)   !< parameter values
      integer   (4), intent(in   ) :: nosfun                  !< nr of segment functions
      character(20), intent(in   ) :: sfname(nosfun )         !< names of the segment functions
      real      (4), intent(in   ) :: segfun(noseg  ,nosfun)  !< segment function values
      integer   (4), intent(in   ) :: isdmp (noseg  )         !< volume to dump-location pointer
      real      (4), intent(inout) :: dmps  (notot  ,ndmps,*) !< dumped segment fluxes if INTOPT > 7
      real      (4), intent(inout) :: amass2(notot  , 5 )     !< mass balance array
      real      (4), intent(inout) :: wstdmp(notot,nowst,2)   !< accumulated wasteloads 1/2 in and out
      integer   (4), intent(in   ) :: isys                    !< first substance in array
      integer   (4), intent(in   ) :: nsys                    !< number of substances  to deal with

!     local simple variables

      logical          fluxes     ! set .true. if intopt > 7
      logical          massbal    ! set .true. if iaflag eq 1
      logical          surfbed    ! set true is surface or bed effects are wanted
      integer          nrdetec    ! number of wasteloads detected for missing flow value
      integer          i, i1      ! loop counter
      integer          iwst       ! wasteload location volume nr.
      integer          ip         ! help variable for 'from' and 'to' volume nr
      integer          ipb        ! help variable for balances
      real             wasteflow  ! help variable for the flow of 'this' waste
      integer          ierr_alloc ! error indicator for allocations
      real             ahelp      ! help variable for waste mass detection
      integer          indx       ! index in parameter or segment function arrays
      integer          istrt      ! loop bounds
      integer          istop      ! loop bounds
      integer          ikmrk2     ! second feature (second decimal in feature array)
      integer          icel       ! loop counter computational volumes

!     Wasteflow detection items

      type DetectStruct
         integer  :: icell               ! number of the comp. volume
         integer  :: iwast               ! wasteload reference nr
         real     :: flow                ! the associated flow
      end type DetectStruct

      integer           ,              save :: MaxDetec               ! maximum number of detection items
      type(DetectStruct), pointer    , save :: IDetec (:), Itemp  (:)
      integer           , allocatable, save :: IBpoint(:), IWpoint(:)
      real              , allocatable, save :: wflow  (:), surf   (:), length(:)
      data     MaxDetec / 0 /

      integer(4) ithandl
      data       ithandl /0/
      if ( timon ) call timstrt ( "dlwq15", ithandl )

!          No wasteloads, then no glory

      if ( nowst .eq. 0 ) goto 9999

!     in steady state mode this subroutine is called substance by substance
!     preparations only in the call for the first substance

      if ( isys .eq. 1 ) then

!          Create and dimension a backpointering array for load detection
!          Dimension a work array for surface and bottom loads

         if ( .not. allocated(ibpoint) ) then
            allocate( IBpoint(noseg), stat=ierr_alloc )
            allocate( IWpoint(nowst), stat=ierr_alloc )
            allocate( wflow  (noseg), stat=ierr_alloc )
            allocate( surf   (noseg), stat=ierr_alloc )
            allocate( length (noseg), stat=ierr_alloc )
            if ( ierr_alloc .ne. 0 ) then
               write(*,*) 'ERROR: allocating work array in DLWQ15'
               write(*,*) 'ierr_alloc :',ierr_alloc
               write(*,*) 'noseg,nowst:',noseg,nowst
               call SRSTOP(1)
            endif
         endif
         IBpoint = 0
         IWpoint = 0

!          Detect all locations with missing wasteflow

         NrDetec = 0
         surfbed = .false.
         do i = 1 , nowst
            iwst = iwaste(i)
            if ( iwst .lt. 0 ) then
               surfbed = .true.
               cycle
            endif
            if ( abs(waste(0,i)+999.0) .lt. 0.001 ) then      ! flow not known yet
               NrDetec = NrDetec + 1                          ! fill array with cells
               if ( NrDetec .gt. MaxDetec ) then              ! to detect flow in
                  allocate ( Itemp(MaxDetec+10) )             ! resize if needed
                  do i1 = 1, MaxDetec
                     Itemp(i1)%icell = IDetec(i1)%icell
                     Itemp(i1)%iwast = IDetec(i1)%iwast
                     Itemp(i1)%flow  = IDetec(i1)%flow
                  enddo
                  MaxDetec = MaxDetec+10
                  if ( associated(IDetec) ) deallocate ( IDetec )
                  IDetec => Itemp                             ! end of resizing operation
               endif
               IDetec(NrDetec)%icell = iwst
               IDetec(NrDetec)%iwast = i
               IDetec(NrDetec)%flow  = (vol2(iwst) - volume(iwst))/idt
               IBpoint(iwst)         = NrDetec                ! backpointering
               IWpoint( i )          = IWpoint( i ) + 1       ! multiple loads
            endif
         enddo

!        Make wasteflows if missings have been detected

         if ( NrDetec .gt. 0 ) then
            do i = 1 , noq
               ip = ipoint(1,i)
               if ( ip .gt. 0 ) then
                  if ( IBpoint(ip) .gt. 0 ) then    ! this is a cell with unknown loads
                     do i1 = 1 , NrDetec            ! accumulate in all unknown loads in this cell
                        if ( IDetec(i1)%icell .eq. ip ) IDetec(i1)%flow = IDetec(i1)%flow + Flow(i)
                     enddo
                  endif
               endif
               ip = ipoint(2,i)                    ! same for outflow
               if ( ip .gt. 0 ) then
                  if ( IBpoint(ip) .gt. 0 ) then
                     do i1 = 1 , NrDetec
                        if ( IDetec(i1)%icell .eq. ip ) IDetec(i1)%flow = IDetec(i1)%flow - Flow(i)
                     enddo
                  endif
               endif
            enddo
!           And insert them where needed
            do i = 1 , NrDetec
               iwst = IDetec(i)%iwast
               waste(0,iwst) = IDetec(i)%flow/IWpoint(iwst) ! divide by nr of loads in this cell
            enddo
         endif

      ! call the user wasteload dll

         call wascal ( nowst  , notot  , nosys  , noseg  , syname ,
     &                 conc   , itime  , nowtyp , wastid , wstnam ,
     &                 wsttyp , iwaste , iwtype , waste  )

!         Process for all waste locations

         if ( surfbed ) then
            surf   = 0.0
            length = 0.0
            call zoek20 ( 'SURF      ', nopa  , paname, 10, indx )
            if ( indx .gt. 0 ) then
               surf = param(indx,:)
            else
               call zoek20 ( 'SURF      ', nosfun, sfname, 10, indx )
               if ( indx .gt. 0 ) then
                  surf = segfun(:,indx)
               endif
            endif
            call zoek20 ( 'LENGTH    ', nopa  , paname, 10, indx )
            if ( indx .gt. 0 ) then
               length = param(indx,:)
            else
               call zoek20 ( 'LENGTH    ', nosfun, sfname, 10, indx )
               if ( indx .gt. 0 ) then
                  length = segfun(:,indx)
               endif
            endif
         endif

!         end of preparations for first substance call only

      endif

!         Normal processing
!         accumulation?

      massbal = iaflag .eq. 1
      fluxes  = btest(intopt,3)

      do 10 i = 1 , nowst

        iwst = iwaste(i)

        select case ( iwstkind(i) )

           case ( 0 )                               ! old situation
              if ( iwst .le. 0 ) cycle

              WasteFlow = waste(0,i)
              ipb  = isdmp(iwst)
              if ( abs(WasteFlow) .le. 1.0E-30 ) then       ! just load of mass
                 do i1 = isys,isys+nsys-1
                    deriv (i1,iwst) = deriv (i1,iwst) + waste(i1,i)
                    if ( massbal ) amass2(i1, 3  ) = amass2(i1, 3  ) + waste(i1,i)*idt
                    if ( ipb .gt. 0 .and. fluxes ) then
                       if ( waste(i1,i) .lt. 0.0 ) then     ! withdrawal
                          dmps(i1,ipb,3) = dmps(i1,ipb,3) - waste(i1,i)*idt
                          wstdmp(i1,i,2) = wstdmp(i1,i,2) - waste(i1,i)*idt
                       else                                 ! load
                          dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*idt
                          wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*idt
                       endif
                    endif
                 enddo
              endif

              if ( WasteFlow .gt.  1.0E-30 ) then           ! mass = flow * conc
                 do i1 = isys,isys+nsys-1
                    deriv (i1,iwst) = deriv (i1,iwst) + waste(i1,i)*WasteFlow
                    if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + waste(i1,i)*WasteFlow*idt
                    if ( ipb .gt. 0  .and. fluxes )
     &                                dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*WasteFlow*idt
                    if ( fluxes     ) wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*WasteFlow*idt
                 enddo
              endif

              if ( WasteFlow .LT. -1.0E-30 ) then           ! withdrawal
                 do i1 = isys,isys+nsys-1
                    ahelp = 0.0
                    if ( abs(waste(i1,i)) .lt. 1.0E-30 ) then      !  with model concentration
                       if ( i1 .le. nosys ) ahelp = conc(i1,iwst)*WasteFlow  ! transported substances
                    else                                           !  with prescribed concentration
                       ahelp = waste(i1,i)*WasteFlow
                    endif
                    deriv (i1,iwst) = deriv (i1,iwst) + ahelp
                    if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + ahelp*idt
                    if ( ipb .gt. 0  .and. fluxes )
     &                                dmps(i1,ipb,3) = dmps(i1,ipb,3) - ahelp*idt
                    if ( fluxes     ) wstdmp(i1,i,2) = wstdmp(i1,i,2) - ahelp*idt
                 enddo
              endif

           case ( 1 )         ! always MASS
              wflow = 0.0
              if ( iwst .gt. 0 ) then               ! normal processing
                 wflow(iwst) = 1.0
                 istrt = iwst
                 istop = iwst
              else                                  ! surface or bed processing
                 do i1 = 1, noseg
                    if ( btest( iknmrk(i1), 0 ) ) then
                       select case ( iwst )
                          case ( -1 )               ! surface processing
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 0 .or. ikmrk2 .eq. 1 ) wflow(i1) = surf(i1)
                          case ( -2 )               ! bank processing
                             wflow(i1) = length(i1)
                          case ( -3 )               ! bed processing
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 3 .or. ikmrk2 .eq. 0 ) wflow(i1) = surf(i1)
                       end select
                    endif
                 enddo
                 istrt = 1
                 istop = noseg
              endif
              do icel = istrt,istop
                 WasteFlow = wflow(icel)
                 ipb = isdmp(icel)
                 do i1 = isys,isys+nsys-1
                    deriv (i1,icel) = deriv (i1,icel) + waste(i1,i)*WasteFlow
                    if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + waste(i1,i)*WasteFlow*idt
                    if (  fluxes    ) then
                       if ( waste(i1,i) .lt. 0.0 ) then     ! withdrawal
                          wstdmp(i1,i,1) = wstdmp(i1,i,2) - waste(i1,2)*WasteFlow*idt
                       else                                 ! load
                          wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*WasteFlow*idt
                       endif
                    endif
                    if ( ipb .gt. 0  .and. fluxes ) then
                       if ( waste(i1,i) .lt. 0.0 ) then     ! withdrawal
                          dmps(i1,ipb,3) = dmps(i1,ipb,3) - waste(i1,i)*WasteFlow*idt
                       else                                 ! load
                          dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*WasteFlow*idt
                       endif
                    endif
                 enddo
              enddo

           case ( 2 )         ! always CONC
              wflow = 0.0
              if ( iwst .gt. 0 ) then               ! normal processing
                 wflow(iwst) = waste(0,i)
                 istrt = iwst
                 istop = iwst
              else                                  ! surface or bed processing
                 do i1 = 1, noseg
                    if ( btest( iknmrk(i1), 0 )) then
                       select case ( iwst )
                          case ( -1 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 0 .or. ikmrk2 .eq. 1 ) wflow(i1) = waste(0,i)*surf(i1)
                          case ( -2 )
                             wflow(i1) = waste(0,i)*length(i1)
                          case ( -3 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 3 .or. ikmrk2 .eq. 0 ) wflow(i1) = waste(0,i)*surf(i1)
                       end select
                    endif
                 enddo
                 istrt = 1
                 istop = noseg
              endif
              do icel = istrt,istop
                 WasteFlow = wflow(icel)
                 ipb = isdmp(icel)
                 do i1 = isys,isys+nsys-1
                    deriv (i1,icel) = deriv (i1,icel) + waste(i1,i)*WasteFlow
                    if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + waste(i1,i)*WasteFlow*idt
                    if (  fluxes    ) then
                       if ( waste(i1,i)*WasteFlow .lt. 0.0 ) then     ! withdrawal
                          wstdmp(i1,i,1) = wstdmp(i1,i,2) - waste(i1,2)*WasteFlow*idt
                       else                                 ! load
                          wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*WasteFlow*idt
                       endif
                    endif
                    if ( ipb .gt. 0 .and. fluxes ) then
                       if ( waste(i1,i)*WasteFlow .lt. 0.0 ) then     ! withdrawal
                          dmps(i1,ipb,3) = dmps(i1,ipb,3) - waste(i1,i)*WasteFlow*idt
                       else                                 ! load
                          dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*WasteFlow*idt
                       endif
                    endif
                 enddo
              enddo

           case ( 3 )         ! RAIN ( load is CONC, withdrawal is zero )
              wflow = 0.0
              if ( iwst .gt. 0 ) then               ! normal processing
                 wflow(iwst) = waste(0,i)
                 istrt = iwst
                 istop = iwst
              else                                  ! surface or bed processing
                 do i1 = 1, noseg
                    if ( btest( iknmrk(i1), 0 )  ) then
                       select case ( iwst )
                          case ( -1 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 0 .or. ikmrk2 .eq. 1 ) wflow(i1) = waste(0,i)*surf(i1)
                          case ( -2 )
                             wflow(i1) = waste(0,i)*length(i1)
                          case ( -3 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 3 .or. ikmrk2 .eq. 0 ) wflow(i1) = waste(0,i)*surf(i1)
                       end select
                    endif
                 enddo
                 istrt = 1
                 istop = noseg
              endif
              do icel = istrt,istop
                 WasteFlow = wflow(icel)
                 ipb = isdmp(icel)
                 if ( WasteFlow .gt.  1.0E-30 ) then           ! mass = flow * conc
                    do i1 = isys,isys+nsys-1
                       deriv (i1,icel) = deriv (i1,icel) + waste(i1,i)*WasteFlow
                       if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + waste(i1,i)*WasteFlow*idt
                       if ( ipb .gt. 0 .and. fluxes )
     &                                   dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*WasteFlow*idt
                       if ( fluxes     ) wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*WasteFlow*idt
                    enddo
                 endif
              enddo

           case ( 4 )         ! WELL ( load is CONC, withdrawal is with model conc
              wflow = 0.0
              if ( iwst .gt. 0 ) then               ! normal processing
                 wflow(iwst) = waste(0,i)
                 istrt = iwst
                 istop = iwst
              else                                  ! surface or bed processing
                 do i1 = 1, noseg
                    if ( btest( iknmrk(i1), 0 ) ) then
                       select case ( iwst )
                          case ( -1 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 0 .or. ikmrk2 .eq. 1 ) wflow(i1) = waste(0,i)*surf(i1)
                          case ( -2 )
                             wflow(i1) = waste(0,i)*length(i1)
                          case ( -3 )
                             call evaluate_waq_attribute( 2, iknmrk(i1), ikmrk2 )
                             if ( ikmrk2 .eq. 3 .or. ikmrk2 .eq. 0 ) wflow(i1) = waste(0,i)*surf(i1)
                       end select
                    endif
                 enddo
                 istrt = 1
                 istop = noseg
              endif
              do icel = istrt,istop
                 WasteFlow = wflow(icel)
                 ipb = isdmp(icel)
                 if ( WasteFlow .gt.  1.0E-30 ) then           ! mass = flow * conc
                    do i1 = isys,isys+nsys-1
                       deriv (i1,icel) = deriv (i1,icel) + waste(i1,i)*WasteFlow
                       if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + waste(i1,i)*WasteFlow*idt
                       if ( ipb .gt. 0 .and. fluxes )
     &                                   dmps(i1,ipb,2) = dmps(i1,ipb,2) + waste(i1,i)*WasteFlow*idt
                       if ( fluxes     ) wstdmp(i1,i,1) = wstdmp(i1,i,1) + waste(i1,i)*WasteFlow*idt
                    enddo
                 else
                    do i1 = isys,isys+nsys-1
                       deriv (i1,iwst) = deriv (i1,iwst) + conc(i1,iwst)*WasteFlow
                       if (  massbal   ) amass2(i1,  3) = amass2(i1,  3) + conc(i1,iwst)*WasteFlow*idt
                       if ( ipb .gt. 0 .and. fluxes )
     &                                   dmps(i1,ipb,3) = dmps(i1,ipb,3) - conc(i1,iwst)*WasteFlow*idt
                       if ( fluxes     ) wstdmp(i1,i,2) = wstdmp(i1,i,2) - conc(i1,iwst)*WasteFlow*idt
                    enddo
                 endif
              enddo

        end select

   10 continue

 9999 if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlwq15
