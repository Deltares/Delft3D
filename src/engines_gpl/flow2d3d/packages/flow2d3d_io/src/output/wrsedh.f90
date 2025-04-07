subroutine wrsedh(lundia    ,error     ,filename  ,ithisc    ,ntruv     , &
                & nostat    ,kmax      ,lsed      ,lsedtot   ,zmodel    , &
                & zws       ,zrsdeq    ,zbdsed    ,zdpsed    ,zdps      , &
                & zsbu      ,zsbv      ,zssu      ,zssv      ,sbtr      , &
                & sstr      ,sbtrc     ,sstrc     ,zrca      ,zsourse   , &
                & zsinkse   ,zfrac     ,zmudfrac  ,zsandfrac ,zfixfac   , &
                & ztaub     ,zhidexp   ,zseddif   , &
                & irequest  ,fds       ,nostatto  ,nostatgl  ,order_sta , &
                & ntruvto   ,ntruvgl   ,order_tra ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for sediment to the Delft3D-FLOW
!              TRIH file. Output is performed conform the times of history
!              file. The routine is called if LSEDTOT > 0 or in case of
!              subsidence and uplift (to store time varying bed level), so we
!              still have to check for lsedtot > 0.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use dfparall, only: inode, master
    use wrtarray, only: wrtvar, wrtarray_n, station, transec
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    integer                              , pointer :: io_prec
    integer       , dimension(:)         , pointer :: shlay
    real(hp)                             , pointer :: morft
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)      , dimension(:)         , pointer :: rhosol
    real(fp)      , dimension(:)         , pointer :: cdryb
    type (moroutputtype)                 , pointer :: moroutput
    type (datagroup)                     , pointer :: group4
    type (datagroup)                     , pointer :: group5
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest  !< REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc    !< current time counter for the history data file
    integer                                                             , intent(in)  :: kmax      !< number of layers in water column
    integer                                                             , intent(in)  :: lsed      !< number of suspended sediment fractions
    integer                                                             , intent(in)  :: lsedtot   !< total number of sediment fractions
    integer                                                             , intent(in)  :: lundia    !< unit number of diagnostic output file
    integer                                                             , intent(in)  :: nostat    !< number of stations
    integer                                                             , intent(in)  :: ntruv     !< number of cross-sections
    logical                                                             , intent(in)  :: zmodel    !< flag indicating z-model
    logical                                                             , intent(out) :: error     !< flag indicating error while writing output
    real(fp), dimension(nostat)                                         , intent(in)  :: zdps      !< bed level at station
    real(fp), dimension(nostat)                                         , intent(in)  :: zdpsed    !< thickness of sediment layer at station
    real(fp), dimension(nostat, 0:kmax, lsed)                           , intent(in)  :: zws       !< settling velocity profile at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zrsdeq    !< equilibrium concentration at station (2D only)
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zbdsed    !< composition of sediment layer at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zrca      !< near-bed reference concentration at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zsourse   !< suspended sediment sourse term at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zsinkse   !< suspended sediment sink term at station
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zfrac     !< sediment fraction in top layer at station
    real(fp), dimension(nostat)                                         , intent(in)  :: zmudfrac  !< total mud fraction in top layer at station
    real(fp), dimension(nostat)                                         , intent(in)  :: zsandfrac !< total sand fraction in top layer at station
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zfixfac   !< reduction factor due to limited sediment availability at station
    real(fp), dimension(nostat)                                         , intent(in)  :: ztaub     !< bed shear stress used in morphology at station
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zhidexp   !< hiding and exposure factor at station
    real(fp), dimension(nostat, 0:kmax, lsed)                           , intent(in)  :: zseddif   !< vertical sediment diffusion profile at station
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zsbu      !< instataneous bed load (due to currents and waves) plus suspended transport due to waves in M direction at station
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zsbv      !< instataneous bed load (due to currents and waves) plus suspended transport due to waves in N direction at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zssu      !< instataneous suspended load transport due to currents in M direction at station
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zssv      !< instataneous suspended load transport due to currents in N direction at station
    real(fp), dimension(ntruv, lsedtot)                                 , intent(in)  :: sbtr      !< instataneous bed load (due to currents and waves) plus suspended transport due to waves through cross-section
    real(fp), dimension(ntruv, lsedtot)                                 , intent(in)  :: sbtrc     !< cumulative bed load (due to currents and waves) plus suspended transport due to waves through cross-section
    real(fp), dimension(ntruv, lsed)                                    , intent(in)  :: sstr      !< instataneous suspended load transport due to currents through cross-section
    real(fp), dimension(ntruv, lsed)                                    , intent(in)  :: sstrc     !< cumulative suspended load transport due to currents through cross-section
    character(*)                                                        , intent(in)  :: filename  !< file name
    integer                                                             , intent(in)  :: fds       !< file handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: nostatgl  !< global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: nostatto  !< total number of stations (including "duplicate" stations located in halo regions)
    integer       , dimension(nostat)                                   , intent(in)  :: order_sta !< order of stations in input configuration
    integer                                                             , intent(in)  :: ntruvgl   !< global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: ntruvto   !< total number of tracks (including "duplicate" stations located in halo regions)
    integer       , dimension(ntruv)                                    , intent(in)  :: order_tra !< order of cross-sections in input configuration
!
! Local variables
!
    integer                                           :: filetype
    real(fp)        , dimension(:,:)  , allocatable   :: rbuff2
    real(fp)        , dimension(:,:,:), allocatable   :: rbuff3
    real(fp)                                          :: rhol
    integer                                           :: ierror         ! Local error flag
    integer                                           :: istat
    integer                                           :: k
    integer                                           :: kmaxout        ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                           :: l
    integer                                           :: n
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_nostat
    integer                                           :: iddim_ntruv
    integer                                           :: iddim_lsed
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_kmax
    integer                                           :: iddim_kmaxout
    !
    integer                                           :: idatt_sta
    integer                                           :: idatt_tra
    !
    character(10)                                     :: transpcrsunit
    character(16)                                     :: grnam4
    character(16)                                     :: grnam5
!
! Data statements
!
    data grnam4/'his-infsed-serie'/
    data grnam5/'his-sed-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_HIS, grnam4, group4)
    call getdatagroup(gdp, FILOUT_HIS, grnam5, group5)
    celidt     => group4%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    io_prec     => gdp%gdpostpr%io_prec
    shlay       => gdp%gdpostpr%shlay
    morft       => gdp%gdmorpar%morft
    morfac      => gdp%gdmorpar%morfac
    sus         => gdp%gdmorpar%sus
    bed         => gdp%gdmorpar%bed
    rhosol      => gdp%gdsedpar%rhosol
    cdryb       => gdp%gdsedpar%cdryb
    moroutput   => gdp%gdmorpar%moroutput
    !
    kmaxout = size(shlay)
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element characteristics
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time', nf90_unlimited)
       iddim_nostat  = adddim(gdp, lundia, FILOUT_HIS, 'NOSTAT', nostatgl)
       iddim_ntruv   = adddim(gdp, lundia, FILOUT_HIS, 'NTRUV', ntruvgl)
       if (lsed>0) then
          iddim_lsed    = adddim(gdp, lundia, FILOUT_HIS, 'LSED', lsed)
       else
          iddim_lsed    = -1
       endif
       if (lsedtot>0) then
          iddim_lsedtot = adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT', lsedtot)
       else
          iddim_lsedtot = -1
       endif
       if (zmodel) then
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'K_LYR'  , kmax) ! Number of layers
       else
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'SIG_LYR'  , kmax) ! Number of layers
       endif
       iddim_kmaxout = adddim(gdp, lundia, FILOUT_HIS, 'KMAXOUT', kmaxout)
       !
       idatt_sta = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMST XSTAT YSTAT')
       idatt_tra = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMTRA')
       !
       !
       ! his-infsed-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'ITHISS', ' ', IO_INT4       , 0, longname='timestep number (ITHISS*DT*TUNIT := time in sec from ITDATE)')
       endif
       if (lsedtot > 0) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'MORFAC', ' ', io_prec       , 0, longname='morphological acceleration factor (MORFAC)')
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'MORFT', ' ', IO_REAL8       , 0, longname='morphological time (days since start of simulation)', unit='days')
       endif
       !
       ! his-sed-series: stations
       !
       if (nostat > 0) then
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZWS', ' ', io_prec      , 3, dimids=(/iddim_nostat, iddim_kmaxout, iddim_lsed/), longname='Settling velocity in station', unit='m/s', attribs=(/idatt_sta/) )
           if (moroutput%seddif) then
              call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSEDDIF', ' ', io_prec      , 3, dimids=(/iddim_nostat, iddim_kmaxout, iddim_lsed/), longname='Vertical sediment diffusion (zeta point)', unit='m2/s', attribs=(/idatt_sta/) )
           endif
           if (kmax == 1) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZRSDEQ', ' ', io_prec , 3, dimids=(/iddim_nostat, iddim_kmax, iddim_lsed/), longname='Equilibrium concentration of sediment at station (2D only)', unit='kg/m3', attribs=(/idatt_sta/) )
           endif
         endif
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZBDSED', ' ', io_prec     , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Available mass of sediment at bed at station', unit='kg/m2', attribs=(/idatt_sta/) )
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDPSED', ' ', io_prec     , 1, dimids=(/iddim_nostat/), longname='Sediment thickness at bed at station (zeta point)', unit='m', attribs=(/idatt_sta/) )
            !
            ! nlyr is unknown in esm_alloc_real and therefore we can't follow the same method as for the other station quantities using an old fashioned esm/fsm array
            !call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZMSED', ' ', io_prec      , 3, dimids=(/iddim_nostat, iddim_nlyr, iddim_lsedtot/), longname='Mass of sediment in layer at station', unit='kg/m2', attribs=(/idatt_sta/) )
            !call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZLYRFRAC', ' ', io_prec   , 3, dimids=(/iddim_nostat, iddim_nlyr, iddim_lsedtot/), longname='Volume fraction of sediment in layer at station', attribs=(/idatt_sta/) )
            !call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDP_BEDLYR', ' ', io_prec , 2, dimids=(/iddim_nostat, iddim_nlyrp1/), longname='Vertical position of sediment layer interface at station', units='m', attribs=(/idatt_sta/) )
            !call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZEPSPOR', ' ', io_prec    , 2, dimids=(/iddim_nostat, iddim_nlyr/), longname='Porosity coefficient in layer at station', attribs=(/idatt_sta/) )
         endif
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDPS', ' ', io_prec       , 1, dimids=(/iddim_nostat/), longname='Morphological depth at station (zeta point)', unit='m', attribs=(/idatt_sta/) )
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSBU', ' ', io_prec       , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Bed load transport in u-direction at station (zeta point)', unit=moroutput%unit_transport_rate, attribs=(/idatt_sta/) )
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSBV', ' ', io_prec       , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Bed load transport in v-direction at station (zeta point)', unit=moroutput%unit_transport_rate, attribs=(/idatt_sta/) )
         endif
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSSU', ' ', io_prec     , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Susp. load transport in u-direction at station (zeta point)', unit=moroutput%unit_transport_rate, attribs=(/idatt_sta/) )
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSSV', ' ', io_prec     , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Susp. load transport in v-direction at station (zeta point)', unit=moroutput%unit_transport_rate, attribs=(/idatt_sta/) )
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZRCA', ' ', io_prec     , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Near-bed reference concentration of sediment at station', unit='kg/m3', attribs=(/idatt_sta/) )
           !
           if (moroutput%sourcesink) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSOURSE', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Source term suspended sediment transport at station', unit='kg/m3/s', attribs=(/idatt_sta/) )
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSINKSE', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Sink term suspended sediment transport at station', unit='1/s', attribs=(/idatt_sta/) )
           endif
         endif
         !
         if (lsedtot > 0) then
           if (moroutput%frac) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZFRAC', ' ', io_prec    , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Availability fraction in top layer at station', attribs=(/idatt_sta/) )
           endif
           !
           if (moroutput%mudfrac) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZMUDFRAC', ' ', io_prec , 1, dimids=(/iddim_nostat/), longname='Mud fraction in top layer at station', attribs=(/idatt_sta/) )
           endif
           !
           if (moroutput%sandfrac) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSANDFRAC', ' ', io_prec, 1, dimids=(/iddim_nostat/), longname='Sand fraction in top layer at station', attribs=(/idatt_sta/) )
           endif
           !
           if (moroutput%fixfac) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZFIXFAC', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Reduction factor due to limited sediment thickness at station', attribs=(/idatt_sta/) )
           endif
           !
           if (moroutput%taub) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZTAUB', ' ', io_prec    , 1, dimids=(/iddim_nostat/), longname='Bed shear stress for morphology at station', unit='N/m2', attribs=(/idatt_sta/) )
           endif
           !
           if (moroutput%hidexp) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZHIDEXP', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Hiding and exposure factor at station', attribs=(/idatt_sta/) )
           endif
         endif
       endif
       !
       ! his-sed-series: cross-sections
       !
       if (ntruv > 0) then
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SBTR', ' ', io_prec       , 2, dimids=(/iddim_ntruv, iddim_lsedtot/), longname='Instantaneous bed load transport through section', unit=moroutput%unit_transport_per_crs, attribs=(/idatt_tra/) )
         endif
         if (lsed > 0) then         
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SSTR', ' ', io_prec     , 2, dimids=(/iddim_ntruv, iddim_lsed/), longname='Instantaneous susp. load transport through section', unit=moroutput%unit_transport_per_crs, attribs=(/idatt_tra/) )
         endif
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SBTRC', ' ', io_prec      , 2, dimids=(/iddim_ntruv, iddim_lsedtot/), longname='Cumulative bed load transport through section', unit=moroutput%unit_sediment_amount, attribs=(/idatt_tra/) )
         endif
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SSTRC', ' ', io_prec    , 2, dimids=(/iddim_ntruv, iddim_lsed/), longname='Cumulative susp. load transport through section', unit=moroutput%unit_sediment_amount, attribs=(/idatt_tra/) )
         endif
       endif
       !
       ! Add fluff fields  
       !
       if (lsed > 0) then
          call wrhfluff(lundia    ,error     ,filename  ,grnam5    , &
                      & nostat    ,lsed      ,REQUESTTYPE_DEFINE   , &
                      & fds       ,nostatto  ,nostatgl  ,order_sta , gdp     )
       endif
       !
       group4%grp_dim = iddim_time
       group5%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group5%celidt = celidt
       !
       if (filetype == FTYPE_NEFIS) then
          !
          ! element 'ITHISS'
          !
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, ithisc, 'ITHISS')
          if (ierror/= 0) goto 9999
       endif
       !
       if (lsedtot > 0) then
          !
          ! element 'MORFAC'
          !
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, morfac, 'MORFAC')
          if (ierror/= 0) goto 9999
          !
          ! element 'MORFT'
          !
          call wrtvar(fds, filename, filetype, grnam4, celidt, &
                    & gdp, ierror, lundia, morft, 'MORFT')
          if (ierror/= 0) goto 9999
       endif
       !
       if (nostat > 0) then
          if (lsed > 0) then     
             !
             ! element 'ZWS'
             !
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay, kmaxout, 0, kmax, lsed, &
                    & ierror, lundia, zws, 'ZWS', station)
             if (ierror/= 0) goto 9999
             !
             if (moroutput%seddif) then
                !
                ! element 'ZSEDDIF'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & shlay, kmaxout, 0, kmax, lsed, &
                       & ierror, lundia, zseddif, 'ZSEDDIF', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (kmax == 1) then
                !
                ! element 'ZRSDEQ'
                ! kmax=1: don't use kmaxout/shlay
                !
                allocate(rbuff3(nostat,1,lsed), stat=istat)
                rbuff3(:,1,:) = zrsdeq
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & (/1/), 1, 1, kmax, lsed, &
                       & ierror, lundia, rbuff3, 'ZRSDEQ', station)
                deallocate(rbuff3, stat=istat)
                if (ierror/= 0) goto 9999
             endif
          endif
          !
          if (lsedtot > 0) then
             !
             ! element 'ZBDSED'
             !
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsedtot, &
                    & ierror, lundia, zbdsed, 'ZBDSED', station)
             if (ierror/= 0) goto 9999
             !
             ! element 'ZDPSED'
             !
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zdpsed, 'ZDPSED', station)
             if (ierror/= 0) goto 9999
             !
             ! need to add these quantities to be consistent with FM his-file
             ! however, the number of layers is not (yet) known in esm_alloc_real
             ! where all Z arrays are currently allocated. Consider restructuring.
             !
             ! element 'ZMSED'
             ! element 'ZLYRFRAC'
             ! element 'ZDP_BEDLYR'
             ! element 'ZEPSPOR'
          endif
          !
          ! element 'ZDPS'
          !
          call wrtarray_n(fds, filename, filetype, grnam5, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zdps, 'ZDPS', station)
          if (ierror/= 0) goto 9999
          !
          if (lsedtot > 0) then
             !
             ! element 'ZSBU'
             !
             allocate(rbuff2(nostat,lsedtot), stat=istat)
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, nostat
                   rbuff2(n, l) = zsbu(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsedtot, &
                    & ierror, lundia, rbuff2, 'ZSBU', station)
             if (ierror/= 0) goto 9999
             !
             ! element 'ZSBV'
             !
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, nostat
                   rbuff2(n, l) = zsbv(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsedtot, &
                    & ierror, lundia, rbuff2, 'ZSBV', station)
             deallocate(rbuff2)
             if (ierror/= 0) goto 9999
          endif
          !
          if (lsed > 0) then     
             !
             ! element 'ZSSU'
             !
             allocate(rbuff2(nostat, lsed), stat=istat)
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, nostat
                   rbuff2(n, l) = zssu(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsed, &
                    & ierror, lundia, rbuff2, 'ZSSU', station)
             if (ierror/= 0) goto 9999
             !
             ! element 'ZSSV'
             !
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, nostat
                   rbuff2(n, l) = zssv(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsed, &
                    & ierror, lundia, rbuff2, 'ZSSV', station)
             deallocate(rbuff2)
             if (ierror/= 0) goto 9999
             !
             ! element 'ZRCA'
             !
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & lsed, &
                    & ierror, lundia, zrca, 'ZRCA', station)
             if (ierror/= 0) goto 9999
             !
             if (moroutput%sourcesink) then
                !
                ! element 'ZSOURSE'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & lsed, &
                       & ierror, lundia, zsourse, 'ZSOURSE', station)
                if (ierror/= 0) goto 9999
                !
                ! element 'ZSINKSE'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & lsed, &
                       & ierror, lundia, zsinkse, 'ZSINKSE', station)
                if (ierror/= 0) goto 9999
             endif
          endif
          !
          if (lsedtot > 0) then
             if (moroutput%frac) then 
                !
                ! element 'ZFRAC'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & lsedtot, &
                       & ierror, lundia, zfrac, 'ZFRAC', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (moroutput%mudfrac) then
                !
                ! element 'ZMUDFRAC'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & ierror, lundia, zmudfrac, 'ZMUDFRAC', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (moroutput%sandfrac) then
                !
                ! element 'ZSANDFRAC'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & ierror, lundia, zsandfrac, 'ZSANDFRAC', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (moroutput%fixfac) then
                !
                ! element 'ZFIXFAC'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & lsedtot, &
                       & ierror, lundia, zfixfac, 'ZFIXFAC', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (moroutput%taub) then
                !
                ! element 'ZTAUB'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & ierror, lundia, ztaub, 'ZTAUB', station)
                if (ierror/= 0) goto 9999
             endif
             !
             if (moroutput%hidexp) then
                !
                ! element 'ZHIDEXP'
                !
                call wrtarray_n(fds, filename, filetype, grnam5, &
                       & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                       & lsedtot, &
                       & ierror, lundia, zhidexp, 'ZHIDEXP', station)
                if (ierror/= 0) goto 9999
             endif
          endif
       endif
       !
       ! his-sed-series: cross-sections
       !
       if (ntruv>0) then
          !
          if (lsedtot > 0) then
             !
             ! element 'SBTR'
             !
             allocate(rbuff2(ntruv, lsedtot), stat=istat)
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, ntruv
                   rbuff2(n, l) = sbtr(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lsedtot, &
                    & ierror, lundia, rbuff2, 'SBTR', transec)
             if (ierror/= 0) goto 9999
             !
             ! element 'SBTRC'
             !
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, ntruv
                   rbuff2(n, l) = sbtrc(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lsedtot, &
                    & ierror, lundia, rbuff2, 'SBTRC', transec)
             deallocate(rbuff2)
             if (ierror/= 0) goto 9999
          endif
          !
          if (lsed > 0) then     
             !
             ! element 'SSTR'
             !
             allocate(rbuff2(ntruv, lsed), stat=istat)
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, ntruv
                   rbuff2(n, l) = sstr(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lsed, &
                    & ierror, lundia, rbuff2, 'SSTR', transec)
             if (ierror/= 0) goto 9999
             !
             ! element 'SSTRC'
             !
             do l = 1, lsed
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do n = 1, ntruv
                   rbuff2(n, l) = sstrc(n, l)/rhol
                enddo
             enddo
             call wrtarray_n(fds, filename, filetype, grnam5, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lsed, &
                    & ierror, lundia, rbuff2, 'SSTRC', transec)
             deallocate(rbuff2)
             if (ierror/= 0) goto 9999
          endif
       endif
       !
       ! Add fluff fields  
       !
       if (lsed > 0) then
          call wrhfluff(lundia    ,error     ,filename  ,grnam5    , &
                      & nostat    ,lsed      ,REQUESTTYPE_WRITE    , &
                      & fds       ,nostatto  ,nostatgl  ,order_sta , gdp     )
       endif
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrsedh
