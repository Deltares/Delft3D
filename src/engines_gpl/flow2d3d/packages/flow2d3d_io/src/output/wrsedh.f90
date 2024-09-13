subroutine wrsedh(lundia    ,error     ,filename  ,ithisc    , &
                & nostat    ,kmax      ,lsed      ,lsedtot   , &
                & zws       ,zrsdeq    ,zbdsed    ,zdpsed    ,zdps      , &
                & ntruv     ,zmodel    , &
                & zsbu      ,zsbv      ,zssu      ,zssv      ,sbtr      , &
                & sstr      ,sbtrc     ,sstrc     ,zrca      ,irequest  , &
                & fds       ,nostatto  ,nostatgl  ,order_sta ,ntruvto   , &
                & ntruvgl   ,order_tra ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: wrsedh.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrsedh.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for sediment (4 & 5)
!              to the FLOW HIS file.
!              Output is performed conform the times of history
!              file and only in case lsed > 0.
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
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc   !!  Current time counter for the history data file
    integer                                                                           :: ithisi   !  Description and declaration in inttim.igs
    integer                                                                           :: itstrt   !  Description and declaration in inttim.igs
    integer                                                                           :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lsed     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lsedtot  !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    integer                                                                           :: nostat   !  Description and declaration in dimens.igs
    integer                                                                           :: ntruv    !  Description and declaration in dimens.igs
    logical                                                             , intent(in)  :: zmodel   !  Description and declaration in procs.igs
    logical                                                             , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    real(fp), dimension(nostat)                                                       :: zdps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat)                                                       :: zdpsed   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, 0:kmax, lsed)                                         :: zws      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsed)                 :: zrsdeq !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsedtot)                                              :: zbdsed   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsed)                                                 :: zrca     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zsbu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsedtot)                                , intent(in)  :: zsbv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zssu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nostat, lsed)                                   , intent(in)  :: zssv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ntruv, lsedtot)                                 , intent(in)  :: sbtr     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ntruv, lsedtot)                                 , intent(in)  :: sbtrc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ntruv, lsed)                                    , intent(in)  :: sstr     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ntruv, lsed)                                    , intent(in)  :: sstrc    !  Description and declaration in esm_alloc_real.f90
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: nostatgl ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: nostatto ! total number of stations (including "duplicate" stations located in halo regions)
    integer       , dimension(nostat)                                   , intent(in)  :: order_sta
    integer                                                             , intent(in)  :: ntruvgl  ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: ntruvto  ! total number of tracks (including "duplicate" stations located in halo regions)
    integer       , dimension(ntruv)                                    , intent(in)  :: order_tra
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
    character(2)                                      :: sedunit
    character(10)                                     :: transpunit
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
       iddim_lsed    = adddim(gdp, lundia, FILOUT_HIS, 'LSED', lsed)
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT', lsedtot)
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
       select case(moroutput%transptype)
       case (0)
          sedunit = 'kg'
       case (1)
          sedunit = 'm3'
       case (2)
          sedunit = 'm3'
       end select
       !
       ! his-infsed-serie
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'ITHISS', ' ', IO_INT4       , 0, longname='timestep number (ITHISS*DT*TUNIT := time in sec from ITDATE)')
       endif
       if (lsedtot > 0) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'MORFAC', ' ', IO_REAL4      , 0, longname='morphological acceleration factor (MORFAC)')
          call addelm(gdp, lundia, FILOUT_HIS, grnam4, 'MORFT', ' ', IO_REAL8       , 0, longname='morphological time (days since start of simulation)', unit='days')
       endif
       !
       ! his-sed-series: stations
       !
       if (nostat > 0) then
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZWS', ' ', IO_REAL4     , 3, dimids=(/iddim_nostat, iddim_kmaxout, iddim_lsed/), longname='Settling velocity in station', unit='m/s', attribs=(/idatt_sta/) )
           if (kmax == 1) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZRSDEQ', ' ', IO_REAL4, 3, dimids=(/iddim_nostat, iddim_kmax, iddim_lsed/), longname='Equilibrium concentration of sediment at station (2D only)', unit='kg/m3', attribs=(/idatt_sta/) )
           endif
         endif
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZBDSED', ' ', IO_REAL4    , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Available mass of sediment at bed at station', unit='kg/m2', attribs=(/idatt_sta/) )
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDPSED', ' ', IO_REAL4    , 1, dimids=(/iddim_nostat/), longname='Sediment thickness at bed at station (zeta point)', unit='m', attribs=(/idatt_sta/) )
         endif
         call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZDPS', ' ', IO_REAL4      , 1, dimids=(/iddim_nostat/), longname='Morphological depth at station (zeta point)', unit='m', attribs=(/idatt_sta/) )
         if (lsedtot > 0) then
            transpunit = sedunit // '/(s m)'
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSBU', ' ', IO_REAL4      , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Bed load transport in u-direction at station (zeta point)', unit=transpunit, attribs=(/idatt_sta/) )
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSBV', ' ', IO_REAL4      , 2, dimids=(/iddim_nostat, iddim_lsedtot/), longname='Bed load transport in v-direction at station (zeta point)', unit=transpunit, attribs=(/idatt_sta/) )
         endif
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSSU', ' ', IO_REAL4    , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Susp. load transport in u-direction at station (zeta point)', unit=transpunit, attribs=(/idatt_sta/) )
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZSSV', ' ', IO_REAL4    , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Susp. load transport in v-direction at station (zeta point)', unit=transpunit, attribs=(/idatt_sta/) )
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'ZRCA', ' ', IO_REAL4    , 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Near-bed reference concentration of sediment at station', unit='kg/m3', attribs=(/idatt_sta/) )
         endif
       endif
       !
       ! his-sed-series: cross-sections
       !
       if (ntruv > 0) then
         transpunit = sedunit // '/s'
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SBTR', ' ', IO_REAL4      , 2, dimids=(/iddim_ntruv, iddim_lsedtot/), longname='Instantaneous bed load transport through section', unit=transpunit, attribs=(/idatt_tra/) )
         endif
         if (lsed > 0) then         
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SSTR', ' ', IO_REAL4    , 2, dimids=(/iddim_ntruv, iddim_lsed/), longname='Instantaneous susp. load transport through section', unit=transpunit, attribs=(/idatt_tra/) )
         endif
         transpunit = sedunit
         if (lsedtot > 0) then
            call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SBTRC', ' ', IO_REAL4     , 2, dimids=(/iddim_ntruv, iddim_lsedtot/), longname='Cumulative bed load transport through section', unit=transpunit, attribs=(/idatt_tra/) )
         endif
         if (lsed > 0) then
           call addelm(gdp, lundia, FILOUT_HIS, grnam5, 'SSTRC', ' ', IO_REAL4   , 2, dimids=(/iddim_ntruv, iddim_lsed/), longname='Cumulative susp. load transport through section', unit=transpunit, attribs=(/idatt_tra/) )
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
