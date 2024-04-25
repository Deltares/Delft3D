subroutine wrthis(lundia    ,error     ,filename  ,selhis    ,ithisc    , &
                & itstrt    ,ithisi    ,zmodel    ,nostat    ,ntruv     , &
                & kmax      ,lmax      ,lstsci    ,lsal      ,ltem      , &
                & ltur      ,zkfs      ,zwl       ,zcuru     ,zcurv     , &
                & zcurw     ,zqxk      ,zqyk      ,ztauks    ,ztauet    , &
                & zvicww    ,zdicww    ,zrich     ,zrho      ,gro       , &
                & ztur      ,zvort     ,zenst     ,hydprs    ,fltr      , &
                & ctr       ,atr       ,dtr       ,velt      ,zdps      , &
                & zwndsp    ,zwnddr    ,zairp     ,wind      ,sferic    , &
                & zprecp    ,zevap     ,itdate    ,dtsec     ,irequest  , &
                & fds       ,nostatto  ,nostatgl  ,order_sta ,ntruvto   , &
                & ntruvgl   ,order_tra ,nsluv     ,cbuv      ,zwndcd    ,gdp       )
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
!    Function: Writes the time varying groups (1 & 3) to the
!              FLOW HIS file
!              Selection is done using SELHIS. For elements like
!              ZCURW where KMAX must be > 1 this coupling between
!              KMAX and SELHIS is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use datagroups
    use globaldata
    use dffunctionals
    use netcdf
    use wrtarray, only: wrtvar, wrtarray_n, station, transec
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    integer       , dimension(:, :) , pointer :: mnstat
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
    integer       , dimension(:)    , pointer :: shlay
    logical                         , pointer :: temp
    real(fp)      , dimension(:, :) , pointer :: xystat
    character(20) , dimension(:)    , pointer :: namst
    character*(10)                  , pointer :: trans_unit !  Unit of the variables ATR and DTR
    type (datagroup)                , pointer :: group1
    type (datagroup)                , pointer :: group3
    type (flwoutputtype)            , pointer :: flwoutput
    integer                         , pointer :: ktemp
    logical                         , pointer :: free_convec 
    real(fp)      , dimension(:)    , pointer :: zqeva_out  
    real(fp)      , dimension(:)    , pointer :: zqco_out  
    real(fp)      , dimension(:)    , pointer :: zqbl_out   
    real(fp)      , dimension(:)    , pointer :: zqin_out   
    real(fp)      , dimension(:)    , pointer :: zqnet_out  
    real(fp)      , dimension(:)    , pointer :: zhlc_out   
    real(fp)      , dimension(:)    , pointer :: zhfree_out 
    real(fp)      , dimension(:)    , pointer :: zefree_out 
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: itdate   !  Description and declaration in exttim.igs
    integer                                                             , intent(in)  :: ithisc   !!  Current time counter for the history data file
    integer                                                             , intent(in)  :: ithisi   !  Description and declaration in inttim.igs
    integer                                                             , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer                                                             , intent(in)  :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lmax     !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lsal     !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lstsci   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: ltem     !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: ltur     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: nostat   !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: nsluv    !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: ntruv    !  Description and declaration in dimens.igs
    integer      , dimension(nostat)                                                  :: zkfs     !  KFS in monitoring station
    logical                                                             , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    logical                                                             , intent(in)  :: sferic   !  Description and declaration in tricom.igs
    logical                                                             , intent(in)  :: wind     !  Description and declaration in procs.igs
    logical                                                             , intent(in)  :: zmodel   !  Description and declaration in procs.igs
    real(fp)                                                            , intent(in)  :: dtsec    !  Integration time step [in seconds]
    real(fp)     , dimension(nostat)                                                  :: zdps     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: ztauet   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: ztauks   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zwl      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zwndsp   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zwndcd   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zprecp   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zevap    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zwnddr   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                                                  :: zairp    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                                          :: zdicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                                          :: zrich    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                                          :: zvicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax, ltur)                                    :: ztur     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: hydprs   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zcuru    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zcurv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zcurw    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zenst    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zqxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zqyk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zrho     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                                            :: zvort    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax, lstsci)                                    :: gro      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                                                   :: ctr      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                                                   :: fltr     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                                           :: atr      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                                           :: dtr      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(4, nsluv)                                  , intent(in)  :: cbuv     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                        , intent(in)  :: filename !  File name
    character(23)                                                       , intent(in)  :: selhis   !  Description and declaration in tricom.igs
    character(10)                                                       , intent(in)  :: velt     !! Velocity type 'eulerian' or 'GLM'
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
    integer                                           :: i             ! Help var. 
    integer                                           :: ierror        ! Local error flag
    integer        , dimension(1)                     :: idummy        ! Help array to write integers
    integer                                           :: istat
    integer                                           :: kmaxout       ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                           :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    !
    integer                                           :: year
    integer                                           :: month
    integer                                           :: day
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_kmax
    integer                                           :: iddim_kmaxout
    integer                                           :: iddim_kmaxout_restr
    integer                                           :: iddim_kmax1
    integer                                           :: iddim_lstsci
    integer                                           :: iddim_ltur
    integer                                           :: iddim_nostat
    integer                                           :: iddim_nsluv
    integer                                           :: iddim_ntruv
    integer                                           :: iddim_2
    !
    integer                                           :: idatt_bar
    integer                                           :: idatt_cal
    integer                                           :: idatt_sta
    integer                                           :: idatt_tra
    !
    integer                                           :: n             ! Help var.
    integer                                           :: m             ! Help var.
    integer        , dimension(:)       , allocatable :: ibuff1        ! work array
    integer        , dimension(:,:)     , allocatable :: ibuff2        ! work array
    integer        , dimension(:,:)     , allocatable :: ibuff2b       ! work array
    integer        , dimension(:)       , allocatable :: nostatarr     ! number of stations per partition
    integer        , dimension(:)       , allocatable :: shlay_restr   ! copy of shlay, excluding layer zero
    real(fp)       , dimension(:)       , allocatable :: rbuff1        ! work array
    real(fp)       , dimension(:,:)     , allocatable :: rbuff2        ! work array
    real(fp)       , dimension(:,:)     , allocatable :: rbuff2gl      ! work array
    real(fp)       , dimension(:,:,:)   , allocatable :: rbuff3        ! work array
    real(fp)       , dimension(:,:,:)   , allocatable :: rbuff3gl      ! work array
    character(64)                                     :: xcoordname    ! Name of X coordinate: PROJECTION_X_COORDINATE or LONGITUDE
    character(64)                                     :: xcoordunit    ! Unit of X coordinate: M or DEGREES_EAST
    character(64)                                     :: ycoordname    ! Name of Y coordinate: PROJECTION_Y_COORDINATE or LATITUDE
    character(64)                                     :: ycoordunit    ! Unit of Y coordinate: M or DEGREES_NORTH
    character(16)                                     :: grnam1        ! Data-group name defined for the NEFIS-files group 1 
    character(16)                                     :: grnam3        ! Data-group name defined for the NEFIS-files group 3 
    character(256)                                    :: string
!
! Data statements
!
    data grnam1/'his-info-series'/
    data grnam3/'his-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_HIS, grnam1, group1)
    call getdatagroup(gdp, FILOUT_HIS, grnam3, group3)
    celidt      => group1%celidt
    mnstat      => gdp%gdstations%mnstat
    xystat      => gdp%gdstations%xystat
    flwoutput   => gdp%gdflwpar%flwoutput
    namst       => gdp%gdstations%namst
    mfg         => gdp%gdparall%mfg
    nfg         => gdp%gdparall%nfg
    io_fp       => gdp%gdpostpr%io_fp
    io_prec     => gdp%gdpostpr%io_prec
    shlay       => gdp%gdpostpr%shlay
    temp        => gdp%gdprocs%temp
    xystat      => gdp%gdstations%xystat
    flwoutput   => gdp%gdflwpar%flwoutput
    ktemp       => gdp%gdtricom%ktemp
    free_convec => gdp%gdheat%free_convec
    zqeva_out   => gdp%gdheat%zqeva_out
    zqco_out    => gdp%gdheat%zqco_out
    zqbl_out    => gdp%gdheat%zqbl_out
    zqin_out    => gdp%gdheat%zqin_out
    zqnet_out   => gdp%gdheat%zqnet_out
    zhlc_out    => gdp%gdheat%zhlc_out
    zhfree_out  => gdp%gdheat%zhfree_out
    zefree_out  => gdp%gdheat%zefree_out
    !
    ! Initialize local variables
    !
    kmaxout = size(shlay)
    if (shlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay
    endif
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       if (sferic) then
          xcoordname = 'longitude'
          xcoordunit = 'degrees_east'
          ycoordname = 'latitude'
          ycoordunit = 'degrees_north'
       else
          xcoordname = 'projection_x_coordinate'
          xcoordunit = 'm'
          ycoordname = 'projection_y_coordinate'
          ycoordunit = 'm'
       endif
       !
       ! dimensions
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time'   , nf90_unlimited)
       if (zmodel) then
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'K_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_HIS, 'K_INTF' , kmax+1) ! Number of layer interfaces
       else
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'SIG_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_HIS, 'SIG_INTF' , kmax+1) ! Number of layer interfaces
       endif
       iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'KMAX'   , kmax          ) ! Number of layers
       iddim_kmaxout = adddim(gdp, lundia, FILOUT_HIS, 'KMAXOUT', kmaxout       ) ! Number of layer interfaces written
       iddim_kmaxout_restr = adddim(gdp, lundia, FILOUT_HIS, 'KMAXOUT_RESTR', kmaxout_restr ) ! Number of layers written
       !
       idatt_cal = addatt(gdp, lundia, FILOUT_HIS, 'calendar','proleptic_gregorian')
       idatt_sta = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMST XSTAT YSTAT')
       idatt_tra = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMTRA')
       idatt_bar = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMBAR')
       !
       if (lstsci  >0) iddim_lstsci = adddim(gdp, lundia, FILOUT_HIS, 'LSTSCI'            , lstsci  ) ! Number of constituents
       if (ltur    >0) iddim_ltur   = adddim(gdp, lundia, FILOUT_HIS, 'LTUR'              , ltur    ) ! Number of turbulence quantities
       if (nostat  >0) iddim_nostat = adddim(gdp, lundia, FILOUT_HIS, 'NOSTAT'            , nostatgl) ! Number of monitoring stations
       if (ntruv   >0) iddim_ntruv  = adddim(gdp, lundia, FILOUT_HIS, 'NTRUV'             , ntruvgl ) ! Number of monitoring cross-sections
                       iddim_2      = adddim(gdp, lundia, FILOUT_HIS, 'length_2'          , 2       )
       if (nsluv   >0) iddim_nsluv  = adddim(gdp, lundia, FILOUT_HIS, 'NBARRIERS'         , nsluv   ) ! Number of barriers'
       !
       ! his-info-series
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam1, 'ITHISC', ' ', IO_INT4     , 0, longname='timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)')
       else
          year  = itdate / 10000
          month = (itdate - year*10000) / 100
          day   = itdate - year*10000 - month*100
          write(string,'(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day,' 00:00:00'
          call addelm(gdp, lundia, FILOUT_HIS, grnam1, 'time'  , 'time', io_fp    , 0, longname='time', unit=trim(string), attribs=(/idatt_cal/) )
       endif
       !
       ! his-series
       !
       if (nostatgl > 0) then
          if (selhis(1:1)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZKFS', ' ', IO_INT4    , 1, dimids=(/iddim_nostat/), longname='Non-active (0) or active (1) zeta point (time-dependent)')
          endif
          if (selhis(1:1)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZWL', ' ', io_prec     , 1, dimids=(/iddim_nostat/), longname='Water-level in station (zeta point)', unit='m', attribs=(/idatt_sta/))
          endif
          if (index(selhis(2:3), 'Y')>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZCURU', ' ', io_prec   , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='U-velocity per layer in station (zeta point, '//velt//')', unit='m/s', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZCURV', ' ', io_prec   , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='V-velocity per layer in station (zeta point, '//velt//')', unit='m/s', attribs=(/idatt_sta/))
          endif
          if (selhis(4:4)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZCURW', ' ', io_prec   , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='W-velocity per layer in station (zeta point)', unit='m/s', attribs=(/idatt_sta/))
          endif
          if (selhis(20:20)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQXK', ' ', io_prec    , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='U-discharge per layer in station (zeta point)', unit='m3/s', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQYK', ' ', io_prec    , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='V-discharge per layer in station (zeta point)', unit='m3/s', attribs=(/idatt_sta/))
          endif
          if (index(selhis(5:12), 'Y')/=0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'GRO', ' ', io_prec     , 3, dimids=(/iddim_nostat, iddim_kmaxout_restr, iddim_lstsci/), longname='Concentrations per layer in station (zeta point)', attribs=(/idatt_sta/))
          endif
          if (index(selhis(13:14), 'Y')/=0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZTUR', ' ', io_prec    , 3, dimids=(/iddim_nostat, iddim_kmaxout, iddim_ltur/), longname='Turbulent quantity per layer in station (zeta point)', attribs=(/idatt_sta/))
          endif
          if (index(selhis(15:16), 'Y')>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZTAUKS', ' ', io_prec  , 1, dimids=(/iddim_nostat/), longname='Bottom stress U in station (zeta point)', unit='N/m2', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZTAUET', ' ', io_prec  , 1, dimids=(/iddim_nostat/), longname='Bottom stress V in station (zeta point)', unit='N/m2', attribs=(/idatt_sta/))
          endif
          if (selhis(17:17)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZVICWW', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_kmaxout/), longname='Vertical eddy viscosity-3D in station (zeta point)', unit='m2/s', attribs=(/idatt_sta/))
          endif
          if (selhis(18:18)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZDICWW', ' ', io_prec  , 2, dimids=(/iddim_nostat, iddim_kmaxout/), longname='Vertical eddy diffusivity-3D in station (zeta point)', unit='m2/s', attribs=(/idatt_sta/))
          endif
          if (index(selhis(17:18), 'Y')>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZRICH', ' ', io_prec   , 2, dimids=(/iddim_nostat, iddim_kmaxout/), longname='Richardson number in station (zeta point)', attribs=(/idatt_sta/))
          endif
          if (selhis(19:19)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZRHO', ' ', io_prec    , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='Density per layer in station (zeta point)', unit='kg/m3', attribs=(/idatt_sta/))
          endif
          if (wind .and. flwoutput%air) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZWNDSPD', ' ', io_prec , 1, dimids=(/iddim_nostat/), longname='Wind-speed in station', unit='m/s', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZWNDDIR', ' ', io_prec , 1, dimids=(/iddim_nostat/), longname='Wind-direction in station', unit='degrees_Celsius', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'PATM', ' ', io_prec    , 1, dimids=(/iddim_nostat/), longname='Air pressure', unit='N/m2', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZWNDCD', ' ', io_prec , 1, dimids=(/iddim_nostat/), longname='Wind drag coef', unit='-', attribs=(/idatt_sta/))
          endif
          if (flwoutput%air .and. temp) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZPRECP', ' ', io_prec  , 1, dimids=(/iddim_nostat/), longname='Instantaneous precipitation rate in station', unit='mm/h', attribs=(/idatt_sta/))
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZEVAP', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Instantaneous evaporation rate in station', unit='mm/h', attribs=(/idatt_sta/))
          endif
          if (zmodel) then
             if (selhis(2:2)=='Y') then
                call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'HYDPRES', ' ', io_prec , 2, dimids=(/iddim_nostat, iddim_kmaxout_restr/), longname='Non-hydrostatic pressure at station (zeta point)', unit='N/m2', attribs=(/idatt_sta/))
             endif
          endif
          if (filetype == FTYPE_NEFIS) then ! for NEFIS only
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'XYSTAT', ' ', io_fp          , 2, dimids=(/iddim_2, iddim_nostat/), longname='(X,Y) coordinates of monitoring stations', unit=xcoordunit, attribs=(/idatt_sta/))
          else
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'XSTAT', xcoordname, io_fp    , 1, dimids=(/iddim_nostat/), longname='X coordinates of monitoring stations', unit=xcoordunit)
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'YSTAT', ycoordname, io_fp    , 1, dimids=(/iddim_nostat/), longname='Y coordinates of monitoring stations', unit=ycoordunit)
          endif
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'MNSTAT', ' ', IO_INT4        , 2, dimids=(/iddim_2, iddim_nostat/), longname='(M,N) indices of monitoring stations', attribs=(/idatt_sta/))
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'DPS', ' ', io_prec           , 1, dimids=(/iddim_nostat/), longname='Depth in station', unit='m', attribs=(/idatt_sta/))
       endif
       if (ntruvgl > 0) then
          if (selhis(20:20)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'FLTR', ' ', io_prec       , 1, dimids=(/iddim_ntruv/), longname='Total discharge through cross section (velocity points)', unit='m3', attribs=(/idatt_tra/))
          endif
          if (selhis(21:21)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'CTR', ' ', io_prec        , 1, dimids=(/iddim_ntruv/), longname='Momentary discharge through cross section (velocity points)', unit='m3/s', attribs=(/idatt_tra/))
          endif
          if (selhis(22:22)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ATR', ' ', io_prec        , 2, dimids=(/iddim_ntruv, iddim_lstsci/), longname='Advective transport through cross section (velocity points)', attribs=(/idatt_tra/))
          endif
          if (selhis(23:23)=='Y') then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'DTR', ' ', io_prec        , 2, dimids=(/iddim_ntruv, iddim_lstsci/), longname='Dispersive transport through cross section (velocity points)', attribs=(/idatt_tra/))
          endif
       endif
       if (nsluv > 0 .and. flwoutput%hisbar) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZBAR', ' ', io_prec        , 1, dimids=(/iddim_nsluv/), longname='Barrier height', unit='m', attribs=(/idatt_bar/))
       endif
       if (ktemp>0 .and. flwoutput%temperature) then       
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQEVA ', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Evaporation heat flux in ZETA-point'         , unit='W/m2', attribs=(/idatt_sta/))       !
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQCO  ', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Heat flux of forced convection in ZETA-point', unit='W/m2', attribs=(/idatt_sta/)) 
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQBL  ', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Back radiation in ZETA-point'                , unit='W/m2', attribs=(/idatt_sta/))       !
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQIN  ', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Incoming solar radiation flux in ZETA-point' , unit='W/m2', attribs=(/idatt_sta/))              
          call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZQNET ', ' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='INetto heat flux in ZETA-point'              , unit='W/m2', attribs=(/idatt_sta/))  
          if (free_convec) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZEFREE', ' ',io_prec   , 1, dimids=(/iddim_nostat/), longname='Heat flux free convection in ZETA-point'   , unit='W/m2', attribs=(/idatt_sta/))       !   
             call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZHFREE',' ', io_prec   , 1, dimids=(/iddim_nostat/), longname='Heat flux forced convection in ZETA-point' , unit='W/m2', attribs=(/idatt_sta/)) 
          endif
       endif
       group1%grp_dim = iddim_time
       group3%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group3%celidt = celidt
       !
       if (inode == master) then
          !
          ! element 'ITHISC' / time
          !
          if (filetype == FTYPE_NEFIS) then
             call wrtvar(fds, filename, filetype, grnam1, celidt, &
                       & gdp, ierror, lundia, ithisc, 'ITHISC')
          elseif (filetype == FTYPE_NETCDF) then
             call wrtvar(fds, filename, filetype, grnam1, celidt, &
                       & gdp, ierror, lundia, ithisc*dtsec, 'time')
          endif
       endif
       if (ierror/=0) goto 9999
       !
       if (nostatgl > 0) then
          !
          ! element 'ZKFS'
          !
          if (selhis(1:1)=='Y') then
             if (inode == master) then
                allocate( ibuff1(1:nostatgl) )
                ibuff1 = 0
             else
                allocate( ibuff1(1) )
             endif
             if (parll) then    
                call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, zkfs, ibuff1, gdp )
             else
                ibuff1 = zkfs
             endif     
             if (inode == master) then
                call wrtvar(fds, filename, filetype, grnam3, celidt, &
                          & gdp, ierror, lundia, ibuff1, 'ZKFS')
             endif
             deallocate( ibuff1 )
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZWL'
          !
          if (selhis(1:1)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zwl, 'ZWL', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZCURU' & 'ZCURV'
          !
          if (index(selhis(2:3), 'Y')>0) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zcuru, 'ZCURU', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zcurv, 'ZCURV', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZCURW'
          !
          if (selhis(4:4)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zcurw, 'ZCURW', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZQXK' & 'ZQYK'
          !
          if (selhis(20:20)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zqxk, 'ZQXK', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zqyk, 'ZQYK', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'GRO'
          !
          if (index(selhis(5:12), 'Y')/=0) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, lstsci, &
                    & ierror, lundia, gro, 'GRO', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZTUR'
          !
          if (index(selhis(13:14), 'Y')/=0) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay, kmaxout, 0, kmax, ltur, &
                    & ierror, lundia, ztur, 'ZTUR', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZTAUKS' & 'ZTAUET'
          !
          if (selhis(15:15)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, ztauks, 'ZTAUKS', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, ztauet, 'ZTAUET', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZVICWW'
          !
          if (selhis(17:17)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay, kmaxout, 0, kmax, &
                    & ierror, lundia, zvicww, 'ZVICWW', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZDICWW'
          !
          if (selhis(18:18)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay, kmaxout, 0, kmax, &
                    & ierror, lundia, zdicww, 'ZDICWW', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZRICH'
          !
          if (index(selhis(17:18), 'Y')>0) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay, kmaxout, 0, kmax, &
                    & ierror, lundia, zrich, 'ZRICH', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZRHO'
          !
          if (selhis(19:19)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, zrho, 'ZRHO', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZWNDSPD', 'ZWNDDIR', 'PATM' & 'ZWNDCD'
          !
          if (wind .and. flwoutput%air) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zwndsp, 'ZWNDSPD', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zwnddr, 'ZWNDDIR', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zairp, 'PATM', station)
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zwndcd, 'ZWNDCD', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ZPRECP' & 'ZEVAP'
          !
          if (flwoutput%air .and. temp) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zprecp, 'ZPRECP', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, zevap, 'ZEVAP', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'HYDPRES'
          !
          if (index(selhis(2:2), 'Y')>0 .and. zmodel) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & shlay_restr, kmaxout_restr, 1, kmax, &
                    & ierror, lundia, hydprs, 'HYDPRES', station)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'XYSTAT'
          !
          if (filetype == FTYPE_NEFIS) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & 2, &
                    & ierror, lundia, xystat, 'XYSTAT', station, mergedim=2)
             if (ierror/=0) goto 9999
          else
             allocate(rbuff1(nostat), stat=istat)
             do i = 1, nostat
                rbuff1(i) = xystat(1,i)
             enddo
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, rbuff1, 'XSTAT', station)
             if (ierror/=0) goto 9999
             !
             do i = 1, nostat
                rbuff1(i) = xystat(2,i)
             enddo
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, rbuff1, 'YSTAT', station)
             deallocate(rbuff1, stat=istat)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'MNSTAT'
          !
          allocate(ibuff2b(2,nostat), stat=istat)
          do i=1,nostat
             !
             ! mnstat contains indices with respect to this partion
             ! transfer into global indices
             !
             ibuff2b(1,i) = mnstat(1,i) + mfg - 1
             ibuff2b(2,i) = mnstat(2,i) + nfg - 1
          enddo
          if (inode == master) then
             allocate( ibuff2(2,nostatgl) )
          else
             allocate( ibuff2(1,1) )
          endif
          if (parll) then
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, ibuff2b, ibuff2, gdp)
          else
             ibuff2 = ibuff2b
          endif 
          deallocate(ibuff2b)
          if (inode == master) then
             call wrtvar(fds, filename, filetype, grnam3, celidt, &
                       & gdp, ierror, lundia, ibuff2, 'MNSTAT')
          endif
          deallocate( ibuff2 )
          if (ierror/=0) goto 9999
          !
          ! element 'DPS'
          !
          call wrtarray_n(fds, filename, filetype, grnam3, &
                 & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, zdps, 'DPS', station)
          if (ierror/=0) goto 9999
          !          
          ! Output of heat fluxes from temperature model
          ! 
          if (ktemp > 0 .and. flwoutput%temperature) then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                           & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                           & ierror, lundia, zqeva_out, 'ZQEVA', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                           & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                           & ierror, lundia, zqco_out, 'ZQCO', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                           & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                           & ierror, lundia, zqbl_out, 'ZQBL ', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                           & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                           & ierror, lundia, zqin_out, 'ZQIN  ', station)
             if (ierror/=0) goto 9999
             !
             call wrtarray_n(fds, filename, filetype, grnam3, &
                           & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                           & ierror, lundia, zqnet_out, 'ZQNET  ', station)
             if (ierror/=0) goto 9999
             !
             if (free_convec) then
                call wrtarray_n(fds, filename, filetype, grnam3, &
                              & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                              & ierror, lundia, zefree_out, 'ZEFREE  ', station)
                if (ierror/=0) goto 9999
                !
                call wrtarray_n(fds, filename, filetype, grnam3, &
                              & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
                              & ierror, lundia, zhfree_out, 'ZHFREE  ', station)
                if (ierror/=0) goto 9999
             endif
          endif
       endif
       !
       if (ntruvgl > 0) then
          !
          ! element 'FLTR'
          !
          if (selhis(20:20)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & ierror, lundia, fltr, 'FLTR', transec)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'CTR'
          !
          if (selhis(21:21)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & ierror, lundia, ctr, 'CTR', transec)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'ATR'
          !
          if (selhis(22:22)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lstsci, &
                    & ierror, lundia, atr, 'ATR', transec)
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'DTR'
          !
          if (selhis(23:23)=='Y') then
             call wrtarray_n(fds, filename, filetype, grnam3, &
                    & celidt, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & lstsci, &
                    & ierror, lundia, dtr, 'DTR', transec)
             if (ierror/=0) goto 9999
          endif
       endif
       !
       ! Following not yet compatible with parallel simulations (also check WRTHISDIS)
       !
       if (inode == master .and. nsluv > 0 .and. flwoutput%hisbar) then
          allocate(rbuff1(nsluv), stat=istat)
          do i = 1, nsluv
             rbuff1(i) = cbuv(1,i)
          enddo
          call wrtvar(fds, filename, filetype, grnam3, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'ZBAR')
          deallocate(rbuff1, stat=istat)
          if (ierror/=0) goto 9999
       endif
       !
    end select
    deallocate(shlay_restr)
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrthis
