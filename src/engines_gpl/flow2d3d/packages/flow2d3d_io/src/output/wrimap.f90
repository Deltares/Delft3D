subroutine wrimap(lundia      ,error     ,filename  ,selmap    ,simdat    , &
                  & itdate    ,tzone     ,tunit     ,dt        ,mmax      , &
                  & kmax      ,lmax      ,lstsci    ,ltur      ,nmaxus    , &
                  & noroco    ,norow     ,nostat    ,nsrc      ,ntruv     , &
                  & grdang    ,dpsopt    ,sferic    ,lsed      ,lsedtot   , &
                  & zmodel    ,namsrc    ,namcon    ,namsed    , &
                  & kcu       ,kcv       ,kcs       ,irocol    , &
                  & xcor      ,ycor      ,xz        ,yz        ,alfas     , &
                  & dpd       ,thick     ,zk        ,sig       , &
                  & dps       ,dpu       ,dpv       ,gsqs      ,wrifou    , &
                  & irequest  ,fds       ,iarrc     ,mf        ,ml        , &
                  & nf        ,nl        ,nostatto  ,nostatgl  ,order_sta , &
                  & ntruvto   ,ntruvgl   ,order_tra ,ipartition,gdp       )
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
!    Function: Writes the initial group 2 ('map-const') to
!              MAP-DAT
!              Selection is done using SELMAP. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELMAP is done in subroutine RDPRFL
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
    use wrtarray, only: wrtarray_nm, wrtvar, wrtarray_n, station, transec
    use netcdf
    !
    implicit none
    !
    type(globdat), target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(hp)                        , pointer :: dearthrad
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    integer                         , pointer :: lsal
    integer                         , pointer :: ltem
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
    integer       , dimension(:)    , pointer :: smlay
    logical                         , pointer :: densin
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
    logical                         , pointer :: ztbml
    real(fp)                        , pointer :: rhow
    real(fp)                        , pointer :: ag
!
! Global variables
!
    integer                                                                           , intent(in)  :: irequest    !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           , intent(in)  :: itdate      !  Description and declaration in exttim.igs
    integer                                                                                         :: kmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: lmax        !  Description and declaration in dimens.igs
    integer                                                                           , intent(in)  :: lsed        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: lsedtot     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: lstsci      !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: ltur        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: noroco      !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: norow       !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nostat      !  Description and declaration in dimens.igs
    integer                                                                                         :: nsrc        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: ntruv       !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: ipartition  !  Partition number
    integer , dimension(5, noroco)                                                                  :: irocol      !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: kcs         !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: kcu         !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: kcv         !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical                                                                           , intent(in)  :: sferic      !  Description and declaration in tricom.igs
    logical                                                                           , intent(in)  :: zmodel      !  Description and declaration in procs.igs
    logical                                                                           , intent(in)  :: wrifou      !  Description and declaration in procs.igs
    real(fp)                                                                          , intent(in)  :: dt          !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                                          , intent(in)  :: grdang      !  Description and declaration in tricom.igs
    real(fp)                                                                          , intent(in)  :: tunit       !  Description and declaration in exttim.igs
    real(fp)                                                                          , intent(in)  :: tzone       !  Description and declaration in exttim.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: alfas       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: dpd         !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                   , intent(in)  :: dps         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: dpu         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: dpv         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: gsqs        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: xcor        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: xz          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: ycor        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     , intent(in)  :: yz          !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                                       :: thick       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                         , intent(in)  :: sig         !  Vertical coordinates of cell interfaces (SIGMA-MODEL)
    real(fp), dimension(0:kmax)                                                       , intent(in)  :: zk          !!  Vertical coordinates of cell interfaces
                                                                                                                   !!  Flag for activation of Z-MODEL
    character(*)                                                                      , intent(in)  :: filename    !  File name
    character(16)                                                                     , intent(in)  :: simdat      !!  Simulation date representing the
                                                                                                                   !!  flow condition at this date
    character(20), dimension(lmax)                                                    , intent(in)  :: namcon      !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lsedtot)                                                 , intent(in)  :: namsed      !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                                                                  :: namsrc      !  Description and declaration in esm_alloc_char.f90
    character(21)                                                                     , intent(in)  :: selmap      !  Description and declaration in tricom.igs
    character(8)                                                                      , intent(in)  :: dpsopt      !  Description and declaration in numeco.igs
    integer                                                                           , intent(in)  :: fds         !  File handle of output NEFIS/NetCDF file
    !
    integer    , dimension(4,0:nproc-1)                                               , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
    !
    integer    , dimension(nostat)                                                    , intent(in)  :: order_sta
    integer    , dimension(ntruv)                                                     , intent(in)  :: order_tra
    integer                                                                           , intent(in)  :: nostatgl    ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                                           , intent(in)  :: nostatto    ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                                                           , intent(in)  :: ntruvgl     ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                                           , intent(in)  :: ntruvto     ! total number of tracks (including "duplicate" stations located in halo regions)
!
! Local variables
!
    integer                                           :: epsg
    integer                                           :: filetype
    integer      , dimension(1)                       :: idummy    ! Help array to write integers
    integer                                           :: ierror    ! Local error flag
    integer                                           :: istat
    integer                                           :: ifile
    integer                                           :: ip        ! node number 
    integer      , dimension(:)         , allocatable :: ibuff1
    integer      , dimension(:,:)       , allocatable :: ibuff2
    integer      , dimension(2)                       :: ival      ! Local array for writing ITDATE and time (:= 00:00:00)
    integer                                           :: k
    integer                                           :: kmaxout
    integer                                           :: kmaxout_restr
    integer                                           :: l
    integer                                           :: lengl     ! length of field containing collected data
    integer                                           :: lenlo     ! length of field containing collected data
    integer                                           :: lhlp      ! Help variable for teller constituents and turbulent quantities 
    integer                                           :: lsedbl    ! Number of bed load fractions: lsedtot-lsed
    integer                                           :: m         ! Help variable 
    integer                                           :: n         ! Help variable 
    integer                             , external    :: neferr
    integer                             , external    :: clsnef
    integer                             , external    :: open_datdef
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_n
    integer                                           :: iddim_nc
    integer                                           :: iddim_m
    integer                                           :: iddim_mc
    integer                                           :: iddim_kmax
    integer                                           :: iddim_kmaxout
    integer                                           :: iddim_kmaxout_restr
    integer                                           :: iddim_kmax1
    integer                                           :: iddim_lstsci
    integer                                           :: iddim_ltur
    integer                                           :: iddim_nostat
    integer                                           :: iddim_nsrc
    integer                                           :: iddim_ntruv
    integer                                           :: iddim_norow    
    integer                                           :: iddim_noroco    
    integer                                           :: iddim_lsed
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_lsedbl
    integer                                           :: iddim_2
    integer                                           :: iddim_4
    integer                                           :: iddim_5
    integer                                           :: iddim_7
    integer                                           :: iddim_x
    !
    integer                                           :: idatt_cmpintf
    integer                                           :: idatt_cmplyr
    integer                                           :: idatt_grd
    integer   , dimension(7)                          :: idatt_grid
    integer                                           :: idatt_sigfc
    integer                                           :: idatt_sigfi
    integer                                           :: idatt_stgd
    integer                                           :: idatt_stgu
    integer                                           :: idatt_stgv
    integer                                           :: idatt_stgz
    integer                                           :: idatt_up
    integer                                           :: idatt_xyc
    integer                                           :: idatt_xyw
    !
    integer                                           :: idvar_coordmap
    integer        , dimension(:)       , allocatable :: smlay_restr   ! copy of smlay, excluding layer zero
    real(fp)       , dimension(:)       , allocatable :: rbuff1      ! local work array for gathering reals (1 dim)
    !
    character(8)   , dimension(1)                     :: cdum8     ! Help array to write Nefis files 
    character(16)  , dimension(1)                     :: layermode ! Help array to write LAYER_MODEL
    character(16)  , dimension(1)                     :: cdum16    ! Help array to write Nefis files
    character(21)  , dimension(1)                     :: cdum21    ! Help array to write Nefis files
    character(20)  , dimension(:)       , allocatable :: csbuff2   ! work array for gathering names of stations (exc. duplicates)
    character(11)                                     :: epsgstring
    character(16)                                     :: grnam2    ! Data-group name defined for the NEFIS-files
    character(20)  , dimension(:)       , allocatable :: namhlp    ! Help array for name constituents and turbulent quantities
    character(64)                                     :: xcoordname  ! Name of X coordinate: PROJECTION_X_COORDINATE or LONGITUDE
    character(64)                                     :: xcoordunit  ! Unit of X coordinate: M or DEGREES_EAST
    character(64)                                     :: ycoordname  ! Name of Y coordinate: PROJECTION_Y_COORDINATE or LATITUDE
    character(64)                                     :: ycoordunit  ! Unit of Y coordinate: M or DEGREES_NORTH
!
! Data statements
!
    data grnam2/'map-const'/
!
!! executable statements -------------------------------------------------------
!
    dearthrad      => gdp%gdconstd%dearthrad
    !
    mnit           => gdp%gdstations%mnit
    mnstat         => gdp%gdstations%mnstat
    io_fp          => gdp%gdpostpr%io_fp
    io_prec        => gdp%gdpostpr%io_prec
    smlay          => gdp%gdpostpr%smlay
    namst          => gdp%gdstations%namst
    namtra         => gdp%gdstations%namtra
    ztbml          => gdp%gdzmodel%ztbml
    rhow           => gdp%gdphysco%rhow
    ag             => gdp%gdphysco%ag
    lsal           => gdp%d%lsal
    ltem           => gdp%d%ltem
    densin         => gdp%gdmorpar%densin
    !
    ! Initialize local variables
    !
    kmaxout = size(smlay)
    if (smlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay
    endif
    error   = .false.
    if (wrifou) then
       ifile = FILOUT_FOU
    else
       ifile = FILOUT_MAP
    endif
    filetype = getfiletype(gdp, ifile)
    lsedbl  = lsedtot - lsed
    !
    if (zmodel) then
       if (ztbml) then
          layermode(1) = 'Z-MODEL, ZTBML'
       else
          layermode(1) = 'Z-MODEL'
       endif
    else
       layermode(1) = 'SIGMA-MODEL'
    endif
    !
    mfg    => gdp%gdparall%mfg
    nfg    => gdp%gdparall%nfg
    mmaxgl => gdp%gdparall%mmaxgl
    nmaxgl => gdp%gdparall%nmaxgl
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
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
       ! Define dimensions
       !
       iddim_time    = adddim(gdp, lundia, ifile, 'time'   , nf90_unlimited)
       iddim_n       = adddim(gdp, lundia, ifile, 'N'      , nmaxgl) ! Number of N-grid points (cell centres)
       iddim_nc      = adddim(gdp, lundia, ifile, 'NC'     , nmaxgl) ! Number of N-grid points (corner points)
       iddim_m       = adddim(gdp, lundia, ifile, 'M'      , mmaxgl) ! Number of M-grid points (cell centres)
       iddim_mc      = adddim(gdp, lundia, ifile, 'MC'     , mmaxgl) ! Number of M-grid points (corner points)
       if (zmodel) then
          iddim_kmax    = adddim(gdp, lundia, ifile, 'K_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, ifile, 'K_INTF' , kmax+1) ! Number of layer interfaces
          idatt_cmpintf = addatt(gdp, lundia, ifile, 'compress','ZK')
          idatt_cmplyr  = addatt(gdp, lundia, ifile, 'compress','ZK_LYR')
       else
          iddim_kmax    = adddim(gdp, lundia, ifile, 'SIG_LYR', kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, ifile, 'SIG_INTF', kmax+1) ! Number of layer interfaces
          idatt_cmpintf = addatt(gdp, lundia, ifile, 'compress','SIG_INTF')
          idatt_cmplyr  = addatt(gdp, lundia, ifile, 'compress','SIG_LYR')
       endif
       iddim_kmaxout = adddim(gdp, lundia, ifile, 'KMAXOUT', kmaxout) ! Number of layers written
       iddim_kmaxout_restr = adddim(gdp, lundia, ifile, 'KMAXOUT_RESTR', kmaxout_restr) ! Number of layers written
       !
       if (lstsci  >0) iddim_lstsci = adddim(gdp, lundia, ifile, 'LSTSCI'            , lstsci  ) ! Number of constituents
       if (ltur    >0) iddim_ltur   = adddim(gdp, lundia, ifile, 'LTUR'              , ltur    ) ! Number of turbulence quantities
       if (nostatgl>0) iddim_nostat = adddim(gdp, lundia, ifile, 'NOSTAT'            , nostatgl) ! Number of monitoring stations
       if (nsrc    >0) iddim_nsrc   = adddim(gdp, lundia, ifile, 'NSRC'              , nsrc    ) ! Number of discharges
       if (ntruvgl >0) iddim_ntruv  = adddim(gdp, lundia, ifile, 'NTRUV'             , ntruvgl ) ! Number of monitoring cross-sections
                       iddim_norow  = adddim(gdp, lundia, ifile, 'NOROW'             , norow   ) ! Number of rows for IROCOL table 
                       iddim_noroco = adddim(gdp, lundia, ifile, 'NOROCO'            , noroco  ) ! Number of columns of IROCOL table
       if (lsed    >0) iddim_lsed   = adddim(gdp, lundia, ifile, 'LSED'              , lsed    ) ! Number of sediment constituents
       if (lsedtot >0) iddim_lsedtot= adddim(gdp, lundia, ifile, 'LSEDTOT'           , lsedtot ) ! Number of total sediment fractions
       if (lsedbl  >0) iddim_lsedbl = adddim(gdp, lundia, ifile, 'LSEDBL'            , lsedbl  ) ! Number of bedload sediment fractions
                       iddim_2      = adddim(gdp, lundia, ifile, 'length_2'          , 2       )
                       iddim_4      = adddim(gdp, lundia, ifile, 'length_4'          , 4       )
                       iddim_5      = adddim(gdp, lundia, ifile, 'length_5'          , 5       )
                       iddim_7      = adddim(gdp, lundia, ifile, 'length_7'          , 7       )
       !
       idatt_xyc  = addatt(gdp, lundia, ifile, 'coordinates','XCOR YCOR')
       idatt_xyw  = addatt(gdp, lundia, ifile, 'coordinates','XZ YZ')
       idatt_grd  = addatt(gdp, lundia, ifile, 'grid','grid')
       idatt_stgu = addatt(gdp, lundia, ifile, 'location','edge1')
       idatt_stgv = addatt(gdp, lundia, ifile, 'location','edge2')
       idatt_stgz = addatt(gdp, lundia, ifile, 'location','face')
       idatt_stgd = addatt(gdp, lundia, ifile, 'location','node')
       !
       call addatt_class(gdp, lundia, ifile, 'u', (/idatt_grd, idatt_stgu/) )
       call addatt_class(gdp, lundia, ifile, 'v', (/idatt_grd, idatt_stgv/) )
       call addatt_class(gdp, lundia, ifile, 'z', (/idatt_xyw, idatt_grd, idatt_stgz/) )
       call addatt_class(gdp, lundia, ifile, 'd', (/idatt_xyc, idatt_grd, idatt_stgd/) )
       !
       lhlp = 0
       if (index(selmap(6:13), 'Y')/=0) lhlp = lhlp + lstsci
       if (index(selmap(14:15), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
       iddim_x      = adddim(gdp, lundia, ifile, 'length_x'          , lhlp    )
       !
       if (lsedtot>0) then
          idatt_sigfc   = addatt(gdp, lundia, ifile, 'formula_terms', 'sigma: SIG_LYR eta: S1 depth: DPS')
          idatt_sigfi   = addatt(gdp, lundia, ifile, 'formula_terms', 'sigma: SIG_INTF eta: S1 depth: DPS')
       else
          idatt_sigfc   = addatt(gdp, lundia, ifile, 'formula_terms', 'sigma: SIG_LYR eta: S1 depth: DPS0')
          idatt_sigfi   = addatt(gdp, lundia, ifile, 'formula_terms', 'sigma: SIG_INTF eta: S1 depth: DPS0')
       endif
       idatt_up      = addatt(gdp, lundia, ifile, 'positive','up')
       !
       ! map-const
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, ifile, grnam2, 'ITDATE', ' ', IO_INT4      , 1, dimids=(/iddim_2/), longname='Initial date (input) & time (default 00:00:00)', unit='[YYYYMMDD]')
          call addelm(gdp, lundia, ifile, grnam2, 'TZONE', ' ', io_prec       , 0, longname='Local time zone', unit='h')
          call addelm(gdp, lundia, ifile, grnam2, 'TUNIT', ' ', io_prec       , 0, longname='Time scale related to seconds', unit='s')
          call addelm(gdp, lundia, ifile, grnam2, 'DT',    ' ', io_fp         , 0, longname='Time step (DT*TUNIT sec)')
          call addelm(gdp, lundia, ifile, grnam2, 'SIMDAT', ' ', 16           , 0, longname='Simulation date and time [YYYYMMDD  HHMMSS]') !CHARACTER
          call addelm(gdp, lundia, ifile, grnam2, 'SELMAP', ' ', 21           , 0, longname='Selection flag for field values (2dH, 1dV & 2dV)') !CHARACTER
          call addelm(gdp, lundia, ifile, grnam2, 'NMAX', ' ', IO_INT4        , 0, longname='Number of N-grid points')
          call addelm(gdp, lundia, ifile, grnam2, 'MMAX', ' ', IO_INT4        , 0, longname='Number of M-grid points')
          call addelm(gdp, lundia, ifile, grnam2, 'KMAX', ' ', IO_INT4        , 0, longname='Number of layers')
          ! LSTSCI var. name in MAP FILE must remain LSTCI for GPP to work properly
          call addelm(gdp, lundia, ifile, grnam2, 'LSTCI', ' ', IO_INT4       , 0, longname='Number of constituents')
          call addelm(gdp, lundia, ifile, grnam2, 'LTUR', ' ', IO_INT4        , 0, longname='Number of turbulence quantities')
          call addelm(gdp, lundia, ifile, grnam2, 'NOSTAT', ' ', IO_INT4      , 0, longname='Number of monitoring stations')
          call addelm(gdp, lundia, ifile, grnam2, 'NSRC', ' ', IO_INT4        , 0, longname='Number of discharge')
          call addelm(gdp, lundia, ifile, grnam2, 'NTRUV', ' ', IO_INT4       , 0, longname='Number of monitoring cross-sections')
          call addelm(gdp, lundia, ifile, grnam2, 'GRDANG', ' ', io_prec      , 0, longname='Edge between y-axis and real north', unit='arc_degrees')
       endif
       call addelm(gdp, lundia, ifile, grnam2, 'XCOR', xcoordname, io_fp   , 2, dimids=(/iddim_nc, iddim_mc/), longname='X-coordinate of grid points', unit=xcoordunit)
       call addelm(gdp, lundia, ifile, grnam2, 'YCOR', ycoordname, io_fp   , 2, dimids=(/iddim_nc, iddim_mc/), longname='Y-coordinate of grid points', unit=ycoordunit)
       call addelm(gdp, lundia, ifile, grnam2, 'XZ', xcoordname, io_fp     , 2, dimids=(/iddim_n , iddim_m /), longname='X-coordinate of cell centres', unit=xcoordunit)
       call addelm(gdp, lundia, ifile, grnam2, 'YZ', ycoordname, io_fp     , 2, dimids=(/iddim_n , iddim_m /), longname='Y-coordinate of cell centres', unit=ycoordunit)
       call addelm(gdp, lundia, ifile, grnam2, 'ALFAS', ' ', io_prec       , 2, dimids=(/iddim_n , iddim_m /), longname='Orientation ksi-axis w.r.t. pos.x-axis at water level point', unit='arc_degrees', acl='z')
       call addelm(gdp, lundia, ifile, grnam2, 'KCU', ' ', IO_INT4         , 2, dimids=(/iddim_n , iddim_mc/), longname='Mask array for U-velocity points', acl='u')
       call addelm(gdp, lundia, ifile, grnam2, 'KCV', ' ', IO_INT4         , 2, dimids=(/iddim_nc, iddim_m /), longname='Mask array for V-velocity points', acl='v')
       call addelm(gdp, lundia, ifile, grnam2, 'KCS', ' ', IO_INT4         , 2, dimids=(/iddim_n , iddim_m /), longname='Non-active/active water-level point', acl='z')
       if (dpsopt == 'DP') then
          call addelm(gdp, lundia, ifile, grnam2, 'DP0', ' ', io_prec         , 2, dimids=(/iddim_n, iddim_m/), longname='Initial bottom depth (positive down)', unit='m', acl='z')
       else
          call addelm(gdp, lundia, ifile, grnam2, 'DP0', ' ', io_prec         , 2, dimids=(/iddim_nc, iddim_mc/), longname='Initial bottom depth (positive down)', unit='m', acl='d')
       endif
       call addelm(gdp, lundia, ifile, grnam2, 'DPS0', ' ', io_prec        , 2, dimids=(/iddim_n , iddim_m /), longname='Initial bottom depth at zeta points (positive down)', unit='m', acl='z')
       call addelm(gdp, lundia, ifile, grnam2, 'DPU0', ' ', io_prec        , 2, dimids=(/iddim_n , iddim_mc/), longname='Initial bottom depth at u points (positive down)', unit='m', acl='u')
       call addelm(gdp, lundia, ifile, grnam2, 'DPV0', ' ', io_prec        , 2, dimids=(/iddim_nc, iddim_m /), longname='Initial bottom depth at v points (positive down)', unit='m', acl='v')
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, ifile, grnam2, 'DRYFLP', ' ', 8            , 0, longname='Criterium to calculate depth in zeta points') !CHARACTER
          call addelm(gdp, lundia, ifile, grnam2, 'NOROW', ' ', IO_INT4       , 0, longname='Number of rows for IROCOL table')
          call addelm(gdp, lundia, ifile, grnam2, 'NOROCO', ' ', IO_INT4      , 0, longname='Number of rows & columns of IROCOL table')
          call addelm(gdp, lundia, ifile, grnam2, 'IROCOL', ' ', IO_INT4      , 2, dimids=(/iddim_5, iddim_noroco/), longname='Administration of zeta points')
          call addelm(gdp, lundia, ifile, grnam2, 'THICK', ' ', io_prec       , 1, dimids=(/iddim_kmax/), longname='Fraction part of layer thickness of total water-height', unit='[ .01*% ]')
          call addelm(gdp, lundia, ifile, grnam2, 'NAMCON', ' ', 20           , 1, dimids=(/iddim_x/), longname='Name of constituent & turbulent quantity') !CHARACTER
          if (nostatgl>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'MNSTAT', ' ', IO_INT4   , 2, dimids=(/iddim_2, iddim_nostat/), longname='(M,N) indices of monitoring stations')
             call addelm(gdp, lundia, ifile, grnam2, 'NAMST', ' ', 20         , 1, dimids=(/iddim_nostat/), longname='Name of monitoring station') !CHARACTER
          endif
          if (nsrc>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'NAMSRC', ' ', 20         , 1, dimids=(/iddim_nsrc/), longname='Name of discharge source') !CHARACTER
          endif
          if (ntruvgl>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'MNTRA', ' ', IO_INT4     , 2, dimids=(/iddim_4, iddim_ntruv/), longname='(M1,N1)-(M2,N2) indices of monitoring cross-sections')
             call addelm(gdp, lundia, ifile, grnam2, 'NAMTRA', ' ', 20         , 1, dimids=(/iddim_ntruv/), longname='Name of monitoring cross-section') !CHARACTER
          endif
          if (lsed>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'LSED', ' ', IO_INT4      , 0, longname='Number of sediment constituents')
          endif
          if (lsedbl>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'LSEDBL', ' ', IO_INT4    , 0, longname='Number of bedload sediment fractions')
          endif
          if (lsedtot>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'NAMSED', ' ', 20         , 1, dimids=(/iddim_lsedtot/), longname='Name of sediment fraction') !CHARACTER
          endif
       else
          if (lstsci>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'NAMCON', ' ', 20           , 1, dimids=(/iddim_lstsci/), longname='Name of constituent quantity') !CHARACTER
          endif
          if (ltur>0) then
             call addelm(gdp, lundia, ifile, grnam2, 'NAMTUR', ' ', 20           , 1, dimids=(/iddim_ltur/)  , longname='Name of turbulent quantity'  ) !CHARACTER
          endif
       endif
       if (zmodel) then
          if (filetype /= FTYPE_NEFIS) then
             call addelm(gdp, lundia, ifile, grnam2, 'ZK_LYR', ' ', io_prec    , 1, dimids=(/iddim_kmax/) , longname='Vertical coordinates of layer centres'   , unit='m', attribs=(/idatt_up/) )
          endif
          call addelm(gdp, lundia, ifile, grnam2, 'ZK', ' ', io_prec        , 1, dimids=(/iddim_kmax1/), longname='Vertical coordinates of layer interfaces', unit='m', attribs=(/idatt_up/) )
       elseif (filetype /= FTYPE_NEFIS) then
          call addelm(gdp, lundia, ifile, grnam2, 'SIG_LYR' , 'ocean_sigma_coordinate', io_prec        , 1, dimids=(/iddim_kmax/) , longname='Sigma coordinates of layer centres'   , attribs=(/idatt_sigfc/) )
          call addelm(gdp, lundia, ifile, grnam2, 'SIG_INTF', 'ocean_sigma_coordinate', io_prec        , 1, dimids=(/iddim_kmax1/), longname='Sigma coordinates of layer interfaces', attribs=(/idatt_sigfi/) )
       endif
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, ifile, grnam2, 'COORDINATES', ' ', 16       , 0, longname='Cartesian or Spherical coordinates') !CHARACTER
          call addelm(gdp, lundia, ifile, grnam2, 'LAYER_MODEL', ' ', 16       , 0, longname='Sigma-model or Z-model') !CHARACTER
       elseif (filetype == FTYPE_NETCDF) then
           ierror = nf90_put_att(fds, nf90_global,  'LAYER_MODEL', layermode(1));
           call nc_check_err(lundia, ierror, "put_att global LAYER_MODEL", filename)
       endif
       call addelm(gdp, lundia, ifile, grnam2, 'GSQS', ' ', io_prec         , 2, dimids=(/iddim_n, iddim_m/), longname='Horizontal area of computational cell', unit='m2', acl='z')
       call addelm(gdp, lundia, ifile, grnam2, 'PPARTITION', ' ', IO_INT4   , 2, dimids=(/iddim_n, iddim_m/), longname='Partition', acl='z')
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, ifile, grnam2, 'OUTPUT_LAYERS', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout/), longname='User selected output layers')
       else
          call addelm(gdp, lundia, ifile, grnam2, 'KMAXOUT', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout/), longname='User selected output layer interfaces', attribs=(/idatt_cmpintf/) )
          call addelm(gdp, lundia, ifile, grnam2, 'KMAXOUT_RESTR', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout_restr/), longname='User selected output layer centres', attribs=(/idatt_cmplyr/) )
       endif
       call addelm(gdp, lundia, ifile, grnam2, 'RHOCONST', ' ', io_prec     , 0, longname='User specified constant density', unit='kg/m3')
       call addelm(gdp, lundia, ifile, grnam2, 'GRAVITY', ' ', io_prec      , 0, longname='Gravitational acceleration', unit='m/s2')
       !
       if (filetype == FTYPE_NETCDF) then
          !
          ! grid topology according SGRID conventions
          !
          idatt_grid(1) = addatt(gdp, lundia, ifile, 'cf_role','grid_topology')
          idatt_grid(2) = addatt(gdp, lundia, ifile, 'topology_dimension',2)
          idatt_grid(3) = addatt(gdp, lundia, ifile, 'node_dimensions','MC NC')
          idatt_grid(4) = addatt(gdp, lundia, ifile, 'face_dimensions','M:MC (padding: low) N:NC (padding: low)')
          idatt_grid(5) = addatt(gdp, lundia, ifile, 'face_coordinates','XZ YZ')
          idatt_grid(6) = addatt(gdp, lundia, ifile, 'node_coordinates','XCOR YCOR')
          if (zmodel) then
             idatt_grid(7) = addatt(gdp, lundia, ifile, 'vertical_dimensions','K_LYR:K_INTF (padding: none)')
          else
             idatt_grid(7) = addatt(gdp, lundia, ifile, 'vertical_dimensions','SIG_LYR:SIG_INTF (padding: none)')
          endif
          call addelm(gdp, lundia, ifile, grnam2, 'grid', ' ',IO_INT4, 0, attribs=idatt_grid )
          !
          ! coordinate mapping
          !
          !ierror = nf90_def_var(fds, 'projected_coordinate_system', nf90_int, idvar_coordmap); call nc_check_err(lundia, ierror, "def_var coordinate mapping", filename)
          !if (sferic) then
          !    epsg       = 4326
          !    epsgstring = 'EPSG:4326'
          !    ierror = nf90_put_att(fds, idvar_coordmap, 'name',                        'WGS84'             ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !    ierror = nf90_put_att(fds, idvar_coordmap, 'grid_mapping_name',           'latitude_longitude'); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !else
          !    epsg       = 28992
          !    epsgstring = 'EPSG:28992'
          !    ierror = nf90_put_att(fds, idvar_coordmap, 'name',                        'Unknown projected' ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !    ierror = nf90_put_att(fds, idvar_coordmap, 'grid_mapping_name',           'Unknown projected' ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !endif
          !ierror = nf90_put_att(fds, idvar_coordmap, 'epsg',                        epsg                ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'longitude_of_prime_meridian', 0d0                 ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !
          ! M = semi_major_axis and m = semi_minor_axis, then inverse_flatting should be M/(M-m)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'semi_major_axis',             dearthrad           ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'semi_minor_axis',             6356752.314245d0    ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'inverse_flattening',          298.257223563d0     ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'proj4_params',                ' '                 ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'EPSG_code',                   trim(epsgstring)    ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'projection_name',             ' '                 ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !ierror = nf90_put_att(fds, idvar_coordmap, 'wkt',                         ' '                 ); call nc_check_err(lundia, ierror, "coordinate mapping put_att", filename)
          !
          !ierror     = nf90_put_att(fds, idvar_yz, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(lundia, ierror, "put_att YZ grid_mapping", trim(filename))
       endif
       !
    case (REQUESTTYPE_WRITE)
       !
       if (filetype == FTYPE_NETCDF) then
          !
          !ierror = nf90_put_var(fds, idvar_coordmap, epsg);call nc_check_err(lundia, ierror, "put_var coordmap", filename)
          !
          ! dummy value for grid
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, 0, 'grid')
       endif
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          !
          ! element 'ITDATE'
          !
          ival(1) = itdate
          ival(2) = 000000
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, ival, 'ITDATE')
          if (ierror/=0) goto 9999
          !
          ! element 'TZONE'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, tzone, 'TZONE')
          if (ierror/=0) goto 9999
          !
          ! element 'TUNIT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, tunit, 'TUNIT')
          if (ierror/=0) goto 9999
          !
          ! element 'DT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, dt, 'DT')
          if (ierror/=0) goto 9999
          !
          ! element 'SIMDAT'
          !
          cdum16(1) = simdat
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, cdum16, 'SIMDAT')
          if (ierror/=0) goto 9999
          !
          ! element 'SELMAP'
          !
          cdum21(1) = selmap
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, cdum21, 'SELMAP')
          if (ierror/=0) goto 9999
          !
          ! element 'NMAX'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, nmaxgl, 'NMAX')
          if (ierror/=0) goto 9999
          !
          ! element 'MMAX'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, mmaxgl, 'MMAX')
          if (ierror/=0) goto 9999
          !
          ! element 'KMAX'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, kmax, 'KMAX')
          if (ierror/=0) goto 9999
          !
          ! element 'LSTCI' Variable is now LSTSCI
          !
          idummy(1) = 0
          if (index(selmap(6:13), 'Y')/=0 .and. lstsci>0) idummy(1) = lstsci
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, idummy, 'LSTCI')
          if (ierror/=0) goto 9999
          !
          ! element 'LTUR'
          !
          idummy(1) = 0
          if (index(selmap(14:15), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, idummy, 'LTUR')
          if (ierror/=0) goto 9999
          !
          ! element 'NOSTAT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, nostatgl, 'NOSTAT')
          if (ierror/=0) goto 9999
          !
          ! element 'NSRC'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, nsrc, 'NSRC')
          if (ierror/=0) goto 9999
          !
          ! element 'NTRUV'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, ntruvgl, 'NTRUV')
          if (ierror/=0) goto 9999
          !
          ! element 'GRDANG'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, grdang, 'GRDANG')
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'XCOR'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, xcor, 'XCOR')    
       if (ierror/=0) goto 9999
       !
       ! element 'YCOR'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, ycor, 'YCOR')               
       if (ierror/=0) goto 9999
       !
       ! element 'XZ'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, xz, 'XZ')    
       if (ierror/=0) goto 9999
       !
       ! element 'YZ'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, yz, 'YZ')    
       if (ierror/=0) goto 9999
       !
       ! element 'ALFAS'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, alfas, 'ALFAS')    
       if (ierror/=0) goto 9999
       !
       ! element 'KCU'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, kcu, 'KCU')    
       if (ierror/=0) goto 9999
       !
       ! element 'KCV'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, kcv, 'KCV')    
       if (ierror/=0) goto 9999
       !
       ! element 'KCS'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, kcs, 'KCS')    
       if (ierror/=0) goto 9999
       !
       ! element 'DP0'
       !
       if (dpsopt == 'DP') then
          call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, dps, 'DP0')    
       else
          call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, dpd, 'DP0')    
       endif
       if (ierror/=0) goto 9999
       !
       ! element 'DPS0'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, dps, 'DPS0')    
       if (ierror/=0) goto 9999
       !
       ! element 'DPU0'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, dpu, 'DPU0')    
       if (ierror/=0) goto 9999
       !
       ! element 'DPV0'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, dpv, 'DPV0')    
       if (ierror/=0) goto 9999
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          !
          ! The necessary information is currently held by DPSOPT but
          ! for backward compatibility the quantity is still called
          ! DRYFLP on the TRIM file.
          !
          ! element 'DRYFLP'
          !
          cdum8(1) = dpsopt
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, cdum8, 'DRYFLP')
          if (ierror/=0) goto 9999
          !
          ! element 'NOROW'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, norow, 'NOROW')
          if (ierror/=0) goto 9999
          !
          ! element 'NOROCO'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, noroco, 'NOROCO')
          if (ierror/=0) goto 9999
          !
          ! element 'IROCOL'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, irocol, 'IROCOL')
          if (ierror/=0) goto 9999
          !
          ! element 'THICK'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, thick, 'THICK')
          if (ierror/=0) goto 9999
          !
          ! only if lmax   > 0  (:= SELMAP( 6:15) <> 'NNNNNNNNNN')
          !
          if (index(selmap(6:15), 'Y')/=0) then
             allocate(namhlp(lstsci+ltur), stat=istat)
             lhlp = 0
             if (index(selmap(6:13), 'Y')>0) then
                do l = 1, lstsci
                   namhlp(l) = namcon(l)
                enddo
                lhlp = lhlp + lstsci
             endif
             if (index(selmap(14:15), 'Y')>0) then
                do l = 1, ltur
                   namhlp(lhlp + l) = namcon(lstsci + l)
                enddo
             endif
             !
             ! element 'NAMCON'
             !
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, namhlp, 'NAMCON')
             if (ierror/=0) goto 9999
             deallocate(namhlp, stat=istat)
          endif
          !
          ! only if nostat > 0
          !
          if (nostatgl > 0) then
             !
             ! element 'MNSTAT'
             !
             allocate(ibuff2(2,nostat), stat=istat)
             do k=1,nostat
                !
                ! mnstat contains indices with respect to this partion
                ! transfer into global indices
                !
                ibuff2(1,k) = mnstat(1,k) + mfg - 1
                ibuff2(2,k) = mnstat(2,k) + nfg - 1
             enddo
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & 2, &
                    & ierror, lundia, ibuff2, 'MNSTAT', station, mergedim=2)
             deallocate(ibuff2, stat=istat)
             if (ierror/=0) goto 9999
             !
             ! element 'NAMST'
             !
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, namst, 'NAMST')
             if (ierror/=0) goto 9999
          endif
          !
          ! only if nsrc   > 0
          !
          if (nsrc > 0) then
             !
             ! element 'NAMSRC'
             !
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, namsrc, 'NAMSRC')
             if (ierror/=0) goto 9999
          endif
          !
          ! only if ntruv  > 0
          !
          if (ntruvgl > 0) then
             !
             ! element 'MNTRA'
             !
             allocate(ibuff2(4,ntruv), stat=istat)
             ibuff2 = 0
             do k = 1, ntruv
                !
                ! mnit contains indices with respect to this partion
                ! transfer into global indices
                !
                ibuff2(1,k) = mnit(1,k) + mfg - 1
                ibuff2(2,k) = mnit(2,k) + nfg - 1
                ibuff2(3,k) = mnit(3,k) + mfg - 1
                ibuff2(4,k) = mnit(4,k) + nfg - 1
             enddo
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & 4, &
                    & ierror, lundia, ibuff2, 'MNTRA', transec, mergedim=2)
             deallocate(ibuff2, stat=istat)
             if (ierror/=0) goto 9999
             !
             ! element 'NAMTRA'
             !
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                    & ierror, lundia, namtra, 'NAMTRA')
          endif
          !
          ! element 'LSED'
          !
          if (lsed>0) then
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, lsed, 'LSED')
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'LSEDBL'
          !
          if (lsedbl>0 .and. inode == master) then
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, lsedbl, 'LSEDBL')
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'NAMSED'
          !
          if (lsedtot>0 .and. inode == master) then
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, namsed, 'NAMSED')
             if (ierror/=0) goto 9999
          endif
       else
          !
          ! element 'NAMCON'
          !
          if (lstsci>0) then
             allocate(namhlp(lstsci), stat=istat)
             do l = 1, lstsci
                namhlp(l) = namcon(l)
             enddo
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, namhlp, 'NAMCON')
             if (ierror/=0) goto 9999
             deallocate(namhlp, stat=istat)
          endif
          !
          ! element 'NAMTUR'
          !
          if (ltur>0) then
             allocate(namhlp(ltur), stat=istat)
             do l = 1, ltur
                namhlp(l) = namcon(lstsci+l)
             enddo
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                       & gdp, ierror, lundia, namhlp, 'NAMTUR')
             if (ierror/=0) goto 9999
             deallocate(namhlp, stat=istat)
          endif
       endif
       !
       ! element 'ZK'
       !
       if (zmodel) then
          if (filetype /= FTYPE_NEFIS) then
             allocate(rbuff1(kmax), stat=istat)
             do k = 1, kmax
                rbuff1(k) = (zk(k-1)+zk(k))/2.0_fp
             enddo
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, rbuff1, 'ZK_LYR')
             if (ierror/=0) goto 9999
             deallocate(rbuff1, stat=istat)
          endif
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, zk, 'ZK')
          if (ierror/=0) goto 9999
       elseif (filetype /= FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, sig, 'SIG_LYR')
          if (ierror/=0) goto 9999
          !
          allocate(rbuff1(0:kmax), stat=istat)
          rbuff1(0) = 0.0_fp
          do k = 1, kmax
             rbuff1(k) = rbuff1(k-1) - thick(k)
          enddo
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, rbuff1, 'SIG_INTF')
          if (ierror/=0) goto 9999
          deallocate(rbuff1, stat=istat)
       endif
       !
       ! element 'COORDINATES'
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          if (sferic) then
             cdum16(1) = 'SPHERICAL'
          else
             cdum16(1) = 'CARTESIAN'
          endif
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                   & gdp, ierror, lundia, cdum16, 'COORDINATES')
          if (ierror/=0) goto 9999
          !
          ! element 'LAYER_MODEL'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                   & gdp, ierror, lundia, layermode, 'LAYER_MODEL')
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'GSQS'
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, gsqs, 'GSQS')    
       if (ierror/=0) goto 9999
       !
       ! Parallel partition
       !
       call wrtarray_nm(fds, filename, filetype, grnam2, 1, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, ipartition, 'PPARTITION')    
       if (ierror/=0) goto 9999
       !
       if (filetype == FTYPE_NEFIS) then
          !
          ! element 'OUTPUT_LAYERS'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, smlay, 'OUTPUT_LAYERS')
          if (ierror/=0) goto 9999
       else
          !
          ! element 'KMAXOUT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, smlay, 'KMAXOUT')
          if (ierror/=0) goto 9999
          !
          ! element 'KMAXOUT_RESTR'
          !
          allocate(ibuff1(kmaxout_restr), stat=istat)
          ibuff1 = smlay_restr-1 ! the compress attribute unfortunately requires starting index equal to 0 like C
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, ibuff1, 'KMAXOUT_RESTR')
          deallocate(ibuff1, stat=istat)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'RHOCONST'
       !
       call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, rhow, 'RHOCONST')
       if (ierror/=0) goto 9999
       !
       ! element 'GRAVITY'
       !
       call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, ag, 'GRAVITY')
       if (ierror/=0) goto 9999
    end select
    deallocate(smlay_restr)
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrimap
