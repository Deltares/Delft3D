subroutine wrtmap(lundia    ,error     ,filename  ,selmap    ,itmapc    , &
                & rhow      ,mmax      ,itdate    ,dtsec     , &
                & kmax      ,nmaxus    ,lstsci    ,ltur      , &
                & nsrc      ,zmodel    ,kcs       ,kfs       ,kfu       , &
                & kfv       ,kfumin    ,kfvmin    ,kfumax    ,kfvmax    , &
                & kfsmin    ,kfsmax    ,mnksrc    ,s1        , &
                & dps       ,dzs1      ,thick     ,windcd    , &
                & u1        ,v1        ,w1        ,wphy      ,r1        , &
                & rtur1     ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
                & vicww     ,dicww     ,rich      ,rho       ,p1        , &
                & vortic    ,enstro    ,umnldf    ,vmnldf    ,vicuv     , &
                & taubmx    ,windu     ,windv     ,velt      ,cvalu0    , &
                & cvalv0    ,cfurou    ,cfvrou    ,rouflo    ,patm      , &
                & z0ucur    ,z0vcur    ,z0urou    ,z0vrou    ,ktemp     , &
                & precip    ,evap      ,irequest  ,fds       ,iarrc     , &
                & mf        ,ml        ,nf        ,nl        ,gdp       )
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
!              FLOW MAP file
!              Selection is done using SELMAP. For elements like
!              WPHY where KMAX must be > 1 this coupling between
!              KMAX and SELMAP is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use datagroups
    use globaldata
    use wrtarray, only: wrtarray_nm, wrtarray_nm_2d, wrtarray_nmk, wrtarray_nmkl, wrtarray_nmkli, wrtarray_nmkl_ptr, wrtvar
    use netcdf
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: nmmax
    integer                         , pointer :: ltem
    integer                         , pointer :: celidt
    integer                         , pointer :: keva
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
    integer                          ,pointer :: no_dis
    integer  , dimension(:)         , pointer :: smlay
    logical                         , pointer :: temp
    real(fp) , dimension(:,:,:)     , pointer :: fluxu
    real(fp) , dimension(:,:,:)     , pointer :: fluxuc
    real(fp) , dimension(:,:,:)     , pointer :: fluxv
    real(fp) , dimension(:,:,:)     , pointer :: fluxvc
    real(fp)                        , pointer :: mom_accum
    real(fp)                        , pointer :: rhum
    real(fp)                        , pointer :: tair
    real(fp) , dimension(:)         , pointer :: qeva_out
    real(fp) , dimension(:)         , pointer :: qco_out
    real(fp) , dimension(:)         , pointer :: qbl_out
    real(fp) , dimension(:)         , pointer :: qin_out
    real(fp) , dimension(:)         , pointer :: qnet_out
    real(fp) , dimension(:)         , pointer :: hlc_out
    real(fp) , dimension(:)         , pointer :: hfree_out
    real(fp) , dimension(:)         , pointer :: efree_out
    real(fp) , dimension(:)         , pointer :: rhumarr
    real(fp) , dimension(:)         , pointer :: tairarr
    real(fp) , dimension(:)         , pointer :: clouarr
    real(fp) , dimension(:)         , pointer :: qmis_out
    real(fp), dimension(:,:,:)      , pointer :: disnf
    real(fp), dimension(:,:,:)      , pointer :: disnf_intake
    real(fp), dimension(:,:,:,:)    , pointer :: sournf
    logical                         , pointer :: nfl
    logical                         , pointer :: rhum_file
    logical                         , pointer :: tair_file
    logical                         , pointer :: clou_file
    logical                         , pointer :: prcp_file
    logical                         , pointer :: free_convec    
    type (datagroup)                , pointer :: group1
    type (datagroup)                , pointer :: group3
    type (flwoutputtype)            , pointer :: flwoutput
!
! Global variables
!
    integer                                                                           , intent(in)  :: itdate      !  Description and declaration in exttim.igs
    integer                                                                           , intent(in)  :: itmapc      !!  Current time counter for the MAP data file
    integer                                                                                         :: kmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: ktemp       !  Description and declaration in tricom.f90
    integer                                                                                         :: lstsci      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: ltur        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nsrc        !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(7, nsrc)                                                              :: mnksrc      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kcs         !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfs         !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfu         !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfv         !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmin      !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical                                                                           , intent(in)  :: zmodel      !  Description and declaration in procs.igs
    real(fp)                                                                          , intent(in)  :: rhow        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                                   , intent(in)  :: thick       !  Description and declaration in esm_alloc_real.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: dps         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: patm        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: s1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubmx      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubpu      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubpv      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubsu      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubsv      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: umnldf      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: vmnldf      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: windcd      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: windu       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: windv       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: precip      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: evap        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)            , intent(in)  :: cfurou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)            , intent(in)  :: cfvrou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: cvalu0      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: cvalv0      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: dicww       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: rich        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: vicww       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: w1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur) , intent(in)  :: rtur1       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 1)     , intent(in)  :: vicuv       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: dzs1        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: enstro      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: p1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: rho         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: u1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: v1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: vortic      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: wphy        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) , intent(in)  :: r1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0ucur      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0vcur      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0urou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0vrou      !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                                          , intent(in)  :: dtsec       !  Integration time step [in seconds]
    character(*)                                                                      , intent(in)  :: filename    !  File name
    character(4)                                                                      , intent(in)  :: rouflo      !  Description and declaration in esm_alloc_char.f90
    character(21)                                                                     , intent(in)  :: selmap      !  Description and declaration in tricom.igs
    character(10)                                                                     , intent(in)  :: velt        !! Velocity type 'eulerian' or 'GLM'
    integer                                                                           , intent(in)  :: irequest    !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           , intent(in)  :: fds         !  File handle of output NEFIS/NetCDF file
    !
    integer    , dimension(4,0:nproc-1)                                               , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                       :: filetype
    integer                                       :: i             ! Help var.
    integer                                       :: ierror        ! Local error flag
    integer                                       :: istat
    integer                                       :: k
    integer                                       :: km
    integer                                       :: kmaxout       ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                       :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                                       :: l
    integer                                       :: m             ! Help var.
    integer                                       :: n             ! Help var.
    integer                                       :: nm
    integer    , dimension(1)                     :: idummy        ! Help array to write integers
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    integer    , dimension(:,:)    , allocatable  :: ibuff2
    integer    , dimension(:)      , allocatable  :: smlay_restr   ! copy of smlay, excluding layer zero
    !
    integer                                       :: year
    integer                                       :: month
    integer                                       :: day
    !
    integer                                       :: iddim_time
    integer                                       :: iddim_n
    integer                                       :: iddim_nc
    integer                                       :: iddim_m
    integer                                       :: iddim_mc
    integer                                       :: iddim_kmax
    integer                                       :: iddim_kmax1
    integer                                       :: iddim_kmaxout
    integer                                       :: iddim_kmaxout_restr
    integer                                       :: iddim_lstsci
    integer                                       :: iddim_ltur
    integer                                       :: iddim_nsrc
    integer                                       :: iddim_nfdis
    integer                                       :: iddim_7
    !
    integer                                       :: idatt_cal
    !
    real(fp)   , dimension(:,:)    , allocatable  :: rbuff2
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    real(fp)   , dimension(:,:,:,:,:), allocatable  :: rbuff5
    real(fp)   , dimension(:,:,:)  , allocatable  :: zkt           ! Vertical coordinates of layering interfaces
    character(10)                                 :: runit
    character(16)                                 :: grnam1        ! Data-group name defined for the NEFIS-files group 1
    character(16)                                 :: grnam3        ! Data-group name defined for the NEFIS-files group 3
    character(64)                                 :: rdesc
    character(256)                                :: string
    character(1024)                               :: error_string
!
! Data statements
!
    data grnam1/'map-info-series'/
    data grnam3/'map-series'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grnam1, group1)
    call getdatagroup(gdp, FILOUT_MAP, grnam3, group3)
    celidt         => group1%celidt
    mfg            => gdp%gdparall%mfg
    nfg            => gdp%gdparall%nfg
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    nmmax          => gdp%d%nmmax
    ltem           => gdp%d%ltem
    keva           => gdp%gdtricom%keva
    io_fp          => gdp%gdpostpr%io_fp
    io_prec        => gdp%gdpostpr%io_prec
    no_dis         => gdp%gdnfl%no_dis
    smlay          => gdp%gdpostpr%smlay
    temp           => gdp%gdprocs%temp
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    flwoutput      => gdp%gdflwpar%flwoutput
    rhum           => gdp%gdheat%rhum
    tair           => gdp%gdheat%tair
    qeva_out       => gdp%gdheat%qeva_out
    qco_out        => gdp%gdheat%qco_out
    qbl_out        => gdp%gdheat%qbl_out
    qin_out        => gdp%gdheat%qin_out
    qnet_out       => gdp%gdheat%qnet_out
    hlc_out        => gdp%gdheat%hlc_out
    hfree_out      => gdp%gdheat%hfree_out
    efree_out      => gdp%gdheat%efree_out
    qmis_out       => gdp%gdheat%qmis_out
    disnf          => gdp%gdnfl%disnf
    disnf_intake   => gdp%gdnfl%disnf_intake
    sournf         => gdp%gdnfl%sournf
    nfl            => gdp%gdprocs%nfl
    rhumarr        => gdp%gdheat%rhumarr
    tairarr        => gdp%gdheat%tairarr
    clouarr        => gdp%gdheat%clouarr
    rhum_file      => gdp%gdheat%rhum_file
    tair_file      => gdp%gdheat%tair_file
    clou_file      => gdp%gdheat%clou_file
    prcp_file      => gdp%gdheat%prcp_file
    free_convec    => gdp%gdheat%free_convec
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
    filetype = getfiletype(gdp, FILOUT_MAP)
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_MAP, 'time'   , nf90_unlimited)
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centers)
       iddim_nc      = adddim(gdp, lundia, FILOUT_MAP, 'NC'     , nmaxgl        ) ! Number of N-grid points (corner points)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centers)
       iddim_mc      = adddim(gdp, lundia, FILOUT_MAP, 'MC'     , mmaxgl        ) ! Number of M-grid points (corner points)
       if (zmodel) then
          iddim_kmax    = adddim(gdp, lundia, FILOUT_MAP, 'K_LYR'  , kmax          ) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_MAP, 'K_INTF' , kmax+1        ) ! Number of layer interfaces
       else
          iddim_kmax    = adddim(gdp, lundia, FILOUT_MAP, 'SIG_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_MAP, 'SIG_INTF' , kmax+1) ! Number of layer interfaces
       endif
       iddim_kmaxout = adddim(gdp, lundia, FILOUT_MAP, 'KMAXOUT', kmaxout       ) ! Number of layer interfaces written
       iddim_kmaxout_restr = adddim(gdp, lundia, FILOUT_MAP, 'KMAXOUT_RESTR', kmaxout_restr ) ! Number of layers written
       !
       if (lstsci  >0) iddim_lstsci = adddim(gdp, lundia, FILOUT_MAP, 'LSTSCI'            , lstsci  ) !'Number of constituents             '
       if (ltur    >0) iddim_ltur   = adddim(gdp, lundia, FILOUT_MAP, 'LTUR'              , ltur    ) !'Number of turbulence quantities    '
       if (nsrc    >0) iddim_nsrc   = adddim(gdp, lundia, FILOUT_MAP, 'NSRC'              , nsrc    ) !'Number of discharge                '
       if (no_dis  >0) iddim_nfdis  = adddim(gdp, lundia, FILOUT_MAP, 'NFDIS'             , no_dis  ) !'Number of nearfield discharge      '
                       iddim_7      = adddim(gdp, lundia, FILOUT_MAP, 'length_7'          , 7       )
       !
       idatt_cal = addatt(gdp, lundia, FILOUT_MAP, 'calendar','proleptic_gregorian')
       !
       ! map-info-series
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam1, 'ITMAPC', ' ', IO_INT4   , 0, longname='timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)')
       else
          year  = itdate / 10000
          month = (itdate - year*10000) / 100
          day   = itdate - year*10000 - month*100
          write(string,'(a,i0.4,a,i0.2,a,i0.2,a)') 'seconds since ', year, '-', month, '-', day,' 00:00:00'
          call addelm(gdp, lundia, FILOUT_MAP, grnam1, 'time'  , 'time', io_fp   , 0, longname='time', unit=trim(string), attribs=(/idatt_cal/) )
       endif
       !
       ! map-series
       !
       if (selmap(1:1) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'S1', ' ', io_prec       , 2, dimids=(/iddim_n, iddim_m/), longname='Water-level in zeta point', unit='m', acl='z')
!             ierror   = nf90_put_att(fds, idvar_s1, 'grid_mapping', 'projected_coordinate_system'); call nc_check_err(lundia, ierror, "put_att waterlevel projection", filename)
       endif
       call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFU', ' ', IO_INT4         , 2, dimids=(/iddim_n , iddim_mc/), longname='Non-active/active in U-point', acl='u')
       call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFV', ' ', IO_INT4         , 2, dimids=(/iddim_nc, iddim_m /), longname='Non-active/active in V-point', acl='v')
       if (zmodel .and. flwoutput%kf_minmax) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFSMIN', ' ', IO_INT4   , 2, dimids=(/iddim_n , iddim_m /), longname='Bottom-most active layer at water level point', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFSMAX', ' ', IO_INT4   , 2, dimids=(/iddim_n , iddim_m /), longname='Top-most active layer at water level point', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFUMIN', ' ', IO_INT4   , 2, dimids=(/iddim_n , iddim_mc/), longname='Bottom-most active layer at U-point', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFUMAX', ' ', IO_INT4   , 2, dimids=(/iddim_n , iddim_mc/), longname='Top-most active layer at U-point', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFVMIN', ' ', IO_INT4   , 2, dimids=(/iddim_nc, iddim_m /), longname='Bottom-most active layer at V-point', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'KFVMAX', ' ', IO_INT4   , 2, dimids=(/iddim_nc, iddim_m /), longname='Top-most active layer at V-point', acl='v')
       endif
       if (index(selmap(2:3), 'Y') > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'U1', ' ', io_prec       , 3, dimids=(/iddim_n , iddim_mc, iddim_kmaxout_restr/), longname='U-velocity per layer in U-point ('//trim(velt)//')', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'V1', ' ', io_prec       , 3, dimids=(/iddim_nc, iddim_m , iddim_kmaxout_restr/), longname='V-velocity per layer in V-point ('//trim(velt)//')', unit='m/s', acl='v')
       endif
       if (selmap(4:4) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'W', ' ', io_prec        , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout/), longname='W-omega per layer in zeta point', unit='m/s', acl='z')
       endif
       if (selmap(5:5) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'WPHY', ' ', io_prec     , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr/), longname='W-velocity per layer in zeta point', unit='m/s', acl='z')
       endif
       if (index(selmap(6:13), 'Y') /= 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'R1', ' ', io_prec       , 4, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr, iddim_lstsci/), longname='Concentrations per layer in zeta point', acl='z')
       endif
       if (flwoutput%difuflux) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'R1FLX_UU', ' ', io_prec , 4, dimids=(/iddim_n , iddim_mc, iddim_kmaxout_restr, iddim_lstsci/), longname='Constituent flux in u-direction (u point)', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'R1FLX_VV', ' ', io_prec , 4, dimids=(/iddim_nc, iddim_m , iddim_kmaxout_restr, iddim_lstsci/), longname='Constituent flux in v-direction (v point)', acl='v')
       endif
       if (flwoutput%cumdifuflux) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'R1FLX_UUC', ' ', io_prec , 4, dimids=(/iddim_n , iddim_mc, iddim_kmaxout_restr, iddim_lstsci/), longname='Cumulative constituent flux in u-direction (u point)', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'R1FLX_VVC', ' ', io_prec , 4, dimids=(/iddim_nc, iddim_m , iddim_kmaxout_restr, iddim_lstsci/), longname='Cumulative constituent flux in v-direction (v point)', acl='v')
       endif
       if (flwoutput%momentum) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_DUDT ', ' ', io_prec        , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Acceleration in GLM coordinate (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UDENSITY ', ' ', io_prec    , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Density term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_URESISTANCE ', ' ', io_prec , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Flow resistance term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UCORIOLIS ', ' ', io_prec   , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Coriolis term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UVISCO ', ' ', io_prec      , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Viscosity term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UPRESSURE ', ' ', io_prec   , 2, dimids=(/iddim_n , iddim_mc/), longname='Pressure term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UTIDEGEN ', ' ', io_prec    , 2, dimids=(/iddim_n , iddim_mc/), longname='Tide generating forces term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UWINDFORCE ', ' ', io_prec  , 2, dimids=(/iddim_n , iddim_mc/), longname='Wind forcing term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UBEDSHEAR ', ' ', io_prec   , 2, dimids=(/iddim_n , iddim_mc/), longname='Bed shear term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UWAVES ', ' ', io_prec      , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Wave forces term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UDUDX ', ' ', io_prec       , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Convection term (u point)', unit='m/s2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VDUDY ', ' ', io_prec       , 3, dimids=(/iddim_n , iddim_mc, iddim_kmax/), longname='Cross advection term (u point)', unit='m/s2', acl='u')
          !
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_DVDT ', ' ', io_prec        , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Acceleration in GLM coordinates (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VDENSITY ', ' ', io_prec    , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Density term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VRESISTANCE ', ' ', io_prec , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Flow resistance term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VCORIOLIS ', ' ', io_prec   , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Coriolis term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VVISCO ', ' ', io_prec      , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Viscosity term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VPRESSURE ', ' ', io_prec   , 2, dimids=(/iddim_nc, iddim_m /), longname='Pressure term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VTIDEGEN ', ' ', io_prec    , 2, dimids=(/iddim_nc, iddim_m /), longname='Tide generating forces term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VWINDFORCE ', ' ', io_prec  , 2, dimids=(/iddim_nc, iddim_m /), longname='Wind forcing term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VBEDSHEAR ', ' ', io_prec   , 2, dimids=(/iddim_nc, iddim_m /), longname='Bed shear term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VWAVES ', ' ', io_prec      , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Wave forces term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_VDVDY ', ' ', io_prec       , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Convection term (v point)', unit='m/s2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MOM_UDVDX ', ' ', io_prec       , 3, dimids=(/iddim_nc, iddim_m , iddim_kmax/), longname='Cross advection term (v point)', unit='m/s2', acl='v')
       endif
       if (index(selmap(14:15),'Y') /= 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'RTUR1', ' ', io_prec            , 4, dimids=(/iddim_n, iddim_m, iddim_kmaxout, iddim_ltur/), longname='Turbulent quantity per layer in zeta point', acl='z')
       endif
       if (index(selmap(16:17), 'Y') > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'TAUKSI', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_mc/), longname='Bottom stress in U-point', unit='N/m2', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'TAUETA', ' ', io_prec           , 2, dimids=(/iddim_nc, iddim_m /), longname='Bottom stress in V-point', unit='N/m2', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'TAUMAX', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_m /), longname='Tau_max in zeta points (scalar)', unit='N/m2', acl='z')
       endif
       if (selmap(18:18) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'VICWW', ' ', io_prec            , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout/), longname='Vertical eddy viscosity-3D in zeta point', unit='m2/s', acl='z')
       endif
       if (selmap(19:19) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'DICWW', ' ', io_prec            , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout/), longname='Vertical eddy diffusivity-3D in zeta point', unit='m2/s', acl='z')
       endif
       if (index(selmap(18:19),'Y') > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'RICH', ' ', io_prec             , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout/), longname='Richardson number', acl='z')
       endif
       if (selmap(20:20) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'RHO', ' ', io_prec              , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr/), longname='Density per layer in zeta point', unit='kg/m3', acl='z')
       endif
       if (selmap(21:21) == 'Y') then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'UMNLDF', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_mc/), longname='Filtered U-velocity', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'VMNLDF', ' ', io_prec           , 2, dimids=(/iddim_nc, iddim_m /), longname='Filtered V-velocity', unit='m/s', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'VICUV', ' ', io_prec            , 3, dimids=(/iddim_n , iddim_m , iddim_kmaxout_restr/), longname='Horizontal eddy viscosity in zeta point', unit='m2/s', acl='z')
       endif
       if (nsrc > 0) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'MNKSRC', ' ', IO_INT4           , 2, dimids=(/iddim_7, iddim_nsrc/), longname='(M,N,K) indices of discharge sources and time dep. location')
       endif
       if (flwoutput%vortic) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'VORTIC', ' ', io_prec           , 3, dimids=(/iddim_nc, iddim_mc, iddim_kmaxout_restr/), longname='Vorticity at each layer in depth point', unit='1/s', acl='d')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'ENSTRO', ' ', io_prec           , 3, dimids=(/iddim_nc, iddim_mc, iddim_kmaxout_restr/), longname='Enstrophy at each layer in depth point', unit='1/s2', acl='d')
       endif
       if (index(selmap(2:2), 'Y')>0 .and. zmodel) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'HYDPRES', ' ', io_prec          , 3, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr/), longname='Non-hydrostatic pressure at each layer in zeta point', unit='N/m2', acl='z')
       endif
       if (flwoutput%air) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'WINDU', ' ', io_prec            , 2, dimids=(/iddim_n, iddim_m/), longname='Wind speed in x-direction (zeta point)', unit='m/s', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'WINDV', ' ', io_prec            , 2, dimids=(/iddim_n, iddim_m/), longname='Wind speed in y-direction (zeta point)', unit='m/s', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'PATM', ' ', io_prec             , 2, dimids=(/iddim_n, iddim_m/), longname='Air pressure (zeta point)', unit='N/m2', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'WINDCD', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Wind drag coeffcient (zeta point)', unit='-', acl='z')
          if (clou_file) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'CLOUDS', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Cloud coverage percentage (zeta point)', unit='percent', acl='z')
          endif
          if (rhum_file) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'AIRHUM', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Relative air humidity (zeta point)', unit='percent', acl='z')
          endif
          if (tair_file) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'AIRTEM', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Air temperature (zeta point)', unit='degrees_Celsius', acl='z')
          endif
          if (prcp_file) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'PRECIP', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Precipitation rate (zeta point)', unit='mm/h', acl='z')
          endif
          if (keva < 2 .and. temp) then 
             !
             ! evaporation is calculated by the model
             !
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'EVAP', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Evaporation rate (zeta point)', unit='mm/h', acl='z')
          endif
       endif
       if(flwoutput%temperature) then
          if (ktemp == 3) then
             !
             ! Different output for Excess Temperature model
             !
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'HLC', ' ', io_prec           , 2, dimids=(/iddim_n, iddim_m/), longname='Exchange coefficient in Excess temperature model', unit='W/(m2 K)', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QNET', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Total nett heat flux in zeta point', unit='W/m2', acl='z')
          elseif (ktemp > 0) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QEVA', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Evaporation heat flux in zeta point', unit='W/m2', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QCO', ' ', io_prec           , 2, dimids=(/iddim_n, iddim_m/), longname='Heat flux of forced convection in zeta point', unit='W/m2', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QBL', ' ', io_prec           , 2, dimids=(/iddim_n, iddim_m/), longname='Nett back radiation in zeta point', unit='W/m2', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QIN', ' ', io_prec           , 2, dimids=(/iddim_n, iddim_m/), longname='Nett solar radiation in zeta point', unit='W/m2', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QNET', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Total nett heat flux in zeta point', unit='W/m2', acl='z')
             if (free_convec) then
                call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'HFREE', ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname='Free convection of sensible heat in zeta point', unit='W/m2', acl='z')
                call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'EFREE', ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname='Free convection of latent heat in zeta point', unit='W/m2', acl='z')
             endif
          endif
          if (keva == 3) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'QMIS', ' ', io_prec          , 2, dimids=(/iddim_n, iddim_m/), longname='Computed minus derived heat flux in zeta point', unit='W/m2', acl='z')
          endif
       endif
       if (flwoutput%chezy) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'CFUROU', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_mc/), longname='Chezy roughness parameter in U-point', unit='m0.5/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'CFVROU', ' ', io_prec           , 2, dimids=(/iddim_nc, iddim_m /), longname='Chezy roughness parameter in V-point', unit='m0.5/s', acl='v')
       endif
       if (flwoutput%roughness) then
          select case (rouflo)
          case ('CHEZ')
             runit = 'm0.5/s'
             rdesc = 'Chezy roughness parameter'
          case ('WHIT')
             runit = 'm'
             rdesc = 'Nikuradse roughness parameter'
          case ('MANN')
             runit = 's/m{1/3}'
          rdesc = 'Manning roughness parameter'
          case ('Z ')
             runit = 'm'
             rdesc = 'Z0 roughness parameter'
          end select
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'ROUMETU', ' ', io_prec          , 2, dimids=(/iddim_n , iddim_mc/), longname=trim(rdesc)//' in U-point', unit=runit, acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'ROUMETV', ' ', io_prec          , 2, dimids=(/iddim_nc, iddim_m /), longname=trim(rdesc)//' in V-point', unit=runit, acl='v')
       endif
       if (flwoutput%z0cur) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'Z0UCUR', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_mc/), longname='Current only z0 bed roughness in U-point', unit='m', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'Z0VCUR', ' ', io_prec           , 2, dimids=(/iddim_nc, iddim_m /), longname='Current only z0 bed roughness in V-point', unit='m', acl='v')
       endif
       if (flwoutput%z0rou) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'Z0UROU', ' ', io_prec           , 2, dimids=(/iddim_n , iddim_mc/), longname='Wave enhanced z0 bed roughness in U-point', unit='m', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'Z0VROU', ' ', io_prec           , 2, dimids=(/iddim_nc, iddim_m /), longname='Wave enhanced z0 bed roughness in V-point', unit='m', acl='v')
       endif
       if (flwoutput%layering) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'LAYER_INTERFACE', ' ', io_prec  , 3, dimids=(/iddim_n, iddim_m, iddim_kmax1/), longname='Vertical coordinate of layer interface', unit='m', acl='z')
       endif
       !
       if (nfl) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'DISNF' , ' ', io_prec           , 4, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr, iddim_nfdis/), longname='Near field discharge', unit='m3/s', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam3, 'SOURNF', ' ', io_prec           , 5, dimids=(/iddim_n, iddim_m, iddim_kmaxout_restr, iddim_lstsci, iddim_nfdis/), longname='Near field sources', acl='z')
       endif
       !
       group1%grp_dim = iddim_time
       group3%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       group3%celidt = celidt
       !
       ! Writing of output on every itmapc/time
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grnam1, celidt, &
                    & gdp, ierror, lundia, itmapc, 'ITMAPC')
       elseif (filetype == FTYPE_NETCDF) then
          call wrtvar(fds, filename, filetype, grnam1, celidt, &
                    & gdp, ierror, lundia, itmapc*dtsec, 'time')
       endif
       if (ierror/=0) goto 9999
       !
       ! element 'S1' only if SELMAP( 1: 1) = 'Y'
       !
       if (selmap(1:1) == 'Y') then
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, s1, 'S1')
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'KFU'
       !
       call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, kfu, 'KFU')
       if (ierror /= 0) goto 9999
       !
       ! element 'KFV'
       !
       call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & ierror, lundia, kfv, 'KFV')
       if (ierror /= 0) goto 9999
       !
       if (zmodel .and. flwoutput%kf_minmax) then
          !
          ! element 'KFSMIN'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfsmin, 'KFSMIN')
          if (ierror /= 0) goto 9999
          !
          ! element 'KFSMAX'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfsmax, 'KFSMAX')
          if (ierror /= 0) goto 9999
          !
          ! element 'KFUMIN'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfumin, 'KFUMIN')
          if (ierror /= 0) goto 9999
          !
          ! element 'KFUMAX'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfumax, 'KFUMAX')
          if (ierror /= 0) goto 9999
          !
          ! element 'KFVMIN'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfvmin, 'KFVMIN')
          if (ierror /= 0) goto 9999
          !
          ! element 'KFVMAX'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, kfvmax, 'KFVMAX')
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'U1' & 'V1' only if SELMAP( 2: 3) <> 'NN'
       !
       if (index(selmap(2:3),'Y') > 0) then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, u1, 'U1', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
          if (ierror /= 0) goto 9999
          !
          ! element 'V1'
          !
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, v1, 'V1', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'W' only if kmax > 1 (:=  SELMAP( 4: 4) = 'Y')
       !
       if (selmap(4:4) == 'Y') then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, w1, 'W', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'WPHY' only if KMAX > 1 (:=  SELMAP( 5: 5) = 'Y')
       !
       if (selmap(5:5) == 'Y') then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, wphy, 'WPHY', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'R1', only if LSTSCI > 0
       ! (:= SELMAP( 6:13) <> 'NNNNNNNN')
       !
       if (index(selmap(6:13),'Y') /= 0) then
          call wrtarray_nmkl(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, r1, 'R1', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
          if (flwoutput%difuflux) then
             !
             ! element 'R1FLX_UU'
             !
             call wrtarray_nmkl_ptr(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, fluxu, 'R1FLX_UU', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
       
             !
             ! element 'R1FLX_VV'
             !
             call wrtarray_nmkl_ptr(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, fluxv, 'R1FLX_VV', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          if (flwoutput%cumdifuflux) then
             !
             ! element 'R1FLX_UUC'
             !
             call wrtarray_nmkl_ptr(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, fluxuc, 'R1FLX_UUC', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
             !
             ! element 'R1FLX_VVC'
             !
             call wrtarray_nmkl_ptr(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, ierror, lundia, fluxvc, 'R1FLX_VVC', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
       endif
       if (flwoutput%momentum) then
          mom_accum => gdp%gdflwpar%mom_accum
          !
          ! element 'MOM_DUDT'
          !
          if (associated(gdp%gdflwpar%mom_m_velchange)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_velchange(nm,k) = gdp%gdflwpar%mom_m_velchange(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_velchange, 'MOM_DUDT', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UDENSITY'
          !
          if (associated(gdp%gdflwpar%mom_m_densforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_densforce(nm,k) = gdp%gdflwpar%mom_m_densforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_densforce, 'MOM_UDENSITY', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_URESISTANCE'
          !
          if (associated(gdp%gdflwpar%mom_m_flowresist)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_flowresist(nm,k) = gdp%gdflwpar%mom_m_flowresist(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_flowresist, 'MOM_URESISTANCE', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UCORIOLIS'
          !
          if (associated(gdp%gdflwpar%mom_m_corioforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_corioforce(nm,k) = gdp%gdflwpar%mom_m_corioforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_corioforce, 'MOM_UCORIOLIS', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UVISCO'
          !
          if (associated(gdp%gdflwpar%mom_m_visco)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_visco(nm,k) = gdp%gdflwpar%mom_m_visco(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_visco, 'MOM_UVISCO', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UPRESSURE'
          !
          if (associated(gdp%gdflwpar%mom_m_pressure)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_m_pressure(nm) = gdp%gdflwpar%mom_m_pressure(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_m_pressure, 'MOM_UPRESSURE')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UTIDEGEN'
          !
          if (associated(gdp%gdflwpar%mom_m_tidegforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_m_tidegforce(nm) = gdp%gdflwpar%mom_m_tidegforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_m_tidegforce, 'MOM_UTIDEGEN')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UWINDFORCE'
          !
          if (associated(gdp%gdflwpar%mom_m_windforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_m_windforce(nm) = gdp%gdflwpar%mom_m_windforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_m_windforce, 'MOM_UWINDFORCE')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UBEDSHEAR'
          !
          if (associated(gdp%gdflwpar%mom_m_bedforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_m_bedforce(nm) = gdp%gdflwpar%mom_m_bedforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_m_bedforce, 'MOM_UBEDSHEAR')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UWAVES'
          !
          if (associated(gdp%gdflwpar%mom_m_waveforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_waveforce(nm,k) = gdp%gdflwpar%mom_m_waveforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_waveforce, 'MOM_UWAVES', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UDUDX'
          !
          if (associated(gdp%gdflwpar%mom_m_convec)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_convec(nm,k) = gdp%gdflwpar%mom_m_convec(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_convec, 'MOM_UDUDX', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VDUDY'
          !
          if (associated(gdp%gdflwpar%mom_m_xadvec)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_m_xadvec(nm,k) = gdp%gdflwpar%mom_m_xadvec(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_m_xadvec, 'MOM_VDUDY', &
                        & smlay_restr, kmaxout_restr, kfumin, kfumax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! NOW ALL QUANTITIES IN V/N DIRECTION
          !
          ! element 'MOM_DVDT'
          !
          if (associated(gdp%gdflwpar%mom_n_velchange)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_velchange(nm,k) = gdp%gdflwpar%mom_n_velchange(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_velchange, 'MOM_DVDT', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VDENSITY'
          !
          if (associated(gdp%gdflwpar%mom_n_densforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_densforce(nm,k) = gdp%gdflwpar%mom_n_densforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_densforce, 'MOM_VDENSITY', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VRESISTANCE'
          !
          if (associated(gdp%gdflwpar%mom_n_flowresist)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_flowresist(nm,k) = gdp%gdflwpar%mom_n_flowresist(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_flowresist, 'MOM_VRESISTANCE', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VCORIOLIS'
          !
          if (associated(gdp%gdflwpar%mom_n_corioforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_corioforce(nm,k) = gdp%gdflwpar%mom_n_corioforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_corioforce, 'MOM_VCORIOLIS', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VVISCO'
          !
          if (associated(gdp%gdflwpar%mom_n_visco)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_visco(nm,k) = gdp%gdflwpar%mom_n_visco(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_visco, 'MOM_VVISCO', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VPRESSURE'
          !
          if (associated(gdp%gdflwpar%mom_n_pressure)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_n_pressure(nm) = gdp%gdflwpar%mom_n_pressure(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_n_pressure, 'MOM_VPRESSURE')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VTIDEGEN'
          !
          if (associated(gdp%gdflwpar%mom_n_tidegforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_n_tidegforce(nm) = gdp%gdflwpar%mom_n_tidegforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_n_tidegforce, 'MOM_VTIDEGEN')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VWINDFORCE'
          !
          if (associated(gdp%gdflwpar%mom_n_windforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_n_windforce(nm) = gdp%gdflwpar%mom_n_windforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_n_windforce, 'MOM_VWINDFORCE')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VBEDSHEAR'
          !
          if (associated(gdp%gdflwpar%mom_n_bedforce)) then
             do nm = 1,nmmax
                 gdp%gdflwpar%mom_n_bedforce(nm) = gdp%gdflwpar%mom_n_bedforce(nm) / mom_accum
             enddo
             call wrtarray_nm_2d(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, gdp%gdflwpar%mom_n_bedforce, 'MOM_VBEDSHEAR')
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VWAVES'
          !
          if (associated(gdp%gdflwpar%mom_n_waveforce)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_waveforce(nm,k) = gdp%gdflwpar%mom_n_waveforce(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_waveforce, 'MOM_VWAVES', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_VDVDY'
          !
          if (associated(gdp%gdflwpar%mom_n_convec)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_convec(nm,k) = gdp%gdflwpar%mom_n_convec(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_convec, 'MOM_VDVDY', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
          !
          ! element 'MOM_UDVDX'
          !
          if (associated(gdp%gdflwpar%mom_n_xadvec)) then
             do k = 1,kmax
                 do nm = 1,nmmax
                     gdp%gdflwpar%mom_n_xadvec(nm,k) = gdp%gdflwpar%mom_n_xadvec(nm,k) / mom_accum
                 enddo
             enddo
             call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, gdp%gdflwpar%mom_n_xadvec, 'MOM_UDVDX', &
                        & smlay_restr, kmaxout_restr, kfvmin, kfvmax)
             if (ierror /= 0) goto 9999
          endif
       endif
       
       !
       ! element 'RTUR1', only if LTUR > 0
       ! (:= SELMAP(14:15) <> 'NN')
       !
       if (index(selmap(14:15),'Y') /= 0) then
          call wrtarray_nmkl(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ltur, ierror, lundia, rtur1, 'RTUR1', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'TAUKSI' & 'TAUETA' only if SELMAP(16:17) <> 'NN'
       !
       if (index(selmap(16:17),'Y') > 0) then
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -9999.0_fp
          if (zmodel) then
             do m = 1, mmax
                do n = 1, nmaxus
                   km = kfumin(n, m)
                   if (1<=km .and. km<=kmax) then
                      rbuff2(n, m) = ( taubpu(n, m)*u1(n, m, km) + taubsu(n, m) ) * rhow
                   endif
                enddo
             enddo
          else
             km = kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff2(n, m) = ( taubpu(n, m)*u1(n, m, km) + taubsu(n, m) ) * rhow
                enddo
             enddo
          endif
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'TAUKSI')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
          !
          ! element 'TAUETA'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -9999.0_fp
          if (zmodel) then
             do m = 1, mmax
                do n = 1, nmaxus
                   km = kfvmin(n, m)
                   if (1<=km .and. km<=kmax) then
                      rbuff2(n, m) = ( taubpv(n, m)*v1(n, m, km) + taubsv(n, m) ) * rhow
                   endif
                enddo
             enddo
          else
             km = kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff2(n, m) = ( taubpv(n, m)*v1(n, m, km) + taubsv(n, m) ) * rhow
                enddo
             enddo
          endif
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'TAUETA')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
          !
          ! element 'TAUMAX'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, taubmx, 'TAUMAX')
          if (ierror /= 0) goto 9999
          !
       endif
       !
       ! element 'VICWW' if KMAX > 1 (:= SELMAP(18:18) = 'Y')
       ! vicww is defined on cell boundary planes
       !
       if (selmap(18:18) == 'Y') then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, vicww, 'VICWW', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'DICWW' if KMAX > 1 (:= SELMAP(19:19) = 'Y')
       ! dicww is defined on cell boundary planes
       !
       if (selmap(19:19) == 'Y') then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, dicww, 'DICWW', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'RICH' if KMAX > 1 and DICWW or VICWW written to file
       ! (:= SELMAP(18:19) <> 'NN')
       !
       if (index(selmap(18:19),'Y') > 0) then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, rich, 'RICH', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'RHO' if LSAL > 0 or LTEM > 0
       ! (:= SELMAP(20:20) = 'Y')
       !
       if (selmap(20:20) == 'Y') then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, rho, 'RHO', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! elements 'UMNLDF', 'VMNLDF' and 'VICUV' if htur2d = true
       ! (:= SELMAP(21:21) = 'Y')
       !
       if (selmap(21:21) == 'Y') then
          !
          ! element 'UMNLDF'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, umnldf, 'UMNLDF')
          if (ierror /= 0) goto 9999
          !
          ! element 'VMNLDF'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, vmnldf, 'VMNLDF')
          if (ierror /= 0) goto 9999
          !
          ! element 'VICUV'
          ! kmax+1 contains initial values and should not be written
          !
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax+1, ierror, lundia, vicuv, 'VICUV', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       if (nsrc>0 .and. inode==master) then
          !
          ! element 'MNKSRC' when discharges are present
          !
          allocate(ibuff2(7,nsrc))
          do i=1,nsrc
             !
             ! mnksrc contains indices with respect to this partion
             ! transfer into global indices
             !
             ibuff2(1,i) = mnksrc(1,i) + mfg - 1
             ibuff2(2,i) = mnksrc(2,i) + nfg - 1
             ibuff2(3,i) = mnksrc(3,i)
             ibuff2(4,i) = mnksrc(4,i) + mfg - 1
             ibuff2(5,i) = mnksrc(5,i) + nfg - 1
             ibuff2(6,i) = mnksrc(6,i)
             ibuff2(7,i) = mnksrc(7,i)
          enddo
          call wrtvar(fds, filename, filetype, grnam3, celidt, &
                    & gdp, ierror, lundia, ibuff2, 'MNKSRC')
          deallocate(ibuff2)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'VORTIC' & 'ENSTRO' only if SELMAP( 2: 3) <> 'NN'
       ! First VORTIC
       !
       if (flwoutput%vortic) then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, vortic, 'VORTIC', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
          !
          ! Next ENSTRO
          !
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, enstro, 'ENSTRO', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'HYDPRES'
       !
       if (index(selmap(4:4),'Y')>0 .and. zmodel) then
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, ierror, lundia, p1, 'HYDPRES', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
       endif
       !
       if (flwoutput%chezy) then
          !
          ! element 'CFUROU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, cvalu0, 'CFUROU')
          if (ierror /= 0) goto 9999
          !
          ! element 'CFVROU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, cvalv0, 'CFVROU')
          if (ierror /= 0) goto 9999
       endif
       if (flwoutput%roughness) then
          !
          ! element 'ROUMETU'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:,:) = cfurou(:,:,2)
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'ROUMETU')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
          !
          ! element 'ROUMETV'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:,:) = cfvrou(:,:,2)
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff2, 'ROUMETV')
          deallocate(rbuff2)
          if (ierror /= 0) goto 9999
       endif
       if (flwoutput%z0cur) then
          !
          ! element 'Z0UCUR'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, z0ucur, 'Z0UCUR')
          if (ierror /= 0) goto 9999
          !
          ! element 'Z0VCUR'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, z0vcur, 'Z0VCUR')
          if (ierror /= 0) goto 9999
       endif
       if (flwoutput%z0rou) then
          !
          ! element 'Z0UROU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, z0urou, 'Z0UROU')
          if (ierror /= 0) goto 9999
          !
          ! element 'Z0VROU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, z0vrou, 'Z0VROU')
          if (ierror /= 0) goto 9999
       endif
       !
       ! Output of vertical coordinates of the layer interfaces (both for Sigma- and Z-model)
       ! Note: this will NOT work in parallel (yet) !
       !
       if (flwoutput%layering) then
          !
          ! element 'LAYER_INTERFACE'
          !
          allocate (zkt(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), stat=istat)
          if (istat /= 0) then
             write(lundia, '(''ERROR: Memory allocation error in routine WRTMAP'')')
          endif
          !
          ! Vertical coordinates of layer interfaces requested for output?
          ! Calculate time dependent z-coordinate z(nm,k,t) of layer interfaces
          ! Both for Sigma- and Z-model
          !
          call layer_interfaces(zmodel     ,kmax      ,mmax     ,nmaxus   ,s1      , &
                              & dps        ,thick     ,dzs1     ,kcs      ,kfs     , &
                              & kfsmin     ,kfsmax    ,zkt      ,gdp      )
          call wrtarray_nmk(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 0, kmax, ierror, lundia, zkt, 'LAYER_INTERFACE', &
                        & smlay, kmaxout, kfsmin, kfsmax)
          if (ierror /= 0) goto 9999
          !
          ! Deallocate the array with vertical layer coordinates
          !
          deallocate (zkt)
       endif
       !
       ! Output of air parameters: wind, pressure, cloudiness, relative humidity, temperature, precipitation, and evaporation
       !
       if (flwoutput%air) then
          !
          ! element 'WINDU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, windu, 'WINDU')
          if (ierror /= 0) goto 9999
          !
          ! element 'WINDV'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, windv, 'WINDV')
          if (ierror /= 0) goto 9999
          !
          ! element 'WINDCD'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, windcd, 'WINDCD')
          if (ierror /= 0) goto 9999
          !
          ! element 'PATM'
          !
          call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, patm, 'PATM')
          if (ierror /= 0) goto 9999
          !
          if (prcp_file) then
             !
             ! element 'PRECIP'
             !
             ! Convert to mm/h
             !
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2 = precip * 3600000.0_fp
             call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff2, 'PRECIP')
             deallocate(rbuff2)
             if (ierror /= 0) goto 9999
          endif
          !
          if (keva < 2 .and. temp) then
             !
             ! element 'EVAP'
             !
             ! Convert from [kg m-2 s-1] to [mm h-1]
             !
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2 = evap * 3600000.0_fp / rhow
             call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff2, 'EVAP')
             deallocate(rbuff2)
             if (ierror /= 0) goto 9999
          endif
          !
          if (clou_file) then
             !
             ! element 'CLOUDS'
             !
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -9999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = clouarr(nm)
             enddo
             call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff2, 'CLOUDS')
             deallocate(rbuff2)
             if (ierror /= 0) goto 9999
          endif
          !
          if (rhum_file) then
             !
             ! element 'AIRHUM'
             !
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -9999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = rhumarr(nm)
             enddo
             call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff2, 'AIRHUM')
             deallocate(rbuff2)
             if (ierror /= 0) goto 9999
          endif
          !
          if (tair_file) then
             !
             ! element 'AIRTEM'
             !
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -9999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = tairarr(nm)
             enddo
             call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff2, 'AIRTEM')
             deallocate(rbuff2)
             if (ierror /= 0) goto 9999
          endif
       endif
       !
       ! Output of heat fluxes from temperature model
       !
       if (flwoutput%temperature) then
          if (ktemp == 3) then
             !
             ! element 'HLC'
             !
             if (associated(hlc_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = hlc_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'HLC')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             ! element 'QNET'
             !
             if (associated(qnet_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qnet_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QNET')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
          elseif (ktemp > 0) then
             !
             ! element 'QEVA'
             !
             if (associated(qeva_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qeva_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QEVA')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             ! element 'QCO'
             !
             if (associated(qco_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qco_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QCO')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             ! element 'QBL'
             !
             if (associated(qbl_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qbl_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QBL')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             ! element 'QIN'
             !
             if (associated(qin_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qin_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QIN')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             ! element 'QNET'
             !
             if (associated(qnet_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -9999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qnet_out(nm)
                enddo
                call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                             & nf, nl, mf, ml, iarrc, gdp, &
                             & ierror, lundia, rbuff2, 'QNET')
                deallocate(rbuff2)
                if (ierror /= 0) goto 9999
             endif
             !
             if (free_convec) then
                !
                ! element 'HFREE'
                !
                if (associated(hfree_out)) then
                   allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                   rbuff2(:, :) = -9999.0_fp
                   do nm = 1, nmmax
                      call nm_to_n_and_m(nm, n, m, gdp)
                      rbuff2(n,m) = hfree_out(nm)
                   enddo
                   call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                                & nf, nl, mf, ml, iarrc, gdp, &
                                & ierror, lundia, rbuff2, 'HFREE')
                   deallocate(rbuff2)
                   if (ierror /= 0) goto 9999
                endif
                !
                ! element 'EFREE'
                !
                if (associated(efree_out)) then
                   allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                   rbuff2(:, :) = -9999.0_fp
                   do nm = 1, nmmax
                      call nm_to_n_and_m(nm, n, m, gdp)
                      rbuff2(n,m) = efree_out(nm)
                   enddo
                   call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                                & nf, nl, mf, ml, iarrc, gdp, &
                                & ierror, lundia, rbuff2, 'EFREE')
                   deallocate(rbuff2)
                   if (ierror /= 0) goto 9999
                endif
             endif
             !
             ! element 'QMIS'
             !
             if (keva == 3) then
                if (associated(qmis_out)) then
                   allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                   rbuff2(:, :) = -9999.0_fp
                   do nm = 1, nmmax
                      call nm_to_n_and_m(nm, n, m, gdp)
                      rbuff2(n,m) = qmis_out(nm)
                   enddo
                   call wrtarray_nm(fds, filename, filetype, grnam3, celidt, &
                                & nf, nl, mf, ml, iarrc, gdp, &
                                & ierror, lundia, rbuff2, 'QMIS')
                   deallocate(rbuff2)
                   if (ierror /= 0) goto 9999
                endif
             endif
          else
             !
             ! ktemp = 0, no additional output
             !
          endif
       endif
       if (nfl) then
          !
          ! element 'DISNF'
          !
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, no_dis), stat=ierror )
          rbuff4(:, :, :, :) = -9999.0_fp
          if (zmodel) then
             do n = 1, nmaxus
                do m = 1, mmax
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do k = kfsmin(n,m), kfsmax(n,m)
                      do i = 1, no_dis
                         rbuff4(n,m,k,i) = disnf(nm, k, i) + disnf_intake(nm, k, i)
                      enddo
                   enddo
                enddo
             enddo
          else
             do n = 1, nmaxus
                do m = 1, mmax
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do k = 1, kmax
                      do i = 1, no_dis
                         rbuff4(n,m,k,i) = disnf(nm,k,i) + disnf_intake(nm,k,i)
                      enddo
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nmkl(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, no_dis, ierror, lundia, rbuff4, 'DISNF', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          deallocate(rbuff4, stat=ierror)
          if (ierror /= 0) goto 9999
          !
          ! element 'SOURNF'
          !
          allocate( rbuff5(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci, no_dis), stat=ierror )
          rbuff5(:, :, :, :, :) = -9999.0_fp
          if (zmodel) then
             do n = 1, nmaxus
                do m = 1, mmax
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do k = kfsmin(n,m), kfsmax(n,m)
                      do l = 1, lstsci
                         do i = 1, no_dis
                            rbuff5(n,m,k,l,i) = sournf(nm,k,l,i)
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          else
             do n = 1, nmaxus
                do m = 1, mmax
                   call n_and_m_to_nm(n, m, nm, gdp)
                   do k = 1, kmax
                      do l = 1, lstsci
                         do i = 1, no_dis
                            rbuff5(n,m,k,l,i) = sournf(nm,k,l,i)
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          endif
          call wrtarray_nmkli(fds, filename, filetype, grnam3, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & 1, kmax, lstsci, no_dis, ierror, lundia, rbuff5, 'SOURNF', &
                        & smlay_restr, kmaxout_restr, kfsmin, kfsmax)
          deallocate(rbuff5, stat=ierror)
          if (ierror /= 0) goto 9999
       endif
    end select
    deallocate(smlay_restr)
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrtmap
