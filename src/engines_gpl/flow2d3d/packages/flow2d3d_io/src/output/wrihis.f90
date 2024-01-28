subroutine wrihis(lundia    ,error     ,filename  ,selhis    ,simdat    , &
                & itdate    ,tzone     ,tunit     ,dt        ,nostat    , &
                & ntruv     ,nmax      ,mmax      ,kmax      ,lmax      , &
                & lstsci    ,ltur      ,grdang    ,sferic    ,lsed      , &
                & lsedtot   ,zmodel    ,namcon    ,namsed    ,xz        , &
                & yz        ,alfas     ,dps       ,thick     ,sig       , &
                & zk        ,irequest  ,fds       ,nostatto  ,nostatgl  , &
                & order_sta ,ntruvto   ,ntruvgl   ,order_tra ,xcor      , &
                & ycor      ,kcs       ,nsluv     ,nambar    ,gdp       )
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
!    Function: Writes the initial group 2 ('his-const') to
!              HIS-DAT
!              Selection is done using SELHIS. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELHIS is done in subroutine RDPRFL
!              This routine works for both sequential and parallel computations.
!              Note that, for a sequential computation, (inode ==MASTER) = TRUE.  

! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use dfparall, only: inode, master, parll
    use wrtarray, only: wrtvar, wrtarray_n, station, transec
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    integer       , dimension(:, :) , pointer :: elmdms
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
    integer       , dimension(:)    , pointer :: shlay
    real(fp)      , dimension(:, :) , pointer :: xystat
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
    logical                         , pointer :: ztbml
    type (flwoutputtype)            , pointer :: flwoutput
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: itdate   !  Description and declaration in exttim.igs
    integer                                                                           :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lmax     !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lsed     !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lsedtot  !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lstsci   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: ltur     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nostat   !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: nsluv    !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: ntruv    !  Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcs      !  Description and declaration in esm_alloc_int.f90
    logical                                                             , intent(out) :: error    !  Flag=TRUE if an error is encountered
    logical                                                             , intent(in)  :: sferic   !  Description and declaration in tricom.igs
    logical                                                             , intent(in)  :: zmodel   !  Description and declaration in procs.igs
    real(fp)                                                            , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            , intent(in)  :: grdang   !  Description and declaration in tricom.igs
    real(fp)                                                            , intent(in)  :: tunit    !  Description and declaration in exttim.igs
    real(fp)                                                            , intent(in)  :: tzone    !  Description and declaration in exttim.igs
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: alfas    !  Description and declaration in esm_alloc_real.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: xcor     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: xz       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: ycor     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                                   :: thick    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                     , intent(in)  :: sig      !  Vertical coordinates of cell interfaces (SIGMA-MODEL)
    real(fp)      , dimension(0:kmax)                                   , intent(in)  :: zk       !  Vertical coordinates of cell interfaces (Z-MODEL)
    character(*)                                                        , intent(in)  :: filename !  File name
    character(16)                                                       , intent(in)  :: simdat   !  Simulation date representing the flow condition at this date
    character(20) , dimension(nsluv)                                    , intent(in)  :: nambar   !  Description and declaration in esm_alloc_char.f90
    character(20) , dimension(lmax)                                     , intent(in)  :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20) , dimension(lsedtot)                                  , intent(in)  :: namsed   !  Description and declaration in esm_alloc_char.f90
    character(23)                                                       , intent(in)  :: selhis   !  Description and declaration in tricom.igs
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
    integer                                           :: ierror      ! Local errorflag for NEFIS files
    integer                                           :: istat
    integer                                           :: k
    integer                                           :: kmaxout       ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                           :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                                           :: l      
    integer                                           :: lhlp        ! Help var. for teller constituents and turbulent quantities 
    integer                                           :: lsedbl      ! Number of bed load fractions: lsedtot-lsed
    integer                                           :: m           ! Help var. 
    integer                                           :: m1     
    integer                                           :: m2     
    integer                                           :: n           ! Help var. 
    integer                                           :: n1     
    integer                                           :: n2     
    integer        , dimension(:)       , allocatable :: ibuff1      ! Help array for layer selection
    integer        , dimension(:,:)     , allocatable :: ibuff2      ! Help array for (n,m)-coordinates of cross section locations
    integer        , dimension(1)                     :: idummy      ! Help array to read/write Nefis files 
    integer        , dimension(2)                     :: ival        ! Local array for writing ITDATE and time (:= 00:00:00) 
    integer                            , external     :: clsnef
    integer                            , external     :: open_datdef
    integer                            , external     :: neferr
    !
    integer                                           :: iddim_time
    integer                                           :: iddim_kmax
    integer                                           :: iddim_kmaxout
    integer                                           :: iddim_kmaxout_restr
    integer                                           :: iddim_kmax1
    integer                                           :: iddim_nostat
    integer                                           :: iddim_nsluv
    integer                                           :: iddim_ntruv
    integer                                           :: iddim_lsed
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_2
    integer                                           :: iddim_4
    integer                                           :: iddim_x
    !
    integer                                           :: idatt_cmpintf
    integer                                           :: idatt_cmplyr
    integer                                           :: idatt_sigfc
    integer                                           :: idatt_sigfi
    integer                                           :: idatt_up
    !
    character(16)                                     :: xcoordunit  ! Unit of X coordinate: M or DEGREES_EAST
    character(16)                                     :: ycoordunit  ! Unit of Y coordinate: M or DEGREES_NORTH
    character(16)                                     :: grnam2      ! Data-group name defined for the NEFIS-files 
    character(16)  , dimension(1)                     :: cdum16      ! Help array to read/write Nefis files 
    character(20)  , dimension(:)       , allocatable :: namhlp      ! Help array for name constituents and turbulent quantities 
    character(23)  , dimension(1)                     :: cdum23      ! Help array to read/write Nefis files 
    integer        , dimension(:)       , allocatable :: shlay_restr   ! copy of shlay, excluding layer zero
    real(fp)       , dimension(:)       , allocatable :: rbuff1      ! local work array for gathering reals (1 dim)
    real(fp)       , dimension(:,:)     , allocatable :: rbuff2      ! local work array for gathering reals (2 dim)
!
! Data statements
!
    data grnam2/'his-const'/
!
!! executable statements -------------------------------------------------------
!
    mfg         => gdp%gdparall%mfg
    nfg         => gdp%gdparall%nfg
    mnit        => gdp%gdstations%mnit
    mnstat      => gdp%gdstations%mnstat
    io_fp       => gdp%gdpostpr%io_fp
    io_prec     => gdp%gdpostpr%io_prec
    shlay       => gdp%gdpostpr%shlay
    xystat      => gdp%gdstations%xystat
    namst       => gdp%gdstations%namst
    namtra      => gdp%gdstations%namtra
    ztbml       => gdp%gdzmodel%ztbml
    flwoutput   => gdp%gdflwpar%flwoutput
    !
    ! Initialize local variables
    !
    filetype = getfiletype(gdp, FILOUT_HIS)
    kmaxout  = size(shlay)
    if (shlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay
    endif
    ierror   = 0
    lsedbl   = lsedtot - lsed
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       if (sferic) then
          xcoordunit = 'degrees_east'
          ycoordunit = 'degrees_north'
       else
          xcoordunit = 'm'
          ycoordunit = 'm'
       endif
       !
       ! Define dimensions
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time'   , nf90_unlimited)
       if (zmodel) then
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'K_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_HIS, 'K_INTF' , kmax+1) ! Number of layer interfaces
          idatt_cmpintf = addatt(gdp, lundia, FILOUT_HIS, 'compress','ZK')
          idatt_cmplyr  = addatt(gdp, lundia, FILOUT_HIS, 'compress','ZK_LYR')
       else
          iddim_kmax    = adddim(gdp, lundia, FILOUT_HIS, 'SIG_LYR'  , kmax) ! Number of layers
          iddim_kmax1   = adddim(gdp, lundia, FILOUT_HIS, 'SIG_INTF' , kmax+1) ! Number of layer interfaces
          idatt_cmpintf = addatt(gdp, lundia, FILOUT_HIS, 'compress','SIG_INTF')
          idatt_cmplyr  = addatt(gdp, lundia, FILOUT_HIS, 'compress','SIG_LYR')
       endif
       iddim_kmaxout = adddim(gdp, lundia, FILOUT_HIS, 'KMAXOUT', kmaxout) ! Number of layer interfaces written
       iddim_kmaxout_restr = adddim(gdp, lundia, FILOUT_HIS, 'KMAXOUT_RESTR', kmaxout_restr) ! Number of layers written
       !
       if (nostat  >0) iddim_nostat = adddim(gdp, lundia, FILOUT_HIS, 'NOSTAT'            , nostatgl) ! Number of monitoring stations
       if (ntruv   >0) iddim_ntruv  = adddim(gdp, lundia, FILOUT_HIS, 'NTRUV'             , ntruvgl ) ! Number of monitoring cross-sections
       if (lsed    >0) iddim_lsed   = adddim(gdp, lundia, FILOUT_HIS, 'LSED'              , lsed    ) ! Number of sediment constituents
       if (lsedtot >0) iddim_lsedtot= adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT'           , lsedtot ) ! Number of total sediment fractions
                       iddim_2      = adddim(gdp, lundia, FILOUT_HIS, 'length_2'          , 2       )
                       iddim_4      = adddim(gdp, lundia, FILOUT_HIS, 'length_4'          , 4       )
       if (nsluv   >0) iddim_nsluv  = adddim(gdp, lundia, FILOUT_HIS, 'NBARRIERS'         , nsluv   ) ! Number of barriers
       !
       lhlp = 0
       if (index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) lhlp = lhlp + lstsci
       if (index(selhis(13:14), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
                       iddim_x      = adddim(gdp, lundia, FILOUT_HIS, 'length_x'          , lhlp    )
       !
       idatt_sigfc   = addatt(gdp, lundia, FILOUT_HIS, 'formula_terms', 'sigma: SIG_LYR eta: ZWL depth: DPS')
       idatt_sigfi   = addatt(gdp, lundia, FILOUT_HIS, 'formula_terms', 'sigma: SIG_INTF eta: ZWL depth: DPS')
       idatt_up      = addatt(gdp, lundia, FILOUT_HIS, 'positive','up')
       !
       ! his-const
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'ITDATE', ' ', IO_INT4       , 1, dimids=(/iddim_2/), longname='Initial date (input) & time (default 00:00:00)', unit='[YYYYMMDD]')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'TZONE', ' ', io_prec        , 0, longname='Local time zone', unit='h')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'TUNIT', ' ', io_prec        , 0, longname='Time scale related to seconds', unit='s')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'DT', ' ', io_fp             , 0, longname='Time step (DT*TUNIT sec)')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'SIMDAT', ' ', 16            , 0, longname='Simulation date and time [YYYYMMDD  HHMMSS]') !CHARACTER
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'SELHIS', ' ', 23            , 0, longname='Selection flag for time histories') !CHARACTER
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NOSTAT', ' ', IO_INT4       , 0, longname='Number of monitoring stations')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NTRUV', ' ', IO_INT4        , 0, longname='Number of monitoring cross-sections')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'LSTCI', ' ', IO_INT4        , 0, longname='Number of constituents')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'LTUR', ' ', IO_INT4         , 0, longname='Number of turbulence quantities')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'KMAX', ' ', IO_INT4         , 0, longname='Number of layers')
          if (nostatgl > 0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'MNSTAT', ' ', IO_INT4    , 2, dimids=(/iddim_2, iddim_nostat/), longname='(M,N) indices of monitoring stations')
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'XYSTAT', ' ', io_fp      , 2, dimids=(/iddim_2, iddim_nostat/), longname='(X,Y) coordinates of monitoring stations', unit=xcoordunit)
          endif
       endif
       if (nostatgl > 0) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMST', ' ', 20          , 1, dimids=(/iddim_nostat/), longname='Name of monitoring station') !CHARACTER
       endif
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'GRDANG', ' ', io_prec       , 0, longname='Edge between y-axis and real north', unit='arc_degrees')
       endif
       if (nostatgl > 0) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'ALFAS', ' ', io_prec     , 1, dimids=(/iddim_nostat/), longname='Orientation ksi-axis w.r.t. pos.x-axis at water level point', unit='arc_degrees')
          if (filetype /= FTYPE_NETCDF) then ! for NetCDF just store the time-dependent version
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'DPS', ' ', io_prec       , 1, dimids=(/iddim_nostat/), longname='Depth in station', unit='m')
          endif
       endif
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'THICK', ' ', io_prec        , 1, dimids=(/iddim_kmax/), longname='Fraction part of layer thickness of total water-height', unit='[ .01*% ]')
       endif
       if (ntruvgl > 0) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'MNTRA', ' ', IO_INT4     , 2, dimids=(/iddim_4, iddim_ntruv/), longname='(M1,N1)-(M2,N2) indices of monitoring cross-sections')
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'XYTRA', ' ', io_fp       , 2, dimids=(/iddim_4, iddim_ntruv/), longname='(X1,Y1)-(X2,Y2) coordinates of monitoring cross-sections', unit=xcoordunit)
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMTRA', ' ', 20         , 1, dimids=(/iddim_ntruv/), longname='Name of monitoring cross-section') !CHARACTER
       endif
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMCON', ' ', 20            , 1, dimids=(/iddim_x/), longname='Name of constituents / turbulent quantities') !CHARACTER
          if (lsed>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'LSED', ' ', IO_INT4      , 0, longname='Number of sediment constituents')
          endif
          if (lsedbl>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'LSEDBL', ' ', IO_INT4    , 0, longname='Number of bedload sediment fractions')
          endif
          if (lsedtot>0) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMSED', ' ', 20         , 1, dimids=(/iddim_lsedtot/), longname='Name of sediment fraction') !CHARACTER
          endif
       endif
       if (zmodel) then
          if (filetype /= FTYPE_NEFIS) then
             call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'ZK_LYR', ' ', io_prec    , 1, dimids=(/iddim_kmax/) , longname='Vertical coordinates of layer centres'   , unit='m', attribs=(/idatt_up/) )
          endif
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'ZK', ' ', io_prec           , 1, dimids=(/iddim_kmax1/), longname='Vertical coordinates of layer interfaces', unit='m', attribs=(/idatt_up/) )
       elseif (filetype /= FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'SIG_LYR' , 'ocean_sigma_coordinate', io_prec        , 1, dimids=(/iddim_kmax/) , longname='Sigma coordinates of layer centres'   , attribs=(/idatt_sigfc/) )
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'SIG_INTF', 'ocean_sigma_coordinate', io_prec        , 1, dimids=(/iddim_kmax1/), longname='Sigma coordinates of layer interfaces', attribs=(/idatt_sigfi/) )
       endif
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'COORDINATES', ' ', 16       , 0, longname='Cartesian or Spherical coordinates') !CHARACTER
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'LAYER_MODEL', ' ', 16       , 0, longname='Sigma-model or Z-model') !CHARACTER
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'OUTPUT_LAYERS', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout/), longname='User selected output layers')
       else
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'KMAXOUT', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout/), longname='User selected output layer interfaces', attribs=(/idatt_cmpintf/) )
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'KMAXOUT_RESTR', ' ', IO_INT4, 1, dimids=(/iddim_kmaxout_restr/), longname='User selected output layer centres', attribs=(/idatt_cmplyr/) )
       endif
       if (nsluv > 0 .and. flwoutput%hisbar) then
          call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMBAR', ' ', 20       , 1, dimids=(/iddim_nsluv/), longname='Barrier names') !CHARACTER
       endif
       !
    case (REQUESTTYPE_WRITE)
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
          ! element 'SELHIS'
          !
          cdum23(1) = selhis
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, cdum23, 'SELHIS')
          if (ierror/=0) goto 9999
          !
          ! element 'NOSTAT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, nostatgl, 'NOSTAT')
          if (ierror/=0) goto 9999
          !
          ! element 'NTRUV'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, ntruvgl, 'NTRUV')
          if (ierror/=0) goto 9999
          !
          ! element 'LSTCI' Variable is now LSTSCI
          !
          idummy(1) = 0
          if ((index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) .and. lstsci>0) idummy(1) = lstsci
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, idummy, 'LSTCI')
          if (ierror/=0) goto 9999
          !
          ! element 'LTUR'
          !
          idummy(1) = 0
          if (index(selhis(13:14), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, idummy, 'LTUR')
          if (ierror/=0) goto 9999
          !
          ! element 'KMAX'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, kmax, 'KMAX')
          if (ierror/=0) goto 9999
          !
          ! element 'GRDANG'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, grdang, 'GRDANG')
          if (ierror/=0) goto 9999
          !
          ! element 'THICK'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, thick, 'THICK')
          if (ierror/=0) goto 9999
       endif ! for NEFIS only
       !
       ! only if nostat > 0 (next 3 elements)
       !
       if (nostatgl > 0) then
          !
          ! element 'MNSTAT'
          !
          if (filetype == FTYPE_NEFIS) then ! for NetCDF just store the time-dependent version
             allocate(ibuff2(2,nostat), stat=istat)
             do k=1,nostat
                !
                ! mnstat contains indices with respect to this partition
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
          endif
          !
          ! element 'XYSTAT': filling of XYSTAT should also be done in case of
          !                   NetCDF output since this array will otherwise
          !                   never be filled for stationary observation points
          !
          do k = 1, nostat
             m              = mnstat(1,k)
             n              = mnstat(2,k)
             xystat(1,k)    = xz(n,m)
             xystat(2,k)    = yz(n,m)
          enddo
          if (filetype == FTYPE_NEFIS) then ! for NetCDF just store the time-dependent version
             allocate(rbuff2(2, nostat), stat=istat)
             do k = 1, nostat
                rbuff2(1:2,k)  = (/xystat(1,k), xystat(2,k)/)
             enddo
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & 2, &
                    & ierror, lundia, rbuff2, 'XYSTAT', station, mergedim=2)
             deallocate(rbuff2, stat=istat)
             if (ierror/=0) goto 9999
          endif
          !
          ! element  'NAMST'
          !
          call wrtarray_n(fds, filename, filetype, grnam2, &
                 & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, namst, 'NAMST')
          if (ierror/=0) goto 9999
          !
          ! element 'ALFAS'
          !
          allocate(rbuff1(nostat), stat=istat)
          do k = 1, nostat
             m = mnstat(1, k)
             n = mnstat(2, k)
             rbuff1(k) = alfas(n, m)
          enddo
          call wrtarray_n(fds, filename, filetype, grnam2, &
                 & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                 & ierror, lundia, rbuff1, 'ALFAS', station)
          deallocate(rbuff1, stat=istat)
          if (ierror/=0) goto 9999
          !
          ! element 'DPS'
          !
          if (filetype /= FTYPE_NETCDF) then ! for NetCDF just store the time-dependent version
             allocate(rbuff1(nostat), stat=istat)
             do k = 1, nostat
                m = mnstat(1, k)
                n = mnstat(2, k)
                rbuff1(k) = real(dps(n, m),fp)
             enddo
             call wrtarray_n(fds, filename, filetype, grnam2, &
                    & 1, nostat, nostatto, nostatgl, order_sta, gdp, &
                    & ierror, lundia, rbuff1, 'DPS', station)
             deallocate(rbuff1, stat=istat)
             if (ierror/=0) goto 9999
          endif
       endif
       !
       ! only if ntruv  > 0
       ! the next element of group 3 will be written
       !
       if (ntruvgl > 0) then
          !
          ! element 'MNTRA'
          !
          if (inode == master) then
             allocate(ibuff2(4,ntruvgl), stat=istat)
             ibuff2 = 0
             if (parll) then
                do k = 1, ntruvgl
                   !
                   ! mnit_global contains the global mn indices of all partitions, before local re-ordering
                   ! so don't use gather_filter
                   !
                   ibuff2(:,k) = gdp%gdparall%mnit_global(:,k)
                enddo
             else
                do k = 1, ntruv
                   !
                   ! mnit is re-ordered; use order_tra to get the original order
                   !
                   ibuff2(:,order_tra(k)) = mnit(:,k)
                enddo
             endif
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, ibuff2, 'MNTRA')
             deallocate(ibuff2, stat=istat)
          endif
          if (ierror/=0) goto 9999
          !
          ! element 'XYTRA'
          !
          allocate(rbuff2(4,ntruv), stat=istat)
          rbuff2 = 0.0_fp
          do k = 1, ntruv
             m1           = mnit(1,k)
             n1           = mnit(2,k)
             m2           = mnit(3,k)
             n2           = mnit(4,k)
             if (kcs(n1,m1)>=0) then
                if (m1 == m2) then ! n1 /= n2
                   if (n1 < n2) n1 = n1-1
                else ! m1 /= m2
                   if (m1 < m2) m1 = m1-1
                endif
                rbuff2(1,k) = xcor(n1,m1)
                rbuff2(2,k) = ycor(n1,m1)
             endif
             if (kcs(n2,m2)>=0) then
                if (m1 == m2) then ! n1 /= n2
                   if (n2 < n1) n2 = n2-1
                else ! m1 /= m2
                   if (m2 < m1) m2 = m2-1
                endif
                rbuff2(3,k) = xcor(n2,m2)
                rbuff2(4,k) = ycor(n2,m2)
             endif
          enddo
          call wrtarray_n(fds, filename, filetype, grnam2, &
                 & 1, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                 & 4, &
                 & ierror, lundia, rbuff2, 'XYTRA', transec, mergedim=2)
          deallocate(rbuff2, stat=istat)
          if (ierror/=0) goto 9999
          !
          ! element 'NAMTRA'
          !
          call wrtarray_n(fds, filename, filetype, grnam2, &
                 & 1, ntruv, ntruvto, ntruvgl, order_tra, gdp, &
                 & ierror, lundia, namtra, 'NAMTRA')
          if (ierror/=0) goto 9999
       endif  ! (ntruvgl > 0)
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          !
          ! only if lmax   > 0 (:= selhis( 5:14) <> 'NNNNNNNNNN')
          !
          if (index(selhis(5:14), 'Y')>0 .or. index(selhis(22:23), 'Y')>0) then
             allocate(namhlp(lstsci+ltur), stat=istat)
             lhlp = 0
             if (index(selhis(5:12), 'Y')>0 .or. index(selhis(22:23), 'Y')/=0) then
                do l = 1, lstsci
                   namhlp(l) = namcon(l)
                enddo
                lhlp = lhlp + lstsci
             endif
             if (index(selhis(13:14), 'Y')>0) then
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
          if (lsedbl>0) then
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, lsedbl, 'LSEDBL')
             if (ierror/=0) goto 9999
          endif
          !
          ! element 'NAMSED'
          !
          if (lsedtot>0) then
             call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, namsed, 'NAMSED')
             if (ierror/=0) goto 9999
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
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          !
          ! element 'COORDINATES'
          !
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
          if (zmodel) then
             if (ztbml) then
                cdum16(1) = 'Z-MODEL, ZTBML'
             else
                cdum16(1) = 'Z-MODEL'
             endif
          else
             cdum16(1) = 'SIGMA-MODEL'
          endif
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, cdum16, 'LAYER_MODEL')
          if (ierror/=0) goto 9999
          !
          ! element 'OUTPUT_LAYERS'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, shlay, 'OUTPUT_LAYERS')
          if (ierror/=0) goto 9999
       else
          !
          ! element 'KMAXOUT'
          !
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, shlay, 'KMAXOUT')
          if (ierror/=0) goto 9999
          !
          ! element 'KMAXOUT_RESTR'
          !
          allocate(ibuff1(kmaxout_restr), stat=istat)
          ibuff1 = shlay_restr-1 ! the compress attribute unfortunately requires starting index equal to 0 like C
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                    & gdp, ierror, lundia, ibuff1, 'KMAXOUT_RESTR')
          deallocate(ibuff1, stat=istat)
          if (ierror/=0) goto 9999
       endif
       !
       if (nsluv > 0 .and. flwoutput%hisbar) then
          call wrtvar(fds, filename, filetype, grnam2, 1, &
                 & gdp, ierror, lundia, nambar, 'NAMBAR')
          if (ierror/=0) goto 9999
       endif
    end select
    deallocate(shlay_restr)
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrihis
