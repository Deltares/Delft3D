subroutine incbc(lundia    ,timnow    ,zmodel    ,nmax      ,mmax      , &
               & kmax      ,kcd       ,nto       ,ntof      ,ntoq      , &
               & kc        ,nrob      ,noroco    , &
               & tprofu    ,itbct     ,mnbnd     ,nob       ,kfumin    , &
               & kfumax    ,kfvmin    ,kfvmax    ,hydrbc    ,circ2d    , &
               & circ3d    ,patm      ,guu       ,gvv       ,gsqs      , &
               & hu        ,hv        ,omega     ,alpha     ,dps       , &
               & z0urou    ,z0vrou    ,qxk       ,qyk       ,s0        , &
               & u0        ,v0        ,grmasu    ,grmasv    ,cfurou    , &
               & cfvrou    ,qtfrac    ,qtfrct    ,qtfrt2    ,thick     , &
               & xz        ,yz        ,qzk       ,alfas                , &
               & dzu1      ,dzv1      ,thklay    ,kcu       ,kcv       , &
               & kfu       ,kfv       ,kcs       ,timhr     ,nambnd    , &
               & typbnd    ,nst       ,gdp       )
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
!  $Id: incbc.f90 7024 2017-02-20 13:43:29Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/incbc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Carry out interpolation in space, determine time
!              increments and updates the hydrodynamic BC
! Method used: - At each time step the increment values (stored
!                in HYDRBC(3/4,N,L)) are added to update HYDRBC
!                (1/2,N,L).
!              - Hereafter space interpolation is applied to cal-
!                culate the boundary values at each point. If
!                frequencies are involved the spatial interpola-
!                tion is carried out for each and every freq.
!                components.
!              - Interpolation depends from the opening type
!              - Smoothing is applied if ITLFSM*DT > or =
!                TSTART-TIMNOW
!              - Smoothing for water level bnd. starts from S0
!              - Smoothing for other type  bnd. starts from 0.0
!              - If Space varying wind is used then the influ-
!                ence of atmospheric pressure at the water-level
!                open boundary can also be included
!                (Z= - (P(N,M)-PAVER)/(AG*RHOW) if PCORR = TRUE)
!              - Vertical profile functions set for velocity
!                and discharge boundary. Profiles are uniform,
!                logarithmic or 3d
!                This function is called even if nto <= 0, since 
!                nto is referred to the number of boundaries in 
!                subdomains in parallel case. When nto <= 0, 
!                nrob should be 0, most part of the function should
!                be skipped.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use dfparall
    use dffunctionals
    use flow_tables
    use globaldata
    use m_openda_exchange_items, only : get_openda_buffer
    !
    implicit none
    !
    ! Enumeration
    !
    integer, parameter :: start_pivot  = 1
    integer, parameter ::   end_pivot  = 2
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                            , pointer :: lunscr
    integer                            , pointer :: itfinish
    integer                            , pointer :: itlfsm
    integer                            , pointer :: julday
    real(fp)                           , pointer :: time_nodal_update_bnd
    real(fp)                           , pointer :: tstart
    real(fp)                           , pointer :: tstop
    real(fp)                           , pointer :: dt
    real(fp)                           , pointer :: tunit
    integer                            , pointer :: lunbct
    integer                            , pointer :: lunbcq
    real(fp)                           , pointer :: rhow
    real(fp)                           , pointer :: ag
    real(fp)                           , pointer :: z0
    real(fp)                           , pointer :: z0v
    integer                            , pointer :: iro
    real(fp)                           , pointer :: paver
    real(fp)                           , pointer :: thetqh
    logical                            , pointer :: use_zavg_for_qtot
    logical                            , pointer :: pcorr
    real(fp), dimension(:,:,:)         , pointer :: rttfu
    real(fp), dimension(:,:,:)         , pointer :: rttfv
    logical                            , pointer :: relxqh
    type (handletype)                  , pointer :: fbcrfile
    type (fbcrbndtype)  , dimension(:) , pointer :: fcrbnd
    logical                            , pointer :: fbccorrection
    real(fp), dimension(:,:)           , pointer :: dist_pivot_part
    logical                            , pointer :: distr_qtq
    logical                            , pointer :: distr_qtq_per
    logical                            , pointer :: distr_bdl_per
    logical                            , pointer :: distr_qtqNNprism
    real(fp), dimension(:)             , pointer :: cwidth
    real(fp), dimension(:)             , pointer :: zavg    
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    integer                   , pointer :: cutcell
    logical                   , pointer :: floodplain_inflow
    integer, dimension(:,:)   , pointer :: kfs_cc
    logical                   , pointer :: periodSURFACE
    logical                   , pointer :: PERIODICwaterDEPTH
    integer                   , pointer :: nrPER
    real(fp), dimension(:,:,:), pointer :: INTx_GRS
    real(fp), dimension(:,:,:), pointer :: INTy_GRS
    real(fp), dimension(:,:)  , pointer :: xG
    real(fp), dimension(:,:)  , pointer :: yG
    integer, dimension(:,:)   , pointer :: Ndry_GRS
    real(fp), dimension(:,:)  , pointer :: poros
    real(fp), dimension(:,:)  , pointer :: dpL
    real(fp), dimension(:,:)  , pointer :: dpH
    real(fp)                  , pointer :: perSMOfac
    integer, dimension(:)     , pointer :: mPH_ext
    integer, dimension(:)     , pointer :: nPH_ext
    integer, dimension(:)     , pointer :: mPQ_ext
    integer, dimension(:)     , pointer :: nPQ_ext
    integer, dimension(:)     , pointer :: mPH_int
    integer, dimension(:)     , pointer :: mPQ_int
    integer, dimension(:)     , pointer :: nPH_int
    integer, dimension(:)     , pointer :: nPQ_int
    integer                   , pointer :: distQHm
    integer                   , pointer :: distQHn
    real(fp), dimension(:,:)  , pointer :: qfilt
    real(fp)                  , pointer :: reltim_qtq
    logical                   , pointer :: perCIRC
    real(fp)                  , pointer :: distanceBOUNDper
    logical                   , pointer :: bedPERIODIC
    real(fp), dimension(:)    , pointer :: qfilt_s1
    real(fp)                  , pointer :: reltim_s1
    logical                   , pointer :: prescrDEPTH
    logical                   , pointer :: use_DPSavg_for_qtot
    logical                   , pointer :: zavg_global
!
! Global variables
!
    integer                                                            , intent(in)  :: nst    !  Description and declaration in dimens.igs
    integer                                                            , intent(in)  :: kc     !  Description and declaration in dimens.igs
    integer                                                                          :: kcd    !  Description and declaration in dimens.igs
    integer                                                                          :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: lundia !  Description and declaration in inout.igs
    integer                                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in)  :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in)  :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in)  :: ntof   !  Description and declaration in dimens.igs
    integer                                                            , intent(in)  :: ntoq   !  Description and declaration in dimens.igs
    integer , dimension(7, nto)                                        , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(5, nto)                                                      :: itbct  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(8, nrob)                                       , intent(in)  :: nob    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    logical                                                            , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                                                           , intent(in)  :: timhr  !!  Current timestep (in hours) TIMNOW * 2 * HDT / 3600. 
    real(fp)                                                                         :: timnow !!  Current timestep (multiples of dt)
    real(fp), dimension(4, noroco)                                                   :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, nto, kcd)                                                 :: hydrbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: grmasu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: grmasv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: gsqs 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: alfas 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: xz 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: yz
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)    , intent(inout) :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: patm   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)   , intent(in)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)   , intent(in)  :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,0:kmax), intent(in) :: qzk  
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kc)                                            , intent(in)  :: omega  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                          , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                        :: thklay
    real(fp), dimension(kmax, 2, noroco)                                             :: circ3d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nrob)                                                        :: qtfrac !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                           , intent(in)  :: alpha  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                                         :: qtfrct !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                                         :: zbfrct
    real(fp), dimension(nto)                                                         :: zbavg
    real(fp), dimension(nto)                                                         :: qtfrt2 !  Description and declaration in esm_alloc_real.f90
    character(20), dimension(nto)                                                    :: tprofu !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                                      , intent(in)  :: nambnd !  Description and declaration in esm_alloc_char.f90
    character(1) , dimension(nto)                                      , intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                             :: mq,nq,Ndry !,nH,mH
    integer                             :: i
    integer                             :: ibtype         ! Type of open boundary: (see global var. NOB) 
    integer                             :: incx           ! Nr. of grid points-1 (in the x-dir.) between the begin and the end point of an opening section 
    integer                             :: incy           ! Nr. of grid points-1 (in the y-dir.) between the begin and the end point 
    integer                             :: ito            ! Index number of open boundary loc. 
    integer                             :: j              ! Loop variable 
    integer                             :: k              ! Loop variable 
    integer                             :: k1st
    integer                             :: k2nd
    integer                             :: kcsi           ! Local value of kcs(npbi, mpbi), 1 for boundary points in the partition, -1 for halo points.
    integer                             :: kfuv           ! Value of KCU or KCV in boundary point
    integer                             :: kp             ! First array index of array CIRC2/3D pointing to the nr. of row/column in array IROCOL
    integer                             :: kpc            ! First array index of array CIRC2/3D pointing to the column number in arra IROCOL 
    integer                             :: kpp            ! Hulp varible 
    integer                             :: kpr            ! First array index of array CIRC2/3D pointing to the row number in array IROCOL 
    integer                             :: kq             ! Second array index of array CIRC2/3D pointing to the nr. of row/column in array IROCOL 
    integer                             :: kqc            ! Second array index of array CIRC2/3D pointing to the column number in arra IROCOL 
    integer                             :: kqq            ! Hulp varible 
    integer                             :: kqr            ! Second array index of array CIRC2/3D pointing to the row number in array IROCOL 
    integer                             :: lunsol
    integer                             :: maxinc         ! Max. of (INCX,INCY,1) 
    integer                             :: mend           ! End coord. (in the x-dir.) of an open bound. section 
    integer                             :: mgg            ! M-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering 
    integer                             :: mp
    integer                             :: mpbAL          ! M index of 1st velocity point inside domain ALigned with the direction of the boundary discharge
    integer                             :: mpbORT         ! M index of the first of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge
    integer                             :: mpbORTm1       ! M index of the second of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge (if this is the orthogonal direction it is lowered by 1)
    integer                             :: npbORT         ! N index of the first of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge
    integer                             :: npbORTm1       ! N index of the second of the two velocity points just inside the domain and ORThogonal to the direction of the boundary discharge (if this is the orthogonal direction it is lowered by 1)
    integer                             :: mpbi           ! M index of 1st water level point inside domain
    integer                             :: mpbt           ! M index of boundary velocity point
    integer                             :: msta           ! Starting coord. (in the x-dir.) of an open bound. section 
    integer                             :: n              ! Loop variable 
    integer                             :: n1             ! Pointer var. relating NOB to MNBND 
    integer                             :: nend           ! End coord. (in the y-dir.) of an open bound. section 
    integer, external                   :: newlun
    integer                             :: ngg            ! N-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering 
    integer                             :: np
    integer                             :: npbAL           ! N index of 1st velocity point inside domain
    integer                             :: npbi           ! N index of 1st water level point inside domain
    integer                             :: npbt           ! N index of boundary velocity point
    integer                             :: nsta           ! Starting coord. (in the y-dir.) of an open bound. section 
    integer                             :: ntoftoq        ! Offset for open boundary sections of the Time-series type (NTOF+NTOQ) 
    integer                             :: posrel         ! code denoting the position of the open boundary, related to the complete grid
    integer                             :: lb             ! lowerboundary of loopcounter
    integer                             :: ub             ! upperboundary of loopcounter
    integer                             :: Ndryp1
    integer                             :: cont
    logical                             :: NOTfirst
    logical,SAVE                        :: firstCALL = .TRUE.
    logical                             :: error          ! errorstatus
    logical                             :: horiz          ! Flag=TRUE if open boundary lies parallel to x-/KSI-dir. 
    logical                             :: posdir
    logical                             :: udir
    logical                             :: vdir
    logical                             :: foundPER
    logical                             :: NMcut
    logical                             :: neumDWNSTRM
    real(fp)                            :: hnew,hini,hnewSMO
    real(fp)                            :: zz,slope,rr,xgg,ygg,xggH,yggH,areaDRY,xgCOMP,ygCOMP,angFIRSTbaric,angLASTbaric
    real(fp)                            :: distBOUND
    real(fp)                            :: distINTERNAL
    real(fp)                            :: dz
    real(fp)                            :: bedELEV
    real(fp)                            :: slopex
    real(fp)                            :: slopey
    real(fp)                            :: distGx
    real(fp)                            :: distGy
    real(fp)                            :: distGm
    real(fp)                            :: distGn
    real(fp)                            :: slopeM
    real(fp)                            :: amplik
    real(fp)                            :: alfa
    real(fp)                            :: angle          ! The actual phase of the 'Harmonics' at this time step 
    real(fp)                            :: czbed
    real(fp)                            :: czeff
    real(fp)                            :: diff           ! Difference between the actual bounda- ry value and the initial value at the openings 
    real(fp)                            :: dini           ! Initial value of the prescribed sig- nal at open boundary. For water ele- cation type opening DINI = S0. For Other opening types DINI = 0.0 
    real(fp)                            :: dist           ! Real distance between an open bounda- ry point to the begin point of the related opening section 
    real(fp)                            :: distx          ! Incremental distance (in the x-dir.) between two consecutive open boundary points belonging to the same section 
    real(fp)                            :: disty          ! Incremental distance (in the x-dir.) between two consecutive open boundary points belonging to the same section 
    real(fp)                            :: dpvel
    real(fp)                            :: dz0
    real(fp)                            :: dz1
    real(fp)                            :: frac           ! Fraction between DIST and the total length of an opening section 
    real(fp)                            :: fbcr_array(2)  ! Corrective flow boundary conditions array
    real(fp)                            :: guuz1
    real(fp)                            :: guuz2
    real(fp)                            :: gvvz1
    real(fp)                            :: gvvz2
    real(fp)                            :: grmass
    real(fp)                            :: h0             ! Total depth in velocity point of open boundary point 
    real(fp)                            :: hu0            ! Total depth in velocity point of open boundary U-point. MAX (HU,0.01) 
    real(fp)                            :: hv0            ! Total depth in velocity point of open boundary V-point. MAX (HV,0.01) 
    real(fp)                            :: pcr
    real(fp)                            :: pdiff
    real(fp)                            :: phasek
    real(fp)                            :: q
    real(fp)                            :: a
    real(fp)                            :: qz
    real(fp)                            :: q0avg
    real(fp)                            :: qtfrc
    real(fp)                            :: sig1           ! Layer thickness as fraction of previous layer 
    real(fp)                            :: sig2           ! Layer thickness as fraction 
    real(fp)                            :: tcur           ! Current time in hours since last nodal update time
    real(fp)                            :: tdif           ! Time difference (in minutes) between TIMNOW and TSTART 
    real(fp)                            :: tfrac          ! Fraction of TDIF and Smoothing time 
    real(fp)                            :: tfracper       ! Fraction of TDIF and Smoothing time for periodical BC
    real(fp)                            :: thickOpen      ! When mnbnd(5,n) <> 0: sum of thickness of all open layers
    real(fp)                            :: timscl         ! Multiple factor to create minutes from read times 
    real(fp)                            :: totl           ! Actual length of an openbnd. section 
    real(fp)                            :: ttfhsum        ! Temporary variable for depth-averaging RTTFU/V resistance
    real(fp)                            :: width
    real(fp)                            :: wlvl
    real(fp)                            :: z1             ! Previous layer: (1+SIG1)*H0 
    real(fp)                            :: z2             ! Currect layer: (1+SIG2)*H0 
    real(fp)                            :: zbulk          ! Sommation of all layers ZLAYER*THICK 
    real(fp)                            :: zl             ! Z for layer: (Z1+Z2)/2. 
    real(fp)                            :: zlayer         ! Z layer: LOG (1.+ZL/Z0) 
    real(fp)                            :: itlfsm_per
    real(fp)                            :: Lchan
    real(fp)                            :: wzb
    real(fp)                            :: zavg_gl
    real(fp)                            :: zbavg_gl
    real(fp)                            :: Q_bnd
    real(fp), dimension(kmax,nrob)      :: qtfracV
    real(fp), dimension(:), allocatable :: qtfrct_global  ! work array
    real(fp), dimension(6)              :: polyx
    real(fp), dimension(6)              :: polyy
    integer                             :: nobcgl         ! global number of open boudnaries (i.e. original number excluding duplicate open boudnaries located in the halo regions)
    integer                             :: nobcto         ! total number of open boundaries (including "duplicate" open boudnaries located in halo regions)
    integer                             :: istat
!
!! executable statements -------------------------------------------------------
!
    aguu                => gdp%gdimbound%aguu
    agvv                => gdp%gdimbound%agvv
    cutcell             => gdp%gdimbound%cutcell
    floodplain_inflow   => gdp%gdimbound%floodplain_inflow
    kfs_cc              => gdp%gdimbound%kfs_cc
    periodSURFACE       => gdp%gdimbound%periodSURFACE
    PERIODICwaterDEPTH  => gdp%gdimbound%PERIODICwaterDEPTH
    nrPER               => gdp%gdimbound%nrPER
    INTx_GRS            => gdp%gdimbound%INTx_GRS
    INTy_GRS            => gdp%gdimbound%INTy_GRS
    xG                  => gdp%gdimbound%xG
    yG                  => gdp%gdimbound%yG
    Ndry_GRS            => gdp%gdimbound%Ndry_GRS
    poros               => gdp%gdimbound%poros
    dpL                 => gdp%gdimbound%dpL
    dpH                 => gdp%gdimbound%dpH
    perSMOfac           => gdp%gdimbound%perSMOfac
    mPH_ext             => gdp%gdimbound%mPH_ext
    nPH_ext             => gdp%gdimbound%nPH_ext
    mPQ_ext             => gdp%gdimbound%mPQ_ext
    nPQ_ext             => gdp%gdimbound%nPQ_ext
    mPH_int             => gdp%gdimbound%mPH_int
    mPQ_int             => gdp%gdimbound%mPQ_int
    nPH_int             => gdp%gdimbound%nPH_int
    nPQ_int             => gdp%gdimbound%nPQ_int
    mPH_ext             => gdp%gdimbound%mPH_ext
    distQHm             => gdp%gdimbound%distQHm
    distQHn             => gdp%gdimbound%distQHn
    qfilt               => gdp%gdimbound%qfilt
    reltim_qtq          => gdp%gdimbound%reltim_qtq
    perCIRC             => gdp%gdimbound%perCIRC
    distanceBOUNDper    => gdp%gdimbound%distanceBOUNDper
    bedPERIODIC         => gdp%gdimbound%bedPERIODIC
    qfilt_s1            => gdp%gdimbound%qfilt_s1
    reltim_s1           => gdp%gdimbound%reltim_s1
    prescrDEPTH         => gdp%gdimbound%prescrDEPTH
    use_DPSavg_for_qtot => gdp%gdimbound%use_DPSavg_for_qtot
    zavg_global         => gdp%gdimbound%zavg_global
    relxqh                => gdp%gdincbc%relxqh
    paver                 => gdp%gdnumeco%paver
    thetqh                => gdp%gdnumeco%thetqh
    use_zavg_for_qtot     => gdp%gdnumeco%use_zavg_for_qtot
    pcorr                 => gdp%gdnumeco%pcorr
    rhow                  => gdp%gdphysco%rhow
    ag                    => gdp%gdphysco%ag
    z0                    => gdp%gdphysco%z0
    z0v                   => gdp%gdphysco%z0v
    iro                   => gdp%gdphysco%iro
    lunbct                => gdp%gdluntmp%lunbct
    lunbcq                => gdp%gdluntmp%lunbcq
    tstart                => gdp%gdexttim%tstart
    tstop                 => gdp%gdexttim%tstop
    dt                    => gdp%gdexttim%dt
    tunit                 => gdp%gdexttim%tunit
    itfinish              => gdp%gdinttim%itfinish
    itlfsm                => gdp%gdinttim%itlfsm
    julday                => gdp%gdinttim%julday
    time_nodal_update_bnd => gdp%gdinttim%time_nodal_update_bnd
    rttfu                 => gdp%gdtrachy%rttfu
    rttfv                 => gdp%gdtrachy%rttfv
    fbcrfile              => gdp%gdflwpar%fbcrfile
    fcrbnd                => gdp%gdflwpar%fcrbnd
    fbccorrection         => gdp%gdflwpar%fbccorrection
    dist_pivot_part       => gdp%gdbcdat%dist_pivot_part
    distr_bdl_per         => gdp%gdbcdat%distr_bdl_per
    distr_qtq             => gdp%gdbcdat%distr_qtq
    distr_qtq_per         => gdp%gdbcdat%distr_qtq_per
    distr_qtqNNprism      => gdp%gdbcdat%distr_qtqNNprism
    lunscr                => gdp%gdinout%lunscr
        
    !
    ! initialize local parameters
    ! omega in deg/hour & time in seconds !!, alfa = in minuten
    ! TIMSCL will not been used in UPDBCC
    !
    horiz   = .false.
    udir    = .false.
    vdir    = .false.
    ntoftoq = ntof + ntoq
    timscl  = 1.0_fp
    tcur    = (timnow - time_nodal_update_bnd)*dt*tunit/3600.0_fp
    !
    k1st  = -999
    k2nd  = -999
    dpvel = -999.0_fp
    !
    if (parll) then 
       !
       ! Recalculates the effective global number of open boundary conditions
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, nto, nobcto, nobcgl,  gdp%gdbcdat%bct_order, gdp)
    else
       nobcto = nto
       nobcgl = nto
    endif
    !
    ! calculate zavg, using cwidth
    !
    if (.not.associated(gdp%gdincbc%cwidth)) then
       allocate(gdp%gdincbc%cwidth(nto), stat=istat)
       if (istat == 0) allocate(gdp%gdincbc%zavg(nto), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in incbc')
          call d3stop(1, gdp)
       endif
    endif
    cwidth => gdp%gdincbc%cwidth
    zavg   => gdp%gdincbc%zavg
    !
    qtfrct = 0.0_fp
    zbfrct = 0.0_fp
    qtfracV= 0.0_fp
    cwidth = 1.0e-9_fp
    !
    do n = 1, nrob
       !
       ! only for total discharge boundaries (nob=7) and for water level (nob=2)
       ! Determine average water level (nob=7) or average bed elavation (nob=2)
       !
       if (nob(3,n) /= 7.AND.(.not.(nob(3,n) == 2 .and. prescrDEPTH))) then !
          cycle
       endif
       qtfrac(n) = 0.0_fp
       n1        = nob(8,n)
       mpbt      = nob(1,n)
       npbt      = nob(2,n)
       if (nob(4, n)==2) then
          mpbt = mpbt - 1
          mpbi = mpbt
       elseif (nob(4, n)==1) then
          mpbi = mpbt + 1
       else
          mpbi = mpbt
       endif
       if (nob(6, n)==2) then
          npbt = npbt - 1
          npbi = npbt
       elseif (nob(6, n)==1) then
          npbi = npbt + 1
       else
          npbi = npbt
       endif
       !
       ! Determine direction dependent parameters
       !
       if (nob(4,n) > 0) then
          udir = .true.
          vdir = .false.
          wlvl = s0(npbi, mpbi)
          if (nob(3,n) == 2) then
             wzb  = dps(npbi, mpbi)
          endif
          if (kfu(npbt,mpbt) == 1 .and. kcs(npbi,mpbi) == 1) then
             width = guu(npbt,mpbt)*aguu(npbt,mpbt)
          else
             width = 0.0_fp
          endif
          if (cutcell==2) then
              if (.not.floodplain_inflow .and. kfs_cc(npbi,mpbi)==2) then
                  width = 0.0_fp !aguu(npbt,mpbt) = 0.0_fp
              endif
          endif
       elseif (nob(6,n) > 0) then
          udir = .false.
          vdir = .true.
          wlvl = s0(npbi, mpbi)
          if  (nob(3,n) == 2) then
             wzb  = dps(npbi, mpbi)
          endif
          if (kfv(npbt,mpbt) == 1 .and. kcs(npbi,mpbi) == 1) then
             width = gvv(npbt,mpbt)*agvv(npbt,mpbt)
          else
             width = 0.0_fp
          endif
          if (cutcell==2) then
              if (.not.floodplain_inflow .and. kfs_cc(npbi,mpbi)==2) then
                 width = 0.0_fp !agvv(npbt,mpbt) = 0.0_fp
              endif
          endif
       else
       endif
       !
       if  (nob(3,n) == 2) zbfrct(n1) = zbfrct(n1) + wzb*width
       qtfrct(n1) = qtfrct(n1) + wlvl*width
       cwidth(n1) = cwidth(n1) + width
    enddo
    !
    ! accumulate information across MPI partitions
    !
    if (parll) then
       call dfsync(gdp)
       allocate( qtfrct_global(nobcgl), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in incbc')
          call d3stop(1, gdp)
       endif
       !
       ! exchange cwidth data
       !
       qtfrct_global = 0.0_fp
       call dfgather_filter(lundia, nto, nobcto, nobcgl, gdp%gdbcdat%bct_order, cwidth, qtfrct_global, gdp, filter_op=FILTER_SUM)
       call dfbroadc_gdp(qtfrct_global, nobcgl, dfloat, gdp)
       do n1 = 1, nto
          if(typbnd(n1) == 'T') then
             cwidth(n1) = qtfrct_global(gdp%gdbcdat%bct_order(n1))
          endif
       enddo
       !
       ! exchange qtfrct data
       !
       qtfrct_global = 0.0_fp
       call dfgather_filter(lundia, nto, nobcto, nobcgl, gdp%gdbcdat%bct_order, qtfrct, qtfrct_global, gdp, filter_op=FILTER_SUM)
       call dfbroadc_gdp(qtfrct_global, nobcgl, dfloat, gdp)
       do n1 = 1, nto
          if(typbnd(n1) == 'T') then
             qtfrct(n1) = qtfrct_global(gdp%gdbcdat%bct_order(n1))
          endif
       enddo
       !
       if (allocated(qtfrct_global)) deallocate(qtfrct_global, stat=istat)
    endif
    !
    ! calculate total discharge fractions
    ! calculate qtot for QH boundaries
    !
    do n1 = 1, nto
       zbavg(n1) = zbfrct(n1)/cwidth(n1) !to be parallelized
       zavg(n1) = qtfrct(n1)/cwidth(n1)
       qtfrct(n1) = 0.0_fp
    enddo
    !
    NOTfirst  = .true.
10  continue
    !
    ! Needed for smoothing
    !
    Q_bnd = 0.0_fp 
    do n = 1, nrob
       !
       ! Only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       ! for now exclude water level boundaries (2) 
       ! (that can be of type QH, otherwise for qtq_per it is going out of array when doing: 
       ! qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
       !
       if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then
          cycle
       endif
       qtfrac(n) = 0.0
       n1        = nob(8, n)
       mpbt      = nob(1, n)
       npbt      = nob(2, n)
       if (nob(4, n)==2) then
          mpbt = mpbt - 1
          mpbi = mpbt
          mpbAL = mpbt - 1
          mpbORT = mpbAL
          mpbORTm1 = mpbAL 
          posdir = .false.
       elseif (nob(4, n)==1) then
          mpbi = mpbt + 1
          mpbAL = mpbt + 1
          mpbORT = mpbAL
          mpbORTm1 = mpbAL 
          posdir = .true.
       else
          mpbi = mpbt
          mpbAL = mpbt
          mpbORT = mpbAL
          mpbORTm1 = mpbORT-1
       endif
       if (nob(6, n)==2) then
          npbt = npbt - 1
          npbi = npbt
          npbAL = npbt - 1
          npbORT = npbAL
          npbORTm1 = npbAL
          posdir = .false.
       elseif (nob(6, n)==1) then
          npbi = npbt + 1
          npbAL = npbt + 1
          npbORT = npbAL
          npbORTm1 = npbAL
          posdir = .true.
       else
          npbi = npbt
          npbAL = npbt
          npbORT = npbAL
          npbORTm1 = npbORT-1
       endif
       !
       ! Determine direction dependent parameters
       !
       ttfhsum = 0.0
       q = 0._fp
       kcsi  = kcs(npbi, mpbi)
       if (nob(4,n) > 0) then
          udir  = .true.
          vdir  = .false.
          if (use_zavg_for_qtot) then
             dpvel = max(0.0_fp, hu(npbt, mpbt)-s0(npbt, mpbt)+zavg(n1))
          else
             dpvel = max(0.0_fp, hu(npbt, mpbt))
          endif
          width = guu(npbt, mpbt)*aguu(npbt, mpbt)      
          if (cutcell==2) then
              if (.not.floodplain_inflow .and. kfs_cc(npbi,mpbi)==2) then
                  width = 0.0_fp !aguu(npbt,mpbt) = 0.0_fp
              endif
          endif                
          czbed = cfurou(npbt, mpbt, 1)
          kfuv  = kfu(npbt, mpbt)
          !
          ! Determine depth-averaged vegetation effect
          !
          if (zmodel) then
             k1st = kfumin(npbt, mpbt)
             k2nd = kfumax(npbt, mpbt)
             do k = k1st, k2nd
                ttfhsum = ttfhsum + rttfu(npbt, mpbt, k)*dzu1(npbt, mpbt, k)
             enddo
          else
             do k = 1, kmax
                ttfhsum = ttfhsum + rttfu(npbt, mpbt, k)*thick(k)
             enddo
             ttfhsum = ttfhsum * dpvel
          endif
          !
          ! qxk should be zero for z-layers out of range
          ! needed for smoothing
          !
          Q_bnd = Q_bnd + sum(qxk(npbt, mpbt, 1:kmax))    
          if (distr_qtq) then
             do k = 1, kmax
                qz = 0._fp
                if (distr_qtqNNprism) then 
                   !
                   ! Add them even if negative
                   !
                   qz = qz + qyk(npbORT, mpbORT, k) - qyk(npbORTm1, mpbORTm1, k) 
                endif
                !
                ! The if can be removed, I am not sure if qzk are initialized to zero for 2D simulation
                ! To be corrected: qzk has w1, that is the sigma coordinate velocity. the physical velocity is wphy and we have to compute a qzk_phy
                !
                if (distr_qtqNNprism) then 
                   !
                   ! Add them even if negative
                   !
                   if (kmax.gt.1) qz = qz + qzk(npbAL, mpbAL, k-1) - qzk(npbAL, mpbAL, k) 
                endif
                if (posdir .and. qxk(npbAL, mpbAL, k)>0.0) then
                   qz = qz + qxk(npbAL, mpbAL, k)
                   qtfracV(k,n) = qz
                elseif (.not.posdir .and. qxk(npbAL, mpbAL, k)<0.0) then
                   qz = qz - qxk(npbAL, mpbAL, k)
                   qtfracV(k,n) = qz
                else
                   !
                   ! If flux is exiting from the domain, no discharge enters from that layer
                   !
                   qtfracV(k,n) = 0._fp 
                endif
                q = q + qz
             enddo     
             if (reltim_qtq>0) then
                if (.NOT.firstCALL) then
                   do k=1,kmax
                      !
                      ! They are both in minutes
                      !
                      a = exp( - (0.5_fp*dt)/reltim_qtq) 
                      qfilt(k,n) = a*qfilt(k,n) + (1._fp - a)*qtfracV(k,n)
                      qtfracV(k,n) = qfilt(k,n)
                   enddo
                   q = sum(qtfracV(:,n))  
                else
                   qfilt(1:kmax,n) = qtfracV(1:kmax,n)
                endif
             endif              
!
          elseif (distr_qtq_per) then
             do k = 1, kmax
                !
                ! distQHm,distQHn: shift in m,n between the Q (internal)  boundary and the H (internal) periodic boundary
                !
                qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k) 
                if (reltim_qtq>0) then
                   if (.NOT.firstCALL) then
                      a = exp( - (0.5_fp*dt)/reltim_qtq) !they are both in minutes
                      qfilt(k,n) = a*qfilt(k,n) + (1._fp - a)*qtfracV(k,n)
                      qtfracV(k,n) = qfilt(k,n)
                   else
                      qfilt(k,n) = qtfracV(k,n)
                   endif
                endif
             enddo
             q = sum(qtfracV(:,n))
          endif
       elseif (nob(6,n) > 0) then
          udir  = .false.
          vdir  = .true.
          if (use_zavg_for_qtot) then
             dpvel = max(0.0_fp, hv(npbt, mpbt)-s0(npbt, mpbt)+zavg(n1))
          else
             dpvel = max(0.0_fp, hv(npbt, mpbt))
          endif
          width = gvv(npbt, mpbt)*agvv(npbt,mpbt)
          if (cutcell==2) then
              if (.not.floodplain_inflow .and. kfs_cc(npbi,mpbi)==2) then
                 width = 0.0_fp !agvv(npbt,mpbt) = 0.0_fp
              endif
          endif
          czbed = cfvrou(npbt, mpbt, 1)
          kfuv  = kfv(npbt, mpbt)
          !
          ! Determine depth-averaged vegetation effect
          !
          if (zmodel) then
             k1st = kfvmin(npbt, mpbt)
             k2nd = kfvmax(npbt, mpbt)
             do k = k1st, k2nd
                ttfhsum = ttfhsum + rttfv(npbt, mpbt, k)*dzv1(npbt, mpbt, k)
             enddo
          else
             do k = 1, kmax
                ttfhsum = ttfhsum + rttfv(npbt, mpbt, k)*thick(k)
             enddo
             ttfhsum = ttfhsum * dpvel
          endif
          !
          ! qyk should be zero for z-layers out of range
          !
          Q_bnd = Q_bnd + sum(qyk(npbt, mpbt, 1:kmax))    ! needed for smoothing
          if (distr_qtq) then
             do k = 1, kmax
                qz = 0._fp
                if (distr_qtqNNprism) then 
                   !
                   ! Add them even if negative
                   !
                   qz = qz + qxk(npbORT, mpbORT, k) - qxk(npbORTm1, mpbORTm1, k)
                endif
                !
                ! The if can be removed, I am not sure if qzk are initialized to zero for 2D simulation
                ! To be corrected: qzk has w1, that is the sigma coordinate velocity. 
                ! The physical velocity is wphy and I have to compute a qzk_phy
                !
                if (distr_qtqNNprism) then 
                   !
                   ! Add them even if negative
                   !
                   if (kmax.gt.1) qz = qz + qzk(npbAL, mpbAL, k-1) - qzk(npbAL, mpbAL, k) 
                endif
                if (posdir .and. qyk(npbAL, mpbAL, k)>0.0) then
                   qz = qz + qyk(npbAL, mpbAL, k)
                   qtfracV(k,n) = qz
                elseif (.not.posdir .and. qyk(npbAL, mpbAL, k)<0.0) then
                   qz = qz - qyk(npbAL, mpbAL, k)
                   qtfracV(k,n) = qz
                else
                   !
                   ! If flux is exiting from the domain, no discharge enters from that layer
                   !
                   qtfracV(k,n) = 0._fp 
                endif
                q = q + qz
             enddo      
             if (reltim_qtq>0) then
                if (.NOT.firstCALL) then
                   do k=1,kmax
                      !
                      ! They are both in minutes
                      !
                      a = exp( - (0.5_fp*dt)/reltim_qtq) 
                      qfilt(k,n) = a*qfilt(k,n) + (1._fp - a)*qtfracV(k,n)
                      qtfracV(k,n) = qfilt(k,n)
                   enddo
                   q = sum(qtfracV(:,n))  
                else
                   qfilt(1:kmax,n) = qtfracV(1:kmax,n)
                endif
             endif  
             
          elseif (distr_qtq_per) then
             !
             ! To me moved in rdgrid when distr_qtq_per will be read there
             !
             if (.not.periodSURFACE.and. (distr_qtq_per.OR.distr_bdl_per)) then 
                write(*,*) 'Flow is not periodic, distr_qtq_per has to be false'
                !pause
                stop
             endif
             do k = 1, kmax
                !
                ! distQHm,distQHn: shift in m,n between the Q (internal)  boundary and the H (internal) periodic boundary
                !
                qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
                if (reltim_qtq>0) then
                   if (.NOT.firstCALL) then
                      !
                      ! They are both in minutes
                      !
                      a = exp( - (0.5_fp*dt)/reltim_qtq) 
                      qfilt(k,n) = a*qfilt(k,n) + (1._fp - a)*qtfracV(k,n)
                      qtfracV(k,n) = qfilt(k,n)
                   else
                      qfilt(k,n) = qtfracV(k,n)
                   endif
                endif
             enddo
             q = sum(qtfracV(:,n))
          endif
       else
       endif
       !
       ! With abs we accept also negative discharges
       !
       qtfracV(1:kmax,n) = qtfracV(1:kmax,n)/(sign(1._fp,q)*max(abs(q),0.00000000000001_fp))
       !
       if (nob(3,n) == 7) then
          !
          ! Part of total discharge boundary, compute B*h^(1.5)*C
          !
          if ((distr_qtq.or.distr_qtq_per) .and. NOTfirst) then
              !
              ! Distribution based on simulated distribution of q inside domain
              !
              qtfrac(n) = q
          else
              !
              ! Distribution based on B*h^(1.5)*C
              !
              if (kfuv == 0) then
                 qtfrac(n)  = 0.0_fp
              else
                 ! Determine effective roughness
                 ! Note: czbed contains Chezy/sqrt(ag) !!
                 !
                 czeff      = czbed / sqrt(1.0_fp + 0.5_fp*ttfhsum*czbed*czbed)
                 !
                 !  This leads to oscillations parallel to the open boundary:
                 ! 
                 qtfrac(n)  = (dpvel**1.5_fp) * width * czeff
                 !
                 !  Alternative (more robust?) implementation is switched off
                 !
                 !  qtfrac(n)  = dpvel*width
              endif
          endif
          if (kcsi==1) then ! only sum up discharge weights for boundary cells strictly inside this partition
             qtfrct(n1) = qtfrct(n1) + qtfrac(n)
          endif          

       elseif (nob(3,n) == 2) then
          !
          ! waterlevel boundary might be QH boundary
          !
          if ((n1>ntof) .and. (n1<=ntof + ntoq)) then
             !
             ! QH boundary, compute Q = SUM (B*u*h)
             ! USE 1 to KMAX for the loop index for ZMODEL and SIGMA
             ! No differentiation in the approach is needed because
             ! QXK/QYK = zero at the top layers anyway
             !
             ! Only sum up discharge for boundary cells strictly inside this partition
             !
             if (kcsi==1) then 
                q0avg = 0.0
                if (udir) then
                   do k = 1, kmax
                      q0avg = q0avg + qxk(npbt, mpbt, k)
                   enddo
                elseif (vdir) then
                   do k = 1, kmax
                      q0avg = q0avg + qyk(npbt, mpbt, k)
                   enddo
                else
                endif
                qtfrct(n1) = qtfrct(n1) + q0avg
             endif
          endif
       else
       endif
    enddo
    !
    ! Merging  of small cut edges for any type of distribution, also for distribution based on B*h^(1.5)*C
    !
    if (cutcell>0) call mergSMALLbound(guu,gvv,nrob,nob,qtfrac,qtfracV,kmax,gdp%d%mlb,gdp%d%mub,gdp%d%nlb,gdp%d%nub, gdp)  
    !
    ! Update the discharge for total discharge or QH boundaries for the overall domain by summing up among those
    !
    if (parll) then
       call dfsync(gdp)
       allocate( qtfrct_global(nobcgl), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'P004', 'memory alloc error in incbc')
          call d3stop(1, gdp)
       endif
       qtfrct_global = 0.0_fp
       call dfgather_filter(lundia, nto, nobcto, nobcgl, gdp%gdbcdat%bct_order, qtfrct, qtfrct_global, gdp, filter_op=FILTER_SUM)
       call dfbroadc_gdp(qtfrct_global, nobcgl, dfloat, gdp)
       do n1 = 1, nto
          !
          ! Total discharge or QH boundary
          !
          if(typbnd(n1)=='T' .or. ((n1>ntof) .and. (n1<=ntof + ntoq))) then  
             qtfrct(n1) = qtfrct_global(gdp%gdbcdat%bct_order(n1))
          endif
       enddo
       if (allocated(qtfrct_global)) deallocate(qtfrct_global, stat=istat)
    endif 
    !
    if ((distr_qtq.or.distr_qtq_per) .and. NOTfirst) then
       !
       ! verify whether sum of discharge components is positive
       !
       do n1 = 1, nto
          !
          ! Total discharge boundary
          !
          if(typbnd(n1)=='T') then  
             if (comparereal(abs(qtfrct(n1)),1.e-10_fp)<0) then 
                !
                ! Very close to zero
                ! If not, then go back and use default discharge distribution based on depth at boundary
                ! here we assume that this condition holds for all total discharge boundaries
                !
                NOTfirst = .false.
                goto 10
             endif
          endif
       enddo
    endif
    !
    ! Update QH values if necessary
    ! Necessary if: the discharge is not in the selected range
    ! or the QH table has not yet been read: itbct(5,ito)<0
    !
    do ito = ntof + 1, ntof + ntoq
       if (     (itbct(5, ito)<0)               &
         & .or. (qtfrct(ito)<hydrbc(1, ito, 1)) &
         & .or. (qtfrct(ito)>hydrbc(3, ito, 1))  ) then
          call updbcq(lunbcq    ,lundia    ,itbct     ,ito       ,nto       , &
                    & kcd       ,hydrbc    ,qtfrct(ito)          ,gdp       )
       endif
       !
       ! Change QTFRCT(ITO) from discharge into waterlevel using table
       !
       qtfrct(ito) = hydrbc(2, ito, 1) + (hydrbc(4, ito, 1) - hydrbc(2, ito, 1))&
                   & *((qtfrct(ito) - hydrbc(1, ito, 1))                        &
                   & /(hydrbc(3, ito, 1) - hydrbc(1, ito, 1)))
       !
       ! Apply relaxation, default thetqh=0 (no relaxation)
       !
       if (relxqh) then
          qtfrct(ito) = qtfrct(ito)*(1.0 - thetqh) + qtfrt2(ito)*thetqh
       endif
       !
       ! Backup for relaxation
       !
       qtfrt2(ito) = qtfrct(ito)
    enddo
    !
    ! Enable relaxation in following time steps if thetqh>0
    !
    relxqh = thetqh>0.0
    !
    ! Update time series values if necessary
    !
    if (nto > ntoftoq) then
       call updbct(lundia, ' ', ntoftoq, nto, kcd, kmax, hydrbc, tprofu, error, gdp)
       if (error) call d3stop(1, gdp)
    endif
    !
    ! All zvg are averaged and all boundaries have the same value. 
    ! Only for water level boundary and if prescrDEPTH=true. 
    ! Useful for multiple level boundarires on a complex zig zag border.
    !
    if (zavg_global) then 
       zbavg_gl =0._fp
       zavg_gl  =0._fp
       cont = 0 
       do n = 1, nrob
          if (nob(3,n) == 2) then !
             n1     = nob(8, n)
             zbavg_gl = zbavg_gl + zbavg(n1)
             zavg_gl  = zavg_gl  + zavg(n1)
             cont = cont+1
          endif
       enddo ! zbavg_gl = zbavg_gl/cont
       if (cont==0) then
          write(*,*) 'zavg_global=true but no water level boundaries'
          !pause
          stop
       endif
       do n = 1, nrob !this should be n1 = 1, nto      
          if (nob(3,n) == 2) then ! this should be if (typbnd(n1)=='Z') 
             n1     = nob(8, n) !this should be removed
             zbavg(n1) = zbavg_gl/cont !this shound be zbavg_gl and above scomemnt  zbavg_gl = zbavg_gl/cont
             zavg(n1)  = zavg_gl/cont !this shound be zbavg_gl and above scomemnt  zavg_gl = zbavg_gl/cont
          endif
       enddo
    endif
    !
    ! calculate fraction of opening for function values
    ! only for all NTO open boundaries
    !
    do n = 1, nrob
       n1     = nob(8, n)
       msta   = mnbnd(1, n1)
       nsta   = mnbnd(2, n1)
       mend   = mnbnd(3, n1)
       nend   = mnbnd(4, n1)
       posrel = mnbnd(7, n1)
       incx   = mend - msta
       incy   = nend - nsta
       maxinc = max(abs(incx), abs(incy))
       incx   = incx/max(1, maxinc)
       incy   = incy/max(1, maxinc)
       !
       ibtype = nob(3, n)
       mpbt   = nob(1, n)
       npbt   = nob(2, n)
       mp     = mpbt
       np     = npbt
       if (nob(4, n)==2) mpbt = mpbt - 1
       if (nob(6, n)==2) npbt = npbt - 1
       !
       ! Defined HU/HV as in TAUBOT as > 0.01
       !
       if (.not. zmodel) then
          hu0 = max(hu(npbt, mpbt), 0.01_fp)
          hv0 = max(hv(npbt, mpbt), 0.01_fp)
       else
          !
          hu0 = 0.0_fp
          do k = kfumin(npbt, mpbt), kfumax(npbt, mpbt)
             hu0 = hu0 + dzu1(npbt, mpbt,k)
          enddo
          !
          hv0 = 0.0_fp
          do k = kfvmin(npbt, mpbt), kfvmax(npbt, mpbt)
             hv0 = hv0 + dzv1(npbt, mpbt,k)
          enddo
       endif
       !
       ! Determine direction dependent parameters
       !
       if (nob(4,n) > 0) then
          udir  = .true.
          vdir  = .false.
          dpvel = max(0.0_fp, hu0)
          h0    = hu0
          z0    = z0urou(npbt, mpbt)
          !
          ! ensure that z1 /= 0 to avoid division by zero in case of dry point
          !
          zl    = hu0
       elseif (nob(6,n) > 0) then
          udir  = .false.
          vdir  = .true.
          dpvel = max(0.0_fp, hv0)
          h0    = hv0
          z0    = z0vrou(npbt, mpbt)
          !
          ! ensure that z1 /= 0 to avoid division by zero in case of dry point
          !
          zl    = hv0
       else
       endif
       !
       ! Determine lower and upper bound of layer k for the loop
       !
       if (zmodel) then
          if (udir) then
             k1st = kfumin(npbt, mpbt)
             k2nd = kfumax(npbt, mpbt)
          elseif (vdir) then
             k1st = kfvmin(npbt, mpbt)
             k2nd = kfvmax(npbt, mpbt)
          else
          endif
       else
          k1st = 1
          k2nd = kmax
       endif
       do k = 1,kmax
          thklay(k) = 0.0
       enddo
       do k = k1st, k2nd
          if (dpvel > 0.0_fp) then
             if (zmodel) then
                if (udir) then
                   thklay(k) = dzu1(npbt, mpbt, k)/dpvel
                elseif (vdir) then
                   thklay(k) = dzv1(npbt, mpbt, k)/dpvel
                else
                endif
             else
                thklay(k) = thick(k)
             endif
          else
             !
             ! In case of dry point:
             ! thklay /= 0 to avoid division by zero
             ! boundary value is not realistic but will not be used
             !
             thklay(k) = thick(k)
          endif
       enddo
       !
       ! Avoid division by zero in inactive velocity points
       !
       hu0 = max(hu0, 0.01_fp)
       hv0 = max(hv0, 0.01_fp)
       h0  = max(h0 , 0.01_fp)
       !
       ! calculate CIRC2/3D array
       ! where nob (4,n) := type of opening and
       ! nob (5,n) := row (conform irocol table)
       ! nob (6,n) := type of opening and
       ! nob (7,n) := column (conform irocol table)
       !
       kpr = nob(4, n)
       kqr = nob(5, n)
       kpc = nob(6, n)
       kqc = nob(7, n)
       kp  = kpr
       kq  = kqr
       kpp = kpc
       kqq = kqc
       !
       ! If row boundary not available then use col boundary
       !
       if (kp*kq == 0) then
          kp  = kpc
          kq  = kqc
          kpp = kpr
          kqq = kqr
       endif
       !
       ! If IBTYPE = 2 use coord. at zeta points to calculate the
       ! interpolated values from the two utmost points of the opening
       ! If QH boundary (n1 between ntof and ntof+ntoq) use constant
       ! waterlevel.
       ! CIRC3D will never be used in CUCBP(2)
       !
       if (ibtype == 2) then
          !
          ! Initialize CIRC2D for KP,KQ
          !
          if (parll) then
             !
             ! If the start point/pivot is outside this partition,
             ! add the distance from that point/pivot to the first point inside this partition
             ! to totl and distl
             !
             totl = dist_pivot_part(start_pivot,n1)
          else
             totl = 0.0_fp
          endif
          dist = totl
          frac = 0.0_fp
          do j = 1, maxinc
             msta  = msta + incx
             nsta  = nsta + incy
             !
             ! In case of a diagonal water level boundary (e.g. south-east):
             ! Pythagoras is used to calculate the distance from xz,yz(m,n) to xz,yz(m+1,n+1):
             !       d_y((m,n),(m+1,n+1)) = 0.5*(guuz(m,n) + guuz(m+1,n+1))
             !       d_x((m,n),(m+1,n+1)) = 0.5*(gvvz(m,n) + gvvz(m+1,n+1))
             !       dist = sqrt(d_x*d_x + d_y*d_y)
             !       Where guuz/gvvz is guu/gvv, extrapolated to the boundary zeta-point (outside the domain),
             !       using the first two guu/gvv inside the domain:
             !       guuz(m,n) = ( 3*guu(m+offm1,n+offn1) - guu(m+offm2,n+offn2) ) / 2
             !       Where the indices offset values offm/n1/2 can have the values 0, 1, 2, 3, depending on:
             !       - the orientation of the open boundary related to the domain
             !         north      boundary: nob(4)=0, nob(6)=2, incy=0
             !         north-east boundary: nob(4)=2, nob(6)=2, incx=-incy
             !         east       boundary: nob(4)=2, nob(6)=0, incx=0
             !         south-east boundary: nob(4)=2, nob(6)=1, incx= incy
             !         south      boundary: nob(4)=0, nob(6)=1, incy=0
             !         south-west boundary: nob(4)=1, nob(6)=1, incx=-incy
             !         west       boundary: nob(4)=1, nob(6)=0, incx=0
             !         north-west boundary: nob(4)=1, nob(6)=2, incx= incy
             !       - The value of incx/incy on diagonal boundaries (+1 or -1)
             ! Assumption: - the grid is more or less cartesian locally
             ! Note:       - incx and incy are -1, 0 or 1
             !             - gvvz is based on 2 gvv values with constant m-index and n-indices difference 1
             !             - guuz is based on 2 guu values with constant n-index and m-indices difference 1
             !             - vertical/horizontal boundaries (north, east, south, west):
             !               - flagged by incx=0 or incy=0
             !               - d_y=0 or d_x=0, so instead of Pythagoras: dist = dist + d_x + d_y
             !                 (guu/gvv are distances and always >0)
             !               - extrapolation of guu/gvv to guuz/gvvz is still needed for the non-zero d_x/d_y
             !
             !
             ! Compute distance in xi-direction
             !
             if (incx == 0) then
                !
                ! east or west boundary
                !
                distx = 0.0_fp
             else
                ngg = nsta
                select case(nob(4,n))
                case (0)
                   if (nob(6,n) == 1) then
                      ! south boundary, gvv(ngg,..) and gvv(ngg+1,..) are inside domain
                      gvvz1 = (3.0_fp*gvv(ngg,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                      gvvz2 = (3.0_fp*gvv(ngg,msta-incx) - gvv(ngg+1,msta-incx)) / 2.0_fp
                   elseif (nob(6,n) == 2) then
                      ! north boundary, gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                      gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                      gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg-2,msta-incx)) / 2.0_fp
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case (1)
                   if (nob(6,n) == 1) then
                      ! south-west boundary
                      if (incy > 0) then
                         ! incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg  ,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg+1,msta-incx) - gvv(ngg+2,msta-incx)) / 2.0_fp
                      endif
                   elseif (nob(6,n) == 2) then
                      ! north-west boundary
                      if (incy > 0) then
                         ! incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-2,msta-incx) - gvv(ngg-3,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg  ,msta-incx) - gvv(ngg-1,msta-incx)) / 2.0_fp
                      endif
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case (2)
                   if (nob(6,n) == 1) then
                      ! south-east boundary
                      if (incy > 0) then
                         ! incx>0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !         msta-incx: gvv(ngg-1,..) and gvv(ngg,..)   are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-1,msta-incx) - gvv(ngg  ,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx<0, msta     : gvv(ngg,..)   and gvv(ngg+1,..) are inside domain
                         !                 msta-incx: gvv(ngg+1,..) and gvv(ngg+2,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg  ,msta)      - gvv(ngg+1,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg+1,msta-incx) - gvv(ngg+2,msta-incx)) / 2.0_fp
                      endif
                   elseif (nob(6,n) == 2) then
                      ! north-east boundary
                      if (incy > 0) then
                         ! incx<0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !         msta-incx: gvv(ngg-2,..) and gvv(ngg-3,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg-2,msta-incx) - gvv(ngg-3,msta-incx)) / 2.0_fp
                      else
                         ! incy<0, incx>0, msta     : gvv(ngg-1,..) and gvv(ngg-2,..) are inside domain
                         !                 msta-incx: gvv(ngg,..)   and gvv(ngg-1,..) are inside domain
                         gvvz1 = (3.0_fp*gvv(ngg-1,msta)      - gvv(ngg-2,msta)     ) / 2.0_fp
                         gvvz2 = (3.0_fp*gvv(ngg  ,msta-incx) - gvv(ngg-1,msta-incx)) / 2.0_fp
                      endif
                   else
                      ! nob(6) is always 1 or 2 for open boundaries that are not east or west boundaries
                   endif
                case default
                   ! nob(4) is always 0, 1 or 2
                endselect
                distx = 0.5_fp * (gvvz1 + gvvz2)
             endif
             !
             ! Compute distance in eta-direction
             !
             if (incy == 0) then
                !
                ! north or south boundary
                !
                disty = 0.0_fp
             else
                mgg = msta
                select case(nob(6,n))
                case (0)
                   if (nob(4,n) == 1) then
                      ! west boundary, guu(..,mgg) and guu(..,mgg+1) are inside domain
                      guuz1 = (3.0_fp*guu(nsta     ,mgg) - guu(nsta     ,mgg+1)) / 2.0_fp
                      guuz2 = (3.0_fp*guu(nsta-incy,mgg) - guu(nsta-incy,mgg+1)) / 2.0_fp
                   elseif (nob(4,n) == 2) then
                      ! east boundary, guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                      guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                      guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg-2)) / 2.0_fp
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case (1)
                   if (nob(4,n) == 1) then
                      ! south-west boundary
                      if (incx > 0) then
                         ! incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg)  ) / 2.0_fp
                      else
                         ! incx<0, incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg+1) - guu(nsta-incy,mgg+2)) / 2.0_fp
                      endif
                   elseif (nob(4,n) == 2) then
                      ! south-east boundary
                      if (incx > 0) then
                         ! incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-2) - guu(nsta-incy,mgg-3)) / 2.0_fp
                      else
                         ! incx<0, incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg)   - guu(nsta-incy,mgg-1)) / 2.0_fp
                      endif
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case (2)
                   if (nob(4,n) == 1) then
                      ! north-west boundary
                      if (incx > 0) then
                         ! incy>0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !         nsta-incy: guu(..,mgg-1) and guu(..,mgg)   are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-1) - guu(nsta-incy,mgg)  ) / 2.0_fp
                      else
                         ! incx<0, incy<0, nsta     : guu(..,mgg)   and guu(..,mgg+1) are inside domain
                         !                 nsta-incy: guu(..,mgg+1) and guu(..,mgg+2) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg)   - guu(nsta     ,mgg+1)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg+1) - guu(nsta-incy,mgg+2)) / 2.0_fp
                      endif
                   elseif (nob(4,n) == 2) then
                      ! north-east boundary
                      if (incx > 0) then
                         ! incy<0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !         nsta-incy: guu(..,mgg-2) and guu(..,mgg-3) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg-2) - guu(nsta-incy,mgg-3)) / 2.0_fp
                      else
                         ! incx<0, incy>0, nsta     : guu(..,mgg-1) and guu(..,mgg-2) are inside domain
                         !                 nsta-incy: guu(..,mgg)   and guu(..,mgg-1) are inside domain
                         guuz1 = (3.0_fp*guu(nsta     ,mgg-1) - guu(nsta     ,mgg-2)) / 2.0_fp
                         guuz2 = (3.0_fp*guu(nsta-incy,mgg)   - guu(nsta-incy,mgg-1)) / 2.0_fp
                      endif
                   else
                      ! nob(4) is always 1 or 2 for open boundaries that are not north or south boundaries
                   endif
                case default
                   ! nob(6) is always 0, 1 or 2
                endselect
                disty = 0.5_fp * (guuz1 + guuz2)
             endif
             if (incx/=0 .and. incy/=0) then
                distx = distx * distx
                disty = disty * disty
                totl  = totl + sqrt(distx + disty)
             else
                ! distx==0 or disty==0
                totl  = totl + distx + disty
             endif
             if (msta==mp .and. nsta==np) then
                dist = totl
             endif
          enddo
          if (parll) then
             !
             ! If the end point/pivot is outside this partition,
             ! add the distance from that point/pivot to the last point inside this partition
             ! to totl
             !
             totl = totl + dist_pivot_part(end_pivot,n1)
          endif
          if (maxinc > 0) frac = dist/totl
          !
          ! Correction for atmosferic pressure only for Water-level open
          ! boundaries and space varying wind & pressure
          !
          if (pcorr) then
             pdiff = patm(np, mp) - paver
             circ2d(kp, kq) = -pdiff/(ag*rhow)
          else
             circ2d(kp, kq) = 0.0
          endif
          !
          ! Amplitude and phase values at individual boundary points
          ! for all KC components
          !
          if (n1 <= ntof) then
             do k = 1, kc
                amplik = hydrbc(1*2 - 1, n1, k)                                 &
                       & + frac*(hydrbc(1*2, n1, k) - hydrbc(1*2 - 1, n1, k))
                phasek = hydrbc(2*2 - 1, n1, k)                                 &
                       & + frac*(hydrbc(2*2, n1, k) - hydrbc(2*2 - 1, n1, k))
                angle  = degrad*(omega(k)*tcur - phasek)
                circ2d(kp, kq) = circ2d(kp, kq) + amplik*cos(angle)
             enddo
             !
             ! Adjust boundaries by OpenDA if necessary
             !
             call get_openda_buffer('bound_astroH', n1, 1, 1, circ2d(kp,kq))
             !
          elseif (n1 <= ntof+ntoq) then
             !
             ! boundary defined with QH relation (S1) boundary value
             ! stored in QTFRCT.
             !
             circ2d(kp, kq) = circ2d(kp, kq) + qtfrct(n1)
          else
             !
             ! Add hydrodynamic open boundary value
             ! Amplitude value at individual boundary points
             !
             circ2d(kp, kq) = circ2d(kp, kq) + hydrbc(1*2 - 1, n1, 1)           &
                            & + frac*(hydrbc(1*2, n1, 1)                        &
                            & - hydrbc(1*2 - 1, n1, 1))
          endif
          if (fbccorrection) then
             !
             ! Time-varying correction
             !
             if (fcrbnd(n1)%ibct(1) > 0) then
                call flw_gettabledata(fbcrfile , fcrbnd(n1)%ibct(1)     , &
                     & fcrbnd(n1)%ibct(2)      , fcrbnd(n1)%ibct(3)     , &
                     & fcrbnd(n1)%ibct(4)      , fbcr_array             , &
                     & timhr , julday          , gdp )
                select case (fcrbnd(n1)%ibct(3))
                case (1)
                   circ2d(kp, kq) = circ2d(kp, kq) + fbcr_array(1)
                case (2)
                   circ2d(kp, kq) = circ2d(kp, kq) + fbcr_array(1)                 &
                                  & + frac*(fbcr_array(2) - fbcr_array(1))
                end select
             endif
          endif
          !
          tdif = timnow*dt - tstart
          if (.not.periodSURFACE) then
             if (use_DPSavg_for_qtot) then
                bedELEV = - zbavg(n1)
             else
                bedELEV = - dps(np, mp) 
             endif
             !
             ! smoothing
             !
             if (itlfsm>0 .and. tdif<=itlfsm*dt) then
                if (use_DPSavg_for_qtot) then
                   dini = zavg(n1) !THIS IS FOR ZIG ZAG BOUNDARIES (2/1 SLOPE)
                else
                   dini  = s0(np, mp)
                endif
                if (prescrDEPTH) dini = dini - bedELEV  !from water level to water depth
                tfrac = tdif/(itlfsm*dt)
                diff  = circ2d(kp, kq) - dini
                circ2d(kp, kq) = dini + tfrac*diff             
             endif
             if (prescrDEPTH) circ2d(kp, kq) = circ2d(kp, kq) + bedELEV ! from depth to water level
          else !periodic BC for level 
           !release crashes if I call PERIODIC_incbc
            ! CALL PERIODIC_incbc(circ2d,itlfsm,tdif,dt,s0,dps,mp,np,xz,yz,polyx,polyy,lunscr,gsqs,kp,kq,alfas,noroco,gdp%d%nlb,gdp%d%mlb,gdp%d%nub,gdp%d%mub)
            !
            ! smoothing factor specific for periodic BC
            !
            itlfsm_per = itlfsm*perSMOfac 
            if (itlfsm_per>0 .and. tdif<=itlfsm_per*dt) then
               tfracPER = tdif/(itlfsm_per*dt) 
            else
               tfracPER  = 1._fp
            endif
      !
            ! prescribe periodic condition 
            foundPER =.false.
            do k=1,nrPER ! to be optimized 
               if (mPH_ext(k)==mp.and.nPH_ext(k)==nP) then
                  hini= s0(np, mp)+ dps(np, mp)
                  nQ = nPQ_int(k)
                  mQ = mPQ_int(k)
                  !            PERIODIC POROS AND DPS/DPL, NOT NEEDED SINCE ITS DONE AFTER BED AND BANK UPDATE NOW
!                  !determine baricenter of boundary internal cel at Q
!                  if (cutcell.gt.0) then !poros,xg and yg not defined for non cut cell simulation
!                     if (comparereal(poros(nQ,mQ),1._fp).eq.0) then !  cell is uncut
!                        xgg = xg(nQ,mQ)
!                        ygg = yg(nQ,mQ)
!                     elseif (comparereal(poros(nQ,mQ),0._fp).eq.0) then !  vegetated
!                        xgg = xz(nQ,mQ) !random value, the prescribed depth and bed level will not be used
!                        ygg = yz(nQ,mQ) !random value, the prescribed depth and bed level will not be used
!                     else !cut cell
!                        Ndry= Ndry_GRS(nQ,mQ)  
!                        Ndryp1 = Ndry+1
!                        polyx(1:Ndry) = INTx_GRS(1:Ndry,nQ,mQ) ; polyx(Ndryp1) = INTx_GRS(1,nQ,mQ)
!                        polyy(1:Ndry) = INTy_GRS(1:Ndry,nQ,mQ) ; polyy(Ndryp1) = INTy_GRS(1,nQ,mQ)
!                        CALL A_G_Poly(polyx,polyy,Ndryp1,areaDRY,xgCOMP,ygCOMP,2,lunscr) 
!                        xgg = (xg(nQ,mQ)*gsqs(nQ,mQ) - xgCOMP*areaDRY)/(gsqs(nQ,mQ)-areaDRY) !these gives rounding error for small cut cells
!                        ygg = (Yg(nQ,mQ)*gsqs(nQ,mQ) - ygCOMP*areaDRY)/(gsqs(nQ,mQ)-areaDRY)  !these gives rounding error for small cut cells
!                     endif
!                  else
!                     xgg = xz(nQ,mQ)
!                     ygg = yz(nQ,mQ)
!                  endif
!                  !determine baricenter of boundary internal cel at H
!                  nH = nPH_int(k)
!                  mH = mPH_int(k) 
!                  if (cutcell.gt.0) then !poros,xg and yg not defined for non cut cell simulation
!                     if (comparereal(poros(nH,mH),1._fp).eq.0) then ! no cut cell
!                        xggH = xg(nH,mH)
!                        yggH = yg(nH,mH)
!                     elseif (comparereal(poros(nH,mH),0._fp).eq.0) then !  vegetated
!                        xggH = xz(nH,mH) !random value, the prescribed depth and bed level will not be used
!                        yggH = yz(nH,mH) !random value, the prescribed depth and bed level will not be used
!                     else !cut cell
!                        Ndry= Ndry_GRS(nH,mH)  
!                        Ndryp1 = Ndry+1
!                        polyx(1:Ndry) = INTx_GRS(1:Ndry,nH,mH) ; polyx(Ndryp1) = INTx_GRS(1,nH,mH)
!                        polyy(1:Ndry) = INTy_GRS(1:Ndry,nH,mH) ; polyy(Ndryp1) = INTy_GRS(1,nH,mH)
!                        CALL A_G_Poly(polyx,polyy,Ndryp1,areaDRY,xgCOMP,ygCOMP,2,lunscr) 
!                        xggH = (xg(nH,mH)*gsqs(nH,mH) - xgCOMP*areaDRY)/(gsqs(nH,mH)-areaDRY) !these gives rounding error for small cut cells
!                        yggH = (Yg(nH,mH)*gsqs(nH,mH) - ygCOMP*areaDRY)/(gsqs(nH,mH)-areaDRY)  !these gives rounding error for small cut cells
!                     endif
!                  else
!                     xggH = xz(nH,mH)
!                     yggH = yz(nH,mH)
!                  endif
!               !determine dz, variation of depth (-bed elevation) at boundary
!                  if (perCIRC) then !circular periodic channel with center in (zero,zero)
!                     rr = sqrt(xgg**2 + ygg**2)                   
!                     angFIRSTbaric = mod(1._fp/2._fp*pi-atan2(ygg,xgg),2._fp*pi) !i put 1/2*pi to have the zero on the North, but then I use differences of angles so it should be general
!                     angLASTbaric =  mod(1._fp/2._fp*pi-atan2(yggH,xggH),2._fp*pi) ! da controllare!!!
!                     distBOUND =  (abs(angLASTbaric)+abs(angFIRSTbaric)) !angular distance
!                     distINTERNAL = 2._fp*pi - distBOUND
!                     slope = (dps(nH,mH) - dps(nQ,mQ))/distINTERNAL !angular slope.ITS POSITIVE SINCE bottom elvation is smaller at H, so depth below reference is larger at H
!                     !zz = slope*(2._fp*pi+angFIRSTbaric)
!                     dz = distBOUND*slope
!                  else !straight periodic channel
!                  ! Note: this approach is not perfect, in the sense that the like distINTERNAL is not parallel to the banks but almost, 
!                  !       so slope is slightly off the real slope. I could not find a way to do it exact. It can be done exact for non
!                  !       cut cells just computing  slopeM = (dps(int)-dps(intint))/dm and extrapolate in m, but if the cell is cut thats not 
!                  !       exact anymore. I think the only way to do it exact is taking 3 internal cells and do the plane between the 3 baricenter,
!                  !       too complicated and maybe not worth it. 
!                     distGx = xggH-xgg
!                     distGy = yggH-ygg
!                     distINTERNAL = sqrt((xggH-xgg)**2 + (yggH-ygg)**2)
!                     alfa = alfas(nH, mH)*degrad ! any (nH,mH) is ok since the grid is cartesian non curvilinear!!(perCIRC=false)
!                     distGm = distGx*cos(alfa) - distGy*sin(alfa)
!                     !distGn = distGx*sin(alfa) + distGy*cos(alfa)
!                     !slope = (dps(nH,mH) - dps(nQ,mQ))/distINTERNAL !linear slope in x or y ITS POSITIVE SINCE bottom elvation is smaller at H, so depth below reference is larger at H
!                     slope  = (dps(nH,mH) - dps(nQ,mQ))/distINTERNAL
!                     slopeM = slope*cos(alfa) ! only because its supposed to have dz/dn =0 normally to the wall,so dz/dx is simply dz/dn*nx
!      !                      if (comparereal(distGx,0_fp).ne.0) then
!      !                         slopex = cos(atan(distGy/distGx))*slope
!      !                         distBOUND = distanceBOUNDper - distGx     !distance along x between baricenter at the boundary as it was an infinite long channel
!      !                      else !else the channel is perfectly vertical i use along y slope
!      !                         slopey = slope
!      !                         distBOUND = distanceBOUNDper - distGy     !distance along x between baricenter at the boundary as it was an infinite long channel
!      !                      endif 
!                     distBOUND = distanceBOUNDper - distGm
!                     dz = distBOUND*slopeM                          
!                  endif
!                  !
!                  ! prescribe periodic bed elevation at Q halo 
!                  !
!                  IF (bedPERIODIC) dps(nPQ_ext(k),mPQ_ext(k)) = dps(nQ,mQ) - dz 
!                  if (cutcell.gt.0) then 
!                     IF (bedPERIODIC) THEN
!                        dpL(nPQ_ext(k),mPQ_ext(k)) = dpL(nQ,mQ) - dz  
!                        if (comparereal(poros(nPQ_ext(k),mPQ_ext(k)),1._fp)==0) then
!                           dpH(nPQ_ext(k),mPQ_ext(k)) = dpL(nPQ_ext(k),mPQ_ext(k)) ! I set it equal to dpL if fully unvegetated cell
!                        else
!                           dpH(nPQ_ext(k),mPQ_ext(k)) = dpH(nQ,mQ) - dz     ! for floodplains
!                        endif             
!                     ENDIF           
!                     poros(nPQ_ext(k),mPQ_ext(k)) = poros(nH,mH)   
!                  endif  
!                  !
!                  !  prescribe periodic bed elevation at H halo 
!                  !
!                  IF (bedPERIODIC) dps(nPH_ext(k),mPH_ext(k)) = dps(nH,mH) + dz 
!                  if (cutcell.gt.0)  then
!                     IF (bedPERIODIC) THEN
!                        dpL(nPH_ext(k),mPH_ext(k)) = dpL(nH,mH) + dz  
!                        if (comparereal(poros(nPH_ext(k),mPH_ext(k)),1._fp)==0) then
!                           dpH(nPH_ext(k),mPH_ext(k)) = dpL(nPH_ext(k),mPH_ext(k)) ! I set it equal to dpL if fully unvegetated cell
!                        else
!                           dpH(nPH_ext(k),mPH_ext(k)) = dpH(nH,mH) + dz ! for floodplains
!                        endif
!                     ENDIF
!                     poros(nPH_ext(k),mPH_ext(k)) = poros(nQ,mQ) 
!                  endif
      !
      !                  set periodic water level 
      !             
                  neumDWNSTRM = .false.
                  if (PERIODICwaterDEPTH) then 
                     hnew =  s0(nQ,mQ) + dps(nQ,mQ)
                     IF (perSMOfac<-998.and.tdif<=itlfsm*dt/2._fp ) then !first third I prescribe depth Neumann
                         neumDWNSTRM = .true.
                         hnew = s0(nPH_int(k),mPH_int(k)) +dps(nPH_int(k),mPH_int(k)) !EXPLICIT NEWMANN CONDITION AT H BOUNDARY!! I used *0.5_fp, i.e. if perSMOfac=-999 I use a Newmann condition on the water level until half of the hydrod. smoothing time. After that time, the periodic condition is used
                         !Note: I prescribed neumann for depth  it creats]es steep withdrawn profile (I dont know why). But if I use neumann for s0 its even worst (nothing happens at the beginning)
                        ! write(909090,'(2i9,25f25.15)') nst,k,dps(nPH_ext(k),mPH_ext(k)),dps(nPH_int(k),mPH_int(k)) !
                     ENDIF
                     diff = hnew - hini
                     hnewSMO = hini + tfracPER*diff
                     circ2d(kp, kq) = hnewSMO - dps(nPH_ext(k),mPH_ext(k))
                  else
                     circ2d(kp, kq) = s0(nQ,mQ)
                  endif
                  if (reltim_s1>0) then
                     Hnew = circ2d(kp, kq)
                     if (.NOT.firstCALL) then
                        a = exp( - (0.5_fp*dt)/reltim_s1) !they are both in minutes
                        qfilt_s1(k) = a*qfilt_s1(k) + (1._fp - a)*Hnew
                     else
                        qfilt_s1(k) = Hnew
                     endif
                     if (.NOT.neumDWNSTRM) circ2d(kp, kq) = qfilt_s1(k) !I still want to update qfilt_s1 when neumDWNSTRM=T otherwise it stays undefined. 
                  endif  
      !                     
                  foundPER = .true.              
                  exit !periodic cell has been found, exit
               endif
            enddo
            if (.not.foundPER) then
               write(*,*) 'All water level locations have to be periodic!' 
               !pause 
               stop
            endif
!
          endif
       !
       ! end water level boundary
       !
       else
          !
          ! If IBTYPE = 3, 5 or 6 use the distance arrays GUU and GVV to
          ! calculate the interpolated values from the two utmost points
          ! of the opening
          ! Start filling CIRC3D and calculate CIRC2D
          !
          if (parll) then
             !
             ! If the start point/pivot is outside this partition,
             ! add the distance from that point/pivot to the first point inside this partition
             ! to totl and distl
             !
             totl = dist_pivot_part(start_pivot,n1)
          else
             totl = 0.0_fp
          endif
          dist = totl
          frac  = 0.0_fp
          mgg   = msta
          ngg   = nsta
          horiz = .true.
          if (nob(4,n)+nob(6,n) == 2) ngg = ngg - 1
          if (msta == mend) then
             if (nsta == nend) then
                !
                ! Opening consists of one point
                !
                maxinc = 0
             else
                !
                ! Opening in the vertical direction
                !
                horiz = .false.
                if (nob(4,n)+nob(6,n) == 2) mgg = mgg - 1
             endif
          endif
          !
          ! In case of a total discharge boundary use frac = 0.0
          ! otherwise calculate the distance between points ...
          !
          if (ibtype /= 7) then
             !
             ! Distance between points calculated
             ! When MSTA/NSTA are updated first use lower GVV/GUU
             !
             do j = 1, maxinc
                msta = msta + incx
                nsta = nsta + incy
                if (horiz) then
                   totl = totl + 0.5*(gvv(ngg, msta) + gvv(ngg, msta - incx))
                else
                   totl = totl + 0.5*(guu(nsta, mgg) + guu(nsta - incy, mgg))
                endif
                if (msta==mp .and. nsta==np) dist = totl
             enddo
             if (parll) then
                !
                ! If the end point/pivot is outside this partition,
                ! add the distance from that point/pivot to the last point inside this partition
                ! to totl
                !
                totl = totl + dist_pivot_part(end_pivot,n1)
             endif
             if (maxinc > 0) frac = dist/totl
          endif
          !
          ! Mass Flux component
          !
          if (ibtype==3 .or. ibtype==6) then
             if (udir) then
                grmass = grmasu(npbt, mpbt)/hu0
             elseif (vdir) then
                grmass = grmasv(npbt, mpbt)/hv0
             else
             endif
          elseif (ibtype==5 .or. ibtype==7) then
             if (udir) then
                grmass = grmasu(npbt, mpbt)*guu(npbt, mpbt)
             elseif (vdir) then
                grmass = grmasv(npbt, mpbt)*gvv(npbt, mpbt)
             else
             endif
          else
             grmass=0.0
          endif
          !
          ! Logarithmic or uniform velocity profile at velocity,
          ! discharge boundary or Riemann boundary (for those open
          ! boundary types the oblique boundary is not allowed).
          !
          if (  tprofu(n1)(1:7)  == 'uniform' .or. &
              & tprofu(n1)(1:11) == 'logarithmic'   ) then
             !
             ! atmospheric pressure correction for Riemann boundaries
             !
             if (ibtype==6 .and. pcorr) then   
                pdiff = patm(np, mp) - paver
                if (posrel <= 2) then
                   circ2d(kp, kq) = (-pdiff/(ag*rhow))*sqrt(ag/h0)
                else
                   circ2d(kp, kq) = (pdiff/(ag*rhow))*sqrt(ag/h0)
                endif
             else
                circ2d(kp, kq) = 0.0
             endif
             !
             if (n1 <= ntof) then
                !
                ! Amplitude and phase values at individual boundary
                ! points for all KC components,
                ! for all profile types the boundary values are
                ! defined as depth averaged
                !
                do k = 1, kc
                   amplik = hydrbc(1*2 - 1, n1, k)                              &
                          & + frac*(hydrbc(1*2, n1, k) - hydrbc(1*2 - 1, n1, k))
                   phasek = hydrbc(2*2 - 1, n1, k)                              &
                          & + frac*(hydrbc(2*2, n1, k) - hydrbc(2*2 - 1, n1, k))
                   angle  = degrad*(omega(k)*tcur - phasek)
                   circ2d(kp, kq) = circ2d(kp, kq) + amplik*cos(angle)
                enddo
             else
                !
                ! Time dependent open boundary value
                ! Amplitude value at individual boundary points
                !
                circ2d(kp, kq) = circ2d(kp, kq) + hydrbc(1*2 - 1, n1, 1)        &
                               & + frac*(hydrbc(1*2, n1, 1)                     &
                               &       - hydrbc(1*2 - 1, n1, 1))
                if (distr_qtq .or. distr_qtq_per) then
                   !
                   ! Smoothing has to be done here before applying the percentage,
                   ! otherwise if done below to the single i^th discharges it changes the percentages
                   ! and the sum is different from the Q prescribed. 
                   ! It gave oscillations in the periodic channel, especially when discharge changes sign downstream
                   !
                   tdif = timnow*dt - tstart
                   if (itlfsm>0 .and. tdif<=itlfsm*dt) then
                      tfrac = tdif/(itlfsm*dt)
                         diff = circ2d(kp, kq)-Q_bnd
                         circ2d(kp, kq) = Q_bnd + tfrac*diff
                      endif
                   endif
             endif
             !
             ! For total discharge boundary use fraction
             !
             if (ibtype == 7) then
                !
                ! Prevent division by zero
                !
                if (comparereal(qtfrct(n1),0._fp).eq.0) qtfrct(n1) = 1.0
                circ2d(kp, kq) = circ2d(kp, kq)*qtfrac(n)/qtfrct(n1)
             endif
             if (fbccorrection) then
                !
                ! Time-varying correction
                !
                if (fcrbnd(n1)%ibct(1) > 0) then
                   call flw_gettabledata(fbcrfile , fcrbnd(n1)%ibct(1)     , &
                           & fcrbnd(n1)%ibct(2)   , fcrbnd(n1)%ibct(3)     , &
                           & fcrbnd(n1)%ibct(4)   , fbcr_array             , &
                           & timhr , julday       , gdp )
                   if (ibtype == 7) then
                      circ2d(kp, kq) = circ2d(kp, kq) + fbcr_array(1)*qtfrac(n)/qtfrct(n1)
                   else
                      select case (fcrbnd(n1)%ibct(3))
                      case (1)
                         circ2d(kp, kq) = circ2d(kp, kq) + fbcr_array(1)
                      case (2)
                         circ2d(kp, kq) = circ2d(kp, kq) + fbcr_array(1)              &
                                        & + frac*(fbcr_array(2) - fbcr_array(1))
                      end select
                   endif
                endif
             endif
             !
             ! Add mass flux correction
             !
             circ2d(kp, kq) = circ2d(kp, kq) + grmass
             !
             if ((distr_qtq.or.distr_qtq_per) .and. NOTfirst) then
                  ! compute the vertical profile from the internal profile.
                do k = 1, kmax
                   circ3d(k, kp, kq) = circ2d(kp, kq)*qtfracV(k,n)
                enddo
                !write(987654,'( 2i7,20f25.15)') nst,n,(qtfracV(k,n),k=1,kmax),sum(qtfracV(:,n))
             else
                if (tprofu(n1)(1:7) == 'uniform') then
                !
                ! Define 3D boundary values for profile "uniform"
                ! For Discharge take layer thickness into account
                !
                   if (ibtype==5 .or. ibtype==7) then
                      if (mnbnd(5,n1) == 0 ) then
                         !
                         ! Normal total discharge boundary
                         !
                         do k = 1, kmax
                            circ3d(k, kp, kq) = circ2d(kp, kq)*thklay(k)
                         enddo
                      else
                         !
                         ! Discharge in a restricted number of layers
                         !
                         thickOpen = 0.0_fp
                         do k=mnbnd(5,n1), mnbnd(6,n1)
                            thickOpen = thickOpen + thklay(k)
                         enddo
                         do k = 1, kmax
                            if (k < max(k1st,mnbnd(5,n1))) then
                               circ3d(k, kp, kq) = 0.0_fp
                            elseif (k > min(k2nd,mnbnd(6,n1))) then
                               circ3d(k, kp, kq) = 0.0_fp
                            else
                               circ3d(k, kp, kq) = circ2d(kp, kq) * thklay(k)/thickOpen
                            endif
                         enddo
                      endif
                   else
                      do k = 1, kmax
                         circ3d(k, kp, kq) = circ2d(kp, kq)
                      enddo
                   endif
                   !
                   ! end uniform profile
                   !
                elseif (tprofu(n1)(1:11) == 'logarithmic') then
                   !
                   ! Define 3D boundary values for profile "logarithmic"
                   !
                   zbulk          = 0.0
                   sig2           = 0.0
                   !
                   ! Split approach for ZMODEL and SIGMA model
                   ! First ZMODEL
                   !
                   if (zmodel) then
                      do k = k2nd, k1st, -1
                         if (k == k2nd) then
                            if (udir) then
                               dz0 = dzu1(npbt, mpbt, k)
                            elseif (vdir) then
                               dz0 = dzv1(npbt, mpbt, k)
                            else
                            endif
                            zl = zl - .5*dz0
                         else
                            if (udir) then
                               dz0 = dzu1(npbt, mpbt, k)
                               dz1 = dzu1(npbt, mpbt, k + 1)
                            elseif (vdir) then
                               dz0 = dzv1(npbt, mpbt, k)
                               dz1 = dzv1(npbt, mpbt, k + 1)
                            else
                            endif
                            zl = zl - .5*dz0 - .5*dz1
                         endif
                         zlayer = log(1. + zl/z0)
                         zbulk = zbulk + zlayer*thklay(k)
                         zbulk = max(zbulk, 0.01_fp)
                         circ3d(k, kp, kq) = circ2d(kp, kq)*zlayer
                      enddo
                      !
                      ! For Discharge take layer thickness into account
                      !
                      if (ibtype==5 .or. ibtype==7) then
                         do k = 1, kmax
                            circ3d(k, kp, kq) = circ3d(k, kp, kq)*thklay(k)/zbulk
                         enddo
                      else
                         do k = 1, kmax
                            circ3d(k, kp, kq) = circ3d(k, kp, kq)/zbulk
                         enddo
                      endif
                   else
                      !
                      ! SIGMA model
                      ! to avoid break off error in Z2 use Z2 = MAX (0,Z2_ORG)
                      !
                      do k = 1, kmax
                         sig1              = sig2
                         sig2              = sig1 - thick(k)
                         z1                = (1. + sig1)*h0
                         z2                = max(0.0_fp, (1. + sig2)*h0)
                         zl                = (z1 + z2)/2.
                         zlayer            = log(1. + zl/z0)
                         zbulk             = zbulk + zlayer*thklay(k)
                         circ3d(k, kp, kq) = circ2d(kp, kq)*zlayer
                      enddo
                      !
                      ! For Discharge take layer thickness into account
                      !
                      if (ibtype==5 .or. ibtype==7) then
                         do k = 1, kmax
                            circ3d(k, kp, kq) = circ3d(k, kp, kq)*thklay(k)/zbulk
                         enddo
                      else
                         do k = 1, kmax
                            circ3d(k, kp, kq) = circ3d(k, kp, kq)/zbulk
                         enddo
                      endif
                   endif
                   !
                   ! end logarithmic profile
                   !
                endif
             endif
          elseif (tprofu(n1)(1:10) == '3d-profile') then
             !
             ! Add hydrodynamic open boundary value
             ! Amplitude value at individual boundary points all layers
             ! Only allowed as time serie (tested in RDBNDD)
             !
             ! For ZMODEL we may assume that all values accross KMAX
             ! layer has been specified (e.g. through NESTING program)
             ! So for ZMODEL we do not test whether 1 = kfu/vmin &
             ! kmax = kfu/vmax
             !
             if (ibtype==7) then
                !
                ! For total discharge boundary use fraction
                !
                !
                ! Prevent division by zero
                !
                if (qtfrct(n1) == 0.0) qtfrct(n1) = 1.0
                qtfrc = qtfrac(n)/qtfrct(n1)
                !
                do k = 1, kmax
                   circ3d(k, kp, kq) = (hydrbc(1*2 - 1, n1, k) + frac*(hydrbc(1*&
                                     & 2, n1, k) - hydrbc(1*2 - 1, n1, k)))     &
                                     & *qtfrc
                enddo
             else
                !
                ! atmospheric pressure correction for Riemann boundaries
                !
                if (ibtype==6 .and. pcorr) then   
                   pdiff = patm(np, mp) - paver
                   if (posrel <= 2) then
                      pcr = (-pdiff/(ag*rhow))*sqrt(ag/h0)
                   else
                      pcr = (pdiff/(ag*rhow))*sqrt(ag/h0)
                   endif
                   do k = 1, kmax
                      circ3d(k, kp, kq) = pcr
                   enddo
                else
                   do k = 1, kmax
                      circ3d(k, kp, kq) = 0.0
                   enddo
                endif
                !
                do k = 1, kmax
                   circ3d(k, kp, kq) = circ3d(k, kp, kq)                        &
                                     & + hydrbc(1*2 - 1, n1, k)                 &
                                     & + frac*(hydrbc(1*2, n1, k)               &
                                     & - hydrbc(1*2 - 1, n1, k))
                enddo
             endif
             !
             ! For Discharge take layer thickness into account
             !
             if (ibtype==5 .or. ibtype==7) then
                do k = 1, kmax
                   circ3d(k, kp, kq) = circ3d(k, kp, kq) + grmass*thklay(k)
                enddo
             else
                do k = 1, kmax
                   circ3d(k, kp, kq) = circ3d(k, kp, kq) + grmass
                enddo
             endif
          else
          endif
          !
          ! end 3D profile
          !
          !
          ! smoothing depending on direction for IBTYPE=3, 5 or 6
          !
          if (.not.(distr_qtq .or. distr_qtq_per)) then !in this case smoothing is done above
             tdif = timnow*dt - tstart
             if (itlfsm>0 .and. tdif<=itlfsm*dt) then
                tfrac = tdif/(itlfsm*dt)
                do k = 1, kmax
                   if (ibtype==3 .or. ibtype==6) then
                      if (udir) then
                         dini = u0(npbt, mpbt, k)
                      elseif (vdir) then
                         dini = v0(npbt, mpbt, k)
                      else
                      endif
                   elseif (ibtype==5 .or. ibtype==7) then
                      if (udir) then
                         dini = qxk(npbt, mpbt, k)
                      elseif (vdir) then
                         dini = qyk(npbt, mpbt, k)
                      else
                      endif
                   else
                      dini=0.0
                   endif
                   diff = circ3d(k, kp, kq) - dini
                   circ3d(k, kp, kq) = dini + tfrac*diff
                enddo
             endif
          endif
          !
          ! Define depth averaged boundary condition in CIRC2D for
          ! calibration of S0
          ! For Discharge sommation over Q without layer thickness
          !
          circ2d(kp, kq) = 0.
          if (ibtype==5 .or. ibtype==7) then
             do k = 1, kmax
                circ2d(kp, kq) = circ2d(kp, kq) + circ3d(k, kp, kq)
             enddo
          else
             do k = k1st, k2nd
                circ2d(kp, kq) = circ2d(kp, kq) + circ3d(k, kp, kq)*thklay(k)
             enddo
          endif
       endif
       !
       ! Include weakly reflective coeff. ALFA in CIRC2D
       !
       circ2d(kp + 2, kq) = alpha(n1)
       !
       ! If oblique boundary is defined then fill the
       ! computed CIRC2D also in other direction
       ! NOTE: Only waterlevel boundary conditions can be oblique
       !
       if (kpp*kqq /= 0) then
          circ2d(kpp, kqq) = circ2d(kp, kq)
          circ2d(kpp + 2, kqq) = circ2d(kp + 2, kq)
       endif
    enddo
    firstCALL = .FALSE.
end subroutine incbc
