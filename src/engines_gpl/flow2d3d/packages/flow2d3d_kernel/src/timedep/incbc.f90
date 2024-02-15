subroutine incbc(lundia    ,timnow    ,zmodel    ,nmax      ,mmax      , &
               & kmax      ,kcd       ,nto       ,ntof      ,ntoq      , &
               & kc        ,nrob      ,noroco    , &
               & tprofu    ,itbct     ,mnbnd     ,nob       ,kfumin    , &
               & kfumax    ,kfvmin    ,kfvmax    ,hydrbc    ,circ2d    , &
               & circ3d    ,patm      ,guu       ,gvv       , &
               & hu        ,hv        ,omega     ,alpha     , &
               & z0urou    ,z0vrou    ,qxk       ,qyk       ,s0        , &
               & u0        ,v0        ,grmasu    ,grmasv    ,cfurou    , &
               & cfvrou    ,qtfrac    ,qtfrct    ,qtfrt2    ,thick     , &
               & dzu1      ,dzv1      ,thklay    ,kcu       ,kcv       , &
               & kfu       ,kfv       ,kcs       ,timhr     ,nambnd    , &
               & typbnd    ,gdp       )
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
    real(fp)                           , pointer :: thetqt
    logical                            , pointer :: use_zavg_for_qtot
    logical                            , pointer :: pcorr
    real(fp), dimension(:,:,:)         , pointer :: rttfu
    real(fp), dimension(:,:,:)         , pointer :: rttfv
    logical                            , pointer :: relxqh
    type (handletype)                  , pointer :: fbcrfile
    type (fbcrbndtype)  , dimension(:) , pointer :: fcrbnd
    logical                            , pointer :: fbccorrection
    real(fp), dimension(:,:)           , pointer :: dist_pivot_part
    real(fp), dimension(:)             , pointer :: cwidth
    real(fp), dimension(:)             , pointer :: zavg    
!
! Global variables
!
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
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
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
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kc)                                            , intent(in)  :: omega  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                          , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                        :: thklay
    real(fp), dimension(kmax, 2, noroco)                                             :: circ3d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nrob)                                                        :: qtfrac !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                           , intent(in)  :: alpha  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                                         :: qtfrct !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nto)                                                         :: qtfrt2 !  Description and declaration in esm_alloc_real.f90
    character(20), dimension(nto)                                                    :: tprofu !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                                      , intent(in)  :: nambnd !  Description and declaration in esm_alloc_char.f90
    character(1) , dimension(nto)                                      , intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
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
    integer                             :: mpbi           ! M index of point at boundary inside domain
    integer                             :: mpbt           ! M index of boundary point
    integer                             :: msta           ! Starting coord. (in the x-dir.) of an open bound. section 
    integer                             :: n              ! Loop variable 
    integer                             :: n1             ! Pointer var. relating NOB to MNBND 
    integer                             :: nend           ! End coord. (in the y-dir.) of an open bound. section 
    integer, external                   :: newlun
    integer                             :: ngg            ! N-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering 
    integer                             :: np
    integer                             :: npbi           ! N index of point at boundary inside domain
    integer                             :: npbt           ! N index of boundary point
    integer                             :: nsta           ! Starting coord. (in the y-dir.) of an open bound. section 
    integer                             :: ntoftoq        ! Offset for open boundary sections of the Time-series type (NTOF+NTOQ) 
    integer                             :: posrel         ! code denoting the position of the open boundary, related to the complete grid
    integer                             :: lb             ! lowerboundary of loopcounter
    integer                             :: ub             ! upperboundary of loopcounter
    logical                             :: first          ! Flag = TRUE in case a time-dependent file is read for the 1-st time 
    logical                             :: error          ! errorstatus
    logical                             :: horiz          ! Flag=TRUE if open boundary lies parallel to x-/KSI-dir. 
    logical                             :: udir
    logical                             :: vdir
    real(fp)                            :: amplik
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
    real(fp)                            :: q0avg
    real(fp)                            :: qtfrc
    real(fp)                            :: sig1           ! Layer thickness as fraction of previous layer 
    real(fp)                            :: sig2           ! Layer thickness as fraction 
    real(fp)                            :: tcur           ! Current time in hours since last nodal update time
    real(fp)                            :: tdif           ! Time difference (in minutes) between TIMNOW and TSTART 
    real(fp)                            :: tfrac          ! Fraction of TDIF and Smoothing time 
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
    real(fp), dimension(:), allocatable :: qtfrct_global  ! work array
    integer                             :: nobcgl         ! global number of open boudnaries (i.e. original number excluding duplicate open boudnaries located in the halo regions)
    integer                             :: nobcto         ! total number of open boundaries (including "duplicate" open boudnaries located in halo regions)
    integer                             :: istat
!
    real(fp), dimension(nrob)           :: qtfrc2         ! Temporary array for old values of qtfrac for relaxation
!
!! executable statements -------------------------------------------------------
!
    relxqh                => gdp%gdincbc%relxqh
    paver                 => gdp%gdnumeco%paver
    thetqh                => gdp%gdnumeco%thetqh
    thetqt                => gdp%gdnumeco%thetqt
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
    !
    ! initialize local parameters
    ! omega in deg/hour & time in seconds !!, alfa = in minuten
    ! TIMSCL will not been used in UPDBCC
    !
    qtfrc2 = qtfrac
    !
    first   = .false.
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
       if (gdp%gdbcdat%gntoftoq > 0) then
          call dffind_duplicate(lundia, nto, nobcto, nobcgl,  gdp%gdbcdat%bct_order, gdp)
       else
          nobcto = nto
          nobcgl = nto
       endif
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
    cwidth = 1.0e-9_fp
    !
    do n = 1, nrob
       !
       ! only for total discharge boundaries (7):
       ! Determine average water level
       !
       if (nob(3,n) /= 7) then
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
          if (kfu(npbt,mpbt) == 1 .and. kcs(npbi,mpbi) == 1) then
             width = guu(npbt,mpbt)
          else
             width = 0.0_fp
          endif
       elseif (nob(6,n) > 0) then
          udir = .false.
          vdir = .true.
          wlvl = s0(npbi, mpbi)
          if (kfv(npbt,mpbt) == 1 .and. kcs(npbi,mpbi) == 1) then
             width = gvv(npbt,mpbt)
          else
             width = 0.0_fp
          endif
       else
       endif
       !
       qtfrct(n1) = qtfrct(n1) + wlvl*width
       cwidth(n1) = cwidth(n1) + width
    enddo
    !
    ! accumulate information across MPI partitions
    !
    if (parll .and. gdp%gdbcdat%gntoftoq>0) then
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
       zavg(n1) = qtfrct(n1)/cwidth(n1)
       qtfrct(n1) = 0.0_fp
    enddo
    !
    do n = 1, nrob
       ! only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       if (nob(3, n)/=7 .and. nob(3, n)/=2) then
          cycle
       endif
       qtfrac(n) = 0.0
       n1        = nob(8, n)
       mpbt      = nob(1, n)
       npbt      = nob(2, n)
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
       ttfhsum = 0.0
       kcsi  = kcs(npbi, mpbi)
       if (nob(4,n) > 0) then
          udir  = .true.
          vdir  = .false.
          if (use_zavg_for_qtot) then
             dpvel = max(0.0_fp, hu(npbt, mpbt)-s0(npbt, mpbt)+zavg(n1))
          else
             dpvel = max(0.0_fp, hu(npbt, mpbt))
          endif
          width = guu(npbt, mpbt)
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
       elseif (nob(6,n) > 0) then
          udir  = .false.
          vdir  = .true.
          if (use_zavg_for_qtot) then
             dpvel = max(0.0_fp, hv(npbt, mpbt)-s0(npbt, mpbt)+zavg(n1))
          else
             dpvel = max(0.0_fp, hv(npbt, mpbt))
          endif
          width = gvv(npbt, mpbt)
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
       else
       endif
       
       if (nob(3,n) == 7) then
          !
          ! part of total discharge boundary, compute B*h^(1.5)*C
          !
          ! Determine effective roughness
          ! Note: czbed contains Chezy/sqrt(ag) !!
          !
          if (kfuv == 0) then
             qtfrac(n)  = 0.0_fp
          else
             czeff      = czbed / sqrt(1.0_fp + 0.5_fp*ttfhsum*czbed*czbed)
             !
             !  This leads to oscillations parallel to the open boundary:
             ! 
             qtfrac(n)  = (dpvel**1.5_fp) * width * czeff
             qtfrac(n)  = qtfrac(n)*(1.0 - thetqt) + qtfrc2(n)*thetqt
             !
             !  Alternative (more robust?) implementation is switched off
             !
             !  qtfrac(n)  = dpvel*width
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
             if (kcsi==1) then ! only sum up discharge for boundary cells strictly inside this partition
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
    ! Update the discharge for total discharge or QH boundaries for the overall domain by summing up among those
    !
    if (parll .and. gdp%gdbcdat%gntoftoq>0) then
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
          if(typbnd(n1)=='T' .or. ((n1>ntof) .and. (n1<=ntof + ntoq))) then  ! total discharge or QH boundary
             qtfrct(n1) = qtfrct_global(gdp%gdbcdat%bct_order(n1))
          endif
       enddo
       if (allocated(qtfrct_global)) deallocate(qtfrct_global, stat=istat)
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
          ! smoothing
          !
          tdif = timnow*dt - tstart
          if (itlfsm>0 .and. tdif<=itlfsm*dt) then
             dini  = s0(np, mp)
             tfrac = tdif/(itlfsm*dt)
             diff  = circ2d(kp, kq) - dini
             circ2d(kp, kq) = dini + tfrac*diff
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
             endif
             !
             ! For total discharge boundary use fraction
             !
             if (ibtype == 7) then
                !
                ! Prevent division by zero
                !
                if (qtfrct(n1) == 0.0) qtfrct(n1) = 1.0
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
end subroutine incbc
