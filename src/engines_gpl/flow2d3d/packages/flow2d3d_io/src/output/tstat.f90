subroutine tstat(prshis    ,selhis    ,rhow      ,zmodel    ,nostat    , &
               & nmax      ,mmax      ,kmax      ,lmax      ,lstsci    , &
               & ltur      ,lsal      ,ltem      ,lsed      ,lsedtot   , &
               & kfs       ,kfu       ,kfv       ,kcs       ,kfuz1     , &
               & kfvz1     ,kfumin    ,kfumax    ,kfvmin    ,kfvmax    , &
               & kfsmin    ,kfsmax    ,zkfs      ,s1        ,u1        , &
               & v1        ,r1        ,rtur1     ,wphy      ,qxk       , &
               & qyk       ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
               & alfas     ,vicww     ,dicww     ,rich      ,rho       , &
               & ws        ,dps       , &
               & zwl       ,zalfas    ,zcuru     ,zcurv     ,zcurw     , &
               & zqxk      ,zqyk      ,gro       ,ztur      , &
               & ztauks    ,ztauet    ,zvicww    ,zdicww    ,zrich     , &
               & zrho      ,zbdsed    ,zrsdeq    ,zdpsed    ,zdps      , &
               & zws       ,hydprs    ,p1        ,vortic    ,enstro    , &
               & zvort     ,zenst     ,zsbu      ,zsbv      ,zssu      , &
               & zssv      ,sbuu      ,sbvv      , &
               & zhs       ,ztp       ,zdir      ,zrlabd    ,zuorb     , &
               & hrms      ,tp        ,teta      ,rlabda    ,uorb      , &
               & wave      ,zrca      ,windu     ,windv     , &
               & zwndsp    ,zwnddr    ,patm      ,zairp     ,wind      , &
               & precip    ,evap      ,zprecp    ,zevap     ,gdp       )
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
!  $Id: tstat.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/tstat.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Updates the monitoring station informations at
!                each time step
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use mathconsts
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , dimension(:,:)             , pointer :: mnstat
    type (flwoutputtype)                 , pointer :: flwoutput
    real(fp), dimension(:,:)             , pointer :: rca
    real(fp), dimension(:,:)             , pointer :: rsedeq
    real(fp), dimension(:,:)             , pointer :: ssuu
    real(fp), dimension(:,:)             , pointer :: ssvv
!
! Global variables
!
    integer                                                                      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                    :: lmax   !  Description and declaration in dimens.igs
    integer                                                                                    :: lsal   !  Description and declaration in dimens.igs
    integer                                                                      , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                                    :: ltem   !  Description and declaration in dimens.igs
    integer                                                                      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                      , intent(in)  :: nostat !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90    
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(nostat)                                                , intent(out) :: zkfs   !  Description and declaration in esm_alloc_int.f90
    logical                                                                      , intent(in)  :: wave   !  Description and declaration in procs.igs
    logical                                                                      , intent(in)  :: wind   !  Description and declaration in procs.igs
    logical                                                                      , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                                                                     , intent(in)  :: rhow   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubpu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubpv !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubsv !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: teta   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in)  :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: enstro !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: vortic !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: wphy   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: windu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: windv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: patm   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: precip !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: zalfas !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                              :: zdir
    real(fp)  , dimension(nostat)                                                , intent(out) :: zdps   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: zdpsed !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: zhs
    real(fp)  , dimension(nostat)                                                , intent(out) :: zrlabd
    real(fp)  , dimension(nostat)                                                , intent(out) :: ztauet !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: ztauks !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: ztp
    real(fp)  , dimension(nostat)                                                , intent(out) :: zuorb
    real(fp)  , dimension(nostat)                                                , intent(out) :: zwndsp
    real(fp)  , dimension(nostat)                                                , intent(out) :: zwnddr
    real(fp)  , dimension(nostat)                                                , intent(out) :: zairp
    real(fp)  , dimension(nostat)                                                , intent(out) :: zprecp !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: zevap  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat)                                                , intent(out) :: zwl    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, 0:kmax)                                        , intent(out) :: zdicww !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, 0:kmax)                                        , intent(out) :: zrich  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, 0:kmax)                                        , intent(out) :: zvicww !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, 0:kmax, lsed)                                  , intent(out) :: zws    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, 0:kmax, ltur)                                  , intent(out) :: ztur   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: hydprs !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zcuru  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zcurv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zcurw  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zenst  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zqxk   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zqyk   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zrho   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax)                                          , intent(out) :: zvort  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zrsdeq !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, kmax, lstsci)                                  , intent(out) :: gro    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zbdsed !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zrca   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zsbu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsedtot)                                       , intent(out) :: zsbv   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zssu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nostat, lsed)                                          , intent(out) :: zssv   !  Description and declaration in esm_alloc_real.f90
    character(23)                                                                , intent(in)  :: prshis !  Description and declaration in tricom.igs
    character(23)                                                                , intent(in)  :: selhis !  Description and declaration in tricom.igs
!
! Local variables
!
    integer :: ii
    integer :: k     ! Help var.
    integer :: kf
    integer :: kfd
    integer :: km
    integer :: l     ! Help var.
    integer :: m     ! Help var. counter for array index in the X-/M-direction
    integer :: md    ! M-1
    integer :: n     ! Help var. counter for array index in the Y-/N-direction
    integer :: nd    ! N-1
    integer :: ndm
    integer :: nm
    integer :: nmd
    real(fp):: sqrt2
!
!! executable statements -------------------------------------------------------
!
    ! Parallel:
    ! When n,m is in the halo, kcs is -1
    ! => always use the ABSOLUTE value of kcs
    !
    mnstat              => gdp%gdstations%mnstat
    flwoutput           => gdp%gdflwpar%flwoutput
    rca                 => gdp%gderosed%rca
    rsedeq              => gdp%gderosed%rsedeq
    ssuu                => gdp%gderosed%e_ssn
    ssvv                => gdp%gderosed%e_sst
    !
    ! Store water-levels and concentrations in defined stations
    ! and calculated discharges to zeta points
    ! for defined stations
    ! Store mask array KFS (active or non-active zeta-point) in zkfs for all stations
    !
    zwl  = -999.0_fp
    zkfs = -1
    if (prshis(1:1)=='Y' .or. selhis(1:1)=='Y') then
       do ii = 1, nostat
          m        = mnstat(1, ii)
          if (m<0) cycle
          n        = mnstat(2, ii)
          if (n<0) cycle
          zwl(ii)  = s1(n, m)
          zkfs(ii) = kfs(n, m)
       enddo
    endif
    !
    ! Calculated U-velocities to zeta points for defined stations
    ! The boundary points on M=1 use U1(N,M) instead of U1(N,M-1)
    ! to avoid velocities which are two times to large multiply
    ! U1(N,M-1) with (M-MD)
    !
    if (index(prshis(2:5), 'Y')/=0 .or. index(selhis(2:3), 'Y')/=0) then
       zcuru = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          md = max(1, m - 1)
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n,m)) then
                   zcuru(ii, k) = abs(kcs(n,m))                            &
                                & * (         kfuz1(n,m ,k)*u1(n,m ,k)     &
                                &    + (m-md)*kfuz1(n,md,k)*u1(n,md,k) )/2.0_fp
                endif
             else
                zcuru(ii, k) = abs(kcs(n,m))                        &
                             & * (         kfu(n,m )*u1(n,m ,k)     &
                             &    + (m-md)*kfu(n,md)*u1(n,md,k) )/2.0_fp
             endif
          enddo
       enddo
    endif
    !
    ! Calculated V-velocities to zeta points for defined stations
    ! The boundary points on N=1 use V1(N,M) instead of V1(N-1,M)
    ! to avoid velocities which are two times to large multiply
    ! V1(N-1,M) with (N-ND)
    !
    if (index(prshis(2:5), 'Y')/=0 .or. index(selhis(2:3), 'Y')/=0) then
       zcurv = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          nd = max(1, n - 1)
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                   zcurv(ii, k) = abs(kcs(n,m))                            &
                                & * (         kfvz1(n ,m,k)*v1(n ,m,k)     &
                                &    + (n-nd)*kfvz1(nd,m,k)*v1(nd,m,k) )/2.0_fp
                endif
             else
                zcurv(ii, k) = abs(kcs(n,m))                        &
                             & * (         kfv(n ,m)*v1(n ,m,k)     &
                             &    + (n-nd)*kfv(nd,m)*v1(nd,m,k) )/2.0_fp
             endif
          enddo
       enddo
    endif
    !
    ! Store angles formed by the line spanned vy the U-velocity points
    ! around the zeta point and the x-axis in defined stations
    !
    if (index(prshis(2:5), 'Y')/=0) then
       zalfas = -999.0_fp
       do ii = 1, nostat
          m          = mnstat(1, ii)
          if (m<0) cycle
          n          = mnstat(2, ii)
          if (n<0) cycle
          zalfas(ii) = alfas(n, m)
       enddo
    endif
    !
    ! Store W-physical velocities in defined stations
    !
    if (prshis(6:6)=='Y' .or. selhis(4:4)=='Y') then
       zcurw = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                   zcurw(ii, k) = wphy(n, m, k)
                endif
             else
                zcurw(ii, k) = wphy(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store concentrations in defined stations
    !
    if (index(prshis(7:14), 'Y')/=0 .or. index(selhis(5:12), 'Y')/=0) then
       gro = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 1, kmax
             do l = 1, lstsci
                if (zmodel) then
                   if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                      gro(ii, k, l) = r1(n, m, k, l)
                   endif
                else
                   gro(ii, k, l) = r1(n, m, k, l)
                endif
             enddo
          enddo
       enddo
    endif
    !
    ! Store non-hydrostatic pressure in defined stations
    !
    if (index(selhis(21:21), 'Y')/=0 .and. zmodel) then
       hydprs = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 1, kmax
             if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                hydprs(ii, k) = p1(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store turbulence quantities in defined stations
    !
    if (index(prshis(15:16), 'Y')/=0 .or. index(selhis(13:14), 'Y')/=0) then
       ztur = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 0, kmax
             do l = 1, ltur
                if (zmodel) then
                   if (k>=(kfsmin(n, m)-1) .and. k<=kfsmax(n, m)) then
                      ztur(ii, k, l) = rtur1(n, m, k, l)
                   endif
                else
                   ztur(ii, k, l) = rtur1(n, m, k, l)
                endif
             enddo
          enddo
       enddo
    endif
    !
    ! Store bottom friction in KSI direction in defined stations
    ! The boundary points on M=1 use TAU(N,M) instead of TAU(N,M-1)
    ! to avoid bottom stresses which are two times to large multiply
    ! TAU(N,M-1) with (M-MD)
    !
    if (selhis(15:15)=='Y') then
       ztauks = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          md = max(1, m - 1)
          if (zmodel) then
             km  = max(kfsmin(n, m), 1)
             kf  = kfuz1(n, m, km)
             kfd = kfuz1(n, md, km)
          else
             km  = kmax
             kf  = kfu(n, m)
             kfd = kfu(n, md)
          endif
          ztauks(ii) = rhow * abs(kcs(n,m))                                          &
                     & * (         (taubpu(n,m )*u1(n,m ,km) + taubsu(n,m ))*kf      &
                     &    + (m-md)*(taubpu(n,md)*u1(n,md,km) + taubsu(n,md))*kfd )/2.0_fp
       enddo
    endif
    !
    ! Store bottom friction in ETA direction in defined stations
    ! The boundary points on N=1 use TAU(N,M) instead of TAU(N-1,M)
    ! to avoid bottom stresses which are two times to large multiply
    ! TAU(N-1,M) with (N-ND)
    !
    if (selhis(16:16)=='Y') then
       ztauet = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          nd = max(1, n - 1)
          if (zmodel) then
             km  = max(kfsmin(n, m), 1)
             kf  = kfvz1(n, m, km)
             kfd = kfvz1(nd, m, km)
          else
             km  = kmax
             kf  = kfv(n, m)
             kfd = kfv(nd, m)
          endif
          ztauet(ii) = rhow * abs(kcs(n,m))                                          &
                     & * (         (taubpv(n ,m)*v1(n ,m,km) + taubsv(n ,m))*kf      &
                     &    + (n-nd)*(taubpv(nd,m)*v1(nd,m,km) + taubsv(nd,m))*kfd )/2.0_fp
       enddo
    endif
    !
    ! Store vertical eddy viscosity in defined stations
    ! vicww is defined on cell boundary planes
    !
    if (index(prshis(17:18), 'Y')/=0 .or. selhis(17:17)=='Y') then
       zvicww = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 0, kmax
             if (zmodel) then
                if (k>=(kfsmin(n, m)-1) .and. k<=kfsmax(n, m)) then
                   zvicww(ii, k) = vicww(n, m, k)
                endif
             else
                zvicww(ii, k) = vicww(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store vertical eddy diffusivity in defined stations
    ! dicww is defined on cell boundary planes
    !
    if (index(prshis(17:18), 'Y')/=0 .or. selhis(18:18)=='Y') then
       zdicww = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 0, kmax
             if (zmodel) then
                if (k>=(kfsmin(n, m)-1) .and. k<=kfsmax(n, m)) then
                   zdicww(ii, k) = dicww(n, m, k)
                endif
             else
                zdicww(ii, k) = dicww(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store Richardson numbers in defined stations
    !
    if (index(prshis(17:18), 'Y')/=0 .or. index(selhis(17:18), 'Y')/=0) then
       zrich = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 0, kmax
             if (zmodel) then
                if (k>=(kfsmin(n, m)-1) .and. k<=kfsmax(n, m)) then
                   zrich(ii, k) = rich(n, m, k)
                endif
             else
                zrich(ii, k) = rich(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store Vorticity and Enstrophy in defined stations
    !
    if (flwoutput%vortic) then
       zvort = -999.0_fp
       zenst = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                   zvort(ii, k) = vortic(n, m, k)
                   zenst(ii, k) = enstro(n, m, k)
                endif
             else
                zvort(ii, k) = vortic(n, m, k)
                zenst(ii, k) = enstro(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Store density in defined stations
    !
    if (prshis(19:19)=='Y' .or. selhis(19:19)=='Y') then
       zrho = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                   zrho(ii, k) = rho(n, m, k)
                endif
             else
                zrho(ii, k) = rho(n, m, k)
             endif
          enddo
       enddo
    endif
    !
    ! Calculated discharges to zeta points for defined stations
    ! The boundary points on M=1 use QXK(N,M) instead of QXK(N,M-1)
    ! to avoid discharges which are two times to large multiply
    ! QXK(N,M-1) with (M-MD). Similar the boundary points on N=1 use
    ! QYK(N,M) instead of QYK(N,M-1) to avoid discharges which are
    ! two times to large multiply QYK(N-1,M) with (N-ND)
    !
    if (selhis(20:20)=='Y') then
       zqxk = -999.0_fp
       zqyk = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          md = max(1, m - 1)
          nd = max(1, n - 1)
          do k = 1, kmax
             if (zmodel) then
                if (k>=kfsmin(n, m) .and. k<=kfsmax(n, m)) then
                   zqxk(ii, k) = abs(kcs(n,m))                             &
                               & * (         kfuz1(n,m ,k)*qxk(n,m ,k)     &
                               &    + (m-md)*kfuz1(n,md,k)*qxk(n,md,k) )/2.0_fp
                   zqyk(ii, k) = abs(kcs(n,m))                             &
                               & * (         kfvz1(n ,m,k)*qyk(n ,m,k)     &
                               &    + (n-nd)*kfvz1(nd,m,k)*qyk(nd,m,k) )/2.0_fp
                endif
             else
                zqxk(ii, k) = abs(kcs(n,m))                         &
                            & * (         kfu(n,m )*qxk(n,m ,k)     &
                            &    + (m-md)*kfu(n,md)*qxk(n,md,k) )/2.0_fp
                zqyk(ii, k) = abs(kcs(n,m))                         &
                            & * (         kfv(n ,m)*qyk(n ,m,k)     &
                            &    + (n-nd)*kfv(nd,m)*qyk(nd,m,k) )/2.0_fp
             endif
          enddo
       enddo
    endif
    !
    ! Depth may vary due to morphology or due to movement of the observation point.
    !
    zdps   = -999.0_fp
    do ii = 1, nostat
       m  = mnstat(1, ii)
       if (m<0) cycle
       n  = mnstat(2, ii)
       if (n<0) cycle
       zdps(ii)   = real(dps(n, m),fp)
    enddo
    !
    ! Store quantities specific for sediment in defined stations
    !
    if (lsed>0) then
       zrca   = -999.0_fp
       zsbu   = -999.0_fp
       zsbv   = -999.0_fp
       zssu   = -999.0_fp
       zssv   = -999.0_fp
       zws    = -999.0_fp
       zrsdeq = -999.0_fp
       do ii = 1, nostat
          m  = mnstat(1, ii)
          if (m<0) cycle
          n  = mnstat(2, ii)
          if (n<0) cycle
          md = max(1, m - 1)
          nd = max(1, n - 1)
          !
          call n_and_m_to_nm(n , m , nm , gdp)
          call n_and_m_to_nm(n , md, nmd, gdp)
          call n_and_m_to_nm(nd, m , ndm, gdp)
          !
          zdps(ii)   = real(dps(n, m),fp)
          do l = 1, lsed
             zrca(ii, l)   = rca(nm, l)
             zrsdeq(ii, l) = rsedeq(nm, l)
             zsbu(ii, l) = abs(kcs(n,m))                          &
                         & * (         kfu(n,m )*sbuu(n,m ,l)     &
                         &    + (m-md)*kfu(n,md)*sbuu(n,md,l) )/2.0_fp
             zsbv(ii, l) = abs(kcs(n,m))                          &
                         & * (         kfv(n ,m)*sbvv(n ,m,l)     &
                         &    + (n-nd)*kfv(nd,m)*sbvv(nd,m,l) )/2.0_fp
             zssu(ii, l) = abs(kcs(n,m))                          &
                         & * (         kfu(n,m )*ssuu(nm ,l)      &
                         &    + (m-md)*kfu(n,md)*ssuu(nmd,l) )/2.0_fp
             zssv(ii, l) = abs(kcs(n,m))                          &
                         & * (         kfv(n ,m)*ssvv(nm ,l)      &
                         &    + (n-nd)*kfv(nd,m)*ssvv(ndm,l) )/2.0_fp
             do k = 0, kmax
                zws(ii, k, l) = ws(n, m, k, l)
             enddo
          enddo
       enddo
    endif
    !
    if (lsedtot>0) then
       call tstat_bed(nostat, lsedtot, nmax, zdpsed, zbdsed, gdp)
    endif
    !
    ! Store wind speed and direction in defined stations
    !
    if (wind) then
       zwndsp = -999.0_fp
       zwnddr = -999.0_fp
       zairp  = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          !
          zwndsp(ii) = sqrt(windu(n, m)**2 + windv(n, m)**2)
          !
          ! zwnddir is in nautical convention (direction wind is coming from, CW from North)
          !
          zwnddr(ii) = 270.0_fp - 180.0_fp*(atan2(windv(n, m), windu(n, m)))/pi
          if (zwnddr(ii)<0.0_fp)    zwnddr(ii) = zwnddr(ii) + 360.0_fp
          if (zwnddr(ii)>=360.0_fp) zwnddr(ii) = zwnddr(ii) - 360.0_fp
          !
          zairp(ii) = patm(n, m)
          !
       enddo
    endif
    !
    ! Store precipitation and evaporation in defined stations
    !
    if (flwoutput%air) then
       zprecp = -999.0_fp
       zevap   = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          !
          ! Precipitation: convert from m/s to mm/h
          zprecp(ii) = precip(n, m) * 3600000.0_fp
          !
          ! Evaporation: convert from kg/s to mm/h
          zevap(ii)  = evap(n, m) * 3600000.0_fp / rhow
       enddo
    endif
    !
    ! Store quantities specific for waves in defined stations
    !
    if (wave) then
       sqrt2 = sqrt(2.0_fp)
       zhs    = -999.0_fp
       ztp    = -999.0_fp
       zdir   = -999.0_fp
       zrlabd = -999.0_fp
       zuorb  = -999.0_fp
       do ii = 1, nostat
          m = mnstat(1, ii)
          if (m<0) cycle
          n = mnstat(2, ii)
          if (n<0) cycle
          !
          zhs(ii) = hrms(n, m)*sqrt2
          if (zhs(ii) > 0.01_fp) then
             ztp(ii) = tp(n, m)
             !
             ! zdir is in nautical convention (direction waves are coming from, CW from North)
             !
             zdir(ii) = 270.0_fp - (teta(n, m) + zalfas(ii))
             if (zdir(ii)<0.0_fp)    zdir(ii) = zdir(ii) + 360.0_fp
             if (zdir(ii)>=360.0_fp) zdir(ii) = zdir(ii) - 360.0_fp
             zrlabd(ii) = rlabda(n, m)
             zuorb(ii)  = uorb(n, m)
          endif
       enddo
    endif
end subroutine tstat
