subroutine mom_cw &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,dumm1     ,dumm2     ,dumm3     , &
               & dumm4     ,dumm5     ,dumm6     ,dumm7     ,dumm8     ,mom_output, &
               & u1        ,kWDu      ,kWDv      ,GHOSTu1   , &
               & GHOSTv1   ,aguu      ,advecx    ,advecy    ,nst       ,irov      , &
               & xcor      ,ycor      ,gdp)
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
!  $Id: mom_cw.f90 6388 2016-08-01 01:05:36Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/mom_cw.f90 $
!!--description-----------------------------------------------------------------
!
! This subroutine is part of (called by) CUCNP. It computes the Horizontal 
! Advection in U-direction for the following two MOMSOL Options: 
!  either  explicit, central scheme (MOMSOL = WAQUA)
!  or      explicit, central scheme (MOMSOL = Cyclic; Ref.: Stelling & Leendertse
!                              "Approximation of Convective Processes by Cyclic
!                              AOI methods", Proc. 2nd ASCE Conf. on Estuarine
!                              and Coastal Modelling, Tampa, 1991)
! It computes the terms of the matrix elements AAK, BBK,  CCK and DDK of the
! equation. 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical      , pointer :: cstbnd
    logical      , pointer :: wind
    logical      , pointer :: struct
    real(fp), dimension(:,:)          , pointer :: mom_m_convec        ! convection u*du/dx term
    real(fp), dimension(:,:)          , pointer :: mom_m_xadvec        ! cross-advection v*du/dy term
    integer                       , pointer :: MODadvecGHOSTsud
    integer                       , pointer :: cutcell
    logical                       , pointer :: EXCLouterVEL
    logical                       , pointer :: getADJACENTgrad
    real(fp), dimension(:)        , pointer :: ududx
    real(fp), dimension(:)        , pointer :: vdudy
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANK
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: xcorV1
    real(fp), dimension(:,:)      , pointer :: ycorV1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:)      , pointer :: xcorU1
    real(fp), dimension(:,:)      , pointer :: ycorU1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    integer, dimension(:,:)       , pointer :: KFS_CC
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    logical                       , pointer :: vvvSECord
!
! Global variables
!
    integer                                                       :: nst
    integer                                                       :: irov 
    integer                                                       :: icx
    integer                                                       :: icy
    integer                                                       :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax  !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: GHOSTu1  
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: GHOSTv1 
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)      , intent(in) :: kWDu
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)      , intent(in) :: kWDv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: aguu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: xcor     
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: ycor 
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: advecx
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: advecy
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm3
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm4
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm5
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm6
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm7
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm8
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
                                                                             !  Only used in case mom_output = .true.
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: umean  !  Description and declaration in esm_alloc_real.f90
    logical                                           , intent(in) :: mom_output

!
! Local variables
!
    integer :: iad1
    integer :: iad2
    integer :: k
    integer :: n
    integer :: m
    integer :: kad
    integer :: kad2
    integer :: kspu0k
    integer :: ndm
    integer :: ndmd
    integer :: ndmu
    integer :: neigat  ! =0 for neighbour point is gate
    integer :: nm
    integer :: nmd
    integer :: nmu
    integer :: num
    integer :: numu
    integer :: numd
    integer :: kfv_ndm  
    integer :: kfv_ndmu  
    integer :: kfv_nm   
    integer :: kfv_nmu
    integer :: Lmax    
    integer :: nmLmax      
    real(fp):: advcxe
    !real(fp):: advecx
    !real(fp):: advecy
    real(fp):: geta
    real(fp):: gksi
    real(fp):: gsqi
    real(fp):: gvndm
    real(fp):: gvnm
    integer :: svvv
    real(fp):: uad
    real(fp):: uuu
    real(fp):: uvdgdy
    real(fp):: vvdgdx
    real(fp):: vvv
    real(fp):: vvvANA    
    real(fp):: u0nmk
    real(fp):: maxL1_x
    real(fp):: maxL1_y
    real(fp),allocatable :: uEXACT(:,:)
!
!! executable statements -------------------------------------------------------
!
    MODadvecGHOSTsud => gdp%gdimbound%MODadvecGHOSTsud
    cutcell          => gdp%gdimbound%cutcell
    EXCLouterVEL     => gdp%gdimbound%EXCLouterVEL
    getADJACENTgrad  => gdp%gdimbound%getADJACENTgrad
    EDGExyBANK       => gdp%gdimbound%EDGExyBANK
    EDGEtypeBANK     => gdp%gdimbound%EDGEtypeBANK
    PSIx             => gdp%gdimbound%PSIx
    PSIy             => gdp%gdimbound%PSIy
    xcorV1           => gdp%gdimbound%xcorV1
    ycorV1           => gdp%gdimbound%ycorV1
    xG_U1            => gdp%gdimbound%xG_U1
    yG_U1            => gdp%gdimbound%yG_U1
    xcorU1           => gdp%gdimbound%xcorU1
    ycorU1           => gdp%gdimbound%ycorU1
    xG_V1            => gdp%gdimbound%xG_V1
    yG_V1            => gdp%gdimbound%yG_V1
    KFS_CC           => gdp%gdimbound%KFS_CC
    ETAx             => gdp%gdimbound%ETAx
    ETAy             => gdp%gdimbound%ETAy
    vvvSECord        => gdp%gdimbound%vvvSECord
    wind       => gdp%gdprocs%wind
    struct     => gdp%gdprocs%struct
    cstbnd     => gdp%gdnumeco%cstbnd
    !
   ! if (mom_output) then !commente so I can pass it to extrADVECTsud_sub
       if (icx==1) then ! solve V/N component
          mom_m_convec => gdp%gdflwpar%mom_n_convec
          mom_m_xadvec => gdp%gdflwpar%mom_n_xadvec
       else ! solve U/M component
          mom_m_convec => gdp%gdflwpar%mom_m_convec
          mom_m_xadvec => gdp%gdflwpar%mom_m_xadvec
       endif
   ! endif
    !
    ! INITIALISATION
    !
    Lmax=-1    
    do k = 1, kmax
       nmd = -icx
       ndm = -icy
       ndmd = -icx - icy
       nmu = icx
       num = icy
       numu = icx + icy
       ndmu = icx - icy
       numd =-icx + icy
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1
          ndmd = ndmd + 1
          nmu = nmu + 1
          num = num + 1
          numu = numu + 1
          ndmu = ndmu + 1
          numd = numd + 1
          !
          ! For an active point and not a gate and plate
          !
          kspu0k = kspu(nm, 0)*kspu(nm, k)
          if (kfu(nm)==1 .and. kspu0k /=4 .and. kspu0k /=10) then
             gksi  = gvu(nm)
             geta  = guu(nm)
             gvnm  = gvd(nm )
             gvndm = gvd(ndm)
             gsqi  = gsqiu(nm )
             !
             !  For 2D weir use UUU value derived from flux QXK
             !
             if (abs(kspu(nm, 0))==9) then
                uuu = qxk(nm, k)/(guu(nm)*hu(nm))
                u0nmk = u0(nm, k)
             else
                uuu = u0(nm, k)
                u0nmk = uuu
             endif
             kfv_ndm  = kfv(ndm) * kWDv(ndm,2) 
             kfv_ndmu = kfv(ndmu)* kWDv(ndmu,2) 
             kfv_nm   = kfv(nm)  * kWDv(nm,1)
             kfv_nmu  = kfv(nmu) * kWDv(nmu,1)
             svvv = max(kfv_ndm+kfv_ndmu+kfv_nm+kfv_nmu, 1)             
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3               ) .or. EXCLouterVEL) then
                vvv = (v1(ndm, k)*kfv_ndm+ v1(ndmu, k)*kfv_ndmu + v1(nm, k)  &
                    & *kfv_nm + v1(nmu, k)*kfv_nmu)/svvv
             else
                vvv = .25*(v1(ndm, k)+ v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
             endif        
             
             !
             ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
             !
             uvdgdy = 0.5*vvv*gsqi*(gvnm - gvndm)
             !
             ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
             !
             vvdgdx = 0.5*vvv*gsqi*(guu(nmu) - guu(nmd))
             !
             ! ADVECTION IN U-DIRECTION; DU/DX AND CENTRIFUGAL ACCELERATION
             ! NON-CONSERVATIVE FORM WITH UPWIND NEAR BOUNDARIES AND DRY POINTS
             !
             advecx(nm,k) = 0.0
             advcxe = 0.0
             advecy(nm,k) = 0.0
             uad = 1.0
             if (uuu>0.0) then
                kspu0k = kspu(nmd, 0)*kspu(nmd, k)
                if (kspu0k==4 .or. kspu0k==10) then
                   neigat = 0
                else
                   neigat = 1
                endif
                if (kadu(nm, k)==0) then
                   !
                   ! Energy conservative discretisation for structure points
                   !
                   advecx(nm,k) = uad*2.*uvdgdy*kfu(nmd)*kWDu(nmd,2)*neigat
                   advcxe = ((u0nmk + u0(nmd, k))                        &
                          & *(u0nmk - u0(nmd, k)))*kfu(nmd)*kWDu(nmd,2)             &
                          & *neigat/(2*gksi)
                else
                   !
                   ! upwind approach near structure points and inactive
                   ! u-points for STANDARD (Cyclic) Delft3D FLOW
                   ! approach; No special discretisation at structure points
                   ! for WAQUA approach
                   !
                   iad1 = kfu(nmd)*kWDu(nmd,2)*kadu(nmd, k)      !*NINT(1._fp - real(min(ghostU1(nmd),1),fp))
                   iad2 = iad1*kfu(nmu)*kWDu(nmu,1)*kadu(nmu, k) !*NINT(1._fp - real(min(ghostU1(nmu),1),fp))
                   !
                   advecx(nm,k) = uad*                                             &
                          & (((0.5*iad2)*u0(nmu, k) + (iad1 - iad2)*u0nmk    &
                          & - (iad1 - 0.5*iad2)*u0(nmd, k)) /gksi)*kfu(nmd)*kWDu(nmd,2)*neigat + &
                          &  2.*uvdgdy
                   if (cutcell.eq.2.and.MODadvecGHOSTsud==2) THEN
                      if (getADJACENTgrad) then
                         if (comparereal(aguu(nm),0._fp).gt.0.and.comparereal(aguu(nm),1._fp).lt.0) then
                            if (kfu(num)==1) then !use gradient above (explicit)
                               u0nmk = u0(num, k)
                               iad1 = kfu(numd)*kWDu(numd,2)*kadu(numd, k)      
                               iad2 = iad1*kfu(numu)*kWDu(numu,1)*kadu(numu, k)  
                               advecx(nm,k) = uad*                                             &
                                      & (((0.5*iad2)*u0(numu, k) + (iad1 - iad2)*u0nmk    &
                                      & - (iad1 - 0.5*iad2)*u0(numd, k)) /gksi)*kfu(numd)*kWDu(numd,2)*neigat ! no curv
                            elseif (kfu(ndm)==1) then
                               u0nmk = u0(ndm, k)
                               iad1 = kfu(ndmd)*kWDu(ndmd,2)*kadu(ndmd, k)      
                               iad2 = iad1*kfu(ndmu)*kWDu(ndmu,1)*kadu(ndmu, k)  
                               advecx(nm,k) = uad*                                             &
                                      & (((0.5*iad2)*u0(ndmu, k) + (iad1 - iad2)*u0nmk    &
                                      & - (iad1 - 0.5*iad2)*u0(ndmd, k)) /gksi)*kfu(ndmd)*kWDu(ndmd,2)*neigat ! no curv
                            else
                               iad1 = 0
                               iad2 = 0
                            endif
                         endif
                      endif
                   endif
                endif
             else
                kspu0k = kspu(nmu, 0)*kspu(nmu, k)
                if (kspu0k==4 .or. kspu0k==10) then
                   neigat = 0
                else
                   neigat = 1
                endif
                if (kadu(nm, k)==0) then
                   !
                   ! Energy conservative discretisation for structure points
                   !
                   advecx(nm,k) = uad*2.*uvdgdy*kfu(nmu)*kWDu(nmu,1)*neigat
                   advcxe = ((u0(nmu, k) + u0nmk)                        &
                          & *(u0(nmu, k) - u0nmk))*kfu(nmu)*kWDu(nmu,1)  &
                          & *neigat/(2*gksi)
                else
                   !
                   ! upwind approach near structure points and inactive
                   ! u-points for STANDARD (Cyclic) Delft3D FLOW
                   ! approach; No special discretisation at structure points
                   ! for WAQUA approach
                   !
                   iad1 = kfu(nmu)*kWDu(nmu,1)*kadu(nmu, k)      !*NINT(1._fp - real(min(ghostU1(nmu),1),fp))
                   iad2 = iad1*kfu(nmd)*kWDu(nmd,2)*kadu(nmd, k) !*NINT(1._fp - real(min(ghostU1(nmd),1),fp))
!
                   advecx(nm,k) = uad*                                             &
                          & (((iad1 - 0.5*iad2)*u0(nmu, k) + (iad2 - iad1)   &
                          & *u0nmk - (0.5*iad2)*u0(nmd, k)) /gksi)*kfu(nmu)*kWDu(nmu,1)*neigat +  &
                          &  2.*uvdgdy
                   if (cutcell.eq.2.and.MODadvecGHOSTsud==2) THEN
                      if (getADJACENTgrad) then
                         if (comparereal(aguu(nm),0._fp).gt.0.and.comparereal(aguu(nm),1._fp).lt.0) then
                            if (kfu(num)==1) then !use gradient above (explicit)
                               u0nmk = u0(num, k)
                               iad1 = kfu(numu)*kWDu(numu,1)*kadu(numu, k)      !*NINT(1._fp - real(min(ghostU1(nmu),1),fp))
                               iad2 = iad1*kfu(numd)*kWDu(numd,2)*kadu(numd, k) !*NINT(1._fp - real(min(ghostU1(nmd),1),fp))
                               advecx(nm,k) = uad*                                             &
                                      & (((iad1 - 0.5*iad2)*u0(numu, k) + (iad2 - iad1)   &
                                      & *u0nmk - (0.5*iad2)*u0(numd, k)) /gksi)*kfu(numu)*kWDu(numu,1)*neigat !no curv
                            elseif (kfu(ndm)==1) then !use gradient below (explicit)
                               u0nmk = u0(ndm, k)
                               iad1 = kfu(ndmu)*kWDu(ndmu,1)*kadu(ndmu, k)      !*NINT(1._fp - real(min(ghostU1(nmu),1),fp))
                               iad2 = iad1*kfu(ndmd)*kWDu(ndmd,2)*kadu(ndmd, k) !*NINT(1._fp - real(min(ghostU1(nmd),1),fp))
                               advecx(nm,k) = uad*                                             &
                                      & (((iad1 - 0.5*iad2)*u0(ndmu, k) + (iad2 - iad1)   &
                                      & *u0nmk - (0.5*iad2)*u0(ndmd, k)) /gksi)*kfu(ndmu)*kWDu(ndmu,1)*neigat +  &
                                      &  2.*uvdgdy
                            else
                               iad1 = 0
                               iad2 = 0
                            endif
                         endif
                      endif
                   endif

                endif
             endif
             !
             ! ADVECTION IN V-DIRECTION; DU/DY AND CENTRIFUGAL ACCELERATION
             !
             if (kadu(num, k)*kadu(ndm, k)*kadu(nm, k)==1) then
                if (cstbnd) then
                   !
                   ! leave vdu/dy intact at left and right hand boundary;
                   ! see also new treatment of vvv
                   !
                   if (kcs(nm)==2) then
                      kad2 = 1._fp  !kfv(nmu)*kWDv(nmu,1)*kfv(ndmu)*kWDv(ndmu,2) vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                      kad = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                    !  vvdgdx = 0.0
                   elseif (kcs(nmu)==2) then
                      kad2 = 1._fp !kfv(nm)*kWDv(nm,1)*kfv(ndm)*kWDv(ndm,2)  vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                      kad = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                    !  vvdgdx = 0.0
                   else
                     ! kad = kfv(nm)*kfv(nmu)*kfu(num)*kfv(ndm)*kfv(ndmu)     &
                     !     & *kfu(ndm)
                      if (vvv>0.) then
                         kad2 = 1._fp !kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                         kad  = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                      else
                         kad2 = 1._fp !kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                         kad  = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                      endif
                   endif
                elseif (vvv>0.) then
                   if ( EXCLouterVEL) then
                      kad2 = 1._fp !kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                   else
                      kad2 = kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) 
                   endif
                   kad  = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                else
                   if ( EXCLouterVEL) then
                      kad2 = 1._fp !kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2) vvv goes already to zero if they are all zeros. And if one is not zero I dont wanna sent it to zero
                   else
                      kad2 = kfv(nm)*kWDv(nm,1)*kfv(nmu)*kWDv(nmu,1)*kfv(ndm)*kWDv(ndm,2)*kfv(ndmu)*kWDv(ndmu,2)
                   endif
                   kad  = kad2*kfu(num)*kWDu(num,3)*kfu(ndm)*kWDu(ndm,4)
                endif
                advecy(nm,k) = kad*(0.5*vvv*(u0(num, k) - u0(ndm, k))              &
                          & /geta) - vvv*vvdgdx*kad2
             endif
             if (cutcell.eq.2) then
                if(MODadvecGHOSTsud==1) THEN
                   call nm_to_n_and_m(nm, n, m, gdp)
                   if (GHOSTu1(nm).EQ.1) then  
                      advecx(nm,k) = 0._fp
                      advecy(nm,k) = 0._fp
                      advcxe = 0._fp
                   endif 
                elseif(MODadvecGHOSTsud==2) THEN
                   if (GHOSTu1(nmd).EQ.1.or.GHOSTu1(nmu).EQ.1.or.GHOSTu1(nm).EQ.1) then  
                  ! if (GHOSTu1(nm).eq.1.or.(GHOSTu1(nmd).EQ.1.and.comparereal(aguu(nmd),0._fp).eq.0).or.(GHOSTu1(nmu).EQ.1.and.comparereal(aguu(nmu),0._fp).eq.0)) then
                  ! if (GHOSTu1(nm).eq.1) then
                 !      advecx(nm,k) = 0._fp
                   endif
                   !if (GHOSTu1(ndm).EQ.1.or.GHOSTu1(num).EQ.1) then  
                   !if (GHOSTu1(nm).eq.1.or.(GHOSTu1(ndm).EQ.1.and.comparereal(aguu(ndm),0._fp).eq.0).or.(GHOSTu1(num).EQ.1.and.comparereal(aguu(num),0._fp).eq.0)) then
                !   if (GHOSTu1(nm).eq.1 ) then !.or.GHOSTu1(ndm).EQ.1.or.GHOSTu1(num).EQ.1) then  
                   if (GHOSTu1(ndm).EQ.1.or.GHOSTu1(num).EQ.1) then  
                   !if (GHOSTu1(nm).eq.1) then
                      advecy(nm,k) = 0._fp
                   endif
                endif
             endif
             if (mom_output) then
                if (ghostu1(nm)/=1) then
                   mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - advecx(nm,k)*u1(nm, k) - advcxe
                   mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) - advecy(nm,k)
                endif
             else
                !
               bbk(nm, k)  = bbk(nm, k) + advecx(nm,k)
               ddk(nm, k)  = ddk(nm, k) - advecy(nm,k) - advcxe    
                !
             endif
          endif
       enddo
    enddo
    !
end subroutine mom_cw
