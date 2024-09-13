subroutine adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
                        & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
                        & suu       ,svv       ,sbuut     ,sbvvt     ,dzduu     , &
                        & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
                        & hu        ,hv        ,dm        ,hidexp    ,slopecor  , &
                        & avalan    ,rhowat    ,kmax      ,dps       ,gsqs      , &
                        & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
                        & gdp       )
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
!  $Id: adjust_bedload.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/adjust_bedload.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes 
! Method used: -
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use sediment_basics_module
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                         , pointer :: ag
    real(fp)      , dimension(:)     , pointer :: rhosol
    real(fp)      , dimension(:)     , pointer :: sedd50
    real(fp)      , dimension(:)     , pointer :: sedd50fld
    real(fp)                         , pointer :: alfabs
    real(fp)                         , pointer :: alfabn
    real(fp)                         , pointer :: wetslope
    real(fp)                         , pointer :: avaltime
    real(fp)                         , pointer :: ashld
    real(fp)                         , pointer :: bshld
    real(fp)                         , pointer :: cshld
    real(fp)                         , pointer :: dshld
    real(fp)                         , pointer :: alfpa
    real(fp)                         , pointer :: thcrpa
    integer                          , pointer :: islope
    integer       , dimension(:)     , pointer :: sedtyp
    real(fp)                         , pointer :: eps
    real(fp)                         , pointer :: morfac
    real(fp)                         , pointer :: hdt
    integer                 , pointer :: cutcell
    logical                 , pointer :: useCUTstyle
    logical                 , pointer :: skip_aval_adjust
    real(fp), dimension(:)  , pointer :: dzduu_w
    real(fp), dimension(:)  , pointer :: dzdvv_w
    integer, dimension(:)   , pointer :: isMERGEDv_bed
    integer, dimension(:)   , pointer :: isMERGEDu_bed
    integer                 , pointer :: CORRbedSLOPEcut
    real(fp), dimension(:)  , pointer :: dzduuCENTR
    real(fp), dimension(:)  , pointer :: dzdvvCENTR
    integer                 , pointer :: exactSLOPE
    real(fp), dimension(:,:), pointer :: xG_U1
    real(fp), dimension(:,:), pointer :: YG_U1
    real(fp), dimension(:,:), pointer :: xG_V1
    real(fp), dimension(:,:), pointer :: yG_V1
    real(fp), dimension(:,:), pointer :: xG
    real(fp), dimension(:,:), pointer :: yG
    real(fp)                , pointer :: ccofu_stored
    logical                 , pointer :: bdslpINupwnbed
!
! Global variables
!
    integer                                               , intent(in)  :: icx     !!  Increment in the X-dir., if ICX= NMAX
                                                                                   !!  then computation proceeds in the X-
                                                                                   !!  dir. If icx=1 then computation pro-
                                                                                   !!  ceeds in the Y-dir.
    integer                                               , intent(in)  :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                               , intent(in)  :: kmax    !  Number of layers
    integer                                               , intent(in)  :: kbed    !  Index of near bed layer
    integer                                               , intent(in)  :: lsedtot !!  Total number of sediment fractions
    integer                                               , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    logical                                               , intent(in)  :: avalan
    logical                                               , intent(in)  :: slopecor
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: dm
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: dzduu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: dzdvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot) , intent(in)  :: fixfac
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot) , intent(in)  :: frac
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot) , intent(in)  :: hidexp
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in)  :: rhowat
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)               :: suu     !  sbcuu, sbwuu, or sswuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)               :: svv     !  sbcvv, sbwvv, or sswvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                        :: sbuut
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                        :: sbvvt
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot) , intent(in)  :: taurat
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in)  :: ust2
!
! Local variables
!
    integer  :: idir      ! direction U=1, V=2
    integer  :: l
    integer  :: kf
    integer  :: ndu
    integer  :: ndv
    integer  :: ndm
    integer  :: ndmu
    integer  :: nm
    integer  :: nm2       ! nmu (idir==1) or num (idir==2)
    integer  :: nmd
    integer  :: nmu
    integer  :: num
    integer  :: numd
    integer  :: m
    integer  :: n
    logical  :: di50spatial
    real(fp) :: alfas
    real(fp) :: bagnol
    real(fp) :: cosa      ! used in computation of Koch-Flokstra bed slope effect
    real(fp) :: di50      ! local value of d50
    real(fp) :: delta
    real(fp) :: depth     ! local water depth (hu or hv)
    real(fp) :: dmloc     ! local value of dm
    real(fp) :: dzdn
    real(fp) :: dzds
    real(fp) :: dzdu
    real(fp) :: dzdv
    real(fp) :: fixf
    real(fp) :: fnorm
    real(fp) :: frc
    real(fp) :: ftheta    ! used in computation of Koch-Flokstra bed slope effect
    real(fp) :: hidexploc
    real(fp) :: phi
    real(fp) :: sbedcorr  ! corrected bedload component
    real(fp) :: sbedm
    real(fp) :: sbedu
    real(fp) :: sbedv
    real(fp) :: shield    ! Shields' parameter
    real(fp) :: sina      ! used in computation of Koch-Flokstra bed slope effect
    real(fp) :: tnorm     ! used in computation of Koch-Flokstra bed slope effect
    real(fp) :: tphi
    real(fp) :: tratio
    real(fp) :: ust2avg
    real(fp) :: avtime
    real(fp) :: avflux
    real(fp) :: slp
    real(fp) :: xx  
    real(fp) :: yy 
    real(fp) :: dzdr 
    real(fp) :: dzdteta 
    real(fp) :: x2y2 
    real(fp) :: drdx 
    real(fp) :: drdy  
    real(fp) :: dtetadx
    real(fp) :: dtetady 
!
!! executable statements -------------------------------------------------------
!
    cutcell          => gdp%gdimbound%cutcell
    useCUTstyle      => gdp%gdimbound%useCUTstyle
    skip_aval_adjust => gdp%gdimbound%skip_aval_adjust
    dzduu_w          => gdp%gdimbound%dzduu_w
    dzdvv_w          => gdp%gdimbound%dzdvv_w
    isMERGEDv_bed    => gdp%gdimbound%isMERGEDv_bed
    isMERGEDu_bed    => gdp%gdimbound%isMERGEDu_bed
    CORRbedSLOPEcut  => gdp%gdimbound%CORRbedSLOPEcut
    dzduuCENTR       => gdp%gdimbound%dzduuCENTR
    dzdvvCENTR       => gdp%gdimbound%dzdvvCENTR
    exactSLOPE       => gdp%gdimbound%exactSLOPE
    xG_U1            => gdp%gdimbound%xG_U1
    YG_U1            => gdp%gdimbound%YG_U1
    xG_V1            => gdp%gdimbound%xG_V1
    yG_V1            => gdp%gdimbound%yG_V1
    xG               => gdp%gdimbound%xG
    yG               => gdp%gdimbound%yG
    ccofu_stored     => gdp%gdimbound%ccofu_stored
    bdslpINupwnbed   => gdp%gdimbound%bdslpINupwnbed
    ag                  => gdp%gdphysco%ag
    rhosol              => gdp%gdsedpar%rhosol
    sedd50              => gdp%gdsedpar%sedd50
    sedd50fld           => gdp%gdsedpar%sedd50fld
    sedtyp              => gdp%gdsedpar%sedtyp
    alfabs              => gdp%gdmorpar%alfabs
    alfabn              => gdp%gdmorpar%alfabn
    wetslope            => gdp%gdmorpar%wetslope
    avaltime            => gdp%gdmorpar%avaltime
    ashld               => gdp%gdmorpar%ashld
    bshld               => gdp%gdmorpar%bshld
    cshld               => gdp%gdmorpar%cshld
    dshld               => gdp%gdmorpar%dshld
    alfpa               => gdp%gdmorpar%alfpa
    thcrpa              => gdp%gdmorpar%thcrpa
    islope              => gdp%gdmorpar%islope
    morfac              => gdp%gdmorpar%morfac
    eps                 => gdp%gdconst%eps
    hdt                 => gdp%gdnumeco%hdt
    !
    ! Make assumptions for friction angle
    !
    phi  = 30.0 / 180.0 * pi
    tphi = tan(phi)
    !
    do l = 1, lsedtot
       if (sedtyp(l) /= SEDTYP_COHESIVE) then
          di50        = sedd50(l)
          di50spatial = .false.
          if (di50<0 .and. lsedtot==1) di50spatial = .true.
          do nm = 1, nmmax
             !
             ! initialise variables
             !
             nmd   = nm - icx
             ndm   = nm - icy
             nmu   = nm + icx
             num   = nm + icy
             ndmu  = nm - icy + icx
             numd  = nm + icy - icx
             !
             ! clear temporary arrays
             !
             sbuut(nm) = 0.0
             sbvvt(nm) = 0.0
             !
             ! calculate bed gradient parallel and perpendicular to BED LOAD
             ! TRANSPORT vector.
             ! NOTE: gradient and transport vectors are calculated in U and V
             !       directions are calculated at each U and V point.
             !       i.e. at the locations at which the bed load transport
             !       components are actually applied.
             !
             do idir = 1,2
                !
                if (idir == 1) then
                   !
                   ! AT U POINT
                   !
                   if (kfu(nm)==0 .or. (abs(kcs(nm))/=1 .and. abs(kcs(nmu))/=1)) cycle
                   !
                   ! set bed gradients in u and v directions at u point
                   !
                   dzdv =0.0_fp
                   ndv = 0
                   if (cutcell==0.AND..not.useCUTstyle) then
                      dzdu = dzduu(nm)
                      if (kcv(nmu) > 0) then
                         dzdv = dzdv + dzdvv(nmu)
                         ndv  = ndv + 1
                      endif
                      if (kcv(nm) > 0) then
                         dzdv = dzdv + dzdvv(nm)
                         ndv  = ndv + 1
                      endif
                      if (kcv(ndmu) > 0) then
                         dzdv = dzdv + dzdvv(ndmu)
                         ndv  = ndv + 1
                      endif
                      if (kcv(ndm) > 0) then
                         dzdv = dzdv + dzdvv(ndm)
                         ndv  = ndv + 1
                      endif
                   else 
                      dzdu = dzduu_w(nm)
                      if (CORRbedSLOPEcut==1.or.CORRbedSLOPEcut==0) then
                      !uses kfv, so slope on points that are on bank are not considered. Also use slopes of wet velocity points dzduu_w/dzdvv_w
                         if (kfv(nmu) > 0 .and. isMERGEDv_bed(nmu)==0) then
                            dzdv = dzdv + dzdvv_w(nmu)
                            ndv  = ndv + 1
                         endif
                         if (kfv(nm) > 0 .and. isMERGEDv_bed(nm)==0) then
                            dzdv = dzdv + dzdvv_w(nm)
                            ndv  = ndv + 1
                         endif
                         if (kfv(ndmu) > 0 .and. isMERGEDv_bed(ndmu)==0) then
                            dzdv = dzdv + dzdvv_w(ndmu)
                            ndv  = ndv + 1
                         endif
                         if (kfv(ndm) > 0 .and. isMERGEDv_bed(ndm)==0) then
                            dzdv = dzdv + dzdvv_w(ndm)
                            ndv  = ndv + 1
                         endif
                      else !if (CORRbedSLOPEcut==2.or.CORRbedSLOPEcut==3) then
                         ! I dont need mask. kfu is 1 here (tested above). So dzdvvCENTR is defined.
                         dzdv = 0._fp !(dzdvvCENTR(nm) + dzdvvCENTR(num))*0.5_fp !max(1,ndv) is 1 since ndv is zero
                      endif
                   endif
                   dzdv = dzdv/max(1,ndv)
                   !
                   ! set bed load transports in u and v directions at u point
                   !
                   sbedu    = suu(nm, l)
                   if (cutcell==0) then
                      sbedv    = (svv(nm, l) + svv(nmu, l) + svv(ndm, l)               &
                               & + svv(ndmu, l))/4.0_fp
                   else
                      kf = max(kfv(nm) + kfv(nmu) + kfv(ndm) + kfv(ndmu),1)
                      sbedv = (kfv(nm)*svv(nm, l) + kfv(nmu)*svv(nmu, l) + kfv(ndm)*svv(ndm, l) &
                               & + kfv(ndmu)*svv(ndmu, l))/kf
                   endif
                   sbedcorr = sbedu
                   !
                   nm2   = nmu
                   depth = hu(nm)
                else ! idir==2
                   !
                   ! AT V POINT
                   !
                   if (kfv(nm)==0 .or. (abs(kcs(nm))/=1 .and. abs(kcs(num))/=1)) cycle
                   !
                   ! set bed gradients in u and v directions at v point
                   !
                   dzdu =0.0_fp
                   ndu = 0
                   if (cutcell==0.and..not.useCUTstyle) then
                      dzdv = dzdvv(nm)
                      if (kcu(num) > 0) then
                         dzdu = dzdu + dzduu(num)
                         ndu  = ndu + 1
                      endif
                      if (kcu(nm) > 0) then
                         dzdu = dzdu + dzduu(nm)
                         ndu  = ndu + 1
                      endif
                      if (kcu(numd) > 0) then
                         dzdu = dzdu + dzduu(numd)
                         ndu  = ndu + 1
                      endif
                      if (kcu(nmd) > 0) then
                         dzdu = dzdu + dzduu(nmd)
                         ndu  = ndu + 1
                      endif
                   else !uses kfu, so slope on points that are on bank are not considered. Also use slopes of wet velocity points dzduu_w/dzdvv_w
                      dzdv = dzdvv_w(nm)
                      if (CORRbedSLOPEcut==1.or.CORRbedSLOPEcut==0) then
                         if (kfu(num) > 0 .and. isMERGEDu_bed(num)==0) then
                            dzdu = dzdu + dzduu_w(num)
                            ndu  = ndu + 1
                         endif
                         if (kfu(nm) > 0 .and. isMERGEDu_bed(nm)==0) then
                            dzdu = dzdu + dzduu_w(nm)
                            ndu  = ndu + 1
                         endif
                         if (kfu(numd) > 0 .and. isMERGEDu_bed(numd)==0) then
                            dzdu = dzdu + dzduu_w(numd)
                            ndu  = ndu + 1
                         endif
                         if (kfu(nmd) > 0 .and. isMERGEDu_bed(nmd)==0) then
                            dzdu = dzdu + dzduu_w(nmd)
                            ndu  = ndu + 1
                         endif
                      else !if (CORRbedSLOPEcut==2.or.CORRbedSLOPEcut==3) then
                         ! I dont need mask. kfv is 1 here (tested above). So dzduuCENTR is defined.
                         dzdu = 0._fp !(dzduuCENTR(nm) + dzduuCENTR(num))*0.5_fp !max(1,ndu) is 1 since ndu is zero
                      endif
                   endif
                   dzdu = dzdu/max(1,ndu)
                   !
                   ! set bed load transports in u and v directions at v point
                   !
                   sbedv    = svv(nm, l)
                   if (cutcell==0) then
                      sbedu    = (suu(nm, l) + suu(num, l) + suu(nmd, l)           &
                               & + suu(numd, l))/4.0_fp
                   else
                      kf = max(kfu(nm) + kfu(num) + kfu(ndm) + kfu(numd),1)
                      sbedu = (kfu(nm)*suu(nm, l) + kfu(num)*suu(num, l) + kfu(ndm)*suu(ndm, l) &
                               & + kfu(numd)*suu(numd, l))/kf
                   endif
                   sbedcorr = sbedv
                   !
                   nm2   = num
                   depth = hv(nm)
                endif
                ! if exactSLOPE>0 overwrite the numerical slope with the analytical one
                If (exactSLOPE ==1.and.cutcell==2) then
                   call nm_to_n_and_m(nm, n, m, gdp)
                   if (idir==1) then
                      xx = xG_U1(n,m)
                      yy = yG_U1(n,m)
                   else
                      xx = xG_V1(n,m)
                      yy = yG_V1(n,m)
                   endif
                   select case (exactSLOPE)
                   case(1)
                   !anular channel with 60 m of radius
                      dzdr =    0.043955483387823_fp !0.035 !
                      dzdteta = 0.06_fp
                      x2y2 = xx**2+yy**2
                      drdx = xx/sqrt(x2y2)
                      drdy = yy/sqrt(x2y2)
                      dtetadx = - yy/x2y2
                      dtetady =   xx/x2y2
                      dzdu = dzdr*drdx+dzdteta*dtetadx
                      dzdv = dzdr*drdy+dzdteta*dtetady
                      ! provide dzduu_w,dzdvv_w for printing
                      if (idir==1) then
                        dzduu_w(nm) = dzdu
                      else
                        dzdvv_w(nm) = dzdv
                      endif
                   case default
                      write(*,*) ' exactSLOPE not admitted' 
                      !pause
                      stop
                   end select
                   if (idir==1) then
                      write(98989898,*) nm,dzdu
                   else
                      write(98989899,*) nm,dzdv
                   endif
                endif
                !
                ! changed to reduce bed-load component perpendicular to dry points
                !
                ! calculate magnitude of bed-load transport
                !
                sbedm    = sqrt(sbedu**2 + sbedv**2)
                !
                if (sbedm>eps .and. slopecor.and..not.bdslpINupwnbed) then
                   dzds =  dzdu*sbedu/sbedm + dzdv*sbedv/sbedm
                   dzdn = -dzdu*sbedv/sbedm + dzdv*sbedu/sbedm
                   !
                   ! limit dzds to 90% of phi (Alberto: should not this also be applied to dzdn?)
                   !
                   dzds = min(0.9*tphi, dzds)
                   !
                   ! Apply bed slope effect according to
                   !   1: No correction
                   !   2: Bagnold (long. slope) and Ikeda / Van Rijn (transv. slope)
                   !   3: Van Bendegom and Koch & Flokstra
                   !   4: Parker and Andrews
                   !
                   select case (islope)
                   case(1)
                      !
                      ! no correction: default values
                      !
                   case(2)
                      !
                      ! adjust bed load for longitudinal bed slope (following Bagnold (1956))
                      ! note alfabs is user-specified scaling parameter
                      !
                      bagnol = tphi / (cos(atan(dzds))*(tphi-dzds))
                      alfas  = 1.0_fp + alfabs*(bagnol-1.0_fp)
                      alfas  = max(0.0_fp , alfas)
                      sbedu  = alfas * sbedu
                      sbedv  = alfas * sbedv
                      !
                      ! adjust bed load for transverse bed slope
                      ! note alfabn is user-specified scaling parameter
                      ! note taurat=(taubcw/taucrb) stored above
                      !
                      if (kcs(nm2) == 3) then
                         tratio = taurat(nm, l)
                      elseif (kcs(nm) == 3) then
                         tratio = taurat(nm2, l)
                      else
                         tratio = (taurat(nm, l) + taurat(nm2, l)) / 2.0
                      endif
                      if (tratio >= 1.0) then
                         fnorm = alfabn * (1.0/tratio)**0.5 * dzdn
                      else
                         fnorm = alfabn * dzdn
                      endif
                      !
                      ! note adjusted bedload put in temporary array so doesn't influence
                      ! surrounding points
                      !
                      if (idir == 1) then
                         sbedcorr = sbedu - sbedv*fnorm
                      else
                         sbedcorr = sbedv + sbedu*fnorm
                      endif
                   case(3,4,5)
                      !
                      ! 3: Formulation according Van Bendegom (1947), Koch & Flokstra (1980)
                      ! as described in Struiksma et al. (1985)
                      !
                      ! 4: Formulation according Parker & Andrews (1985)
                      !
                      ! 5: Formulaiton as Talmon 1995, but with constant Shield, h and v (typically at the centerline of the circular channel)
                      !
                      ust2avg = (ust2(nm) + ust2(nm2)) / 2.0_fp
                      if (di50spatial) then
                         di50 = sqrt(sedd50fld(nm)*sedd50fld(nm2))
                      endif
                      delta   = (rhosol(l) - rhowat(nm,kbed))/rhowat(nm,kbed)
                      shield  = ust2avg/ag/delta/di50
                      !
                      if (shield/=0.0_fp) then
                         if (islope==3) then
                            dmloc = sqrt(dm(nm)*dm(nm2))
                            if (comparereal(dmloc,0.0_fp)==0) then
                                if (abs(kcs(nm))==1) then
                                    dmloc = dm(nm)
                                elseif (abs(kcs(nm2))==1) then
                                    dmloc = dm(nm2)
                                endif
                            endif
                            ftheta  = ashld*(shield**bshld)* &
                                    & ((di50/depth)**cshld)*((di50/dmloc)**dshld)
                         elseif(islope==4) then
                            hidexploc = (hidexp(nm, l)+hidexp(nm2, l)) / 2.0_fp
                            ftheta    = alfpa * sqrt( shield / &
                                      & max(shield*0.1_fp , hidexploc*thcrpa) )
                         endif
                      else
                         ftheta  = 0.0_fp
                      endif
                      if (islope==5) then
                         !u_axis = 1.473450872804613
                         shield = 1.473450872804613_fp**2/di50/delta/ccofu_stored**2;
                         !if (comparereal(di50,0.0002_fp)==0) then
                         !   shield =    4.111851277592192_fp 
                         !elseif (comparereal(di50,0.001_fp)==0) then
                         !   shield =     0.822370255518438_fp 
                         !elseif (comparereal(di50,0.002_fp)==0) then
                         !   shield =     0.411185127759219_fp
                         !else
                         !   write(*,*) 'shield not defined for islope=5'
                         !   stop 
                         !   pause
                         !endif
                         ftheta  = ashld*(shield**bshld)  !note  cshld and dshld are zero by default,while ashld=0.85 and bshld=0.5 . So ftheta  = 0.85*(shield**0.5), as in eq 17 of Talmon 1995                           
                      endif
                      !
                      ! deal with exeptional case when ftheta, dzdv and dzdu are exactly
                      ! equal to zero
                      !
                      if (dzdu/=0.0_fp .or. dzdv/=0.0_fp) then
                         sina    = ftheta*sbedu/sbedm + dzdu
                         cosa    = ftheta*sbedv/sbedm + dzdv
                      else
                         sina    = sbedu/sbedm
                         cosa    = sbedv/sbedm
                      endif
                      tnorm = sqrt(sina**2 + cosa**2)
                      !
                      ! note adjusted bedload put in temporary array so doesn't influence
                      ! surrounding points
                      !
                      sbedm = sbedm * (1.0_fp + alfabs*dzds)
                      if (idir == 1) then
                         sbedcorr = sbedm * (sina/tnorm)
                      else
                         sbedcorr = sbedm * (cosa/tnorm)
                      endif
                   endselect
                endif
                ! 
                if (avalan.and..not.skip_aval_adjust) then !TO BE MODIFIED BY ADDING AGSQS, PAYING ATTENTION TO BY ZERO DIVISIONS
                   !               
                   ! Avalanching (MvO, 2011-04-06)
                   !
                   ! To be used instead of avalanching routine that is called at the end of BOTT3D.
                   ! Uses a maximum wet slope (keyword WetSlope in the mor file).
                   ! The default for Wetslope is 10.0 (i.e. 10:1, extremely steep, so no avalanching).
                   !
                   ! Sediment flux (avflux) equals volume exchange between two adjacent cells that is required
                   ! to reach maximum allowed slope, divided by avalanching time (1 day). This avalanching time has
                   ! no real physical meaning! The sediment flux due to avalanching is added to the bed load transport.
                   !
                   ! The wet slope should really be a function of sediment characteristics. This has not yet been implemented.
                   !
                   slp = sqrt(dzduu(nm)**2 + dzdvv(nm)**2)
                   !
                   if (slp>wetslope) then
                      if (idir == 1) then
                         avflux = gsqs(nm)*((dps(nmu) - dps(nm) + wetslope*(dzduu(nm)/slp)*gvu(nm)) / (1.0 + gsqs(nm)/gsqs(nmu))) / avaltime
                         sbedcorr = sbedcorr + avflux*rhosol(l)/guu(nm)
                      else
                         avflux = gsqs(nm)*((dps(num) - dps(nm) + wetslope*(dzdvv(nm)/slp)*guv(nm)) / (1.0 + gsqs(nm)/gsqs(num))) / avaltime
                         sbedcorr = sbedcorr + avflux*rhosol(l)/gvv(nm)
                      endif
                   endif
                   !
                endif
                !
                ! Apply upwind frac and fixfac.
                !
                ! At inflow (open and dd) boundaries the fixfac should not be taken upwind.
                !
                if ((sbedcorr>0.0 .and. abs(kcs(nm))==1) .or. abs(kcs(nm2))/=1) then
                   fixf = fixfac(nm,l)
                else
                   fixf = fixfac(nm2,l)
                endif
                !
                ! At dd boundaries the fraction should not be taken upwind (because these quantities have not been communicated).
                !
                if ((sbedcorr>0.0 .and. kcs(nm)/=3) .or. (kcs(nm2)==3)) then
                   frc = frac(nm,l)
                else
                   frc = frac(nm2,l)
                endif
                sbedcorr = sbedcorr * frc * fixf
                !
                if (idir == 1) then
                   sbuut(nm) = sbedcorr
                else
                   sbvvt(nm) = sbedcorr
                endif
                !
                ! continue to next direction
                !
             enddo
             If (exactSLOPE ==1.and.cutcell==2) then !xG not defined for non cutcells
                if (kfu(nm)==1.or.kfv(nm)==1.or.kfu(nmd)==1.or.kfv(ndm)==1) then !since kfs is not passed
                  call nm_to_n_and_m(nm, n, m, gdp)
                  ! provide dzduuCENTR,dzdvvCENTR for printing. Note that it has done twice (idir==1 and 2) but it still not enough, since both the dir for a cut cell can be wall and it is skipped 
                  !dzdr and dzdteta defined above
                  select case (exactSLOPE)
                  case(1)
                  !anular channel with 60 m of radius
                     xx = xG(n,m)
                     yy = yG(n,m)
                     x2y2 = xx**2+yy**2
                     drdx = xx/sqrt(x2y2)
                     drdy = yy/sqrt(x2y2)
                     dtetadx = - yy/x2y2
                     dtetady =   xx/x2y2
                     dzduuCENTR(nm) = dzdr*drdx+dzdteta*dtetadx
                     dzdvvCENTR(nm) = dzdr*drdy+dzdteta*dtetady  
                  case default
                     write(*,*) ' exactSLOPE not admitted' 
                     !pause
                     stop
                  end select

                endif
             endif                 
          enddo
          !
          ! continue to next nm
          !
          ! transfer values back into bedload arrays
          !
          do nm = 1, nmmax
             !
             ! Copy sbuut/sbvvt to suu(l)/svv(l)
             ! Put a zero on positions in suu/svv that must be overwritten by neighbouring domains
             !
             nmd   = nm - icx
             ndm   = nm - icy
             nmu   = nm + icx
             num   = nm + icy
             !
             ! suu
             !
             if (abs(kcs(nm))==1 .and. kcs(nmu)==3) then
                if (sbuut(nm) < 0.0_fp) then ! To be used: sbu(nm)
                   ! transport from right neighbour into this domain: this suu must be overwritten
                   suu(nm,l) = 0.0_fp
                else
                   suu(nm,l) = sbuut(nm)
                endif
             elseif (kcs(nm)==3 .and. abs(kcs(nmu))==1) then
                if (sbuut(nm) > 0.0_fp) then ! To be used: sbu(nmu)
                   ! transport from left neighbour into this domain: this suu must be overwritten
                   suu(nm,l) = 0.0_fp
                else
                   suu(nm,l) = sbuut(nm)
                endif
             else
                suu(nm,l) = sbuut(nm)
             endif
             !
             ! svv
             !
             if (abs(kcs(nm))==1 .and. kcs(num)==3) then
                if (sbvvt(nm) < 0.0_fp) then ! To be used: sbv(nm)
                   ! transport from top neighbour into this domain: this svv must be overwritten
                   svv(nm,l) = 0.0_fp
                else
                   svv(nm,l) = sbvvt(nm)
                endif
             elseif (kcs(nm)==3 .and. abs(kcs(num))==1) then
                if (sbvvt(nm) > 0.0_fp) then ! To be used: sbv(num)
                   ! transport from bottom neighbour into this domain: this svv must be overwritten
                   svv(nm,l) = 0.0_fp
                else
                   svv(nm,l) = sbvvt(nm)
                endif
             else
                svv(nm,l) = sbvvt(nm)
             endif
          enddo
       endif
    enddo
end subroutine adjust_bedload
