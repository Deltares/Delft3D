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
    integer       , dimension(:)     , pointer :: tratyp
    real(fp)                         , pointer :: eps
    real(fp)                         , pointer :: morfac
    real(fp)                         , pointer :: hdt
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
!
!! executable statements -------------------------------------------------------
!
    ag                  => gdp%gdphysco%ag
    rhosol              => gdp%gdsedpar%rhosol
    sedd50              => gdp%gdsedpar%sedd50
    sedd50fld           => gdp%gdsedpar%sedd50fld
    tratyp              => gdp%gdsedpar%tratyp
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
       if (has_bedload(tratyp(l))) then
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
                   dzdu = dzduu(nm)
                   dzdv =0.0_fp
                   ndv = 0
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
                   dzdv = dzdv/max(1,ndv)
                   !
                   ! set bed load transports in u and v directions at u point
                   !
                   sbedu    = suu(nm, l)
                   sbedv    = (svv(nm, l) + svv(nmu, l) + svv(ndm, l)           &
                            & + svv(ndmu, l))/4.0_fp
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
                   dzdv = dzdvv(nm)
                   dzdu =0.0_fp
                   ndu = 0
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
                   dzdu = dzdu/max(1,ndu)
                   !
                   ! set bed load transports in u and v directions at v point
                   !
                   sbedv    = svv(nm, l)
                   sbedu    = (suu(nm, l) + suu(num, l) + suu(nmd, l)           &
                            & + suu(numd, l))/4.0_fp
                   sbedcorr = sbedv
                   !
                   nm2   = num
                   depth = hv(nm)
                endif
                !
                ! changed to reduce bed-load component perpendicular to dry points
                !
                ! calculate magnitude of bed-load transport
                !
                sbedm    = sqrt(sbedu**2 + sbedv**2)
                !
                if (sbedm>eps .and. slopecor) then
                   dzds =  dzdu*sbedu/sbedm + dzdv*sbedv/sbedm
                   dzdn = -dzdu*sbedv/sbedm + dzdv*sbedu/sbedm
                   !
                   ! limit dzds to 90% of phi
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
                   case(3,4)
                      !
                      ! 3: Formulation according Van Bendegom (1947), Koch & Flokstra (1980)
                      ! as described in Struiksma et al. (1985)
                      !
                      ! 4: Formulation according Parker & Andrews (1985)
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
                         else ! islope==4
                            hidexploc = (hidexp(nm, l)+hidexp(nm2, l)) / 2.0_fp
                            ftheta    = alfpa * sqrt( shield / &
                                      & max(shield*0.1_fp , hidexploc*thcrpa) )
                         endif
                      else
                         ftheta  = 0.0_fp
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
                if (avalan) then
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
             enddo
             !
             ! continue to next direction
             !
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
