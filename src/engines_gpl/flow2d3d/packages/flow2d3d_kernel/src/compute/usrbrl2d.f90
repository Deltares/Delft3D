subroutine usrbrl2d(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                  & kspu      ,guu       ,gvu       ,qxk       ,bbk       , &
                  & ddk       ,ubrlsu    ,dps       ,hkru      ,s0        , &
                  & hu        ,umean     ,thick     ,dteu      ,taubpu    , &
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
! The routine adds additional energy losses due to 2D  hydraulic structures.
! The energy loss is modelled as linear or quadratic friction term which is 
! integrated implicitly. This implies adding this term in the main diagonal matrix
! element BBK of the momentum equation.
! The following hydr. structures are implemented:
!    2D weir     - KSPU(NM,0)=9 (WAQUA formulation)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: eps
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: thetaw
!
! Global variables
!
    integer                                             , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                !!  then computation proceeds in the X-
                                                                                !!  dir. If icx=1 then computation pro-
                                                                                !!  ceeds in the Y-dir.
    integer                                             , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dteu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk    !!  Internal work array, coefficient la-
                                                                                !!  yer velocity in (N,M,K) implicit part
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space
                                                                                !!  at (N,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubpu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                         , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer      :: itel   ! Loop counter
    integer      :: k      ! Loop counter over KMAX 
    integer      :: ksp    ! Value for structure point KFU(NM)*ABS (KSPU(NM,0))*KSPU(NM,K) 
    integer      :: ndm    ! NM-ICY 
    integer      :: ndmu   ! NMU-ICY 
    integer      :: nm     ! Loop counter over NMMAX 
    integer      :: nmu    ! NM+ICX 
    real(fp)     :: avolk  ! help constant in computation of qvolk
    real(fp)     :: absvbov! absolute value of vbov
    real(fp)     :: d1     ! Distance between crest and downstream depth 
    real(fp)     :: dd     ! Downstream depth 
    real(fp)     :: dte0   ! Local backup of DTEU value 
    real(fp)     :: dtefri ! Energy loss due to friction
    real(fp)     :: ewben  ! Excess waterdepth downstream
    real(fp)     :: ewbov  ! Excess waterdepth upstream
    real(fp)     :: eweir  ! Energy level at weir
    real(fp)     :: hkruin ! Crest height (downward positive). 
    real(fp)     :: hov    ! Total water depth at crest weir
    real(fp)     :: hvolk  ! Water depth associated with critical flow
    real(fp)     :: qov
    real(fp)     :: qunit  ! Discharge at weir crest
    real(fp)     :: qvolk  ! Maximal discharge (super critical flow ) 
    real(fp)     :: vbov
    real(fp)     :: vov
    real(fp)     :: wsben  ! Water level d/s 
    real(fp)     :: wsbov  ! Water level u/s 
    character(4) :: toest  ! Status weir: volk = perfect weir onvo = imperfect weir 
!
!! executable statements -------------------------------------------------------
!
    eps       => gdp%gdconst%eps
    ag        => gdp%gdphysco%ag
    thetaw    => gdp%gdrivpro%thetaw
    !
    avolk = 2.0*sqrt(2.0*ag/3.0)/3.0
    !
    ndm  = -icy
    nmu  = icx
    ndmu = -icy + icx
    do nm = 1, nmmax
       ndm  = ndm + 1
       nmu  = nmu + 1
       ndmu = ndmu + 1
       !
       ! 2D weir, quadratic friction
       !
       ksp    = kfu(nm)*abs(kspu(nm, 0))
       hkruin = hkru(nm)
       if ((ksp==9) .and. (real(dps(nm) ,fp)>hkruin) &
           &        .and. (real(dps(nmu),fp)>hkruin) ) then
          !
          ! Initial values
          !
          qunit = umean(nm)*hu(nm)
          vbov  = umean(nm)
          !
          ! Define flow direction and downstream resp. upstream points
          !
          if (vbov>=0.001) then
             dd    = real(dps(nmu),fp)
             wsbov = s0(nm)
             wsben = s0(nmu)
          elseif (vbov<= - 0.001) then
             dd    = real(dps(nm),fp)
             wsbov = s0(nmu)
             wsben = s0(nm)
          elseif (s0(nmu)>s0(nm)) then
             dd    = real(dps(nm),fp)
             wsbov = s0(nmu)
             wsben = s0(nm)
          else
             dd    = real(dps(nmu),fp)
             wsbov = s0(nm)
             wsben = s0(nmu)
          endif
          !
          ! Initial values
          !
          qunit = abs(qunit)
          eweir = (max(1e-6_fp, wsbov + hkruin)) + vbov**2/(2.*ag)
          ewbov = wsbov + hkruin
          ewben = wsben + hkruin
          !
          qvolk = avolk*eweir**1.5
          !
          ! calculate the maximum velocity above the weir crest
          ! reduced wet surface
          !
          hov  = max(1e-6_fp, wsbov + hkruin)
          vov  = qunit/(max(1e-6_fp, wsbov + hkruin))
          itel = 0
          if (vov<0.5) then
             !
             ! this is only interesting for velocity less than 0.5 m/s
             ! vov will only be used in cases vov is less than 0.5 m/s
             !
             hvolk = (2./3.)*eweir
             !
  100        continue
             itel = itel + 1
             vov  = qunit/hov
             hov  = max(hvolk, eweir - vov**2/(2.*ag))
             qov  = vov*hov
             !
             if ((abs(qunit - qov))>0.001*max(0.0001_fp, qunit) .and. (itel<101))   &
               & goto 100
          endif
          !
          dte0   = dteu(nm)
          d1     = dd - hkruin
          dtefri = taubpu(nm)*abs(umean(nm))*gvu(nm)/(ag*hu(nm))
          !
          ! Determine subgrid energy loss (DTEU)
          !
          call enloss(ag        ,d1        ,eweir     ,hkruin    ,hov       , &
                    & qunit     ,qvolk     ,toest     ,vov       , &
                    & ewbov     ,ewben     ,wsben     ,dteu(nm)  ,dtefri    )
          !
          ! Use overrelaxation (following experience Stelling '98)
          ! Notice the unusual thetaw definition (thetaw=0: dteu, thetaw=1: dte0)
          !
          dteu(nm) = (1.0_fp - thetaw)*dteu(nm) + thetaw*dte0
          if (toest=='volk') vbov = qvolk/max(hu(nm), 1E-6_fp)
          absvbov = abs(vbov)
          if (absvbov>eps) then
             do k = 1, kmax
                bbk(nm, k) = bbk(nm, k) + (ag*dteu(nm)/absvbov)*ubrlsu(nm, k)      &
                           & /(gvu(nm)*thick(k))
             enddo
          endif
       endif
    enddo
end subroutine usrbrl2d
