subroutine mom_fls &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,dumm1     ,dumm2     ,dumm3     , &
               & dumm4     ,dumm5     ,dumm6     ,ua        ,ub        ,thick     , &
               & mom_output,gdp) 
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
! This subroutine is part of (called by) CUCNP and UZD. It computes the
! Horizontal Advection in U- and V-direction.
! It is an explicit scheme. Energy conserving for converging flow and momentum
! conserving for expanding flow (Flooding Scheme-FLS)
!
! It computes the terms of the matrix elements BBK and DDK of the equation.
! 
! DUMMY ARRAYS: 1 to 6 are introduced to ensure all subroutines that solve the
!               momentum equation have the same parameter headers
!
!!--pseudo code and references--------------------------------------------------
!
! Stelling&Duijnmeijer "A Staggered conservative scheme for every Froude
!                       number in rapidly varied shallow water flows", 
!                      Num. Method in Fluids, No. 34, 2003
!
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
    real(fp)     , pointer :: dgcuni
    real(fp)     , pointer :: dryflc
    logical      , pointer :: cstbnd
    real(fp), dimension(:,:)          , pointer :: mom_m_convec        ! convection u*du/dx term
    real(fp), dimension(:,:)          , pointer :: mom_m_xadvec        ! cross-advection v*du/dy term
!
! Global variables
!
    integer                                                          :: icx
    integer                                                          :: icy
    integer                                                          :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ua
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ub
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ddk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm2
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm3
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm4
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm5
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dumm6
    real(fp)  , dimension(kmax)                                       :: thick  !  Description and declaration in esm_alloc_real.f90
    logical                                              , intent(in) :: mom_output
!
! Local variables
!
    integer            :: k
    integer            :: kspu0k
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmdd
    integer            :: nmu
    integer            :: num
    integer            :: nuum
    integer            :: numu
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: dpsu
    real(fp)           :: du1
    real(fp)           :: du2
    real(fp)           :: factor
    real(fp)           :: hl
    real(fp)           :: hr
    real(fp)           :: gsqi
    real(fp)           :: gvndm
    real(fp)           :: gvnm
    real(fp)           :: qxup
    real(fp)           :: qxdo
    real(fp)           :: qyup
    real(fp)           :: qydo
    real(fp)           :: svvv
    real(fp)           :: termex
    real(fp)           :: trsh
    real(fp), external :: ulim
    real(fp)           :: uvdgdy
    real(fp)           :: vvdgdx
    real(fp)           :: vvv
!
!! executable statements -------------------------------------------------------
!
    dgcuni  => gdp%gdnumeco%dgcuni
    dryflc  => gdp%gdnumeco%dryflc
    cstbnd  => gdp%gdnumeco%cstbnd
    !
    if (mom_output) then
       if (icx==1) then ! solve V/N component
          mom_m_convec => gdp%gdflwpar%mom_n_convec
          mom_m_xadvec => gdp%gdflwpar%mom_n_xadvec
       else ! solve U/M component
          mom_m_convec => gdp%gdflwpar%mom_m_convec
          mom_m_xadvec => gdp%gdflwpar%mom_m_xadvec
       endif
    endif
    !
    ! INITIALISATION
    !
    trsh   = dryflc
    !

    do k = 1, kmax
       nmd  = -icx
       nmdd = nmd - icx
       ndm  = -icy
       nmu  = icx
       num  = icy
       nuum = num + icy
       do nm = 1, nmmax
          ua(nm,k) = 0.0_fp
          ub(nm,k) = 0.0_fp
          nmd      = nmd  + 1
          nmdd     = nmdd + 1
          ndm      = ndm  + 1
          nmu      = nmu  + 1
          num      = num  + 1
          nuum     = nuum + 1
          !
          ! Compute UA (appr. of velocity in waterlevel points) at internal points
          ! At open boundary UA == U0 for inflow
          !
          du2 = (u0(nm,k)-u0(nmd,k)) * kfu(nm) * kfu(nmd)
          if (kspu(nm,0) > 0 .or. kspu(nmd,0) > 0) then
             du2 = 0.0_fp
          endif
          if (qxk(nm,k)+qxk(nmd,k) > 0.0_fp) then
            du1      = (u0(nmd,k)-u0(nmdd,k)) * kfu(nmd) * kfu(nmdd) * kadu(nmd,k) * kadu(nmdd,k)
            if (kspu(nmd,0)>0 .or. kspu(nmdd,0)>0 ) then
               du1 = 0.0_fp
            endif
            ua(nm,k) =  u0(nmd,k) + ulim(du1,du2)*du1
          else
            du1      = (u0(nmu,k)-u0(nm,k)) * kfu(nmu) * kfu(nm) * kadu(nmu,k) * kadu(nm,k)
            if (kspu(nm,0)>0 .or. kspu(nmu,0)>0 ) then
               du1 = 0.0_fp
            endif
            ua(nm,k) =  u0(nm,k)  - ulim(du1,du2)*du1
          endif

          !
          ! Compute UB (appr. of velocity in depth points) at internal points
          ! At open boundary UB == U0 for inflow
          ! In case thin dams in the transverse direction let UB = 0
          !
          if (kfv(nm)*kfv(nmu) /= 0) then
            if (qyk(nm,k)+qyk(nmu,k) > 0.0_fp) then
              du1      = (u0(nm ,k) - u0(ndm,k)) * kfu(nm)  * kfu(ndm) * kadu(nm,k) * kadu(ndm,k)
              du2      = (u0(num,k) - u0(nm ,k)) * kfu(num) * kfu(nm)  * kadu(nm,k) * kadu(num,k)
              ub(nm,k) =  u0(nm ,k) + ulim(du1,du2)*du1
            else
              du1      = (u0(nuum,k) - u0(num,k)) * kfu(nuum) * kfu(num) * kadu(nuum,k) * kadu(num,k)
              du2      = (u0(num ,k) - u0(nm ,k)) * kfu(num)  * kfu(nm)  * kadu(num,k)  * kadu(nm,k)
              ub(nm,k) =  u0(num ,k) - ulim(du1,du2)*du1
            endif
          endif
       enddo
    enddo
    do k = 1, kmax
       nmd  = -icx
       ndm  = -icy
       ndmd = -icx - icy
       nmu  =  icx
       num  =  icy
       numu =  icx + icy
       ndmu =  icx - icy
       do nm = 1, nmmax
          nmd    = nmd  + 1
          ndm    = ndm  + 1
          ndmd   = ndmd + 1
          nmu    = nmu  + 1
          num    = num  + 1
          numu   = numu + 1
          ndmu   = ndmu + 1
          kspu0k = kspu(nm,0) * kspu(nm,k)
          !
          ! Check for domain decomposition points = kcs(nm)*kcs(nmu) > 0
          !
          ! Note: abs(kcs) is needed here in parallel computations
          !
          if (kfu(nm)==1 .and. abs(kcs(nm)*kcs(nmu)) > 0 .and. kspu0k /=4 .and. kspu0k /=10) then
             advecx = 0.0_fp
             advecy = 0.0_fp
             vvdgdx = 0.0_fp
             uvdgdy = 0.0_fp
             hl     = real(dps(nm) ,fp) + s0(nm)
             hr     = real(dps(nmu),fp) + s0(nmu)
             factor = 1.0_fp
             gvnm   = gvd  (nm)
             gvndm  = gvd  (ndm)
             gsqi   = gsqiu(nm)
             !
             ! Compute VVV
             !
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3               )  ) then
                svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                vvv  = (  v1(ndm,k)*kfv(ndm) + v1(ndmu,k)*kfv(ndmu)  &
                     &  + v1(nm ,k)*kfv(nm ) + v1(nmu ,k)*kfv(nmu )   ) / svvv
             else
                vvv = 0.25_fp * (v1(ndm,k)+v1(ndmu,k)+v1(nm,k)+v1(nmu,k))
             endif
             !
             ! ADVECTION IN U-DIRECTION; DU/DX AND CENTRIFUGAL ACCELERATION
             ! MOMENT - CONSERVATIVE LIMITER scheme
             ! No advection in the Y-direction along open boundary
             !
             qxup   = (qxk(nm,k)  + qxk(nmu,k) )/ max(1, (kfu(nm) *kadu(nm,k)  + kfu(nmu) *kadu(nmu,k) ))
             qxdo   = (qxk(nmd,k) + qxk(nm,k)  )/ max(1, (kfu(nmd)*kadu(nmd,k) + kfu(nm)  *kadu(nm,k)  ))
             qyup   = (qyk(nm,k)  + qyk(nmu,k) )/ max(1, (kfv(nm) *kadv(nm,k)  + kfv(nmu) *kadv(nmu,k) ))
             qydo   = (qyk(ndm,k) + qyk(ndmu,k))/ max(1, (kfv(ndm)*kadv(ndm,k) + kfv(ndmu)*kadv(ndmu,k)))
             dpsu   = max(0.5_fp*(real(dps(nm),fp)+s0(nm) + real(dps(nmu),fp)+s0(nmu)),trsh)
             if (comparereal(ua(nm ,k),0.0_fp) /= 0 .and. &
               & comparereal(ua(nmu,k),0.0_fp) /= 0        ) then
                advecx = (qxup*ua(nmu,k) - qxdo*ua(nm,k)  - u0(nm,k)*(qxup - qxdo)) * gsqi / (dpsu * thick(k))
                !
                ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
                !
                uvdgdy = vvv * gsqi * (gvnm-gvndm)
             endif
             if (comparereal(ub(ndm,k),0.0_fp) /= 0 .and. &
               & comparereal(ub(nm ,k),0.0_fp) /= 0        ) then
                advecy = (qyup*ub(nm,k)  - qydo*ub(ndm,k) - u0(nm,k)*(qyup - qydo)) * gsqi / (dpsu * thick(k)) 
                !
                ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
                !
                vvdgdx = 0.5 * vvv * gsqi * (guu(nmu)-guu(nmd))
             endif
             !
             ! Switch from momentum conservation to energy conservation
             !
             if (       (umean(nm) > 0.0_fp .and. (hl > hr) .and. kfu(nmu) == 1) &
                 & .or. (umean(nm) < 0.0_fp .and. (hr > hl) .and. kfu(nmd) == 1)  ) then
                 if (       (real(dps(nm),fp)          > real(dps(nmu),fp)+ dgcuni) & 
                     & .or. (real(dps(nm),fp) + dgcuni < real(dps(nmu),fp)        )  ) then
                    factor = ( hr * hl / (dpsu*dpsu) )
                    !
                    ! avoid factor becoming small (synchronised with SOBEK FLS)
                    !
                    factor = max (0.33_fp , factor)
                 else
                    !
                    ! avoid dividing by zero
                    !
                    factor = 1.0_fp
                 endif
             endif
             advecy = advecy - vvv*vvdgdx
             advecx = advecx + u0(nm,k)*uvdgdy
             if (mom_output) then
                mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) - advecy/factor
                mom_m_convec(nm, k) = mom_m_convec(nm, k) - advecx/factor
             else
                ddk(nm, k)  = ddk(nm, k) - advecx/factor - advecy/factor
             endif
          endif
       enddo
    enddo
end subroutine mom_fls 
