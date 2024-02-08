subroutine mom_cw &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,dumm1     ,dumm2     ,dumm3     , &
               & dumm4     ,dumm5     ,dumm6     ,dumm7     ,dumm8     ,mom_output, &
               & u1        ,gdp)
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
!
! Global variables
!
    integer                                                       :: icx
    integer                                                       :: icy
    integer                                                       :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax  !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)     , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
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
    integer :: kad
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
    real(fp):: advcxe
    real(fp):: advecx
    real(fp):: advecy
    real(fp):: geta
    real(fp):: gksi
    real(fp):: gsqi
    real(fp):: gvndm
    real(fp):: gvnm
    real(fp):: svvv
    real(fp):: uad
    real(fp):: uuu
    real(fp):: uvdgdy
    real(fp):: vvdgdx
    real(fp):: vvv
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    struct     => gdp%gdprocs%struct
    cstbnd     => gdp%gdnumeco%cstbnd
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
    do k = 1, kmax
       nmd = -icx
       ndm = -icy
       ndmd = -icx - icy
       nmu = icx
       num = icy
       numu = icx + icy
       ndmu = icx - icy
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1
          ndmd = ndmd + 1
          nmu = nmu + 1
          num = num + 1
          numu = numu + 1
          ndmu = ndmu + 1
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
             else
                uuu = u0(nm, k)
             endif
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3               )  ) then
                svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                vvv = (v1(ndm, k)*kfv(ndm) + v1(ndmu, k)*kfv(ndmu) + v1(nm, k)  &
                    & *kfv(nm) + v1(nmu, k)*kfv(nmu))/svvv
             else
                vvv = .25*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
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
             advecx = 0.0
             advcxe = 0.0
             advecy = 0.0
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
                   advecx = uad*2.*uvdgdy*kfu(nmd)*neigat
                   advcxe = ((u0(nm, k) + u0(nmd, k))                        &
                          & *(u0(nm, k) - u0(nmd, k)))*kfu(nmd)              &
                          & *neigat/(2*gksi)
                else
                   !
                   ! upwind approach near structure points and inactive
                   ! u-points for STANDARD (Cyclic) Delft3D FLOW
                   ! approach; No special discretisation at structure points
                   ! for WAQUA approach
                   !
                   iad1 = kfu(nmd)*kadu(nmd, k)
                   iad2 = iad1*kfu(nmu)*kadu(nmu, k)
                   !
                   advecx = uad*                                             &
                          & (((0.5*iad2)*u0(nmu, k) + (iad1 - iad2)*u0(nm, k)&
                          & - (iad1 - 0.5*iad2)*u0(nmd, k)) /gksi +          &
                          &  2.*uvdgdy)*kfu(nmd)*neigat
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
                   advecx = uad*2.*uvdgdy*kfu(nmu)*neigat
                   advcxe = ((u0(nmu, k) + u0(nm, k))                        &
                          & *(u0(nmu, k) - u0(nm, k)))*kfu(nmu)              &
                          & *neigat/(2*gksi)
                else
                   !
                   ! upwind approach near structure points and inactive
                   ! u-points for STANDARD (Cyclic) Delft3D FLOW
                   ! approach; No special discretisation at structure points
                   ! for WAQUA approach
                   !
                   iad1 = kfu(nmu)*kadu(nmu, k)
                   iad2 = iad1*kfu(nmd)*kadu(nmd, k)
                   advecx = uad*                                             &
                          & (((iad1 - 0.5*iad2)*u0(nmu, k) + (iad2 - iad1)   &
                          & *u0(nm, k) - (0.5*iad2)*u0(nmd, k)) /gksi +      &
                          &  2.*uvdgdy)*kfu(nmu)*neigat
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
                      kad = kfu(num)*kfu(ndm)*kfv(nmu)*kfv(ndmu)
                      vvdgdx = 0.0
                   elseif (kcs(nmu)==2) then
                      kad = kfu(num)*kfu(ndm)*kfv(nm)*kfv(ndm)
                      vvdgdx = 0.0
                   else
                      kad = kfv(nm)*kfv(nmu)*kfu(num)*kfv(ndm)*kfv(ndmu)     &
                          & *kfu(ndm)
                   endif
                elseif (vvv>0.) then
                   kad = kfv(nm)*kfv(nmu)*kfu(num)*kfv(ndm)*kfv(ndmu)
                else
                   kad = kfv(nm)*kfv(nmu)*kfv(ndm)*kfv(ndmu)*kfu(ndm)
                endif
                advecy = kad*(0.5*vvv*(u0(num, k) - u0(ndm, k))              &
                       & /geta - vvv*vvdgdx)
             endif
             if (mom_output) then
                mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                    & - advecx*u1(nm, k) - advcxe
                mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) - advecy
             else
                bbk(nm, k)  = bbk(nm, k) + advecx
                ddk(nm, k)  = ddk(nm, k) - advecy - advcxe
             endif
          endif
       enddo
    enddo
end subroutine mom_cw
