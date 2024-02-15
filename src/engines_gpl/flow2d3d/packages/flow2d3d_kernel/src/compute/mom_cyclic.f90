subroutine mom_cyclic &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
               & bdy       ,bux       ,buy       ,buux      ,buuy      ,mom_output, &
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
! This subroutine is part of (called by) UZD. It computes the Horizontal
! Advection in U- and V-direction following the default cyclic scheme.
! In both U- and V-direction an implicit 2-nd order upwind.
! (Ref.: Stelling & Leendertse
!        "Approximation of Convective Processes by Cyclic
!         AOI methods", Proc. 2nd ASCE Conf. on Estuarine
!         and Coastal Modelling, Tampa, 1991)
!
! Along open boundaries the advection terms normal to the open boundaries can
! be switched off (option: CSTBND = TRUE)
!
! In case of hydraulic structure a special energy conserving discretisation is
! implemented.
! It computes the contribution of the advection terms to the matrix elements
! and the right hand side of the system of discretised momentum equations.
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
    integer                                                           :: icx
    integer                                                           :: icy
    integer                                                           :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: nmmax  !  Description and declaration in dimens.igs
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer,    dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddx
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buux
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buuy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
                                                                                !  Only used in case mom_output = .true.
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    logical                                              , intent(in) :: mom_output
!
! Local variables
!
    integer :: iad1
    integer :: iad2
    integer :: k
    integer :: kenm
    integer :: kspu0k
    integer :: nddm
    integer :: nddmu
    integer :: ndm
    integer :: ndmd
    integer :: ndmu
    integer :: neigat  ! =0 for neighbour point is gate
    integer :: nm
    integer :: nmd
    integer :: nmdd
    integer :: nmu
    integer :: nmuu
    integer :: num
    integer :: numu
    integer :: nuum
    real(fp):: adfac
    real(fp):: gsqi
    real(fp):: termc
    real(fp):: termd
    real(fp):: termdd
    real(fp):: termex
    real(fp):: termu
    real(fp):: termuu
    real(fp):: uu
    real(fp):: vvv
    real(fp):: vvhr
    real(fp):: uvdgdy
    real(fp):: vvdgdx
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
    !  INITIALIZE
    !
    do k = 1, kmax
       nmd   = -icx
       nmdd  = -icx - icx
       ndm   = -icy
       nddm  = -icy - icy
       nddmu = -icy - icy + icx
       ndmd  = -icy - icx
       nmu   = icx
       num   = icy
       nuum  = icy + icy
       numu  = icx + icy
       nmuu  = icx + icx
       ndmu  = -icy + icx
       do nm = 1, nmmax
          nmd   = nmd + 1
          nmdd  = nmdd + 1
          ndm   = ndm + 1
          nddm  = nddm + 1
          nddmu = nddmu + 1
          ndmd  = ndmd + 1
          nmu   = nmu + 1
          num   = num + 1
          nuum  = nuum + 1
          numu  = numu + 1
          nmuu  = nmuu + 1
          ndmu  = ndmu + 1
          kspu0k= kspu(nm, 0)*kspu(nm, k)
          !
          ! For an active point and not a gate or plate
          !
          if ( ((kcu(nm)==1) .and. (kfu(nm)==1)) .and. kspu0k /=4 .and. kspu0k /=10) then
             gsqi   = gsqiu(nm)
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3               )  ) then
                kenm = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                vvv = (v(ndm, k)*kfv(ndm) + v(ndmu, k)*kfv(ndmu) + v(nm, k)     &
                    & *kfv(nm) + v(nmu, k)*kfv(nmu))/kenm
             else
                vvv = .25*(v(nm, k) + v(nmu, k) + v(ndm, k) + v(ndmu, k))
             endif
             !
             ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
             !
             uvdgdy = vvv*gsqi*0.5*((gvv(nm) + gvv(nmu) - gvv(ndm) - gvv(ndmu)))
             !
             ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
             !
             vvdgdx = 0.5*vvv*gsqi*(guu(nmu) - guu(nmd))
             !
             adfac  = 0.50/gvu(nm)
             vvhr   = 0.5*vvv/guu(nm)
             !
             ! CONTRIBUTION OF CONVECTION IN X DIRECTION
             !
             ! begin standard delft3d-flow (compare to mom_waqua)
             !
             if (u0(nm, k)>0.0) then
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
                   uu = adfac*(u0(nm, k) + u0(nmd, k))
                   termc = (uu + uvdgdy)*kfu(nmd)*neigat
                   termd = -uu*kfu(nmd)*neigat
                   if (mom_output) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termd*u1(nmd, k)
                   else
                      bbk(nm, k) = bbk(nm, k) + termc
                      bdx(nm, k) = termd
                   endif
                else
                   !
                   ! Upwind approach near structure points and inactive u-points
                   !
                   iad1 = (kfu(nmd) + kfu(nmd))*kadu(nmd, k)
                   iad2 = kfu(nmd)*kfu(nmdd)*kfu(nmu)*kadu(nmd, k)*kadu(nmdd, k)&
                        & *kadu(nmu, k)
                   !
                   ! CONSERVATIVE FORM ( LESS STABLE! )
                   !             BBK (NM,K)=BBK(NM,K)
                   !    *                  +U0(NM  ,K)*ADFAC*( IAD1+IAD2)+UVDGDY*KFU(NMD)
                   !             BDX (NM,K)=U0(NMD ,K)*ADFAC*(-IAD1-IAD2-IAD2)
                   !             BDDX(NM,K)=U0(NMDD,K)*ADFAC*(           IAD2)
                   !
                   ! NON CONSERVATIVE FORM
                   !
                   termc  = u0(nm, k)*adfac*(iad1 + iad2) + uvdgdy*kfu(nmd)*neigat
                   termd  = u0(nm, k)*adfac*( - iad1 - iad2 - iad2)
                   termdd = u0(nm, k)*adfac*(iad2)
                   if (mom_output) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termd*u1(nmd, k) - termdd*u1(nmdd, k)
                   else
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bdx(nm, k)  = termd
                      bddx(nm, k) = termdd
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
                   uu = adfac*(u0(nm, k) + u0(nmu, k))
                   termc = (uvdgdy - uu)*kfu(nmu)*neigat
                   termu = uu*kfu(nmu)*neigat
                   if (mom_output) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termu*u1(nmu, k)
                   else
                      bbk(nm, k) = bbk(nm, k) + termc
                      bux(nm, k) = termu
                   endif
                else
                   !
                   ! Upwind approach near structure points and inactive u-points
                   !
                   iad1 = (kfu(nmu) + kfu(nmu))*kadu(nmu, k)
                   iad2 = kfu(nmu)*kfu(nmuu)*kfu(nmd)*kadu(nmu, k)*kadu(nmuu, k)&
                        & *kadu(nmd, k)
                   !
                   ! CONSERVATIVE FORM ( LESS STABLE! )
                   !
                   !             BBK (NM,K)=BBK(NM,K)
                   !    *                  +U0(NM  ,K)*ADFAC*(-IAD1-IAD2)+UVDGDY*KFU(NMU)
                   !             BUX (NM,K)=U0(NMU ,K)*ADFAC*( IAD1+IAD2+IAD2)
                   !             BUUX(NM,K)=U0(NMUU,K)*ADFAC*(          -IAD2)
                   !
                   !
                   ! NON CONSERVATIVE FORM
                   !
                   termc  = u0(nm, k)*adfac*( - iad1 - iad2) + uvdgdy*kfu(nmu)*neigat
                   termu  = u0(nm, k)*adfac*(iad1 + iad2 + iad2)
                   termuu = u0(nm, k)*adfac*( - iad2)
                   if (mom_output) then
                      mom_m_convec(nm, k) = mom_m_convec(nm, k) &
                                          & - termc*u1(nm, k) - termu*u1(nmu, k) - termuu*u1(nmuu, k)
                   else
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bux(nm, k)  = termu
                      buux(nm, k) = termuu
                   endif
                endif
             endif
             !
             ! end standard delft3d-flow (compare to mom_waqua)
             !
             !
             ! CONTRIBUTION OF ADVECTION IN Y DIRECTION
             !           IAD1      =KFV(NDM) *KFV(NDMU)*KFU(NDM) for VVHR > 0
             !           IAD1      =KFV(NM) *KFV(NMU)*KFU(NUM) for VVHR < 0
             !
             if (kadu(num, k)*kadu(ndm, k)*kadu(nm, k)==1) then
                if (vvhr>0.0) then
                   if (cstbnd) then
                      if (kcs(nm)==2) then
                         iad1 = kfu(ndm)*kfv(ndmu)
                         iad2 = iad1*kfv(nddmu)*kfu(nddm)
                         vvdgdx = 0.0
                      elseif (kcs(nmu)==2) then
                         iad1 = kfu(ndm)*kfv(ndm)
                         iad2 = iad1*kfv(nddm)*kfu(nddm)
                         vvdgdx = 0.0
                      else
                         iad1 = kfu(ndm)*kfv(ndm)*kfv(ndmu)
                         iad2 = iad1*kfv(nddm)*kfv(nddmu)*kfu(nddm)
                      endif
                   else
                      iad1 = kfv(ndm)*kfv(ndmu)
                      iad2 = iad1*kfv(nddm)*kfv(nddmu)*kfu(nddm)
                   endif
                   termc  = vvhr*(iad1 + iad1 + iad2)
                   termd  = vvhr*( - iad1 - iad1 - iad2 - iad2)
                   termdd = vvhr*(iad2)
                   termex = vvv*vvdgdx*iad1
                   if (mom_output) then
                      mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) &
                                          & - termc*u1(nm, k) - termd*u1(ndm, k) - termdd*u1(nddm, k) + termex
                   else
                      bbk(nm, k)  = bbk(nm, k) + termc
                      bdy(nm, k)  = termd
                      bddy(nm, k) = termdd
                      ddk(nm, k)  = ddk(nm, k) + termex
                   endif
                else
                   if (cstbnd) then
                      if (kcs(nm)==2) then
                         iad1 = kfu(num)*kfv(nmu)
                         iad2 = iad1*kfv(numu)*kfu(nuum)
                         vvdgdx = 0.0
                      elseif (kcs(nmu)==2) then
                         iad1 = kfu(num)*kfv(nm)
                         iad2 = iad1*kfv(num)*kfu(nuum)
                         vvdgdx = 0.0
                      else
                         iad1 = kfu(num)*kfv(nm)*kfv(nmu)
                         iad2 = iad1*kfv(num)*kfv(numu)*kfu(nuum)
                      endif
                   else
                      iad1 = kfv(nm)*kfv(nmu)
                      iad2 = iad1*kfv(num)*kfv(numu)*kfu(nuum)
                   endif
                   termc  = vvhr*( - iad1 - iad1 - iad2)
                   termu  = vvhr*(iad1 + iad1 + iad2 + iad2)
                   termuu = vvhr*( - iad2)
                   termex = vvv*vvdgdx*iad1
                   if (mom_output) then
                      mom_m_xadvec(nm, k) = mom_m_xadvec(nm, k) &
                                          & - termc*u1(nm, k) - termu*u1(num, k) - termuu*u1(nuum, k) + termex
                   else
                      bbk(nm, k)  = bbk(nm, k) + termc
                      buy(nm, k)  = termu
                      buuy(nm, k) = termuu
                      ddk(nm, k)  = ddk(nm, k) + termex
                   endif
                endif
             endif
          endif
       enddo
    enddo
end subroutine mom_cyclic
