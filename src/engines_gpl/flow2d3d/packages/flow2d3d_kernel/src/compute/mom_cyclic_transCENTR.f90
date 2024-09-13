subroutine mom_cyclic_transCENTR &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
               & bdy       ,bux       ,buy       ,buux      ,buuy      ,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: mom_cyclic_transCENTR.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/mom_cyclic_transCENTR.f90 $
!!--description-----------------------------------------------------------------
!
! This subroutine is part of (called by) UZD. It computes the Horizontal
! Advection.
! In U- and V-direction an implicit 2-nd order upwind.
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
!!
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
    logical, pointer :: EXCLouterVEL
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
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v
    real(fp),   dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: iad3,kad2,kad
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
    real(fp):: uu
    real(fp):: vvv
    real(fp):: vvhr
    real(fp):: uvdgdy
    real(fp):: vvdgdx
!
!! executable statements -------------------------------------------------------
!
    EXCLouterVEL => gdp%gdimbound%EXCLouterVEL
    wind       => gdp%gdprocs%wind
    struct     => gdp%gdprocs%struct
    cstbnd     => gdp%gdnumeco%cstbnd
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
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3               ) .or. EXCLouterVEL) then
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
             if (u0(nm, k)>0.0) then
                !
                ! begin standard delft3d-flow (compare to uzd_wq)
                !
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
                   bbk(nm, k) = bbk(nm, k) + (uu + uvdgdy)*kfu(nmd)*neigat
                   bdx(nm, k) = -uu*kfu(nmd)*neigat
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
                   bbk(nm, k) = bbk(nm, k) + u0(nm, k)*adfac*(iad1 + iad2)      &
                              & + uvdgdy*neigat
                   bdx(nm, k) = u0(nm, k)*adfac*( - iad1 - iad2 - iad2)
                   bddx(nm, k) = u0(nm, k)*adfac*(iad2)
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
                   bbk(nm, k) = bbk(nm, k) + (uvdgdy - uu)*kfu(nmu)*neigat
                   bux(nm, k) = uu*kfu(nmu)*neigat
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
                   bbk(nm, k) = bbk(nm, k) + u0(nm, k)*adfac*( - iad1 - iad2)   &
                              & + uvdgdy*neigat
                   bux(nm, k) = u0(nm, k)*adfac*(iad1 + iad2 + iad2)
                   buux(nm, k) = u0(nm, k)*adfac*( - iad2)
                endif
             !
             ! end standard delft3d-flow (compare to uzd_wq)
             !
             endif
             !
             ! CONTRIBUTION OF ADVECTION IN Y DIRECTION
             !           IAD1      =KFV(NDM) *KFV(NDMU)*KFU(NDM) for VVHR > 0
             !           IAD1      =KFV(NM) *KFV(NMU)*KFU(NUM) for VVHR < 0
             !
             if (kadu(num, k)*kadu(ndm, k)*kadu(nm, k)==1) then
                if (cstbnd) then
                   !
                   ! leave vdu/dy intact at left and right hand boundary;
                   ! see also new treatment of vvv
                   !
                   if (kcs(nm)==2) then
                      kad2 = kfv(nmu)*kfv(ndmu)
                      kad = kad2*kfu(num)*kfu(ndm)
                    !  vvdgdx = 0.0
                   elseif (kcs(nmu)==2) then
                      kad2 = kfv(nm)*kfv(ndm)
                      kad = kad2*kfu(num)*kfu(ndm)
                    !  vvdgdx = 0.0
                   else
                     ! kad = kfv(nm)*kfv(nmu)*kfu(num)*kfv(ndm)*kfv(ndmu)     &
                     !     & *kfu(ndm)
                      if (vvv>0.) then
                         kad2 = kfv(nm)*kfv(nmu)*kfv(ndm)*kfv(ndmu)
                         kad  = kad2 * kfu(ndm)*kfu(num)
                      else
                         kad2 = kfv(nm)*kfv(nmu)*kfv(ndm)*kfv(ndmu)
                         kad  = kad2 * kfu(num)*kfu(ndm)
                      endif
                   endif
                elseif (vvv>0.) then
                   kad2 = kfv(nm)*kfv(nmu)*kfv(ndm)*kfv(ndmu)
                   kad = kad2*kfu(num)*kfu(ndm)
                else
                   kad2 = kfv(nm)*kfv(nmu)*kfv(ndm)*kfv(ndmu)
                   kad = kad2*kfu(ndm)*kfu(num)
                endif
              !  bbk(nm, k) = bbk(nm, k)  
                buy(nm, k) =  vvhr*kad
                bdy(nm, k) = -vvhr*kad
                ddk(nm, k) = ddk(nm, k) + vvv*vvdgdx*kad2             
             endif
          endif
       enddo
    enddo
!        
end subroutine mom_cyclic_transCENTR
