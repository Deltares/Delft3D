subroutine sourmu(soumud    ,excbed    ,entr      ,wssus     ,j         , &
                & nmmaxj    ,nmmax     ,nmax      ,mmax      ,kmax      , &
                & icx       ,icy       ,kfs       ,kfu       ,kfv       , &
                & kcs       ,s0        ,dps       ,u0        ,v0        , &
                & usus      ,vsus      ,taubu     ,taubv     ,czusus    , &
                & czvsus    ,rsed      ,sett      ,sepsus    ,gdp       )
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
!    Function: Computation of source term for combined effect of
!              erosion/dewater at interface mud - bed
!              settling/entrainment at interface mud - water.
!              This subroutine is called only once in a timestep.
!              New formulation entrainment rate, following
!              Winterwerp 1997.
!
! Method used: Reference: Transport of Fluid Mud, numerical
!              modelling with a two-layer system. Research
!              documentation. November 1995. Deltares.
!     Comment:
!
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
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhofrac
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: cbed
    real(fp)               , pointer :: cmud
    real(fp)               , pointer :: fmud
    real(fp)               , pointer :: fwat
    real(fp)               , pointer :: mers
    real(fp)               , pointer :: rhosus
    real(fp)               , pointer :: rhomud
    real(fp)               , pointer :: taubng
    real(fp)               , pointer :: tauers
    real(fp)               , pointer :: tauset
    real(fp)               , pointer :: vdew
!
! Local parameters
!
    real(fp), parameter :: vismud = 5.E-6, rencri = 600., tauly = 0.5
!
! Global variables
!
    integer                                           , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-dir.
                                                                              !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                           , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: czusus !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: czvsus !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: entr   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: excbed !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: sepsus !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: sett   !!  Settling suspension --> mud layer
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: soumud !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubu  !!  Bottom friction term in the x-dir. (from flow file in zeta point)
                                                                              !!  Called with WINDU
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubv  !!  Bottom friction term in the y-dir. (from flow file in zeta point)
                                                                              !!  Called with WINDV
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: wssus  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: rsed   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: usus   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: vsus   !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer        :: ndm
    integer        :: nm
    integer        :: nmd
    real(fp)       :: alfa
    real(fp)       :: buoyan
    real(fp)       :: cu
    real(fp)       :: cu2
    real(fp)       :: cv
    real(fp)       :: cv2
    real(fp)       :: dewat
    real(fp)       :: dmud
    real(fp)       :: dtau
    real(fp)       :: eps2
    real(fp)       :: eps2t
    real(fp)       :: erosi
    real(fp)       :: fact
    real(fp)       :: h0sus
    real(fp)       :: re1
    real(fp)       :: re2
    real(fp)       :: renold
    real(fp)       :: tau
    real(fp)       :: taum
    real(fp)       :: taux
    real(fp)       :: tauy
    real(fp)       :: uabs
    real(fp)       :: um
    real(fp)       :: us
    real(fp)       :: ustbe2
    real(fp)       :: ustin2
    real(fp)       :: usttot
    real(fp)       :: usty2
    real(fp)       :: uv2
    real(fp)       :: uvmag2
    real(fp)       :: vm
    real(fp)       :: vs
!
    !
    !
    data eps2/0.000001/
!
!! executable statements -------------------------------------------------------
!
    !
    ! GLOBAL DATA INITIALISATION
    !
    cbed     => gdp%gdmudcoe%cbed
    cmud     => gdp%gdmudcoe%cmud
    fmud     => gdp%gdmudcoe%fmud
    fwat     => gdp%gdmudcoe%fwat
    mers     => gdp%gdmudcoe%mers
    rhosus   => gdp%gdmudcoe%rhosus
    rhomud   => gdp%gdmudcoe%rhomud
    taubng   => gdp%gdmudcoe%taubng
    tauers   => gdp%gdmudcoe%tauers
    tauset   => gdp%gdmudcoe%tauset
    vdew     => gdp%gdmudcoe%vdew
    rhofrac  => gdp%gdphysco%rhofrac
    ag       => gdp%gdphysco%ag
    hdt      => gdp%gdnumeco%hdt
    !
    !      fact   = (1./cmud - 1./cbed)
    !
    !  The definition of fact above takes care of a rising bed level when
    !  consolidation occurs. This approach requires updating the bed level
    !  later on, which has not been implemented yet, so the thickness of
    !  mud layer is calculated wrongly. Since changes in the bed level are
    !  insignificant anyhow it is easiest to ignore these. This is done by
    !  the folowing definition of fact:(L. Merckelbach)
    !
    fact  = 1./cmud
    eps2t = 10.*eps2
    !
    !     loop over grid rows
    !
    do nm = 1, nmmax
       !
       !        only computational points, no open boundaries
       !
       if (kcs(nm)/=1) then
          cycle
       endif
       !
       !        ***********************************************
       !        * a. check for mud layer for actual gridpoint *
       !        ***********************************************
       !
       !        compute relevant grid point numbers
       !
       nmd = nm - icx
       ndm = nm - icy
       !
       !        check whether actual grid point is dry (at zeta-location)
       !        if actual grid point is dry, mud layer < critical depth
       !
       !        hmud = depth (w.r.t. ref. level) of mud layer computed by
       !               averaging over active grid points.
       !               hmud is located at zeta-point
       !
       dmud = max(real(dps(nm),fp) + s0(nm), 0.0_fp)
       if (kfs(nm)==1) then
          !
          !           *********************************************
          !           * a. mud layer present at actual grid point *
          !           *********************************************
          !
          !           *********************************************
          !           * a.1 computation of source term for effect *
          !           *     of erosion/dewater                    *
          !           *     (exchange mud layer and bed)          *
          !           *********************************************
          !
          !           source term is defined in zeta-point (+);
          !           so first u- and v-velocities will be transformed to zeta-point
          !           by averaging. (kmax = bottom layer)
          !
          um = 0.5*(u0(nm, kmax) + u0(nmd, kmax))
          vm = 0.5*(v0(ndm, kmax) + v0(nm, kmax))
          !
          !           a.1.1 computation of source term for dewater
          !
          uvmag2 = um**2 + vm**2
          !
          ! Erosie only occurs when there is turbulent flow in the mud-layer (Wang)
          ! Dewater occurs when effective Reynolds number is smaller than 400.
          !
          if (uvmag2<eps2) then
             renold = 10.
          else
             re1    = vismud/sqrt(uvmag2)/max(dmud, 0.00001_fp)
             re2    = tauly/2/rhomud/uvmag2
             renold = 1./(re1 + re2)
          endif
          !
          if (renold<400.0) then
             dewat = -vdew*cmud
          else
             dewat = 0.0
          endif
          !
          !           a.1.2. computation of shear stress -taum-
          !
          if (uvmag2<=eps2t) then
             !
             !              for velocities < eps, shear stress -taum- is proportional
             !              to magnitude of velocity.
             !
             alfa = taubng/eps2t + fmud*rhomud/8.0
             taum = alfa*uvmag2
          else
             !
             !              for velocities > eps
             !
             taum = taubng + (fmud*rhomud*uvmag2)/8.0
          endif
          !
          !           a.1.3. compute the source term for erosion
          !
          !           dtau = relative shear stress
          !
          dtau = taum - tauers
          !
          if (dtau>0.0 .and. renold>rencri) then
             !
             !              if shear stress > critical shear stress (between layer-bed)
             !
             erosi = mers*dtau/tauers
          else
             !
             !              if shear stress < critical shear stress (between layer-bed)
             !
             erosi = 0.0
          endif
          !
          !           a.1.4. compute exchange bed-mud layer
          !                  (erosion+dewater)
          !
          excbed(nm) = dewat + erosi
          !
          !           *********************************************
          !           * a.2 computation of source term for effect *
          !           *     of settling/entrainment               *
          !           *     (interaction suspension/mud layer)    *
          !           *********************************************
          !
          !           a.2.1. calculate richardson number rib
          !
          us = 0.5*(usus(nm, kmax) + usus(nmd, kmax))
          vs = 0.5*(vsus(ndm, kmax) + vsus(nm, kmax))
          !
          uv2 = (us - um)**2 + (vs - vm)**2
          !
          !           a.2.2. calculate entrainment rate entr
          !
          !           fwat = friction coefficient suspension/mud layer
          !
          ustin2 = fwat*uv2/8.0
          tau    = rhosus*ustin2
          usty2  = taubng/rhosus
          ustbe2 = taum/rhosus
          usttot = (ustin2**1.5 + ustbe2**1.5)**(1./3.)
          h0sus  = sepsus(nm) + real(dps(nm),fp)
          buoyan = ag*max(h0sus, 0.01_fp)*rhofrac
          if (ustin2>usty2) then
             entr(nm) = (0.5*(ustin2 - usty2)*sqrt(uv2) + 0.42*(usttot**2 -     &
                      & usty2)*usttot)*cmud/(buoyan + 0.25*uv2)
          else
             entr(nm) = max(mers*(tau - tauers)/tauers, 0.0_fp)
          endif
          !
          !           a.2.3. calculate surface shear stress tau
          !
          !
          !           a.2.4. calculate settling
          !
          !           tauset : critical shear stress for settling
          !                   (between suspension/mud layer)
          !
          !            if ( tau.gt. tauset ) then
          !
          !              critical shear stress exceeded --> no settling
          !
          !               sett(nm) = 0.0
          !            else
          !
          !              shear stress < critical shear stress --> settling
          !
          !            wssus is the settling velocity including shear stress
          !            (computed in online sediment-module)
          sett(nm) = wssus(nm)*rsed(nm, kmax)
          !
          !     *                  * (1.- tau/tauset)
          !            endif
          !
          !           a.2.5. calculate source term for combined effect of
          !                  erosion/dewater (exchange) and settling/entrainment
          !
          soumud(nm) = (sett(nm) - entr(nm))/cmud + excbed(nm)*fact
          !
          !           a.2.6. check for drying/ flooding
          !
          !           check whether actual grid point will become dry on
          !           next time step.
          !
          !           approximate thickness of mud layer for next half time
          !           step. (=dnext)
          !
          !           if (soumud(nm)*hdt+dmud .lt. 0.0) then
          !
          if (entr(nm)>sett(nm) + excbed(nm) + dmud*cmud) then
             !
             !              >>> drying
             !
             !              a.2.7. recalculate entrainment + source term
             !
             !              dmud = thickness mud layer
             !
             entr(nm)   = sett(nm) + excbed(nm) + dmud*cmud/hdt
             soumud(nm) = (sett(nm) - entr(nm))/cmud + excbed(nm)*fact
          endif
       !
       else
          !
          !           ************************************************
          !           * b. no mud layer present at actual grid point *
          !           *    so, only suspension layer                 *
          !           ************************************************
          !
          !           b.1 calculate bed shear stress for suspension
          !               layer
          !
          us   = 0.5*(usus(nm, kmax) + usus(nmd, kmax))
          vs   = 0.5*(vsus(ndm, kmax) + vsus(nm, kmax))
          !
          uv2  = us**2 + vs**2
          uabs = sqrt(uv2)
          !
          cu   = 0.5*(czusus(nmd) + czusus(nm))
          cu2  = cu**2
          cv   = 0.5*(czvsus(ndm) + czvsus(nm))
          cv2  = cv**2
          !
          taux = 0.
          if (cu2>eps2) taux = rhosus*ag*uabs*us/cu2
          tauy = 0.
          if (cv2>eps2) tauy = rhosus*ag*uabs*vs/cv2
          !
          tau = sqrt(taux**2 + tauy**2)
          !
          !           b.3. calculate entrainment rate entr and source term soumud
          !
          ustin2 = tau/rhosus
          usty2  = taubng/rhosus
          usttot = sqrt(ustin2)
          h0sus  = sepsus(nm) + real(dps(nm),fp)
          buoyan = ag*max(h0sus, 0.01_fp)*(rhomud - rhosus)/rhosus
          if (ustin2>usty2) then
             entr(nm) = (0.5*(ustin2 - usty2)*sqrt(uv2) + 0.42*(usttot**2 -     &
                      & usty2)*usttot)*cmud/(buoyan + 0.25*uv2)
          else
             entr(nm) = max(mers*(tau - tauers)/tauers, 0.0_fp)
          endif
          !
          !           ***********************************************
          !           * b.2. calculate erosion + settling           *
          !           ***********************************************
          !
          !           calculate erosion in suspension layer
          !
          if (tau>tauers) then
             !
             !              ----> erosion
             !
             !              critical shear stress for erosion exceeded --> erosion
             !
             erosi = mers*(tau - tauers)/tauers
             sett(nm) = 0.0
          else
             !
             !              ---> settling
             !
             !              shear stress < critical shear stress
             !
             erosi = 0.0
             !
             !              calculate settling in suspension layer
             !
             !               if ( tau .gt. tauset ) then
             !
             !                 critical shear stress (for settling) exceeded -> no settling
             !
             !                  sett  (nm) = 0.0
             !               else
             !
             !                 shear stress < critical shear stress --> settling
             !
             !                 sett(nm) = wssus(nm) * rsed(nm,kmax) *
             !     *                          (1.- tau/tauset)
             !               endif
             !
             !         See remark made at defining sett(nm) above
             !
             sett(nm) = wssus(nm)*rsed(nm, kmax)
          endif
          excbed(nm) = erosi
          soumud(nm) = (sett(nm) - entr(nm))/cmud + excbed(nm)*fact
          !
          !           a.2.6. check for drying/ flooding
          !
          !           check whether actual grid point will become dry on
          !           next time step.
          !
          !           approximate thickness of mud layer for next half time
          !           step. (=dnext)
          !
          !            if (soumud(nm)*hdt+dmud .lt. 0.0) then
          if (entr(nm)>sett(nm) + excbed(nm) + dmud*cmud) then
             !
             !              >>> drying
             !
             !              a.2.7. recalculate entrainment + source term
             !
             !              dmud = thickness mud layer
             !
             entr(nm)   = sett(nm) + excbed(nm) + dmud*cmud/hdt
             soumud(nm) = (sett(nm) - entr(nm))/cmud + excbed(nm)*fact
          endif
       endif
    enddo
end subroutine sourmu
