subroutine adv2d(timest    ,lundia    ,nst       ,icx       ,icy       , &
               & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
               & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
               & kmax      ,ibtyp     ,bval      ,layer     , &
               & qxk       ,qyk       ,gsqs      ,e0        ,e1        , &
               & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
               & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
               & vvdwk     ,bbkl      ,ddkl      ,irol      ,gdp       )
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
! Computes 2D transport in u- and v-direction.
! Implicit in the u-direction, and explicit in v-direction.
! Sinks are treated implicitly and sources explicitly.
!
! - Horizontal Advection in U-direction : implicit, higher order upwind
! - Horizontal Advection in V-direction : explicit, central scheme
! - At closed boundaries first order upwind discretization.
! - Sources are integrated explicitly.
! - Sinks are integrated implicitly.
! At the closed boundaries at outflow the energy
! may flow out freely. At inflow zero is prescribed.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    include 'flow_steps_f.inc'
!
! Global variables
!
    integer                                           , intent(in)  :: icx
    integer                                           , intent(in)  :: icy
    integer                                           , intent(in)  :: irol
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: layer
    integer                                                         :: lundia !  Description and declaration in inout.igs
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nst
    integer   , dimension(5, norow)                   , intent(in)  :: irocol !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(2, norow)                   , intent(in)  :: ibtyp
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                          , intent(in)  :: timest
    real(fp)  , dimension(2, norow)                   , intent(in)  :: bval
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bbkl
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bdddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bdx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: buuux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: buux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: ddkl
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: e0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: e1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: uvdwk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: vvdwk
!
! Local variables
!
    integer       :: ddb
    integer       :: iad1
    integer       :: iad2
    integer       :: iad3
    integer       :: ibf
    integer       :: ibl
    integer       :: ic
    integer       :: icxy
    integer       :: iter
    integer       :: itr
    integer       :: j1
    integer       :: j2
    integer       :: j3
    integer       :: kkad
    integer       :: mf
    integer       :: mfu
    integer       :: ml
    integer       :: mlu
    integer       :: n
    integer       :: ndm
    integer       :: nhystp
    integer       :: nm
    integer       :: nmd
    integer       :: nmdd
    integer       :: nmf
    integer       :: nmfu
    integer       :: nml
    integer       :: nmlu
    integer       :: nmu
    integer       :: nmuu
    integer       :: num
    integer       :: nmub
    real(fp)      :: bi
    real(fp)      :: d0k    !   Internal work array
    real(fp)      :: epsitr !   Maximum value of relative error and
    real(fp)      :: facmax
    real(fp)      :: qxu
    real(fp)      :: qyv
    character(11) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    if (layer==1) then
       ! surface
       kkad = 1
    else
       ! bottom
       kkad = kmax
    endif
    !
    ! Initialisation of arrays buuux - bdddx for all (nm)
    !
    buuux = 0.0_fp
    buux  = 0.0_fp
    bux   = 0.0_fp
    bdx   = 0.0_fp
    bddx  = 0.0_fp
    bdddx = 0.0_fp
    !
    do nm = 1, nmmax
       if (kfs(nm) == 0) then 
          vvdwk(nm) = 0.0_fp
       else
          vvdwk(nm) = e0(nm)
       endif
    enddo
    !
    !  D3dFlow_Build_2DAD:
    !   * Get VVDWK/E0 values at DD boundaries
    !
    nhystp = nxtstp(d3dflow_build_2DAD, gdp)
    !
    do nm = 1, nmmax
       if (kfs(nm)*kcs(nm) == 1) then
          bbk(nm) = gsqs(nm)/timest
       else
          bbk(nm) = 1.0_fp
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kfs(nm)*kcs(nm) == 1) then
          ddkl(nm) = gsqs(nm)*e0(nm)/timest
       else
          ddkl(nm) = e0(nm)
       endif
    enddo
    !
    ! Contribution of advection in X-direction
    ! Contribution to cells NM and NMU
    !
    nmd   = -icx
    nmdd  = -icx - icx
    nmu   =  icx
    nmuu  =  icx + icx
    do nm = 1, nmmax
       nmd   = nmd  + 1
       nmdd  = nmdd + 1
       nmu   = nmu  + 1
       nmuu  = nmuu + 1
       !
       qxu = qxk(nm)
       !
       if (qxu > 0.0_fp) then
          !
          if (irol==1) then
             !
             ! In case roller model, always an outflux, even when velocity points are inactive
             !
             iad1 = 1
          else
             iad1 =      kfu(nm)  *kadu(nm  ,kkad)
          endif
          !
          iad2 = iad1*kfu(nmd) *kadu(nmd ,kkad)
          iad3 = iad2*kfu(nmdd)*kadu(nmdd,kkad)
          !
          j1 = 6*iad1 + 3*iad2 +   iad3
          j2 =        - 3*iad2 - 2*iad3
          j3 =                     iad3
          !
          bbk  (nm ) = bbk  (nm ) + qxu*j1/6.0_fp
          bdx  (nm ) = bdx  (nm ) + qxu*j2/6.0_fp
          bddx (nm ) = bddx (nm ) + qxu*j3/6.0_fp
          !
          if (kfu(nm)*kadu(nm ,kkad) == 1 .or. irol==0) then
             !
             ! In case roller model:
             ! Only influx of energy if the cell face is open
             ! Otherwise the energy is considered dissipated
             ! 
             bdx  (nmu) = bdx  (nmu) - qxu*j1/6.0_fp
             bddx (nmu) = bddx (nmu) - qxu*j2/6.0_fp
             bdddx(nmu) = bdddx(nmu) - qxu*j3/6.0_fp
             !
          endif
       else
          if (irol==1) then
             !
             ! In case roller model, always an outflux, even when velocity points are inactive
             !
             iad1 = 1
          else
             iad1 =        kfu(nm)   * kadu(nm  ,kkad)
          endif
          !
          iad2 = iad1 * kfu(nmu)  * kadu(nmu ,kkad)
          iad3 = iad2 * kfu(nmuu) * kadu(nmuu,kkad)
          !
          j1 = 6*iad1 + 3*iad2 +   iad3
          j2 =        - 3*iad2 - 2*iad3
          j3 =                     iad3
          !
          ! Always an outflux
          !
          bbk  (nmu) = bbk  (nmu) - qxu*j1/6.0_fp
          bux  (nmu) = bux  (nmu) - qxu*j2/6.0_fp
          buux (nmu) = buux (nmu) - qxu*j3/6.0_fp
          !
          if (kfu(nm)*kadu(nm ,kkad) == 1 .or. irol==0) then
             !
             ! In case roller model:
             ! Only influx of energy if the cell face is open
             ! Otherwise the energy is considered dissipated
             ! 
             bux  (nm ) = bux  (nm ) + qxu*j1/6.0_fp
             buux (nm ) = buux (nm ) + qxu*j2/6.0_fp
             buuux(nm ) = buuux(nm ) + qxu*j3/6.0_fp
             !
          endif
       endif
    enddo
    !
    ! Contribution of advection in Y-direction
    ! Contribution to cells NM and NUM 
    !
    ndm = -icy
    num =  icy
    do nm = 1, nmmax
       ndm = ndm + 1
       num = num + 1
       !
       qyv = qyk(nm)
       !
       if (irol==1) then
          !
          ! In case roller model, always an outflux
          !
          iad1 = 1
       else
          iad1 = kfv(nm) * kadv(nm, kkad)
       endif
       !iad2 = iad1 * kfv(num) *kadv(num,kkad) * kfv(ndm) * kadv(ndm,kkad)
       if (kcs(nm) == 2 .or. kcs(num) == 2) then
          iad2 = 0
       else
          iad2 = iad1
       endif
       !
       if (qyv > 0.0_fp) then
          !
          d0k = 0.5*qyv*( (2*iad1 - iad2)*e0(nm)   &
              &          +          iad2 *e0(num))
          if (kcs(nm) == 1) then
             !
             ! Always an outflux
             !
             ddkl(nm)  = ddkl(nm)  - d0k
          endif
          if (kcs(num) == 1) then
             if (kfv(nm) * kadv(nm, kkad) == 1 .or. irol==0) then
                !
                ! Only influx of energy if the cell face is open
                ! Otherwise the energy is considered dissipated
                ! 
                ddkl(num) = ddkl(num) + d0k
             endif
          endif
       elseif (qyv < 0.0_fp) then
          d0k = 0.5*qyv*( (2*iad1 - iad2)*e0(num)  &
              &          +          iad2 *e0(nm ))
          if (kcs(nm) == 1)  then
             if (kfv(nm) * kadv(nm, kkad) == 1 .or. irol==0) then
                !
                ! Only influx of energy if the cell face is open
                ! Otherwise the energy is considered dissipated
                ! 
                ddkl(nm)  = ddkl(nm)  - d0k
             endif
          endif
          if (kcs(num) == 1) then
             !
             ! Always an outflux
             !
             ddkl(num) = ddkl(num) + d0k
          endif
       endif
    enddo
    !
    do nm = 1, nmmax
       bbkl(nm) = bbk(nm)
    enddo
    !
    ! Set values in open boundary points (in part. for Y-direction)
    !
    do nm = 1, nmmax
       if (kcs(nm) == 2) then
          ddkl(nm) = e0(nm)
          bbkl(nm) = 1.0_fp
       elseif (kfs(nm) == 0) then
          ddkl(nm) = 0.0_fp
          bbkl(nm) = 1.0_fp
       else
       endif
    enddo
    !
    ! Boundary conditions
    !
    do ic = 1, norow
       !
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       mfu  = mf + 1
       ml   = irocol(3, ic)
       mlu  = ml + 1
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu = nml + icx
       ibf  = ibtyp(1, ic) !for flow: irocol(4, ic)
       ibl  = ibtyp(2, ic) !for flow: irocol(5, ic)
       !
       ! Dirichlet boundary
       !
       if (ibf==1) then
          ddkl(nmf) = bval(1,ic)
       endif
       if (ibl==1) then
          ddkl(nmlu) = bval(2,ic)
       endif
       !
       ! Neumann boundary
       !
       if (ibf==2) then
          if (kcu(nmf) == 1) then
             ddkl(nmf) = e0(nmfu)
          else
             ddkl(nmf) = 0.0_fp
          endif
       endif
       if (ibl==2) then
          if (kcu(nml) == 1) then
             ddkl(nmlu) = e0(nml)
          else
             ddkl(nmlu) = 0.0_fp
          endif
       endif
    enddo
    !
    ! Source and sink terms
    ! Sinks are treated implicitly
    !
    do nm = 1, nmmax
       if (kfs(nm)*kcs(nm) == 1) then
          bbkl(nm) = bbkl(nm) + sink(nm)*gsqs(nm)
          ddkl(nm) = ddkl(nm) + sour(nm)*gsqs(nm)
       endif
    enddo
    !
    ! set concentrations in dry points and in open boundary points
    !
    do nm = 1, nmmax
       if (kfs(nm)==0 .or. kcs(nm)==2) then
          vvdwk(nm) = ddkl(nm)
          e1(nm) = ddkl(nm)
       endif
    enddo
    !
    ! Solution procedure system of equations
    !
    do nm = 1, nmmax
       if (kfs(nm)*kcs(nm) == 1) then
          bi       = 1.0_fp/bbkl(nm)
          bbkl(nm) = bi
       endif
    enddo
    !
    ! Iteration loop
    !
    iter = 0
    do nm = 1, nmmax
       if (kfs(nm)*kcs(nm) == 1) then
          e1(nm) = e0(nm)
       endif
    enddo
    !
    !       (re)solve system of equations
    !
  111 continue
    !
1100 continue
    iter = iter + 1
    !
    ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
    ! IN HORIZONTAL DIRECTION
    !
    itr = 0
    do nm = 1, nmmax, 2
       !
       ! COMPUTE RIGHT HAND SIDE
       ! ( CHECK FOR KCS TO AVOID AN ARRAY INDEX OUT OF BOUNDS )
       !
       if (kfs(nm)*kcs(nm)==1) then
          uvdwk(nm) = ddkl(nm)                       &
                    & - bdddx(nm)*vvdwk(nm-icx-icx-icx) &
                    & - bddx (nm)*vvdwk(nm-icx-icx    ) &
                    & - bdx  (nm)*vvdwk(nm-icx        ) &
                    & - bux  (nm)*vvdwk(nm+icx        ) &
                    & - buux (nm)*vvdwk(nm+icx+icx    ) &
                    & - buuux(nm)*vvdwk(nm+icx+icx+icx)
       endif
    enddo
    do nm = 1, nmmax, 2
       if (kfs(nm)*kcs(nm) == 1) e1(nm) = uvdwk(nm)*bbkl(nm)
    enddo
    !
    ! CHECK FOR CONVERGENCE
    !
    do nm = 1, nmmax, 2
       if (kfs(nm)*kcs(nm)==1) then
          epsitr = max(1.E-4_fp, 1.E-3_fp*abs(vvdwk(nm)))
          if (abs(vvdwk(nm)-e1(nm)) > epsitr) itr = 1
          vvdwk(nm) = e1(nm)
       endif
    enddo
    !
    do nm = 2, nmmax, 2
       !
       ! COMPUTE RIGHT HAND SIDE
       !
       if (kfs(nm)*kcs(nm) == 1) then
          uvdwk(nm) = ddkl(nm)                       &
                    & - bdddx(nm)*vvdwk(nm-icx-icx-icx) &
                    & - bddx (nm)*vvdwk(nm-icx    -icx) &
                    & - bdx  (nm)*vvdwk(nm-icx        ) &
                    & - bux  (nm)*vvdwk(nm+icx        ) &
                    & - buux (nm)*vvdwk(nm+icx    +icx) &
                    & - buuux(nm)*vvdwk(nm+icx+icx+icx)
       endif
    enddo
    !
    do nm = 2, nmmax, 2
       if (kfs(nm)*kcs(nm) == 1) e1(nm) = uvdwk(nm)*bbkl(nm)
    enddo
    !
    ! CHECK FOR CONVERGENCE
    !
    do nm = 2, nmmax, 2
       if (kfs(nm)*kcs(nm) == 1) then
          epsitr = max(1.E-4_fp, 1.E-3_fp*abs(vvdwk(nm)))
          if (abs(vvdwk(nm)-e1(nm)) > epsitr) itr = 1
          vvdwk(nm) = e1(nm)
       endif
    enddo
    !
    ! NEUMANN BC
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       mfu  = mf + 1
       ml   = irocol(3, ic)
       mlu  = ml + 1
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu = nml + icx
       ibf  = ibtyp(1, ic) !for flow: irocol(4, ic)
       ibl  = ibtyp(2, ic) !for flow: irocol(5, ic)
       if (ibf==2) then
          if (kcu(nmf) == 1) then
             e1(nmf) = e1(nmfu)
             vvdwk(nmf) = vvdwk(nmfu)
          endif
       endif
       if (ibl==2) then
          if (kcu(nml) == 1) then
             e1(nmlu) = e1(nml)
             vvdwk(nmlu) = vvdwk(nml)
          endif
       endif
    enddo
    !
    if (itr>0 .and. iter<50) goto 1100
    if (iter >= 50) then
       write (errtxt, '(a6,i5)') 'Adv2D', nst
       call prterr(lundia    ,'S206'    ,errtxt    )
    endif
    !
    !  D3dFlow_Solve_2DAD:
    !   * Check for convergence
    !   * Get VVDWK/E1 values at DD boundaries
    !
    nhystp = nxtstp(d3dflow_solve_2DAD, gdp)
    do nm = gdp%d%nmlb, gdp%d%nmub
       if (kcs(nm) == 3) then 
          e1(nm) = vvdwk(nm)
       endif
    enddo
    if (nhystp == d3dflow_solve_2DAD) goto 111
end subroutine adv2d
