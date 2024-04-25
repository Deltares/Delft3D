subroutine difuwe(timest    ,lundia    ,nst       ,icx       ,icy       , &
                & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
                & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                & kmax      , &
                & qxk       ,qyk       ,gsqs      ,e0        ,e1        , &
                & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
                & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
                & vvdwk     ,bbkl      ,ddkl      ,quant     ,wavcmp    , &
                & wenf      ,wenl      ,dps       ,s0        ,gdp       )
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
! Computes transport of wave energy in u- and
! v-direction.
! Implicit in the u-direction, and explicit in
! v-direction.
! Sinks are treated implicitly and sources
! explicitly.
!
! - Horizontal Advection in U-direction :
! implicit, higher order upwind
! - Horizontal Advection in V-direction :
! explicit, central scheme
! - At closed boundaries first order upwind
! discretization.
! - Sources are integrated explicitly.
! - Sinks are integrated implicitly.
! Comment: For the Thatcher Harleman boundaries the boundary
! points for outflow are reflected from the inner
! points; for inflow the boundary conditions are
! used (see also thahbc.for).
! At the closed boundaries at outflow the energy
! may flow out freely. At inflow zero is prescribed.
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
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
    real(fp)               , pointer :: gammax
    integer   , dimension(:,:)    , pointer  :: ibtyp
    real(fp)  , dimension(:,:)    , pointer  :: bval
!
! Global variables
!
    integer                                           , intent(in)   :: icx
    integer                                           , intent(in)   :: icy
    integer                                           , intent(in)   :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: lundia !  Description and declaration in inout.igs
    integer                                           , intent(in)   :: nmmax  !  Description and declaration in dimens.igs
    integer                                           , intent(in)   :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)   :: nst
    integer   , dimension(5, norow)                   , intent(in)   :: irocol !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)   :: kadu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)   :: kadv   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)   :: quant  !  0=wave energy, 1=breaking wave 2=roller energy
    logical                                           , intent(in)   :: wavcmp
    real(fp)                                          , intent(in)   :: timest
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bbkl
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bdddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bddx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bdx
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: buuux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: buux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: bux
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: ddkl
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: dps
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: s0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: e0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: e1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(inout):: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(inout):: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)   :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: uvdwk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: vvdwk
    real(fp)  , dimension(norow)                      , intent(in)   :: wenf   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(norow)                      , intent(in)   :: wenl   !  Description and declaration in esm_alloc_real.f90
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
    real(fp)      :: bi
    real(fp)      :: d0k    !   Internal work array
    real(fp)      :: epsitr !   Maximum value of relative error and
    real(fp)      :: facmax
    real(fp)      :: dep    !   local water depth
    real(fp)      :: emax   !   maximum allowable energy
    character(20) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    iro      => gdp%gdphysco%iro
    gammax   => gdp%gdnumeco%gammax
    ibtyp    => gdp%gdadv2d%ibtyp
    bval     => gdp%gdadv2d%bval
    !
    !  only wave energy is transported, velocities are group velocities
    !  for roller energy the speed of propagation becomes: 2* C
    !
    if (quant==2) then
       qxk  = qxk * 2.0_fp
       qyk  = qyk * 2.0_fp
    endif
    !
    ! BOUNDARY CONDITIONS
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
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
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       !
       ! EWAVE (wave energy)  : quant=0 (previously fac=1, brk=.false.)
       ! EWABR (breaking wave): quant=1 (previously fac=1, brk=.true.)
       ! EROLL (roller energy): quant=2 (previously fac=2)
       !
       ! DIRICHLET/NEUMANN BOUNDARY CONDITIONS
       !
       ibtyp(1,ic) = -1
       bval(1,ic) = 0.0_fp
       if (kcu(nmf)/=1) then
          !
          ! boundary point is closed
          !
       elseif ((ibf==6 .or. ibf==2) .and. wavcmp) then
          !
          !    type 2 : water level flow boundary
          !    or type 6 : riemann flow boundary
          !    and wave components specified
          !
          if (quant==0) then
             !
             ! wave energy
             !
             ibtyp(1,ic) = 1
             bval(1,ic) = wenf(ic)
          elseif (quant==1) then
             !
             ! breaking wave
             !
             ibtyp(1,ic) = 1
             bval(1,ic) = 0.0_fp
          else ! quant==2
             !
             ! roller energy
             ! NEUMANN
             !
             ibtyp(1,ic) = 2
             bval(1,ic) = 0.0_fp
          endif
       elseif (ibf==8) then
          !
          ! type 8 : neumann flow boundary
          !
          if (quant==2 .or. .not.wavcmp) then
             !
             ! roller energy, or wave components .not. specified
             ! NEUMANN
             !
             ibtyp(1,ic) = 2
             bval(1,ic) = 0.0_fp
          else
             !
             ! wave components specified and .not. roller energy
             !
          endif
       else
          !
          ! other cases
          !
       endif
       !
       ibtyp(2,ic) = -1
       bval(2,ic) = 0.0_fp
       if (kcu(nml)/=1) then
       elseif ((ibl==6 .or. ibl==2) .and. wavcmp) then
          if (quant==0) then
             ibtyp(2,ic) = 1
             bval(2,ic) = wenl(ic)
          elseif (quant==1) then
             ibtyp(2,ic) = 1
             bval(2,ic) = 0.0_fp
          else ! quant==2
             ! NEUMANN
             ibtyp(2,ic) = 2
             bval(2,ic) = 0.0_fp
          endif
       elseif (ibl==8) then
          if (quant==2 .or. .not.wavcmp) then
             ! NEUMANN
             ibtyp(2,ic) = 2
             bval(2,ic) = 0.0_fp
          else
          endif
       else
       endif
    enddo
    !
    call adv2d(timest    ,lundia    ,nst       ,icx       ,icy       , &
             & nmmax     ,norow     ,irocol    ,kadu      ,kadv      , &
             & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
             & kmax      ,ibtyp     ,bval      ,1         , &
             & qxk       ,qyk       ,gsqs      ,e0        ,e1        , &
             & sour      ,sink      ,bbk       ,bdddx     ,bddx      , &
             & bdx       ,bux       ,buux      ,buuux     ,uvdwk     , &
             & vvdwk     ,bbkl      ,ddkl      ,1         ,gdp       )
    !
    if (quant==2) then
       qxk  = qxk / 2.0_fp
       qyk  = qyk / 2.0_fp
    endif
    !
    ! Prevent negative energy
    !
    do nm = 1, nmmax
       if (kfs(nm) > 0) then
          e1(nm) = max(e1(nm) , 0.0_fp)
       else
          e1(nm) = 0.0_fp
       endif
    enddo
    !
end subroutine difuwe
