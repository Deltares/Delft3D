subroutine secrhs(s0        ,s1        ,dps       ,u1        ,v1        , &
                & guu       ,gvv       ,gsqs      ,j         ,nmmaxj    , &
                & nmmax     ,kmax      ,lstsci    ,lsecfl    ,icx       , &
                & icy       ,kfu       ,kfv       ,kfs       ,kcs       , &
                & xcor      ,ycor      ,sour      ,sink      ,cfurou    , &
                & cfvrou    ,fcorio    ,curstr    ,x3        ,x2y       , &
                & xy2       ,y3        ,gdp       )
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
!    Function: Computes righthandside terms for the transport
!              equation of spiral motion intensity (secondary
!              flow).
!              3-1-94 CF formula Ta aangepast.
! Method used:
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
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vonkar
    integer                , pointer :: lundia
    real(fp)               , pointer :: chzmin
    real(fp)               , pointer :: dryflc
    real(fp)               , pointer :: rmincf
!
! Global variables
!
    integer                                                                 :: icx    !!  Increment in the x-dir., if icx= nmax then computation proceeds in the x-dir.
                                                                                      !!  if icx=1 then computation proceeds in the y-dir.
    integer                                                                 :: icy    !!  Increment in the y-dir. (see icx)
    integer                                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1d arrays.
                                                                                      !!  due to the shift in the 2nd (m-)index, j = -2*nmax + 1
    integer                                                                 :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: lsecfl !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                 :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: curstr !!  Internal work array
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: fcorio !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: x2y    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: x3     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: xy2    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: y3     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 3)           , intent(in)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 3)           , intent(in)  :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(out) :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(out) :: sour   !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer        :: ierr    ! Error counter 
    integer        :: iexit   ! Exit code when ALPHA < 0.5 
    integer        :: k
    integer        :: ndm
    integer        :: nm
    integer        :: nmd
    real(fp)       :: alpha
    real(fp)       :: bendeq  ! Bend equilibrium for spiral motion 
    real(fp)       :: czosg   ! Chezy/sqrt(ag) coefficient at zeta point 
    real(fp)       :: corieq  ! Coriolis equilibrium for spiral m. 
    real(fp)       :: geta2
    real(fp)       :: gksi2
    real(fp)       :: h0new   ! Total waterdepth at new time 
    real(fp)       :: h0old   ! Total waterdepth at old time 
    real(fp)       :: htrsh
    real(fp)       :: riv
    real(fp)       :: rminiv
    real(fp)       :: tai
    real(fp)       :: tanew
    real(fp)       :: taold
    real(fp)       :: umod    ! Sqrt(uuu*uuu+vvv*vvv) 
    real(fp)       :: uuu     ! U-velocity at zeta point 
    real(fp)       :: vvv     ! V-velocity at zeta point 
    character(15)  :: errmsg  ! Text string for error message 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    chzmin   => gdp%gdnumeco%chzmin
    dryflc   => gdp%gdnumeco%dryflc
    rmincf   => gdp%gdnumeco%rmincf
    lundia   => gdp%gdinout%lundia
    ag       => gdp%gdphysco%ag
    vonkar   => gdp%gdphysco%vonkar
    !
    htrsh = 0.5*dryflc
    k = 1
    !
    !     compute curvature of streakline
    !
    call curvat(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
              & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
              & x2y       ,xy2       ,y3        ,gdp       )
    !
    !     compute source and sink terms
    !     For SOUR the old time and for SINK the new time
    !
    ierr = 0
    nmd = -icx
    ndm = -icy
    !
    do nm = 1, nmmax
       nmd = nmd + 1
       ndm = ndm + 1
       !
       !-------Actual point should be active
       !
       if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
          h0old = max(htrsh, s0(nm) + real(dps(nm),fp))
          h0new = max(htrsh, s1(nm) + real(dps(nm),fp))
          uuu = 0.5*(u1(nm, k) + u1(nmd, k))
          vvv = 0.5*(v1(nm, k) + v1(ndm, k))
          umod = sqrt(uuu*uuu + vvv*vvv)
          gksi2 = gvv(nm) + gvv(ndm)
          geta2 = guu(nm) + guu(nmd)
          rminiv = 1.0/(rmincf*max(gksi2, geta2))
          if (abs(curstr(nm))<rminiv) then
             riv = curstr(nm)
          else
             riv = sign(rminiv, curstr(nm))
          endif
          bendeq = umod*h0old*riv
          corieq = 0.5*fcorio(nm)*h0old
          czosg = (kfu(nm)*cfurou(nm, 1) + kfu(nmd)*cfurou(nmd, 1) + kfv(nm)    &
                & *cfvrou(nm, 1) + kfv(ndm)*cfvrou(ndm, 1))                     &
                & /(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
          !
          !---------ALPHA should be >= .5 else SOURC & SINK become < 0.
          !         because TAH0 = 0.5*(1.-2.*ALPHA)*H0 should be > 0
          !
          czosg = max(czosg, chzmin/sqrt(ag))
          alpha = 1.0_fp/(vonkar*czosg)
          if (alpha>=0.5) then
             ierr = ierr + 1
          endif
          taold = 0.5*(1. - 2.*alpha)*h0old
          tanew = 0.5*(1. - 2.*alpha)*h0new
          tai = ((vonkar**2*alpha)*umod)/taold
          !
          !---------Subtract Coriolis contribution
          !
          sour(nm, k, lsecfl) = (bendeq - corieq)*tai
          tai = ((vonkar**2*alpha)*umod)/tanew
          sink(nm, k, lsecfl) = 1.*tai
       endif
    enddo
    !
    !-----Number of errors
    !
    if (ierr>0) then
       write (errmsg, '(a,i3,a)') 'in ', ierr, ' point(s)'
       call prterr(lundia    ,'S230'    ,errmsg    )
       !
       !
       !-------stop routine for DELFT3D
       !
       iexit = 4
       call d3stop(iexit     ,gdp       )
    !
    endif
end subroutine secrhs
