subroutine secrhs(s0        ,s1        ,dps       ,u1        ,v1        , &
                & guu       ,gvv       ,gsqs      ,j         ,nmmaxj    , &
                & nmmax     ,kmax      ,lstsci    ,lsecfl    ,icx       , &
                & icy       ,kfu       ,kfv       ,kfs       ,kcs       , &
                & xcor      ,ycor      ,sour      ,sink      ,cfurou    , &
                & cfvrou    ,fcorio    ,curstr    ,x3        ,x2y       , &
                & xy2       ,y3        ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: secrhs.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/secrhs.f90 $
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
    integer                   , pointer :: cutcell
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    real(fp), dimension(:,:)  , pointer :: xG_L
    real(fp), dimension(:,:)  , pointer :: yG_L
    real(fp), dimension(:,:,:), pointer :: dxk
    real(fp), dimension(:,:,:), pointer :: dyk
    real(fp), dimension(:,:)  , pointer :: agsqs
    real(fp), dimension(:,:)  , pointer :: ETAx
    real(fp), dimension(:,:)  , pointer :: ETAy
    real(fp), dimension(:,:)  , pointer :: PSIx
    real(fp), dimension(:,:)  , pointer :: PSIy
    logical                   , pointer :: printCURV
    real(fp), dimension(:,:)  , pointer :: curstr_print
    logical                   , pointer :: EXACTcurv
    logical                   , pointer :: CURVboogaard
    logical, dimension(:,:)   , pointer :: oneEXIT
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
    integer        :: kenm
    integer        :: ierr    ! Error counter 
    integer        :: iexit   ! Exit code when ALPHA < 0.5 
    integer        :: k
    integer        :: n
    integer        :: m
    integer        :: ndm
    integer        :: nm
    integer        :: nmd
    real(fp)       :: alpha
    real(fp)       :: bendeq  ! Bend equilibrium for spiral motion 
    real(fp)       :: chezy   ! Chezy coefficient at zeta point 
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
    cutcell      => gdp%gdimbound%cutcell
    aguu         => gdp%gdimbound%aguu
    agvv         => gdp%gdimbound%agvv
    cutcell      => gdp%gdimbound%cutcell
    xG_L         => gdp%gdimbound%xG_L
    yG_L         => gdp%gdimbound%yG_L
    dxk          => gdp%gdimbound%dxk
    dyk          => gdp%gdimbound%dyk
    agsqs        => gdp%gdimbound%agsqs
    ETAx         => gdp%gdimbound%ETAx
    ETAy         => gdp%gdimbound%ETAy
    PSIx         => gdp%gdimbound%PSIx
    PSIy         => gdp%gdimbound%PSIy
    printCURV    => gdp%gdimbound%printCURV
    curstr_print => gdp%gdimbound%curstr_print
    EXACTcurv    => gdp%gdimbound%EXACTcurv
    CURVboogaard => gdp%gdimbound%CURVboogaard
    oneEXIT      => gdp%gdimbound%oneEXIT
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
    if (cutcell>0.and..not.CURVboogaard) then
       call curvat_bis(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
                     & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                     & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
                     & x2y       ,xy2       ,y3        ,kfu       ,kfv       , &
                     & aguu      ,agvv      ,cutcell   ,xG_L      ,yG_L      , & !it I pass xG ,yG instead of xG_L,yG_L I have also to change definition of dxk
                     & dxk       ,dyk       ,agsqs     ,ETAx      ,ETAy      , &
                     & PSIx      ,PSIy      ,oneEXIT   ,gdp       )
    else
       call curvat(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
                 & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
                 & x2y       ,xy2       ,y3        ,kfu       ,kfv       , &
                 & aguu      ,agvv      ,cutcell   ,gdp       )
    endif
    IF (EXACTcurv) then !OVERWRITE EVERYTHING WITH THE EXACT CURVATURE FOR A CIRCULAR CHANNEL CENTERED IN (0,0)
       do nm = 1, nmmax
           call nm_to_n_and_m(nm, n, m, gdp)
           curstr(nm) = 1._fp/(sqrt(xG_L(n,m)**2+yG_L(n,m)**2))
       enddo
    ENDIF
    if (printCURV) THEN
        do nm = 1, nmmax
           call nm_to_n_and_m(nm, n, m, gdp)
           curstr_print(n,m) = curstr(nm)
        enddo
    endif
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

          if (cutcell>0) then
            kenm = max(kfu(nm)+kfu(nmd),1)
            uuu = u1(nm, k)*kfu(nm) + u1(nmd, k)*kfu(nmd)
            uuu = uuu/kenm
            kenm = max(kfv(nm)+kfv(ndm),1)
            vvv = v1(nm, k)*kfv(nm) + v1(ndm, k)*kfv(ndm)
            vvv = vvv/kenm
          else
            uuu = 0.5_fp*(u1(nm, k) + u1(nmd, k))
            vvv = 0.5_fp*(v1(nm, k) + v1(ndm, k))
          endif
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
          chezy = (kfu(nm)*cfurou(nm, 1) + kfu(nmd)*cfurou(nmd, 1) + kfv(nm)    &
                & *cfvrou(nm, 1) + kfv(ndm)*cfvrou(ndm, 1))                     &
                & /(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
          !
          !---------ALPHA should be >= .5 else SOURC & SINK become < 0.
          !         because TAH0 = 0.5*(1.-2.*ALPHA)*H0 should be > 0
          !
          if (chezy>chzmin) then
             alpha = sqrt(ag)/(vonkar*chezy)
          else
             alpha = sqrt(ag)/(vonkar*chzmin)
          endif
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
