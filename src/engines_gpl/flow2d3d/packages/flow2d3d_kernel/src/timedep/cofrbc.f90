subroutine cofrbc(j         ,nmmaxj    ,norow     ,icx       ,icy       , &
                & urf       ,hu        ,dpu       ,umean     ,vmean     , &
                & crbc      ,ctbf      ,ctbl      ,ctif      ,ctil      , &
                & ctrf      ,ctrl      ,stbf      ,stbl      ,stif      , &
                & stil      ,cgdghf    ,cgdghl    ,zetabf    ,zetabl    , &
                & zetaif    ,zetail    ,zmeanf    ,zmeanl    ,umeanf    , &
                & umeanl    ,circ2d    ,gvu       ,wsu       ,irocol    , &
                & timsec    ,hdt       ,gdp       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: nmskf
    integer                , pointer :: nmskl
    integer                , pointer :: mmskf
    integer                , pointer :: mmskl
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
!
! Global variables
!
    integer                                :: icx
    integer                                :: icy
    integer                                :: j
    integer                                :: nmmaxj !  Description and declaration in dimens.igs
    integer                                :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)           :: irocol !  Description and declaration in esm_alloc_int.f90
    real(fp)                               :: hdt    !  Description and declaration in esm_alloc_real.f90
    real(fp)                               :: timsec !  Description and declaration in inttim.igs
    real(fp)                               :: urf
    real(fp), dimension(norow)                 :: cgdghf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: cgdghl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctbf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctbl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctif   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctil   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctrf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: ctrl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stbf   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stbl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stif   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: stil   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: umeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: umeanl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetabf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetabl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetaif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zetail !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zmeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow)                 :: zmeanl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow)              :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(12, norow)             :: crbc   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wsu    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: ddb
    integer :: ibf
    integer :: ibl
    integer :: ic
    integer :: icxy
    integer :: mf
    integer :: ml
    integer :: n
    integer :: nmf
    integer :: nmfu
    integer :: nmfuu
    integer :: nml
    integer :: nmld
    integer :: nmldd
    real(fp):: alfas
    real(fp):: ctr
    real(fp):: ctr0
    real(fp):: depth
    real(fp):: dt
    real(fp):: f
    real(fp):: fdenom
    real(fp):: rmux
    real(fp):: sepu
    real(fp):: tapf
    real(fp):: tau
    real(fp):: temp1
    real(fp):: temp2
    real(fp):: temp3
    real(fp):: temp4
    real(fp):: urfa
    real(fp):: zbfic
    real(fp):: zblic
!
!! executable statements -------------------------------------------------------
!
    nmskf     => gdp%gdbcdat%nmskf
    nmskl     => gdp%gdbcdat%nmskl
    mmskf     => gdp%gdbcdat%mmskf
    mmskl     => gdp%gdbcdat%mmskl
    rhow      => gdp%gdphysco%rhow
    ag        => gdp%gdphysco%ag
    iro       => gdp%gdphysco%iro
    !
    ctr0 = cos(8.0*pi/18.0)
    dt = 2.0*hdt
    !
    ! First smoothen the angles of the reflected outgoing waves
    !
    if (timsec<4*hdt) then
       do ic = 1, norow
          ctrf(ic) = 1.0
          ctrl(ic) = 1.0
       enddo
    endif
    if (norow<2) then
       temp1 = 1.0
       temp3 = 1.0
    else 
       temp1 = 0.5*(ctrf(1) + ctrf(2))
       temp3 = 0.5*(ctrl(1) + ctrl(2))
    endif
    do ic = 2, norow - 1
       temp2        = 0.25*(ctrf(ic - 1) + 2.0*ctrf(ic) + ctrf(ic + 1))
       temp4        = 0.25*(ctrl(ic - 1) + 2.0*ctrl(ic) + ctrl(ic + 1))
       ctrf(ic - 1) = temp1
       ctrl(ic - 1) = temp3
       temp1        = temp2
       temp3        = temp4
    enddo
    if (norow>1) then
       ctrf(norow)     = 0.5*(ctrf(norow - 1) + ctrf(norow))
       ctrl(norow)     = 0.5*(ctrl(norow - 1) + ctrl(norow))
       ctrf(norow - 1) = temp1
       ctrl(norow - 1) = temp3
    endif
    !
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    do ic = 1, norow
       n     = irocol(1, ic)
       mf    = irocol(2, ic) - 1
       ml    = irocol(3, ic)
       ibf   = irocol(4, ic)
       ibl   = irocol(5, ic)
       nmf   = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu  = nmf + icx
       nmfuu = nmfu + icx
       nml   = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmld  = nml - icx
       nmldd = nmld - icx
       depth = hu(nmf)
       if ((ibf==6 .or. ibf==2).and.depth>1.0e-6) then
          !
          ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT LEFT BOUNDARY
          !
          sepu  = hu(nmf) - dpu(nmf)
          urfa  = urf
          !
          ! reduction of locked incoming wave due to mask effects
          ! (nearly) equidistant grid assumed
          !
          zbfic = zetabf(ic)
          !
          ! left BC
          !
          if (icy==1) then
             if (mmskf>0) then
                zbfic = 0.0
             else
                if (nmskf>0 .and. ic<nmskf) then
                   tau   = real(nmskf - ic, fp)/real(nmskf, fp)
                   zbfic = zbfic*tapf(tau)
                endif
                if (nmskl>0 .and. ic>nmskl) then
                   tau   = real(ic - nmskl, fp)/real(icx - nmskl, fp)
                   zbfic = zbfic*tapf(tau)
                endif
             endif
          endif
          !
          ! bottom BC
          !
          if (icx==1) then
             if (nmskf>0) then
                zbfic = 0.0
             else
                if (mmskf>0 .and. ic<mmskf) then
                   tau   = real(mmskf - ic, fp)/real(mmskf, fp)
                   zbfic = zbfic*tapf(tau)
                endif
                if (mmskl>0 .and. ic>mmskl) then
                   tau   = real(ic - mmskl, fp)/real(icy - mmskl, fp)
                   zbfic = zbfic*tapf(tau)
                endif
             endif
          endif
          !
          alfas = sqrt(ag/depth)
          fdenom = umean(nmf) - alfas*(sepu - zetaif(ic)*(1.0 - ctif(ic))       &
                 & - zbfic*(1.0 - cgdghf(ic)*ctbf(ic)))
          if (abs(fdenom)>1.0E-4) then
             f = (0.25*(3.0*vmean(nmfu) - vmean(nmfuu) + 3.0*vmean(nmfu - icy)  &
               & - vmean(nmfuu - icy)) - alfas*stif(ic)*zetaif(ic)              &
               & - alfas*cgdghf(ic)*stbf(ic)*zbfic)/fdenom
             if (f*f<=1.0) then
                ctr = (1.0 - f*f)/(1.0 + f*f)
             else
                ctr  = 1.0
                urfa = 0.0
             endif
             ctr      = max(ctr0, ctr)
             ctrf(ic) = ctrf(ic)*(1.0 - urfa) + urfa*ctr
          endif
          !
          ! In the case of tidal computations the mean waterlevel at
          ! the sea boundary can be prescribed through circ2d
          !
          if (ibf==2) then
             zmeanf(ic) = circ2d(1, ic)
             umeanf(ic) = 0.0
          endif
          crbc(1, ic) = 0.5*alfas*ctrf(ic)
          crbc(2, ic) = 0.5*alfas*ctrf(ic)
          crbc(3, ic) = umeanf(ic) + alfas*(zmeanf(ic)*ctrf(ic)    &
                      &    + zetaif(ic)*(ctrf(ic) + ctif(ic))        &
                      &    + zbfic*(ctrf(ic) + cgdghf(ic)*ctbf(ic)))
          !
          ! LAYER VELOCITIES ( VELOCITY PROFILE )
          !
          rmux        = sqrt(ag*depth)*dt/gvu(nmf)
          crbc(4, ic) = ctrf(ic) + rmux
          crbc(5, ic) = ctrf(ic) - rmux
          crbc(6, ic) = 2.0*zetaif(ic)*(ctrf(ic) + ctif(ic))                    &
                      & + 2.0*zbfic*(ctrf(ic) + cgdghf(ic)*ctbf(ic))            &
                      & + 2.0*zmeanf(ic)*ctrf(ic)                               &
                      & - (2.0*(umean(nmf) - umeanf(ic)) + dt*wsu(nmf)          &
                      & /rhow/depth)/alfas
       endif
       depth = hu(nml)
       if ((ibl==6 .or. ibl==2) .and. depth>1.0e-6) then
          !
          ! WEAKLY REFLECTIVE BOUNDARY CONDITION AT RIGHT BOUNDARY
          !
          sepu  = hu(nml) - dpu(nml)
          urfa  = urf
          !
          ! reduction of locked incoming wave due to mask strips
          ! (nearly) equidistant grid assumed
          !
          zblic = zetabl(ic)
          !
          ! right BC
          !
          if (icy==1) then
             if (mmskl>0) then
                zblic = 0.0
             else
                if (nmskf>0 .and. ic<nmskf) then
                   tau   = real(nmskf - ic, fp)/real(nmskf, fp)
                   zblic = zblic*tapf(tau)
                endif
                if (nmskl>0 .and. ic>nmskl) then
                   tau   = real(ic - nmskl, fp)/real(icx - nmskl, fp)
                   zblic = zblic*tapf(tau)
                endif
             endif
          endif
          !
          ! top BC
          !
          if (icx==1) then
             if (nmskl>0) then
                zblic = 0.0
             else
                if (mmskf>0 .and. ic<mmskf) then
                   tau   = real(mmskf - ic, fp)/real(mmskf, fp)
                   zblic = zblic*tapf(tau)
                endif
                if (mmskl>0 .and. ic>mmskl) then
                   tau   = real(ic - mmskl, fp)/real(icy - mmskl, fp)
                   zblic = zblic*tapf(tau)
                endif
             endif
          endif
          !
          alfas = sqrt(ag/depth)
          fdenom = umean(nml) + alfas*(sepu - zetail(ic)*(1.0 - ctil(ic))       &
                 & - zblic*(1.0 - cgdghl(ic)*ctbl(ic)))
          if (abs(fdenom)>1.0E-4) then
             f = (0.25*(3.0*vmean(nml) - vmean(nmld) + 3.0*vmean(nml - icy)  &
               &  - vmean(nmld - icy)) - alfas*stil(ic)*zetail(ic)     &
               & - alfas*cgdghl(ic)*stbl(ic)*zblic)/fdenom
             if (f*f<=1.0) then
                ctr = (1.0 - f*f)/(1.0 + f*f)
             else
                ctr  = 1.0
                urfa = 0.0
             endif
             ctr      = max(ctr0, ctr)
             ctrl(ic) = ctrl(ic)*(1.0 - urfa) + urfa*ctr
          endif
          !
          ! In the case of tidal computations the mean waterlevel at
          ! the sea boundary can be prescribed through circ2d
          !
          if (ibl==2) then
             zmeanl(ic) = circ2d(2, ic)
             umeanl(ic) = 0.0
          endif
          !
          crbc(7, ic) = -0.5*alfas*ctrl(ic)
          crbc(8, ic) = -0.5*alfas*ctrl(ic)
          crbc(9, ic) = umeanl(ic) - alfas*(zmeanl(ic)*ctrl(ic)     &
                      &    + zetail(ic)*(ctrl(ic) + ctil(ic))              &
                      &    + zblic*(ctrl(ic) + cgdghl(ic)*ctbl(ic)))
          !
          rmux = sqrt(ag*depth)*dt/gvu(nml)
          crbc(10, ic) = ctrl(ic) - rmux
          crbc(11, ic) = ctrl(ic) + rmux
          crbc(12, ic) = 2.0*zetail(ic)*(ctrl(ic) + ctil(ic))                    &
                      & + 2.0*zblic*(ctrl(ic) + cgdghl(ic)*ctbl(ic))            &
                      & + 2.0*zmeanl(ic)*ctrl(ic)                               &
                      & + (2.0*(umean(nml) - umeanl(ic)) + dt*wsu(nml)          &
                      & /rhow/depth)/alfas
       endif
    enddo
end subroutine cofrbc
