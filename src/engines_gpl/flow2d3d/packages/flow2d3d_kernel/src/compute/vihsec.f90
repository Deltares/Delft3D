subroutine vihsec(u         ,v         ,guu       ,gvu       ,gvv       , &
                & r0        ,icx       ,icy       ,j         ,nmmaxj    , &
                & nmmax     ,kmax      ,lsecfl    ,lstsci    ,betac     , &
                & kfu       ,kfv       ,ddk       ,cfurou    ,cfvrou    , &
                & rxx       ,rxy       ,gdp       )
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
!    Function: Computes the horizontal stresses RXX, RXY and RYY
!              due to secondary motion (spiral motion), based on
!              the velocities at the old time level.
!              The gradient of the stresses is added to
!              the momentum equation.
! Method used:
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
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: chzmin
!
! Global variables
!
    integer, intent(in)            :: icx
                                   !!  Increment in the x-dir., if icx= nmax
                                   !!  then computation proceeds in the x-
                                   !!  dir. if icx=1 then computation pro-
                                   !!  ceeds in the y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the y-dir. (see icx)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1d arrays.
                                   !!  due to the shift in the 2nd (m-)
                                   !!  index, j = -2*nmax + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: lsecfl !  Description and declaration in dimens.igs
    integer, intent(in)            :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    real(fp), intent(in)               :: betac !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: guu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3), intent(in) :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3), intent(in) :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: ddk
                                   !!  Internal work array, diagonal space
                                   !!  at (n,m,k)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: rxx !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: rxy !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: u
                                   !!  U-velocities at old time level
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: v
                                   !!  V-velocities at old time level
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: r0 !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: k                    ! 2dH application 
    integer                        :: kenm                 ! Help mask parameter 
    integer                        :: kenml
    integer                        :: kenmr
    integer                        :: kenmu
    integer                        :: kenmv
    integer                        :: ndm                  ! NM - ICY 
    integer                        :: ndmu
    integer                        :: nm                   ! Loop parameter 1,NMMAX 
    integer                        :: nmd                  ! NM - ICX 
    integer                        :: nmu                  ! NM + ICX 
    integer                        :: num                  ! NM + ICY 
    integer                        :: numu                 ! NM + ICY+ICX 
    real(fp)                       :: alpha
    real(fp)                       :: beta
    real(fp)                       :: betast
    real(fp)                       :: czosg                ! Chezy/sqrt(ag) coefficient at zeta point 
    real(fp)                       :: dgdx
    real(fp)                       :: dgdy
    real(fp)                       :: dgdyl
    real(fp)                       :: dgdyr
    real(fp)                       :: fsign                ! Multiplying factor depending in which direction the matrix has to be solved 
    real(fp)                       :: rsecfl
    real(fp)                       :: umod                 ! Sqrt(uuu*uuu+vvv*vvv) 
    real(fp)                       :: uuu                  ! U-velocity at zeta point 
    real(fp)                       :: vvv                  ! V-velocity at zeta point 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    chzmin   => gdp%gdnumeco%chzmin
    ag       => gdp%gdphysco%ag
    vonkar   => gdp%gdphysco%vonkar
    !
    k = 1
    if (abs(betac)<1E-4) goto 999
    !
    do nm = 1, nmmax
       rxx(nm, k) = 0.0
       rxy(nm, k) = 0.0
    enddo
    !
    if (icx==1) then
       fsign = -1.0
    else
       fsign = 1.0
    endif
    !
    !     computation rxx in waterlevel point nm
    !
    nmd = -icx
    ndm = -icy
    do nm = 1, nmmax
       nmd = nmd + 1
       ndm = ndm + 1
       kenm = kfu(nm) + kfu(nmd)
       if (kenm==2) then
          uuu = 0.5*(u(nm, k) + u(nmd, k))
          vvv = 0.5*(v(nm, k) + v(ndm, k))
          umod = max(1.E-8_fp, sqrt(uuu*uuu + vvv*vvv))
          czosg = 0.5*(cfurou(nm, 1) + cfurou(nmd, 1))
          czosg = max(czosg, chzmin/sqrt(ag))
          alpha = 1.0_fp/(vonkar*czosg)
          betast = betac*(5.0*alpha - 15.6*alpha*alpha + 37.5*alpha*alpha*alpha)
          beta = betast*r0(nm, k, lsecfl)/umod
          !
          !     for stability:  r0/umod < 0.1
          !
          if (abs(beta)>abs(0.1*betast)) beta = 0.1*betast
          rxx(nm, k) = fsign*2.*beta*uuu*vvv
       endif
    enddo
    !
    !     computation rxy in depth point nm
    !
    nmu = icx
    num = icy
    numu = icx + icy
    do nm = 1, nmmax
       nmu = nmu + 1
       num = num + 1
       numu = numu + 1
       kenmu = kfu(nm) + kfu(num)
       kenmv = kfv(nm) + kfv(nmu)
       if (kenmu>0 .or. kenmv>0) then
          uuu = 0.0
          vvv = 0.0
          if (kenmu>0) uuu = (kfu(nm)*u(nm, k) + kfu(num)*u(num, k))/kenmu
          if (kenmv>0) vvv = (kfv(nm)*v(nm, k) + kfv(nmu)*v(nmu, k))/kenmv
          umod = max(1.E-8_fp, sqrt(uuu*uuu + vvv*vvv))
          czosg = (kfu(nm)*cfurou(nm, 1) + kfu(num)*cfurou(num, 1) + kfv(nm)    &
                & *cfvrou(nm, 1) + kfv(nmu)*cfvrou(nmu, 1))/(kenmu + kenmv)
          czosg = max(czosg, chzmin/sqrt(ag))
          alpha = 1.0_fp/(vonkar*czosg)
          betast = betac*(5.0*alpha - 15.6*alpha*alpha + 37.5*alpha*alpha*alpha)
          rsecfl = (kfu(nm)*(r0(nm, k, lsecfl) + r0(nmu, k, lsecfl)) + kfu(num) &
                 & *(r0(num, k, lsecfl) + r0(numu, k, lsecfl)) + kfv(nm)        &
                 & *(r0(nm, k, lsecfl) + r0(num, k, lsecfl)) + kfv(nmu)         &
                 & *(r0(nmu, k, lsecfl) + r0(numu, k, lsecfl)))                 &
                 & /(2.*(kenmu + kenmv))
          beta = betast*rsecfl/umod
          !
          !     for stability:  r0/umod < 0.1
          !
          if (abs(beta)>abs(0.1*betast)) beta = 0.1*betast
          rxy(nm, k) = beta*(uuu*uuu - vvv*vvv)
       endif
    enddo
    !
    !     horizontal diffusion in x- and y-direction
    !
    num = icy
    nmu = icx
    nmd = -icx
    ndm = -icy
    do nm = 1, nmmax
       num = num + 1
       nmu = nmu + 1
       nmd = nmd + 1
       ndm = ndm + 1
       if (kfu(nm)==1) then
          ddk(nm, k) = ddk(nm, k) + (rxx(nmu, k) - rxx(nm, k))/gvu(nm)          &
                     & + (rxy(nm, k) - rxy(ndm, k))/guu(nm)
       endif
    enddo
    !
    !     contribution curvature terms, ksi-direction
    !
    nmd = -icx
    nmu = icx
    do nm = 1, nmmax
       nmd = nmd + 1
       nmu = nmu + 1
       kenm = kfu(nmd) + kfu(nmu)
       if (kfu(nm)==1 .and. kenm/=0) then
          dgdx = (kfu(nmu)*(guu(nmu) - guu(nm)) + kfu(nmd)*(guu(nm) - guu(nmd)))&
               & /kenm
          ddk(nm, k) = ddk(nm, k) + (rxx(nmu, k) + rxx(nm, k))                  &
                     & *dgdx/(gvu(nm)*guu(nm))
       endif
    enddo
    !
    !     contribution curvature terms, eta-direction
    !
    nmu = icx
    ndm = -icy
    ndmu = -icy + icx
    do nm = 1, nmmax
       nmu = nmu + 1
       ndm = ndm + 1
       ndmu = ndmu + 1
       if (kfu(nm)==1) then
          kenml = kfv(nm) + kfv(ndm)
          kenmr = kfv(nmu) + kfv(ndmu)
          if (kenml==2 .or. kenmr==2) then
             dgdyl = kfv(nm)*gvv(nm) - kfv(ndm)*gvv(ndm)
             dgdyr = kfv(nmu)*gvv(nmu) - kfv(ndmu)*gvv(ndmu)
             dgdy = (kfv(nm)*dgdyl + kfv(nmu)*dgdyr)/(kfv(nm) + kfv(nmu))
          else
             dgdy = 0.0
          endif
          ddk(nm, k) = ddk(nm, k) + (rxy(nm, k) + rxy(ndm, k))                  &
                     & *dgdy/(gvu(nm)*guu(nm))
       endif
    enddo
    !
  999 continue
end subroutine vihsec
