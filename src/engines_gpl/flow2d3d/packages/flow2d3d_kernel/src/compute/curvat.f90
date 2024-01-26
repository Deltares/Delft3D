subroutine curvat(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
                & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
                & x2y       ,xy2       ,y3        ,gdp       )
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
!    Function: Computes the local curvature of streakline (2dh)
!              derived by dr. h.f.p. van den boogaard
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
!
! Global variables
!
    integer, intent(in)            :: icx
                                   !!  Increment in the X-dir., if ICX= NMAX
                                   !!  then computation proceeds in the X-
                                   !!  dir. if ICX=1 then computation pro-
                                   !!  ceeds in the Y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the Y-dir. (see ICX)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1d arrays.
                                   !!  due to the shift in the 2nd (M-)
                                   !!  index, J = -2*NMAX + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfs !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: curstr
                                   !!  Local curvature of streakline [1/M]
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gsqs !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: guu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: x2y !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: x3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xy2 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: y3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: v1 !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: k                    ! 2dH application 
    integer                        :: kenm
    integer                        :: kfsd                 ! Equal 1 if KFS(-1)=1 else 0 
    integer                        :: kfsu                 ! Equal 1 if KFS(+1)=1 else 0 
    integer                        :: ndm                  ! NM - ICY 
    integer                        :: ndmd                 ! NM - ICY-ICX 
    integer                        :: ndmu                 ! NM - ICY+ICX 
    integer                        :: nm                   ! Loop parameter 1,NMMAX 
    integer                        :: nmd                  ! NM - ICX 
    integer                        :: nmu                  ! NM + ICX 
    integer                        :: num                  ! NM + ICY 
    integer                        :: numd                 ! NM + ICY-ICX 
    real(fp)                       :: dux                  ! First derivative of u in KSI-dir. 
    real(fp)                       :: duy                  ! First derivative of u in ETA-dir. 
    real(fp)                       :: dvx                  ! First derivative of v in KSI-dir. 
    real(fp)                       :: dvy                  ! First derivative of v in ETA-dir. 
    real(fp)                       :: geta                 ! Physical distance in ETA-direction 
    real(fp)                       :: gksi                 ! Physical distance in KSI-direction 
    real(fp)                       :: umod                 ! Sqrt(UUU*UUU+VVV*VVV) 
    real(fp)                       :: uu                   ! UUU/GKSI 
    real(fp)                       :: uuu                  ! U-velocity at zeta point 
    real(fp)                       :: vv                   ! VVV/GETA 
    real(fp)                       :: vvv                  ! V-velocity at zeta point 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !     2dh
    !
    k = 1
    !
    nmu = icx
    nmd = -icx
    numd = icy - icx
    num = icy
    ndm = -icy
    ndmu = -icy + icx
    ndmd = -icy - icx
    do nm = 1, nmmax
       nmu = nmu + 1
       nmd = nmd + 1
       numd = numd + 1
       num = num + 1
       ndm = ndm + 1
       ndmu = ndmu + 1
       ndmd = ndmd + 1
       !
       curstr(nm) = 0.
       if (kfs(nm)*kcs(nm)==1) then
          uuu = 0.5*(u1(nm, k) + u1(nmd, k))
          vvv = 0.5*(v1(nm, k) + v1(ndm, k))
          umod = sqrt(uuu*uuu + vvv*vvv)
          if (umod>1.E-6) then
             !
             !     derivatives of velocities in fysical space
             !
             gksi = 0.5*(gvv(nm) + gvv(ndm))
             geta = 0.5*(guu(nm) + guu(nmd))
             dux = (u1(nm, k) - u1(nmd, k))/gksi
             dvy = (v1(nm, k) - v1(ndm, k))/geta
             !
             duy = 0.
             kfsu = 0
             if (kfs(num)==1) kfsu = 1
             kfsd = 0
             if (kfs(ndm)==1) kfsd = 1
             kenm = kfsu + kfsd
             if (kenm/=0) then
                duy = (kfsu*(u1(numd, k) + u1(num, k) - u1(nmd, k) - u1(nm, k)) &
                    & + kfsd*(u1(nmd, k) + u1(nm, k) - u1(ndmd, k) - u1(ndm, k))&
                    & )/(2.*geta*(kfsu + kfsd))
             endif
             !
             dvx = 0.
             kfsu = 0
             if (kfs(nmu)==1) kfsu = 1
             kfsd = 0
             if (kfs(nmd)==1) kfsd = 1
             kenm = kfsu + kfsd
             if (kenm/=0) then
                dvx = (kfsu*(v1(ndmu, k) + v1(nmu, k) - v1(ndm, k) - v1(nm, k)) &
                    & + kfsd*(v1(ndm, k) + v1(nm, k) - v1(ndmd, k) - v1(nmd, k))&
                    & )/(2.*gksi*(kfsu + kfsd))
             endif
             !
             !     compute local curvature of streakline
             !
             uu = uuu/gksi
             vv = vvv/geta
             curstr(nm) = -(x3(nm)*uu**3 + x2y(nm)*uu**2*vv + xy2(nm)*uu*vv**2 +&
                        & y3(nm)*vv**3 + gsqs(nm)                               &
                        & *(uu*uu*dvx + uu*vv*(dvy - dux) - vv*vv*duy))/umod**3
          endif
       endif
    enddo
end subroutine curvat
