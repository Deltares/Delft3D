subroutine curvat(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
                & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
                & x2y       ,xy2       ,y3        ,kfu       ,kfv       , &
                & aguu      ,agvv      ,cutcell   ,gdp       )
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
!  $Id: curvat.f90 6021 2016-04-15 14:20:51Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/curvat.f90 $
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
    integer, intent(in)            :: cutcell
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: aguu !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: agvv !  Description and declaration in esm_alloc_int.f90
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
    integer                        :: kfLU,klRU,klLD,klRD,kf1
    integer                        :: kfTOT 
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
    real(fp)                       :: derLU,derRU,derLD,derRD 
    real(fp)                       :: gpsiLU,gpsiRU,gpsiLD,gpsiRD 
    real(fp)                       :: getaLU,getaRU,getaLD,getaRD 
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
          if (umod>1.E-6) then
             !
             !     derivatives of velocities in physical space
             !
             gksi = 0.5*(gvv(nm) + gvv(ndm))
             geta = 0.5*(guu(nm) + guu(nmd))
             kf1 = min(kfu(nm),kfu(nmd))
             dux = (u1(nm, k) - u1(nmd, k))/gksi*kf1
             kf1 = min(kfv(nm),kfv(ndm))
             dvy = (v1(nm, k) - v1(ndm, k))/geta*kf1
             !
             duy = 0.
             kfLU = min(kfu(numd),kfu(nmd))
             klRU = min(kfu(num) ,kfu(nm))
             klLD = min(kfu(nmd) ,kfu(ndmd))
             klRD = min(kfu(nm)  ,kfu(ndm))
             kfTOT = kfLU+klRU+klLD+klRD
             if (kfTOT/=0) then
                getaLU = 0.5*(guu(numd)+ guu(nmd))
                getaRU = 0.5*(guu(num) + guu(nm))
                getaLD = 0.5*(guu(nmd) + guu(ndmd))
                getaRD = 0.5*(guu(nm)  + guu(ndm))
                derLU = (u1(numd, k) - u1(nmd, k)) /getaLU * kfLU
                derRU = (u1(num, k)  - u1(nm, k))  /getaRU * klRU
                derLD = (u1(nmd, k)  - u1(ndmd, k))/getaLD * klLD 
                derRD = (u1(nm, k)   - u1(ndm, k)) /getaRD * klRD
                duy = (derLU+derRU+derLD+derRD) /kfTOT
                !to be removed
!                duy = (kfsu*(u1(numd, k) + u1(num, k) - u1(nmd, k) - u1(nm, k)) &
!                    & + kfsd*(u1(nmd, k) + u1(nm, k) - u1(ndmd, k) - u1(ndm, k))&
!                    & )/(2.*geta*(kfsu + kfsd))
             endif
             !
             dvx = 0.
             kfLU = min(kfv(nm)  ,kfv(nmd))
             klRU = min(kfv(nmu) ,kfv(nm))
             klLD = min(kfv(ndm) ,kfv(ndmd))
             klRD = min(kfv(ndmu),kfv(ndm))
             kfTOT = kfLU+klRU+klLD+klRD
             if (kfTOT/=0) then
                gpsiLU = 0.5*(gvv(nm)  + gvv(nmd))
                gpsiRU = 0.5*(gvv(nmu) + gvv(nm))
                gpsiLD = 0.5*(gvv(ndm) + gvv(ndmd))
                gpsiRD = 0.5*(gvv(ndmu)+ gvv(ndm))
                derLU = (v1(nm, k)   - v1(nmd, k)) /gpsiLU * kfLU
                derRU = (v1(nmu, k)  - v1(nm, k))  /gpsiRU * klRU
                derLD = (v1(ndm, k)  - v1(ndmd, k))/gpsiLD * klLD 
                derRD = (v1(ndmu, k) - v1(ndm, k)) /gpsiRD * klRD
                dvx = (derLU+derRU+derLD+derRD) / kfTOT
                !to be removed
!                dvx = (kfsu*(v1(ndmu, k) + v1(nmu, k) - v1(ndm, k) - v1(nm, k)) &
!                    & + kfsd*(v1(ndm, k) + v1(nm, k) - v1(ndmd, k) - v1(nmd, k))&
!                    & )/(2.*gksi*(kfsu + kfsd))
             endif
             !
             !     compute local curvature of streakline
             !
             uu = uuu/gksi
             vv = vvv/geta
             curstr(nm) = -(x3(nm)*uu**3 + x2y(nm)*uu**2*vv + xy2(nm)*uu*vv**2 +&
                        & y3(nm)*vv**3 + gsqs(nm)                               &
                        & *(uu*uu*dvx + uu*vv*(dvy - dux) - vv*vv*duy))/umod**3
          !   write(10191817,'(i9,10f25.15)') nm,dux,duy,dvx,dvy,uuu,vvv,uu,vv,curstr(nm)
          endif
       endif
    enddo
end subroutine curvat
