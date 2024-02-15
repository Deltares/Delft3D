subroutine chkdry_dd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,kcu       ,kcv       ,kcs       ,kfu       , &
                   & kfv       ,kfs       ,hu        ,hv        ,guu       , &
                   & gvv       ,thick     ,u1        ,v1        ,qxk       , &
                   & qyk       ,gdp       )
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
!    Function: - Redefine u1,v1,qxk and qyk
!                at subdomain interfaces
!                (in case of domain decomposition)
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
                                   !!  dir. If icx=1 then computation pro-
                                   !!  ceeds in the Y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the Y-dir. (see ICX)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1D arrays.
                                   !!  Due to the shift in the 2nd (M-)
                                   !!  index, J = -2*NMAX + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcv !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: kfs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: guu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: hu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: hv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: qxk !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: qyk !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) :: v1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax), intent(in) :: thick !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: k                    ! Help var. 
    integer                        :: ndm                  ! Help var. NM-ICY 
    integer                        :: nm                   ! Help var. loops 1,nmmax and j,nmmaxj 
    integer                        :: nmd                  ! Help var. NM-ICX 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !
    !
    !-----recompute velocities at subdomain interfaces (in case of DD)
    !
    do nm = 1, nmmax
       do k = 1, kmax
          if (kcu(nm)==3) then
             u1(nm, k) = u1(nm, k)*kfu(nm)
          endif
          if (kcv(nm)==3) then
             v1(nm, k) = v1(nm, k)*kfv(nm)
          endif
       enddo
    enddo
    !
    !-----recalculate flows in x- and y- direction
    !
    do nm = 1, nmmax
       do k = 1, kmax
          if (kcu(nm)==3) then
             qxk(nm, k) = guu(nm)*hu(nm)*thick(k)*u1(nm, k)
          endif
          if (kcv(nm)==3) then
             qyk(nm, k) = gvv(nm)*hv(nm)*thick(k)*v1(nm, k)
          endif
       enddo
    enddo
    !
    !-----set KFS to 0 if the surrounding velocity points are dry
    !     -icx := -1 in m-direction, -icy := -1 in n-direction
    !
    do nm = 1, nmmax
       if (kcs(nm)>0) then
          nmd = nm - icx
          ndm = nm - icy
          kfs(nm) = max(kfu(nm), kfu(nmd), kfv(nm), kfv(ndm))
       endif
    enddo
end subroutine chkdry_dd
