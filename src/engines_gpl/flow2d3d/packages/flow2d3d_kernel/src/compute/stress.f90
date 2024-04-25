subroutine stress(u0        ,v0        ,usus      ,vsus      ,windu     , &
                & kfu       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                & icx       ,icy       ,gdp       )
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
!    Function: The interface stress between the suspension layer
!              and the fluid mud layer is computed.
! Method used: Reference: Transport of Fluid Mud, numerical
!              modelling with a two-layer system. Research
!              documentation. November 1995. Deltares.
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
    real(fp) , pointer :: fwat
    real(fp) , pointer :: rhosus
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
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: windu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: u0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: usus !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: v0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: vsus !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: ndm
    integer                        :: ndmu
    integer                        :: nm
    integer                        :: nmu
    real(fp)                       :: udif                 ! U-velocity difference 
    real(fp)                       :: umag                 ! Magnitude velocity difference 
    real(fp)                       :: vdif                 ! V-velocity difference 
    real(fp)                       :: vvvm                 ! V-velocity mud in U-point 
    real(fp)                       :: vvvs                 ! V-velocity suspension in U-point 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    fwat    => gdp%gdmudcoe%fwat
    rhosus  => gdp%gdmudcoe%rhosus
    !
    do nm = 1, nmmax
       if (kfu(nm)==1) then
          nmu = nm + icx
          ndm = nm - icy
          ndmu = ndm + icx
          udif = usus(nm, 1) - u0(nm, 1)
          vvvs = 0.25*(vsus(nm, 1) + vsus(nmu, 1) + vsus(ndm, 1) + vsus(ndmu, 1)&
               & )
          vvvm = 0.25*(v0(nm, 1) + v0(nmu, 1) + v0(ndm, 1) + v0(ndmu, 1))
          vdif = vvvs - vvvm
          umag = sqrt(vdif**2 + udif**2)
          windu(nm) = -0.125*fwat*rhosus*umag*udif
       endif
    enddo
end subroutine stress
