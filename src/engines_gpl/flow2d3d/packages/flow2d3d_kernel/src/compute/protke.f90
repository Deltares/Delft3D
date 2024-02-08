subroutine protke(j         ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                & kfs       ,kfu       ,kfv       ,kcs       ,umnflc    , &
                & vmnflc    ,guu       ,gvv       ,wrk1      ,wrk2      , &
                & ptke      ,gdp       )
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
!    Function: Determines horizontal production term for
!              TKE according to document "first2d.doc"
!              (R.E. Uittenboogaard 24-12-99)
!              NOTE: boundary assumption: protke = 0.
!                    Latest change to allow HLES in DD
! Method used:
!     Comment:
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
    integer, intent(in)                                       :: icx    !! Increment in the X-dir., if ICX= NMAX
                                                                        !! then computation proceeds in the X-
                                                                        !! dir. If icx=1 then computation pro-
                                                                        !! ceeds in the Y-dir.
    integer, intent(in)                                       :: icy
                                                                        !! Increment in the Y-dir. (see ICX)
    integer                                                   :: j      !! Begin pointer for arrays which have
                                                                        !! been transformed into 1D arrays.
                                                                        !! Due to the shift in the 2nd (M-)
                                                                        !! index, J = -2*NMAX + 1
    integer                                     , intent(in)  :: nmmax  ! Description and declaration in dimens.igs
    integer                                                   :: nmmaxj ! Description and declaration in dimens.igs
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcs    ! Description and declaration in esm_alloc_int.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kfs    ! Description and declaration in esm_alloc_int.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kfu    ! Description and declaration in esm_alloc_int.f90
    integer  , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kfv    ! Description and declaration in esm_alloc_int.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: guu    ! Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: gvv    ! Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(out) :: ptke   !! TKE production due to velocity fluctuation
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: umnflc ! Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: vmnflc ! Description and declaration in esm_alloc_real.f90
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrk1
    real(fp) , dimension(gdp%d%nmlb:gdp%d%nmub)               :: wrk2
!
!
! Local variables
!
    integer                        :: kz
    integer                        :: ndm
    integer                        :: ndmd
    integer                        :: nm
    integer                        :: nmd
    integer                        :: nmu
    integer                        :: num
    real(fp)                       :: dx
    real(fp)                       :: dy
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialise the work arrays at 0
    !    
    wrk1 = 0.0_fp
    wrk2 = 0.0_fp
    !
    !-----(du/dx)2 + (dv/dy)2 (at zeta points)
    !
    do nm = 1, nmmax
       if (kfs(nm)==1) then
          nmu = nm + icx
          num = nm + icy
          nmd = nm - icx
          ndm = nm - icy
          dx = 0.5*(gvv(nm) + gvv(ndm))
          dy = 0.5*(guu(nm) + guu(nmd))
          wrk1(nm) = ((umnflc(nm) - umnflc(nmd))/dx)                            &
                   & **2 + ((vmnflc(nm) - vmnflc(ndm))/dy)**2
       endif
    enddo
    !
    !-----(du/dy)2 + (dv/dx)2 + 2du/dydv/dx (at depth points)
    !
    do nm = 1, nmmax
       nmu = nm + icx
       num = nm + icy
       nmd = nm - icx
       ndm = nm - icy
       kz = kfv(nm)*kfv(nmu)*kfu(nm)*kfu(num)
       if (kz==1) then
          dx = 0.5*(gvv(nm) + gvv(nmu))
          dy = 0.5*(guu(nm) + guu(num))
          wrk2(nm) = ((umnflc(num) - umnflc(nm))/dy + (vmnflc(nmu) - vmnflc(nm))&
                   & /dx)**2
       endif
    enddo
    !
    !-----total S2 (at zeta points)
    !
    do nm = 1, nmmax
       if (kfs(nm)==1) then
          nmd = nm - icx
          ndm = nm - icy
          ndmd = nm - icx - icy
          ptke(nm) = 2*wrk1(nm) + (wrk2(nm) + wrk2(nmd) + wrk2(ndm) + wrk2(ndmd)&
                   & )/4.0
       else
          ptke(nm) = 0.
       endif
    enddo
end subroutine protke
