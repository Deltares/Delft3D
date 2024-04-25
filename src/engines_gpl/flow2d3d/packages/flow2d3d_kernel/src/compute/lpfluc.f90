subroutine lpfluc(j         ,nmmaxj    ,nmmax     ,kfu       ,kfv       , &
                & umean     ,vmean     ,umnldf    ,vmnldf    ,umnflc    , &
                & vmnflc    ,gdp       )
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
!    Function: Determines horizontal fluctuating velocities
!              as described in "first2d.doc"
!              (R.E. Uittenboogaard 24-12-99)
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
    real(fp)     , pointer :: hdt
    integer      , pointer :: nd
    real(fp)     , pointer :: reltim
!
! Global variables
!
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1D arrays.
                                   !!  Due to the shift in the 2nd (M-)
                                   !!  index, J = -2*NMAX + 1
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: umean !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: umnflc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: vmean !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: vmnflc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: vmnldf !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: nm
    real(fp)                       :: a                    ! Help var. See "first2d.doc" formula (2b) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    nd      => gdp%gdhtur2d%nd
    reltim  => gdp%gdhtur2d%reltim
    hdt     => gdp%gdnumeco%hdt
    !
    do nm = 1, nmmax
       if (reltim>0) then
          a = exp( - hdt/(60.*reltim))
          if (kfu(nm)==1) then
             !
             !-------------determine filtered u-velocities
             !
             umnldf(nm) = a*umnldf(nm) + (1.0 - a)*umean(nm)
          else
             umnldf(nm) = 0.
          endif
          if (kfv(nm)==1) then
             !
             !-------------determine filtered v-velocities
             !
             vmnldf(nm) = a*vmnldf(nm) + (1.0 - a)*vmean(nm)
          else
             vmnldf(nm) = 0.
          endif
       else
          umnldf(nm) = 0.
          vmnldf(nm) = 0.
       endif
       !
       !-------determine fluctuating velocity component
       !
       umnflc(nm) = umean(nm) - umnldf(nm)
       vmnflc(nm) = vmean(nm) - vmnldf(nm)
    enddo
end subroutine lpfluc
