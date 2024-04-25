subroutine c_vvv(j         ,nmmaxj    ,kmax      ,nm        ,nmu       , &
               & ndm       ,ndmu      ,zmodel    ,cstbnd    ,kcs       , &
               & kfv       ,kcscuttest,kmaxx     ,v1        ,vvv       )
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
! computes average vvv velocity
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                        , intent(in) :: j
    integer                        , intent(in) :: kmax !  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in) :: kmaxx
    integer                        , intent(in) :: ndm
    integer                        , intent(in) :: ndmu
    integer                        , intent(in) :: nm
    integer                        , intent(in) :: nmmaxj !  Description and declaration in dimens.igs
    integer                        , intent(in) :: nmu
    integer, dimension(j:nmmaxj)   , intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(j:nmmaxj)   , intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    logical                        , intent(in) :: cstbnd !  Description and declaration in numeco.igs
    logical                        , intent(in) :: kcscuttest
    logical                        , intent(in) :: zmodel !  Description and declaration in procs.igs
    real(fp)                                    :: vvv
    real(fp), dimension(j:nmmaxj, kmax), intent(in) :: v1 !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: kenm
    logical :: actual_avg
!
!! executable statements -------------------------------------------------------
!
    actual_avg = (cstbnd .and. .not.zmodel .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
               & .or. (zmodel .and. kcscuttest)
    if (actual_avg) then
       kenm = max(1, kfv(nm) + kfv(ndm) + kfv(ndmu) + kfv(nmu))
       vvv = (v1(nm, kmaxx)*kfv(nm) + v1(ndm, kmaxx)*kfv(ndm) + v1(ndmu, kmaxx) &
           & *kfv(ndmu) + v1(nmu, kmaxx)*kfv(nmu))
       vvv = vvv/kenm
    else
       vvv = 0.25*(v1(nm, kmaxx) + v1(ndm, kmaxx) + v1(ndmu, kmaxx)             &
           & + v1(nmu, kmaxx))
    endif
end subroutine c_vvv
