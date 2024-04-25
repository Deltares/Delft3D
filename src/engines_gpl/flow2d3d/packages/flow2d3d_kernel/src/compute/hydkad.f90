subroutine hydkad(j         ,nmmaxj    ,nmmax     ,kmax      ,kspu      , &
                & kspv      ,kadu      ,kadv      ,gdp       )
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
!    Function: Computes KADU/V values for hydrodynamic routine
!              UZD and CUCNP(2) for the effect of structures
!              on the advective terms
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
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1D arrays.
                                   !!  Due to the shift in the 2nd (M-)
                                   !!  index, J = -2*NMAX + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: kspu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: kspv !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kadu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kadv !  Description and declaration in esm_alloc_int.f90
!
!
! Local variables
!
    integer                        :: k                    ! Loop counter for loop 1,KMAX 
    integer                        :: nm                   ! Loop counter for loop 1,NMMAX 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----KADU/V(NM,K) filled with 0 for all structure for all layers
    !     For KSPU/V(NM,0) < 0 no upwinding => KADU/V (NM,K) = 1
    !
    do k = 1, kmax
       do nm = 1, nmmax
          kadu(nm, k) = 1 - min(max(kspu(nm, 0), 0), 1)
          kadv(nm, k) = 1 - min(max(kspv(nm, 0), 0), 1)
       enddo
    enddo
end subroutine hydkad
