subroutine initau(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & rouflo    ,zmodel    , &
                & kcs       ,kcu       ,kfu       ,kspu      , &
                & s1        ,dpu       ,umean     ,hu        ,dps       , &
                & cfurou    ,z0urou    ,gdp       )
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
!    Function: Computation initial roughness heights
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: vonkar
!
! Global variables
!
    integer                                                :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                     !!  then computation proceeds in the X-
                                                                     !!  dir. If icx=1 then computation pro-
                                                                     !!  ceeds in the Y-dir.
    integer                                                :: j      !!  Begin pointer for arrays which have
                                                                     !!  been transformed into 1D arrays.
                                                                     !!  Due to the shift in the 2nd (M-)
                                                                     !!  index, J = -2*NMAX + 1
    integer                                                :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                :: nmmax  !  Description and declaration in dimens.igs
    integer                                                :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)              :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)              :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      :: kspu   !  Description and declaration in esm_alloc_int.f90
    logical                                                :: zmodel !  Description and declaration in procs.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                 :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                 :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                 :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                 :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                 :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(out) :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3), intent(in)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    character(4)                             , intent(in)  :: rouflo !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer :: nm
    real(fp):: cfu
    real(fp):: ee
    real(fp):: option_roughness
    real(fp):: sag
    real(fp):: sixth
!
!! executable statements -------------------------------------------------------
!
    ag       => gdp%gdphysco%ag
    z0       => gdp%gdphysco%z0
    vonkar   => gdp%gdphysco%vonkar
    !
    sixth = 1./6.
    sag = sqrt(ag)
    ee = exp(1.0)
    !
    !***the value of HU is in one direction correct in reference with
    !     the S1 value so this will always be >= 0. (SUD)
    !     in the other direction it might be incorrect, so in
    !     this routine the HU values are local calculated.
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    !
    do nm = 1, nmmax
       hu(nm) = max(hu(nm), 0.01_fp)
    enddo
    !
    !    Z0UROU(NM)   will be filled with roughness height
    !
    !***COMPUTATION BOTTOM STRESS DUE TO FLOW
    !
    do nm = 1, nmmax
       if (kcu(nm)/=0) then
          optionroughness:select case (rouflo)
          case ('MANN')
             cfu = hu(nm)**sixth/cfurou(nm, 2)
             z0urou(nm) = hu(nm)/(ee*(exp(vonkar*cfu/sag) - 1.0))
          case ('CHEZ')
             z0urou(nm) = hu(nm)/(ee*(exp(vonkar*cfurou(nm, 2)/sag) - 1.0))
          case ('WHIT')
             z0urou(nm) = cfurou(nm, 2)/30.
          case ('Z   ')
             z0urou(nm) = cfurou(nm, 2)
          case default
          end select optionroughness
       !
       endif
    enddo
end subroutine initau
