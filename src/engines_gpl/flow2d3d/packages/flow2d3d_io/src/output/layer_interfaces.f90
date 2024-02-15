subroutine layer_interfaces(zmodel  ,kmax    ,mmax    ,nmaxus  ,s1     , &
                          & dps     ,thick   ,dzs1    ,kcs     ,kfs    , &
                          & kfsmin  ,kfsmax  ,zkt     ,gdp     )
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
!    Function: - Calculates the vertical coordinates of the layer interfaces
!                Both for Sigma- and for Z-model layering
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
!
! Global variables
!
    logical                                                                                   :: zmodel !  
    integer                                                                     , intent(in)  :: kmax   !  Description and declaration in esm_alloc_real.f90
    integer                                                                     , intent(in)  :: mmax   !  Description and declaration in esm_alloc_real.f90
    integer                                                                     , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: kcs    !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: kfs    !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_real.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)   , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                             , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax) , intent(out) :: zkt    !  Vertical coordinates of layering interfaces
!
! Local variables
!
    integer                           :: k              ! Loop counter for K
    integer                           :: kc             ! Help variable
    integer                           :: kmin           ! Help variable
    integer                           :: m              ! Loop counter for M
    integer                           :: n              ! Loop counter for N
!
!! executable statements -------------------------------------------------------
!
    ! Default for permanently dry cell
    !
    zkt = -999.0_fp
    !
    if (.not. zmodel) then
       !
       ! Sigma-layering
       !
       do k = 0, kmax
          kc = min(k + 1, kmax)
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n,m) /= 0) then
                   !
                   !  Active cell or temporarily inactive cell
                   !
                   if (k == 0) then
                      zkt(n,m,k) = s1(n,m)
                   elseif (k == kmax) then
                      zkt(n,m,k) = -real(dps(n,m),fp)
                   else
                      zkt(n,m,k) = zkt(n,m,k-1) - (s1(n,m)+real(dps(n,m),fp))*thick(kc)
                   endif
                endif
             enddo
          enddo
       enddo
    else
       !
       ! Fixed Z-layering
       !
       do n = 1, nmaxus
          do m = 1, mmax
             if (kcs(n,m) /= 0)  then
                !
                ! Active cell or temporarily inactive cell
                !
                kmin = max(kfsmin(n,m),kfsmax(n,m))
                do k = kmax, 0, -1
                   kc = min(k + 1, kmax)
                   if (k >= kmin) then
                      zkt(n,m,k) = s1(n,m)
                   elseif (k < kfsmin(n,m)) then
                      zkt(n,m,k) = - real(dps(n,m),fp)
                   else
                      zkt(n,m,k) = zkt(n,m,k+1) - dzs1(n,m,kc)
                   endif
                enddo
             endif
          enddo
       enddo
    endif
end subroutine layer_interfaces
