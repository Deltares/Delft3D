subroutine dif_ws(j         ,nmmaxj    ,nmmax     ,kmax      ,lsal      , &
                & ltem      ,lstsci    ,lsed      ,kcs       ,kfs       , &
                & gsqs      ,ws        ,aakl      ,bbkl      ,cckl      , &
                & gdp       )
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
!    Function: Includes settling velocity in the coefficient
!              matrices (for sediment constituents only).
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
    integer , dimension(:,:)             , pointer :: kmxsed
!
! Global variables
!
    integer                                                                 :: j      !! Begin pointer for arrays which have been transformed into 1D arrays
                                                                                      !! Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                    , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                    , intent(in) :: lsal   !  Description and declaration in dimens.igs
    integer                                                    , intent(in) :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                    , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                    , intent(in) :: ltem   !  Description and declaration in dimens.igs
    integer                                                    , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)   , intent(in) :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                :: aakl   !! Internal work array, lower diagonal tridiagonal matrix, implicit coupling
                                                                                      !! of concentration in (N,M,K) with concentration in (N,M,K-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                :: bbkl   !! Internal work array, main diagonal tridiagonal matrix, implicit coupling
                                                                                      !! of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                :: cckl   !! Internal work array, lower diagonal tridiagonal matrix, implicit coupling
                                                                                      !! of concentration in (N,M,K) with concentration in (N,M,K+1)
!
!
! Local variables
!
    integer     :: k   ! Help variable 
    integer     :: kfd ! Mutiplicity factor for top flux 
    integer     :: kft ! Mutiplicity factor for bottom flux 
    integer     :: l   ! Help variable
    integer     :: ll
    integer     :: lst
    integer     :: nm  ! Help variable
!
!
!! executable statements -------------------------------------------------------
!
    kmxsed              => gdp%gderosed%kmxsed
    !
    ! Vertical advection; particles fall downward
    ! No fluxes through free surface
    ! No fluxes through bottom sand layer for 'sand'
    ! No fluxes through bed for non-'sand'
    !
    lst = max(lsal, ltem)
    !
    do l = 1, lsed
       !
       ! *** Previous approach used a 1st order UPWIND scheme       ***
       ! *** Replaced with CENTRAL scheme                           ***
       ! *** Purpose is to reduce upward numerical diffusion        ***
       !
       ll = lst + l
       !
       do nm = 1, nmmax
          if (kfs(nm)==1 .and. kcs(nm)<=2) then
             do k = 1, kmxsed(nm, l)
                if (k/=kmxsed(nm, l)) then
                   kft = 1
                else
                   kft = 0
                endif
                if (k/=1) then
                   kfd = 1
                else
                   kfd = 0
                endif
                aakl(nm, k, ll) = aakl(nm, k, ll) - gsqs(nm)*ws(nm, k - 1, l)*kfd
                bbkl(nm, k, ll) = bbkl(nm, k, ll) + gsqs(nm)*ws(nm, k, l)*kft
             enddo
          endif
       enddo
    enddo
end subroutine dif_ws
