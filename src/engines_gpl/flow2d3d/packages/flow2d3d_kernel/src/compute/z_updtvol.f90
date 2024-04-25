subroutine z_updtvol(nmmax     ,kmax      ,kcs       ,kcu       ,kcv       , &
                   & gsqs      ,dzs1      ,dzu0      ,dzv0      , &
                   & volum1    ,areau     ,areav     ,gdp       )
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
! Update volumes
! (to maintain compatibility with Delft3D-WAQ)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                , pointer :: nonhyd
    integer                , pointer :: nh_level
!
! Global variables
!
    integer                                         , intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areau   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areav   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: volum1  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: nm
    !integer                            :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    !integer, dimension(:), allocatable :: masks  ! temporary array for masking volumes
    !integer, dimension(:), allocatable :: masku  ! temporary array for masking areas
!
!! executable statements -------------------------------------------------------
!
    nonhyd      => gdp%gdprocs%nonhyd
    nh_level    => gdp%gdnonhyd%nh_level
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    ! mask initial arrays
    ! Note: for parallel runs, temporary arrays are allocated for masking volumes and areas
    !
    !allocate(masks(gdp%d%nmlb:gdp%d%nmub))
    !allocate(masku(gdp%d%nmlb:gdp%d%nmub))
    !masks(:) = min(1, kcs(:))
    !masku(:) = min(1, kcu(:))
    !nm_pos   = 1
    !call dfexchg ( masks, 1, 1, dfint, nm_pos, gdp )
    !call dfexchg ( masku, 1, 1, dfint, nm_pos, gdp )
    !
    do k = 1, kmax
       do nm = 1, nmmax
          volum1(nm, k) = dzs1(nm, k)*gsqs(nm)*min(1,kcs(nm))
       enddo
    enddo
    !
    if (nonhyd .and. nh_level==nh_full) then
       do k = 1, kmax
          do nm = 1, nmmax
             areau(nm, k) = dzu0(nm, k)*gsqs(nm)*min(1,kcu(nm))
             areav(nm, k) = dzv0(nm, k)*gsqs(nm)*min(1,kcv(nm))
          enddo
       enddo
    endif
    !deallocate(masks,masku)
end subroutine z_updtvol
