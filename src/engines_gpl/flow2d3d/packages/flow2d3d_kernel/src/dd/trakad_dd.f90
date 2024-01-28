subroutine trakad_dd(nmmax  ,kmax   ,icx    ,icy    , &
                   & kcs    ,kadu   ,kadv   ,gdp    )
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
!    Function: See trakad
!              Correction on coupling points
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
    integer                                        , intent(in)  :: icx
    integer                                        , intent(in)  :: icy
    integer                                        , intent(in)  :: kmax  !  Description and declaration in esm_alloc_int.f90
    integer                                        , intent(in)  :: nmmax !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kadu  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kadv  !  Description and declaration in esm_alloc_int.f90
!
! Local variables
!
    integer :: k       ! Loop counter for loop 1,KMAX
    integer :: nm      ! Loop counter for loop 1,NMMAX
    integer :: nmd
    integer :: nmu
    integer :: ndm
    integer :: num
!
!! executable statements -------------------------------------------------------
!
    do k = 1, kmax
       do nm = 1, nmmax
          nmd = nm - icx
          nmu = nm + icx
          ndm = nm - icy
          num = nm + icy
          !
          ! at left side of subdomain interface:
          !
          if (kcs(nm)==3 .and. kcs(nmu)==1) then
             kadu(nmd, k) = 0
             kadu(nm , k) = 1
          endif
          !
          ! at right side of subdomain interface:
          !
          if (kcs(nmd)==1 .and. kcs(nm)==3) then
             kadu(nmd, k) = 1
             kadu(nm , k) = 0
          endif
          !
          !
          ! at bottom side of subdomain interface:
          !
          if (kcs(nm)==3 .and. kcs(num)==1) then
             kadv(ndm, k) = 0
             kadv(nm , k) = 1
          endif
          !
          ! at top side of subdomain interface:
          !
          if (kcs(ndm)==1 .and. kcs(nm)==3) then
             kadv(ndm, k) = 1
             kadv(nm , k) = 0
          endif
       enddo
    enddo
end subroutine trakad_dd
