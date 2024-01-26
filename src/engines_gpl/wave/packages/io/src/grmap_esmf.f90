subroutine grmap_esmf(i1, f1, n1, f2, mmax, nmax, f2s, f2g)
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
! http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/node3.html
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use swan_flow_grid_maps
    use mathconsts
    !
    implicit none
!
! Global variables
!
    integer                   , intent(in)  :: i1
    integer                   , intent(in)  :: n1
    integer                   , intent(in)  :: mmax
    integer                   , intent(in)  :: nmax
    real   , dimension(n1)    , intent(in)  :: f1
    real   , dimension(mmax,nmax)           :: f2
    type(grid_map)            , intent(in)  :: f2s
    type(grid)                              :: f2g  ! f2 grid
!
! Local variables
!
    integer :: i
    integer :: j
    integer :: n
    integer :: n2
!
!! executable statements -------------------------------------------------------
!
    if (.not.f2s%grids_linked) return
    n2 = mmax * nmax
    !
    ! f2 already contains valid data
    ! Only replace f2 values when covered by f1
    ! Start with setting f2=0 for covered points only
    !
    do i=1, mmax
       do j=1, nmax
          if (f2g%covered(i,j) == i1) then
             f2(i,j) = 0.0_sp
          endif
       enddo
    enddo
    !
    if (f2s%sferic) then
       do n=1, f2s%n_s
          i       = floor(real(f2s%row(n)-1)/real(nmax)) + 1
          j       = f2s%row(n) - nmax*(i-1)
          if (f2g%covered(i,j) == i1) then
             f2(i,j) = f2(i,j) + f2s%s(n)*f1(f2s%col(n))
          endif
          !
          ! The following method (without conversion to i,j) does not work:
          ! f2(f2s%row(n)) = f2(f2s%row(n)) + f2s%s(n)*f1(f2s%col(n))
          ! Probably because f2 is mapped different when passed through as 1D array
          !
       enddo
    else
       do n=1, f2s%n_s
          j       = floor(real(f2s%row(n)-1)/real(mmax)) + 1
          i       = f2s%row(n) - mmax*(j-1)   
          !
          if (f2g%covered(i,j)== i1) then
             f2(i,j) = f2(i,j) + f2s%s(n)*f1(f2s%col(n))    ! generate_partioning_pol_from_idomain
          endif
       enddo
    endif
end subroutine grmap_esmf
