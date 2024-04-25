subroutine xzyz_dd(xz        ,yz        ,kcs       ,nmax      ,mmax      ,&
                 & nmaxus    ,xcor      ,ycor      ,gdp                  )
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
!    Function: - Computes coordinates of the couple (water level) points
!                (to be used by the wave computation/interpolation)
!
! Determine coordinates of mirror points
! (identical to inigeo for open boundaries, with one modification
!  multiplication with 2.0 and division by 2.0 is skipped.
!  This should also be skipped in inigeo.)
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
    integer                                                                    :: mmax
    integer                                                                    :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: ycor   !  Description and declaration in esm_alloc_real.f90
!        
! Local variables
!        
    integer :: m
    integer :: n
    integer :: md
    integer :: mu
    integer :: nd
    integer :: nu
!
!! executable statements -------------------------------------------------------
!
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcs (n,m) == 3) then
             nd  = n - 1
             nu  = n + 1
             md =  m - 1
             mu =  m + 1
             if (kcs(nd, m)==1 .and. kcs(nu, m)==0) then
                !
                ! Upper boundary
                !
                xz(n, m) = (xcor(nd, m) + xcor(nd, md)) - xz(nd, m)
                yz(n, m) = (ycor(nd, m) + ycor(nd, md)) - yz(nd, m)
             elseif (kcs(nd, m)==0 .and. kcs(nu, m)==1) then
                !
                ! Lower boundary
                !
                xz(n, m) = (xcor(n, m) + xcor(n, md)) - xz(nu, m)
                yz(n, m) = (ycor(n, m) + ycor(n, md)) - yz(nu, m)
             elseif (kcs(n, md)==1 .and. kcs(n, mu)==0) then
                !
                ! Right boundary
                !
                xz(n, m) = (xcor(n, md) + xcor(nd, md)) - xz(n, md)
                yz(n, m) = (ycor(n, md) + ycor(nd, md)) - yz(n, md)
             elseif (kcs(n, md)==0 .and. kcs(n, mu)==1) then
                !
                ! Left boundary
                !
                xz(n, m) = (xcor(n, m) + xcor(nd, m)) - xz(n, mu)
                yz(n, m) = (ycor(n, m) + ycor(nd, m)) - yz(n, mu)
             !
             ! Inner corners; 4 possible orientations
             ! Each orientation has two choices for implementation
             ! Choose a useful one (allow kcs to be zero at one of the two needed sides)
             !
             elseif (kcs(n, mu)==3 .and. kcs(nd, m)==3) then
                !
                ! Upper left inner corner
                !
                if (kcs(n, md) == 1) then
                   !
                   ! right boundary is choosen
                   !
                   xz(n, m) = (xcor(n, md) + xcor(nd, md)) - xz(n, md)
                   yz(n, m) = (ycor(n, md) + ycor(nd, md)) - yz(n, md)
                elseif (kcs(nu, m) == 1) then
                   !
                   ! lower boundary is choosen
                   !
                   xz(n, m) = (xcor(n, m) + xcor(n, md)) - xz(nu, m)
                   yz(n, m) = (ycor(n, m) + ycor(n, md)) - yz(nu, m)
                endif
             elseif (kcs(n, md)==3 .and. kcs(nd, m)==3) then
                !
                ! Upper right corner
                ! left boundary is choosen
                !
                if (kcs(n, mu) == 1) then
                   !
                   ! left boundary is choosen
                   !
                   xz(n, m) = (xcor(n, m) + xcor(nd, m)) - xz(n, mu)
                   yz(n, m) = (ycor(n, m) + ycor(nd, m)) - yz(n, mu)
                elseif (kcs(nu, m) == 1) then
                   !
                   ! lower boundary is choosen
                   !
                   xz(n, m) = (xcor(n, m) + xcor(n, md)) - xz(nu, m)
                   yz(n, m) = (ycor(n, m) + ycor(n, md)) - yz(nu, m)
                endif
             elseif (kcs(n, mu)==3 .and. kcs(nu, m)==3) then
                !
                ! Lower left corner
                !
                if (kcs(n, md) == 1) then
                   !
                   ! right boundary is choosen
                   !
                   xz(n, m) = (xcor(n, md) + xcor(nd, md)) - xz(n, md)
                   yz(n, m) = (ycor(n, md) + ycor(nd, md)) - yz(n, md)
                elseif (kcs(nd, m) == 1) then
                   !
                   ! upper boundary is choosen
                   !
                   xz(n, m) = (xcor(nd, m) + xcor(nd, md)) - xz(nd, m)
                   yz(n, m) = (ycor(nd, m) + ycor(nd, md)) - yz(nd, m)
                endif
             elseif (kcs(n, md)==3 .and. kcs(nu, m)==3) then
                !
                ! Lower right corner
                !
                if (kcs(n, mu) == 1) then
                   !
                   ! left boundary is choosen
                   !
                   xz(n, m) = (xcor(n, m) + xcor(nd, m)) - xz(n, mu)
                   yz(n, m) = (ycor(n, m) + ycor(nd, m)) - yz(n, mu)
                elseif (kcs(nd, m) == 1) then
                   !
                   ! upper boundary is choosen
                   !
                   xz(n, m) = (xcor(nd, m) + xcor(nd, md)) - xz(nd, m)
                   yz(n, m) = (ycor(nd, m) + ycor(nd, md)) - yz(nd, m)
                endif
             endif
          endif
       enddo
    enddo
end subroutine xzyz_dd
