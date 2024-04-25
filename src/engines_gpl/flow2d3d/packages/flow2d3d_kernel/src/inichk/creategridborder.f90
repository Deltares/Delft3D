subroutine creategridborder(xcor, ycor, mmax, nmax, gdp)
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
! NONE
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
    integer              :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer              :: nmax   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)  :: ycor   !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                               :: ddb
    integer                               :: m
    integer                               :: n
    real(fp), allocatable, dimension(:,:) :: xc
    real(fp), allocatable, dimension(:,:) :: yc
!
!! executable statements -------------------------------------------------------
!
    allocate ( xc(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) )
    allocate ( yc(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) )
    xc = xcor
    yc = ycor 
    !
    ddb = gdp%d%ddbound
    !          
    do m = 1-ddb, mmax+ddb
       do n = 1-ddb, nmax+ddb
          if (xcor(n,m) == 0.0_fp) then
             if (m >= 1-ddb+2) then
                if (xcor(n,m-1) /= 0.0_fp .and. xcor(n,m-2) /= 0.0_fp) then
                   xc(n,m) = 2*xcor(n,m-1) - xcor(n,m-2)
                   yc(n,m) = 2*ycor(n,m-1) - ycor(n,m-2)
                endif 
             endif
             !
             if (m <= mmax+ddb-2) then 
                if (xcor(n,m+1) /= 0.0_fp .and. xcor(n,m+2) /= 0.0_fp) then
                   xc(n,m) = 2*xcor(n,m+1) - xcor(n,m+2)
                   yc(n,m) = 2*ycor(n,m+1) - ycor(n,m+2)
                endif
             endif
             !
             if (n >= 1-ddb+2) then
                if (xcor(n-1,m) /= 0.0_fp .and. xcor(n-2,m) /= 0.0_fp) then
                   xc(n,m) = 2*xcor(n-1,m) - xcor(n-2,m)
                   yc(n,m) = 2*ycor(n-1,m) - ycor(n-2,m)
                endif 
             endif
             !
             if (n <= nmax+ddb-2) then
                if (xcor(n+1,m) /= 0.0_fp .and. xcor(n+2,m) /= 0.0_fp) then
                   xc(n,m) = 2*xcor(n+1,m) - xcor(n+2,m)
                   yc(n,m) = 2*ycor(n+1,m) - ycor(n+2,m)
                endif 
             endif
          endif
       enddo
    enddo
    !
    do m = 1-ddb, mmax+ddb
       do n = 1-ddb, nmax+ddb
          if (xc(n,m) == 0.0_fp) then
             if (m >= 1-ddb+2 .and. n >= 1-ddb+2) then           ! from top right
                 if (xcor(n-1,m-1)/= 0.0_fp  .and. xcor(n-2,m-2) /= 0.0_fp) then
                    xc(n,m) = 2*xcor(n-1,m-1) - xcor(n-2,m-2)
                    yc(n,m) = 2*ycor(n-1,m-1) - ycor(n-2,m-2)
                 endif
             endif 
             !
             if (m <= mmax+ddb-2 .and. n <= nmax+ddb-2) then ! from bot left
                 if (xcor(n+1,m+1)/= 0.0_fp  .and. xcor(n+2,m+2) /= 0.0_fp) then
                    xc(n,m) = 2*xcor(n+1,m+1) - xcor(n+2,m+2)
                    yc(n,m) = 2*ycor(n+1,m+1) - ycor(n+2,m+2)
                 endif
             endif
             !
             if (m <= mmax+ddb-2 .and. n >= 1-ddb+2) then      ! from bot right  
                 if (xcor(n-1,m+1)/= 0.0_fp  .and. xcor(n-2,m+2) /= 0.0_fp) then
                    xc(n,m) = 2*xcor(n-1,m+1) - xcor(n-2,m+2)
                    yc(n,m) = 2*ycor(n-1,m+1) - ycor(n-2,m+2)
                 endif
             endif
             !
             if (m >= 1-ddb+2 .and. n <= nmax+ddb-2) then      ! from top left
                 if (xcor(n+1,m-1)/= 0.0_fp  .and. xcor(n+2,m-2) /= 0.0_fp) then
                    xc(n,m) = 2*xcor(n+1,m-1) - xcor(n+2,m-2)
                    yc(n,m) = 2*ycor(n+1,m-1) - ycor(n+2,m-2)
                 endif
             endif
          endif
       enddo
    enddo
    !
    xcor = xc
    ycor = yc
    !
    deallocate(xc,yc) 
end subroutine creategridborder
