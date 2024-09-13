subroutine ghosttype_is_kf(kfs,kfu,kfv,mmax,nmax,nmaxus,kmax,nlb,nub,mlb,mub,nmlb,nmub, gdp) 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   Interpolate the values at the Image points
!
!   NOTE: Vandermonde matrices are notoriously ill-conditioned. The inverses of large floating-point
! Vandermonde matrices are subject to severe round-off effects.fROM http://www.mathworks.com/help/symbolic/mupad_ref/linalg-invvandermonde.html
! 
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, dimension(:,:), pointer :: GHOSTs1
    integer, dimension(:,:), pointer :: GHOSTu1
    integer, dimension(:,:), pointer :: GHOSTv1
!
! global variables
!
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfv 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
!
! local variables
!
  integer                    :: i
  integer                    :: m
  integer                    :: n
!
! executable statements -------------------------------------------------------
!
    GHOSTs1 => gdp%gdimbound%GHOSTs1
    GHOSTu1 => gdp%gdimbound%GHOSTu1
    GHOSTv1 => gdp%gdimbound%GHOSTv1
    DO m=1,mmax
       DO n=1,nmaxus
!
          if (GHOSTs1(n,m).eq.0) then !wet point
             kfs(n,m) = 1 
          elseif (GHOSTs1(n,m).ge.1) then !1:ghost point.   2: dry non-ghost
             kfs(n,m) = 0
          endif
!
          if (GHOSTu1(n,m).eq.0) then !wet point
             kfu(n,m) = 1 
          elseif (GHOSTu1(n,m).ge.1) then !1:ghost point.   2: dry non-ghost
             kfu(n,m) = 0
          endif
!
          if (GHOSTv1(n,m).eq.0) then !wet point
             kfv(n,m) = 1 
          elseif (GHOSTv1(n,m).ge.1) then !1:ghost point.   2: dry non-ghost
             kfv(n,m) = 0
          endif
!
       ENDDO
    ENDDO
!
RETURN
END
!
! 
