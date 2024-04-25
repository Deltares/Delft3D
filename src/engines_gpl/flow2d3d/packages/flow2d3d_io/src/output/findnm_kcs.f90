subroutine findnm_kcs (xp    , yp    , x     , y     , mlb   , mub   , &
                     & nlb   , nub   , mmax  , nmax  , mp    , np    , &
                     & rmp   , rnp   , kcs   , inside, spher ,dearthrad)
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
!    Function: - Locate a point in a curvilinear mesh
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                              ,intent(in)    :: mmax
    integer                              ,intent(in)    :: nmax
    integer                              ,intent(in)    :: mlb       ! lower bound M grid co-ordinate
    integer                              ,intent(in)    :: mub       ! upper bound M grid co-ordinate
    integer                              ,intent(in)    :: nlb       ! lwoer bound N grid co-ordinate
    integer                              ,intent(in)    :: nub       ! upper bound N grid co-ordinate
    integer                              ,intent(inout) :: mp        ! M co-ordinate grid cell with location inside
    integer                              ,intent(inout) :: np        ! N co-ordinate grid cell with location inside
    integer , dimension(nlb:nub,mlb:mub) ,intent(in)    :: kcs       ! Active cell KCS=1
    real(fp), dimension(nlb:nub,mlb:mub) ,intent(in)    :: x         ! X co-ordinates depth points overall model
    real(fp), dimension(nlb:nub,mlb:mub) ,intent(in)    :: y         ! Y co-ordinates depth points overall model
    real(fp)                             ,intent(in)    :: xp        ! X co-ordinate of specified location
    real(fp)                             ,intent(in)    :: yp        ! Y co-ordinate of specified location
    real(fp)                             ,intent(out)   :: rmp       ! relative M co-ordinate of spec location
    real(fp)                             ,intent(out)   :: rnp       ! relative N co-ordinate of spec location
    logical                              ,intent(inout) :: inside    ! true if spec location lies within grid
    logical                              ,intent(in)    :: spher     ! true if spherical co-ordinates
    real(hp)                             ,intent(in)    :: dearthrad ! Earth radius
!
! Local variables
!
    integer                :: loop
    integer                :: mb    ! Begin M index
    integer                :: me    ! End M index
    integer                :: nb    ! Begin N index
    integer                :: ne    ! End N index
    integer                :: m     ! Loop variable M index
    integer                :: n     ! Loop variable N index
    real(fp), dimension(4) :: xx    ! X co-ordinates of grid cell
    real(fp), dimension(4) :: yy    ! Y co-ordinates of grid cell
    real(fp)               :: a     ! Local distance variable for computation of rmp,rnp
    real(fp)               :: b     ! Local distance variable for computation of rmp,rnp
    real(fp)               :: c     ! Local distance variable for computation of rmp,rnp
!
!! executable statements -------------------------------------------------------
!
    outerloop: do loop = 1,2
       !
       ! initialisation
       !
       if (loop==1) then
          !
          ! if np,mp contain previous cell index of point, then first check all cells
          ! adjacent to this cell
          !
          if (.not.inside) cycle
          mb = max(2,mp-1)
          me = min(mp+1,mmax-1)
          nb = max(2,np-1)
          ne = min(np+1,nmax-1)
       else
          !
          ! if not found, then check all grid cells
          !
          mb = 2
          me = mmax-1
          nb = 2
          ne = nmax-1
       endif
       !
       ! check all points within the selected block
       !
       do m = mb, me
          do n = nb, ne
             !
             ! continue if cell is active
             !
             if (kcs(n,m) == 1) then
                !
                ! set corner points
                !
                xx (1) = x (n-1,m-1)
                yy (1) = y (n-1,m-1)
                xx (2) = x (n-1,m  )
                yy (2) = y (n-1,m  )
                xx (3) = x (n  ,m  )
                yy (3) = y (n  ,m  )
                xx (4) = x (n  ,m-1)
                yy (4) = y (n  ,m-1)
                !
                ! specified location in grid cell ?
                !
                call pinpol (xp    , yp    , 4    , xx   , yy   , inside)
                !
                ! if inside set M,N co-ordinates and compute relative M,N
                !
                if (inside) then
                   mp  = m
                   np  = n
                   !
                   call  distance(spher ,xx(1),yy(1),xx(2),yy(2),a,dearthrad)
                   call  distance(spher ,xx(1),yy(1),xp   ,yp   ,b,dearthrad)
                   call  distance(spher ,xx(2),yy(2),xp   ,yp   ,c,dearthrad)
                   rmp = 0.5 + (b*b - c*c)/(2*a*a)
                   !
                   call  distance(spher ,xx(1),yy(1),xx(4),yy(4),a,dearthrad)
                   call  distance(spher ,xx(4),yy(4),xp   ,yp   ,c,dearthrad)
                   rnp = 0.5 + (b*b - c*c)/(2*a*a)
                   exit outerloop
                endif
             endif
          enddo
       enddo
    enddo outerloop
end subroutine findnm_kcs
