subroutine write_bnd_loc (inest,sg)
!
! Head routine for calling write_bnd
!
use swan_flow_grid_maps
use read_grids
implicit none
   type (grid) :: sg       ! actual swan grid
   integer     :: inest    ! swan nested grid no.
   call write_bnd(sg%x        ,sg%y       ,sg%kcs     ,sg%xymiss  ,sg%mmax   ,sg%nmax   , &
                & inest      )
   call write_swan_grid (sg%x,sg%y,sg%mmax,sg%nmax,inest,sg%tmp_name)
end subroutine write_bnd_loc


subroutine write_bnd(xc        ,yc        ,kcs       ,xymiss    ,mc        ,nc        , &
                & inest      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer                   , intent(in)  :: inest
    integer                   , intent(in)  :: mc
    integer                   , intent(in)  :: nc
    integer                   , dimension(mc, nc), intent(in) :: kcs
    real(kind=hp), dimension(mc, nc), intent(in) :: xc
    real(kind=hp), dimension(mc, nc), intent(in) :: yc
    real(kind=hp)   , intent(in)            :: xymiss
!
! Local variables
!
    integer           :: i
    integer           :: j
    integer           :: lunbot
    character(37)     :: fname
    real(kind=hp), parameter :: TOL = 1.0e-6_hp
    ! Move each location by a small fraction of its incoming edge. This makes
    ! every child boundary point belong to one nesting segment only; SWAN can
    ! otherwise see a point at the end of both adjacent segments when
    ! BOUN NEST is CLOSED (most visibly at corners and local edge kinks).
    real(kind=hp), parameter :: CORNER_OFFSET = 0.001_hp
    real(kind=hp)           :: xout
    real(kind=hp)           :: yout
!
!! executable statements -------------------------------------------------------
!
    if (inest>1) then
       fname      = ' '
       fname(1:12) = 'SWANIN_NGRID'
       write (fname(13:15),'(I3.3)') inest
       open (newunit=lunbot, file=fname(1:15))
       ! Exclude inactive points and coordinate pairs equal to the declared
       ! grid missing value. Do not pass those points to SWAN as nesting
       ! locations.
       do i=1,mc
          xout = xc(i,1)
          yout = yc(i,1)
          if (i == 1) then
             call offset_corner(xc(1,2), yc(1,2), xout, yout)
          else
             call offset_corner(xc(i-1,1), yc(i-1,1), xout, yout)
          endif
          if (valid_bnd_point(xout, yout, kcs(i,1), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xout, yout
       enddo
       do j=2,nc
          xout = xc(mc,j)
          yout = yc(mc,j)
          if (j == 2) then
             call offset_corner(xc(mc-1,1), yc(mc-1,1), xout, yout)
          else
             call offset_corner(xc(mc,j-1), yc(mc,j-1), xout, yout)
          endif
          if (valid_bnd_point(xout, yout, kcs(mc,j), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xout, yout
       enddo
       do i=mc-1,1,-1
          xout = xc(i,nc)
          yout = yc(i,nc)
          if (i == mc-1) then
             call offset_corner(xc(mc,nc-1), yc(mc,nc-1), xout, yout)
          else
             call offset_corner(xc(i+1,nc), yc(i+1,nc), xout, yout)
          endif
          if (valid_bnd_point(xout, yout, kcs(i,nc), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xout, yout
       enddo
       do j=nc-1,2,-1
          xout = xc(1,j)
          yout = yc(1,j)
          if (j == nc-1) then
             call offset_corner(xc(1,nc), yc(1,nc), xout, yout)
          else
             call offset_corner(xc(1,j+1), yc(1,j+1), xout, yout)
          endif
          if (valid_bnd_point(xout, yout, kcs(1,j), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xout, yout
       enddo
       close(lunbot)
    endif
contains
    logical function valid_bnd_point(x, y, mask, missing)
        real(kind=hp), intent(in) :: x
        real(kind=hp), intent(in) :: y
        real(kind=hp), intent(in) :: missing
        integer, intent(in) :: mask

        valid_bnd_point = mask > 0 .and. &
            .not. (abs(x - missing) < TOL .and. abs(y - missing) < TOL)
    end function valid_bnd_point

    subroutine offset_corner(xprevious, yprevious, xcorner, ycorner)
        real(kind=hp), intent(in)    :: xprevious
        real(kind=hp), intent(in)    :: yprevious
        real(kind=hp), intent(inout) :: xcorner
        real(kind=hp), intent(inout) :: ycorner
        xcorner = xcorner + CORNER_OFFSET * (xcorner - xprevious)
        ycorner = ycorner + CORNER_OFFSET * (ycorner - yprevious)
    end subroutine offset_corner
end subroutine write_bnd
