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
    real(kind=hp)   , dimension(mc, nc)     :: xc
    real(kind=hp)   , dimension(mc, nc)     :: yc
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
    real(kind=hp), parameter :: BOUNDARY_OFFSET_FRACTION = 0.001_hp
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
          if (i == 1) then
             call write_bnd_point(xc(i,1), yc(i,1), kcs(i,1), &
                                  xc(1,2), yc(1,2), kcs(1,2), &
                                  xc(2,1), yc(2,1), kcs(2,1))
          else if (i == mc) then
             call write_bnd_point(xc(i,1), yc(i,1), kcs(i,1), &
                                  xc(i-1,1), yc(i-1,1), kcs(i-1,1), &
                                  xc(mc,2), yc(mc,2), kcs(mc,2))
          else
             call write_bnd_point(xc(i,1), yc(i,1), kcs(i,1), &
                                  xc(i-1,1), yc(i-1,1), kcs(i-1,1), &
                                  xc(i+1,1), yc(i+1,1), kcs(i+1,1))
          endif
       enddo
       do j=2,nc
          if (j == 2) then
             call write_bnd_point(xc(mc,j), yc(mc,j), kcs(mc,j), &
                                  xc(mc-1,1), yc(mc-1,1), kcs(mc-1,1), &
                                  xc(mc,3), yc(mc,3), kcs(mc,3))
          else if (j == nc) then
             call write_bnd_point(xc(mc,j), yc(mc,j), kcs(mc,j), &
                                  xc(mc,j-1), yc(mc,j-1), kcs(mc,j-1), &
                                  xc(mc-1,nc), yc(mc-1,nc), kcs(mc-1,nc))
          else
             call write_bnd_point(xc(mc,j), yc(mc,j), kcs(mc,j), &
                                  xc(mc,j-1), yc(mc,j-1), kcs(mc,j-1), &
                                  xc(mc,j+1), yc(mc,j+1), kcs(mc,j+1))
          endif
       enddo
       do i=mc-1,1,-1
          if (i == mc-1) then
             call write_bnd_point(xc(i,nc), yc(i,nc), kcs(i,nc), &
                                  xc(mc,nc-1), yc(mc,nc-1), kcs(mc,nc-1), &
                                  xc(mc-2,nc), yc(mc-2,nc), kcs(mc-2,nc))
          else if (i == 1) then
             call write_bnd_point(xc(i,nc), yc(i,nc), kcs(i,nc), &
                                  xc(i+1,nc), yc(i+1,nc), kcs(i+1,nc), &
                                  xc(1,nc-1), yc(1,nc-1), kcs(1,nc-1))
          else
             call write_bnd_point(xc(i,nc), yc(i,nc), kcs(i,nc), &
                                  xc(i+1,nc), yc(i+1,nc), kcs(i+1,nc), &
                                  xc(i-1,nc), yc(i-1,nc), kcs(i-1,nc))
          endif
       enddo
       do j=nc-1,2,-1
          if (j == nc-1) then
             call write_bnd_point(xc(1,j), yc(1,j), kcs(1,j), &
                                  xc(1,nc), yc(1,nc), kcs(1,nc), &
                                  xc(1,nc-2), yc(1,nc-2), kcs(1,nc-2))
          else if (j == 2) then
             call write_bnd_point(xc(1,j), yc(1,j), kcs(1,j), &
                                  xc(1,j+1), yc(1,j+1), kcs(1,j+1), &
                                  xc(1,1), yc(1,1), kcs(1,1))
          else
             call write_bnd_point(xc(1,j), yc(1,j), kcs(1,j), &
                                  xc(1,j+1), yc(1,j+1), kcs(1,j+1), &
                                  xc(1,j-1), yc(1,j-1), kcs(1,j-1))
          endif
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

    subroutine write_bnd_point(x, y, mask, xprevious, yprevious, previous_mask, &
                               xnext, ynext, next_mask)
        real(kind=hp), intent(in) :: x
        real(kind=hp), intent(in) :: y
        integer, intent(in) :: mask
        real(kind=hp), intent(in)    :: xprevious
        real(kind=hp), intent(in)    :: yprevious
        integer, intent(in) :: previous_mask
        real(kind=hp), intent(in)    :: xnext
        real(kind=hp), intent(in)    :: ynext
        integer, intent(in) :: next_mask
        real(kind=hp) :: xout
        real(kind=hp) :: yout

        if (.not. valid_bnd_point(x, y, mask, xymiss)) return

        xout = x
        yout = y
        ! Coordinates at inactive points are undefined. Use the other local
        ! edge when the predecessor is invalid, so the point remains offset
        ! without using undefined coordinates.
        if (valid_bnd_point(xprevious, yprevious, previous_mask, xymiss)) then
            xout = xout + BOUNDARY_OFFSET_FRACTION * (xout - xprevious)
            yout = yout + BOUNDARY_OFFSET_FRACTION * (yout - yprevious)
        else if (valid_bnd_point(xnext, ynext, next_mask, xymiss)) then
            xout = xout + BOUNDARY_OFFSET_FRACTION * (xnext - xout)
            yout = yout + BOUNDARY_OFFSET_FRACTION * (ynext - yout)
        endif
        write(lunbot,'(2(F15.6,3X))') xout, yout
    end subroutine write_bnd_point
end subroutine write_bnd
