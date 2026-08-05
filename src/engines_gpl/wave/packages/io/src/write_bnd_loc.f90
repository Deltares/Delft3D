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
          if (valid_bnd_point(xc(i,1), yc(i,1), kcs(i,1), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xc(i,1), yc(i,1)
       enddo
       do j=2,nc
          if (valid_bnd_point(xc(mc,j), yc(mc,j), kcs(mc,j), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xc(mc,j), yc(mc,j)
       enddo
       do i=mc-1,1,-1
          if (valid_bnd_point(xc(i,nc), yc(i,nc), kcs(i,nc), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xc(i,nc), yc(i,nc)
       enddo
       do j=nc-1,2,-1
          if (valid_bnd_point(xc(1,j), yc(1,j), kcs(1,j), xymiss)) &
             write(lunbot,'(2(F15.6,3X))') xc(1,j), yc(1,j)
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
end subroutine write_bnd
