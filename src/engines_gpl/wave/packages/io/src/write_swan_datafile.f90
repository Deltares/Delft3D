module write_swan_datafile
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
!!--module description----------------------------------------------------------
!
!!--module declarations---------------------------------------------------------
public write_swan_file

    interface write_swan_file
       module procedure write_swan_file1
       module procedure write_swan_file2
    end interface

contains

subroutine extrapolate(mmax, nmax, covered, var1, var2)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer     , parameter :: iindex = 1
    integer     , parameter :: jindex = 2
!
! Global variables
!
    integer                       , intent(in)               :: mmax
    integer                       , intent(in)               :: nmax
    integer, dimension(mmax, nmax), intent(in)               :: covered
    real   , dimension(mmax, nmax), intent(inout)            :: var1
    real   , dimension(mmax, nmax), intent(inout), optional  :: var2
!
! Local variables
!
    integer, dimension(:,:,:), allocatable :: closestPoint
    integer                                :: dm
    integer                                :: i
    integer                                :: j
    integer                                :: i1
    integer                                :: j1
    integer                                :: ierr
    integer                                :: sweep
    integer                                :: sweepEnd
    integer                                :: sweepStart
    integer                                :: sweepStep
    real   , dimension(:,:)  , allocatable :: cpDistance
    real                                   :: distance
!
!! executable statements -------------------------------------------------------
!
    allocate(closestPoint(mmax, nmax, max(iindex,jindex)))
    allocate(cpDistance  (mmax, nmax))
    do j=1,nmax
       do i=1,mmax
          if (covered(i, j) == 0) then
             !
             ! point is outside all FLOW grids:
             ! - closest point is not defined (zero)
             ! - distance to closest point is infinity
             !
             closestPoint(i, j, iindex) = 0
             closestPoint(i, j, jindex) = 0
             cpDistance  (i, j)         = 1.0e20
          else
             !
             ! point is inside a FLOW grid:
             ! - closest point is the current point itself
             ! - distance to closest point is zero
             !
             closestPoint(i, j, iindex) = i
             closestPoint(i, j, jindex) = j
             cpDistance  (i, j)         = 0.0
          endif
       enddo
    enddo
    !
    ! Detect closest covered point for all not covered points
    !
    do sweep = 1, 2
       if (sweep == 1) then
          !
          ! From top to bottom
          !
          sweepStart =  1
          sweepEnd   = nmax
          sweepStep  =  1
       else
          !
          ! From bottom to top
          !
          sweepStart = nmax
          sweepEnd   =  1
          sweepStep  = -1
       endif
       do j = sweepStart, sweepEnd, sweepStep
          do i = 1, mmax
             if (covered(i, j) == 0) then
                !
                ! point is outside all FLOW grids
                !
                do dm = -1, 1
                   i1 = i + dm
                   j1 = j - sweepStep
                   if (      i1>=1 .and. i1<=mmax &
                     & .and. j1>=1 .and. j1<=nmax  ) then
                      if (closestPoint(i1, j1, 1)>0) then
                         distance =   (i - closestPoint(i1, j1, iindex))**2 &
                                  & + (j - closestPoint(i1, j1, jindex))**2
                         if (distance < cpDistance(i, j)) then
                            closestPoint(i, j, iindex) = closestPoint(i1, j1, iindex)
                            closestPoint(i, j, jindex) = closestPoint(i1, j1, jindex)
                            cpDistance  (i, j)         = distance
                         endif
                      endif
                   endif
                enddo
                i1 = i - 1
                j1 = j
                if (      i1>=1 .and. i1<=mmax &
                  & .and. j1>=1 .and. j1<=nmax  ) then
                   if (closestPoint(i1, j1, 1)>0) then
                      distance =   (i - closestPoint(i1, j1, iindex))**2 &
                               & + (j - closestPoint(i1, j1, jindex))**2
                      if (distance < cpDistance(i, j)) then
                         closestPoint(i, j, iindex) = closestPoint(i1, j1, iindex)
                         closestPoint(i, j, jindex) = closestPoint(i1, j1, jindex)
                         cpDistance  (i, j)         = distance
                      endif
                   endif
                endif
             endif
          enddo
          do i = mmax, 1, -1
             if (covered(i, j) == 0) then
                !
                ! point is outside all FLOW grids
                !
                i1 = i + 1
                j1 = j
                if (      i1>=1 .and. i1<=mmax &
                  & .and. j1>=1 .and. j1<=nmax  ) then
                   if (closestPoint(i1, j1, 1)>0) then
                      distance =   (i - closestPoint(i1, j1, iindex))**2 &
                               & + (j - closestPoint(i1, j1, jindex))**2
                      if (distance < cpDistance(i, j)) then
                         closestPoint(i, j, iindex) = closestPoint(i1, j1, iindex)
                         closestPoint(i, j, jindex) = closestPoint(i1, j1, jindex)
                         cpDistance  (i, j)         = distance
                      endif
                   endif
                endif
             endif
          enddo
       enddo
    enddo
    do j=1,nmax
       do i=1,mmax
          !
          ! Only extrapolate from closestPoints, covered by valid points from source grid
          ! If the closestPoint is covered by invalid source grid points
          ! then covered == -1 and the value at i,j is not changed
          !
          if (covered(i, j) == 0 .and. closestPoint(i, j, iindex)>0) then
             if (covered(closestPoint(i, j, iindex), closestPoint(i, j, jindex)) > 0) then    ! multi-domain: covered can be greater than 1
                var1(i, j) = var1(closestPoint(i, j, iindex), closestPoint(i, j, jindex))
             endif
          endif
       enddo
    enddo
    if (present(var2)) then
       do j=1,nmax
          do i=1,mmax
             !
             ! Only extrapolate from closestPoints, covered by valid points from source grid
             ! If the closestPoint is covered by invalid source grid points
             ! then covered == -1 and the value at i,j is not changed
             !
             if (covered(i, j) == 0 .and. closestPoint(i, j, iindex)>0) then
                if (covered(closestPoint(i, j, iindex), closestPoint(i, j, jindex)) > 0) then
                   var2(i, j) = var2(closestPoint(i, j, iindex), closestPoint(i, j, jindex))
                endif
             endif
          enddo
       enddo
    endif
    deallocate(closestPoint, stat=ierr)
    deallocate(cpDistance  , stat=ierr)
end subroutine extrapolate


subroutine write_swan_file1 (var1  ,mmax   , nmax, covered, &
                           & filnam, extr_var1, minval)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    integer, dimension(mmax, nmax), intent(in)  :: covered
    real   , optional             , intent(in)  :: minval
    real   , dimension(mmax, nmax)              :: var1
    logical                       , intent(in)  :: extr_var1
    character(*)                  , intent(in)  :: filnam
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    integer                                :: lunfil
!
!! executable statements -------------------------------------------------------
!
    if (extr_var1) then
       call extrapolate(mmax, nmax, covered, var1)
    endif
    !
    ! write var1 and var2 to file for SWAN
    !
    open (newunit = lunfil, file = filnam, status = 'unknown')
    !
    ! Up to now, SWAN data files are produced for writing
    ! var1 and var2 (current, wind) or var1+var2 (bottom)
    ! logical sumvars is used to distinct these two cases
    !
    if (present(minval)) then
       write (lunfil,'(4(3X,E13.6))') ( ( max(var1(i,j),minval), i=1,mmax), j=1,nmax )
    else
       write (lunfil,'(4(3X,E13.6))') ( (var1(i,j), i=1,mmax), j=1,nmax )
    endif
    close (lunfil)
end subroutine write_swan_file1


subroutine write_swan_file2 (var1  , var2       , mmax   , nmax, covered, &
                           & filnam, extr_var1, extr_var2, &
                           & sumvars, minval)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    integer, dimension(mmax, nmax), intent(in)  :: covered
    real   , optional             , intent(in)  :: minval
    real   , dimension(mmax, nmax)              :: var1
    real   , dimension(mmax, nmax)              :: var2
    logical                       , intent(in)  :: extr_var1
    logical                       , intent(in)  :: extr_var2
    logical                       , intent(in)  :: sumvars
    character(*)                  , intent(in)  :: filnam
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    integer                                :: lunfil
!
!! executable statements -------------------------------------------------------
!
    if (extr_var1 .and. extr_var2) then
       call extrapolate(mmax, nmax, covered, var1, var2)
    elseif (extr_var1) then
       call extrapolate(mmax, nmax, covered, var1)
    elseif (extr_var2) then
       call extrapolate(mmax, nmax, covered, var2)
    endif
    !
    ! write var1 and var2 to file for SWAN
    !
    open (newunit = lunfil, file = filnam, status = 'unknown')
    !
    ! Up to now, SWAN data files are produced for writing
    ! var1 and var2 (current, wind) or var1+var2 (bottom)
    ! logical sumvars is used to distinct these two cases
    !
    if (sumvars) then
       if (present(minval)) then
          write (lunfil,'(4(3X,E13.6))') ( ( max(var1(i,j)+var2(i,j),minval), i=1,mmax), j=1,nmax )
       else
          write (lunfil,'(4(3X,E13.6))') ( (var1(i,j)+var2(i,j), i=1,mmax), j=1,nmax )
       endif
    else
       if (present(minval)) then
          write (lunfil,'(4(3X,E13.6))') ( ( max(var1(i,j),minval), i=1,mmax), j=1,nmax )
          write (lunfil,'(4(3X,E13.6))') ( ( max(var2(i,j),minval), i=1,mmax), j=1,nmax )
       else
          write (lunfil,'(4(3X,E13.6))') ( (var1(i,j), i=1,mmax), j=1,nmax )
          write (lunfil,'(4(3X,E13.6))') ( (var2(i,j), i=1,mmax), j=1,nmax )
       endif
    endif
    close (lunfil)
end subroutine write_swan_file2

end module write_swan_datafile
