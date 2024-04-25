subroutine dfstrip ( ipown, jpart, npart, iwork, mmax, nmax )
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
!   Performs a stripwise partitioning with straight interfaces
!
!!--pseudo code and references--------------------------------------------------
!
!   create first empty part
!   for all active points do
!       assign this point to the created part
!       if size of created part has been reached
!          determine remaining active points in the current column
!          if no remaining points, create next empty part
!          else remaining points belong to the current part
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    !
    implicit none
!
! Global variables
!
!    integer, intent(in)                          :: idir  ! direction of cutting:
                                                          !    1 = row
                                                          !    2 = column
    integer, intent(in)                          :: jpart ! part number that must be partitioned
    integer, intent(in)                          :: mmax  ! number of gridpoints in the x-direction
    integer, intent(in)                          :: nmax  ! number of gridpoints in the y-direction
    integer, intent(in)                          :: npart ! number of parts to be created
    !
    integer, dimension(mmax*nmax), intent(inout) :: ipown ! array giving the subdomain number of each gridpoint
    integer, dimension(2,nproc)  , intent(inout) :: iwork ! work array with the following meaning:
                                                          !    iwork(1,i) = number of i-th part to be created
                                                          !    iwork(2,i) = size of i-th part to be created
!
! Local variables
!
    integer :: ic     ! index of (M,N)-point
    integer :: icc    ! index of (M,N)-point
    integer :: icx    ! increment for addressing: 1 for x-dir, MMAX for y-dir
    integer :: icy    ! increment for addressing: MMAX for x-dir, 1 for y-dir
    integer :: ipart  ! part counter
    integer :: m      ! current M-index of point in computational row
    integer :: mmaxi  ! number of gridpoints in x/y-direction used for indirect addressing
    integer :: n      ! current N-index of point in computational column
    integer :: nmaxi  ! number of gridpoints in x/y-direction used for indirect addressing
    integer :: nn     ! current N-index of point in computational column
    integer :: ncurpt ! number of currently assigned points to a created part
    integer :: nprem  ! number of remaining points in a row/column
!
!! executable statements -------------------------------------------------------
!
    ! depending on cutting direction, determine indirect addressing for array ipown
    !
    if ( idir == 1 ) then
       mmaxi = nmax
       nmaxi = mmax
       icx   = mmax
       icy   = 1
    elseif ( idir == 2 ) then
       mmaxi = mmax
       nmaxi = nmax
       icx   = 1
       icy   = mmax
    endif
    !
    ! create first empty part
    !
    ipart  = 1
    ncurpt = 0
    !
    ! for all active points do
    !
    do m = 1, mmaxi
       do n = 1, nmaxi

          ic = m*icx + n*icy - mmax

          if ( ipown(ic) == jpart ) then
             !
             ! assign this point to the created part
             !
             ipown(ic) = iwork(1,ipart)
             ncurpt    = ncurpt + 1
             !
             ! if size of created part has been reached
             !
             if ( ncurpt >= iwork(2,ipart) ) then
                !
                ! determine remaining active points in the current column
                !
                nprem = 0
                do nn = n+1, nmaxi
                   icc = m*icx + nn*icy - mmax
                   if (ipown(icc) == jpart) nprem = nprem +1
                enddo

                if ( nprem == 0 ) then
                   !
                   ! if no remaining points, create next empty part
                   !
                   ipart  = ipart + 1
                   ncurpt = 0
                else
                   !
                   ! else remaining points belong to the current part
                   !
                   iwork(2,ipart  ) = iwork(2,ipart  ) + nprem
                   iwork(2,ipart+1) = iwork(2,ipart+1) - nprem
                endif

             endif

          endif

       enddo
    enddo

end subroutine dfstrip
