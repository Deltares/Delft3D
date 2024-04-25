subroutine dforb ( ipown, npart, iwork, mmax, nmax, gdp )
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
!    Function: Performs an Orthogonal Recursive Bisection partitioning
!
! Method used: Starting with a single part (the entire domain), each part is
!              recursively partitioned by bisecting it, until all parts have been
!              created. The bisection direction is swapped in each direction.
!
!!--pseudo code and references--------------------------------------------------
!
!   while not enough parts have been created, do another recursion
!
!      for each part that currently exists
!
!        determine which final parts belong to this part
!        if the number of such parts > 1, do further splitting
!
!          determine into how many parts this part must be split
!          determine the sizes and numbers of parts to be created
!
!          do splitting
!
!          determine whether objective partsizes have been modified
!          and distribute the difference over the constituent parts
!
!      swap cutting direction
!
!   M. Roest, Partitioning for parallel finite difference computations
!   in coastal water simulation, PhD. Thesis, DUT, 1997
!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Local parameters
!
    integer, parameter :: nsplit = 2 ! maximum number of parts to be created in one splitting:
                                     !    2 = bisection
                                     !    4 = quadrisection
!
! Global variables
!
!    integer, intent(inout)                       :: idir  ! direction of cutting:
!                                                          !    1 = row
!                                                          !    2 = column
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
    type(dfparalltype), pointer :: gdparall
    integer, pointer :: lundia
    integer :: idiff                     ! the difference to be applied to a subdomain-size
    integer :: ip                        ! counter of parts to be splitted
    integer :: isplit                    ! counter of parts to be created in splitting
    integer :: issucc                    ! flag indicating success in reducing a difference in size
                                         ! 0=no
                                         ! 1=yes
    integer, dimension(2,npart) :: itemp ! see description of array IWORK
    integer :: j                         ! loop counter
    integer :: jend                      ! number of last new part to be created by splitting
    integer :: jparte                    ! number of last part in 1..npart belonging to jpart
    integer :: jparts                    ! number of first part in 1..npart belonging to jpart
    integer :: jstart                    ! number of first new part to be created by splitting
    integer :: ksplit                    ! number of parts to be created in a particular splitting
    integer :: np                        ! number of parts to be created in a particular recursion
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    ! while not enough parts have been created, do another recursion
    !
    np = 1
    !
100 if ( np <= npart ) then
       !
       ! for each part that currently exists
       !
       do ip = 1, np
          !
          ! determine which final parts belong to this part
          !
          jparts = (ip-1)*npart/np+1
          jparte = (ip  )*npart/np
          !
          ! if the number of such parts > 1, do further splitting
          !
          if ( (jparte-jparts+1) > 1 ) then
             !
             ! determine into how many parts this part must be split
             !
             ksplit = min(nsplit,jparte-jparts+1)
             !
             ! determine the sizes and numbers of parts to be created
             !
             do isplit = 1, ksplit

                jstart = jparts+(isplit-1)*(jparte-jparts+1)/ksplit
                jend   = jparts+    isplit*(jparte-jparts+1)/ksplit-1

                itemp(1,isplit) = jstart

                itemp(2,isplit) = 0
                do j = jstart, jend
                   itemp(2,isplit) = itemp(2,isplit) + iwork(2,j)
                enddo

             enddo
             !
             ! do splitting
             !
             call dfstrip (ipown,jparts,ksplit,itemp,mmax,nmax)
             !
             ! determine whether objective partsizes have been modified
             ! in routine dfstrip in order to make straight interfaces
             !
             do isplit = 1, ksplit

                jstart = jparts+(isplit-1)*(jparte-jparts+1)/ksplit
                jend   = jparts+    isplit*(jparte-jparts+1)/ksplit-1

                do j = jstart, jend
                   itemp(2,isplit) = itemp(2,isplit) - iwork(2,j)
                enddo
                !
                ! and distribute the difference over the contiguous
                ! parts making sure not to cause negative subdomain-sizes
                !
                j      = jstart
                issucc = 0
                if ( itemp(2,isplit) < 0 ) then
                   idiff = -1
                else
                   idiff =  1
                endif
                !
                ! reduce the difference until nothing is left
                !
200             if ( itemp(2,isplit) /= 0 ) then
                   !
                   ! only adjust parts if their size remains valid
                   !
                   if ( iwork(2,j) > 0 .and. ((iwork(2,j)+idiff) > 0) ) then
                      issucc          = 1
                      iwork(2,j)      = iwork(2,j)      + idiff
                      itemp(2,isplit) = itemp(2,isplit) - idiff
                   endif
                   !
                   ! go on to the next part
                   !
                   j = j + 1
                   !
                   ! when all parts have been visited, go back to first
                   !
                   if ( j > jend ) then
                      !
                      ! check whether any reduction of the difference
                      ! was done in last pass over all parts
                      !
                      if ( issucc == 0 ) then
                         call prterr(lundia, 'U021', 'Internal problem in routine dforb')
                         call d3stop(1, gdp)
                      endif
                      j = jstart
                   endif

                   goto 200
                endif

             enddo

          endif

       enddo
       !
       ! swap cutting direction
       !
       ! idir = mod(idir,2) + 1
       np = nsplit*np
       goto 100
    endif
end subroutine dforb
