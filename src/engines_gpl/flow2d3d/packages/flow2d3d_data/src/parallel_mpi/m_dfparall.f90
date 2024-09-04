module m_dfparall
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!   Contains subroutines for partitioning a Delft3D-FLOW domain.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

    use globaldata
    use dfparall
    
    implicit none
    
    public automatic_partitioning, manual_partitioning
    
    contains
    
    !>Automatic partitioning of domain.
    subroutine automatic_partitioning(ipown, icom, mmax, nmax, gdp)
    
    type(globdat), target    :: gdp
    !
    ! Local parameters
    !
    logical, parameter :: lorb = .false. ! logical indicating which partition method will be carried out:
                                         ! true, in case of ORB
                                         ! false, in case of stripwise manner
    !
    ! Global variables
    !
    integer, intent(in)                             :: mmax  ! number of gridpoints in the x-direction
    integer, intent(in)                             :: nmax  ! number of gridpoints in the y-direction
    !
    integer, dimension(-1:mmax+2, nmax), intent(in) :: icom  ! mask array for the water level points in global domain
                                                             !  = 0 : point is not active
                                                             ! <> 0 : point is active
    integer, dimension(mmax,nmax), intent(inout)    :: ipown ! array giving the subdomain number of each gridpoint   
    !
    ! Local variables
    !
    integer, pointer      :: iweig(:)       !< partitioning weights
    integer               :: i              !< loop counter
    integer               :: icnt           !< auxiliary integer to count weights
    integer               :: iwork(2,nproc) !< work array with the following meaning:
                                            !     iwork(1,i) = number of i-th part to be created
                                            !     iwork(2,i) = size of i-th part to be created
    integer               :: m              !< current M-index of point in computational row
    integer               :: n              !< current N-index of point in computational column
    integer(kind=8)       :: nactp          !< total number of active gridpoints
    integer(kind=8)       :: npcum          !< cumulative number of gridpoints

    integer(kind=8)       :: tmp            
    integer(kind=8)       :: tmpsum             
    !
    ! determine number of active points and set ipown to 1 in these points
    !
    nactp = 0
    do m = 1, mmax
       do n = 1, nmax
          if ( icom(m,n) /= 0 ) then
             ipown(m,n) = 1
             nactp      = nactp + 1
          endif
       enddo
    enddo
    !
    ! determine numbers and sizes of parts to be created
    !
    npcum  = 0
    icnt   = 0
    tmpsum = sum(iweig)
    do i = 1, nproc
       icnt       = icnt + iweig(i)
       iwork(1,i) = i
       tmp        = nactp*icnt/tmpsum
       iwork(2,i) = tmp - npcum
       npcum      = tmp
    enddo
    !
    ! partition grid
    !
    if ( lorb ) then
       !
       ! performs orthogonal recursive bisection partitioning
       !
       call dforb ( ipown, nproc, iwork, mmax, nmax, gdp )
    else
      !
      ! performs stripwise partitioning
      !
      call dfstrip ( ipown, 1, nproc, iwork, mmax, nmax )
    endif

    end subroutine automatic_partitioning
    
    !>Manual partitioning of domain.
    subroutine manual_partitioning(ipown, icom, mmax, nmax, gdp)
    
    type(globdat), target    :: gdp
    !
    ! Local parameters
    !
    logical, parameter :: lorb = .false. ! logical indicating which partition method will be carried out:
                                         ! true, in case of ORB
                                         ! false, in case of stripwise manner
    !
    ! Global variables
    !
    integer, intent(in)                             :: mmax  ! number of gridpoints in the x-direction
    integer, intent(in)                             :: nmax  ! number of gridpoints in the y-direction
    !
    integer, dimension(-1:mmax+2, nmax), intent(in) :: icom  ! mask array for the water level points in global domain
                                                             !  = 0 : point is not active
                                                             ! <> 0 : point is active
    integer, dimension(mmax,nmax), intent(inout)    :: ipown ! array giving the subdomain number of each gridpoint   
    !
    ! Local variables
    !
    integer               :: dirmax         !< length of dimension to be partitioned
    integer, pointer      :: lundia         !< unit number of diagnostic output file
    integer               :: i              !< loop counter
    integer               :: m              !< current M-index of point in computational row
    integer               :: n              !< current N-index of point in computational column
    integer               :: partlowerbnd   !< lower bound of the considered partition
    character(1)          :: dirstr         !< string naming the partitioning direction
    character(256)        :: txt1           !< auxiliary text string
    integer               :: partbnd(nproc) !< upper bounds of each partition
    !
    ! determine direction of cutting
    !
    if ( mmax > nmax ) then
       idir = 2
       dirstr = 'M'
       dirmax = mmax
    else
       idir = 1
       dirstr = 'N'
       dirmax = nmax
    endif
    !
    ! check input
    !
    ! check partition 1
    if (partbnd(1) < 4) then
       write(txt1,'(3a,i0,a,i0,a)') 'Partition 1 must be at least 4 cells wide! PartBnd defines it now as ',dirstr,' = 1 to ',partbnd(1),' hence ',max(0,partbnd(1)),' wide'
       call prterr(lundia, 'U021', trim(txt1))
       call d3stop(1, gdp)
    endif
    ! check partitions 2 to nproc
    do i = 2, nproc
       if (partbnd(i) < partbnd(i-1) + 4) then
          write(txt1,'(a,i0,3a,i0,a,i0,a,i0,a)') 'Partition ',i,' must be at least 4 cells wide! PartBnd defines it now as ',dirstr,' = ',partbnd(i-1)+1,' to ',partbnd(i),' hence ',max(0,partbnd(i)-partbnd(i-1)),' wide'
          call prterr(lundia, 'U021', trim(txt1))
          call d3stop(1, gdp)
       endif
    enddo
    ! check partition nproc
    if (partbnd(nproc) /= dirmax) then
       write(txt1,'(3a,i0,a,i0)') 'The last partition boundary index should equal ',dirstr,'MAX: ',partbnd(nproc),' /= ',dirmax
       call prterr(lundia, 'U021', trim(txt1))
       call d3stop(1, gdp)
    endif
    !
    ! stripwise partitioning as specified by user
    !
    if (idir == 1) then ! N
       partlowerbnd = 1
       do i = 1, nproc
          do n = partlowerbnd, partbnd(i)
             do m = 1, mmax
                if ( icom(m,n) /= 0 ) then
                   ipown(m,n) = i
                endif
             enddo
          enddo
          partlowerbnd = partbnd(i)+1
       enddo
    else ! M
       partlowerbnd = 1
       do i = 1, nproc
          do m = partlowerbnd, partbnd(i)
             do n = 1, nmax
                if ( icom(m,n) /= 0 ) then
                   ipown(m,n) = i
                endif
             enddo
          enddo
          partlowerbnd = partbnd(i)+1
       enddo
    endif
       
    end subroutine manual_partitioning

    !>Carries out the partitioning of the computational grid
    subroutine dfpartit ( ipown, icom, mmax, nmax, gdp )

    use properties, only: prop_get_integers
    !
    type(globdat), target    :: gdp
    !
    ! Local parameters
    !
    logical, parameter :: lorb = .false. ! logical indicating which partition method will be carried out:
                                         ! true, in case of ORB
                                         ! false, in case of stripwise manner
    !
    ! Global variables
    !
    integer, intent(in)                             :: mmax  ! number of gridpoints in the x-direction
    integer, intent(in)                             :: nmax  ! number of gridpoints in the y-direction
    !
    integer, dimension(-1:mmax+2, nmax), intent(in) :: icom  ! mask array for the water level points in global domain
                                                             !  = 0 : point is not active
                                                             ! <> 0 : point is active
    integer, dimension(mmax,nmax), intent(inout)    :: ipown ! array giving the subdomain number of each gridpoint
    !
    ! Local variables
    !
    integer, pointer      :: iweig(:)       !< partitioning weights
    integer, pointer      :: lundia         !< unit number of diagnostic output file
    integer               :: i              !< loop counter
    integer               :: istat          !< status code of allocation
    logical               :: partbnd_read   !< Success flag for reading the PartBnd keyword from mdf-file
    character(256)        :: txt1           !< auxiliary text string
    integer               :: partbnd(nproc) !< upper bounds of each partition
    !
    !! executable statements -------------------------------------------------------
    !
    lundia => gdp%gdinout%lundia
    !
    ! allocate and initialize array IWEIG
    !
    allocate (gdp%gdparall%iweig(nproc), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'dfpartit: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    iweig => gdp%gdparall%iweig
    !
    iweig = 100
    do i = 1, nproc
       iweig(i) = 100
    enddo
    !
    ! check if partition boundaries have been specified
    !
    partbnd = 0
    call prop_get_integers(gdp%mdfile_ptr,'*','PartBnd',partbnd,nproc,partbnd_read,valuesfirst=.true.)
    
    if (max(mmax,nmax)/nproc < 4) then
       write(txt1,'(a,i0,a)') 'Domain is too small to divide in ', nproc, ' partitions'
       call prterr(lundia, 'U021', trim(txt1))
       write(lundia,'(10x,a)') '"max(mmax,nmax) / num_partitions" must be greater than 3'
       call d3stop(1, gdp)
    elseif (partbnd_read) then
        call manual_partitioning(ipown, icom, mmax, nmax, gdp)
    else
        call automatic_partitioning(ipown, icom, mmax, nmax, gdp)    
    endif
    
end subroutine dfpartit

end module m_dfparall