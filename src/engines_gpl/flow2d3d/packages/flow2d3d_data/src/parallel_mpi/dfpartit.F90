subroutine dfpartit ( ipown, icom, mmax, nmax, gdp )
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
!   Carries out the partitioning of the computational grid
!
!!--pseudo code and references--------------------------------------------------
!
!   allocate and initialize array IWEIG
!   write or read list of processor speeds and overwrite IWEIG
!   determine direction of cutting
!   determine number of active points
!   determine numbers and sizes of parts to be created
!   partition grid
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
    integer, dimension(:), pointer :: iweig
    integer, pointer            :: lundia
    integer                     :: i      ! loop counter
    integer                     :: icnt   ! auxiliary integer to count weights
    integer, dimension(10)      :: idign  ! auxiliary integer indicating digit in number
    integer                     :: ilen   ! auxiliary integer indicating string length
    integer                     :: ipos   ! auxiliary integer indicating position of digit in number
    integer                     :: istat  ! status code of allocation
    integer                     :: iw     ! auxiliary integer indicating actual speed of processor
    integer, dimension(2,nproc) :: iwork  ! work array with the following meaning:
                                          !    iwork(1,i) = number of i-th part to be created
                                          !    iwork(2,i) = size of i-th part to be created
    integer                     :: j      ! auxiliary integer indicating j-th processor
    integer                     :: k      ! auxiliary integer indicating ASCII code of digit
    integer                     :: l      ! loop counter
    integer                     :: luntmp ! temporary file unit number
    integer                     :: m      ! current M-index of point in computational row
    integer                     :: n      ! current N-index of point in computational column
    integer, external           :: newlun
    integer(kind=8)             :: nactp  ! total number of active gridpoints
    integer(kind=8)             :: npcum  ! cumulative number of gridpoints
    logical                     :: ex     ! Help flag = TRUE when file is found
    character(18)               :: filspp ! file name for list of processor speeds
    character(256)              :: txt1   ! auxiliary text string
    character(256)              :: txt2   ! auxiliary text string
    integer(kind=8)             :: tmp
    integer(kind=8)             :: tmpsum
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
    !
    ! write or read list of processor speeds and overwrite IWEIG
    !
    ! 
    ! removed weighting of processor speeds 
    ! 
    !    filspp = 'speed_of_procs.lst'
    !    !
    !    inquire (file = filspp, exist = ex)
    !    if ( ex ) then
    !       open (newunit=luntmp, file = filspp, form = 'formatted', status = 'old')
    !       do i = 1, 3
    !          read (luntmp, '(a)') txt1
    !       enddo
    !       do l = 1, nproc
    !          read (luntmp, '(i20,a)', err=10, end=10) iw, txt1
    !          txt2 = adjustl(txt1)
    !          ilen = len_trim(txt2)
    !          ipos = 0
    !          do i = 1, ilen
    !             k = ichar(txt2(i:i))
    !             if ( k.ge.48 .and. k.le.57 ) then
    !                ipos = ipos + 1
    !                idign(ipos) = k - 48
    !             endif
    !          enddo
    !          j = 0
    !          do i = 1, ipos
    !             j = j + idign(i)*10**(ipos-i)
    !          enddo
    !          iweig(j) = iw
    !       enddo
    ! 10    close(luntmp)
    !    else
    !       open (newunit=luntmp, file = filspp, form = 'formatted', status = 'unknown')
    !       txt1 = '# list of non-default processor speeds in % of default'
    !       txt2 = '# default speed = 100%'
    !       write (luntmp, '(a)') trim(txt1)
    !       write (luntmp, '(a)') trim(txt2)
    !       write (luntmp, '(a)')
    !       do i = 1, nproc
    !          write (luntmp, '(i5, t41, a19, i3)') iweig(i), 'speed of processor ',i
    !       enddo
    !       close(luntmp)
    !    endif
    ! 
    do i = 1, nproc
       iweig(i) = 100
    enddo
    !
    ! determine direction of cutting
    !
    if ( mmax > nmax ) then
       idir = 2
    else
       idir = 1
    endif
    ! 
    ! check for cutting through smallest dimension
    !    if ( mmax > nmax ) then
    !       idir = 1
    !    else
    !       idir = 2
    !    endif
    ! 
    if (max(mmax,nmax)/nproc < 4) then
       write(txt1,'(a,i0,a)') 'Domain is too small to divide in ', nproc, ' partitions'
       call prterr(lundia, 'U021', trim(txt1))
       write(lundia,'(10x,a)') '"max(mmax,nmax) / num_partitions" must be greater than 3'
       call d3stop(1, gdp)
    endif
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
end subroutine dfpartit
