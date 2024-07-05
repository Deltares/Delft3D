subroutine dfbladm(ipown, icom, mmax, nmax, runid, gdp)
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
!   For the present node, carries out the block administration
!   and determines array bounds with respect to global grid
!
!!--pseudo code and references--------------------------------------------------
!
!   intialize offsets to be used in searching for interfaces
!   determine enclosing box of present subdomain
!   if subdomain appears to be empty
!      give warning and set empty bounding box
!   else
!      extend enclosing box to include halo area
!   determine interface sizes:
!
!      loop over global grid
!         if point belongs to this part
!            for each of the four sizes
!                if a neighbouring subdomain is found there
!                   find it in the list of neighbours
!                   if not yet in the list, add it
!                   store position of neighbour
!                   update number of overlapping unknowns
!                   store indices of point in present and halo areas
!
!   store block administration
!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat)          , target    :: gdp
    type(dfparalltype)     , pointer   :: gdparall
    integer                , pointer   :: lundia
    integer                , pointer   :: nfg
    integer                , pointer   :: nlg
    integer                , pointer   :: mfg
    integer                , pointer   :: mlg
    integer, dimension(:,:), pointer   :: iarrc
    integer, dimension(:)  , pointer   :: nf
    integer, dimension(:)  , pointer   :: nl
    integer, dimension(:)  , pointer   :: mf
    integer, dimension(:)  , pointer   :: ml
    integer                , pointer   :: ngridlo
    integer                , pointer   :: ngridgl
    integer                , pointer   :: nmaxgl
    integer                , pointer   :: mmaxgl
!
! Parameters
!
    integer, parameter :: infg = 1
    integer, parameter :: inlg = 2
    integer, parameter :: imfg = 3
    integer, parameter :: imlg = 4
!
! Global variables
!
    integer                            , intent(in)  :: mmax  ! number of gridpoints in the x-direction
    integer                            , intent(in)  :: nmax  ! number of gridpoints in the y-direction
    integer, dimension(mmax,nmax)      , intent(in)  :: ipown ! array giving the subdomain number of each gridpoint
    integer, dimension(-1:mmax+2, nmax), intent(in)  :: icom
    character(*)                       , intent(in)  :: runid ! Run identification code for the current simulation
!
! Local variables
!
    integer                              :: fillun
    integer, dimension(:), pointer       :: iblkad
    integer                              :: i              ! loop counter
    integer                              :: ibnd
    integer, dimension(:,:), allocatable :: icrecv         ! array containing positions of unknowns to be received from neighbour
    integer, dimension(:,:), allocatable :: icsend         ! array containing positions of unknowns to be sent to neighbour
    integer                              :: idom           ! subdomain number
    integer                              :: ihalo          ! actual width of halo area
    integer                              :: inb            ! neighbour counter
    integer                              :: istart
    integer                              :: iend
    integer                              :: istat          ! status code of allocation
    integer, dimension(:), allocatable   :: itemp          ! temporary work array to store block administration
    integer, dimension(3,nproc)          :: iwork          ! array used to determine interface sizes
                                                           !       iwork(1,i) = number of the i-th neighbour
                                                           !       iwork(2,i) = position of the i-th neighbour with
                                                           !                    respect to present subdomain
                                                           !                    (resp. top, bottom, right, left)
                                                           !       iwork(3,i) = size of interface to i-th neighbour
    integer                              :: j              ! loop counter
    integer, dimension(2,4)              :: joffs          ! offsets at which a point of a neigbhour domain can be found
    integer                              :: length         ! actual length of array IBLKAD
    integer                              :: m              ! current M-index of point in computational row
    integer                              :: moff           ! offset in x-direction
    integer                              :: n              ! current N-index of point in computational column
    integer,external                     :: newlun
    integer                              :: nneigh         ! number of neighbouring subdomains
    integer                              :: noff           ! offset in y-direction
    integer                              :: novlu          ! number of overlapping unknowns
    integer                              :: nsiz           ! size of present subdomain in y-direction
    integer, dimension(:,:), allocatable :: partition_dims
    logical                              :: ex
    character(8)                         :: date
    character(10)                        :: time
    character(300)                       :: message        ! string to pass message
    character(256)                       :: ddbfile
!
!! executable statements -------------------------------------------------------
!
    lundia   => gdp%gdinout%lundia
    gdparall => gdp%gdparall
    mmaxgl   => gdparall%mmaxgl
    nmaxgl   => gdparall%nmaxgl
    nfg      => gdparall%nfg
    nlg      => gdparall%nlg
    mfg      => gdparall%mfg
    mlg      => gdparall%mlg
    !
    write(message,'(a,i3.3,a)') 'Running parallel. Partition ', inode, ':'
    call prterr(lundia, 'G051', trim(message))
    !
    ! intialize offsets to be used in searching for interfaces
    !
    joffs = reshape((/0,1,0,-1,1,0,-1,0/), (/2,4/))
    !
    ! determine enclosing box of present subdomain (and also of the other subdomains)
    !
    allocate(partition_dims(4,nproc), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'dfbladm: memory alloc error')
       call d3stop(1, gdp)
    endif
    partition_dims(infg,:) = nmax+1
    partition_dims(inlg,:) = 0
    partition_dims(imfg,:) = mmax+1
    partition_dims(imlg,:) = 0
    !
    do m = 1, mmax
       do n = 1, nmax
          !
          ! ipown(m,n) contains the number of the partition where this point should
          ! belong to. Ignore the zeros in there.
          !
          if (ipown(m,n)>=1 .and. ipown(m,n)<=nproc) then
             partition_dims(infg,ipown(m,n)) = min(n,partition_dims(infg,ipown(m,n)))
             partition_dims(inlg,ipown(m,n)) = max(n,partition_dims(inlg,ipown(m,n)))
             partition_dims(imfg,ipown(m,n)) = min(m,partition_dims(imfg,ipown(m,n)))
             partition_dims(imlg,ipown(m,n)) = max(m,partition_dims(imlg,ipown(m,n)))
          endif
       enddo
    enddo
    nfg = partition_dims(infg, inode)
    nlg = partition_dims(inlg, inode)
    mfg = partition_dims(imfg, inode)
    mlg = partition_dims(imlg, inode)
    !
    if (inode == master) then
       !
       ! Write the related ddb file. Needed as input by DDCOUPLE to prepare a WAQ calculation
       !
       call DATE_AND_TIME(date, time)
       ddbfile = trim(runid) // "_" // trim(date) // "_" // trim(time) // ".ddb"
       open(newunit=fillun, file=trim(ddbfile), action="WRITE", iostat = istat)
       if (istat /= 0) then
          write(message,'(3a)') "Unable to open file """,trim(ddbfile),""". Skipping generation."
          call prterr(lundia, 'U190', trim(message))
       else
          do i=1,nproc-1
             !
             ! Generate the line related to the couple boundary between partition i and i+1
             !
             write(message,'(2a,i3.3,a)') trim(runid), '-',i,'.grd'
             if (idir == 1) then
                !
                ! The index of the boundary to be coupled is the last non-halo index:
                ! nlg               for partition 1
                ! nlg-(nfg-1)+halo  for the other partitions:
                !                   the lines 1 to nfg-1 are not active in this partition, the model will be shifted
                !                   take into account that a halo will be added in front
                !
                if (i == 1) then
                   ibnd = partition_dims(inlg,i)
                else
                   ibnd = partition_dims(inlg,i) - partition_dims(infg,i) + 1 + ihalon
                endif
                write(message,'(a,4(a,i0))') trim(message), '   ', 1     , '   ', ibnd, &
                                           &                '   ', mmax-1, '   ', ibnd
             else
                !
                ! Idem, with nmaxus/nlg/nfg replaced by mmax/mlg/mfg
                !
                if (i == 1) then
                   ibnd = partition_dims(imlg,i)
                else
                   ibnd = partition_dims(imlg,i) - partition_dims(imfg,i) + 1 + ihalom
                endif
                write(message,'(a,4(a,i0))') trim(message), '   ', ibnd, '   ', 1, &
                                           &                '   ', ibnd, '   ', nmax-1
             endif
             write(message,'(4a,i3.3,a)') trim(message), '   ', trim(runid), '-',i+1,'.grd'
             !
             ! Couple boundary index in the other partition:
             ! The first non-halo index is 3
             !
             if (idir == 1) then
                write(message,'(a,4(a,i0))') trim(message), '   ', 1     , '   ', 3, &
                                           &                '   ', mmax-1, '   ', 3
             else
                write(message,'(a,4(a,i0))') trim(message), '   ', 3, '   ', 1, &
                                           &                '   ', 3, '   ', nmax-1
             endif
             write(fillun,'(a)') message
          enddo
       endif
       close(fillun)
       call removeDuplicateDDBFiles(runid, ddbfile, gdp)
    endif
    deallocate(partition_dims, stat=istat)
    !
    ! if subdomain appears to be empty
    !
    if ( nfg > nlg .or. mfg > mlg ) then
       !
       ! give warning and set empty bounding box
       !
       write (message,'(a,i3.3)') 'Empty subdomain is detected - node number is ',inode
       call prterr(lundia, 'U190', trim(message))
       nfg = 1
       nlg = 0
       mfg = 1
       mlg = 0
    else
       !
       ! extend enclosing box to include halo area
       !
       if (idir==1) then
          nfg = max(   1,nfg-ihalon)
          nlg = min(nmax,nlg+ihalon)
          mfg = 1
          mlg = mmax
       else if (idir==2) then 
          mfg = max(   1,mfg-ihalom)
          mlg = min(mmax,mlg+ihalom)
          nfg = 1
          nlg = nmax
       endif
    endif
    !
    allocate(gdparall%nf(0:nproc-1), &
           & gdparall%nl(0:nproc-1), &
           & gdparall%mf(0:nproc-1), &
           & gdparall%ml(0:nproc-1), &
           & gdparall%iarrc(4,0:nproc-1), stat=istat)
    nf       => gdparall%nf
    nl       => gdparall%nl
    mf       => gdparall%mf
    ml       => gdparall%ml
    iarrc    => gdparall%iarrc
    ngridlo  => gdparall%ngridlo
    ngridgl  => gdparall%ngridgl
    !
    call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
       &                 nf, nl, mf, ml, iarrc, ngridgl, ngridlo, gdp )
    !
    ! broadcast LOCAL grid indices to ALL partitions
    ! so every partition knows the dimensions and positions
    ! of the other partitions in the global domain
    !
    call dfbroadc_gdp ( iarrc, 4*nproc, dfint, gdp )
    call dfbroadc_gdp ( nf, nproc, dfint, gdp )
    call dfbroadc_gdp ( nl, nproc, dfint, gdp )
    call dfbroadc_gdp ( mf, nproc, dfint, gdp )
    call dfbroadc_gdp ( ml, nproc, dfint, gdp )
    !
    nsiz = nlg - nfg + 1
    if ( mod(nsiz,2)==0 ) nsiz = nsiz + 1
    allocate(icrecv(nproc,max(ihalom,ihalon)*max(mmax,nmax)))
    allocate(icsend(nproc,max(ihalom,ihalon)*max(mmax,nmax)))
    !
    iwork  = 0
    icrecv = 0
    icsend = 0
    !
    ! determine interface sizes
    !
    do m = 1, mmax
       do n = 1, nmax
          !
          ! if point belongs to this part
          !
          if ( ipown(m,n) == inode ) then
             !
             ! for each of the four sizes
             !
             do i = 1, 4
                moff  = joffs(1,i)
                noff  = joffs(2,i)
                if ( i==1 .or. i==2 ) then
                   ihalo = ihalon
                else
                   ihalo = ihalom
                endif
                !
                ! if a neighbouring subdomain is found there
                !
                if ( (m+moff) > 0 .and. (m+moff) <= mmax .and.  &
                     (n+noff) > 0 .and. (n+noff) <= nmax ) then
                   if ( ipown(m+moff,n+noff) /= 0    .and.  &
                        ipown(m+moff,n+noff) /= inode ) then
                      !
                      ! find it in the list of neighbours
                      !
                      idom = ipown(m+moff,n+noff)
                      inb = 1
  100                 continue
                      if ( inb <= nproc .and.  &
                           iwork(1,inb) /= idom .and.  &
                           iwork(1,inb) /= 0 )  then
                         inb = inb + 1
                         goto 100
                      endif

                      if ( inb > nproc ) then
                         call prterr(lundia, 'U021', 'Found more neighbours than subdomains in the partitioning')
                         call d3stop(1, gdp)
                      endif
                      !
                      ! if not yet in the list, add it
                      !
                      if ( iwork(1,inb) == 0 ) iwork(1,inb) = idom
                      !
                      ! store position of neighbour with respect to present subdomain
                      !
                      iwork(2,inb) = i
                      !
                      ! update number of overlapping unknowns
                      !
                      iwork(3,inb) = iwork(3,inb) + ihalo
                      !
                      ! store indices of point in present and halo areas
                      !
                      do j = 1, ihalo
                         icsend(inb,iwork(3,inb)-j+1) = (m-(j-1)*moff-mfg)*nsiz + (n-(j-1)*noff-nfg+1)
                         icrecv(inb,iwork(3,inb)-j+1) = (m+    j*moff-mfg)*nsiz + (n+    j*noff-nfg+1)
                      enddo
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! store block administration to be used for communication
    !
    allocate(itemp(1+2*nproc*(2+max(ihalom,ihalon)*max(mmax,nmax))))
    itemp = -999
    !
    nneigh   = count(iwork(1,:)>0)
    itemp(1) = nneigh
    istart   = 3*nneigh+2
    do inb = 1, nneigh
       itemp(3*inb-1) = iwork(1,inb)
       itemp(3*inb  ) = iwork(2,inb)
       itemp(3*inb+1) = istart
       novlu          = iwork(3,inb)
       itemp(istart)  = novlu
       do i = 1, novlu
          itemp(istart      +i) = icsend(inb,i)
          itemp(istart+novlu+i) = icrecv(inb,i)
       enddo
       istart = istart + 2*novlu+1
    enddo
    !
    length = count(itemp/=-999)
    allocate (gdp%gdparall%iblkad(length), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'dfbladm: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! Update references
    !
    iblkad   => gdparall%iblkad
    !
    do i = 1, length
       iblkad(i) = itemp(i)
    enddo
    deallocate(icrecv,icsend,itemp)
end subroutine dfbladm
