!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine setprg ( procesdef, nogrid, notot , grdref, sysgrd,
     +                    sysndt   )

      ! set grid for all processes

      use m_zoek
      use dhralloc
      use processet
      use timers       !   performance timers
      use m_dhggd

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: nogrid          ! number of grids
      integer                   :: notot           ! number of substances
      integer                   :: grdref(nogrid)  ! reference grid number
      integer                   :: sysgrd(notot)   ! grid number substances
      integer                   :: sysndt(notot)   ! timestep multiplier substances

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      integer                   :: iproc2          ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      type(procesprop), pointer :: proc2           ! process description
      integer                   :: isys            ! index substance
      integer                   :: i               ! loop index
      integer                   :: ipgrid          ! index grid
      integer                   :: istochi         ! index stochi
      integer                   :: i_input         ! index input
      integer                   :: ioutput         ! index output
      integer                   :: imnoag          ! index routine
      integer                   :: nmnoag          ! number of routines whic may not be aggregated
      integer, parameter        :: mxnoag = 1000   ! dimension for local array
      character(len=10)         :: monoag(mxnoag)  ! list of routines which may not be aggregated
      integer                   :: maxwrk          ! dimension for local array
      integer                   :: nototg          !
      integer                   :: maxndt          ! max timestep multiplier
      integer                   :: ndt             ! timestep multiplier
      integer                   :: nndt            ! number of ndt
      character(len=20)         :: valnam          ! name
      integer, allocatable      :: grpath(:)       !
      integer, pointer          :: grdwrk(:)       !
      logical                   :: lexi
      logical                   :: l_exchange      ! in or output on exchanges
      integer, allocatable      :: isysto(:)       ! temp, copy of the substances in the fluxstochi
      integer                   :: lun
      integer                   :: ierr
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setprg", ithndl )

      allocate(grpath(nogrid))
      maxwrk = 1000
      allocate(grdwrk(maxwrk))

      ! read list processes not to be aggragated
      monoag(1) = 'TOTDEPTH'
      monoag(2) = 'STADAY'
      monoag(3) = 'STADPT'
      monoag(4) = 'STADSC'
      monoag(5) = 'STAGEO'
      monoag(6) = 'STAPRC'
      monoag(7) = 'STAQTL'
      nmnoag = 7

      inquire ( file='procnoag.dat' , exist = lexi )
      if ( lexi ) then
         open(newunit=lun, file='procnoag.dat')
         do
            nmnoag = nmnoag + 1
            read(lun,*,iostat = ierr) monoag(nmnoag)
            if ( ierr /= 0 ) then
               exit
            endif
         enddo
         nmnoag = nmnoag - 1
         close (lun)
      endif

      ! first step, processes with fluxes set to the grid set for the
      ! substances. for processes with exchange io or in the list with
      ! processes that can not be aggregated always use base grid.
      ! what to do with specials like clcrad that use the pointer
      ! table without using exchange io?

      nproc = procesdef%cursize
      do iproc = 1, nproc
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then

            ! check for in and output on exchange

            l_exchange = .false.
            do i_input = 1, proc%no_input
               if ( proc%input_item(i_input)%type .eq. IOTYPE_EXCHANG_INPUT ) then
                  l_exchange = .true.
               endif
            enddo
            do ioutput = 1, proc%no_output
               if ( proc%output_item(ioutput)%type .eq. IOTYPE_EXCHANG_OUTPUT ) then
                  l_exchange = .true.
               endif
            enddo

            call zoek ( proc%routine, nmnoag , monoag, 10    , imnoag)
            if ( imnoag .le. 0 ) then
               call zoek ( proc%name, nmnoag , monoag, 10    , imnoag)
            endif
            if ( imnoag .gt. 0 ) then
               proc%grid = 1
            elseif ( l_exchange ) then
               proc%grid = 1
            else
               nototg = proc%no_fluxstochi
               if ( nototg .gt. 0 ) then
                  allocate(isysto(nototg))
                  do i=1, nototg
                     isysto(i)=proc%fluxstochi(i)%subindx
                  enddo
                  call setgrd( nogrid, notot , nototg, grdref, sysgrd,
     +                         isysto, grpath, ipgrid)
                  deallocate(isysto)
                  if ( ipgrid .lt. 1 ) then

                     ipgrid = 1

                  endif
                  proc%grid = ipgrid
               else

                  ! no substance, highest grid

                  proc%grid = nogrid

               endif
            endif
         endif
      enddo
      do iproc = nproc, 1, -1
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            ipgrid = proc%grid
            if ( ipgrid .ne. 1 ) then
               nototg    = 1
               grdwrk(1) = ipgrid
               do ioutput = 1 , proc%no_output

                  ! check how the output is used

                  valnam = proc%output_item(ioutput)%name
                  do iproc2 = iproc + 1, nproc
                     proc2 => procesdef%procesprops(iproc2)
                     if ( proc2%active ) then
                        call zoekio ( valnam, proc2%no_input, proc2%input_item, 20, i_input)
                        if ( i_input .gt. 0 ) then
                           nototg = nototg + 1
                           if ( nototg .gt. maxwrk ) then
                              maxwrk = maxwrk*2
                              call dhralloc_int(grdwrk,maxwrk,nototg-1)
                           endif
                           grdwrk(nototg) = proc2%grid
                        endif
                     endif
                  enddo
               enddo
               call setgr2( nogrid, nototg, grdref, grdwrk, grpath,
     +                      ipgrid)
               if ( ipgrid .lt. 1 ) then
cjvb              afhandelen exception? of error
                  ipgrid = 1
               endif
               proc%grid = ipgrid
            endif
         endif
      enddo

      ! now the steps in the fractional step.


      ! set largest step

      maxndt = 1
      do isys = 1 , notot
         maxndt = max(sysndt(isys),maxndt)
      enddo

      ! set largest step for processes with stochi's

      do iproc = 1, nproc
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            call zoek ( proc%routine, nmnoag , monoag, 10    , imnoag)
            if ( imnoag .le. 0 ) then
               call zoek ( proc%name, nmnoag , monoag, 10    , imnoag)
            endif
            if ( imnoag .gt. 0 ) then
               proc%ndt = 1
            else
               nndt = 0
               do istochi = 1 , proc%no_fluxstochi
                  if ( proc%fluxstochi(istochi)%subindx .gt. 0 ) then
                     nndt = nndt + 1
                     if ( nndt .gt. maxwrk ) then
                        maxwrk = maxwrk*2
                        call dhralloc_int(grdwrk,maxwrk,nndt-1)
                     endif
                     grdwrk(nndt) = sysndt(proc%fluxstochi(istochi)%subindx)
                  endif
               enddo
               if ( nndt .gt. 0 ) then
                  call dhggd ( nndt  , grdwrk, ndt   )
                  proc%ndt = ndt
               else

                  ! no substance, largest step

                  proc%ndt = maxndt

               endif
            endif
         endif
      enddo

      ! rest of the processes, check use then set timestep

      do iproc = nproc, 1, -1
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            ndt = proc%ndt
            if ( ndt .ne. 1 ) then
               nndt = 1
               grdwrk(nndt) = ndt
               do ioutput = 1 , proc%no_output

                  ! check how the output is used

                  valnam = proc%output_item(ioutput)%name
                  do iproc2 = iproc + 1, nproc
                     proc2 => procesdef%procesprops(iproc2)
                     if ( proc2%active ) then
                        call zoekio ( valnam, proc2%no_input, proc2%input_item, 20, i_input)
                        if ( i_input .gt. 0 ) then
                           nndt = nndt + 1
                           if ( nndt .gt. maxwrk ) then
                              maxwrk = maxwrk*2
                              call dhralloc_int(grdwrk,maxwrk,nndt-1)
                           endif
                           grdwrk(nndt) = proc2%ndt
                        endif
                     endif
                  enddo
               enddo
               call dhggd ( nndt  , grdwrk, ndt   )
               proc%ndt = ndt
            endif
         endif
      enddo

      deallocate(grpath)
      deallocate(grdwrk)

      if (timon) call timstop( ithndl )
      return
      end
