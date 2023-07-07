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

      subroutine wrstoc ( procesdef, luout , notot  , syname, stoch2,
     +                    noutp    , ioutps, outputs, ndmpar, nbufmx,
     +                    intopt)

      ! writes the stochi file, sets stoch2 array
      ! set output structure for new balance file

      use timers         !< performance timers
      use dhralloc
      use processet
      use output
      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: luout           ! stochi file
      integer                   :: notot           ! number of substances
      character(len=*)          :: syname(notot)   ! name of substances
      real                      :: stoch2(notot,*) ! delwaq stochi array
      integer                   :: noutp           ! number of output variables
      integer                   :: ioutps(7,noutp) ! output structure
      type(outputcoll)          :: outputs         ! output structure
      integer                   :: ndmpar          ! number of stations
      integer                   :: nbufmx          ! max buffer
      integer                   :: intopt          ! integration option

      character(len=80)         :: line            ! output buffer
      integer                   :: noflx           ! number of fluxes
      integer                   :: nproctot        ! number of processes
      integer                   :: iproc           ! index process
      type(procesprop), pointer :: proc            ! process description
      integer                   :: i               ! loop counter
      integer                   :: j               ! loop counter
      integer                   :: niflx           ! number of fluxes
      integer                   :: nflx            ! number of fluxes
      integer                   :: iflx            ! index flux
      integer                   :: iflux           ! index flux
      integer                   :: istochi         ! index flux
      integer                   :: isys            ! index substance
      real                      :: scale           ! stochi factor
      character(len=20)         :: flxnam          ! output buffer
      integer                   :: nrvar           ! counter
      integer                   :: nrvarn          ! counter
      integer                   :: nrvaro          ! counter
      integer                   :: ioutp           ! index output variable
      integer                   :: ivar            ! index output variable
      integer                   :: isrtou          ! type of output
      integer                   :: nobalt          ! number of balances
      integer                   :: nocel           ! number of cells
      integer                   :: nbufou          ! buffer used
      type(outputcoll)          :: outputl         ! local output structure
      integer                   :: nrvarm          ! size of local output structure
      integer(4)                :: ithndl = 0        ! handle for performance timer
      if (timon) call timstrt( "wrstoc", ithndl )

      ! calculate noflx

      noflx = 0
      nproctot = procesdef%cursize
      do iproc = 1 , nproctot
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            noflx = noflx + proc%no_fluxoutput
         endif
      enddo

      ! zero stoch2 array

      do j = 1 , noflx
         do i = 1 , notot
            stoch2(i,j) = 0.0
         enddo
      enddo

      ! write stochiometry output file


      ! fluxes

      if ( btest(intopt,3) .and. .not. btest(intopt,4) ) then
         write (luout,'(i5)') noflx
         do iproc = 1 , nproctot
            proc => procesdef%procesprops(iproc)
            if ( proc%active ) then
               niflx = proc%no_fluxoutput
               do iflx = 1, niflx
                  write (luout,'(a20)') proc%fluxoutput(iflx)%name
               enddo
            endif
         enddo
      endif

      ! stochimetry, write and set stoch2 = complete stochiometry

      nflx = 0
      do iproc = 1 , nproctot
         proc => procesdef%procesprops(iproc)
         if ( proc%active ) then
            do istochi = 1, proc%no_fluxstochi

               flxnam = proc%fluxstochi(istochi)%ioitem
               isys   = proc%fluxstochi(istochi)%subindx
               scale  = proc%fluxstochi(istochi)%scale

               if ( isys.gt.0 .and. abs(scale).gt.1e-10) then
                  call zoekio ( flxnam, proc%no_fluxoutput, proc%fluxoutput, 20, iflux)
                  iflx = nflx + iflux
                  stoch2(isys,iflx) = scale
                  if ( btest(intopt,3) .and. .not. btest(intopt,4) ) then
                     write (luout,'(a20,2x,a20,2x,f10.3)') syname(isys)(1:10), flxnam, scale
                  endif
               endif
            enddo
            nflx = nflx + proc%no_fluxoutput
         endif
      enddo

      ! set variables for output balance file new style

      nrvarm = outputs%cursize*2
      outputl%cursize=nrvarm
      allocate(outputl%names(nrvarm))
      allocate(outputl%stdnames(nrvarm))
      allocate(outputl%pointers(nrvarm))
      allocate(outputl%units(nrvarm))
      allocate(outputl%descrs(nrvarm))
      nrvarn = 0
      nrvaro = 0
      do ioutp = 1 , noutp
         isrtou = ioutps(5,ioutp)
         if ( isrtou .eq. iba2 ) then
            nobalt = 0
            do isys = 1,notot
               if ( nrvarn + nobalt + 4 .gt. nrvarm ) then
                  outputl%cursize=(nrvarn+nobalt+4)*2
                  call dhralloc_int(outputl%pointers ,outputl%cursize,nrvarm)
                  call dhralloc_ch20(outputl%names   ,outputl%cursize,nrvarm)
                  nrvarm = outputl%cursize
               endif
               outputl%names(nrvarn+nobalt+1)( 1:10) = syname(isys)
               outputl%names(nrvarn+nobalt+1)(11:20) = 'Loads in'
               outputl%names(nrvarn+nobalt+2)( 1:10) = syname(isys)
               outputl%names(nrvarn+nobalt+2)(11:20) = 'Loads out'
               outputl%names(nrvarn+nobalt+3)( 1:10) = syname(isys)
               outputl%names(nrvarn+nobalt+3)(11:20) = 'Transp in'
               outputl%names(nrvarn+nobalt+4)( 1:10) = syname(isys)
               outputl%names(nrvarn+nobalt+4)(11:20) = 'Transp out'
               outputl%pointers(nrvarn+nobalt+1)        = 0
               outputl%pointers(nrvarn+nobalt+2)        = 0
               outputl%pointers(nrvarn+nobalt+3)        = 0
               outputl%pointers(nrvarn+nobalt+4)        = 0
               nobalt = nobalt + 4
               nflx   = 0
               do iproc = 1 , nproctot
                  proc => procesdef%procesprops(iproc)
                  if ( proc%active ) then
                     niflx = proc%no_fluxoutput
                     do iflx = 1, niflx
                        scale = stoch2(isys,nflx+iflx)
                        if ( abs(scale) .gt. 1.e-10 ) then
                           nobalt = nobalt + 1
                           if ( nrvarn + nobalt .gt. nrvarm ) then
                              outputl%cursize=(nrvarn+nobalt)*2
                              call dhralloc_int(outputl%pointers ,outputl%cursize,nrvarm)
                              call dhralloc_ch20(outputl%names   ,outputl%cursize,nrvarm)
                              nrvarm = outputl%cursize
                           endif
                           outputl%names(nrvarn+nobalt)( 1:10) = syname(isys)
                           outputl%names(nrvarn+nobalt)(11:20) = proc%fluxoutput(iflx)%name
                           outputl%pointers(nrvarn+nobalt)        = 0
                        endif
                     enddo
                     nflx = nflx + niflx
                  endif
               enddo
            enddo
            nrvar  = ioutps(4,ioutp)
            ioutps(4,ioutp) = nobalt
            nrvaro = nrvaro + nrvar
            nrvarn = nrvarn + nobalt

            nocel = ndmpar
            nbufou = nocel*nobalt
            nbufmx = max ( nbufmx, nbufou )

         elseif ( isrtou .eq. iba3 ) then

            if ( nrvarn + noflx .gt. nrvarm ) then
               outputl%cursize = (nrvarn+noflx)*2
               call dhralloc_int(outputl%pointers ,outputl%cursize,nrvarm)
               call dhralloc_ch20(outputl%names   ,outputl%cursize,nrvarm)
               nrvarm = outputl%cursize
            endif

            iflux = 0
            do iproc = 1 , nproctot
               proc => procesdef%procesprops(iproc)
               if ( proc%active ) then
                  niflx = proc%no_fluxoutput
                  do iflx = 1, niflx
                     iflux = iflux + 1
                     outputl%names(nrvarn+iflux) = proc%fluxoutput(iflx)%name
                     outputl%pointers(nrvarn+iflux) = 0
                  enddo
               endif
            enddo

            nrvar = ioutps(4,ioutp)
            ioutps(4,ioutp) = noflx
            nrvaro = nrvaro + nrvar
            nrvarn = nrvarn + noflx

         else
            nrvar = ioutps(4,ioutp)
            if ( nrvarn + nrvar .gt. nrvarm ) then
               outputl%cursize = (nrvarn+nrvar)*2
               call dhralloc_int(outputl%pointers ,outputl%cursize,nrvarm)
               call dhralloc_ch20(outputl%names   ,outputl%cursize,nrvarm)
               nrvarm = outputl%cursize
            endif
            do ivar = 1 , nrvar
               outputl%names(nrvarn+ivar) = outputs%names(nrvaro+ivar)
               outputl%pointers(nrvarn+ivar) = outputs%pointers(nrvaro+ivar)
            enddo
            nrvaro = nrvaro + nrvar
            nrvarn = nrvarn + nrvar
         endif
      enddo

      ! copy local output structure to argument

      deallocate(outputs%names)
      deallocate(outputs%stdnames)
      deallocate(outputs%pointers)
      deallocate(outputs%units)
      deallocate(outputs%descrs)
      allocate(outputs%names(nrvarn))
      allocate(outputs%stdnames(nrvarn))
      allocate(outputs%pointers(nrvarn))
      allocate(outputs%units(nrvarn))
      allocate(outputs%descrs(nrvarn))
      outputs%cursize  = nrvarn
      outputs%names(1:nrvarn) = outputl%names(1:nrvarn)
      outputs%stdnames(1:nrvarn) = ' ' ! outputl%stdnames(1:nrvarn)
      outputs%units(1:nrvarn) =    ' ' ! outputl%units(1:nrvarn)
      outputs%descrs(1:nrvarn) =   ' ' ! outputl%descrs(1:nrvarn)
      outputs%pointers(1:nrvarn) = outputl%pointers(1:nrvarn)

      ! deallocate local output structure

      deallocate(outputl%names)
      deallocate(outputl%stdnames)
      deallocate(outputl%pointers)
      deallocate(outputl%units)
      deallocate(outputl%descrs)

      if (timon) call timstop( ithndl )
      return
      end
