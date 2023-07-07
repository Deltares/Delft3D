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

      module partable
      use m_zoek
      use m_monsys


! NB This is a module, because the subroutine partab allocates the array proref
!                      that is used outside of partab, through dlwqp1 in wripro.
!                      The alternative is to construct an explicit interface to
!                      the routine partab in dlwqp1. This method seems simpler.

      contains

      subroutine partab ( procesdef, notot  , syname, nocons, constants,                           &
     &                    nopa     , paname , nofun , funame, nosfun   ,                           &
     &                    sfname   , proref , nrref , nowarn, nothread ,                           &
     &                    nopred   , noloc , nodef )

!     Deltares Software Department

!     Created   : March 2010 by Leo Postma

!     Function  : Makes a parallel processing reference table to ensure
!                 integrity at runtime of required process input.
!                 Procesdef is sorted to contain all processes in optimal
!                 calling order, the inactive processes at the end.
!                 Proref(nrref,nproc) contains the reference information
!                 for the active processes only, so nproc < procesdef%cursize

!     Modified  :

      use dlwq_data
      use ProcesSet
      use timers       !   performance timers

      implicit none

!     Arguments           :

!     Kind                  Function         Name                  Description

      type(ProcesPropColl), intent(inout) :: procesdef       ! all processes
      integer             , intent(in   ) :: notot           ! number of substances
      character(20)       , intent(in   ) :: syname(notot)   ! substance names
      integer             , intent(in   ) :: nocons          ! number of constants
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
      integer             , intent(in   ) :: nopa            ! number of parameters
      character(20)       , intent(in   ) :: paname(nopa)    ! parameter names
      integer             , intent(in   ) :: nofun           ! number of functions
      character(20)       , intent(in   ) :: funame(nofun)   ! function names
      integer             , intent(in   ) :: nosfun          ! number of segment functions
      character(20)       , intent(in   ) :: sfname(nosfun)  ! segment function names
      integer, pointer    , intent(  out) :: proref(:,:)     ! input items to be resolved for each process
      integer             , intent(  out) :: nrref           ! maximum nr of references to be resolved
      integer             , intent(inout) :: nowarn          ! number of warnings
      integer             , intent(inout) :: nothread        ! number of threads to be used
      integer             , intent(in   ) :: nopred          ! number of predefined items
      integer             , intent(in   ) :: noloc           ! number of items in local array
      integer             , intent(in   ) :: nodef           ! number of items in default array

!     Local declarations

      integer                                noproc          ! nr of processes ( = ProcesDef%cursize )
      integer                                iproc1, iproc2  ! process loop counters
      type(ProcesProp)    , pointer       :: proc1           ! the process with sequence nr iproc1
      type(ProcesProp)    , pointer       :: proc2           ! the process with sequence nr iproc2
      integer                                iout            ! loop counter for process outputs
      integer                                iin             ! loop counter for process inputs
      integer                                nitem           ! number of needed items
      integer                                ioff            ! offset to flux array
      integer                                nfl             ! number of fluxes till this process
      character(100)                         line            ! output buffer
      integer                                iprocs          ! counter of the saved ordered processes
      integer                                nproc           ! incremental start value or process ordering
      integer                                naproc          ! nr of active processes
      integer                                ifound          ! result of search routine >0 if found
      integer                                k               ! help variable
      integer             , allocatable   :: profreq(:)      ! nr of used forward references per process
      integer             , allocatable   :: prorder(:)      ! final order of execution of processes
      integer             , allocatable   :: needed (:)      ! nr of needer backward references pewr process
      integer             , allocatable   :: work   (:)      ! work array to sort the processes
      type(ProcesProp)    , allocatable   :: cProces(:)      ! work array to rearrange the processes
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "partab", ithndl )

!         initial allocations

      noproc = ProcesDef%cursize
      allocate ( prorder(noproc), profreq(noproc) )
      allocate ( work   (noproc), needed (noproc) )
      allocate ( cProces(noproc)                  )

!         make an array with the number of used forward references per process

      profreq = 0
      naproc  = 0
      do iproc1 = 1 , noproc
         proc1 => procesdef%procesprops(iproc1)
         if ( proc1%active ) then
            naproc = naproc + 1
            nitem = 0
            do iout = 1 , proc1%no_output
               do iproc2 = iproc1+1 , noproc                        ! they are already sorted before
                  proc2 => procesdef%procesprops(iproc2)
                  if ( proc2%active ) then
                     do iin = 1 , proc2%no_input
                        if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                paname, nofun , funame, nosfun   , sfname,                &
     &                                proc2%input_item(iin)%name,                               &
     &                                proc1%output_item(iout)%name) .gt. 0 ) then
                           nitem = nitem + 1
                        endif
                     enddo
                  endif
               enddo
            enddo
            do iout = 1 , proc1%no_fluxoutput
               do iproc2 = iproc1+1 , noproc                        ! they are already sorted before
                  proc2 => procesdef%procesprops(iproc2)
                  if ( proc2%active ) then
                     do iin = 1 , proc2%no_input
                        if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                paname, nofun , funame, nosfun   , sfname,                &
     &                                proc2%input_item(iin)%name,                               &
     &                                proc1%fluxoutput(iout)%name) .gt. 0 ) then
                           nitem = nitem + 1
                        endif
                     enddo
                  endif
               enddo
            enddo
            profreq(iproc1) = nitem
         endif
      enddo

!         determine cyclicly which processes need no further input

      nproc  = 1
      iprocs = 0
      needed = 0
      nrref  = 0
      do while ( nproc .le. noproc )
         do iproc1 = 1 , noproc                                     ! count backward references
            proc1 => procesdef%procesprops(iproc1)
            if ( proc1%active ) then
               if ( needed(iproc1) .eq. -1 ) cycle                  ! this process is already ordered
               nitem = 0
               do iin = 1 , proc1%no_input
                  do iproc2 = 1 , iproc1-1                          ! only the previous processes can provide
                     proc2 => procesdef%procesprops(iproc2)
                     if ( proc2%active ) then
                        if ( needed(iproc2) .eq. -1 ) cycle
                        do iout = 1 , proc2%no_output
                           if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                   paname, nofun , funame, nosfun   , sfname,                &
     &                                   proc1%input_item(iin)%name,                               &
     &                                   proc2%output_item(iout)%name) .gt. 0 ) then
                              nitem = nitem + 1
                           endif
                        enddo
                        do iout = 1 , proc2%no_fluxoutput
                           if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                   paname, nofun , funame, nosfun   , sfname,                &
     &                                   proc1%input_item(iin)%name,                               &
     &                                   proc2%fluxoutput(iout)%name) .gt. 0 ) then
                              nitem = nitem + 1
                           endif
                        enddo
                     endif
                  enddo
               enddo
               if ( nitem .eq. 0 ) then                             ! save the processes that are able to run
                  iprocs  = iprocs + 1                              ! in separate arrays
                  prorder(iprocs) = iproc1
                  work   (iprocs) = profreq(iproc1)
               else
                  nrref = max ( nrref, nitem )                      ! determine the maximum of dependencies
               endif
            endif
         enddo
         if ( nproc .eq. iprocs+1 ) exit                            ! no further progress made
         do iproc1 = nproc , iprocs                                 ! simple bubblesort of those that can run
            do iproc2 = iprocs, iproc1+1, -1                        ! to get those with hihest forward refs first
               if ( work(iproc2) .gt. work(iproc2-1) ) then
                  iin = work   (iproc2-1)
                  work   (iproc2-1) = work   (iproc2)
                  work   (iproc2  ) = iin
                  iin = prorder(iproc2-1)
                  prorder(iproc2-1) = prorder(iproc2)
                  prorder(iproc2  ) = iin
               endif
            enddo
         enddo
         iin  = 0
         iout = 0
         do iproc1 = nproc , iprocs
            needed(prorder(iproc1)) = -1                            ! mark these processes dealt with
            if ( profreq(prorder(iproc1)) .eq. 0 ) then             ! take care that at the end there are
               iout = iout+1                                        ! at least 5 without forward reference
               if ( iout .eq. 5 ) then                              ! Is 5 enough to prevent the running of a
                  iin = iproc1                                      ! process that has not got its input resolved
                  exit                                              ! yet during parallel processing ?
               endif                                                ! You can take a larger value here, but then
            endif                                                   ! you might run out of separators later on.
         enddo                                                      ! There will at least be a check at runtime
         if ( iin .ne. 0 ) iprocs = iin
         nproc = iprocs+1
      enddo
      if ( nothread .gt. 1 ) then

!         if at the end there are still processes left, then add them at the end with a warning

         do iproc1 = 1, noproc
            proc1 => procesdef%procesprops(iproc1)
            if ( proc1%active ) then
               if ( needed(iproc1) .eq. -1 ) cycle
               iprocs  = iprocs + 1
               prorder(iprocs) = iproc1
               write(line,'(a,a)') ' WARNING: possibly unresolved input for process: ',               &
     &                                                      ProcesDef%ProcesProps(iproc1)%name
               call monsys( line , 2 )
               nowarn = nowarn + 1
            endif
         enddo

!         add the inactive processes after that

         iin   = iprocs
         nproc = 0
         do iproc1 = 1, noproc
            proc1 => procesdef%procesprops(iproc1)
            if ( proc1%active ) then
               nproc = nproc + 1
            else
               iprocs  = iprocs + 1
               prorder(iprocs) = iproc1
            endif
         enddo
         if ( nproc .ne. iin ) then
            write(line,'(a,2i5)') ' ERROR: no match in number of active processes: ',nproc,iin
            call monsys( line , 2 )
         endif
      else
         iprocs = 0
         do iproc1 = 1, noproc
            proc1 => procesdef%procesprops(iproc1)
            if ( proc1%active ) then
               iprocs  = iprocs + 1
               prorder(iprocs) = iproc1
            endif
         enddo
         do iproc1 = 1, noproc
            proc1 => procesdef%procesprops(iproc1)
            if ( .not. proc1%active ) then
               iprocs  = iprocs + 1
               prorder(iprocs) = iproc1
            endif
         enddo
      endif

!         put the processes in the right order

      do iproc1 = 1 , noproc
         cProces(iproc1) = ProcesDef%ProcesProps(prorder(iproc1))
      enddo
      do iproc1 = 1 , noproc
         ProcesDef%ProcesProps(iproc1) = cProces(iproc1)
      enddo

!         make the refrence table: proref

      if ( nrref .eq. 0 ) nrref = 1
      allocate ( proref( nrref, naproc ) )
      proref = 0
      do iproc1 = 1 , naproc
         proc1 => procesdef%procesprops(iproc1)
         if ( proc1%active ) then
            nitem = 0
            do iin = 1 , proc1%no_input
               do iproc2 = 1 , noproc
                  if ( iproc2 .eq. iproc1 ) cycle
                  proc2 => procesdef%procesprops(iproc2)
                  if ( proc2%active ) then
                     do iout = 1 , proc2%no_output
                        if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                paname, nofun , funame, nosfun   , sfname,                &
     &                                proc1%input_item (iin )%name,                             &
     &                                proc2%output_item(iout)%name) .gt. 0 ) then
                           nitem = nitem + 1
                           proref(nitem,iproc1) = iproc2
                        endif
                     enddo
                     do iout = 1 , proc2%no_fluxoutput
                        if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                paname, nofun , funame, nosfun   , sfname,                &
     &                                proc1%input_item(iin )%name,                              &
     &                                proc2%fluxoutput(iout)%name) .gt. 0 ) then
                           nitem = nitem + 1
                           proref(nitem,iproc1) = iproc2
                        endif
                     enddo
                  endif
               enddo
            enddo
         endif
      enddo

!         update flux pointers to new order

      ioff   = nopred + nocons + nopa + nofun + nosfun + notot + noloc + nodef
      do iproc1 = 1 , naproc
         proc1 => procesdef%procesprops(iproc1)
         if ( proc1%active ) then
            do iin = 1 , proc1%no_input
               nfl = 0
               do iproc2 = 1 , noproc
                  proc2 => procesdef%procesprops(iproc2)
                  if ( proc2%active ) then
                     if ( iproc2 .ne. iproc1 ) then
                        do iout = 1 , proc2%no_fluxoutput
                           if ( isinput( notot , syname, nocons, constants, nopa  ,                &
     &                                   paname, nofun , funame, nosfun   , sfname,                &
     &                                   proc1%input_item(iin )%name,                              &
     &                                   proc2%fluxoutput(iout)%name) .gt. 0 ) then
                              proc1%input_item(iin)%ip_val = ioff + nfl + iout
                           endif
                        enddo
                     endif
                     nfl = nfl + proc2%no_fluxoutput
                  endif
               enddo
            enddo
         endif
      enddo

      if (timon) call timstop( ithndl )
      return
      end subroutine partab

      integer function isinput( notot , syname, nocons, constants, nopa  ,                &
     &                          paname, nofun , funame, nosfun   , sfname,                &
     &                          valnam, input )

      use dlwq_data

      character(20)       , intent(in   ) :: valnam
      character(20)       , intent(in   ) :: input

      integer             , intent(in   ) :: notot           ! number of substances
      character(20)       , intent(in   ) :: syname(notot)   ! substance names
      integer             , intent(in   ) :: nocons          ! number of constants
      type(t_dlwq_item)   , intent(inout) :: constants       ! delwaq constants list
      integer             , intent(in   ) :: nopa            ! number of parameters
      character(20)       , intent(in   ) :: paname(nopa)    ! parameter names
      integer             , intent(in   ) :: nofun           ! number of functions
      character(20)       , intent(in   ) :: funame(nofun)   ! function names
      integer             , intent(in   ) :: nosfun          ! number of segment functions
      character(20)       , intent(in   ) :: sfname(nosfun)  ! segment function names
!
      integer                                status          ! value to be returned by function
      integer                                ifound          ! result of search routine >0 if found
      integer                                ivalip          ! >0 if valnam was a valid item in user input
      character(20)                          locnam          ! local copy of valnam
      character(100)                         line            ! output buffer
!
      locnam = valnam
      status = -1
      do while (status .eq. -1)
         call valpoi ( notot , nopa     , nosfun, syname, nocons,                &
     &                 nofun , constants, paname, funame, sfname,                &
     &                 locnam, ivalip, line  )
         if ( ivalip .ne. -1 ) then
            status = 0
         else
            call zoek( locnam,  1, input, 20, ifound)
            if ( ifound .gt. 0 ) then
               status = 1
            else
               ! if name contains star, remove it
               i_star = index(locnam,'*')
               if ( i_star .gt. 1 ) then
                  locnam(i_star:) = ' '
               else
                  status = 0
               endif
            endif
         endif
      enddo
      isinput = status
      return
      end function isinput

      end module partable
