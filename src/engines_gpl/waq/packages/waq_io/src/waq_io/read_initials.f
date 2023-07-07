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

      subroutine read_initials ( lun    , lchar  , filtype, inpfil , notot ,
     &                           syname , iwidth , ioutpt , gridps , noseg ,
     &                           conc   , ierr   , iwar   )

!     Deltares Software Centre

!     function : read block 8 of input, initial condtions keyword type of input

!     global declarations

      use m_srstop
      use grids          ! for the storage of contraction grids
      use dlwq_data      ! for definition and storage of data
      use rd_token
      use timers       !   performance timers

      implicit none

!     declaration of arguments

      integer               , intent(in)    :: lun(*)       ! unit numbers used
      character(len=*)      , intent(in)    :: lchar(*)     ! filenames
      integer  ( 4)         , intent(inout) :: filtype(*)   !< type of binary file
      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack and flags
      integer               , intent(in)    :: notot        ! nr of substances
      character(len=*)      , intent(in)    :: syname(*)    ! substance names
      integer               , intent(in)    :: iwidth       ! width of output
      integer               , intent(in)    :: ioutpt       ! level of reporting to ascii output file
      type(gridpointercoll) , intent(in)    :: gridps       ! collection off all grid definitions
      integer               , intent(in)    :: noseg        ! nr of segments
      real                  , intent(inout) :: conc(notot,noseg)  ! initial conditions
      integer               , intent(inout) :: ierr         !< cummulative error count
      integer  ( 4)         , intent(inout) :: iwar         !< cumulative warning count

!     local declarations

      type(t_dlwqdatacoll)                 :: initials             ! all the initial data from file
      type(t_dlwqdata)                     :: dlwqdata             ! one data block
      type(t_dlwq_item)                    :: substances           ! delwaq substances list
      type(t_dlwq_item)                    :: constants            ! delwaq constants list
      type(t_dlwq_item)                    :: parameters           ! delwaq parameters list
      type(t_dlwq_item)                    :: functions            ! delwaq functions list
      type(t_dlwq_item)                    :: segfuncs             ! delwaq segment-functions list
      type(t_dlwq_item)                    :: segments             ! delwaq segment name list
      real, allocatable                    :: fscale(:)            ! scale factors
      character(len=255)                   :: ctoken               ! token from input
      integer                              :: idata                ! index in collection
      integer                              :: itype                ! type of the token
      integer                              :: ierr2                ! error from gettoken and others
      integer                              :: ierr3                ! error from dlwqdataevaluate
      integer                              :: itime                ! dummy time
      integer                              :: i                    ! loop counter
      integer                              :: idummy               ! dummy
      real                                 :: rdummy               ! dummy
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "read_initials", ithndl )


      ! read initial conditions

      initials%maxsize = 0
      initials%cursize = 0
      ierr2 = dlwq_init(substances)
      ierr2 = dlwq_resize(substances,notot)
      substances%no_item = notot
      substances%name(1:notot) = syname(1:notot)
      ierr2 = dlwq_init(constants)
      ierr2 = dlwq_init(parameters)
      ierr2 = dlwq_init(functions)
      ierr2 = dlwq_init(segfuncs)
      ierr2 = dlwq_init(segments)
      ierr2 = dlwq_resize(segments,noseg)
      segments%no_item = noseg
      do i = 1 , noseg
         write ( segments%name(i), '(''segment '',i8)' ) i
      enddo

      lunut = lun(29)
      ierr2 = 0

      do

         if ( gettoken(ctoken,idummy,rdummy,itype,ierr2) .ne. 0 ) exit

         if ( ctoken .eq. 'INITIALS' ) then

            ! new file strucure

            push = .true.
            call read_block ( lun       , lchar     , filtype  , inpfil    , ioutpt   ,
     &                        iwidth    , substances, constants, parameters, functions,
     &                        segfuncs  , segments  , gridps    , dlwqdata , ierr2    ,
     &                        iwar      )
            if ( ierr2 .gt. 0 ) goto 30
            if ( dlwqdata%extern ) then
               ierr = dlwqdataReadExtern(lunut,dlwqdata)
               if ( ierr .ne. 0 ) goto 30
               dlwqdata%extern = .false.
            endif
            idata = dlwqdatacolladd( initials, dlwqdata )

         else

            ! unrecognised keyword

            if ( ctoken(1:1) .ne. '#' ) then
               write ( lunut , 2050 ) trim(ctoken)
               ierr = ierr + 1
               goto 30
            else
               ierr2 = 2
               exit
            endif

         endif

         ! jvb? first_token = .false.

      enddo

!     do not write but evaluate and pass back

      conc  = 0.0
      itime = 0

      do idata = 1 , initials%cursize
         ierr3 = dlwqdataevaluate(initials%dlwqdata(idata),gridps,itime,notot,noseg,conc)
         if ( ierr3 .ne. 0 ) then
            write(lunut,2060)
            call srstop(1)
         endif
      enddo

!     initials opruimen ? or keep for e.g. reboot, or keep for delwaq1-delwaq2 merge

      ierr3 = dlwq_cleanup(substances)
      ierr3 = dlwq_cleanup(parameters)
      ierr3 = dlwq_cleanup(functions)
      ierr3 = dlwq_cleanup(segfuncs)
      ierr3 = dlwq_cleanup(segments)

   30 continue
      if ( ierr2 .gt. 0 .and. ierr2 .ne. 2 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      call check( ctoken, iwidth, 8     , ierr2  , ierr  )
      if (timon) call timstop( ithndl )
      return

      ! output formats

 2050 format (/' ERROR, unrecognized token: ',A)
 2060 format (/' ERROR: evaluating initial conditions')
      end
