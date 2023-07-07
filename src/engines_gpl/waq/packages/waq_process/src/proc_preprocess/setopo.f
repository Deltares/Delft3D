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

      subroutine setopo ( procesdef, outputs, iloc  , idef  , iflx  ,
     +                    nowarn   )

      ! set output pointers

      use m_zoek
      use m_monsys
      use timers         !< performance timers
      use processet
      use output
      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      type(outputcoll)          :: outputs         ! output structure
      integer                   :: iloc            ! offset to local array
      integer                   :: idef            ! offset to default array
      integer                   :: iflx            ! offset to flux array
      integer                   :: nowarn          ! number of warnings

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      character(len=100)        :: line            ! line buffer for output
      integer                   :: ioutput         ! index output item
      integer                   :: i_input         ! index input item
      integer                   :: indx            ! index
      integer                   :: iou             ! loop counter output variable
      integer                   :: iou2            ! loop counter output variable
      character(len=20)         :: predef(3)       ! predefined names
      integer(4)                :: ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "setopo", ithndl )

      predef(1) = 'volume'
      predef(2) = 'itime'
      predef(3) = 'idt'

      ! set "output from active processes to output param" pointers

      write( line, '(a)' ) '# locating requested output from active processes'
      call monsys( line , 2 )
      line = ' '
      call monsys( line , 2 )

      nproc = procesdef%cursize

      do 300 iou = 1 , outputs%cursize

         ! check of deze al eerder aan de beurt is geweest

         call zoek( outputs%names(iou), iou-1 , outputs%names , 10 , iou2 )
         if ( iou2 .gt. 0 ) then

            ! if pointer the same get out ( can they be different except for -1 ?? )

            if ( outputs%pointers(iou) .eq. outputs%pointers(iou2) ) then
               goto 300
            endif

            ! if pointer -1  set pointer equal to iou2 and get out

            if ( outputs%pointers(iou) .eq. -1 ) then
               outputs%pointers(iou) = outputs%pointers(iou2)
               goto 300
            endif
         endif

         ! if this comes from proloc

         if ( outputs%pointers(iou) .gt. iloc ) then
            do iproc = 1, nproc
               proc => procesdef%procesprops(iproc)
               if ( proc%active ) then
                  call zoekio ( outputs%names(iou), proc%no_output, proc%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                  if ( ioutput .gt. 0 ) then

                     ! is this the one which is being used ?

                     if ( proc%output_item(ioutput)%ip_val .eq. outputs%pointers(iou) ) then
                        write ( line , '(5a)' ) ' output [',outputs%names (iou)(1:20),'] from proces [',proc%name(1:10),']'
                        call monsys( line , 4 )
                        goto 300
                     endif
                  endif

               endif

            enddo
         elseif ( outputs%pointers(iou) .eq. -1 ) then

            ! predefined ?

            call zoek ( outputs%names(iou), 3    , predef , 20   , indx  )
            if ( indx .eq. 1 ) then
               write(line,'(3a)') ' output [',outputs%names (iou)(1:20),'] using delwaq volume'
               call monsys( line , 4 )
               outputs%pointers(iou) = 1
               goto 300
            endif
            if ( indx .eq. 2 ) then
               write(line,'(3a)') ' output [',outputs%names (iou)(1:20),'] using delwaq itime'
               call monsys( line , 4 )
               outputs%pointers(iou) = 2
               goto 300
            endif
            if ( indx .eq. 3 ) then
               write(line,'(3a)') ' output [',outputs%names (iou)(1:20),'] using delwaq idt'
               call monsys( line , 4 )
               outputs%pointers(iou) = 3
               goto 300
            endif

            ! investigate wheter a default with this name is being used

            do iproc = 1, nproc
               proc => procesdef%procesprops(iproc)
               if ( proc%active ) then
                  call zoekio ( outputs%names(iou), proc%no_input, proc%input_item, 20, i_input, IOTYPE_SEGMENT_INPUT)
                  if ( i_input .gt. 0 ) then

                     ! is it a default ?

                     if ( proc%input_item(i_input)%ip_val .gt. idef .and.
     +                    proc%input_item(i_input)%ip_val .le. iflx      ) then
                        outputs%pointers(iou) = proc%input_item(i_input)%ip_val
                        write ( line , '(5a)' ) ' output [',outputs%names (iou)(1:20),'] default from [',proc%name,']'
                        call monsys( line , 4 )
                        goto 300
                     endif
                  endif

               endif
            enddo

            nowarn = nowarn + 1
            write (line,'(5a)') ' warning: output [',outputs%names (iou)(1:20),'] not located'
            call monsys( line , 4 )
         endif
  300 continue

      line = ' '
      call monsys( line , 4 )

      if (timon) call timstop( ithndl )
      return
      end
