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

      subroutine setopp ( procesdef, outputs, ioff  )

!>/File
!>      sets processes for requested output

      use m_monsys
      use timers         !< performance timers
      use processet      !< processet definitions
      use output         !< output definitions
      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       !< all processes
      type(outputcoll)          :: outputs         !< output structure
      integer                   :: ioff            !< offset to process output in waq data space

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      character(len=100)        :: line            ! line buffer for output
      integer                   :: ioutput         ! index output item
      integer                   :: iou             ! loop counter output variable
      integer(4)                :: ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "setopp", ithndl )

      ! set process on if output is requested and input ok

      write( line, '(a)' ) '# locating processes for requested output'
      call monsys( line , 2 )
      line = ' '
      call monsys( line , 2 )

      nproc = procesdef%cursize

      do 300 iou = 1 , outputs%cursize

         ! is the output undefined ( pointer -1 ) or from a proces

         if ( outputs%pointers(iou) .eq. -1 .or. outputs%pointers(iou) .gt. ioff ) then
            outputs%pointers(iou) = -1
            do 200 iproc = 1, nproc
               proc => procesdef%procesprops(iproc)
               if ( proc%linvok ) then
                  call zoekio ( outputs%names(iou), proc%no_output, proc%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                  if ( ioutput .gt. 0 ) then
                     if ( .not. proc%active ) then

                        ! turn proces on

                        proc%active = .true.

                        write ( line , '(5a)' ) ' switching [',proc%name(1:10),'] on for output [',outputs%names(iou)(1:20),']'
                        call monsys( line , 4 )
                        line = ' '
                        call monsys( line , 4 )
                     endif

                     goto 300
                  endif

               endif

  200       continue
         endif

  300 continue

      if (timon) call timstop( ithndl )
      return
      end
