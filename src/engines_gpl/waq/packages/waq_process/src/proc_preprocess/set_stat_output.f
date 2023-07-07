!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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

      subroutine set_stat_output( statprocesdef, noutp , ioutps, nrvart, outputs)

!     Deltares Software Centre

!>/File
!>      set the output of the statistical processes in output structure

!     Created   : Aug   2012 by Jan van Beek

      use timers         !< performance timers
      use processet      !< use processet definitions
      use output         !< use output definitions
      implicit none

      ! arguments

      type(procespropcoll), intent(in   ) :: statprocesdef          !< the statistical proces definition
      integer             , intent(in   ) :: noutp                  !< total number of output files
      integer             , intent(inout) :: ioutps(7,*)            !< (old) output structure
      integer             , intent(inout) :: nrvart                 !< total number of output parameters
      type(outputcoll)    , intent(inout) :: outputs                !< output structure

      ! local

      integer             , allocatable   :: iopoi3(:)              !  pointer in the delwaq arrays
      character(len=20)   , allocatable   :: ounam3(:)              !  name of the output variables
      type(arraypropcoll)                 :: normal_output          !  normal output list
      type(arraypropcoll)                 :: stat_output            !  statistical output list
      type(arrayprop)                     :: aarrayprop             !  one array property  to add into collection
      integer                             :: ioutp                  !  index variable
      integer                             :: iout                   !  index variable
      integer                             :: iout1                  !  index variable
      integer                             :: iout3                  !  index variable
      integer                             :: nrvarx                 !  number of extra variables
      integer                             :: nrvar1                 !  number of variables
      integer                             :: nrvar2                 !  number of variables
      integer                             :: istat                  !  loop counter statistical processes
      integer                             :: iioitem                !  loop counter io items
      integer                             :: iret                   !  function return code
      integer                             :: ierr                   !  allocate error code
      
      integer(4)                          :: ithndl = 0             !  handle for performance timer
      if (timon) call timstrt( "set_stat_output", ithndl )

      ! merge the statistical output with the normal output

      normal_output%cursize = 0
      normal_output%maxsize = 0
      stat_output%cursize = 0
      stat_output%maxsize = 0

      if ( statprocesdef%cursize .gt. 0 ) then
         do istat = 1 , statprocesdef%cursize
            do iioitem = 1 , statprocesdef%procesprops(istat)%no_output
               if ( statprocesdef%procesprops(istat)%output_item(iioitem)%type .eq. iotype_segment_output ) then
                  aarrayprop%name = statprocesdef%procesprops(istat)%output_item(iioitem)%name
                  if ( statprocesdef%procesprops(istat)%type .eq. procestype_stat ) then
                     iret = arraypropcolladd( stat_output, aarrayprop )
                  else
                     iret = arraypropcolladd( normal_output, aarrayprop )
                  endif
               endif
            enddo
         enddo
         do ioutp = 1 , noutp - 2

            ! check if there are weigth variables

            if ( ioutps(5,ioutp) .eq. imo3 .or. ioutps(5,ioutp) .eq. imo4 .or.
     +           ioutps(5,ioutp) .eq. ihi3 .or. ioutps(5,ioutp) .eq. ihi4 .or.
     +           ioutps(5,ioutp) .eq. ihn3 .or. ioutps(5,ioutp) .eq. ihn4      ) then
               nrvarx = normal_output%cursize*2
            else
               nrvarx = normal_output%cursize
            endif

            ! add statistical vars to normal output

            if ( ioutps(5,ioutp) .eq. imon .or. ioutps(5,ioutp) .eq. imo2 .or.
     +           ioutps(5,ioutp) .eq. imo3 .or. ioutps(5,ioutp) .eq. imo4 .or.
     +           ioutps(5,ioutp) .eq. idmp .or. ioutps(5,ioutp) .eq. idm2 .or.
     +           ioutps(5,ioutp) .eq. ihis .or. ioutps(5,ioutp) .eq. ihi2 .or.
     +           ioutps(5,ioutp) .eq. ihi3 .or. ioutps(5,ioutp) .eq. ihi4 .or.
     +           ioutps(5,ioutp) .eq. ihnf .or. ioutps(5,ioutp) .eq. ihn2 .or.
     +           ioutps(5,ioutp) .eq. ihn3 .or. ioutps(5,ioutp) .eq. ihn4 .or.
     +           ioutps(5,ioutp) .eq. imap .or. ioutps(5,ioutp) .eq. ima2 .or.
     +           ioutps(5,ioutp) .eq. imnf .or. ioutps(5,ioutp) .eq. imn2      ) then
               nrvart = nrvart + nrvarx
            endif
         enddo
         nrvart = nrvart + stat_output%cursize*3

         allocate(iopoi3(nrvart))
         allocate(ounam3(nrvart))

         ! handle output from statistical processes

         ! for existing active files add the parameters which are not defined on a period for
         ! all but balance file. check whether there is a weight variable.
         ! check the buffer size with routine outboo moved from elsewhere

         iout1 = 0
         iout3 = 0
         do ioutp = 1 , noutp - 2

            ! check if there are weigth variables

            if ( ioutps(5,ioutp) .eq. imo3 .or. ioutps(5,ioutp) .eq. imo4 .or.
     +           ioutps(5,ioutp) .eq. ihi3 .or. ioutps(5,ioutp) .eq. ihi4 .or.
     +           ioutps(5,ioutp) .eq. ihn3 .or. ioutps(5,ioutp) .eq. ihn4      ) then
               nrvar1 = ioutps(4,ioutp)/2
               nrvar2 = ioutps(4,ioutp)/2
            else
               nrvar1 = ioutps(4,ioutp)
               nrvar2 = 0
            endif

            ! original vars

            do iout = 1 , nrvar1
               iout1 = iout1 + 1
               iout3 = iout3 + 1
               ounam3(iout3) = outputs%names   (iout1)
               iopoi3(iout3) = outputs%pointers(iout1)
            enddo

            ! add statistical vars to normal output

            if ( ioutps(5,ioutp) .eq. imon .or. ioutps(5,ioutp) .eq. imo2 .or.
     +           ioutps(5,ioutp) .eq. imo3 .or. ioutps(5,ioutp) .eq. imo4 .or.
     +           ioutps(5,ioutp) .eq. idmp .or. ioutps(5,ioutp) .eq. idm2 .or.
     +           ioutps(5,ioutp) .eq. ihis .or. ioutps(5,ioutp) .eq. ihi2 .or.
     +           ioutps(5,ioutp) .eq. ihi3 .or. ioutps(5,ioutp) .eq. ihi4 .or.
     +           ioutps(5,ioutp) .eq. ihnf .or. ioutps(5,ioutp) .eq. ihn2 .or.
     +           ioutps(5,ioutp) .eq. ihn3 .or. ioutps(5,ioutp) .eq. ihn4 .or.
     +           ioutps(5,ioutp) .eq. imap .or. ioutps(5,ioutp) .eq. ima2 .or.
     +           ioutps(5,ioutp) .eq. imnf .or. ioutps(5,ioutp) .eq. imn2      ) then
               ioutps(4,ioutp) = ioutps(4,ioutp) + normal_output%cursize
               do iout = 1 , normal_output%cursize
                  iout3 = iout3 + 1
                  ounam3(iout3) = normal_output%arrayprops(iout)%name
                  iopoi3(iout3) = -1
               enddo
            endif

            ! weigth variables original vars

            do iout = 1 , nrvar2
               iout1 = iout1 + 1
               iout3 = iout3 + 1
               ounam3(iout3) = outputs%names   (iout1)
               iopoi3(iout3) = outputs%pointers(iout1)
            enddo

            ! add weight variables for statistical vars to normal output

            if ( ioutps(5,ioutp) .eq. imo3 .or. ioutps(5,ioutp) .eq. imo4 .or.
     +           ioutps(5,ioutp) .eq. ihi3 .or. ioutps(5,ioutp) .eq. ihi4 .or.
     +           ioutps(5,ioutp) .eq. ihn3 .or. ioutps(5,ioutp) .eq. ihn4      ) then
               ioutps(4,ioutp) = ioutps(4,ioutp) + normal_output%cursize
               do iout = 1 , normal_output%cursize
                  iout3 = iout3 + 1
                  ounam3(iout3) = 'volume'
                  iopoi3(iout3) = -1
               enddo
            endif
         enddo

         ! the output files for statistical output defined on periods (8 = a map file, 9 = a mon file)

         ioutps(4,8) = stat_output%cursize
         do iout = 1 , stat_output%cursize
            iout3 = iout3 + 1
            ounam3(iout3) = stat_output%arrayprops(iout)%name
            iopoi3(iout3) = -1
         enddo
         ioutps(4,9) = stat_output%cursize*2
         do iout = 1 , stat_output%cursize
            iout3 = iout3 + 1
            ounam3(iout3) = stat_output%arrayprops(iout)%name
            iopoi3(iout3) = -1
         enddo
         do iout = 1 , stat_output%cursize
            iout3 = iout3 + 1
            ounam3(iout3) = 'volume'
            iopoi3(iout3) = -1
         enddo

         ! put the local arrays in the output structure

         deallocate(Outputs%names, Outputs%pointers, Outputs%stdnames, Outputs%units, Outputs%descrs, STAT=ierr)
         allocate(Outputs%names(nrvart), Outputs%pointers(nrvart), Outputs%stdnames(nrvart),
     &          Outputs%units(nrvart), Outputs%descrs(nrvart), STAT=ierr)
         outputs%cursize = nrvart
         outputs%pointers(1:nrvart) = iopoi3(1:nrvart)
         outputs%names(1:nrvart)    = ounam3(1:nrvart)

      endif

      if ( ioutps(4,8) .eq. 0 ) ioutps(5,8) = 0
      if ( ioutps(4,9) .eq. 0 ) ioutps(5,9) = 0

      if (timon) call timstop( ithndl )
      return
      end
