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
      module m_dlwq07
      use m_read_block


      implicit none

      contains


      subroutine dlwq07 ( lun    , lchar  , filtype, inpfil   , syname ,
     &                    iwidth , ioutpt , gridps , constants, chkpar ,
     &                    ierr   , iwar   )

!     Deltares Software Centre

!>\File
!>               Reads block 7 of input, process parameters

!     Global declarations

      use m_check
      use m_zoek
      use m_srstop
      use m_open_waq_files
      use dlwqgrid_mod   ! for the storage of contraction grids
      use dlwq_hyd_data  ! for definition and storage of data
      use rd_token       ! tokenized reading
      use partmem, only: alone, lsettl, layt        ! for the interface with Delpar (Tau and VertDisp)
      use timers       !   performance timers
      use m_sysn
      implicit none

!     implicit none

      include 'omp_lib.h'

!     declaration of arguments

!     kind                    function         name           Descriptipon

      integer               , intent(inout) :: lun(*)       !< unit numbers used
      character(len=*)      , intent(inout) :: lchar(*)     !< filenames
      integer  ( 4)         , intent(inout) :: filtype(*)   !< type of binary file
      type(inputfilestack)  , intent(inout) :: inpfil       !< input file structure with include stack and flags
      character(len=*)      , intent(in   ) :: syname(*)    !< substance names
      integer               , intent(in   ) :: iwidth       !< width of output
      integer               , intent(in   ) :: ioutpt       !< level of reporting to ascii output file
      type(GridPointerColl) , intent(in   ) :: GridPs       !< collection off all grid definitions
      type(t_dlwq_item)     , intent(inout) :: constants    !< delwaq constants list
      logical               , intent(in)    :: chkpar(2)    !< check for SURF and LENGTH
      integer               , intent(inout) :: ierr         !< cummulative error count
      integer  ( 4)         , intent(inout) :: iwar         !< cumulative warning count

!     local declarations

      type(t_dlwqdatacoll)                 :: proc_pars            ! all the process parameters data from file
      type(t_dlwqdata)                     :: dlwqdata             ! one data block
      type(t_dlwq_item)                    :: substances           ! delwaq substances list
      type(t_dlwq_item)                    :: parameters           ! delwaq parameters list
      type(t_dlwq_item)                    :: functions            ! delwaq functions list
      type(t_dlwq_item)                    :: segfuncs             ! delwaq segment-functions list
      type(t_dlwq_item)                    :: segments             ! delwaq segments
      character(len=255)                   :: ctoken               ! token from input
      character(len=20)                    :: ch20                 ! name
      integer                              :: itime                ! time in scu (dummy used for constants)
      integer                              :: nosss                ! total number of segments (water and bottom)
      integer                              :: ierr2                ! error indicator
      integer                              :: ierr3                ! error indicator
      integer                              :: ioerr                ! IO - error indicator
      integer                              :: inovec               ! location of NOVEC
      integer                              :: inothr               ! location of NOTHREADS
      integer                              :: i                    ! loop counter
      integer                              :: idata                ! help variable
      logical                              :: taupart              ! is tau present?
      logical                              :: vdfpart              ! is vertical diffusion present
      integer                              :: special              ! index of special parameters
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq07", ithndl )

!        Read initial conditions

      proc_pars%maxsize = 0
      proc_pars%cursize = 0
      ierr2 = dlwq_init(substances)
      ierr2 = dlwq_resize(substances,notot)
      substances%no_item = notot
      substances%name(1:notot) = syname(1:notot)
      ierr2 = dlwq_init(constants)
      ierr2 = dlwq_init(parameters)
      ierr2 = dlwq_init(functions)
      ierr2 = dlwq_init(segfuncs)

      nosss = noseg + nseg2
      ierr2 = dlwq_init(segments)
      ierr2 = dlwq_resize(segments,nosss)
      segments%no_item = nosss
      do i = 1 , nosss
         write ( segments%name(i), '(''segment '',i8)' ) i
      enddo

      IERR2  = 0
      nothrd = 1
      taupart = .false.
      vdfpart = .false.

      do

         if ( gettoken(ctoken,ierr2) .ne. 0 ) exit

         if ( ctoken .eq. 'CONSTANTS'     .or.
     +        ctoken .eq. 'FUNCTIONS'     .or.
     +        ctoken .eq. 'PARAMETERS'    .or.
     +        ctoken .eq. 'SEG_FUNCTIONS'      ) then

            ! new file strucure

            push = .true.
            call read_block ( lun       , lchar     , filtype   , inpfil    , ioutpt   ,
     &                        iwidth    , substances, constants , parameters, functions,
     &                        segfuncs  , segments  , gridps    , dlwqdata  , ierr2    ,
     &                        iwar      )
            
            if ( ierr2 .gt. 0 ) goto 30
            
            ! check for special constants, get directly from structure (ignore order, scaling etc this is not clean)

            if ( dlwqdata%subject .eq. SUBJECT_CONSTANT ) then
               ch20 = 'NOVEC'
               call zoek( ch20 , dlwqdata%no_param, dlwqdata%param_name, 20 , inovec)
               if ( inovec .gt. 0 ) then
                  novec = nint(dlwqdata%values(inovec,1,1))
                  write(lunut,2240)
                  write(lunut,2250) novec
               endif
               ch20 = 'NOTHREADS'
               call zoek( ch20 , dlwqdata%no_param, dlwqdata%param_name, 20 , inothr)
               if ( inothr .gt. 0 ) then
                  nothrd = nint(dlwqdata%values(inothr,1,1))
                  write(lunut,2310)
                  write(lunut,2320) nothrd
                  if ( nothrd .gt. 0 ) call omp_set_num_threads( nothrd )
                  nothrd = omp_get_max_threads()
               endif
            endif
            ch20 = 'TAU'
            call zoek( ch20 , dlwqdata%no_param, dlwqdata%param_name, 20 , inovec)
            if ( inovec .gt. 0 ) taupart = .true.
            ch20 = 'VERTDISPER'
            call zoek( ch20 , dlwqdata%no_param, dlwqdata%param_name, 20 , inovec)
            if ( inovec .gt. 0 ) vdfpart = .true.

            ! add to the collection

            idata = dlwqdataCollAdd( proc_pars, dlwqdata )

         else

            ! unrecognised keyword

            if ( ctoken(1:1) .ne. '#' ) then
               write ( lunut , 2040 ) trim(ctoken)
               ierr = ierr + 1
               goto 30
            else
               ierr2 = 2
               exit
            endif

         endif

      enddo
      if ( .not. alone ) then              ! Delwaq runs with Delpar
         if ( lsettl .or. layt .gt. 1 ) then
            if ( taupart ) then
               write ( lunut, 2330 )
            else
               write ( lunut, 2340 )
               iwar = iwar + 1
            endif
            if ( layt .gt. 1 ) then
               if ( vdfpart ) then
                  write ( lunut, 2350 )
               else
                  write ( lunut, 2360 )
                  iwar = iwar + 1
               endif
            endif
         endif
      endif

      ! write to output and report files

      nocons = constants%no_item
      nopa   = parameters%no_item
      nofun  = functions%no_item
      nosfun = segfuncs%no_item

      write ( lunut , 2050 ) constants%no_item
      write ( lunut , 2060 ) parameters%no_item
      write ( lunut , 2070 ) functions%no_item
      write ( lunut , 2080 ) segfuncs%no_item
      if ( constants%no_item  .gt. 0 ) write ( lun(2) ) (constants%name(i) , i=1, constants%no_item)
      if ( parameters%no_item .gt. 0 ) write ( lun(2) ) (parameters%name(i), i=1, parameters%no_item)
      if ( functions%no_item  .gt. 0 ) write ( lun(2) ) (functions%name(i) , i=1, functions%no_item)
      if ( segfuncs%no_item   .gt. 0 ) write ( lun(2) ) (segfuncs%name(i)  , i=1, segfuncs%no_item)
      
      
      call open_waq_files  ( lun(16) , lchar(16) , 16    , 1     , ioerr )
      write(lun(16)) ' 5.000PROCES'
      write(lun(16)) proc_pars%cursize
      do i = 1, proc_pars%cursize
         ioerr = dlwqdataWrite( lun(16), proc_pars%dlwqdata(i) )
      enddo
      close ( lun(16) )

!     evaluate constants for report in dlwqp1

      itime = 0
      do i = 1, proc_pars%cursize
         if ( proc_pars%dlwqdata(i)%subject .eq. SUBJECT_CONSTANT ) then
            ierr3 = dlwqdataevaluate(proc_pars%dlwqdata(i),gridps,itime,constants%no_item,1,constants%constant)
         endif
      enddo

!     if necessary, check for the parameters SURF and LENGTH

      if ( chkpar(1) ) then
         ch20 = 'SURF'
         call zoek( ch20 , parameters%no_item, parameters%name, 20 , special )
         if ( special <= 0 ) then
            call zoek( ch20 , segfuncs%no_item, segfuncs%name, 20 , special )
            if ( special <= 0 ) then
               ierr = ierr + 1
               write( lunut, 2410 )
            endif
         endif
      endif

      if ( chkpar(2) ) then
         ch20 = 'LENGTH'
         call zoek( ch20 , parameters%no_item, parameters%name, 20 , special )
         if ( special <= 0 ) then
            call zoek( ch20 , segfuncs%no_item, segfuncs%name, 20 , special )
            if ( special <= 0 ) then
               ierr = ierr + 1
               write( lunut, 2420 )
            endif
         endif
      endif

!     proc_pars cleanup

      ierr3 = dlwq_cleanup(substances)
      ierr3 = dlwq_cleanup(parameters)
      ierr3 = dlwq_cleanup(functions)
      ierr3 = dlwq_cleanup(segfuncs)
      ierr3 = dlwq_cleanup(segments)

   30 continue
      if ( ierr2 .gt. 0 .and. ierr2 .ne. 2 ) ierr = ierr + 1
      if ( ierr2 .eq. 3 ) call srstop(1)
      call check ( ctoken, iwidth, 7     , ierr2  , ierr  )
      if ( timon ) call timstop( ithndl )
      return
!
!       Output formats
!
 2040 FORMAT (/' ERROR, unrecognized token: ',A)
 2050 FORMAT(/' Total number of constants        : ',I4  )
 2060 FORMAT(/' Total number of parameters       : ',I4  )
 2070 FORMAT(/' Total number of functions        : ',I4  )
 2080 FORMAT(/' Total number of segment functions: ',I4  )
 2240 FORMAT ( /,' NOVEC Keyword found')
 2250 FORMAT (   ' Number of fast solver vectors set to :',I6  )
 2310 FORMAT ( /,' NOTHREADS Keyword found')
 2320 FORMAT (   ' Number of threads for parallel processing set to :',I6  )
 2330 FORMAT (   ' Tau from DELWAQ will be used for DELPAR'  )
 2340 FORMAT (   ' WARNING: TAU not found. DELPAR will try to get its own TAU or compute it!' )
 2350 FORMAT (   ' VertDisp from DELWAQ will be used for DELPAR'  )
 2360 FORMAT (   ' WARNING: VertDisp not found. DELPAR will try to get its own VertDisp or compute it!' )
 2410 FORMAT ( /,' ERROR: No parameter or segment function "SURF" found - needed for special waste loads!' )
 2420 FORMAT ( /,' ERROR: No parameter or segment function "LENGTH" found - needed for special waste loads!' )
!
      END

      end module m_dlwq07
