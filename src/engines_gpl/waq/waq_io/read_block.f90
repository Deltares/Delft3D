!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_read_block
      use m_waq_precision
      use m_read_time_delay
      use m_read_items
      use m_read_header
      use m_read_data_ods
      use m_time_validation
      use m_error_status

      implicit none

      contains


      subroutine read_block ( lun       , lchar     , filtype   , inpfil    , ioutpt   , & 
                             iwidth    , substances, constants , parameters, functions, & 
                             segfuncs  , segments  , gridps    , data_block, ierr     , & 
                             status)

!     Deltares Software Centre

!>\File
!>               Reads a block of input items ( procesparameters, initial conditions )

!     Global declarations

      use m_read_data
      use m_print_matrix
      use m_opt1
      use m_compute_matrix
      use m_cli_utils, only : retrieve_command_argument
      use dlwqgrid_mod          ! for the storage of contraction grids
      use dlwq_hyd_data  ! for definition and storage of data
      use rd_token
      use timers       !   performance timers


      implicit none

!     declaration of arguments

      integer(kind=int_wp), intent(inout) ::  lun(*)        !< unit numbers used
      character(len=*)      , intent(inout) :: lchar(*)     !< filenames
      integer(kind=int_wp), intent(inout) ::  filtype(*)    !< type of binary file
      type(inputfilestack)  , intent(inout) :: inpfil       !< input file strucure with include stack and flags
      integer(kind=int_wp), intent(in) ::  ioutpt        !< level of reporting to ascii output file
      integer(kind=int_wp), intent(in) ::  iwidth        !< width of output
      type(t_dlwq_item)     , intent(inout) :: substances   !< delwaq substances list
      type(t_dlwq_item)     , intent(inout) :: constants    !< delwaq constants list
      type(t_dlwq_item)     , intent(inout) :: parameters   !< delwaq parameters list
      type(t_dlwq_item)     , intent(inout) :: functions    !< delwaq functions list
      type(t_dlwq_item)     , intent(inout) :: segfuncs     !< delwaq segment-functions list
      type(t_dlwq_item)     , intent(inout) :: segments     !< delwaq segments name list
      type(GridPointerColl) , intent(in)    :: GridPs       !< collection off all grid definitions
      type(t_dlwqdata)      , intent(out)   :: data_block   !< data block to be filled
      integer(kind=int_wp), intent(out) ::  ierr          !< output error count

      type(error_status), intent(inout) :: status !< current error status

!     local declarations

      type(t_dlwqdata)                      :: data_buffer  ! data block to be read
      type(t_dlwq_item)                     :: waq_param    ! list of param items to be set in this block ( substances etc )
      type(t_dlwq_item)                     :: data_param   ! list of param items in the data
      type(t_dlwq_item)                     :: waq_loc      ! list of loc items to be set in this block (segments, boundaries, loads)
      type(t_dlwq_item)                     :: data_loc     ! list of loc items in the data
      type(t_dlwq_item)                     :: types        ! delwaq (item-) type list, not relevant here for boundaries, loads
      type(t_fdata)                         :: odsdata      ! funtion data block to be read
      type(t_fdata)                         :: fdata        ! funtion data block to be read
      integer(kind=int_wp) ::  ierr2         ! local error indicator (ierr2 = 2, end of block)
      integer(kind=int_wp) ::  i_base_grid   ! index of base grid
      integer(kind=int_wp) ::  igrid         ! index of input grid
      integer(kind=int_wp) ::  noseg         ! number of segments
      integer(kind=int_wp) ::  noseg_org     ! original number of segments
      integer(kind=int_wp) ::  i             ! loop counter
      integer(kind=int_wp) ::  noits         ! number of scale factors / columns sybstances
      integer(kind=int_wp) ::  noits_loc     ! number of scale factors locations
      integer(kind=int_wp) ::  ndim1         ! first dimension matrix
      integer(kind=int_wp) ::  ndim2         ! second dimension matrix
      real(kind=real_wp) ::  amiss         ! missing value
      integer(kind=int_wp) ::  t_asked       ! type of token asked
      integer(kind=int_wp) ::  itype         ! type of token
      character(len=256)                    :: ctoken       ! character token from input
      integer(kind=int_wp) ::  itoken        ! integer token from input
      real(kind=real_wp) ::  rtoken        ! real token from input
      character                             :: cdummy       ! dummy not used
      integer(kind=int_wp) ::  idummy        ! dummy not used
      real(kind=real_wp) ::  rdummy        ! dummy not used
      character(len=256)                    :: adummy       ! dummy not used
      character(len=10)                     :: callr        ! kind of item
      character(len=10)                     :: strng1       ! kind of item
      character(len=10)                     :: strng2       ! kind of item
      character(len=10)                     :: strng3       ! kind of item

      logical                               :: lfound       ! Keyword found (or not)
      logical                               :: lsegfuncheck ! Do check if segmentfunctions are correct
      integer(kind=INT64) ::  filesize      ! Reported size of the file

      logical       dtflg1 , dtflg2, dtflg3
      integer(kind=int_wp) :: chkflg , itfact
      integer(kind=int_wp) ::  nocol         ! number of columns in input
      integer(kind=int_wp) ::  ithndl = 0
      if (timon) call timstrt( "read_block", ithndl )

      call retrieve_command_argument ( '-nosegfuncheck', 0, lfound, idummy, rdummy, adummy, ierr2)
      lsegfuncheck = .not. lfound

!     defaults and initialisation

      data_block%subject         = SUBJECT_UNKNOWN
      data_block%no_param        = 0
      data_block%no_loc          = 0
      data_block%no_brk          = 0
      data_block%functype        = FUNCTYPE_CONSTANT
      data_block%igrid           = 1
      data_block%extern          = .false.
      data_block%filetype        = FILE_NONE
      data_block%filename        = ' '
      data_block%iorder          = ORDER_UNKNOWN
      data_block%param_named     = .false.
      data_block%param_name      =>null()
      data_block%loc_named       = .false.
      data_block%loc_name        =>null()
      data_block%param_pointered = .false.
      data_block%param_pointers  =>null()
      data_block%loc_pointered   = .false.
      data_block%loc_pointers    =>null()
      data_block%scaled          = .false.
      data_block%scale_factor    = 1.0
      data_block%param_scaled    = .false.
      data_block%factor_param    =>null()
      data_block%loc_defaults    = .false.
      data_block%loc_scaled      = .false.
      data_block%factor_loc      =>null()
      data_block%times           =>null()
      data_block%values          =>null()

      ierr2 = dlwq_init(waq_param)
      ierr2 = dlwq_init(data_param)
      ierr2 = dlwq_init(waq_loc)
      ierr2 = dlwq_init(data_loc)
      ierr2 = dlwq_init(types)

      i_base_grid = GridPs%base_grid
      noseg = GridPs%Pointers(i_base_grid)%noseg
      noseg_org = get_original_noseg()

!     initialise a number of variables

      ierr   = 0
      amiss  = -999.0
      dtflg1 = inpfil%dtflg1
      dtflg2 = inpfil%dtflg2
      dtflg3 = inpfil%dtflg3
      itfact = inpfil%itfact

!     loop over the input (tokens) till input is ready or error

      do

         if ( gettoken(ctoken, ierr2) /= 0 ) exit

         if ( ctoken == 'ABSOLUTE' ) then
            write ( lunut , 1900 )
            if ( data_block%functype == 0 ) data_block%functype  = FUNCTYPE_BLOCK
            cycle
         endif
         if ( ctoken == 'BLOCK' ) then
            write ( lunut , 2000 )
            data_block%functype = FUNCTYPE_BLOCK
            cycle
         endif
         if ( ctoken == 'LINEAR' ) then
            write ( lunut , 2000 )
            data_block%functype = FUNCTYPE_LINEAR
            cycle
         endif
         if ( ctoken == 'HARMONICS' ) then
            write ( lunut , 2005 )
            data_block%functype = FUNCTYPE_HARMONIC
            cycle
         endif
         if ( ctoken == 'FOURIERS'  ) then
            write ( lunut , 2010 )
            data_block%functype = FUNCTYPE_FOURIER
            cycle
         endif
         if ( ctoken == 'TIME_DELAY' ) then
            call read_time_delay ( ierr2  )
            if ( ierr2 /= 0 ) goto 100
            cycle
         endif
         if ( ctoken == 'ODS_FILE' ) then
            if ( data_block%functype == FUNCTYPE_HARMONIC .or. data_block%functype == FUNCTYPE_FOURIER ) then
               write ( lunut , 2100 )
               ierr = 1
               exit
            endif
            data_block%extern   = .true.
            data_block%filetype = FILE_ODS
            cycle
         endif
         if ( ctoken == 'BINARY_FILE' ) then
            if ( data_block%functype == FUNCTYPE_HARMONIC .or. data_block%functype == FUNCTYPE_FOURIER ) then
               write ( lunut , 2110 )
               ierr = 1
               exit
            endif
            data_block%extern   = .true.
            data_block%filetype = FILE_BINARY
            cycle
         endif
         if ( ctoken == 'UNFORMATTED' ) then
            if ( data_block%functype == FUNCTYPE_HARMONIC .or. data_block%functype == FUNCTYPE_FOURIER ) then
               write ( lunut , 2110 )
               ierr = 1
               exit
            endif
            data_block%extern   = .true.
            data_block%filetype = FILE_UNFORMATTED
            cycle
         endif
         if ( ctoken == 'BIG_ENDIAN' ) then
            if ( data_block%functype == FUNCTYPE_HARMONIC .or. data_block%functype == FUNCTYPE_FOURIER ) then
               write ( lunut , 2110 )
               ierr = 1
               exit
            endif
            data_block%extern   = .true.
            data_block%filetype = data_block%filetype + FILE_BIG_ENDIAN
            cycle
         endif
         if ( ctoken == 'MULTIPLEHYD_FILE' ) then
            if ( data_block%functype == FUNCTYPE_HARMONIC .or. data_block%functype == FUNCTYPE_FOURIER ) then
               write ( lunut , 2110 )
               ierr = 1
               exit
            endif
            if ( .not. (data_block%subject == SUBJECT_FUNCTION .or. data_block%subject == SUBJECT_SEGFUNC) ) then
               write ( lunut , 2270 )
               ierr = 1
               exit
            endif
            if ( data_param%no_item /= 1 ) then
               write ( lunut , 2280 )
               ierr = 1
               exit
            endif

            ! handle file option, should we resolve the use of 17? = work file segment-functions

            call opt1( -4    , lun    , 17    , lchar  , filtype, & 
                      dtflg1, dtflg3 , noseg , ierr2  , status   , & 
                      .false.)
            if ( ierr2 /= 0 ) exit

            ierr2 = puttoken(lchar(17))
            data_block%extern   = .true.
            data_block%filetype = FILE_BINARY
            cycle
         endif
         if ( ctoken == 'INITIALS' ) then
            callr  = 'initial'
            strng1 = 'segments'
            strng2 = 'substance'
            data_block%subject  = SUBJECT_INITIAL
            data_block%functype = FUNCTYPE_CONSTANT
            chkflg = 1
            call read_items( lunut      , inpfil    , ioutpt    , chkflg   , callr , & 
                            waq_param  , data_param, substances, types    , noits , & 
                            ierr2      , status)

            if ( ierr2 /= 0 ) then
               write ( lunut , 2120 )
               goto 100
            endif
            cycle
         endif
         if ( ctoken == 'CONSTANTS' ) then
            callr  = 'constant'
            data_block%subject  = SUBJECT_CONSTANT
            data_block%functype = FUNCTYPE_CONSTANT
            data_block%no_loc   = 1
            data_block%iorder   = ORDER_PARAM_LOC
            ierr2 = dlwq_resize(waq_loc,1)
            waq_loc%no_item = 1
            waq_loc%name(1) = 'constant'
            waq_loc%ipnt(1) = 1

            chkflg = 0
!
            call read_items( lunut      , inpfil    , ioutpt    , chkflg   , callr , & 
                            waq_param  , data_param, constants , types    , noits , & 
                            ierr2      , status)


            if ( ierr2 /= 0 ) then
               write ( lunut , 2120 )
               ierr = 1
               exit
            endif

            cycle
         endif

         if ( ctoken == 'FUNCTIONS' ) then
            callr  = 'function'
            data_block%subject  = SUBJECT_FUNCTION
            if ( data_block%functype == FUNCTYPE_CONSTANT ) data_block%functype = FUNCTYPE_BLOCK
            data_block%no_loc   = 1
            data_block%iorder = ORDER_PARAM_LOC
            write ( lunut , * ) ' '
            ierr2 = dlwq_resize(waq_loc,1)
            waq_loc%no_item = 1
            waq_loc%name(1) = 'constant'
            waq_loc%ipnt(1) = 1

            chkflg = 0

            call read_items( lunut      , inpfil    , ioutpt    , chkflg   , callr , & 
                            waq_param  , data_param, functions , types    , noits , & 
                            ierr2      , status)


            if ( ierr2 /= 0 ) then
               write ( lunut , 2120 )
               goto 100
            endif
            cycle
         endif

         if ( ctoken=='PARAMETERS' ) then
            strng1 = 'parameter'
            chkflg = -1
            data_block%subject  = SUBJECT_PARAMETER
            data_block%functype = 0
            write ( lunut , * ) ' '

            call read_items( lunut      , inpfil    , ioutpt    , chkflg   , strng1, & 
                            waq_param  , data_param, parameters, types    , noits , & 
                            ierr2      , status)


            if ( ierr2 /= 0 ) then
               write ( lunut , 2150 )
               goto 100
            endif
            if ( data_param%no_item /= 0 .and. data_block%iorder == ORDER_UNKNOWN ) data_block%iorder = ORDER_PARAM_LOC
            cycle
         endif
!
         if ( ctoken=='SEG_FUNCTIONS' ) then
            strng1 = 'seg-funct.'
            chkflg = 0
            data_block%subject  = SUBJECT_SEGFUNC
            if ( data_block%functype == 0 ) data_block%functype = 1
            write ( lunut , * ) ' '

            call read_items( lunut      , inpfil    , ioutpt    , chkflg   , strng1, & 
                            waq_param  , data_param, segfuncs  , types    , noits , & 
                            ierr2      , status)


            if ( ierr2 /= 0 ) then
               write ( lunut , 2150 )
               goto 100
            endif
            if ( data_param%no_item /= 0 .and. data_block%iorder == ORDER_UNKNOWN ) data_block%iorder = ORDER_LOC_PARAM
            cycle
         endif
!
         if ( ctoken  == 'SEGMENTS' .or. ctoken == 'ALL' .or. ctoken == 'INPUTGRID' ) then
            if ( waq_loc%no_item  == -1   ) then
               write ( lunut , 2170 )
               ierr = 1
               goto 100
            endif
            if ( ctoken == 'ALL' ) then
               waq_loc%no_item = noseg
               write ( lunut , 2020 ) waq_loc%no_item
               ierr2 = dlwq_resize(waq_loc,waq_loc%no_item)
               do i = 1 , waq_loc%no_item
                  waq_loc%ipnt(i) = i
                  write(waq_loc%name(i), '(''segment '',i8)' ) i
               enddo
            elseif ( ctoken == 'INPUTGRID' ) then
               if ( gettoken(ctoken,ierr2) /= 0 ) goto 100
               igrid = gridpointercollfind( gridps, ctoken )
               if ( igrid >= 1 ) then
                  data_block%igrid = igrid
                  write ( lunut , 2290 ) trim(ctoken)
                  waq_loc%no_item = gridps%pointers(igrid)%noseg
                  write ( lunut , 2300 ) waq_loc%no_item
                  ierr2 = dlwq_resize(waq_loc,waq_loc%no_item)
                  do i = 1 , waq_loc%no_item
                     waq_loc%ipnt(i) = i
                     write(waq_loc%name(i), '(''segment '',i8)' ) i
                  enddo
               else
                  write ( lunut , 2310 ) trim(ctoken)
                  ierr = 1
                  goto 100
               endif
            else
               callr  = 'segment'
               chkflg = 1
               data_block%loc_pointered   = .true.

               call read_items( lunut    , inpfil  , ioutpt  , chkflg, callr    , & 
                               waq_loc  , data_loc, segments, types , noits_loc, & 
                               ierr2    , status)

               if ( ierr2 /= 0 ) then
                  write ( lunut , 2180 )
                  goto 100
               endif
            endif
            if ( data_param%no_item == 0 ) then
               write ( lunut , 2030 ) strng1, strng1
               data_block%iorder = ORDER_LOC_PARAM
               if ( data_block%subject == SUBJECT_PARAMETER ) ierr2 = puttoken('PARAMETERS')
               if ( data_block%subject == SUBJECT_SEGFUNC ) ierr2 = puttoken('SEG_FUNCTIONS')
               if ( data_block%subject == SUBJECT_INITIAL ) ierr2 = puttoken('INITIALS')
            else
               write ( lunut , 2040 ) strng1, strng1
               data_block%iorder = ORDER_PARAM_LOC
            endif
            cycle
         endif
!
         if ( ctoken == 'DEFAULTS' ) then
            if ( data_block%subject == SUBJECT_PARAMETER .or. &
                data_block%subject == SUBJECT_INITIAL   .or. &
                data_block%subject == SUBJECT_SEGFUNC ) then

               data_block%iorder       = ORDER_PARAM_LOC
               data_block%loc_defaults = .true.
               if ( data_param%no_item > 0 ) then
                  ctoken = 'DATA'
               else
                  if ( data_block%subject == SUBJECT_PARAMETER ) ctoken = 'PARAMETERS'
                  if ( data_block%subject == SUBJECT_INITIAL   ) ctoken = 'INITIALS'
                  if ( data_block%subject == SUBJECT_SEGFUNC ) ctoken = 'SEG_FUNCTIONS'
               endif
               ierr2 = puttoken(ctoken)
               ierr2 = dlwq_resize(waq_loc,1)
               waq_loc%no_item = 1
               waq_loc%name(1) = 'defaults'
               waq_loc%ipnt(1) = 0
               cycle
            else
               write ( lunut , 2190 )
               ierr = 1
               goto 100
            endif
         endif
!
         if ( ctoken == 'DATA' .or. data_block%extern ) then
            if ( data_block%subject == SUBJECT_CONSTANT ) then
               strng1 = 'constants'
               strng2 = 'values'
            endif
            if ( data_block%subject == SUBJECT_PARAMETER ) then
               strng1 = 'parameters'
               strng2 = 'segments'
               if ( waq_loc%no_item == 0 ) then
                  write ( lunut , 2260 )
                  goto 100
               endif
            endif
            if ( data_block%subject == SUBJECT_FUNCTION ) then
               strng1 = 'functions'
               strng2 = 'values'
            endif
            if ( data_block%subject == SUBJECT_SEGFUNC ) then
               strng1 = 'seg-functs'
               strng2 = 'segments'
               if ( waq_loc%no_item == 0 ) then
                  write ( lunut , 2260 )
                  goto 100
               endif
            endif
            strng3 = 'breakpoint'
            if ( data_block%functype == FUNCTYPE_HARMONIC ) strng3 = 'harmonic'
            if ( data_block%functype == FUNCTYPE_FOURIER   ) strng3 = 'fourier'

            if ( data_block%filetype == FILE_ODS ) then

               ! when data_loc is not filled only

               if ( data_loc%no_item == 0 ) then
                  ierr2 = dlwq_resize(data_loc,waq_loc%no_item)
                  data_loc%no_item  = waq_loc%no_item
                  data_loc%name     = waq_loc%name
                  data_loc%ipnt     = waq_loc%ipnt
                  data_loc%sequence = waq_loc%sequence
                  data_loc%constant = waq_loc%constant
               endif

               data_block%no_param = waq_param%no_item
               data_block%no_loc   = waq_loc%no_item

               call read_data_ods( lunut      , ctoken , data_param, data_loc, amiss , & 
                                  data_buffer, ierr2 )
               if ( ierr2 /= 0 ) goto 100
               call compute_matrix ( lunut , data_param , data_loc   , waq_param, waq_loc, & 
                                    amiss , data_buffer, data_block )
               data_block%extern = .false.
               deallocate(data_buffer%times,data_buffer%values)

            elseif ( mod(data_block%filetype,10) == FILE_BINARY .or. &
                    mod(data_block%filetype,10) == FILE_UNFORMATTED ) then
               if ( data_block%subject == SUBJECT_SEGFUNC ) data_block%iorder = ORDER_LOC_PARAM
               write ( lunut   ,   2220   ) ctoken
               data_block%filename = ctoken

               if (lsegfuncheck) then
                  ! Check the size of the file (if it is binary, otherwise this is not reliable)
                  call check_file_size( ctoken, noits*noseg_org, mod(data_block%filetype,10), filesize, ierr2 )
                  if ( ierr2 < 0 ) then
                      ierr2 = 1
                      write( lunut , 2320 ) ctoken
                  elseif ( ierr2 > 0 ) then
                      ierr2 = 0        ! It is a warning, proceed at your own peril
                      call status%increase_warning_count()
                      write( lunut , 2330 ) ctoken, filesize, 4*(1+noits*noseg_org), noits, noseg
                      write( lunut , 2340 )
                  endif
               end if

            else

               ! Check if an inner loop column header exists for the data matrix

               nocol = noits
               call read_header( waq_param, data_param, nocol , itfact, dtflg1, & 
                                dtflg3   , ierr2     , status  )
               if ( ierr2 /= 0 ) goto 100

               ! when data_loc is not filled only

               if ( data_loc%no_item == 0 ) then
                  ierr2 = dlwq_resize(data_loc,waq_loc%no_item)
                  data_loc%no_item  = waq_loc%no_item
                  data_loc%name     = waq_loc%name
                  data_loc%ipnt     = waq_loc%ipnt
                  data_loc%sequence = waq_loc%sequence
                  data_loc%constant = waq_loc%constant
               endif

               ! read the data

               data_block%no_param = waq_param%no_item
               data_block%no_loc   = waq_loc%no_item

               data_buffer%no_param = nocol
               data_buffer%no_loc   = data_loc%no_item
               data_buffer%iorder   = data_block%iorder
               data_buffer%functype = data_block%functype

               call read_data( data_buffer, itfact, dtflg1, dtflg3, ierr2 )
               if ( ierr2 /= 0 ) goto 100

               call validate_time_series_strictly_increasing(lunut,data_buffer, ierr2)

               call compute_matrix ( lunut , data_param , data_loc   , waq_param, waq_loc, & 
                                    amiss , data_buffer, data_block )
               deallocate(data_buffer%times,data_buffer%values)
            endif
            if ( ierr2 == 1 .or. ierr2 == 4 ) then
               write ( lunut , 2200 )
               goto 100
            endif
            if ( waq_loc%no_item == -1 ) write ( lunut , 1910 )
            data_block%no_param       = waq_param%no_item
            data_block%param_named    = .true.
            data_block%param_name     =>waq_param%name
            data_block%param_pointered= .true.
            data_block%param_pointers =>waq_param%ipnt
            data_block%no_loc         = waq_loc%no_item
            data_block%loc_named      = .true.
            data_block%loc_name       =>waq_loc%name
            waq_param%name => null()
            waq_param%ipnt => null()
            waq_loc%name   => null()
            if ( data_block%loc_pointered ) then
               data_block%loc_pointers   =>waq_loc%ipnt
               waq_loc%ipnt => null()
            endif
            call print_matrix( lunut , iwidth, data_block, strng1, strng2, & 
                              strng3, ioutpt)
            if ( ierr2 == 3 ) goto 50
            exit
         endif

         ! unknown keyword

         write ( lunut , 2210 ) trim(ctoken)
         ierr = 1
         exit

      enddo ! end loop over the input

      write ( lunut , 1140 )

!
   50 continue
      goto 110
!
  100 continue
      if ( ierr2 /= 0 ) then
         write ( lunut , 2090 )
         ierr = ierr2
      endif
!
  110 continue

      ierr2 = dlwq_cleanup(waq_param)
      ierr2 = dlwq_cleanup(data_param)
      ierr2 = dlwq_cleanup(waq_loc)
      ierr2 = dlwq_cleanup(data_loc)
      ierr2 = dlwq_cleanup(types)

      if (timon) call timstop( ithndl )
      return
!
 1140 FORMAT(/' ====> input item completed <==== '//   )
 1900 FORMAT(/' Absolute times (YYYY/MM/DD;HH:MM:SS) expected in next' & 
            ,' time function block.' )
 1910 FORMAT(/' Data are supplied as single default values', & 
             ' for all segments !' )
 2000 FORMAT(/' Time function is linearly interpolated.' )
 2005 FORMAT(/' Time function as set of harmonics.' )
 2010 FORMAT(/' Time function as Fourier series.' )
 2020 FORMAT(/' Input will be given for all ',I10,' segments.' )
 2030 FORMAT(/' ',A,'s ordered in groups of ',A,'s per segment.')
 2040 FORMAT(/' ',A,'s ordered in groups of segments per ',A,'.')
 2090 FORMAT( ' ERROR encountered in processing this input item !' )
 2100 FORMAT( ' Harmonics or Fouriers not allowed with ODS-files !' )
 2110 FORMAT( ' Harmonics or Fouriers not allowed with binary files !' )
 2120 FORMAT( ' ERROR during processing of CONSTANT or FUNCTION', & 
             ' names !' )
 2150 FORMAT( ' ERROR during processing of PARAMETERS or', & 
             ' SEG_FUNCTIONS names !' )
 2170 FORMAT( ' ERROR: The DEFAULT keyword was already specified,', & 
             ' no SEGMENTS or ALL expected any more !' )
 2180 FORMAT( ' ERROR during processing of SEGMENT identifiers !')
 2190 FORMAT( ' ERROR: DEFAULTS only allowed after PARAMETERS or', & 
             ' SEG_FUNCTIONS keyword has been set !' )
 2200 FORMAT( ' ERROR reading the data block !' )
 2210 FORMAT( ' ERROR keyword: ',A,' not recognized !' )
 2220 FORMAT( ' Input comes from binary file: ',A      )
 2260 FORMAT( ' ERROR: Segments not defined, use ALL for all segs !' )
 2270 FORMAT( ' ERROR: MULTIPLEHYD_FILE only allowed for (segment-)functions !' )
 2280 FORMAT( ' ERROR: MULTIPLEHYD_FILE only allowed for single item entry!' )
 2290 FORMAT( ' Input grid for this item is :',A)
 2300 FORMAT( ' Input will be given for ',I10,' segments.' )
 2310 FORMAT( ' ERROR: Input grid not defined :',A)
 2320 FORMAT( ' ERROR: Binary/unformatted file does not exist or could not be opened: ',A)
 2330 FORMAT( ' WARNING: Binary/unformatted file does not have the correct size: ',A,/, & 
             '          The reported size is: ',I0,/, & 
             '          The size should not be zero and it should be a whole multiple of ',I0, & 
             ' (= 4*(1+',I0,'*',I0,'))')
 2340 FORMAT( '          As on some file systems the reported size may be incorrect,',/, & 
             '          this is treated as a warning')

      CONTAINS

      subroutine check_file_size( filename, nodata, type, filesize, ierr )


      character(len=*), intent(in)     :: filename
      integer(kind=int_wp), intent(in) ::  nodata
      integer(kind=int_wp), intent(in) ::  type
      integer(kind=INT64), intent(out) ::  filesize
      integer(kind=int_wp), intent(out) ::  ierr

      integer(kind=int_wp) ::  norcd, i
      integer(kind=int_wp) ::  lun
      integer(kind=int_wp) ::  time
      real(kind=real_wp), dimension(:), allocatable  ::  data
      character(14)                   :: strng

      integer(kind=INT64) ::  recordsize

      ierr = 0


      !
      ! Check that the file exists and can be opened
      !

      if ( type == FILE_BINARY ) then
          open( newunit = lun, file = filename, iostat = ierr, status = 'old', access = 'stream' )
      else
          open( newunit = lun, file = filename, iostat = ierr, status = 'old', form = 'unformatted' )
      endif
      if ( ierr /= 0 ) then
          ierr = -999 ! Explicitly mark the file as unuseable
          return
      endif

      !
      ! Check if this is a steering file
      !

      read ( lun , iostat = ierr ) strng
      if ( ierr /= 0 ) then
         ierr  = 0
         strng = 'x'
      endif
      if ( strng(1:14) /= 'Steering file ' ) then
         if ( type == FILE_BINARY ) then
            inquire( lun, size = filesize )
            close( lun )

            recordsize = 4 * (nodata+1) ! single-precision reals occupy four bytes, also 4 bytes for the time
            if ( mod(filesize,recordsize) /= 0 ) then
                ierr = 1
            endif
         else
            rewind( lun )
            allocate( data(nodata) )
            !
            ! Determine the number of records - iostat does not seem to distinguish between partly fulfilled
            ! reads and end-of-file
            !
            norcd   = 0
            do
               read( lun, iostat = ierr ) time, data
               if ( ierr /= 0 ) then
                   exit
               endif
               norcd = norcd + 1
            enddo

            !
            ! The last record may have been too short, so try again:
            ! - Read all the records we have been able to read
            ! - Try reading an extra number (time). This should fail
            !
            rewind( lun )
            do i = 1,norcd
               read( lun, iostat = ierr ) time, data
            enddo

            read( lun, iostat = ierr ) time

            !
            ! If we have been able to read at least one record and the last read
            ! let to and end-of-file condition, we accept the file. Otherwise return
            ! an error.
            !
            if ( norcd > 0 .and. ierr < 0 ) then
                ierr = 0
            else
                ierr = 1
            endif
            close( lun )
            deallocate( data )
         endif
      else
         !
         ! No check yet if it is a steering file
         !
      endif

      end subroutine check_file_size

      ! Function to get around the name clash - information in a COMMON block
      integer function get_original_noseg()
          use m_sysn          ! System characteristics

          get_original_noseg = noseg
      end function get_original_noseg
      end subroutine read_block

      end module m_read_block
