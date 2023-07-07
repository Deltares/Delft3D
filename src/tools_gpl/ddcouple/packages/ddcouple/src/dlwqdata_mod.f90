!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      module dlwqdata_mod
       use m_srstop
       use m_monsys
       use m_dhucas
       use m_open_waq_files
       use m_dhnlun

!
!          module contains everything for model data input and storage
!          created March 2004 by Jan van Beek
!
!     contains the following derived types:
!
!          t_dlwqdata              ! poperties with respect to a data item
!
!          t_dlwqdatacoll          ! a collection of dlwqdata, for instance all wasteloads
!
      implicit none
!
      integer, parameter, private :: ITEM_NAME_SIZE    =  20          ! length all names
      integer, parameter, private :: NAME_SIZE         =  20          ! size of descriptive names
      integer, parameter, private :: FILE_NAME_SIZE    = 256          ! length all names
      integer, parameter, private :: MAX_NUM           =   5          ! allocated per bunch

      integer, parameter :: SUBJECT_UNKNOWN   = 0            ! unknown
      integer, parameter :: SUBJECT_IDT       = 1            ! timestep related input
      integer, parameter :: SUBJECT_VOLUME    = 2            ! volume related input
      integer, parameter :: SUBJECT_DISPERSION= 3            ! dispersion related input
      integer, parameter :: SUBJECT_AREA      = 4            ! area related input
      integer, parameter :: SUBJECT_FLOW      = 5            ! flow related input
      integer, parameter :: SUBJECT_VELOC     = 6            ! velocity related input
      integer, parameter :: SUBJECT_DSPLEN    = 7            ! dispersion length related input
      integer, parameter :: SUBJECT_BOUNDARY  = 8            ! boundary related input
      integer, parameter :: SUBJECT_WASTE     = 9            ! discharge related input
      integer, parameter :: SUBJECT_CONSTANT  =10            ! constant process parameter
      integer, parameter :: SUBJECT_PARAMETER =11            ! parameter process parameter
      integer, parameter :: SUBJECT_FUNCTION  =12            ! function process parameter
      integer, parameter :: SUBJECT_SEGFUNC   =13            ! segment-function process parameter
      integer, parameter :: SUBJECT_INITIAL   =14            ! initial condition
      integer, parameter :: SUBJECT_FEATURE   =15            ! feature (kenmerk)

      integer, parameter :: ORDER_UNKNOWN     = 0            ! data ordering unknown
      integer, parameter :: ORDER_PARAM_LOC   = 1            ! data ordered parameters inners loop, locations outer loop
      integer, parameter :: ORDER_LOC_PARAM   = 2            ! data ordered locations inners loop, parametrs outer loop

      integer, parameter :: FUNCTYPE_CONSTANT = 0            ! constant in time
      integer, parameter :: FUNCTYPE_BLOCK    = 1            ! block funtion
      integer, parameter :: FUNCTYPE_LINEAR   = 2            ! linear function
      integer, parameter :: FUNCTYPE_HARMONIC = 3            ! harmonic function
      integer, parameter :: FUNCTYPE_FOURIER  = 4            ! fourier function
      integer, parameter :: FUNCTYPE_ALLDT    = 5            ! every timestep one record (file option 0)

      integer, parameter :: FILE_NONE         = 0            ! data not in file but in memory
      integer, parameter :: FILE_BINARY       = 1            ! data in (delwaq) binary file
      integer, parameter :: FILE_ODS          = 2            ! data in ODS file
      integer, parameter :: FILE_OMS          = 3            ! data in OMS dataspace
      integer, parameter :: FILE_DIO          = 4            ! data in DIO coupling

      type t_dlwqdata
         integer                                :: subject           ! subject for this data
         integer                                :: no_param          ! number of paramters in this block of data
         integer                                :: no_loc            ! number of locations
         integer                                :: no_brk            ! number of breakpoints or harmonics
         integer                                :: functype          ! constant, block, linear, harmonics, foutier
         integer                                :: igrid             ! grid number of input
         logical                                :: extern            ! is data in file or online coupling
         integer                                :: filetype          ! type of ecternal data source
         character(len=FILE_NAME_SIZE)          :: filename          ! name of file or dataset in coupling
         integer                                :: lun               ! unit number external file
         integer                                :: iorder            ! ordering of the data matrix, param-loc or loc-param
         logical                                :: param_named       ! are the paramters named
         character(len=ITEM_NAME_SIZE), pointer :: param_name(:)     ! parameter names
         logical                                :: loc_named         ! are the locations named
         character(len=ITEM_NAME_SIZE), pointer :: loc_name(:)       ! location names
         logical                                :: param_pointered   ! are the paramters pointered
         integer, pointer                       :: param_pointers(:) ! index of the parameters in the waq substance/constants/etc arrays
         logical                                :: loc_defaults      ! data is default for all locations
         logical                                :: loc_pointered     ! are the locations pointered
         integer, pointer                       :: loc_pointers(:)   ! segment number of the locations in the specific grid
         logical                                :: scaled            ! overall scaling applied?
         real                                   :: scale_factor      ! overall scaling factor
         logical                                :: param_scaled      ! need the parametrs scaling
         real, pointer                          :: factor_param(:)   ! scale factors for parametrers if any
         logical                                :: loc_scaled        ! need the locations scaling
         real, pointer                          :: factor_loc(:)     ! scale factors for locations if any
         integer, pointer                       :: times(:)          ! time at breakpoints
         real, pointer                          :: phase(:)          ! phase in case of harmonics
         real, pointer                          :: values(:,:,:)     ! the data itself either(no_loc,no_param,no_brk)
      end type t_dlwqdata

      type t_dlwqdatacoll
         type(t_dlwqdata), pointer              :: dlwqdata(:)     ! pointer
         integer                                :: maxsize         ! maximum size of the current array
         integer                                :: cursize         ! filled up to this size
      end type t_dlwqdatacoll

      type t_fdata
         integer                                :: ndim1           ! first dimension
         integer                                :: ndim2           ! second dimension
         integer                                :: nobrk           ! third dimension, number of times
         integer, pointer                       :: times(:)        ! times
         real, pointer                          :: values(:,:,:)   ! the data itself either(no_loc,no_param,no_brk)
      end type t_fdata

!     this is a collection of names

      type t_dlwq_namelist
         character(LEN=NAME_SIZE),pointer :: name(:)            ! names of variables
         integer                          :: cursize            ! filled up to this size
         integer                          :: maxsize            ! allocated up to this size
      end type t_dlwq_namelist

!     this is a collection of items

      type t_dlwq_item
         character(LEN=NAME_SIZE),pointer :: name(:)            ! names of item
         integer, pointer                 :: ipnt(:)            ! index pointer of item (in waq list, etc )
         integer, pointer                 :: sequence(:)        ! sequence index of item in input
         real, pointer                    :: constant(:)        ! constant value of item
         integer                          :: no_item            ! filled up to this size
         integer                          :: maxsize            ! allocated up to this size
      end type t_dlwq_item


      integer, parameter :: TYPE_CHAR   =  1                 ! character
      integer, parameter :: TYPE_INT    =  2                 ! integer
      integer, parameter :: TYPE_REAL   =  3                 ! real
      integer, parameter :: TYPE_ALL    =  0                 ! all types allowed
      integer, parameter :: TYPE_NOCHAR = -1                 ! no character allowed
      integer, parameter :: TYPE_NOINT  = -2                 ! no integer allowed
      integer, parameter :: TYPE_NOREAL = -3                 ! no real allowed
      integer, parameter :: maxfil = 5                       ! maximum of include files in stack
      type inputfilestack
         integer             :: inputf          ! current input file in stack
         integer             :: inplun(maxfil)  ! unit numbers
         character(len=256)  :: finame(maxfil)  ! filename call stack
         character(len=1024) :: linbuf(maxfil)  ! line buffer
         integer             :: linenr(maxfil)  ! current line
         integer             :: nrword(maxfil)  ! word count
         integer             :: ilword(maxfil)  ! current word
         character           :: cchar           ! comment character
         character           :: grpsep          ! group seperator
         integer             :: iposr           ! position on line of input file
         integer             :: iposl           ! left position on line of input file
         integer             :: npos            ! number of signifiant characters
         character(len=256)  :: ctoken          ! character token
         integer             :: itoken          ! integer token
         real                :: rtoken          ! real token
         integer             :: t_token         ! type in - out
         logical             :: token_used      ! is last token used
         integer             :: nrepeat         ! repeat factor of a token
         logical             :: dtflg1          ! dtflg1
         logical             :: dtflg2          ! dtflg2
         logical             :: dtflg3          ! dtflg3
         integer             :: itfact          ! itfact
         integer             :: iblock          ! input block
         real                :: vrsion          ! version of input
         integer             :: ierr            ! error on inputfile
      endtype inputfilestack

!     overload the operations

      interface dlwq_init
         module procedure dlwq_init_item
      end interface

      interface dlwq_resize
         module procedure dlwq_resize_item
      end interface

      interface dlwq_find
         module procedure dlwq_find_item
         module procedure dlwq_find_name
      end interface

      interface dlwq_read_token
         module procedure dlwq_read_token_char
         module procedure dlwq_read_token_int
         module procedure dlwq_read_token_real
      end interface

      contains

      function dlwqdatacollAdd( dlwqdatacoll , dlwqdata ) result ( cursize )
!
         type(t_dlwqdatacoll)               :: dlwqdatacoll
         type(t_dlwqdata)                   :: dlwqdata
         integer                            :: cursize

!        local

         type(t_dlwqdata), pointer          :: dlwqdatas(:)   ! should be a pointer for the resize operation
         integer                            :: ierr_alloc
         integer                            :: i
!
         if ( dlwqdatacoll%cursize .eq. dlwqdatacoll%maxsize ) then
            allocate ( dlwqdatas ( dlwqdatacoll%maxsize + MAX_NUM ) , stat = ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then
               write(*,*) 'ERROR : ALLOCATING WORK ARRAY'
               call srstop(1)
            endif
            do i = 1 , dlwqdatacoll%maxsize
               dlwqdatas(i) = dlwqdatacoll%dlwqdata(i)            ! copies the contents
            enddo
            if ( dlwqdatacoll%maxsize .ne. 0 ) deallocate ( dlwqdatacoll%dlwqdata )
            dlwqdatacoll%dlwqdata => dlwqdatas                    ! attaches this new array of pointers
            dlwqdatacoll%maxsize = dlwqdatacoll%maxsize + MAX_NUM
         endif
         dlwqdatacoll%cursize = dlwqdatacoll%cursize + 1
         dlwqdatacoll%dlwqdata( dlwqdatacoll%cursize ) = dlwqdata
         cursize = dlwqdatacoll%cursize
         return
!
      end function dlwqdatacollAdd

      function dlwqdataWrite( ilun, dlwqdata ) result ( ierror )
!
         integer, intent(in)                :: ilun         ! unit number binary file with data
         type(t_dlwqdata), intent(in)       :: dlwqdata     ! datastructure to be written
         integer                            :: ierror       !

!        local

         integer                            :: nopar        ! local copy number of parameters
         integer                            :: noloc        ! local copy number of locations
         integer                            :: nobrk        ! local copy number of breakpoints
         integer                            :: ipar         ! index paramaters
         integer                            :: iloc         ! index locations
         integer                            :: ibrk         ! index breakpoints

!
         ierror  = 0
         nopar = dlwqdata%no_param
         noloc = dlwqdata%no_loc
         nobrk = dlwqdata%no_brk

         write(ilun, err = 100 ) dlwqdata%subject
         write(ilun, err = 100 ) dlwqdata%no_param
         write(ilun, err = 100 ) dlwqdata%no_loc
         write(ilun, err = 100 ) dlwqdata%no_brk
         write(ilun, err = 100 ) dlwqdata%functype
         write(ilun, err = 100 ) dlwqdata%igrid
         write(ilun, err = 100 ) dlwqdata%extern
         write(ilun, err = 100 ) dlwqdata%filetype
         write(ilun, err = 100 ) dlwqdata%filename
         write(ilun, err = 100 ) dlwqdata%iorder
         write(ilun, err = 100 ) dlwqdata%param_named
         if ( dlwqdata%param_named ) then
            write(ilun, err = 100 ) (dlwqdata%param_name(ipar),ipar=1,nopar)
         endif
         write(ilun, err = 100 ) dlwqdata%loc_named
         if ( dlwqdata%loc_named ) then
            write(ilun, err = 100 ) (dlwqdata%loc_name(iloc),iloc=1,noloc)
         endif
         write(ilun, err = 100 ) dlwqdata%param_pointered
         if ( dlwqdata%param_pointered ) then
            write(ilun, err = 100 ) (dlwqdata%param_pointers(ipar),ipar=1,nopar)
         endif
         write(ilun, err = 100 ) dlwqdata%loc_defaults
         write(ilun, err = 100 ) dlwqdata%loc_pointered
         if ( dlwqdata%loc_pointered ) then
            write(ilun, err = 100 ) (dlwqdata%loc_pointers(iloc),iloc=1,noloc)
         endif
         write(ilun, err = 100 ) dlwqdata%scaled
         write(ilun, err = 100 ) dlwqdata%scale_factor
         write(ilun, err = 100 ) dlwqdata%param_scaled
         if ( dlwqdata%param_scaled ) then
            write(ilun, err = 100 ) (dlwqdata%factor_param(ipar),ipar=1,nopar)
         endif
         write(ilun, err = 100 ) dlwqdata%loc_scaled
         if ( dlwqdata%loc_scaled ) then
            write(ilun, err = 100 ) (dlwqdata%factor_loc(iloc),iloc=1,noloc)
         endif
         if ( dlwqdata%functype .ne. FUNCTYPE_CONSTANT .and. dlwqdata%no_brk .gt. 0 ) then
            write(ilun, err = 100 ) (dlwqdata%times(ibrk),ibrk=1,nobrk)
         endif
         if ( dlwqdata%functype .eq. FUNCTYPE_HARMONIC .or. dlwqdata%functype .eq. FUNCTYPE_FOURIER ) then
            write(ilun, err = 100 ) (dlwqdata%phase(ibrk),ibrk=1,nobrk)
         endif
         if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
            write(ilun, err = 100 ) (((dlwqdata%values(ipar,iloc,ibrk),ipar=1,nopar),iloc=1,noloc),ibrk=1,nobrk)
         else
            write(ilun, err = 100 ) (((dlwqdata%values(iloc,ipar,ibrk),iloc=1,noloc),ipar=1,nopar),ibrk=1,nobrk)
         endif

         return
!
  100    continue

         ierror = 1
         return

      end function dlwqdataWrite

      function dlwqdataRead( lunrep, ilun, dlwqdata ) result ( ierror )
!
         integer, intent(in)                :: lunrep       ! unit number report file
         integer, intent(in)                :: ilun         ! unit number binary file with data
         type(t_dlwqdata), intent(out)      :: dlwqdata     ! datastructure to be filled
         integer                            :: ierror       ! return value
!
         integer                            :: ierr2        ! local error

         ierror  = 0

         read(ilun, err = 100 ) dlwqdata%subject
         read(ilun, err = 100 ) dlwqdata%no_param
         read(ilun, err = 100 ) dlwqdata%no_loc
         read(ilun, err = 100 ) dlwqdata%no_brk
         read(ilun, err = 100 ) dlwqdata%functype
         read(ilun, err = 100 ) dlwqdata%igrid
         read(ilun, err = 100 ) dlwqdata%extern
         read(ilun, err = 100 ) dlwqdata%filetype
         read(ilun, err = 100 ) dlwqdata%filename
         read(ilun, err = 100 ) dlwqdata%iorder
         read(ilun, err = 100 ) dlwqdata%param_named
         if ( dlwqdata%param_named ) then
            allocate(dlwqdata%param_name(dlwqdata%no_param))
            read(ilun, err = 100 ) dlwqdata%param_name
         endif
         read(ilun, err = 100 ) dlwqdata%loc_named
         if ( dlwqdata%loc_named ) then
            allocate(dlwqdata%loc_name(dlwqdata%no_loc))
            read(ilun, err = 100 ) dlwqdata%loc_name
         endif
         read(ilun, err = 100 ) dlwqdata%param_pointered
         if ( dlwqdata%param_pointered ) then
            allocate(dlwqdata%param_pointers(dlwqdata%no_param))
            read(ilun, err = 100 ) dlwqdata%param_pointers
         endif
         read(ilun, err = 100 ) dlwqdata%loc_defaults
         read(ilun, err = 100 ) dlwqdata%loc_pointered
         if ( dlwqdata%loc_pointered ) then
            allocate(dlwqdata%loc_pointers(dlwqdata%no_loc))
            read(ilun, err = 100 ) dlwqdata%loc_pointers
         endif
         read(ilun, err = 100 ) dlwqdata%scaled
         read(ilun, err = 100 ) dlwqdata%scale_factor
         read(ilun, err = 100 ) dlwqdata%param_scaled
         if ( dlwqdata%param_scaled ) then
            allocate(dlwqdata%factor_param(dlwqdata%no_param))
            read(ilun, err = 100 ) dlwqdata%factor_param
         endif
         read(ilun, err = 100 ) dlwqdata%loc_scaled
         if ( dlwqdata%loc_scaled ) then
            allocate(dlwqdata%factor_loc(dlwqdata%no_loc))
            read(ilun, err = 100 ) dlwqdata%factor_loc
         endif
         if ( dlwqdata%functype .ne. FUNCTYPE_CONSTANT .and. dlwqdata%no_brk .gt. 0 ) then
            allocate(dlwqdata%times(dlwqdata%no_brk))
            read(ilun, err = 100 ) dlwqdata%times
         endif
         if ( dlwqdata%functype .eq. FUNCTYPE_HARMONIC .or. dlwqdata%functype .eq. FUNCTYPE_FOURIER ) then
            allocate(dlwqdata%phase(dlwqdata%no_brk))
            read(ilun, err = 100 ) dlwqdata%phase
         endif
         if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
            allocate(dlwqdata%values(dlwqdata%no_param,dlwqdata%no_loc,max(dlwqdata%no_brk,1)))
         else
            allocate(dlwqdata%values(dlwqdata%no_loc,dlwqdata%no_param,max(dlwqdata%no_brk,1)))
         endif
         if ( .not. dlwqdata%extern ) then
            read(ilun, err = 100 ) dlwqdata%values
         else
            if ( dlwqdata%functype .eq. FUNCTYPE_CONSTANT ) then
               ierr2 = dlwqdataReadExtern(lunrep,dlwqdata)
               if ( ierr2 .ne. 0 ) goto 100
               dlwqdata%extern = .false.
            endif
         endif

         return
!
  100    continue

         ierror = 1
         return

      end function dlwqdataRead

      function dlwqdataEvaluate(dlwqdata,GridPs,itime,ndim1,ndim2,conc) result ( ierror )
!
         use dlwqgrid_mod
!
         type(t_dlwqdata)     , intent(in)       :: dlwqdata             ! data block to be used
         type(GridPointerColl), intent(in)       :: GridPs               ! collection off all grid definitions
         integer              , intent(in)       :: itime                ! system timer
         integer              , intent(in)       :: ndim1                ! number of substances
         integer              , intent(in)       :: ndim2                ! number of segments
         real                 , intent(inout)    :: conc(ndim1,ndim2)    ! concentrations to be set
         integer                                 :: ierror               !

!        local

         real                                    :: aa                   ! value at first breakpoint and final value
         real                                    :: ab                   ! value at second breakpoint
         real                                    :: factor               ! overall scale factor
         real                                    :: loc_factor           ! location scale factor
         real                                    :: param_factor         ! parameter scale factor
         integer                                 :: notot                ! number of parameters in output array
         integer                                 :: noseg                ! number of segments in output array
         integer                                 :: iloc                 ! index locations
         integer                                 :: ipar                 ! index parameters
         integer                                 :: ibrk                 ! index breakpoints
         integer                                 :: iseg, iseg2          ! index segments
         integer                                 :: isys                 ! index substances
         integer                                 :: itim1                ! first time
         integer                                 :: itim2                ! second time
         integer                                 :: itimf                ! time offset
         integer                                 :: idt                  ! step between times
         integer                                 :: it1c                 ! first time copy
         integer                                 :: it2c                 ! second time copy
         integer                                 :: idtc                 ! step copy
         integer                                 :: i                    ! loop counter
         real                                    :: amiss                ! missing value

         ierror  = 0
         amiss   = -999.0

         if ( dlwqdata%subject .eq. SUBJECT_SEGFUNC ) then
            notot = ndim2
            noseg = ndim1
         else
            notot = ndim1
            noseg = ndim2
         endif

         ! Get the right time in the block

         if ( dlwqdata%no_brk .gt. 1 ) then
            itim1 = dlwqdata%times(1)
            itim2 = dlwqdata%times(dlwqdata%no_brk)
            idt   = itim2 - itim1
            if ( itime .lt. itim1 ) then
               ibrk = 1
               itim1 = 0
               itim2 = 1
               idt   = itim1+itim2
            else
               itimf = itime
               if ( itime .ge. itim2 ) itimf = itime - ( (itime-itim2)/idt + 1 ) * idt

               ! make interpolation constants if iopt = 2

               do i = 2 , dlwqdata%no_brk
                  if ( dlwqdata%times(i) .gt. itimf ) then
                     if ( dlwqdata%functype .eq. FUNCTYPE_LINEAR ) then
                        itim1 = itimf   - dlwqdata%times(i-1)
                        itim2 = dlwqdata%times(i) - itimf
                     else
                        itim1 = 0
                        itim2 = 1
                     endif
                     idt   = itim1+itim2
                     ibrk  = i-1

                     exit

                  endif
               enddo
            endif
         else
            ibrk  = 1
            itim2 = 1
            itim1 = 0
            idt   = 1
         endif

         ! to-do uitzoeken of isys danwel iseg nul kunnen worden en de data niet gebruikt moet worden, in dat geval uit de betreffende loop springen

         if ( dlwqdata%scaled ) then
            factor = dlwqdata%scale_factor
         else
            factor = 1.0
         endif

         if ( dlwqdata%loc_defaults ) then ! default, also in case of igrid .ne. 1 ?
            iloc = 1
            if ( dlwqdata%loc_scaled ) then
               loc_factor = dlwqdata%factor_loc(iloc)*factor
            else
               loc_factor = factor
            endif
            do ipar = 1 , dlwqdata%no_param

               if ( dlwqdata%param_pointered ) then
                  isys = dlwqdata%param_pointers(ipar)
                  if ( isys .le. 0 ) cycle
               else
                  isys = ipar
               endif

               if ( dlwqdata%param_scaled ) then
                  param_factor = dlwqdata%factor_param(ipar)*factor
               else
                  param_factor = 1.0
               endif

               if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
                  aa = dlwqdata%values(ipar,iloc,ibrk)
               else
                  aa = dlwqdata%values(iloc,ipar,ibrk)
               endif
               if ( ibrk .lt. dlwqdata%no_brk ) then ! dlwqdata%nobrk can be 0 so use .lt. instead of .eq.
                  if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
                     ab = dlwqdata%values(ipar,iloc,ibrk+1)
                  else
                     ab = dlwqdata%values(iloc,ipar,ibrk+1)
                  endif
               else
                  ab = 0.0
               endif

               ! Dealing with missing values

               it1c = itim1
               it2c = itim2
               idtc = idt
               if ( aa .eq. amiss .or. ab .eq. amiss ) then
                    call dlwqdataGetValueMiss ( dlwqdata, ipar, iloc, ibrk , amiss, &
                                                itimf   , it1c, it2c, idtc , aa   , &
                                                ab      )
               endif

               ! Make the wanted value

               aa = ( it2c*aa + it1c*ab ) / idtc
               aa = aa*param_factor*loc_factor

               if ( dlwqdata%subject .eq. SUBJECT_SEGFUNC ) then
                  conc(:,isys) = aa
               else
                  conc(isys,:) = aa
               endif

            enddo

         else
            do iloc = 1 , dlwqdata%no_loc

               if ( dlwqdata%loc_scaled ) then
                  loc_factor = dlwqdata%factor_loc(iloc)*factor
               else
                  loc_factor = factor
               endif

               if ( dlwqdata%loc_pointered ) then
                  iseg = dlwqdata%loc_pointers(iloc)
                  if ( iseg .le. 0 ) cycle
               else
                  iseg = iloc
               endif

               do ipar = 1 , dlwqdata%no_param

                  if ( dlwqdata%param_pointered ) then
                     isys = dlwqdata%param_pointers(ipar)
                     if ( isys .le. 0 ) cycle
                  else
                     isys = ipar
                  endif

                  if ( dlwqdata%param_scaled ) then
                     param_factor = dlwqdata%factor_param(ipar)*factor
                  else
                     param_factor = 1.0
                  endif

                  if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
                     aa = dlwqdata%values(ipar,iloc,ibrk)
                  else
                     aa = dlwqdata%values(iloc,ipar,ibrk)
                  endif
                  if ( ibrk .lt. dlwqdata%no_brk ) then ! dlwqdata%nobrk can be 0 so use .lt. instead of .eq.
                     if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
                        ab = dlwqdata%values(ipar,iloc,ibrk+1)
                     else
                        ab = dlwqdata%values(iloc,ipar,ibrk+1)
                     endif
                  else
                     ab = 0.0
                  endif

                  ! Dealing with missing values

                  it1c = itim1
                  it2c = itim2
                  idtc = idt
                  if ( aa .eq. amiss .or. ab .eq. amiss ) then
                        call dlwqdataGetValueMiss ( dlwqdata, ipar, iloc, ibrk , amiss, &
                                                    itimf   , it1c, it2c, idtc , aa   , &
                                                    ab      )
                  endif

                  ! Make the wanted value

                  aa = ( it2c*aa + it1c*ab ) / idtc
                  aa = aa*param_factor*loc_factor

                  if ( dlwqdata%igrid .le. 1 ) then
                     if ( dlwqdata%subject .eq. SUBJECT_SEGFUNC ) then
                        conc(iseg,isys) = aa
                     else
                        conc(isys,iseg) = aa
                     endif
                  else
                     do iseg2 = 1 , noseg
                        if ( iseg .eq. GridPs%Pointers(dlwqdata%igrid)%finalpointer(iseg2) ) then
                           if ( dlwqdata%subject .eq. SUBJECT_SEGFUNC ) then
                              conc(iseg2,isys) = aa
                           else
                              conc(isys,iseg2) = aa
                           endif
                        endif
                     enddo
                  endif

               enddo
            enddo
         endif

      end function dlwqdataEvaluate

      function dlwq_find_name( dlwq_namelist, name ) result ( iret )

!        function to find a grid name in a collection of GridPointers

         type(t_dlwq_namelist)            :: dlwq_namelist
         character(LEN=*)                 :: name
         integer                          :: iret

!        local

         integer                          :: i

         iret = 0
         do i = 1 , dlwq_namelist%cursize
            if ( dlwq_namelist%name(i) .eq. name ) then
               iret = i
               exit
            endif
         end do

      end function dlwq_find_name

      function dlwq_init_item( dlwq_item ) result ( iret )

!        function to initialise an item structure

         type(t_dlwq_item)                :: dlwq_item
         integer                          :: iret

         iret = 0
         dlwq_item%no_item  = 0
         dlwq_item%maxsize  = 0
         dlwq_item%name     => null()
         dlwq_item%ipnt     => null()
         dlwq_item%sequence => null()
         dlwq_item%constant => null()

      end function dlwq_init_item

      function dlwq_find_item( dlwq_item, name ) result ( iret )

!        function to find a grid name in a collection of GridPointers

         type(t_dlwq_item)                :: dlwq_item
         character(LEN=*)                 :: name
         integer                          :: iret

!        local

         character(LEN=NAME_SIZE)         :: name_loc
         character(LEN=NAME_SIZE)         :: name_ucas
         integer                          :: i

         name_loc = name
         call dhucas(name_loc, name_ucas, NAME_SIZE)

         iret = 0
         do i = 1 , dlwq_item%no_item
            call dhucas(dlwq_item%name(i), name_loc, NAME_SIZE)
            if ( name_loc .eq. name_ucas ) then
               iret = i
               exit
            endif
         end do

      end function dlwq_find_item

      function dlwq_resize_item( dlwq_item, newsize ) result ( iret )

!        function to resize a dlwq_item (if needed)

         type(t_dlwq_item)                :: dlwq_item
         integer                          :: newsize
         integer                          :: iret

!        local
         integer, pointer                 :: iarray(:)
         real, pointer                    :: rarray(:)
         character(LEN=NAME_SIZE),pointer :: carray(:)
         integer                          :: newsize_extra

         iret = 0
         if ( newsize .gt. dlwq_item%maxsize ) then
            newsize_extra = newsize + MAX_NUM
            if ( dlwq_item%maxsize .gt. 0 ) then

               allocate(carray(newsize_extra))
               carray(1:dlwq_item%maxsize) = dlwq_item%name
               deallocate(dlwq_item%name)
               dlwq_item%name => carray

               allocate(iarray(newsize_extra))
               iarray(1:dlwq_item%maxsize) = dlwq_item%ipnt
               deallocate(dlwq_item%ipnt)
               dlwq_item%ipnt => iarray

               allocate(iarray(newsize_extra))
               iarray(1:dlwq_item%maxsize) = dlwq_item%sequence
               deallocate(dlwq_item%sequence)
               dlwq_item%sequence => iarray

               allocate(rarray(newsize_extra))
               rarray(1:dlwq_item%maxsize) = dlwq_item%constant
               deallocate(dlwq_item%constant)
               dlwq_item%constant => rarray

            else

               allocate(dlwq_item%name(newsize_extra))
               allocate(dlwq_item%ipnt(newsize_extra))
               allocate(dlwq_item%sequence(newsize_extra))
               allocate(dlwq_item%constant(newsize_extra))

            endif
            dlwq_item%maxsize = newsize_extra
         endif

      end function dlwq_resize_item

      function dlwqdataReadExtern(lunrep,dlwqdata) result ( ierror )

         use dlwqgrid_mod

         integer              , intent(in)       :: lunrep               ! unit number report file
         type(t_dlwqdata)     , intent(inout)    :: dlwqdata             ! data block to be used
         integer                                 :: ierror               ! return value

!        local

         integer                                 :: lun                  ! unit number
         integer                                 :: itime                ! time from file

         call dhnlun(701,lun)
         call open_waq_files( lun, dlwqdata%filename, 3  , 2    , ierror )
         if ( ierror .ne. 0 ) then
            write(lunrep,1000) trim(dlwqdata%filename)
            write(lunrep,1010) ierror
         else
            read(lun,iostat=ierror) itime, dlwqdata%values
            if ( ierror .ne. 0 ) then
               write(lunrep,1020) trim(dlwqdata%filename)
               write(lunrep,1010) ierror
            endif
         endif
         close(lun)

 1000 format(' ERROR opening external data file: ',A)
 1010 format(' error number:',I10)
 1020 format(' ERROR reading external data file: ',A)

      end function dlwqdataReadExtern

      subroutine dlwqdataGetValueMiss ( dlwqdata, ipar, iloc, ibrk , amiss, &
                                        itimf   , it1c, it2c, idtc , aa   , &
                                        ab      )

!     function : make function value in case of missing values

      type(t_dlwqdata)     , intent(in)       :: dlwqdata             ! data block to be used
      integer              , intent(in)       :: ipar                 ! index parameter to get
      integer              , intent(in)       :: iloc                 ! index location to get
      integer              , intent(in)       :: ibrk                 ! index current breakpoint
      real                 , intent(in)       :: amiss                ! missing value
      integer              , intent(in)       :: itimf                ! time offset
      integer              , intent(out)      :: it1c                 ! first time interpolation factor
      integer              , intent(out)      :: it2c                 ! second time interpolation factor
      integer              , intent(out)      :: idtc                 ! dt in interpolation
      real                 , intent(out)      :: aa                   ! first value in interpolation
      real                 , intent(out)      :: ab                   ! second value in interpolation

      ! local

      integer                                 :: jj, kk

      ! search backward for the first valid point

      do 10 jj = ibrk , 1 , -1
         if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
            aa = dlwqdata%values(ipar,iloc,jj)
         else
            aa = dlwqdata%values(iloc,ipar,jj)
         endif
         if ( aa .ne. amiss ) goto 20
   10 continue
      jj = 0
      aa = 0.0

      ! search forward for the first valid point

   20 continue
      do 30 kk = ibrk+1 , dlwqdata%no_brk
         if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
            ab = dlwqdata%values(ipar,iloc,kk)
         else
            ab = dlwqdata%values(iloc,ipar,kk)
         endif
         if ( ab .ne. amiss ) goto 40
   30 continue
      kk = 0
      ab = 0.0

   40 continue
      it1c = 0
      it2c = 0

      ! there was a backward valid point

      if ( jj .ne. 0 ) then
         if ( dlwqdata%functype .eq. FUNCTYPE_BLOCK ) it2c = 1
         if ( dlwqdata%functype .eq. FUNCTYPE_LINEAR ) then
            if ( kk .ne. 0 ) then
               it1c = itimf - dlwqdata%times(jj)
            else
               it2c = 1
            endif
         endif
      endif

      ! there was a forward valid point

      if ( kk .ne. 0 ) then
         if ( dlwqdata%functype .eq. FUNCTYPE_BLOCK .and. jj .eq. 0 ) it1c = 1
         if ( dlwqdata%functype .eq. FUNCTYPE_LINEAR ) then
            if ( jj .ne. 0 ) then
               it2c = dlwqdata%times(kk) - itimf
            else
               it1c = 1
            endif
         endif
      endif
      idtc = it1c + it2c
      if ( idtc .eq. 0 ) idtc = 1
!
      return
      end subroutine dlwqdataGetValueMiss

      subroutine dlwq_read_token_char( inpfil, ctoken, ierr)

      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      character(len=*)      , intent(out)   :: ctoken       ! character token
      integer               , intent(out)   :: ierr         ! error inidication

      integer                               :: t_asked      ! type of token asked

      t_asked = TYPE_ALL
      call read_token( inpfil, t_asked, ierr)
      ctoken = inpfil%ctoken

      end subroutine dlwq_read_token_char

      subroutine dlwq_read_token_int ( inpfil, itoken, ierr)

      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      integer               , intent(out)   :: itoken       ! integer token
      integer               , intent(out)   :: ierr         ! error inidication

      integer                               :: t_asked      ! type of token asked

      t_asked = TYPE_INT
      call read_token( inpfil, t_asked, ierr)
      itoken = inpfil%itoken

      end subroutine dlwq_read_token_int

      subroutine dlwq_read_token_real( inpfil, rtoken, ierr)

      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      real                  , intent(out)   :: rtoken       ! real token
      integer               , intent(inout) :: ierr         ! error inidication

      integer                               :: t_asked      ! type of token asked

      t_asked = TYPE_REAL
      call read_token( inpfil, t_asked, ierr)
      rtoken = inpfil%rtoken

      end subroutine dlwq_read_token_real

      SUBROUTINE read_token ( inpfil , itypex , ierr   )
! ----------------------------------------------------------------------
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : May '96  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : Reads a token en handles messages
!
!     SUBROUTINES CALLED : GETTOK - gets a token
!                          mes_token - messages input errors
!
!     LOGICAL UNITS      : LUNIN   = unit stripped DELWAQ input file
!                          lunrep  = unit formatted output file
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     lunrep   INTEGER     1       INPUT   unit number output file
!     ILUN    INTEGER   LSTACK    IN/OUT  unitnumb include stack
!     LCH     CHAR*(*)  LSTACK    IN/OUT  filename include stack
!     LSTACK  INTEGER     1       INPUT   include file stack size
!     NPOS    INTEGER     1       INPUT   nr of significant characters
!     CCHAR   CHAR*1      1       INPUT   comment character
!     IPOSR   INTEGER     1       IN/OUT  start position on line
!     NPOS    INTEGER     1       INPUT   width of the input file
!     CHULP   CHAR*(*)    1       OUTPUT  string  to be delivered
!     IHULP   INTEGER     1       OUTPUT  integer to be delivered
!     RHULP   REAL*4      1       OUTPUT  real    to be delivered
!     ITYPEX  INTEGER     1       INPUT   type expected
!     IERR    INTEGER     1       OUTPUT  Error code (see below)
!
!     ERROR CODES:
!
!     From GETTOK:                  From this routine:
!     -4 Integer overflow           1 Normal result
!     -3 Exponent out of range      1 Normal result
!     -2 Group separator found      1 General error/error reading etc.
!     -1 No delimiting quote        2 End of data group * message printed
!      0 Normal result              3 End of file       * only if itypex
!      1 End of file encountered    4 Unexpected type   * >0 at entry!
!      2 Read error encountered
!
! DATA ---------------------------------------------------- Arguments --

      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      integer               , intent(in)    :: itypex       ! type of token asked
      integer               , intent(inout) :: ierr         ! error inidication
!
! DATA -------------------------------------------------------- Local --
!
!     CHARACTER  LINE*1000, line2*80 , CHULP*1000
      character(len=80)                     :: line2        ! error line
      integer                               :: lunrep       ! unit number report file
      integer                               :: ifl          ! index in input filestack
      integer                               :: i            ! loop counter
      integer                               :: ioerr        ! io error inidication

! BEGIN ================================================================

      ierr = 0

!
!           Get the data
!
   10 continue

         call get_token(inpfil)

         ! if character wanted, do not mind overflows

         if ( itypex.eq. 1 .and. (inpfil%ierr .eq. -4 .or. inpfil%ierr .eq. -3) ) then
            inpfil%ierr    = 0
            inpfil%t_token = 1
         endif

         ! if all wanted, do not mind overflows

         if ( itypex.eq. 0 .and. (inpfil%ierr .eq. -4 .or. inpfil%ierr .eq. -3) ) then
            inpfil%ierr    = 0
            inpfil%t_token = 1
         endif
!
!           Deal with errors
!
!        Integer overflow
!
      if ( inpfil%ierr.eq.-4 .and. ( itypex.eq. 2 .or. itypex.eq. 0 .or. &
                                     itypex.eq.-1 .or. itypex.eq.-3    ) ) then
         line2= ' ERROR integer value too large or too small (OVERFLOW)'
         call mes_token ( inpfil , 0      , line2, 0 )
         inpfil%t_token = 2
         ierr   = 1
         goto 20
      endif
!        exponent out of range and real value allowed
      if ( inpfil%ierr.eq.-3 .and. ( itypex.eq. 3 .or. itypex.eq. 0 .or. &
                                     itypex.eq.-1 .or. itypex.eq.-2    ) ) then
         line2 = ' ERROR exponent too positive or too negative'
         call mes_token ( inpfil , 0      , line2, 0 )
         inpfil%t_token = 3
         ierr   = 1
         goto 20
      endif
!        End of data block found
      if ( inpfil%ierr .eq. -2 ) then
         if ( itypex .ne. 0 ) then
            line2 = ' ERROR unexpected end of datagroup on unit'
            call mes_token ( inpfil , 0      , line2, 0 )
            goto 20
         endif
         ierr   = 2
         return
      endif

      if ( inpfil%ierr .eq. -1 ) then
         line2 = ' No delimiting quote found !'
         call mes_token ( inpfil , 0      , line2, 0 )
         inpfil%t_token = 1
         ierr   = 1
         goto 20
      endif

      if ( inpfil%ierr .eq. 1 ) then

         if ( itypex .gt. 0 ) then

            line2 = ' End of file on the input unit'

            if ( inpfil%inputf .eq. 1 ) call mes_token ( inpfil , 0      , line2, 0 )

         endif
         ierr   = 3
         goto 20
      endif
      if ( inpfil%ierr .eq. 2 ) then
         line2 = ' ERROR reading from the input unit'
         call mes_token ( inpfil , 0      , line2, 0 )
         inpfil%t_token = 0
         ierr   = 1
         goto 20
      endif
!
!       write(*,*)  'Skip to a new file', ' ', chulp(:40)
!
      if ( inpfil%t_token .eq. 1 .and. inpfil%ctoken .eq. 'INCLUDE' ) then
         call getmlu(lunrep)
         if ( inpfil%inputf .eq. maxfil ) then
            write ( lunrep, 1020 ) maxfil
            ierr = 2
            goto 20
         endif
         call get_token(inpfil)
         if ( inpfil%t_token .ne. 1 .and. inpfil%t_token .ne. -1 ) then
            call mes_token ( inpfil , 1      , ' ', 0 )
            write ( lunrep, 1030 )
            ierr = 2
            goto 20
         endif
         write ( lunrep, 1040 ) inpfil%ctoken

         call filestack_add(inpfil,inpfil%ctoken,ioerr)
         if ( ioerr .gt. 0 ) then
            write ( lunrep, 1050 )
            ierr = 2
            goto 20
         else
            goto 10
         endif
      endif
      if ( (itypex .eq. 2 .and. inpfil%t_token.ne.2                  ) .or. &
           (itypex .eq. 3 .and. inpfil%t_token.ne.2 .and. inpfil%t_token.ne. 3) .or. &
!jvb       (itypex .eq. 1 .and. inpfil%t_token.ne.1 .and. inpfil%t_token.ne.-1) .or. &
           (itypex.eq.1.and.inpfil%t_token.ne.1.and.inpfil%t_token.ne.2.and.inpfil%t_token.ne.3.and.inpfil%t_token.ne.-1).or.&
           (itypex .eq.-1 .and. inpfil%t_token.eq.1                  ) .or. &
           (itypex .eq.-2 .and. inpfil%t_token.eq.2                  ) .or. &
           (itypex .eq.-3 .and. inpfil%t_token.eq.3                  )     ) then
         call mes_token ( inpfil , itypex , ' ', 0 )
         ierr = 4
      endif
      if ( inpfil%t_token .eq. 2 ) then
         inpfil%rtoken = inpfil%itoken
      endif

      if ( ierr .ne. 0 ) goto 20
      return

   20 continue
      call getmlu(lunrep)
      if ( ierr .eq. 3 ) then
         if ( inpfil%inputf .gt. 1 ) then
            write ( lunrep, 1000 ) trim(inpfil%finame(inpfil%inputf))
            close (inpfil%inplun(inpfil%inputf))
            inpfil%finame(inpfil%inputf) = ' '
            inpfil%inplun(inpfil%inputf) = 0
            inpfil%inputf = inpfil%inputf - 1
            write ( lunrep, 1010 ) trim(inpfil%finame(inpfil%inputf))
            inpfil%iposr  = 0
            goto 10
         else
            return
         endif
      else
         if ( inpfil%inputf .gt. 1 ) then
            do i = inpfil%inputf , 2 , -1
               write ( lunrep, 1000 ) trim(inpfil%finame(i))
               close (inpfil%inplun(i))
               inpfil%finame(i) = ' '
               inpfil%inplun(i) = 0
            enddo
            inpfil%inputf = 1
            inpfil%iposr  = 0
         endif
      endif
      return

 1000 format (/' Closing file: ',A )
 1010 format (/' Continuing on file: ',A )
 1020 format (/' ERROR: nr of include stack levels (',I2,') exceeded !')
 1030 format (/' Expected character string should be a valid ', &
                                         ' ASCII filename !' )
 1040 format (/' Including file: ',A )
 1050 format (/' ERROR: Include file does not exist !' )
      end subroutine read_token

      subroutine mes_token ( inpfil, itypex, linerr, ierr  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: may  - 1996 by L. Postma
!
!     FUNCTION            : Produces an error message
!
!     LOGICAL UNITNUMBERS : lunut - output file for the message
!
!     SUBROUTINES CALLED  : none
!
      type(inputfilestack)  , intent(inout) :: inpfil       ! input file strucure with include stack
      integer               , intent(in)    :: itypex       ! type of token asked
      character(len=*)      , intent(in)    :: linerr       ! error message to print
      integer               , intent(in)    :: ierr         ! error code to print

      integer                               :: inputf       ! current file in inputstack
      character(len=80)                     :: line2        ! extra line on output
      integer                               :: lunut        ! report file
      integer                               :: iwidth_trim  ! width of the line from input
      integer                               :: i            ! loop counter
      integer                               :: j            ! loop counter
      integer                               :: ilim         ! ilim
      character(len=10), parameter :: cchar = 'character'
      character(len=8 ), parameter :: cint  = 'integer'
      character(len=5 ), parameter :: creal = 'real'
!
      call getmlu(lunut)
      write ( lunut , * )

      ! File informatie als LUNIN ongelijk aan 0

      inputf = inpfil%inputf
      if ( inpfil%inplun(inputf) .ne. 0 ) then
         if ( inpfil%finame(inputf) .ne. ' ' ) then
            write ( lunut , 1000 ) inpfil%inplun(inputf), trim(inpfil%finame(inputf))
         else
            write ( lunut , 1010 ) inpfil%inplun(inputf)
         endif
      endif

      ! Regel informatie als regel geen blank

      if ( inpfil%linbuf(inputf) .ne. ' ' ) then
         write ( lunut , 1020 )
         if ( inpfil%iposl .ne. 0 .and. inpfil%iposr .ne. 0 ) then
            iwidth_trim = len_trim(inpfil%linbuf(inputf))
            do j = 1 , iwidth_trim , 80
               write ( lunut , '(a)' ) inpfil%linbuf(inputf)(j:min(j+79,iwidth_trim))
               line2 = ' '
               if ( inpfil%iposl-j .lt. 80 ) then
                  ilim = inpfil%iposr
                  if ( inpfil%t_token .eq. -1 ) ilim = ilim-1
                  do i = max(inpfil%iposl-j+1,1), min(ilim-j+1,80)
                     line2(i:i) = '^'
                  enddo
               endif
               write ( lunut , '(a)' ) line2
            enddo
         endif
      endif

      ! Error code ?

      if ( ierr .ne. 0 ) write ( lunut , 1030 ) ierr

      ! You can't always get what you want

      if ( itypex .eq. 1 ) write ( lunut , 1040 ) cchar
      if ( itypex .eq. 2 ) write ( lunut , 1050 ) cint
      if ( itypex .eq. 3 ) write ( lunut , 1040 ) creal
      if ( inpfil%t_token .eq. 1 .or. inpfil%t_token .eq. -1 ) &
                           write ( lunut , 1060 ) cchar
      if ( inpfil%t_token .eq. 2 ) write ( lunut , 1070 ) cint
      if ( inpfil%t_token .eq. 3 ) write ( lunut , 1060 ) creal
      if ( itypex .lt. 0 ) write ( lunut , 1080 )

      ! Something else to say ?

      if ( linerr .ne. ' ' ) write ( lunut , '(a)' ) trim(linerr)

      return

      ! Formats

 1000 format ( ' ERROR reading file on unit:',I4,', filename: ',A )
 1010 format ( ' ERROR reading file on unit:',I4,' !' )
 1020 format ( ' Line on input file was:' )
 1030 format ( ' Error code from input processor was: ',I2 )
 1040 format ( ' Expected was a ',A,'!' )
 1050 format ( ' Expected was an ',A,'!' )
 1060 format ( ' Detected was a ',A,'!' )
 1070 format ( ' Detected was an ',A,'!' )
 1080 format ( ' This item is NOT allowed at this location !' )

      end subroutine mes_token

      end module dlwqdata_mod
