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

      module dlwq_hyd_data
      use m_srstop
      use m_monsys
      use m_open_waq_files
      use m_string_utils
!
!          module contains everything for model data input and storage

!
!     contains the following derived types:
!
!          t_dlwqdata              ! poperties with respect to a data item
!
!          t_dlwqdatacoll          ! a collection of dlwqdata, for instance all wasteloads
!
      implicit none
!
      integer, parameter :: ITEM_NAME_SIZE    =  20          ! length all names
      integer, parameter :: NAME_SIZE         =  20          ! size of descriptive names
      integer, parameter :: FILE_NAME_SIZE    = 256          ! length all names
      integer, parameter :: MAX_NUM           =   5          ! allocated per bunch

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
      integer, parameter :: FILE_UNFORMATTED  = 5            ! real unformatted file (so not binary)
      integer, parameter :: FILE_BIG_ENDIAN  = 10            ! big endian pattern (Telemac)

      type t_dlwqdata
         integer                                :: subject           ! subject for this data
         integer                                :: no_param          ! number of paramters in this block of data
         integer                                :: no_loc            ! number of locations
         integer                                :: no_brk            ! number of breakpoints or harmonics
         integer                                :: functype          ! constant, block, linear, harmonics, fourier
         integer                                :: igrid             ! grid number of input
         logical                                :: extern            ! is data in file or online coupling
         integer                                :: filetype          ! type of external data source
         character(len=FILE_NAME_SIZE)          :: filename          ! name of file or dataset in coupling
         integer                                :: lun               ! unit number external file
         integer                                :: iorder            ! ordering of the data matrix, param-loc or loc-param
         logical                                :: param_named       ! are the parameters named
         character(len=ITEM_NAME_SIZE), pointer :: param_name(:)     ! parameter names
         logical                                :: loc_named         ! are the locations named
         character(len=ITEM_NAME_SIZE), pointer :: loc_name(:)       ! location names
         logical                                :: param_pointered   ! are the parameters pointered
         integer, pointer                       :: param_pointers(:) ! index of the parameters in the waq substance/constants/etc arrays
         logical                                :: loc_defaults      ! data is default for all locations
         logical                                :: loc_pointered     ! are the locations pointered
         integer, pointer                       :: loc_pointers(:)   ! segment number of the locations in the specific grid
         logical                                :: scaled            ! overall scaling applied?
         real                                   :: scale_factor      ! overall scaling factor
         logical                                :: param_scaled      ! need the parameters scaling
         real, pointer                          :: factor_param(:)   ! scale factors for parameters if any
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

!     this is a collection of data_items

      type t_dlwq_data_items
         type(t_dlwq_item), pointer       :: dlwq_foritem(:) ! pointer
         character(LEN=NAME_SIZE),pointer :: name(:)         ! names of item
         logical, pointer                 :: used(:)         ! flag
         integer                          :: maxsize         ! maximum size of the current array
         integer                          :: cursize         ! filled up to this size
      end type t_dlwq_data_items

      integer, parameter :: TYPE_CHAR   =  1                 ! character
      integer, parameter :: TYPE_INT    =  2                 ! integer
      integer, parameter :: TYPE_REAL   =  3                 ! real
      integer, parameter :: TYPE_ALL    =  0                 ! all types allowed
      integer, parameter :: TYPE_NOCHAR = -1                 ! no character allowed
      integer, parameter :: TYPE_NOINT  = -2                 ! no integer allowed
      integer, parameter :: TYPE_NOREAL = -3                 ! no real allowed

      ! the remnant of the old implementation

      type inputfilestack
         logical             :: is_date_format          ! is_date_format
         logical             :: dtflg2          ! dtflg2
         logical             :: is_yyddhh_format          ! is_yyddhh_format
         integer             :: itfact          ! itfact
         integer             :: iblock          ! input block
         integer             :: ierr            ! error on inputfile
      endtype inputfilestack

!     overload the operations

      interface dlwq_init
         module procedure dlwq_init_item
      end interface

      interface dlwq_cleanup
         module procedure dlwq_cleanup_item
      end interface

      interface dlwq_resize
         module procedure dlwq_resize_item
      end interface

      interface dlwq_find
         module procedure dlwq_find_item
         module procedure dlwq_find_name
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
         integer                            :: i

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
            read(ilun, err = 100 ) (dlwqdata%loc_name(i) ,i=1,dlwqdata%no_loc)
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
         use timers
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
         real, allocatable                       :: tmp_conc(:,:)        ! store result on different grid in temp array
         logical, allocatable                    :: iseg_set(:)          ! indicates if segment is set in temporary array
         integer(4) ithandl /0/
         if ( timon ) call timstrt ( "dlwqdataevaluate", ithandl )

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

         ! to-do find out if isys or iseg can become zero and the data must not be used, in that case exit the relevant loop

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
            if ( dlwqdata%igrid .gt. 1 ) then
               allocate(tmp_conc(dlwqdata%no_param,dlwqdata%no_loc),iseg_set(dlwqdata%no_loc))
               iseg_set = .false.
            endif
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
                     iseg_set(iseg) = .true.
                     tmp_conc(ipar,iseg) = aa
                  endif

               enddo
            enddo
            if ( dlwqdata%igrid .gt. 1 ) then
               do iseg2 = 1 , noseg
                  iseg = GridPs%Pointers(dlwqdata%igrid)%finalpointer(iseg2)
                  if ( iseg .gt. 0 ) then
                     if ( iseg_set(iseg) ) then
                        do ipar = 1 , dlwqdata%no_param
                           if ( dlwqdata%param_pointered ) then
                              isys = dlwqdata%param_pointers(ipar)
                              if ( isys .le. 0 ) cycle
                           else
                              isys = ipar
                           endif
                           if ( dlwqdata%subject .eq. SUBJECT_SEGFUNC ) then
                              conc(iseg2,isys) = tmp_conc(ipar,iseg)
                           else
                              conc(isys,iseg2) = tmp_conc(ipar,iseg)
                           endif
                        enddo
                     endif
                  endif
               enddo
               deallocate(tmp_conc,iseg_set)
            endif
         endif

         if ( timon ) call timstop ( ithandl )
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

      function dlwq_cleanup_item( dlwq_item ) result ( iret )

!        function to clean up an item structure

         type(t_dlwq_item)                :: dlwq_item
         integer                          :: iret
         logical                          :: l_alloc

         iret = 0
         dlwq_item%no_item  = 0
         dlwq_item%maxsize  = 0
         l_alloc = associated(dlwq_item%name)
         if ( l_alloc ) deallocate(dlwq_item%name)
         l_alloc = associated(dlwq_item%ipnt)
         if ( l_alloc ) deallocate(dlwq_item%ipnt)
         l_alloc = associated(dlwq_item%sequence)
         if ( l_alloc ) deallocate(dlwq_item%sequence)
         l_alloc = associated(dlwq_item%constant)
         if ( l_alloc ) deallocate(dlwq_item%constant)
         dlwq_item%name     => null()
         dlwq_item%ipnt     => null()
         dlwq_item%sequence => null()
         dlwq_item%constant => null()

      end function dlwq_cleanup_item

      function dlwq_find_item( dlwq_item, name ) result ( iret )

!        function to find a grid name in a collection of GridPointers

         type(t_dlwq_item)                :: dlwq_item
         character(LEN=*)                 :: name
         integer                          :: iret

!        local

         integer                          :: i
         integer                          :: iaindx

         iret = 0
         do i = 1 , dlwq_item%no_item
            if (string_equals(name(1:NAME_SIZE), dlwq_item%name(i))) then
               iret = i
               exit
            endif
         enddo

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

      function dlwq_init_data_items( dlwq_data_items ) result ( iret )

!        function to initialise an data_items structure

         type(t_dlwq_data_items)          :: dlwq_data_items
         integer                          :: iret

         dlwq_data_items%dlwq_foritem  => null()
         dlwq_data_items%name          => null()
         dlwq_data_items%used          => null()
         dlwq_data_items%cursize       = 0
         dlwq_data_items%maxsize       = 0
         iret = 0

      end function dlwq_init_data_items

      function dlwq_data_itemsAdd( dlwq_data_items, data_item_name, dlwq_foritem ) result ( cursize )
!
         type(t_dlwq_data_items)            :: dlwq_data_items
         character(LEN=NAME_SIZE)           :: data_item_name         ! name of item to add
         type(t_dlwq_item)                  :: dlwq_foritem
         integer                            :: cursize

!        local

         type(t_dlwq_item), pointer         :: dlwq_foritems(:)         ! should be a pointer for the resize operation
         character(LEN=NAME_SIZE), pointer  :: data_item_names(:)       ! names of data_items
         logical, pointer                   :: data_item_used(:)       ! use status of data_items
         integer                            :: ierr_alloc1
         integer                            :: ierr_alloc2
         integer                            :: ierr_alloc3
         integer                            :: i
!
         if ( dlwq_data_items%cursize .eq. dlwq_data_items%maxsize ) then
            allocate ( dlwq_foritems ( dlwq_data_items%maxsize + MAX_NUM ) , stat = ierr_alloc1)
            allocate ( data_item_names ( dlwq_data_items%maxsize + MAX_NUM ) , stat = ierr_alloc2)
            allocate ( data_item_used ( dlwq_data_items%maxsize + MAX_NUM ) , stat = ierr_alloc3)
            if ( ierr_alloc1 .ne. 0 .or. ierr_alloc2 .ne. 0 .or. ierr_alloc3 .ne. 0 ) then
               write(*,*) 'ERROR : ALLOCATING WORK ARRAY'
               call srstop(1)
            endif
            do i = 1 , dlwq_data_items%maxsize
               dlwq_foritems(i) = dlwq_data_items%dlwq_foritem(i)                ! copies the contents
               data_item_names(i) = dlwq_data_items%name(i)                      ! copies the contents
               data_item_used(i) = dlwq_data_items%used(i)                       ! copies the contents
            enddo
            if ( dlwq_data_items%maxsize .ne. 0 ) deallocate ( dlwq_data_items%dlwq_foritem )
            dlwq_data_items%dlwq_foritem => dlwq_foritems                        ! attaches this new array of pointers
            dlwq_data_items%name => data_item_names                              ! attaches this new array of pointers
            dlwq_data_items%used => data_item_used                              ! attaches this new array of pointers
            dlwq_data_items%maxsize = dlwq_data_items%maxsize + MAX_NUM
         endif
         dlwq_data_items%cursize = dlwq_data_items%cursize + 1
         dlwq_data_items%dlwq_foritem( dlwq_data_items%cursize ) = dlwq_foritem
         dlwq_data_items%name( dlwq_data_items%cursize ) = data_item_name
         dlwq_data_items%used( dlwq_data_items%cursize ) = .false.
         cursize = dlwq_data_items%cursize
         return
!
      end function dlwq_data_itemsAdd

      function dlwqdataReadExtern(lunrep,dlwqdata) result ( ierror )

         use m_file_unit_number

         integer              , intent(in)       :: lunrep               ! unit number report file
         type(t_dlwqdata)     , intent(inout)    :: dlwqdata             ! data block to be used
         integer                                 :: ierror               ! return value

!        local

         integer                                 :: lun                  ! unit number
         integer                                 :: itime                ! time from file
         integer                                 :: nopar                ! local copy number of parameters
         integer                                 :: noloc                ! local copy number of locations
         integer                                 :: nobrk                ! local copy number of breakpoints
         integer                                 :: ipar                 ! index paramaters
         integer                                 :: iloc                 ! index locations
         integer                                 :: ibrk                 ! index breakpoints
         integer                                    ftype                ! the equivalent of the ftype array elsewhere

         nopar = dlwqdata%no_param
         noloc = dlwqdata%no_loc
         nobrk = max(dlwqdata%no_brk,1)

         call create_new_file_unit_number(701,lun)

         ftype = 2
         if ( mod(dlwqdata%filetype,10) .eq. FILE_UNFORMATTED ) ftype = ftype + 10
         if ( dlwqdata%filetype/10 .eq. 1 ) ftype = ftype + 20       ! I am in for a better solution (lp)

         call open_waq_files( lun, dlwqdata%filename, 3  , ftype , ierror )

         if ( ierror .ne. 0 ) then
            write(lunrep,1000) trim(dlwqdata%filename)
            write(lunrep,1010) ierror
         else
            if ( dlwqdata%iorder .eq. ORDER_PARAM_LOC ) then
               if ( .not. associated(dlwqdata%values) ) allocate(dlwqdata%values(nopar,noloc,nobrk))
               read(lun,iostat=ierror) itime,(((dlwqdata%values(ipar,iloc,ibrk),ipar=1,nopar),iloc=1,noloc),ibrk=1,nobrk)
            else
               if ( .not. associated(dlwqdata%values) ) allocate(dlwqdata%values(noloc,nopar,nobrk))
               read(lun,iostat=ierror) itime,(((dlwqdata%values(iloc,ipar,ibrk),iloc=1,noloc),ipar=1,nopar),ibrk=1,nobrk)
            endif
            if ( ierror .ne. 0 ) then
               write(lunrep,1020) trim(dlwqdata%filename)
               write(lunrep,1010) ierror
            endif
         endif
         close(lun)

 1000 format(' ERROR opening external data file:',A)
 1010 format(' error number:',I10)
 1020 format(' ERROR reading external data file:',A)

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

      function dlwqdataCopy( data1, data2 ) result ( ierror )

         type(t_dlwqdata), intent(in)       :: data1        ! data to be copied
         type(t_dlwqdata), intent(out)      :: data2        ! copy of the data
         integer                            :: ierror       !

         ! local decalaration

         integer                            :: nopar        ! local copy number of parameters
         integer                            :: noloc        ! local copy number of locations
         integer                            :: nobrk        ! local copy number of breakpoints
         integer                            :: ipar         ! index paramaters
         integer                            :: iloc         ! index locations
         integer                            :: ibrk         ! index breakpoints
         integer                            :: lunrep       ! unit number report file
         integer                            :: ierr_alloc

         call getmlu(lunrep)

         ierror  = 0
         nopar = data1%no_param
         noloc = data1%no_loc
         nobrk = data1%no_brk

         data2%subject     = data1%subject
         data2%no_param    = data1%no_param
         data2%no_loc      = data1%no_loc
         data2%no_brk      = data1%no_brk
         data2%functype    = data1%functype
         data2%igrid       = data1%igrid
         data2%extern      = data1%extern
         data2%filetype    = data1%filetype
         data2%filename    = data1%filename
         data2%iorder      = data1%iorder
         data2%param_named = data1%param_named
         if ( data2%param_named ) then
            allocate(data2%param_name(nopar), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%param_name = data1%param_name
         endif
         data2%loc_named = data1%loc_named
         if ( data2%loc_named ) then
            allocate(data2%loc_name(noloc), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%loc_name = data1%loc_name
         endif
         data2%param_pointered = data1%param_pointered
         if ( data2%param_pointered ) then
            allocate(data2%param_pointers(nopar), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%param_pointers = data1%param_pointers
         endif
         data2%loc_defaults  = data1%loc_defaults
         data2%loc_pointered = data1%loc_pointered
         if ( data2%loc_pointered ) then
            allocate(data2%loc_pointers(noloc), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%loc_pointers = data1%loc_pointers
         endif
         data2%scaled        = data1%scaled
         data2%scale_factor  = data1%scale_factor
         data2%param_scaled  = data1%param_scaled
         if ( data2%param_scaled ) then
            allocate(data2%factor_param(nopar), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%factor_param = data1%factor_param
         endif
         data2%loc_scaled    = data1%loc_scaled
         if ( data2%loc_scaled ) then
            allocate(data2%factor_loc(noloc), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%factor_loc = data1%factor_loc
         endif
         if ( data2%functype .ne. FUNCTYPE_CONSTANT .and. data2%no_brk .gt. 0 ) then
            allocate(data2%times(nobrk), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%times = data1%times
         endif
         if ( data2%functype .eq. FUNCTYPE_HARMONIC .or. data2%functype .eq. FUNCTYPE_FOURIER ) then
            allocate(data2%phase(nobrk), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            data2%phase = data1%phase
         endif
         if ( data2%iorder .eq. ORDER_PARAM_LOC ) then
            allocate(data2%values(nopar,noloc,nobrk), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            do ibrk = 1, nobrk
               do iloc = 1, noloc
                  do ipar = 1, nopar
                     data2%values(ipar,iloc,ibrk) = data1%values(ipar,iloc,ibrk)
                  enddo
               enddo
            enddo
         else
            allocate(data2%values(noloc,nopar,nobrk), stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(lunrep,*) ' error allocating memory' ; ierror = 1 ; return ; endif
            do ibrk = 1, nobrk
               do ipar = 1, nopar
                  do iloc = 1, noloc
                     data2%values(iloc,ipar,ibrk) = data1%values(iloc,ipar,ibrk)
                  enddo
               enddo
            enddo
         endif

         return

      end function dlwqdataCopy

      end module dlwq_hyd_data
