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

module m_delwaq_2_openda

use delwaq2_global_data


implicit none

private

integer, parameter :: instance_block_size = 100            ! increase
integer, parameter :: no_instance_yet = -1                 ! initial value, indicating: no instances yet
integer, parameter :: storage_only_at_ctastate = 0         ! storage level: from delwaq towards COSTA state
integer, parameter :: storage_only_to_disk     = 1         ! storage level: from COSTA state to disk
integer, parameter :: storage_whole_range      = 2         ! storage level: to disk, all the
                                                           ! way from delwaq level
integer            :: max_instances_in_memory = 256        ! maximum number of instances in memory

integer, parameter :: UNCREATED_STATE = -1
integer, parameter :: UNCREATED_HANDLE = -1

integer            :: instance_count = 0                   ! actual #instances
integer, public    :: current_instance = no_instance_yet   ! currently active instance
integer            :: instances_in_memory = 0              ! current number of instances in memory

integer            :: state_handles(0:instance_block_size) = UNCREATED_STATE ! COSTA state handles
logical            :: state_in_memory(0:instance_block_size) ! flag: is the state in memory or on file?
character(len=256) :: file_names(0:instance_block_size)      ! State file names
integer            :: file_handles(0:instance_block_size)    ! COSTA state files handles



public :: dlwq_create_instance, dlwq_select_new_instance, dlwq_store_current_instance,&
          dlwq_ctastate_to_netcdf, dlwq_getcorestate, dlwq_setcorestate, dlwq_getinstancesize, &
          max_instances_in_memory, dlwq_close_cta_state_files, dlwq_reset_all

character(len=10)  :: state_file_extension = '.txt'           ! default: ascii; other possible value: nc (netcdf)


contains



!----------------------------
subroutine dlwq_reset_all()

  implicit none

  max_instances_in_memory = 256
  instance_count = 0
  current_instance = no_instance_yet
  instances_in_memory = 0
  state_handles = UNCREATED_STATE
  file_handles = UNCREATED_HANDLE

end subroutine dlwq_reset_all

!-----------------
subroutine dlwq_set_state_file_extension(a_state_file_extension)

    ! arguments
    character(len=*), intent(in) :: a_state_file_extension

    ! body

    state_file_extension = a_state_file_extension

end subroutine dlwq_set_state_file_extension


!-----------------
function dlwq_create_instance() result(instance_id)

    ! return value
    integer :: instance_id

    ! locals
    logical :: isbackground
    integer :: ierr
    integer :: i
    integer :: sz

    type(FilePropColl), pointer, dimension(:) :: collection

    if (current_instance == no_instance_yet) then
       ! First instance creation. Initialize Costa
        call CTA_CORE_INITIALISE(ierr)
        if (ierr .ne. 0) then
            print *,'ERROR initializing COSTA! '
            stop
        endif
    endif

    ! store current instance if needed
    if  (instances_in_memory == max_instances_in_memory) then
       call dlwq_store_current_instance(storage_only_to_disk)
       instances_in_memory = instances_in_memory - 1
       state_in_memory(current_instance) = .false.
       state_handles(instance_count) = state_handles(current_instance)
         ! This will be, in general, <> uncreated_state. So dlwq_create_state_vector will
         ! not allocate new memory but instead use the state structure of the old state.
    endif

    current_instance = instance_count

    isbackground = (current_instance == 0)

    call dlwq_create_cta_state_vector(isbackground, ierr)

    instance_id = current_instance

    ! memory/file management
    state_in_memory(current_instance) = .true.
    instances_in_memory = instances_in_memory + 1

    if (ierr /= 0) then
        instance_id = ierr  ! this is negative!
    endif

    !
    ! Extend the collection of file collections - we need to use a separate
    ! collection per instance
    !
    sz = 1
    if ( associated(dlwqd%PropCollArray) ) then
        sz = size(dlwqd%PropCollArray) + 1
    endif
    allocate( collection(sz) )
    do i = 1,sz-1
        collection(i) = dlwqd%PropCollArray(i)
    enddo
    call dlwq_copy_file_collection( collection(size(collection)), dlwqd%PropColl, instance_count == 1 )
    if ( associated(dlwqd%PropCollArray) ) then
        deallocate( dlwqd%PropCollArray )
    endif
    dlwqd%PropCollArray => collection

    instance_count = instance_count + 1

end function dlwq_create_instance

!-------------------------------------------
subroutine dlwq_copy_file_collection( new_collection, old_collection, true_copy )
    type(FilePropColl) :: new_collection
    type(FilePropColl) :: old_collection
    logical            :: true_copy

    integer :: i
    integer :: sz

    allocate( new_collection%FilePropPnts(old_collection%maxsize) )
    new_collection%maxsize = old_collection%maxsize
    new_collection%cursize = old_collection%cursize

    if ( true_copy ) then
        do i = 1,old_collection%cursize
            allocate( new_collection%FilePropPnts(i)%pnt )
            new_collection%FilePropPnts(i)%pnt = old_collection%FilePropPnts(i)%pnt

            sz = size(old_collection%FilePropPnts(i)%pnt%array1)
            allocate( new_collection%FilePropPnts(i)%pnt%array1(sz), &
                      new_collection%FilePropPnts(i)%pnt%array2(sz) )

            new_collection%FilePropPnts(i)%pnt%array1 = old_collection%FilePropPnts(i)%pnt%array1
            new_collection%FilePropPnts(i)%pnt%array2 = old_collection%FilePropPnts(i)%pnt%array2
        enddo
    else
        do i = 1,old_collection%cursize
            new_collection%FilePropPnts(i)%pnt => old_collection%FilePropPnts(i)%pnt
        enddo
    endif
end subroutine dlwq_copy_file_collection


!-------------------------------------------
subroutine dlwq_select_new_instance(instance_id)

    ! arguments
    integer, intent(in) :: instance_id

    !local
    integer :: ierr
    integer :: i


    ! TODO: check id and throw error if not OK

    ! First check if instance is already selected
    if (current_instance /= instance_id) then

       ! First store the old instance (it may be necessary to save to disk
       ! if the new instance is not in memory yet)
       if (instances_in_memory == max_instances_in_memory .and.    &
               (.not. state_in_memory(instance_id))) then

          call dlwq_store_current_instance(storage_whole_range)


          ! state handle is now free, so reserve for the new instance!
          state_handles(instance_id) = state_handles(current_instance)

          instances_in_memory = instances_in_memory - 1
          state_in_memory(current_instance) = .false.

       else
          ! saving to disk is not necessary
          call dlwq_store_current_instance(storage_only_at_ctastate)
       endif

       current_instance = instance_id

       ! If necessary: Load the new instance from disk
       if (.not. state_in_memory(current_instance)) then
          call dlwq_retrieve_state_from_disk(current_instance)

          instances_in_memory = instances_in_memory + 1
          state_in_memory(current_instance) = .true.
       endif

       ! set the instance state towards delwaq
       call dlwq_set_ctastate_to_delwaq(ierr)
    endif

    !
    ! Make sure DELWAQ will use the correct collection of files
    !
    do i = 1,dlwqd%PropCollArray(current_instance+1)%cursize
        dlwqd%PropColl%FilePropPnts(i)%pnt => dlwqd%PropCollArray(current_instance+1)%FilePropPnts(i)%pnt
    enddo

end subroutine dlwq_select_new_instance

!-------------------------------------------------------------------
subroutine dlwq_store_current_instance(storage_level)

   implicit none

   integer :: storage_level
   integer :: ierr

   !  first get the instance state from delwaq.
   ! if the cta_state is already up to date, this is not necessary!
   if (storage_level == storage_only_at_ctastate .or.            &
       storage_level == storage_whole_range) then
       call dlwq_get_ctastate_from_delwaq(ierr)
   endif
   !  If necessary: Save the  instance to disk
   !  TODO
   if (storage_level == storage_only_to_disk .or.            &
       storage_level == storage_whole_range) then
       call dlwq_save_instance_to_disk(current_instance)
   endif


end subroutine dlwq_store_current_instance

!---------------------------------------------
subroutine dlwq_retrieve_state_from_disk(instance_id)
   implicit none
   include 'cta_f77.inc'

   integer :: instance_id
   integer :: file_handle, sFilename, sReadmode
   integer :: ierr
   character(len=10) :: instance_as_string
   character(len=100) :: fname

!  note: file contains only one time level, of the entire state
!  The file is opened, read, and immediately closed again
!  TODO delete the file?

   write(instance_as_string,'(i3.3)') instance_id
   fname='TMPstate_'//trim(instance_as_string)//'.nc'

   call CTA_STRING_CREATE(sFilename,ierr)
   call CTA_STRING_SET(sFileName,fname,ierr)
   call CTA_FILE_CREATE(file_handle,ierr)
   call CTA_STRING_CREATE(sReadmode,ierr)
   call CTA_STRING_SET(sReadmode,"r",ierr)
   ! the netcdf-file should already exist; so nc_create should not be called!
   call CTA_FILE_OPEN(file_handle,sFileName,sReadmode,ierr)

   call CTA_TREEVECTOR_IMPORT(state_handles(instance_id),file_handle,ierr)
   ! close netcdf-file
   call CTA_FILE_FREE(file_handle,ierr)
   call CTA_STRING_FREE(sFilename,ierr)
   call CTA_STRING_FREE(sReadmode,ierr)

end subroutine dlwq_retrieve_state_from_disk

!---------------------------------------------
subroutine dlwq_save_instance_to_disk(instance_id)
   implicit none
   include 'cta_f77.inc'

   integer :: instance_id
   integer :: file_handle, sFilename, sWritemode
   integer :: ierr
   character(len=10) :: instance_as_string
   character(len=100) :: fname

!  note: only one time level is saved, of the entire state
!  The fle is created or opened, written, and immediately closed again

   write(instance_as_string,'(i3.3)') instance_id
   fname='TMPstate_'//trim(instance_as_string)//'.nc'

   call CTA_STRING_CREATE(sFilename,ierr)
   call CTA_STRING_SET(sFileName,fname,ierr)
   call CTA_FILE_CREATE(file_handle,ierr)
   call CTA_STRING_CREATE(sWritemode,ierr)
   call CTA_STRING_SET(sWritemode,"w",ierr)
   call CTA_FILE_OPEN(file_handle,sFileName,sWritemode,ierr)

   call CTA_TREEVECTOR_EXPORT(state_handles(instance_id),file_handle,ierr)
   ! close netcdf-file
   call CTA_FILE_FREE(file_handle,ierr)
   call CTA_STRING_FREE(sFilename,ierr)
   call CTA_STRING_FREE(sWritemode,ierr)

end subroutine dlwq_save_instance_to_disk


!-----------------------------------------------
subroutine dlwq_getinstancesize(inst_size)

  implicit none
  include 'cta_f77.inc'
  integer :: inst_size, ierr

  ! not necessary to update the state; size does not change
   call CTA_TREEVECTOR_GETSIZE(state_handles(current_instance), inst_size, ierr)

end subroutine  dlwq_getinstancesize

!-------------------------------------------------------------------
!-------------------------------------------------------------------
subroutine dlwq_getcorestate(corestate, size_corestate, retval)

! note: we would like to get the state directly from delwaq and NOT from Costa!
! however, we need it in 1D. So we first get the costa state and then perform a getvals.
! Cumbersome, but this is only temporary.

  implicit none

  include 'cta_f77.inc'

  integer, intent (out) :: retval
  integer :: s_core,  ierr, size_corestate
  double precision, dimension(size_corestate) :: corestate
  !!real, dimension(size_corestate) :: r_corestate
  real, dimension(:), allocatable :: r_corestate

  allocate( r_corestate(size_corestate) )

  call dlwq_get_ctastate_from_delwaq(ierr)

  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_GETVALS(s_core, r_corestate, size_corestate, CTA_REAL,ierr)
  if (ierr .ne. CTA_OK) then
     print *,'dlwq_getcorestate: error getting values ',size_corestate
     retval = -1
  else
     corestate = r_corestate
     retval = 0
  endif


end subroutine dlwq_getcorestate

!-------------------------------------------------------------------
!-------------------------------------------------------------------
subroutine dlwq_setcorestate(corestate,size_corestate,retval)
! note: we would like to set the state directly to delwaq and NOT to Costa!
! however, we provide it in 1D. So we first set the costa state and then perform a setvals.
! Cumbersome, but this is only temporary.

  implicit none


  include 'cta_f77.inc'
  integer :: size_corestate
  double precision, dimension(size_corestate), intent(in) :: corestate
  !!real, dimension(size_corestate) :: r_corestate
  real, dimension(:), allocatable :: r_corestate

  integer, intent(out) :: retval
  integer :: s_core, ierr

  allocate( r_corestate(size_corestate) )

  r_corestate = corestate
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)

  call CTA_TREEVECTOR_SETVALS(s_core, r_corestate, size_corestate, CTA_REAL,ierr)
  if (ierr .ne. CTA_OK) then
      print *,'dlwq_setcorestate: error setting values ',size_corestate
      retval = -1
  else
     call dlwq_set_ctastate_to_delwaq(ierr)
     retval = ierr
  endif

  deallocate( r_corestate )

end subroutine dlwq_setcorestate


!-------------------------------------------------------------------

subroutine dlwq_create_cta_state_vector(isbackground, ierr)

  include 'cta_f77.inc'
 !arguments
  integer         , intent(out) :: ierr          ! error status
  logical         , intent(in)  :: isbackground  ! is this the background state?

  !Local variables
  integer :: sfilename
  character(len=10) :: instance_as_string
  ! Sub-treevectors
  integer ::s_core, s_pseudo, s_output
  ! Sub-treevectors
  integer :: s_conc, s_other
  integer :: s_rbuf, s_mass, s_names, s_timeadmin
  ! Array of sub-treeVectors
  integer, dimension(3) :: sub_states
  integer, dimension(2) :: sub_cores
  integer, dimension(4) :: sub_pseudos
  integer :: hdescr    ! Costa metainfo handle
  integer :: notot, noseg
  integer :: ier2, iColl

! prepare output file : create and open
  write(instance_as_string,'(i3.3)') current_instance
  file_names(current_instance)='cta_state_'//trim(instance_as_string)// trim(state_file_extension)

  ! file creating and opening is not needed yet!
  ! That part has moved to an actual corestate-export; there
  ! the first check is if the file already has been opened.

  if (state_handles(current_instance) == UNCREATED_STATE) then
    ! this will in general be the case for new instances. Costa will create a handle
    ! for the treevector. Later, when the tv is filled, memory will be allocated.
    ! However, if a handle already exists, no new tv-handles will be created. Later,
    ! when the tv is filled, no more extra memory is allocated. The values will
    ! be overwritten so they should have been saved before!

      call CTA_TREEVECTOR_CREATE('whole state','delwaq',state_handles(current_instance), ierr) ;if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('core state',  'core',   s_core, ierr); if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('pseudo state', 'pseudo', s_pseudo, ierr); if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('output state', 'output', s_output, ierr); if (ierr/=CTA_OK) goto 9999

      call CTA_TREEVECTOR_CREATE('concentration','conc',s_conc, ierr)
      call CTA_TREEVECTOR_CREATE('other','other',s_other, ierr)

      if (ierr/=CTA_OK) goto 9999

       ! now add metainfo to substates. First: conc
      call CTA_METAINFO_CREATE(hdescr,ierr)
      if (ierr .ne. 0) goto 9999
      notot = size_dlwq_state%notot

      noseg = size_dlwq_state%noseg
      call CTA_METAINFO_SETREGGRID(hdescr, 'concGrid', notot , noseg , 1, &
                                    0.0d0, 0.0d0, 0.0d0,            &
                                    1.0d0, 1.0d0, 0.0d0, ierr)
      if (ierr .ne. 0) goto 9999

      call CTA_METAINFO_FREE(hdescr,ier2)
      if (ier2/=CTA_OK) then
          print *,'error in freeing metainfo'
          pause
      endif

      sub_cores(1)=s_conc
      sub_cores(2)=s_other

       ! concatenate all core substates substate 'core'
      call CTA_TREEVECTOR_CONC(s_core, sub_cores, 2, ierr); if (ierr/=CTA_OK) goto 9999

    ! now: pseudo state
      call CTA_TREEVECTOR_CREATE('real buffers','rbuf',s_rbuf, ierr)
      call CTA_TREEVECTOR_CREATE('mass','mass',s_mass, ierr)
      call CTA_TREEVECTOR_CREATE('names','names',s_names, ierr)
      call CTA_TREEVECTOR_CREATE('timeadmin','timeadmin',s_timeadmin, ierr)
      sub_pseudos(1) = s_rbuf
      sub_pseudos(2) = s_mass
      sub_pseudos(3) = s_names
      sub_pseudos(4) = s_timeadmin


     ! concatenate all pseudo substates to substate 'pseudo'
      call CTA_TREEVECTOR_CONC(s_pseudo, sub_pseudos, 4, ierr); if (ierr/=CTA_OK) goto 9999

      sub_states(1) = s_core
      sub_states(2) = s_pseudo
      sub_states(3) = s_output
       ! concatenate the three main substates in delwaq state
      call CTA_TREEVECTOR_CONC(state_handles(current_instance), sub_states, 3, ierr); if (ierr/=CTA_OK) goto 9999



      ! set 'nocompute' flag for pseudo and output
      call CTA_TREEVECTOR_SETSUBTREENOCOMPUTE(state_handles(current_instance), 'pseudo',ierr)
      call CTA_TREEVECTOR_SETSUBTREENOCOMPUTE(state_handles(current_instance), 'output',ierr)

  endif
  ! --endif of section "create new treevectors"

  ! now adjust output part of delwaq_state, if necessary
  call dlwq_adjust_output_state(isbackground)

  ! now initialize the values of the state
  ! note: if the state is 'old', this does not require extra memory!

  call dlwq_get_ctastate_from_delwaq(ierr)

  return

 9999 continue
   print *,'FATAL ERROR IN dlwq_create_cta_state_vector ierr=',ierr
   call exit(-1)

end subroutine dlwq_create_cta_state_vector
!----------------------------------


!-----------------
subroutine dlwq_ctastate_to_netcdf(ierr)

  include 'cta_f77.inc'

 !arguments
  integer, intent(out) :: ierr

  ! local
  integer :: s_core  ! COSTA handle to core substate
  integer :: sfilename

  ! first check if the file already has been created!
  if (file_handles(current_instance) == UNCREATED_HANDLE) then
     call CTA_STRING_CREATE(sFilename,ierr)
     call CTA_STRING_SET(sFileName,file_names(current_instance),ierr)
     call CTA_FILE_CREATE(file_handles(current_instance),ierr)
     call CTA_FILE_OPEN(file_handles(current_instance),sFileName,CTA_NULL,ierr)
     call CTA_STRING_FREE(sFilename,ierr)
  endif

 ! only export core state!
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_EXPORT(s_core,file_handles(current_instance),ierr)
  if (ierr .ne. CTA_OK) then
     print *, 'ERROR ctastate_to_netcdf ',ierr
  endif

end subroutine dlwq_ctastate_to_netcdf
!--------------------------------


subroutine dlwq_close_cta_state_files(ierr)

  include 'cta_f77.inc'
 !arguments
  integer, intent(out) :: ierr
integer :: i

  ierr = 0
! close open files
  do i=0, instance_count-1
    if (file_handles(i) /= UNCREATED_HANDLE) then
       call CTA_FILE_FREE(file_handles(i),ierr)
    endif
  enddo


end subroutine dlwq_close_cta_state_files


!----------------------------------------------------------------

subroutine dlwq_set_ctastate_to_delwaq(ierr)


  include 'cta_f77.inc'
  include 'sysi_ff.inc'
  include 'sysa_ff.inc'   ! for iconc
  include 'sysj_ff.inc'   ! for iiout
  include 'sysn_ff.inc'   ! for noutp
 !arguments
 ! (none)

 !local parameters
  integer :: s_core, s_pseudo, s_output
  integer :: s_conc, s_other
  integer :: s_rbuf, s_mass, s_names, s_timeadmin
  integer :: hnames(size_dlwq_state%names)
  integer :: vals_timeadmin(3),  vals_output(size_dlwq_state%output)
  integer :: ierr, i, iColl, j, kk

! gebruik getsubtreevec! (by reference, dus free-en niet nodig)
! NB we kunnen niet een (1) grote getvals doen omdat het natuurlijk niet allemaal CTA_doubles zijn.

! pseudo ...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'pseudo', s_pseudo, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'mass', s_mass, ierr)
  call CTA_TREEVECTOR_GETVALS(s_mass, dlwqd%rbuf(imass), size_dlwq_state%mass, CTA_REAL,ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'rbuf', s_rbuf, ierr)
  call CTA_TREEVECTOR_GETVALS(s_rbuf, dlwqd%rbuf(1), size_dlwq_state%rbuf, CTA_REAL,ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'names', s_names, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'timeadmin', s_timeadmin, ierr)

!
! Core state ...
!
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'conc', s_conc, ierr)

  call CTA_TREEVECTOR_GETVALS(s_conc, dlwqd%rbuf(iconc), size_dlwq_state%conc, CTA_REAL,ierr)

  !strings
  do i=1, size_dlwq_state%names
    call CTA_STRING_CREATE(hnames(i),ierr)
  enddo
  call CTA_TREEVECTOR_GETVALS(s_names,hnames,size_dlwq_state%names,CTA_STRING,ierr)
  do i=1, size_dlwq_state%names
    call CTA_STRING_GET(hnames(i),substance_name(i),ierr)
  enddo
  do i=1,size_dlwq_state%names
    call CTA_STRING_FREE(hnames(i),ierr)
  enddo

  ! time admin
  call CTA_TREEVECTOR_GETVALS(s_timeadmin,vals_timeadmin,3,CTA_INTEGER,ierr)
  dlwqd%itime = vals_timeadmin(1)

! output...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'output', s_output, ierr)

  call CTA_TREEVECTOR_GETVALS(s_output, vals_output, size_dlwq_state%output, CTA_INTEGER,ierr)
  imstrt = vals_output(1)
  imstep = vals_output(2)
  imstop = vals_output(3)
  ihstrt = vals_output(4)
  ihstep = vals_output(5)
  ihstop = vals_output(6)
  idstrt = vals_output(7)
  idstep = vals_output(8)
  idstop = vals_output(9)
  dlwqd%ibuf(iiout:iiout+7*noutp-1) = vals_output(10:size_dlwq_state%output)

end subroutine dlwq_set_ctastate_to_delwaq


!---------------------------------------------

subroutine dlwq_get_ctastate_from_delwaq(ierr)


  include 'cta_f77.inc'
  include 'sysi_ff.inc'
  include 'sysa_ff.inc'
  include 'sysj_ff.inc'   ! for iiout
  include 'sysn_ff.inc'   ! for noutp

 !arguments

  integer, intent(out) :: ierr


!local parameters
  integer :: s_core, s_pseudo, s_output
  integer :: s_conc, s_other
  integer :: s_rbuf, s_mass, s_names, s_timeadmin
  integer :: hnames(size_dlwq_state%names)  !  CTA_Strings
  integer :: vals_timeadmin(3), vals_output(size_dlwq_state%output)
  integer :: i, j, kk, iColl, rbufsize
  real:: rdum(1)

! gebruik getsubtreevec! (by reference, dus free-en niet nodig)
! NB we kunnen niet een (1) grote getvals doen omdat het natuurlijk niet allemaal CTA_doubles zijn.

  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'conc', s_conc, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'other', s_other, ierr)
  call CTA_TREEVECTOR_SETVALS(s_conc, dlwqd%rbuf(iconc), size_dlwq_state%conc, CTA_REAL,ierr)
    rdum(1) = 0.0
  call CTA_TREEVECTOR_SETVALS(s_other, rdum, 1, CTA_REAL,ierr)

! pseudo ...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'pseudo', s_pseudo, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'mass', s_mass, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'rbuf', s_rbuf, ierr)
  call CTA_TREEVECTOR_SETVALS(s_mass, dlwqd%rbuf(imass), size_dlwq_state%mass, CTA_REAL,ierr)
  call CTA_TREEVECTOR_SETVALS(s_rbuf, dlwqd%rbuf(1), size_dlwq_state%rbuf, CTA_REAL,ierr)

  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'names', s_names, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'timeadmin', s_timeadmin, ierr)

  !strings
  do i=1,size_dlwq_state%names
    call CTA_STRING_CREATE(hnames(i),ierr)
  enddo
  do i=1,size_dlwq_state%names
    call CTA_STRING_SET(hnames(i),substance_name(i),ierr)
  enddo
  call CTA_TREEVECTOR_SETVALS(s_names,hnames,size_dlwq_state%names,CTA_STRING,ierr)
  do i=1,size_dlwq_state%names
    call CTA_STRING_FREE(hnames(i),ierr)
  enddo

  ! time admin
  vals_timeadmin(1) = dlwqd%itime
  call CTA_TREEVECTOR_SETVALS(s_timeadmin,vals_timeadmin,3,CTA_INTEGER,ierr)

  ! output...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'output', s_output, ierr)
  vals_output(1) = imstrt
  vals_output(2) = imstep
  vals_output(3) = imstop
  vals_output(4) = ihstrt
  vals_output(5) = ihstep
  vals_output(6) = ihstop
  vals_output(7) = idstrt
  vals_output(8) = idstep
  vals_output(9) = idstop
  vals_output(10:size_dlwq_state%output) = dlwqd%ibuf(iiout:iiout + 7*noutp - 1)

  call CTA_TREEVECTOR_SETVALS(s_output, vals_output, size_dlwq_state%output, CTA_INTEGER,ierr)

end subroutine dlwq_get_ctastate_from_delwaq

!----------------------------------------------

 subroutine dlwq_adjust_output_state(isbackground)

   implicit none

   include 'cta_f77.inc'
   include 'sysi_ff.inc'
   include 'sysj_ff.inc'   ! for iiout
   include 'sysn_ff.inc'   ! for noutp

   logical :: isbackground

   character*3 :: ch_imode
   character*256 :: tmpchar
   integer :: ic


   if (.not. isbackground) then
 ! change these values such that stepyn will not allow a write to file!
     imstrt = 0
     imstep = 0
     imstop = 0
     ihstrt = 0
     ihstep = 0
     ihstop = 0
     idstrt = 0
     idstep = 0
     idstop = 0
     ! also adjust ioutps
     dlwqd%ibuf(iiout:iiout + 7*noutp - 1) = 0

   endif

   write(ch_imode,'(I3.3)') current_instance

   if (.false.) then
     ! adjust the names of the output files using a suffix.
     ! We will not do that yet. For the time being, only mode 0 will give output, other instances
     ! (.not. isbackground) simply do not produce output.

   endif

end subroutine dlwq_adjust_output_state

!----------------------------------------------------------------------

end module m_delwaq_2_openda


