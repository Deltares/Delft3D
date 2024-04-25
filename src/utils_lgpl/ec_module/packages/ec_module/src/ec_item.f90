!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
!

!> This module contains all the methods for the datatype tEcItem.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
!! @author Edwin Bos
module m_ec_item
   use string_module
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   use m_ec_converter
   use m_ec_filereader
   use m_alloc
   use time_class

   implicit none

   private

   public :: ecItemCreate
   public :: ecItemInitialize
   public :: ecItemFree1dArray
   public :: ecItemGetValuesMJD
   public :: ecItemGetValues
   public :: ecItemSetRole
   public :: ecItemSetType
   public :: ecItemSetQuantity
   public :: ecItemSetProperty
   public :: ecItemCopyProperty
   public :: ecItemSetElementSet
   public :: ecItemSetSourceT0Field
   public :: ecItemSetSourceT1Field
   public :: ecItemSetTargetField
   public :: ecItemAddConnection
   public :: ecItemGetProvider
   public :: ecItemGetArr1dPtr
   public :: ecItemGetQHtable
   public :: ecItemEstimateResultSize
   public :: ecItemToTimeseries
   public :: ecItemFinalizeTimeseries
   public :: ecItemFromTimeseries

   contains

      ! =======================================================================

      !> Construct a new Item with the specified id.
      !! Failure is indicated by returning a null pointer
      function ecItemCreate(itemId) result(itemPtr)
         type(tEcItem), pointer            :: itemPtr !< the new Item, intent(out)
         integer,               intent(in) :: itemId  !< unique Item id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(itemPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_item::ecItemCreate: Unable to allocate additional memory")
            itemPtr => null()
            return
         end if
         allocate(itemPtr%connectionsPtr(1), stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_item::ecItemCreate: Unable to allocate additional memory")
            itemPtr => null()
            return
         end if
         ! initialization
         itemPtr%id = itemId
         itemPtr%role = itemType_undefined
         itemPtr%accessType = accessType_undefined
         itemPtr%providerId = ec_undef_int
         itemPtr%nConnections = 0
      end function ecItemCreate

      ! =======================================================================

      !> Free a tEcItem, after which it can be deallocated.
      function ecItemFree(item) result(success)
         logical                      :: success !< function status
         type(tEcItem), intent(inout) :: item    !< Item to free
         !
         integer :: i !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         ! An item does not own the tEcQuantities, the tEcInstance does, so nullify rather then ecQuantityFree().
         item%quantityPtr => null()
         ! An item does not own the tEcElementSets, the tEcInstance does, so nullify rather then ecElementSetFree().
         item%elementSetPtr => null()
         ! An item does not own the tEcFields, the tEcInstance does, so nullify rather then ecFieldFree().
         item%sourceT0FieldPtr => null()
         item%sourceT1FieldPtr => null()
         item%targetFieldPtr => null()
         ! An Item does not own the tEcConnections, the tEcInstance does, so nullify rather then ecConnectionFree().
         do i=1, item%nConnections
            item%connectionsPtr(i)%ptr => null()
         end do

         if (allocated(item%timeseries)) then
            if (allocated(item%timeseries%times)) then
                deallocate(item%timeseries%times, stat = istat)
                if (istat /= 0) success = .false.
            end if
            if (allocated(item%timeseries%values)) then
                deallocate(item%timeseries%values, stat = istat)
                if (istat /= 0) success = .false.
            end if
            deallocate(item%timeseries, stat = istat)
            if (istat /= 0) success = .false.
         end if

         ! Finally deallocate the array of tEcConnection pointers.
         deallocate(item%connectionsPtr, STAT = istat)
         if (istat /= 0) success = .false.
      end function ecItemFree

      ! =======================================================================

      !> Frees a 1D array of tEcItemPtrs, after which the itemPtr is deallocated.
      function ecItemFree1dArray(itemPtr, nItems) result(success)
         logical                                 :: success !< function status
         type(tEcItemPtr), dimension(:), pointer :: itemPtr !< intent(inout)
         integer, intent(inout)                  :: nItems  !< number of Items
         !
         integer :: i     !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(itemPtr)) then
            call setECMessage("WARNING: ec_item::ecItemFree1dArray: Dummy argument itemPtr is already disassociated.")
         else
            ! Free and deallocate all tEcItemPtrs in the 1d array.
            do i=1, nItems
               if (ecItemFree(itemPtr(i)%ptr)) then
                  deallocate(itemPtr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcItemPtr(:) pointer.
            if (success) then
               deallocate(itemPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
         nItems = 0
      end function ecItemFree1dArray

      ! =======================================================================

      !> Retrieve the data of an Item for timestep specified as MJD
      function ecItemGetValuesMJD(instancePtr, itemId, timeAsMJD, target_array) result(success)
         logical                                                 :: success      !< function status
         type(tEcInstance),      pointer                         :: instancePtr  !< intent(in)
         integer,                                  intent(in)    :: itemID       !< unique Item id
         double precision,                         intent(in)    :: timeAsMJD    !< time stamp as Modified Julian Day
         real(hp), dimension(:), target, optional, intent(inout) :: target_array !< kernel's data array for the requested values
         !
         type(c_time)  :: ecTime
         call ecTime%set(timeAsMJD)
         success = ecItemGetValues(instancePtr, itemId, ecTime, target_array)
         !
      end function ecItemGetValuesMJD

      ! =======================================================================

      !> Retrieve the data of an Item for a specific number of timesteps since kernel's reference date and put it in the target Item's Field.
      recursive function ecItemGetValues(instancePtr, itemId, timesteps, target_array) result(success)
         use m_ec_message
         implicit none
         logical                                                 :: success      !< function status
         type(tEcInstance),      pointer                         :: instancePtr  !< intent(in)
         integer,                                  intent(in)    :: itemID       !< unique Item id
         type(c_time),                             intent(in)    :: timesteps    !< get data corresponding to this number of timesteps since k_refdate
         character(len=1000)                                     :: message
         real(hp), dimension(:), target, optional, intent(inout) :: target_array !< kernel's data array for the requested values
         !
         type(tEcItem), pointer :: itemPtr !< Item under consideration
         !
! TODO: Edwin: UNST-683: somehow this itemPtr is *not* thread-safe. When called via ec_gettimespacevalue (see unstruc.f90, ec_gettimespacevalue(ecInstancePtr, item_damlevel, tim, zcdam), with OMP SECTIONS)
!       Access violations and irregular end of files occur. Setting OMP_NUM_THREADS=1 'solves' the errors.
         success = .false.
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (.not.associated(itemPtr)) then
            write(message,'(a,i8)') "Updating target failed, item not found, itemId=",itemId
            call setECMessage(trim(message))
            return
         endif
         if (itemPtr%role == itemType_target) then
            if (present(target_array)) then
               itemPtr%targetFieldPtr%arr1dPtr => target_array
            end if
            if (.not.ecItemUpdateTargetItem(instancePtr, itemPtr, timesteps)) then
               write(message,'(a,i8,a)') "Updating target failed, quantity='" &
                               //trim(itemPtr%quantityPtr%name)   &
                               //"', itemId=",itemPtr%id
               call setECMessage(trim(message))
               return
            endif
         endif
         success = .true.
      end function ecItemGetValues

! =======================================================================
      !> Retrieve the id of the provider (filereader) that supplies this item
      function ecItemEstimateResultSize(instancePtr, itemId) result(ressize)
         implicit none
         integer                    :: ressize
         type(tEcInstance), pointer :: instancePtr  !< intent(in)
         integer, intent(in)        :: itemId
         type(tEcItem), pointer     :: itemPtr !< Item under consideration
         character(len=1000)        :: message

         ressize = -1
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (.not.associated(itemPtr)) then
            write(message,'(a,i8)') "Updating target failed, item not found, itemId=",itemId
            call setECMessage(trim(message))
            return
         endif
         if (itemPtr%role == itemType_target) then
            ressize = itemPtr%quantityPtr%vectormax &
                    * max(itemPtr%elementsetPtr%nCoordinates,1) &
                    * max(1,itemPtr%elementsetPtr%n_layers)
         endif
      end function ecItemEstimateResultSize

! =======================================================================
      !> Retrieve the id of the provider (filereader) that supplies this item
      function ecItemGetProvider(instancePtr, itemId) result(providerID)
         implicit none
         integer                               :: providerID
         type(tEcInstance),      pointer       :: instancePtr  !< intent(in)
         integer,                intent(in)    :: itemID       !< unique Item id

         character(len=1000)    :: message
         type(tEcItem), pointer :: itemPtr            !< Item under consideration

         providerID = ec_undef_int
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (.not.associated(itemPtr)) then
            write(message,'(a,i8)') "Updating target failed, item not found, itemId=",itemId
            call setECMessage(trim(message))
            return
         endif
         providerID = itemPtr%providerID
      end function ecItemGetProvider

! =======================================================================
      !> Retrieve pointers to the Q and H arrays of a QH-table associated with this item
      !> Returns true if this source item is associated with a QH-table
      function ecItemGetQHtable(instancePtr, itemId, h_values, q_values, success) result(is_QH)
      implicit none
      logical                             :: is_QH
      type(tEcInstance),      pointer     :: instancePtr  !< intent(in)
      integer, intent(in)                 :: itemId
      real(hp), dimension(:),     pointer :: q_values, h_values
      logical, intent(out)                :: success

      integer  ::    ec_qh_provider_id
      integer  ::    item_H, item_Q

      success = .False.
      is_QH = .False.
      h_values => null()
      q_values => null()
      ec_qh_provider_id = ecItemGetProvider(instancePtr, itemId)                              ! request the provider for this item
      if (ec_qh_provider_id<1) then
         return
         ! TODO: something went wrong, issue an error message
      end if
      if (ecFileReaderProvidesQHtable(instancePtr, ec_qh_provider_id, item_H, item_Q)) then   ! Infer if this provider is of type QH, if so ...
                                                                                              ! NB: either item_H == itemId or item_Q == itemId
         h_values => ecItemGetArr1DPtr(instancePtr, item_H, 0)                                !    obtain a pointer to the waterlevels
         q_values => ecItemGetArr1DPtr(instancePtr, item_Q, 0)                                !    obtain a pointer to the discharges
         is_QH = .True.
      end if
      success = .True.
      end function ecItemGetQHtable

! =======================================================================
      !> Retrieve a pointer to the item's field's arr1d
      function ecItemGetArr1DPtr(instancePtr, itemId, selector) result(Arr1DPtr)
         implicit none
         type(tEcInstance),      pointer       :: instancePtr  !< intent(in)
         integer,                intent(in)    :: itemID       !< unique Item id
         integer,                intent(in)    :: selector     !< selects field, 0:source0, 1:source1, 2:target
         real(hp), dimension(:), pointer       :: Arr1DPtr     !< points to a 1-dim array field, stored in arr1d OR in a kernel

         character(len=1000)     :: message
         type(tEcItem) , pointer :: itemPtr
         type(tEcField), pointer :: fieldptr

         arr1dPtr => null()
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (.not.associated(itemPtr)) then
            write(message,'(a,i8)') "Updating target failed, item not found, itemId=",itemId
            call setECMessage(trim(message))
            return
         endif
         select case (selector)
         case(0)
            fieldptr => itemPtr%sourceT0FieldPtr
         case(1)
            fieldptr => itemPtr%sourceT1FieldPtr
         case(2)
            fieldptr => itemPtr%targetFieldPtr
         case default
            fieldptr => null()
         end select
         if (associated(fieldptr)) then
            if (allocated(fieldptr%arr1d)) then
               Arr1DPtr => fieldptr%arr1d
            else
               Arr1DPtr => fieldptr%arr1dPtr
            endif
         end if
      end function ecItemGetArr1DPtr
! =======================================================================


      !> Retrieve data for a specific number of timesteps since the kernel's reference date by first updating the source Items.
      !! Their data is processed through a Converter, which updates the Field of each of the Connection's target Items.
      function ecItemUpdateTargetItem(instancePtr, item, timesteps) result(success)
         logical                          :: success     !< function status
         type(tEcInstance), pointer       :: instancePtr !< intent(inout)
         type(tEcItem),     intent(inout) :: item        !< the target item
         type(c_time),      intent(in)    :: timesteps   !< get data corresponding to this number of timesteps since k_refdate
         !
         integer                            :: istat        !< return value of stat
         integer                            :: i            !< loop variable
         integer                            :: j            !< loop variable
         character(len=1000)                :: message
         logical, dimension(:), allocatable :: skipWeights  !< Flags for each connection if weight computation can be skipped
         integer                            :: ntimes
         logical                            :: first_item_is_periodic
         type(tEcItem), pointer             :: source_item  !< the source item
         !
         success = .true.
         first_item_is_periodic = .false.

         allocate(skipWeights(item%nConnections), stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_item::ecItemUpdateTargetItem: Unable to allocate additional memory")
            success = .false.
         end if
         !
         ! Initialize skipWeights array to true
         ! When updating source items:
         ! For each connection i:
         ! If there is one (or more) sourceItem(s) that needs weights then skipWeights(i)=false
         if (success) then
            skipWeights = .true.
            !
            ! update the source Items
            UpdateSrc: do i=1, item%nConnections
               do j=1, item%connectionsPtr(i)%ptr%nSourceItems
                  source_item => item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr
                  if (first_item_is_periodic .and. j > 1) then
                     write(message,'(A)') 'Multiple periodic source items not yet supported'
                     success = .false.
                     return
                  endif
                  if (associated(item%connectionsPtr(i)%ptr%converterPtr)) then
                     if (.not. (ecItemUpdateSourceItem(instancePtr, item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr, timesteps, &
                                item%connectionsPtr(i)%ptr%converterPtr%interpolationType))) then
                        ! If updating source item failed .....
                        ! No interpolation in time possible. => skipWeights(i) is allowed to stay true
                        if(source_item%quantityPtr%timeint == timeint_bfrom) then
                           ! We came beyond the last line in a non-periodic block function
                           ! Adjust the value in T0 field (the converter will only use the T0-field)s
                           source_item%sourceT0FieldPtr%arr1d = source_item%sourceT1FieldPtr%arr1d
                        endif
                        ! Check whether extrapolation is allowed
                        if (item%connectionsPtr(i)%ptr%converterPtr%interpolationType /= interpolate_time_extrapolation_ok) then
                           write(message,'(a,i8,a)') "Updating source failed, quantity='" &
                                           //trim(item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr%quantityPtr%name)   &
                                           //"', item=",item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr%id,   &
                                             ", location="//trim(item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr%elementsetPtr%name)
                           call setECMessage(trim(message))
                           success = .false.
                           exit UpdateSrc
                        end if
                     else
                        skipWeights(i) = .false.
                     end if
                  else
                     if (.not. (ecItemUpdateSourceItem(instancePtr, item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr,  &
                                                       timesteps, interpolate_time_extrapolation_ok))) then
                        ! Note: time interp. does not apply here, so we use the most forgiving type
                        ! 'interpolate_time_extrapolation_ok' to avoid duplicate time exceeded errors.
                        success = .false.
                        return
                     end if
                  end if
               end do
            end do UpdateSrc
         endif
         ! update the weight factors
         if (success) then
            UpdateWeight: do i=1, item%nConnections
               if (.not. skipWeights(i)) then
                  if (.not. (ecConverterUpdateWeightFactors(instancePtr, item%connectionsPtr(i)%ptr))) then
                     write(message,'(a,i5.5)') "Updating weights failed, connection='",item%connectionsPtr(i)%ptr%id
                     call setECMessage(trim(message))
                     success = .false.
                     exit UpdateWeight
                  end if
               end if
            end do UpdateWeight
         end if
         ! Always try to perform the conversions, which update the target Items
         if (success) then
            do i=1, item%nConnections
               if (.not. (ecConverterPerformConversions(item%connectionsPtr(i)%ptr, timesteps))) then
                  write(message,'(a,i5.5)') "Converter operation failed, connection='",item%connectionsPtr(i)%ptr%id
                  call setECMessage(trim(message))
                  success = .false.
                  exit
               end if
            end do
         end if

         ! clean up
         if (allocated(skipWeights)) then
            deallocate(skipWeights, stat = istat)
            if (istat /= 0) then
               call setECMessage("Warning: deallocate skipWeights failed. Will continue.")
               success = .false.
            end if
         end if
!        if (associated(item%quantityPtr)) then
!            if (item%quantityPtr%constant) then
!                item%nconnections = 0
!            end if
!        end if
      end function ecItemUpdateTargetItem

      ! =======================================================================

      !> Retrieve data from a FileReader as needed to achieve t0<=timesteps<=t1.
      function ecItemUpdateSourceItem(instancePtr, item, timesteps, interpol_type) result(success)
         logical                                  :: success       !< function status
         type(tEcInstance), pointer               :: instancePtr   !< intent(inout)
         type(tEcItem),             intent(inout) :: item          !< the source item
         type(c_time),              intent(in)    :: timesteps     !< objective: t0<=timesteps<=t1
         integer ,                  intent(in)    :: interpol_type !< interpolation
         !
         integer                                  :: i                     !< loop counter
         integer                                  :: j                     !< loop counter
         type(tEcFileReader), pointer             :: fileReaderPtr         !< helper pointer for a file reader
         character(len=22)                        :: strnum1               !< 1st number converted to string for error message
         character(len=22)                        :: strnum2               !< 2nd number converted to string for error message
         character(len=*), parameter              :: fmtBignum = '(f22.3)' !< format string also suitable for very big numbers
         character(len=maxFileNameLen), pointer   :: filename              !< file name in error message
         !
         success = .false.
         fileReaderPtr => null()
         !
         ! Check whether the source Items are actually poly_tim Items of type target.
         if (item%role == itemType_target) then
            if (.not. ecItemGetValues(instancePtr, item%id, timesteps)) then
               return
            end if
            success = .true.
            return
         end if
         !
         if (item%accessType==accessType_fileReader) then
            fileReaderPtr => ecSupportFindFileReader(instancePtr, item%providerId)
         endif
         !
         ! timesteps < t0 : not supported
         if (item%quantityPtr%constant) then
            ! TODO EM: Check if we shouldn't be remapping values in this case.
            success = .true.
            return
         else if (comparereal(item%sourceT1FieldPtr%timesteps, timesteps%mjd(), 1.0D-10) == 0) then
            ! requested time equals to T1.
            ! no read action needed, UNLESS 'block-from'
            if (item%quantityPtr%timeint /= timeint_bfrom) then
               success = .true.
               return
            endif
         else if (comparereal(timesteps%mjd(), item%sourceT0FieldPtr%timesteps) == -1) then
            if (interpol_type /= interpolate_time_extrapolation_ok) then
               if (associated (fileReaderPtr)) then
                  if (associated (fileReaderPtr%bc)) then
                     filename => fileReaderPtr%bc%fname
                  else
                     filename => fileReaderPtr%fileName
                  endif
                  call setECMessage("       in file: '"//trim(filename)//"'.")
               endif
               call real2stringLeft(strnum1, '(f22.3)', timesteps%mjd())
               call setECMessage("             Requested: t= " // trim(strnum1) // ' seconds')
               call real2stringLeft(strnum1, '(f22.3)', item%sourceT0FieldPtr%timesteps)
               call setECMessage("       Current EC-time: t= " // trim(strnum1) // ' seconds')
               call real2stringLeft(strnum1, '(f22.3)', item%sourceT0FieldPtr%timesteps-timesteps%mjd())
               call real2stringLeft(strnum2, '(f22.3)', (item%sourceT0FieldPtr%timesteps-timesteps%mjd())*86400)
               call setECMessage("Requested time preceeds current forcing EC-timelevel by " // trim(strnum1) // " days = " // trim(strnum2) // " seconds.")
            else
               success = .true.
            endif
            return
         else if (comparereal(timesteps%mjd(), item%sourceT1FieldPtr%timesteps) == -1) then
            ! requested time is before T1
            success = .true.
            return
         endif

         ! timesteps > t1: update untill t0<=timesteps<=t1

         ! Store the zeroth value into the timeseries
         if (item%quantityPtr%periodic) then
            if (.not.allocated(item%timeseries)) then       ! This must be the first time we are getting here, store value
               if (.not.ecItemToTimeseries(item,item%sourceT0FieldPtr%timesteps,item%sourceT0FieldPtr%arr1dptr)) then
                  ! TODO: handle exception, report error
                  success = .false.
                  return
               end if
               if (.not.ecItemToTimeseries(item,item%sourceT1FieldPtr%timesteps,item%sourceT1FieldPtr%arr1dptr)) then
                  ! TODO: handle exception, report error
                  success = .false.
                  return
               end if
            end if
         endif


         ! Update all source Items which belong to the found FileReader, if associated .
         if (associated(fileReaderPtr)) then
            if (.not. fileReaderPtr%end_of_data) then
               if (associated(item%hframe)) then
                  ! Read and remap source amplitudes (Time input will be ignored)
                  success = ecFileReaderReadNextRecord(fileReaderPtr, timesteps%mjd())
               else
                  do ! read next record untill t0<=timesteps<=t1
                     if (ecFileReaderReadNextRecord(fileReaderPtr, timesteps%mjd())) then
                        if (item%quantityPtr%periodic) then                      ! Store saved to item's timeseries
                           if (.not.ecItemToTimeseries(item,item%sourceT1FieldPtr%timesteps,item%sourceT1FieldPtr%arr1dptr)) then
                              success = .false.
                              return
                           end if
                        end if
                        if (comparereal(item%sourceT1FieldPtr%timesteps, timesteps%mjd()) /= -1) then
                           if (item%quantityPtr%timeint == timeint_bfrom) then
                              if (comparereal(item%sourceT1FieldPtr%timesteps, timesteps%mjd(), 1.0D-7) == 0) then
                                 ! Adjust the value in T0 field (the converter will only use the T0-field)s
                                 item%sourceT0FieldPtr%arr1d = item%sourceT1FieldPtr%arr1d
                              end if
                           end if
                           success = .true.
                           exit
                        end if
                     else
                        if (item%quantityPtr%periodic) then
                           success = ecItemFinalizeTimeseries(item)
                           success = ecItemFromTimeseries(item, timesteps%mjd())
                           return
                        end if
                        if (interpol_type == interpolate_time_extrapolation_ok) then
                           exit
                        else
                           return         ! failed to update item AND no extrapolation allowed !!
                        end if
                     end if
                  end do
               end if
            else
               if (item%quantityPtr%periodic) then
                  success = ecItemFromTimeseries(item, timesteps%mjd())
               endif
            endif
         end if
      end function ecItemUpdateSourceItem

      ! =======================================================================
      ! Set methods
      ! =======================================================================
      !> Place holder for setting several properties of an item in one go,
      !> all specified as optional arguments, which should be passed with names
      !> (RL: Now only used for the vectormax, but easily extensible)
      function ecItemSetProperty(instancePtr, itemId, vectorMax) result(success)
         logical                                 :: success     !< function status
         type(tEcInstance), pointer              :: instancePtr !< intent(in)
         integer,           intent(in)           :: itemId      !< unique Item id
         integer,           intent(in), optional :: vectorMax   !< number of dimensions, in case of multi-dimensional data
         !
         type(tEcItem), pointer :: itemPtr                      !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            if (present(vectorMax)) then
               itemPtr%quantityPtr%vectorMax = vectorMax
            endif
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetRole: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetProperty

      ! =======================================================================
      ! Copy methods
      ! =======================================================================
      !> Copies known properties from one EC item to the other.
      !!
      !! Example use is when a parent provider's item should reflect the same
      !! properties as its child providers item(s).
      function ecItemCopyProperty(instancePtr, itemId_tgt, itemId_src, proplist) result(success)
         logical                                 :: success     !< function status
         type(tEcInstance), pointer              :: instancePtr !< intent(in)
         integer,           intent(in)           :: itemId_src  !< unique Item id source
         integer,           intent(in)           :: itemId_tgt  !< unique Item id target
         character(len=*),  intent(in)           :: proplist    !< name of properties to be copied. Currently supported: "vectorMax", "quantityPtr".

         type(tEcItem), pointer :: itemPtr_src                  !< Item corresponding to itemId, source side
         type(tEcItem), pointer :: itemPtr_tgt                  !< Item corresponding to itemId, target side
         !
         success = .false.
         itemPtr_src => null()
         itemPtr_tgt => null()
         !
         itemPtr_src => ecSupportFindItem(instancePtr, itemId_src)
         itemPtr_tgt => ecSupportFindItem(instancePtr, itemId_tgt)
         if (.not.associated(itemPtr_src)) then
            call setECMessage("ERROR: ec_item::ecItemCopyProperty: Cannot find a source Item with the supplied id.")
         else if (.not.associated(itemPtr_tgt)) then
            call setECMessage("ERROR: ec_item::ecItemCopyProperty: Cannot find a target Item with the supplied id.")
         else
            if (index(proplist,'quantityPtr')>0) then
               itemPtr_tgt%quantityPtr => itemPtr_src%quantityPtr
            endif
            if (index(proplist,'vectorMax')>0) then
               itemPtr_tgt%quantityPtr%vectorMax = itemPtr_src%quantityPtr%vectorMax
            endif
            success = .true.
         end if
      end function ecItemCopyProperty

      !> Change the role of the Item corresponding to itemId.
      function ecItemSetRole(instancePtr, itemId, newRole) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: newRole     !< new role of the Item
         !
         type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            itemPtr%role = newRole
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetRole: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetRole

      ! =======================================================================

      !> Change the accessType of the Item corresponding to itemId.
      function ecItemSetType(instancePtr, itemId, newType) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: newType     !< new accessType of the Item
         !
         type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            itemPtr%accessType = newType
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetType: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetType

      ! =======================================================================

      !> Assign an existing Quantity to the Item corresponding to itemId.
      function ecItemSetQuantity(instancePtr, itemId, quantityId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         !
         type(tEcItem),     pointer :: itemPtr     !< Item corresponding to itemId
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         itemPtr => null()
         quantityPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(itemPtr) .and. associated(quantityPtr)) then
            itemPtr%quantityPtr => quantityPtr
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetQuantity: Cannot find an Item or Quantity with the supplied id.")
         end if
      end function ecItemSetQuantity

      ! =======================================================================

      !> Assign an existing ElementSet to the Item corresponding to itemId.
      function ecItemSetElementSet(instancePtr, itemId, elementSetId) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: itemId       !< unique Item id
         integer,                   intent(in) :: elementSetId !< unique ElementSet id
         !
         type(tEcItem),       pointer :: itemPtr       !< Item corresponding to itemId
         type(tEcElementSet), pointer :: elementSetPtr !< ElementSet corresponding to elementSetId
         !
         success = .false.
         itemPtr => null()
         elementSetPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         elementSetPtr => ecSupportFindElementSet(instancePtr, elementSetId)
         if (associated(itemPtr) .and. associated(elementSetPtr)) then
            itemPtr%elementSetPtr => elementSetPtr
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetElementSet: Cannot find an Item or ElementSet with the supplied id.")
         end if
      end function ecItemSetElementSet

      ! =======================================================================

      !> Assign an existing Field to the source Item for t=t0.
      function ecItemSetSourceT0Field(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_source) then
               itemPtr%sourceT0FieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the source Field of a non-source Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetSourceT0Field: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetSourceT0Field

      ! =======================================================================

      !> Assign an existing Field to the source Item for t=t1.
      function ecItemSetSourceT1Field(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_source) then
               itemPtr%sourceT1FieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the source Field of a non-source Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetSourceT1Field: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetSourceT1Field

      ! =======================================================================

      !> Assign an existing Field to the target Item.
      function ecItemSetTargetField(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_target) then
               itemPtr%targetFieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the target Field of a non-target Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetTargetField: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetTargetField

      ! =======================================================================

      !> Add a Connection an Item's array of Connections.
      function ecItemAddConnection(instancePtr, itemId, connectionId) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: itemId       !< unique Item id
         integer,                   intent(in) :: connectionId !< additional Connection for the Item
         !
         type(tEcItem),       pointer :: itemPtr       !< Item corresponding to itemId
         type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
         !
         success = .false.
         itemPtr => null()
         connectionPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
         if (associated(itemPtr) .and. associated(connectionPtr)) then
            if (itemPtr%role == itemType_target) then
               ! ensure capacity
               if (itemPtr%nConnections == size(itemPtr%connectionsPtr)) then
                  if (.not. ecArrayIncrease(itemPtr%connectionsPtr, itemPtr%nConnections)) then
                     return
                  end if
               end if
               itemPtr%nConnections = itemPtr%nConnections + 1
               itemPtr%connectionsPtr(itemPtr%nConnections)%ptr => connectionPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemAddConnection: Won't add a Connection to a non-target Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemAddConnection: Cannot find an Item or Connection with the supplied id.")
         end if
      end function ecItemAddConnection

      ! =======================================================================
      !> Append the field1%timesteps and field1%arr1d value(s) of the FIRST item of the connection to an array of stored values within the converter.
      !> This serves future reuse of these values.
      function ecItemToTimeseries(item,timestep,values) result (success)
         implicit none
         logical                            :: success    !< function status
         real(hp)                           :: timestep   !< source item t0 and t1
         real(hp), dimension(:), pointer    :: values     !< values at time t0
         type(tEcItem)                      :: item

         type(tEcConverter), pointer        :: cnvrt => null()
         integer, parameter                 :: array_increment = 100
         integer                            :: vectormax, newsize
         logical                            :: do_add

         success = .false.
         vectormax = size(values)
         if (.not.allocated(item%timeseries))  then
            allocate(item%timeseries)
         end if

         do_add = .false.
         if (item%timeseries%ntimes == 0) then
            allocate (item%timeseries%times(array_increment))
            allocate (item%timeseries%values(vectormax,array_increment))
            do_add = .true.
         else
            do_add = timestep > item%timeseries%times(item%timeseries%ntimes)
         end if

         if (do_add) then
            item%timeseries%ntimes = item%timeseries%ntimes + 1
            if (size(item%timeseries%times) < item%timeseries%ntimes) then
               newsize = size(item%timeseries%times) + array_increment
               call realloc(item%timeseries%times,newsize)
               call realloc(item%timeseries%values,vectormax,newsize)
            end if
            item%timeseries%times(item%timeseries%ntimes) = timestep
            item%timeseries%values(1:vectormax,item%timeseries%ntimes) = values(1:vectormax)
         endif
         success = .true.
      end function ecItemToTimeseries

      ! =======================================================================
      !> Stop recording an items timeseries
      function ecItemFinalizeTimeseries(item) result (success)
         implicit none
         logical                             :: success    !< function status
         real(hp)                            :: t0, t1     !< source item t0 and t1
         type(tEcItem), intent(inout)        :: item
         integer, parameter                  :: array_increment = 100
         integer                             :: vectormax, ntimes

         success = .false.
         if (.not.item%timeseries%finalized) then
            ntimes = item%timeseries%ntimes
            item%timeseries%tmin = item%timeseries%times(1)
            item%timeseries%tmax = item%timeseries%times(ntimes)
            vectormax = size(item%timeseries%values,dim=1)
            call realloc(item%timeseries%times,ntimes)
            call realloc(item%timeseries%values,vectormax,ntimes)
            item%timeseries%finalized = .true.
         end if
         success = .true.
      end function ecItemFinalizeTimeseries

      ! =======================================================================
      !> Update the first source item in a periodical sence with values from a stored timeseries
      function ecItemFromTimeseries(item,timesteps) result (success)
         implicit none
         logical                            :: success    !< function status
         real(hp), intent(in)               :: timesteps  !< convert to this number of timesteps past the kernel's reference date
         real(hp)                           :: tmin, tmax        !< source item t0 and t1
         real(hp), dimension(:), pointer    :: valuesT0      !< values at time t0
         real(hp), dimension(:), pointer    :: valuesT1      !< values at time t1
         type(tEcItem), intent(inout)       :: item
         integer, parameter                 :: array_increment = 100
         integer                            :: vectormax, newsize
         real(hp)                           :: tmod
         integer                            :: it
         real(hp)                           :: periodic_shift

         success = .false.

         tmin = item%timeseries%tmin
         tmax = item%timeseries%tmax
         tmod = mod(mod(timesteps-tmin,tmax-tmin)+(tmax-tmin),tmax-tmin)    ! time between (t0-t1) and (t1-t0)
         if (tmod<0.d0) then
            tmod = mod(tmod+(tmax-tmin),tmax-tmin)                          ! time between 0 and t1-t0
         end if
         do it = 1,item%timeseries%ntimes - 1
            if (item%timeseries%times(it)-tmin>tmod) exit
         end do                                                             ! index of the first time in the series exceeding requested time

         periodic_shift = (tmax - tmin) * floor((timesteps - tmin) / (tmax - tmin))

         ! Restore times and values in the T0 and T1 fields of the first item
         item%sourceT0FieldPtr%timesteps = item%timeseries%times(it-1) + periodic_shift
         item%sourceT1FieldPtr%timesteps = item%timeseries%times(it) + periodic_shift
         valuesT0 => item%sourceT0FieldPtr%arr1dPtr
         valuesT1 => item%sourceT1FieldPtr%arr1dPtr
         vectormax = size(item%timeseries%values,dim=1)
         valuesT0(1:vectormax) = item%timeseries%values(1:vectormax,it-1)
         valuesT1(1:vectormax) = item%timeseries%values(1:vectormax,it)
         success = .true.
      end function ecItemFromTimeseries

      ! =======================================================================
      !> Update the first source item in a periodical sence with values from a stored timeseries
      function ecItemInitialize(instancePtr, itemId, itemRole, quantityId, elementSetId, fieldId) result(success)
         logical                          :: success      !< function status
         type(tEcInstance), pointer       :: instancePtr  !<
         integer,           intent(in)    :: itemId       !< Unique Item id.
         integer,           intent(in)    :: itemRole     !< Item role.
         integer,           intent(in)    :: quantityId   !< Unique Quantity id.
         integer,           intent(in)    :: elementSetId !< Unique ElementSet id.
         integer,           intent(in)    :: fieldId      !< Unique Field id.
         !
         success = .true.
         if (itemId /= ec_undef_int) then                ! if Target Item already exists, do NOT create a new one ...
            success              = ecItemSetRole(instancePtr, itemId, itemType_target)
            if (success) success = ecItemSetQuantity(instancePtr, itemId, quantityId)
            if (success) success = ecItemSetElementSet(instancePtr, itemId, elementSetId)
            if (success) success = ecItemSetTargetField(instancePtr, itemId, fieldId)
        endif
     end function ecItemInitialize

end module m_ec_item
