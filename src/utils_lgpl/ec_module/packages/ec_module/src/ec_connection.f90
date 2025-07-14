!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

!> This module contains all the methods for the datatype tEcConnection.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_connection
   use m_ec_typedefs
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   use m_ec_converter

   implicit none

   private

   public :: ecConnectionCreate
   public :: ecConnectionFree1dArray
   public :: ecConnectionSetConverter
   public :: ecConnectionAddSourceItem
   public :: ecConnectionAddTargetItem
   public :: ecConnectionSetIndexWeights

contains

   ! =======================================================================

   !> Construct a new Connection with the specified id.
      !! Failure is indicated by returning a null pointer.
   function ecConnectionCreate(connectionId) result(connectionPtr)
      type(tEcConnection), pointer :: connectionPtr !< the new Connection, intent(out)
      integer, intent(in) :: connectionId !< unique Connection id
      !
      integer :: istat !< allocate() status
      !
      ! allocation
      allocate (connectionPtr, stat=istat)
      if (istat == 0) then
         allocate (connectionPtr%sourceItemsPtr(1), stat=istat)
         if (istat == 0) then
            allocate (connectionPtr%targetItemsPtr(1), stat=istat)
         end if
      end if
      if (istat /= 0) then
         call setECMessage("ERROR: ec_converter::ecConnectionCreate: Unable to allocate additional memory.")
         connectionPtr => null()
         return
      end if
      ! initialization
      connectionPtr%id = connectionId
      connectionPtr%nSourceItems = 0
      connectionPtr%nTargetItems = 0
   end function ecConnectionCreate

   ! =======================================================================

   !> Free a tEcConnection, after which it can be deallocated.
   function ecConnectionFree(connection) result(success)
      logical :: success !< function status
      type(tEcConnection), intent(inout) :: connection !< Connection to free
      !
      integer :: i !< loop counter
      integer :: istat !< deallocate() status
      !
      success = .true.
      !
      ! A connection does not own the tEcConverters, the tEcInstance does, so nullify rather then ecConverterFree().
      connection%converterPtr => null()
      ! A connection does not own the tEcItems, the tEcInstance does, so nullify rather then ecItemFree().
      do i = 1, connection%nSourceItems
         connection%sourceItemsPtr(i)%ptr => null()
      end do
      deallocate (connection%sourceItemsPtr, stat=istat)
      if (istat /= 0) success = .false.
      do i = 1, connection%nTargetItems
         connection%targetItemsPtr(i)%ptr => null()
      end do
      deallocate (connection%targetItemsPtr, stat=istat)
      if (istat /= 0) success = .false.
   end function ecConnectionFree

   ! =======================================================================

   !> Frees a 1D array of tEcConnectionPtrs, after which the connectionPtr is deallocated.
   function ecConnectionFree1dArray(connectionPtr, nConnections) result(success)
      logical :: success !< function status
      type(tEcConnectionPtr), dimension(:), pointer :: connectionPtr !< intent(inout)
      integer, intent(inout) :: nConnections !< number of Connections
      !
      integer :: i !< loop counter
      integer :: istat !< deallocate() status
      !
      success = .true.
      !
      if (.not. associated(connectionPtr)) then
         call setECMessage("WARNING: ec_connection::ecConnectionFree1dArray: Dummy argument connectionPtr is already disassociated.")
      else
         ! Free and deallocate all tEcConnectionPtrs in the 1d array.
         do i = 1, nConnections
            if (ecConnectionFree(connectionPtr(i)%ptr)) then
               deallocate (connectionPtr(i)%ptr, stat=istat)
               if (istat /= 0) success = .false.
            else
               success = .false.
            end if
         end do
         ! Finally deallocate the tEcConnectionPtr(:) pointer.
         if (success) then
            deallocate (connectionPtr, stat=istat)
            if (istat /= 0) success = .false.
         end if
      end if
      nConnections = 0
   end function ecConnectionFree1dArray

   ! =======================================================================

   !> Change the Converter of the Connection corresponding to connectionId.
   function ecConnectionSetConverter(instancePtr, connectionId, converterId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: connectionId !< unique Connection id
      integer, intent(in) :: converterId !< unique Converter id
      !
      type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
      type(tEcConverter), pointer :: converterPtr !< Converter corresponding to converterId
      !
      success = .false.
      connectionPtr => null()
      converterPtr => null()
      !
      connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
      converterPtr => ecSupportFindConverter(instancePtr, converterId)
      if (associated(connectionPtr) .and. associated(converterPtr)) then
         connectionPtr%converterPtr => converterPtr
         success = .true.
      else
         call setECMessage("ERROR: ec_connection::ecConnectionSetConverter: Cannot find a Connection or Converter with the supplied id.")
      end if
   end function ecConnectionSetConverter

   ! =======================================================================

   !> Add a source Item to a Connection's array of source Items.
   function ecConnectionAddSourceItem(instancePtr, connectionId, itemId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: connectionId !< unique Connection id
      integer, intent(in) :: itemId !< unique Item id
      !
      type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
      type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
      !
      success = .false.
      connectionPtr => null()
      itemPtr => null()
      !
      connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
      itemPtr => ecSupportFindItem(instancePtr, itemId)
      if (associated(connectionPtr) .and. associated(itemPtr)) then
         ! ensure capacity
         if (connectionPtr%nSourceItems == size(connectionPtr%sourceItemsPtr)) then
            if (.not. ecArrayIncrease(connectionPtr%sourceItemsPtr, connectionPtr%nSourceItems)) then
               return
            end if
         end if
         connectionPtr%nSourceItems = connectionPtr%nSourceItems + 1
         connectionPtr%sourceItemsPtr(connectionPtr%nSourceItems)%ptr => itemPtr
         success = .true.
      else
         call setECMessage("ERROR: ec_connection::ecConnectionAddSourceItem: Cannot find a Connection or Item with the supplied id.")
      end if
   end function ecConnectionAddSourceItem

   ! =======================================================================

   !> Add a target Item to a Connection's array of target Items.
   function ecConnectionAddTargetItem(instancePtr, connectionId, itemId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: connectionId !< unique Connection id
      integer, intent(in) :: itemId !< unique Item id
      !
      type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
      type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
      !
      success = .false.
      connectionPtr => null()
      itemPtr => null()
      !
      connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
      itemPtr => ecSupportFindItem(instancePtr, itemId)
      if (associated(connectionPtr) .and. associated(itemPtr)) then
         ! ensure capacity
         if (connectionPtr%nTargetItems == size(connectionPtr%targetItemsPtr)) then
            if (.not. ecArrayIncrease(connectionPtr%targetItemsPtr, connectionPtr%nTargetItems)) then
               return
            end if
         end if
         connectionPtr%nTargetItems = connectionPtr%nTargetItems + 1
         connectionPtr%targetItemsPtr(connectionPtr%nTargetItems)%ptr => itemPtr
         success = .true.
      else
         call setECMessage("ERROR: ec_connection::ecConnectionAddTargetItem: Cannot find a Connection or Item with the supplied id.")
      end if
   end function ecConnectionAddTargetItem

   function ecConnectionSetIndexWeights(instancePtr, connectionId) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: connectionId !< unique Connection id
      !
      type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
      !
      success = .false.
      connectionPtr => null()
      !
      connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
      if (associated(connectionPtr)) then
         if (associated(connectionPtr%sourceItemsPtr(1)%ptr%elementSetPtr)) then
            success = ecConverterUpdateWeightFactors(instancePtr, connectionPtr)
         else
            success = .true.
         end if
      end if
   end function ecConnectionSetIndexWeights

end module m_ec_connection
