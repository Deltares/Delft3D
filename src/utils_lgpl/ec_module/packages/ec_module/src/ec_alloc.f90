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

!> This module contains the allocation methods for the EC-module's pointer arrays.
!! @author adri.mourits@deltares.nl
!! @author robert.leander@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_alloc
   use m_ec_typedefs
   use m_ec_message

   implicit none

   private

   public :: ecArrayIncrease

   interface ecArrayIncrease
      module procedure ecConnectionPtrArrayIncrease
      module procedure ecConverterPtrArrayIncrease
      module procedure ecElementSetPtrArrayIncrease
      module procedure ecFieldPtrArrayIncrease
      module procedure ecFileReaderPtrArrayIncrease
      module procedure ecBCBlockPtrArrayIncrease
      module procedure ecNetCDFPtrArrayIncrease
      module procedure ecItemPtrArrayIncrease
      module procedure ecQuantityPtrArrayIncrease
      module procedure ecBCFilePtrArrayIncrease
   end interface ecArrayIncrease

   contains

      !> Increases the size of an array of tEcConnectionPtr instances by 10.
      function ecConnectionPtrArrayIncrease(ptr, nConnections) result(success)
         logical                                       :: success      !< function status
         type(tEcConnectionPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nConnections !< Number of tEcConnectionPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcConnectionPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecConnectionPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConnections
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecConnectionPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConnectionPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcConverterPtr instances by 10.
      function ecConverterPtrArrayIncrease(ptr, nConverters) result(success)
         logical                                      :: success      !< function status
         type(tEcConverterPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                      :: nConverters  !< Number of tEcConverterPtrs =< size(ptr)
         !
         integer                                      :: istat   !< allocate() status
         type(tEcConverterPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                      :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecConverterPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConverters
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecConverterPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConverterPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcElementSetPtr instances by 10.
      function ecElementSetPtrArrayIncrease(ptr, nElementSets) result(success)
         logical                                       :: success      !< function status
         type(tEcElementSetPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nElementSets !< Number of tEcElementSetPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcElementSetPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecElementSetPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nElementSets
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecElementSetPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecElementSetPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcFieldPtr instances by 10.
      function ecFieldPtrArrayIncrease(ptr, nFields) result(success)
         logical                                  :: success !< function status
         type(tEcFieldPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                  :: nFields !< Number of tEcFieldPtrs =< size(ptr)
         !
         integer                                  :: istat   !< allocate() status
         type(tEcFieldPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                  :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecFieldPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFields
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecFieldPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFieldPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcFileReaderPtr instances by 10.
      function ecFileReaderPtrArrayIncrease(ptr, nFileReaders) result(success)
         logical                                       :: success      !< function status
         type(tEcFileReaderPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nFileReaders !< Number of tEcFileReaderPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcFileReaderPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecFileReaderPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFileReaders
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecFileReaderPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFileReaderPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcBCBlockPtr instances by 10.
      function ecBCBlockPtrArrayIncrease(ptr, nBCBlocks) result(success)
         logical                                       :: success      !< function status
         type(tEcBCBlockPtr), dimension(:), pointer    :: ptr          !< intent(inout)
         integer                                       :: nBCBlocks    !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcBCBlockPtr), dimension(:), pointer    :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecBCBlockPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nBCBlocks
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecBCBlockPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecBCBlockPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcNetCDFPtr instances by 10.
      function ecNetCDFPtrArrayIncrease(ptr, nNetCDFs) result(success)
         logical                                       :: success      !< function status
         type(tEcNetCDFPtr), dimension(:), pointer     :: ptr          !< intent(inout)
         integer                                       :: nNetCDFs     !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcNetCDFPtr), dimension(:), pointer     :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecNetCDFPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nNetCDFs
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecNetCDFPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecNetCDFPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcItemPtr instances by 10.
      function ecItemPtrArrayIncrease(ptr, nItems) result(success)
         logical                                 :: success !< function status
         type(tEcItemPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                 :: nItems  !< Number of tEcItemPtrs =< size(ptr)
         !
         integer                                 :: istat   !< allocate() status
         type(tEcItemPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                 :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecItemPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nItems
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecItemPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecItemPtrArrayIncrease

      ! =======================================================================

      !> Increases the size of an array of tEcQuantityPtr instances by 10.
      function ecQuantityPtrArrayIncrease(ptr, nQuantitys) result(success)
         logical                                     :: success    !< function status
         type(tEcQuantityPtr), dimension(:), pointer :: ptr        !< intent(inout)
         integer                                     :: nQuantitys !< Number of tEcQuantityPtrs =< size(ptr)
         !
         integer                                     :: istat   !< allocate() status
         type(tEcQuantityPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                     :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecQuantityPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nQuantitys
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecQuantityPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecQuantityPtrArrayIncrease

      !> Increases the size of an array of tEcBCFilePtr instances by 10.
      function ecBCFilePtrArrayIncrease(ptr, nBCFiles) result(success)
         logical                                      :: success      !< function status
         type(tEcBCFilePtr), dimension(:), pointer    :: ptr          !< intent(inout)
         integer                                      :: nBCFiles     !< Number of tEcBCFilePtrs =< size(ptr)
         !
         integer                                      :: istat   !< allocate() status
         type(tEcBCFilePtr), dimension(:), pointer    :: new_ptr !< new array
         integer                                      :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ec_alloc::ecBCFilePtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nBCFiles
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ec_alloc::ecBCFilePtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecBCFilePtrArrayIncrease

end module m_ec_alloc
