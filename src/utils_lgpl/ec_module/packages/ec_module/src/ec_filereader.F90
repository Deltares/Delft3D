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

!> This module contains all the methods for the datatype tEcFileReader.
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_filereader
   use m_ec_typedefs
   use m_ec_message
   use m_ec_parameters
   use m_ec_alloc
   use m_ec_support
   use m_ec_astro
   use string_module

   implicit none

   private

   public :: ecFileReaderCreate
   public :: ecFileReaderFree1dArray
   public :: ecFileReaderReadNextRecord
   public :: ecFileReaderFindItem
   public :: ecFileReaderAddItem
   public :: ecFileReaderGetNumberOfItems
   public :: ecFileReaderGetItem
   public :: ecFileReaderProvidesQHtable

   contains

      ! =======================================================================

      !> Construct a new FileReader with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecFileReaderCreate(fileReaderId) result(fileReaderPtr)
         type(tEcFileReader), pointer            :: fileReaderPtr !< the new FileReader, intent(out)
         integer,                     intent(in) :: fileReaderId  !< unique FileReader id
         !
         integer :: istat   !< allocate() status
         logical :: success !< helper variable
         !
         success = .false.
         !
         ! allocation
         allocate(fileReaderPtr, stat = istat)
         if (istat == 0) then
            allocate(fileReaderPtr%items(3), stat = istat)
            if (istat == 0) then
               allocate(fileReaderPtr%tframe, stat = istat)
            end if
         end if
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader::ecFileReaderCreate: Unable to allocate additional memory.")
            fileReaderPtr => null()
            return
         end if
         ! initialization
         fileReaderPtr%id = fileReaderId
         fileReaderPtr%ofType = provFile_undefined
         fileReaderPtr%fileName = ' '
         fileReaderPtr%fileHandle = ec_undef_int
         fileReaderPtr%nItems = 0
         fileReaderPtr%tframe%k_refdate = ec_undef_hp
         fileReaderPtr%tframe%k_timestep_unit = ec_undef_int
         fileReaderPtr%tframe%ec_refdate = ec_undef_hp
         fileReaderPtr%tframe%ec_timestep_unit = ec_undef_int
         fileReaderPtr%tframe%nr_timesteps = ec_undef_int
         fileReaderPtr%lastReadTime = ec_undef_hp
         fileReaderPtr%end_of_data = .false.
      end function ecFileReaderCreate

      ! =======================================================================


      !> Free a tEcFileReader, after which it can be deallocated.
      function ecFileReaderFree(fileReader) result(success)
      use m_ec_bcreader, only : ecBCBlockFree
      implicit none
         logical                            :: success !< function status
         type(tEcFileReader), intent(inout) :: fileReader !< intent(inout)
         !
         integer :: istat  !< deallocate() status
         integer :: i      !< loop counter
         logical :: opened
         !
         success = .true.
         !
         if (index(fileReader%fileName, '.nc') > 0) then
            continue
         else
            inquire(fileReader%fileHandle,opened=opened)
            if (opened) then
               close(fileReader%fileHandle, iostat = istat)
            endif
         end if
         ! A fileReader does not own the tEcItems, the tEcInstance does, so nullify rather then ecItemFree().
         do i=1, fileReader%nItems
            fileReader%items(i)%ptr => null()
         end do
         deallocate(fileReader%items, stat = istat)
         if (istat /= 0) success = .false.
         ! The FileReader owns the tframe, so free and deallocate.
         if (allocated(fileReader%tframe%times)) deallocate(fileReader%tframe%times, stat = istat)
         deallocate(fileReader%tframe, stat = istat)
         if (istat /= 0) success = .false.
         deallocate(fileReader%hframe, stat = istat)
         if (istat /= 0) success = .false.

         if (allocated(fileReader%variable_names)) then
            deallocate(fileReader%variable_names)
         endif
         if (allocated(fileReader%standard_names)) then
            deallocate(fileReader%standard_names)
         endif
      end function ecFileReaderFree

      ! =======================================================================

      !> Frees a 1D array of tEcFileReaderPtrs, after which the ptr is deallocated.
      function ecFileReaderFree1dArray(ptr, nFileReaders) result (success)
         logical                                       :: success      !< function status
         type(tEcFileReaderPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer, intent(inout)                        :: nFileReaders !< number of FileReaders
         !
         integer :: i      !< loop counter
         integer :: istat  !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(ptr)) then
            call setECMessage("WARNING: ec_filereader::ecFileReaderFree1dArray: Dummy argument ptr is already disassociated.")
         else
            ! Free and deallocate all tEcFileReaderPtrs in the 1d array.
            do i=1, nFileReaders
               if (ecFileReaderFree(ptr(i)%ptr)) then
                  deallocate(ptr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcFileReaderPtr(:) pointer.
            if (success) then
               deallocate(ptr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
         nFileReaders = 0
      end function ecFileReaderFree1dArray

      ! =======================================================================

      !> Update all Items which belong to this FileReader.
      function ecFileReaderReadNextRecord(fileReaderPtr, timesteps) result(success)
         use m_ec_filereader_read
         use m_ec_support
         use string_module, only: str_lower
         !
         ! return value
         logical                      :: success    !< function status
         !
         ! parameters
         type(tEcFileReader), pointer :: fileReaderPtr !< FileReader which must update
         real(hp), intent(in)         :: timesteps     !< Time steps for nodal factors.
         !
         ! locals
         type(tEcField), pointer :: fieldPtrA     !< helper for pointer flipping
         type(tEcField), pointer :: fieldPtrB     !< helper for pointer flipping
         real(hp)                :: time_steps    !< time steps of read data block
         integer                 :: i             !< loop counter
         integer                 :: t0t1          !< indicates whether the 0 or the 1 field is read. -1: choose yourself
         integer                 :: numlay        !< number of layers in 3D = number of columns in a t3D file
         integer                 :: istat, yyyymmdd, hhmmss
         integer                 :: vectormax
         character(len=255)      :: qname
         integer                 :: nv, nl, iitem
         integer                 :: from, thru
         integer                 :: time_ndx
         real(hp), dimension(:), allocatable    :: values
         type(tEcItem), pointer  :: itemPtr
         integer                 :: n_invalid_components
         integer                 :: timesndx

         ! body
         success = .false.
         istat = 0
         fieldPtrA => null()
         fieldPtrB => null()
         i = 0
         !
         select case(fileReaderPtr%ofType)
            case (provFile_undefined)
               call setECMessage("ERROR: ec_filereader::ecFileReaderReadNextRecord: Unknown file type.")
            case (provFile_uniform, provFile_unimagdir)
               ! read the next record into t0
               success = ecUniReadBlock(fileReaderPtr, fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%timesteps, &
                                                       fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1dPtr)
               ! flip t0 and t1
               fieldPtrA => fileReaderPtr%items(1)%ptr%sourceT1FieldPtr
               fileReaderPtr%items(1)%ptr%sourceT1FieldPtr => fileReaderPtr%items(1)%ptr%sourceT0FieldPtr
               fileReaderPtr%items(1)%ptr%sourceT0FieldPtr => fieldPtrA
            case (provFile_bc)
               select case(fileReaderPtr%bc%func)
               case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                  if (fileReaderPtr%nItems==1) then         ! RL: preserving original code
                     success = ecBCReadBlock(fileReaderPtr, fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%timesteps, &
                                                            fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1dPtr)
                     if (success) then
                        fieldPtrA => fileReaderPtr%items(1)%ptr%sourceT1FieldPtr
                        fileReaderPtr%items(1)%ptr%sourceT1FieldPtr => fileReaderPtr%items(1)%ptr%sourceT0FieldPtr
                        fileReaderPtr%items(1)%ptr%sourceT0FieldPtr => fieldPtrA
                     end if
                  end if
                  if (fileReaderPtr%nItems>1) then          ! RL: in the case of multiple items for this filereader
                     allocate(values(fileReaderPtr%bc%numcols))
                     success = ecBCReadBlock(fileReaderPtr, time_steps, values)
                     from = 1
                     do iitem = 1, fileReaderPtr%nItems
                        itemPtr => fileReaderPtr%items(iitem)%ptr
                        nl = 1
                        if (associated(itemPtr%elementSetPtr%z)) nl = size(itemPtr%elementSetPtr%z)
                        thru = from + nv*nl - 1
                        itemPtr%sourceT0FieldPtr%arr1dPtr(1:nv*nl) = values(from:thru)
                        itemPtr%sourceT0FieldPtr%timesteps = time_steps
                        from = thru + 1
                        fieldPtrA => itemPtr%sourceT1FieldPtr
                        itemPtr%sourceT1FieldPtr => itemPtr%sourceT0FieldPtr
                        itemPtr%sourceT1FieldPtr => itemPtr%sourceT0FieldPtr
                        itemPtr%sourceT0FieldPtr => fieldPtrA
                     end do
                     if (allocated(values)) then
                        deallocate(values)
                     end if
                  end if
               case (BC_FUNC_HARMONIC)
                  ! Fourier values are calculated, so T1 is set arbitrarily, increasably high.
                  do i=1, fileReaderPtr%nItems
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps + 10000.0_hp
                  end do
                  success = .true.
               case (BC_FUNC_ASTRO)
                  success = ecTimeFrameRealHpTimestepsToDateTime(timesteps, yyyymmdd, hhmmss)
                  n_invalid_components = (ecFileReaderLookupAstroComponents(fileReaderPtr))
                  do i = 1, size(fileReaderPtr%items(1)%ptr%sourceT1FieldPtr%arr1d)
                     fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(1)%ptr%sourceT1FieldPtr%arr1d(i)
                     fileReaderPtr%items(2)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(2)%ptr%sourceT1FieldPtr%arr1d(i)
                     fileReaderPtr%items(3)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(3)%ptr%sourceT1FieldPtr%arr1d(i)

                     call asc( fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1d(i), &
                               fileReaderPtr%items(2)%ptr%sourceT0FieldPtr%arr1d(i), &
                               fileReaderPtr%items(3)%ptr%sourceT0FieldPtr%arr1d(i), &
                               fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber(i),   &
                               yyyymmdd, hhmmss, istat)
                     if (istat /= 0) success = .false.
                  end do
                  ! Shift time interval with dtnodal
                  do i=1, fileReaderPtr%nItems
                     fileReaderPtr%items(i)%ptr%sourceT0FieldPtr%timesteps = timesteps
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = timesteps + fileReaderPtr%tframe%dtnodal
                  end do
               case (BC_FUNC_QHTABLE)
                  do i=1, fileReaderPtr%nItems
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps + 10000.0_hp
                  end do
                  success = .true.
               case default
                  call setECMessage("ERROR: ec_filereader::ecFileReaderReadNextRecord: Unsupported BC function type.")
               end select
            case (provFile_t3D)
                numlay = size(fileReaderPtr%items(1)%ptr%elementsetptr%z)
                vectormax = fileReaderPtr%items(1)%ptr%quantityptr%vectormax
                success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, numlay*vectormax, 1, fileReaderPtr%items(1)%ptr)
                fieldPtrA => fileReaderPtr%items(1)%ptr%sourceT1FieldPtr
                fileReaderPtr%items(1)%ptr%sourceT1FieldPtr => fileReaderPtr%items(1)%ptr%sourceT0FieldPtr
                fileReaderPtr%items(1)%ptr%sourceT0FieldPtr => fieldPtrA
            case (provFile_arcinfo)
               success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols, fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows, fileReaderPtr%items(1)%ptr)
               ! flip t0 and t1
               fieldPtrA => fileReaderPtr%items(1)%ptr%sourceT1FieldPtr
               fileReaderPtr%items(1)%ptr%sourceT1FieldPtr => fileReaderPtr%items(1)%ptr%sourceT0FieldPtr
               fileReaderPtr%items(1)%ptr%sourceT0FieldPtr => fieldPtrA
            case (provFile_spiderweb)
               ! read the next record into t0
               success = ecSpiderwebReadBlock(fileReaderPtr, fileReaderPtr%items(1)%ptr, fileReaderPtr%items(2)%ptr, fileReaderPtr%items(3)%ptr, 0, fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols, fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows)
               ! flip t0 and t1
               do i=1, fileReaderPtr%nItems
                  fieldPtrA => fileReaderPtr%items(i)%ptr%sourceT1FieldPtr
                  fileReaderPtr%items(i)%ptr%sourceT1FieldPtr => fileReaderPtr%items(i)%ptr%sourceT0FieldPtr
                  fileReaderPtr%items(i)%ptr%sourceT0FieldPtr => fieldPtrA
               end do
            case (provFile_curvi)
               ! Read a new value into t0 and then flip t0 and t1.
               success = ecCurviReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0)
               if (success) then
                  do i=1, fileReaderPtr%nItems
                     fieldPtrA => fileReaderPtr%items(i)%ptr%sourceT1FieldPtr
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr => fileReaderPtr%items(i)%ptr%sourceT0FieldPtr
                     fileReaderPtr%items(i)%ptr%sourceT0FieldPtr => fieldPtrA
                  end do
               end if
            case (provFile_qhtable)
               ! Qhtable values are calculated, so T1 is set arbitrary, increasably high.
               do i=1, fileReaderPtr%nItems
                  fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps + 10000.0_hp
               end do
               success = .true.
            case (provFile_fourier)
               success = .true.
               if (allocated(fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_components)) then ! Astronomical case
                  success = ecTimeFrameRealHpTimestepsToDateTime(timesteps, yyyymmdd, hhmmss)
                  n_invalid_components = (ecFileReaderLookupAstroComponents(fileReaderPtr))
                  do i = 1, size(fileReaderPtr%items(1)%ptr%sourceT1FieldPtr%arr1d)
                     fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(1)%ptr%sourceT1FieldPtr%arr1d(i)
                     fileReaderPtr%items(2)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(2)%ptr%sourceT1FieldPtr%arr1d(i)
                     fileReaderPtr%items(3)%ptr%sourceT0FieldPtr%arr1d(i) = fileReaderPtr%items(3)%ptr%sourceT1FieldPtr%arr1d(i)

                     call asc( fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%arr1d(i),            &
                               fileReaderPtr%items(2)%ptr%sourceT0FieldPtr%arr1d(i),            &
                               fileReaderPtr%items(3)%ptr%sourceT0FieldPtr%arr1d(i),            &
                               fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber(i),   &
                               yyyymmdd, hhmmss, istat)
                     if (istat /= 0) success = .false.
                  end do
                  ! Shift time interval with dtnodal
                  do i=1, fileReaderPtr%nItems
                     fileReaderPtr%items(i)%ptr%sourceT0FieldPtr%timesteps = timesteps
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = timesteps + fileReaderPtr%tframe%dtnodal
                  end do
               else
                  do i=1, fileReaderPtr%nItems
                     fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps = fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps + 10000.0_hp
                  end do
               end if
            case (provFile_netcdf)
               qname = fileReaderPtr%items(1)%ptr%quantityPtr%name
               call str_lower(qname)
               itemPtr => fileReaderPtr%items(1)%ptr
               if (associated(itemPtr%hframe)) then
                  ! This is a harmonics file, read all the variable values because we actually don't have time steps.
                  success = ecNetcdfReadVariable(fileReaderPtr, itemPtr)
               else
                  if (itemPtr%sourceT0FieldPtr%timesndx < 0) then
                     t0t1 = 0
                     timesndx  = ecNetcdfGetTimeIndexByTime(fileReaderPtr, timesteps)           ! timesteps is MJD in the new EC-module ? CHECK!
                  else if (itemPtr%sourceT0FieldPtr%timesndx > itemPtr%sourceT1FieldPtr%timesndx) then
                     t0t1 = 1
                     timesndx = itemPtr%sourceT0FieldPtr%timesndx + 1
                  else
                     t0t1 = 0
                     timesndx = itemPtr%sourceT1FieldPtr%timesndx + 1
                  end if
                  if(fileReaderPtr%one_time_field) then
                     t0t1 = -1
                     do i=1, fileReaderPtr%nItems
                        success = ecNetcdfReadBlock(fileReaderPtr, fileReaderPtr%items(i)%ptr, t0t1, fileReaderPtr%items(i)%ptr%elementSetPtr%nCoordinates)
                        if (t0t1 == 0) then
                           ! flip t0 and t1
                           fieldPtrA => fileReaderPtr%items(i)%ptr%sourceT1FieldPtr
                           fileReaderPtr%items(i)%ptr%sourceT1FieldPtr => fileReaderPtr%items(i)%ptr%sourceT0FieldPtr
                           fileReaderPtr%items(i)%ptr%sourceT0FieldPtr => fieldPtrA
                        end if
                     end do
                  else
                     do i=1, fileReaderPtr%nItems
                        success = ecNetcdfReadNextBlock(fileReaderPtr, fileReaderPtr%items(i)%ptr, t0t1, timesndx)
                        if (.not.success) then
                           return
                        end if
                     end do
                     if (itemPtr%sourceT1FieldPtr%timesndx < 0) then
                        t0t1 = 1
                        timesndx = timesndx + 1
                        do i=1, fileReaderPtr%nItems
                           success = ecNetcdfReadNextBlock(fileReaderPtr, fileReaderPtr%items(i)%ptr, t0t1, timesndx)
                        end do
                     end if
                     if (t0t1 == 0) then
                        ! flip t0 and t1
                        do i=1, fileReaderPtr%nItems
                           ! flip t0 and t1
                           fieldPtrA => fileReaderPtr%items(i)%ptr%sourceT1FieldPtr
                           fileReaderPtr%items(i)%ptr%sourceT1FieldPtr => fileReaderPtr%items(i)%ptr%sourceT0FieldPtr
                           fileReaderPtr%items(i)%ptr%sourceT0FieldPtr => fieldPtrA
                        end do
                     end if
                  end if
               end if
            case (provFile_svwp, provFile_svwp_weight, provFile_curvi_weight, provFile_samples, &
                  provFile_triangulationmagdir, provFile_poly_tim, provFile_grib)
               ! NOTE for provFile_samples: don't support readNextRecord, because sample data is read once by ecSampleReadAll upon init.
               call setECMessage("ERROR: ec_filereader::ecFileReaderReadNextRecord: Unsupported file type.")
            case default
               call setECMessage("ERROR: ec_filereader::ecFileReaderReadNextRecord: Unknown file type.")
               do i=1, fileReaderPtr%nItems
                  if (fileReaderPtr%items(i)%ptr%sourceT1FieldPtr%timesteps<fileReaderPtr%items(i)%ptr%sourceT0FieldPtr%timesteps) then
                     call setECMessage('Non-progressive time variable detected in file: '//trim(fileReaderPtr%fileName))
                     return
                  end if
               end do
         end select
      end function ecFileReaderReadNextRecord

      ! =======================================================================

      !> Find a source Item with itemId in FileReader with fileReaderId.
      !! Failure is indicated by returning ec_undef_int.
      function ecFileReaderFindItem(instancePtr, fileReaderId, name) result(itemId)
         integer                               :: itemId       !< unique Item id
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: fileReaderId !< unique FileReader id
         character(len=*),          intent(in) :: name         !< Quantity name which identifies the requested Item
         !
         type(tEcFileReader), pointer :: fileReaderPtr !< FileReader corresponding to fileReaderId
         integer                      :: i             !< loop counter
         !
         itemId = ec_undef_int
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         if (associated(fileReaderPtr)) then
            do i=1, fileReaderPtr%nItems
               if (strcmpi(fileReaderPtr%items(i)%ptr%quantityPtr%name, name)) then
                  itemId = fileReaderPtr%items(i)%ptr%id
                  exit
               end if
            end do
         end if

         if (itemId == ec_undef_int) then
            call setECMessage("ERROR: ec_filereader::ecFileReaderFindItem: Cannot find a FileReader with the supplied name: "//trim(name))
         end if
      end function ecFileReaderFindItem

      ! =======================================================================

      !> Find the x-th source Item in FileReader with fileReaderId.
      !! Failure is indicated by returning ec_undef_int.
      function ecFileReaderGetItem(instancePtr, fileReaderId, x) result(itemId)
         integer                               :: itemId       !< unique Item id
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: fileReaderId !< unique FileReader id
         integer,                   intent(in) :: x            !< array-index of Item to return
         !
         type(tEcFileReader), pointer :: fileReaderPtr !< FileReader corresponding to fileReaderId
         !
         itemId = ec_undef_int
         fileReaderPtr => null()
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         if (associated(fileReaderPtr)) then
            if (x > fileReaderPtr%nItems) then
               call setECMessage("ERROR: ec_filereader::ecFileReaderGetItem: Index out of bounds.")
            else
               itemId = fileReaderPtr%items(x)%ptr%id
            end if
         else
            call setECMessage("ERROR: ec_filereader::ecFileReaderGetItem: Cannot find a FileReader with the supplied id.")
         end if
      end function ecFileReaderGetItem

      ! =======================================================================

      !> Return the number of source Items in FileReader with fileReaderId.
      !! Failure is indicated by returning ec_undef_int.
      function ecFileReaderGetNumberOfItems(instancePtr, fileReaderId) result(nr)
         integer                               :: nr           !< number of source Items
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: fileReaderId !< unique FileReader id
         !
         type(tEcFileReader), pointer :: fileReaderPtr !< FileReader corresponding to fileReaderId
         !
         nr = 0
         fileReaderPtr => null()
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         if (associated(fileReaderPtr)) then
            nr = fileReaderPtr%nItems
         else
            call setECMessage("ERROR: ec_filereader::ecFileReaderGetNumberOfItems: Cannot find a FileReader with the supplied id.")
         end if
      end function ecFileReaderGetNumberOfItems

      ! =======================================================================

      !> Return is_QH=true and in that case also pointers to the arrays Q and H if this provider provides a QH-table
      !> Otherwise is_QH is false and the pointers are null
      function ecFileReaderProvidesQHtable(instancePtr, fileReaderId, item_H, item_Q) result(is_QH)
      implicit none
         logical                               :: is_QH        !< true if QH table
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,           intent(in)         :: fileReaderId !< unique FileReader id
         integer,           intent(out)        :: item_Q        !< ID of item holding the discharges
         integer,           intent(out)        :: item_H        !< ID of item holding the waterlevels
         !
         type(tEcFileReader), pointer :: fileReaderPtr  !< FileReader corresponding to fileReaderId
         !
         item_Q = ec_undef_int
         item_H = ec_undef_int
         is_QH = .false.

         fileReaderPtr => null()
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         if (associated(fileReaderPtr%bc)) then
            if (fileReaderPtr%bc%func == BC_FUNC_QHTABLE) then
               is_QH = .true.
               ! find items in this filereader and pointer to their fields arr1d
               item_H = ecFileReaderFindItem(instancePtr, fileReaderId, 'water_level')
               item_Q = ecFileReaderFindItem(instancePtr, fileReaderId, 'water_discharge')
            end if
         else
            ! TODO, give error message
            return
         end if

      end function ecFileReaderProvidesQHtable

      ! =======================================================================

      !> Add a source Item to a FileReader's array of source Items.
      function ecFileReaderAddItem(instancePtr, fileReaderId, itemId) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: fileReaderId !< unique FileReader id
         integer,                   intent(in) :: itemId       !< unique Item id
         !
         type(tEcFileReader), pointer :: fileReaderPtr !< FileReader corresponding to fileReaderId
         type(tEcItem),       pointer :: itemPtr       !< Item corresponding to itemId
         !
         success = .false.
         fileReaderPtr => null()
         itemPtr => null()
         !
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         itemPtr%providerId = fileReaderId
         if (associated(fileReaderPtr) .and. associated(itemPtr)) then
            ! ensure capacity
            if (fileReaderPtr%nItems == size(fileReaderPtr%items)) then
               if (.not. ecArrayIncrease(fileReaderPtr%items, fileReaderPtr%nItems)) then
                  return
               end if
            end if
            fileReaderPtr%nItems = fileReaderPtr%nItems + 1
            fileReaderPtr%items(fileReaderPtr%nItems)%ptr => itemPtr
            if (associated(fileReaderPtr%bc) .and. associated(itemPtr%quantityPtr)) then           ! transfer from filereader%bc to item%quantity
               select case (fileReaderPtr%bc%func)
               case (BC_FUNC_TIM3D, BC_FUNC_TSERIES, BC_FUNC_CONSTANT)
                  itemPtr%quantityPtr%zInterpolationType = fileReaderPtr%bc%zInterpolationType
                  itemPtr%quantityPtr%vectormax = fileReaderPtr%bc%quantity%vectormax
               end select
            endif
            itemPtr%tframe => fileReaderPtr%tframe
            itemPtr%hframe => fileReaderPtr%hframe
            success = .true.
         end if
      end function ecFileReaderAddItem

      function ecFileReaderLookupAstroComponents(fileReaderPtr) result (nmissing)
         implicit none
         integer                               :: nmissing      !< function status
         type(tEcFileReader), pointer          :: fileReaderPtr !< FileReader corresponding to fileReaderId
         integer :: kcmp, icmp

         nmissing = 0
         kcmp = size(fileReaderPtr%items(1)%ptr%sourceT1FieldPtr%arr1d)
         if (.not.allocated(fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber)) then
            allocate (fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber(kcmp))
            nmissing = asc_map_components(kcmp, fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_components, fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber)
            if (nmissing>0) then
               do icmp=1, kcmp
                  if (fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_kbnumber(icmp)<0) then
                     call setECMessage('unknown component '     &
                                 // trim(fileReaderPtr%items(1)%ptr%sourceT0FieldPtr%astro_components(icmp)),                      &
                                      ' amplitude set to 0 ')
                     ! TODO: return the appropriate state
                  end if
               end do
            end if
         end if
      end function ecFileReaderLookupAstroComponents


!!!!!!
!!!!!!
!!!!!!
!!!!!!
!!!!!!==============================================================================
!!!!!function FileReaderInit_ByFileTypeAndIniString(fileReader, actor, ECData, iniString, provFileType, useweightfactors) result(success)
!!!!!  !
!!!!!  ! result
!!!!!  logical :: success
!!!!!  !
!!!!!  ! arguments
!!!!!  type(tFileReader)                 :: fileReader
!!!!!  type(tActor), intent(in), pointer :: actor
!!!!!  type(tECData)                     :: ECData
!!!!!  character(*), intent(in)          :: iniString ! provType = file: iniString = filename
!!!!!  integer     , intent(inout)       :: provFileType
!!!!!  logical     , intent(in)          :: useweightfactors
!!!!!  !
!!!!!  ! locals
!!!!!  integer                           :: ierr, i
!!!!!  integer                           :: found_files
!!!!!  integer                           :: filetype
!!!!!  type(tfileHeader), pointer        :: fileHeader
!!!!!  integer                           :: elmSetId
!!!!!  logical                           :: svwpfile
!!!!!
!!!!!  !
!!!!!  ! functions called
!!!!!  logical, external                 :: openexistingfile
!!!!!  !
!!!!!  ! body
!!!!!  fileReader%name = actor%name
!!!!!
!!!!!  svwpfile = .false.
!!!!!  if (provFileType == provFile_undefined) then
!!!!!     if (index(iniString,'.nc') > 0 .or. index(iniString,'.NC') > 0) then
!!!!!        call setECMessage("NetCDF not supported.");
!!!!!        success = .false.
!!!!!        return
!!!!!     endif
!!!!!     if (index(iniString,'.am') > 0 ) svwpfile = .true.
!!!!!     if (index(iniString,'.wnd') > 0 ) svwpfile = .true.
!!!!!  end if
!!!!!
!!!!!  if (svwpfile) then ! svwp
!!!!!     found_files = 0
!!!!!     ! first check: files exist
!!!!!     inquire(file=trim(iniString), exist=success)
!!!!!     if (success) then
!!!!!        found_files = 1
!!!!!        fileReader%fileName(1) = iniString
!!!!!     else
!!!!!        do i = 1, 3
!!!!!           success = expand_wildcard(i, iniString, fileReader%fileName(i))
!!!!!           if (success) then
!!!!!              found_files = found_files + 1
!!!!!           endif
!!!!!        enddo
!!!!!     endif
!!!!!     filereader%nHandle = found_files
!!!!!     allocate(fileReader%fileHeader(maxFileReaderFiles), stat=ierr)
!!!!!     do i = 1,  found_files ! maxFileReaderFiles
!!!!!        allocate(fileReader%fileHeader(i)%ptr, stat=ierr)
!!!!!     end do
!!!!!
!!!!!     do i = 1, found_files
!!!!!        fileHeader => fileReader%fileHeader(i)%ptr
!!!!!        success = openExistingFile(fileReader%pHandles(i), filereader%fileName(i)) ! inquire and open
!!!!!        if (.not. success) return
!!!!!        success = readmeteoheader(fileReader%pHandles(i),&
!!!!!                                  fileHeader%fileTypeName,&
!!!!!                                  fileHeader%n_quantity,&
!!!!!                                  fileHeader%quantities,&
!!!!!                                  fileHeader%n_rows,&
!!!!!                                  fileHeader%n_cols,&
!!!!!                                  fileHeader%gridFile,&
!!!!!                                  fileHeader%x_llcorner,&
!!!!!                                  fileHeader%y_llcorner,&
!!!!!                                  fileHeader%x_llcenter,&
!!!!!                                  fileHeader%y_llcenter,&
!!!!!                                  fileHeader%dx,&
!!!!!                                  fileHeader%dy,&
!!!!!                                  ierr)
!!!!!        rewind(fileReader%pHandles(i))
!!!!!     end do
!!!!!
!!!!!     ! <<< to do: add consistency checks >>>
!!!!!     ! <<< to do: add consistency checks >>>
!!!!!     ! <<< to do: add consistency checks >>>
!!!!!
!!!!!     fileHeader => fileReader%fileHeader(1)%ptr ! use header of first file
!!!!!
!!!!!     ! create element set and set pointer
!!!!!     elmSetId = FileReaderCreateElementSet(fileHeader, ECData%elementSetPtrs)
!!!!!     if (elmSetId > 0) then
!!!!!        fileReader%elementSet => ECData%elementSetPtrs(elmSetId)%ptr
!!!!!     else
!!!!!        success = .false.
!!!!!        return
!!!!!     end if
!!!!!
!!!!!     ! determine file reader file type
!!!!!     select case (trim(fileHeader%fileTypeName))
!!!!!     case ('meteo_on_computational_grid')
!!!!!        filetype = provFile_svwp
!!!!!        if (useweightfactors) filetype = provFile_svwp_weight
!!!!!     case ('meteo_on_curvilinear_grid')
!!!!!        filetype = provFile_curvi
!!!!!        if (useweightfactors) filetype = provFile_curvi_weight
!!!!!     case ('meteo_on_spiderweb_grid')
!!!!!        filetype = provFile_spiderweb
!!!!!     case ('meteo_on_equidistant_grid')
!!!!!        filetype = provFile_curvi ! ????
!!!!!     case default ! type not recoqnized
!!!!!        filetype = -1
!!!!!     end select
!!!!!
!!!!!     ! set time frame
!!!!!     allocate(fileReader%tframe, stat=ierr)
!!!!!     if (ierr /= 0) then; success = .false.; return; endif
!!!!!     success = readmeteo_times(fileReader%pHandles(1), fileReader%tframe)
!!!!!     rewind(fileReader%pHandles(1))
!!!!!  end if
!!!!!
!!!!!  if (provFileType == provFile_unimagdir) then
!!!!!     filereader%fileName(1) = inistring
!!!!!     success = openExistingFile(fileReader%pHandles(1), filereader%fileName(1)) ! inquire and open
!!!!!     if (.not. success) return
!!!!!     filetype = provFileType
!!!!!  endif
!!!!!  !
!!!!!  success = FileReaderSetFileType(fileReader, filetype)
!!!!!
!!!!!  ! create source items
!!!!!  select case (filetype)
!!!!!  case (provFile_unimagdir)
!!!!!     success = fileReaderinit_unimagdir(actor, fileReader, ECData)
!!!!!  case (provFile_svwp, provFile_svwp_weight, provFile_grib, provFile_curvi, provFile_curvi_weight)
!!!!!     success = fileReaderinit_svwp(actor, fileReader, ECData)
!!!!!  case default
!!!!!     success = .false.
!!!!!  end select
!!!!!
!!!!!!
!!!!!end function FileReaderInit_ByFileTypeAndIniString
!!!!!
!!!!!
!!!!!function fileReaderInit_svwp(actor, fileReader, ECData) result(success)
!!!!!  !
!!!!!  ! result
!!!!!  logical :: success
!!!!!  !
!!!!!  ! arguments
!!!!!  type(tActor)     , pointer :: actor
!!!!!  type(tFileReader)          :: fileReader
!!!!!  type(tECData)              :: ECData
!!!!!  !
!!!!!  ! locals
!!!!!  integer         :: windqId, presqId
!!!!!  integer         :: sWindEId, sPresEId
!!!!!  type(tQuantity), pointer :: windq, presq
!!!!!  type(tItem)    , pointer :: sWindEi, sPresEi
!!!!!  integer :: numdim, xdim, ydim
!!!!!  !
!!!!!  ! body
!!!!!
!!!!!  ! quantities
!!!!!  windqId = QuantFindOrCreate(ECData%quantityPtrs, 'wind', 2)
!!!!!  presqId = QuantFindOrCreate(ECData%quantityPtrs, 'pressure', 1)
!!!!!  if (windqId < 0 .or. presqId < 0) then
!!!!!     success = .false.
!!!!!     return
!!!!!  end if
!!!!!  windq => ECData%quantityPtrs(windqId)%ptr
!!!!!  presq => ECData%quantityPtrs(presqId)%ptr
!!!!!
!!!!!  ! source items
!!!!!  sWindEId = ItemFindOrCreate(ECData%itemPtrs, actor, windq, fileReader%elementSet, role = 0) ! role?
!!!!!  sPresEId = ItemFindOrCreate(ECData%itemPtrs, actor, presq, fileReader%elementSet, role = 0) ! role?
!!!!!  if (sWindEId < 0 .or. sPresEId < 0) then
!!!!!     success = .false.
!!!!!     return
!!!!!  end if
!!!!!  sWindEi => ECData%itemPtrs(sWindEId)%ptr
!!!!!  sPresEi => ECData%itemPtrs(sPresEId)%ptr
!!!!!
!!!!!  success = FileReaderAddItem(fileReader,sWindEi)
!!!!!  if (.not.success) return
!!!!!  success = FileReaderAddItem(fileReader,sPresEi)
!!!!!  if (.not.success) return
!!!!!
!!!!!  sWindEi%accessType = accessType_filereader
!!!!!  sWindEi%role       = ItemRole_output ! role?
!!!!!  sPresEi%accessType = accessType_filereader
!!!!!  sPresEi%role       = ItemRole_output ! role?
!!!!!
!!!!!  ! create fields source items
!!!!!  if (fileReader%elementSet%xdim > 0 .and. fileReader%elementSet%ydim > 0) then
!!!!!     numdim = 2
!!!!!     xdim   = fileReader%elementSet%xdim
!!!!!     ydim   = fileReader%elementSet%ydim
!!!!!  else
!!!!!     numdim = 1
!!!!!     xdim   = fileReader%elementSet%dim
!!!!!     ydim   = 0
!!!!!  end if
!!!!!  success = ItemCreateFields(sWindEI, 2, numdim, xdim, ydim)
!!!!!  if (.not.success) return
!!!!!  success = ItemCreateFields(sPresEi, 2, numdim, xdim, ydim)
!!!!!  if (.not.success) return
!!!!!
!!!!!end function fileReaderInit_svwp
!!!!!!
!!!!!!==============================================================================
end module m_ec_filereader
