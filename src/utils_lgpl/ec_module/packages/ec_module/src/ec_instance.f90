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

!> This module contains the constructor and destructor for the datatype tEcInstance.
!! @author edwin.bos@deltares.nl
module m_ec_instance
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_message
   use m_ec_alloc
   use m_ec_converter
   use m_ec_connection
   use m_ec_item
   use m_ec_quantity
   use m_ec_elementSet
   use m_ec_field
   use m_ec_filereader
   use m_ec_bcreader
   use m_ec_netcdf_timeseries
   use multi_file_io
   
   implicit none
   
   private
   
   public :: ecInstanceCreate
   public :: ecInstanceFree
   public :: ecInstanceCreateItem
   public :: ecInstanceCreateQuantity
   public :: ecInstanceCreateElementSet
   public :: ecInstanceCreateField
   public :: ecInstanceCreateFileReader
   public :: ecInstanceCreateBCBlock
   public :: ecInstanceCreateNetCDF
   public :: ecInstanceCreateConverter
   public :: ecInstanceCreateConnection
   public :: ecInstancePrintState
   public :: ecInstanceListSourceItems
   public :: ecInstanceListFileReaders
   public :: ecIncreaseMaxOpenFiles

   
   contains
      
      ! =======================================================================

      !> Dynamically allocates memory for a new tEcInstance, making the tEcInstance an unnamed data object.
      !! Subsequently, the new tEcInstance is initialized.
      function ecInstanceCreate(ptr) result (success)
         logical                    :: success !< function status
         type(tEcInstance), pointer :: ptr     !< intent(out)

         integer                    :: istat        !< allocate() status
         integer                    :: maxOpenFiles !< default for max. open files
         !
         success = .false.
         !
         if (associated(ptr)) then
            call setECMessage("ERROR: ec_instance::ecInstanceCreate: Dummy argument ptr is already associated.")
         else
            allocate(ptr, STAT = istat)
            if (istat == 0) then
               success = .true.
               ! Allocate arrays with a default size of 10.
               allocate(ptr%ecConnectionsPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecConnectionsPtr array.")
                  success = .false.
               end if
               ptr%nConnections = 0
               allocate(ptr%ecConvertersPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecConvertersPtr array.")
                  success = .false.
               end if
               ptr%nConverters = 0
               allocate(ptr%ecElementSetsPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecElementSetsPtr array.")
                  success = .false.
               end if
               ptr%nElementSets = 0
               allocate(ptr%ecFieldsPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecFieldsPtr array.")
                  success = .false.
               end if
               ptr%nFields = 0
               allocate(ptr%ecFileReadersPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecFileReadersPtr array.")
                  success = .false.
               end if
               ptr%nFileReaders = 0
               allocate(ptr%ecBCBlocksPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecBCBlocksPtr array.")
                  success = .false.
               end if
               ptr%nNetCDFs = 0
               allocate(ptr%ecNetCDFsPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecNetCDFsPtr array.")
                  success = .false.
               end if
               ptr%nBCFiles = 0
               allocate(ptr%ecBCFilesPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecBCFilesPtr array.")
                  success = .false.
               end if
               ptr%nBCBlocks = 0
               allocate(ptr%ecItemsPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecItemsPtr array.")
                  success = .false.
               end if
               ptr%nItems = 0
               allocate(ptr%ecQuantitiesPtr(10), STAT = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_instance::ecInstanceCreate: Unable to allocate memory for ecQuantitiesPtr array.")
                  success = .false.
               end if
               ptr%nQuantities = 0
               ptr%idCounter = 0
            end if
         end if

         !
         ! set max open files
         maxOpenFiles = maxFileUnits
         success = ecIncreaseMaxOpenFiles(maxOpenFiles)

      end function ecInstanceCreate
      
      ! =======================================================================
      
      !> Frees and deallocates dynamically allocated memory and thereby nullifies the tEcInstance's pointers.
      function ecInstanceFree(ptr) result (success)
         logical                    :: success !< function status
         type(tEcInstance), pointer :: ptr     !< intent(inout)
         !
         integer :: istat !< deallocate() status
         !
         success = .false.
         !
         if (.not. associated(ptr)) then
            call setECMessage("INFO: ec_instance::ecInstanceFree: Dummy argument ptr is already disassociated.")
         else
            ! Delegate Free-and-deallocate call to all constituent data types.
            if (.not.ecConnectionFree1dArray(ptr%ecConnectionsPtr, ptr%nConnections)) then
               return
            endif
            if (.not.ecConverterFree1dArray(ptr%ecConvertersPtr, ptr%nConverters)) then
               return
            endif
            if (.not.ecElementSetFree1dArray(ptr%ecElementSetsPtr, ptr%nElementSets)) then
               return
            endif
            if (.not.ecFieldFree1dArray(ptr%ecFieldsPtr, ptr%nFields)) then
               return
            endif
            if (.not.ecFileReaderFree1dArray(ptr%ecFileReadersPtr, ptr%nFileReaders)) then
               return
            endif
            if (.not.ecBCBlockFree1dArray(ptr%ecBCBlocksPtr, ptr%nBCBlocks)) then
               return
            endif
            if (.not.ecNetCDFFree1dArray(ptr%ecNetCDFsPtr, ptr%nNetCDFs)) then
               return
            endif
            if (.not.ecItemFree1dArray(ptr%ecItemsPtr, ptr%nItems)) then
               return
            endif
            if (.not.ecQuantityFree1dArray(ptr%ecQuantitiesPtr, ptr%nQuantities)) then
               return
            endif
               ! Finally deallocate the tEcInstance pointer.
            deallocate(ptr, stat = istat)
            if (istat == 0) success = .true.
         end if
      end function ecInstanceFree
      
      ! =======================================================================
      ! Create methods
      ! =======================================================================
      
      !> Create a new Item and register it with the Instance.
      !! Failure is indicated by: itemId == ec_undef_int
      function ecInstanceCreateItem(instancePtr) result(itemId)
         integer                    :: itemId      !< id of the new Item
         type(tEcInstance), pointer :: instancePtr !< intent(in)
         !
         type(tEcItem), pointer :: itemPtr !< the new Item
         !
         itemPtr => null()
         itemId = ec_undef_int
         itemPtr => ecItemCreate(instancePtr%nItems + 1)
         if (associated(itemPtr)) then
            ! ensure capacity
            if (instancePtr%nItems == size(instancePtr%ecItemsPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecItemsPtr, instancePtr%nItems)) then
                  return
               end if
            end if
            ! register the Item
            instancePtr%nItems = instancePtr%nItems + 1
            instancePtr%ecItemsPtr(instancePtr%nItems)%ptr => itemPtr
            itemId = itemPtr%id
         end if
      end function ecInstanceCreateItem
      
      ! =======================================================================
      
      !> Create a new Quantity and register it with the Instance.
      !! Failure is indicated by: quantityId == ec_undef_int
      function ecInstanceCreateQuantity(instancePtr) result(quantityId)
         integer                    :: quantityId  !< id of the new Quantity
         type(tEcInstance), pointer :: instancePtr !< intent(in)
         !
         type(tEcQuantity), pointer :: quantityPtr !< the new Quantity
         !
         quantityPtr => null()
         quantityId = ec_undef_int
         quantityPtr => ecQuantityCreate(instancePtr%nQuantities + 1)
         if (associated(quantityPtr)) then
            ! ensure capacity
            if (instancePtr%nQuantities == size(instancePtr%ecQuantitiesPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecQuantitiesPtr, instancePtr%nQuantities)) then
                  return
               end if
            end if
            ! register the Quantity
            instancePtr%nQuantities = instancePtr%nQuantities + 1
            instancePtr%ecQuantitiesPtr(instancePtr%nQuantities)%ptr => quantityPtr
            quantityId = quantityPtr%id
         end if
      end function ecInstanceCreateQuantity
      
      ! =======================================================================
      
      !> Create a new ElementSet and register it with the Instance.
      !! Failure is indicated by: elementSetId == ec_undef_int
      function ecInstanceCreateElementSet(instancePtr) result(elementSetId)
         integer                    :: elementSetId !< id of the new ElementSet
         type(tEcInstance), pointer :: instancePtr  !< intent(in)
         !
         type(tEcElementSet), pointer :: elementSetPtr !< the new ElementSet
         !
         elementSetPtr => null()
         elementSetId = ec_undef_int
         elementSetPtr => ecElementSetCreate(instancePtr%nElementSets + 1)
         if (associated(elementSetPtr)) then
            ! ensure capacity
            if (instancePtr%nElementSets == size(instancePtr%ecElementSetsPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecElementSetsPtr, instancePtr%nElementSets)) then
                  return
               end if
            end if
            ! register the ElementSet
            instancePtr%nElementSets = instancePtr%nElementSets + 1
            instancePtr%ecElementSetsPtr(instancePtr%nElementSets)%ptr => elementSetPtr
            elementSetId = elementSetPtr%id
         end if
      end function ecInstanceCreateElementSet
      
      ! =======================================================================
      
      !> Create a new Field and register it with the Instance.
      !! Failure is indicated by: fieldId == ec_undef_int
      function ecInstanceCreateField(instancePtr) result(fieldId)
         integer                    :: fieldId     !< id of the new Field
         type(tEcInstance), pointer :: instancePtr !< intent(in)
         !
         type(tEcField), pointer :: fieldPtr !< the new Field
         !
         fieldPtr => null()
         fieldId = ec_undef_int
         fieldPtr => ecFieldCreate(instancePtr%nFields + 1)
         if (associated(fieldPtr)) then
            ! ensure capacity
            if (instancePtr%nFields == size(instancePtr%ecFieldsPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecFieldsPtr, instancePtr%nFields)) then
                  return
               end if
            end if
            ! register the Field
            instancePtr%nFields = instancePtr%nFields + 1
            instancePtr%ecFieldsPtr(instancePtr%nFields)%ptr => fieldPtr
            fieldId = fieldPtr%id
         end if
      end function ecInstanceCreateField
      
      ! =======================================================================
      
      !> Create a new Connection and register it with the Instance.
      !! Failure is indicated by: connectionId == ec_undef_int
      function ecInstanceCreateConnection(instancePtr) result(connectionId)
         integer                    :: connectionId !< id of the new Connection
         type(tEcInstance), pointer :: instancePtr  !< intent(in)
         !
         type(tEcConnection), pointer :: connectionPtr !< the new Connection
         !
         connectionPtr => null()
         connectionId = ec_undef_int
         connectionPtr => ecConnectionCreate(instancePtr%nConnections + 1)
         if (associated(connectionPtr)) then
            ! ensure capacity
            if (instancePtr%nConnections == size(instancePtr%ecConnectionsPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecConnectionsPtr, instancePtr%nConnections)) then
                  return
               end if
            end if
            ! register the Connection
            instancePtr%nConnections = instancePtr%nConnections + 1
            instancePtr%ecConnectionsPtr(instancePtr%nConnections)%ptr => connectionPtr
            connectionId = connectionPtr%id
         end if
      end function ecInstanceCreateConnection
      
      ! =======================================================================
      
      !> Create a new Converter and register it with the Instance.
      !! Failure is indicated by: converterId == ec_undef_int
      function ecInstanceCreateConverter(instancePtr) result(converterId)
         integer                    :: converterId !< id of the new Converter
         type(tEcInstance), pointer :: instancePtr !< intent(in)
         !
         type(tEcConverter), pointer :: converterPtr !< the new Converter
         !
         converterPtr => null()
         converterId = ec_undef_int
         converterPtr => ecConverterCreate(instancePtr%nConverters + 1)
         if (associated(converterPtr)) then
            ! ensure capacity
            if (instancePtr%nConverters == size(instancePtr%ecConvertersPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecConvertersPtr, instancePtr%nConverters)) then
                  return
               end if
            end if
            ! register the Converter
            instancePtr%nConverters = instancePtr%nConverters + 1
            instancePtr%ecConvertersPtr(instancePtr%nConverters)%ptr => converterPtr
            converterId = converterPtr%id
         end if
      end function ecInstanceCreateConverter
      
      ! =======================================================================
      
      !> Create a new  FileReader and register it with the Instance.
      !! Failure is indicated by: converterId == ec_undef_int
      function ecInstanceCreateFileReader(instancePtr) result(fileReaderId)
         integer                               :: fileReaderId !< id of the new FileReader
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         !
         type(tEcFileReader), pointer :: fileReaderPtr !< the new FileReader
         !
         fileReaderId = ec_undef_int
         fileReaderPtr => ecFileReaderCreate(instancePtr%nFileReaders + 1)
         if (associated(fileReaderPtr)) then
            ! ensure capacity
            if (instancePtr%nFileReaders == size(instancePtr%ecFileReadersPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecFileReadersPtr, instancePtr%nFileReaders)) then
                  return
               end if
            end if
            ! register the FileReader
            instancePtr%nFileReaders = instancePtr%nFileReaders + 1
            instancePtr%ecFileReadersPtr(instancePtr%nFileReaders)%ptr => fileReaderPtr
            fileReaderId = fileReaderPtr%id
         end if
      end function ecInstanceCreateFileReader

      !> Create a new BCBlock and register it with the Instance (analog of filereader creation).
      function ecInstanceCreateBCBlock(instancePtr) result(bcblockId)
         integer                               :: bcblockId    !< id of the new BCBlock
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         !
         type(tEcBCBlock), pointer             :: bcblockPtr   !< intent(out), the new BCBlock
         bcblockId = ec_undef_int
         bcblockPtr => ecBCBlockCreate(instancePtr%nBCblocks + 1)
         if (associated(bcblockPtr)) then
            ! ensure capacity
            if (instancePtr%nBCBlocks == size(instancePtr%ecBCBlocksPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecBCBlocksPtr, instancePtr%nBCBlocks)) then
                  return
               end if
            end if
            ! register the BCBlock
            instancePtr%nBCBlocks = instancePtr%nBCBlocks + 1
            instancePtr%ecBCBlocksPtr(instancePtr%nBCBlocks)%ptr => bcblockPtr
            bcblockId = bcblockPtr%id
         end if
      end function ecInstanceCreateBCBlock
      
      ! =======================================================================
      !> Create a new NetCDF instance and register it with the Instance (analog of filereader creation).
      function ecInstanceCreateNetCDF(instancePtr) result(netCDFId)
      use m_ec_netcdf_timeseries
      implicit none
         integer                               :: netCDFId    !< id of the new BCBlock
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         !
         type(tEcNetCDF), pointer              :: netCDFPtr   !< intent(out), the new BCBlock
         netCDFId = ec_undef_int
         netCDFPtr => ecNetCDFCreate(instancePtr%nNetCDFs + 1)
         if (associated(netCDFPtr)) then
            ! ensure capacity
            if (instancePtr%nNetCDFs == size(instancePtr%ecNetCDFsPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecNetCDFsPtr, instancePtr%nNetCDFs)) then
                  return
               end if
            end if
            ! register the new instance
            instancePtr%nNetCDFs = instancePtr%nNetCDFs + 1
            instancePtr%ecNetCDFsPtr(instancePtr%nNetCDFs)%ptr => netCDFPtr
            netCDFId = netCDFPtr%id
         end if
      end function ecInstanceCreateNetCDF

      ! =======================================================================
      !> 
      subroutine ecInstanceListSourceItems(instancePtr,dev)
         implicit none
         ! List source items by quantity and location
         type(tEcInstance), pointer :: instancePtr           !< EC instance 
         integer, intent(in)        :: dev 
         type(tEcItem), pointer     :: sourceItemPtr
         integer  :: ii 
         do ii=1, instancePtr%nItems 
            sourceItemPtr => instancePtr%ecItemsPtr(ii)%ptr
            if (sourceItemPtr%role == itemType_source) then
                     write(dev,'(a,i5.5)') 'Source Item ',sourceItemPtr%id
                     write(dev,'(a)')      '  Quantity = '//trim(sourceItemPtr%quantityPtr%name)
                     write(dev,'(a)')      '  Location = '//trim(sourceItemPtr%elementsetPtr%name)
                     write(dev,*) ''
            endif 
         enddo
      end subroutine ecInstanceListSourceItems

      ! =======================================================================
      subroutine ecInstanceListFileReaders(instancePtr,dev)
         implicit none
         ! List source items by quantity and location
         type(tEcInstance), pointer :: instancePtr           !< EC instance 
         integer, intent(in)        :: dev 
         type(tEcFileReader), pointer     :: fileReaderPtr
         type(tEcItem)      , pointer     :: sourceItemPtr
         character(len=:)   , allocatable :: filename
         integer  :: ii, jj 

         do ii=1, instancePtr%nFileReaders 
            fileReaderPtr => instancePtr%ecFileReadersPtr(ii)%ptr
            select case (fileReaderPtr%ofType)
            case (provFile_uniform) 
               filename = trim(fileReaderPtr%fileName)
            case (provFile_bc) 
               filename = trim(fileReaderPtr%bc%fname)
            case default
               filename = ''
            end select
            write(dev,'(a,i5.5,a,i1,a)') 'Filereader ',fileReaderPtr%id,' ('''//filename//''') provides items: '
            do jj=1, fileReaderPtr%nItems
               sourceItemPtr => fileReaderPtr%items(jj)%ptr
               write(dev,'(a,i5.5)') '   Item ',sourceItemPtr%id
               write(dev,'(a)')      '      Quantity = '//trim(sourceItemPtr%quantityPtr%name)
               write(dev,'(a)')      '      Location = '//trim(sourceItemPtr%elementsetPtr%name)
            enddo 
         write(dev,*) ''
         enddo
      end subroutine ecInstanceListFileReaders
      ! =======================================================================
      
      subroutine ecInstancePrintState(instancePtr,messenger,lvl)
      use m_ec_message
      implicit none
         type(tEcInstance), pointer :: instancePtr           !< intent(in)
         interface 
            subroutine messenger(lvl,msg)
            integer, intent(in)             :: lvl
            character(len=*), intent(in)    :: msg 
            end subroutine
         end interface
         integer, intent(in)        :: lvl

         integer                      :: ii, ic, js, i, j
         type(tEcItem),       pointer :: targetItemPtr
         type(tEcConnection), pointer :: connectionPtr
         type(tEcItem),       pointer :: sourceItemPtr
         type(tEcFileReader), pointer :: FileReaderPtr
         type(tEcBCBlock),    pointer :: BCBlockPtr
         character(len=maxMessageLen) :: line
    
         call messenger(lvl, '.')
         call messenger(lvl, 'EC MODULE ITEMS:')
         do ii=1, instancePtr%nItems 
            ! TODO: This lookup loop of items may be expensive for large models, use a lookup table with ids.
            targetItemPtr => instancePtr%ecItemsPtr(ii)%ptr
            if (targetItemPtr%role == itemType_target) then
               if (associated(targetItemPtr%quantityPtr)) then
                  write(line,'(a,i5.5,a,i1,a)') 'Target Item ', targetItemPtr%id, ' (name='//trim(targetItemPtr%quantityPtr%name)//', vectormax=',targetItemPtr%quantityPtr%vectormax,')'
               else
                  write(line,'(a,i5.5)') 'Target Item ', targetItemPtr%id
               endif
               call messenger(lvl, line)
               if (associated(targetItemPtr%elementSetPtr)) then
                  write(line,'(a,i5.5,a,i1,a)') 'Element Set ', targetItemPtr%elementSetPtr%id
                  call messenger(lvl, line)
               end if
               if (targetItemPtr%nConnections==0) then
                  write(line,'(a,i5.5,a)') '   TARGET ITEM ',targetItemPtr%id,' HAS NO CONNECTIONS !!!'
                  call messenger(lvl, line)
               end if
               do ic=1, targetItemPtr%nConnections
                  connectionPtr => targetItemPtr%connectionsPtr(ic)%ptr
                  if (associated(connectionPtr%converterPtr)) then
                     write(line,'(a,i5.5,a,i5.5,a,i5.4)') '   Connection ',connectionPtr%id,', Converter ',connectionPtr%converterPtr%id,', targetIndex ',connectionPtr%converterPtr%targetIndex  
                  else
                     write(line,'(a,i5.5,a)') '   Connection ',connectionPtr%id,', Converter NONE !'
                  end if
                  call messenger(lvl, line)
                  if (connectionPtr%nSourceItems==0) then
                     call messenger(lvl, '   CONNECTION HAS NO SOURCE ITEMS !!!')
                  end if
                  do js=1, connectionPtr%nSourceItems
                     sourceItemPtr => connectionPtr%sourceItemsPtr(js)%ptr
                     write(line,'(a,i5.5,a,i1,a)') '      Source Item ',sourceItemPtr%id, ' (name='//trim(sourceItemPtr%quantityPtr%name)//', vectormax=',sourceItemPtr%quantityPtr%vectormax,')'
                     call messenger(lvl, line)
                     ! Find the FileReader which can update this source Item.
                     frs: do i=1, instancePtr%nFileReaders
                        do j=1, instancePtr%ecFileReadersPtr(i)%ptr%nItems
                           if (instancePtr%ecFileReadersPtr(i)%ptr%items(j)%ptr%id == sourceItemPtr%id) then
                              fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
                              if (associated(fileReaderPtr%bc)) then 
                                 BCBlockPtr => fileReaderPtr%bc
                                 write(line,'(a,i5.5,a)') '         File Reader ',fileReaderPtr%id, '(filename='//trim(fileReaderPtr%bc%fname)//')'
                                 call messenger(lvl, line)
                                 write(line,'(a,i5.5)') '            BCBlock ',BCBlockPtr%id 
                                 call messenger(lvl, line)
                              else 
                                 write(line,'(a,i5.5,a)') '         File Reader ',fileReaderPtr%id, '(filename='//trim(fileReaderPtr%filename)//')'
                                 call messenger(lvl, line)
                              end if 
                              if (associated(sourceItemPtr%QuantityPtr)) then
                              !  if (allocated(sourceItemPtr%QuantityPtr%name))  write(dev,'(a)') '            Quantity = '//trim(sourceItemPtr%QuantityPtr%name)
                                 if (len_trim(sourceItemPtr%QuantityPtr%name)>0) then
                                    call messenger(lvl, '            Quantity = '//trim(sourceItemPtr%QuantityPtr%name))
                                 end if
                              end if
                              if (associated(sourceItemPtr%ElementSetPtr)) then
                                 if (len_trim(sourceItemPtr%ElementSetPtr%name)>0) then
                                    call messenger(lvl, '            Location = '//trim(sourceItemPtr%ElementSetPtr%name))
                                 end if
                              end if 
                              exit frs ! exits outer named do loop
                           end if
                        end do
                     end do frs
                  enddo ! SOURCE ITEMS 
               enddo ! CONNECTIONS  
            endif 
         enddo ! TARGET ITEMS OF INSTANCE 
         call messenger(lvl,'.')
      end subroutine ecInstancePrintState


      function ecIncreaseMaxOpenFiles(max_open_files) result(success)
      implicit none
      logical                     :: success          !< function result
      integer(kind=4), intent(in) :: max_open_files   !< preferred max open files
      integer (kind=4)            :: ret              !< actual max open files, as returned by mf_increase_max_open
      character(len=128)          :: message          !< error message

      success = .false.
      ret = mf_increase_max_open(max_open_files)
      if (ret < max_open_files) then
         maxFileUnits = ret
         write(message, '(a,i0,a)') "ERROR: ec_instance::ecIncreaseMaxOpenFiles: Increasing the max number of open files to ", max_open_files, " failed."
         call setECMessage(message)
      else
         success = .true.
      end if

      end function ecIncreaseMaxOpenFiles


end module m_ec_instance
