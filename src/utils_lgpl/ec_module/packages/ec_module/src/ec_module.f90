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

!> This module contains the interfaces of the EC-module.
!! It is the main access point to the EC-module.
!! All interaction with an instance of the EC-module should take place through these interfaces.
!! An instance of the EC-module is a pointer of type tEcInstance.
!!
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
!! @author robert.leander@deltares.nl
module m_ec_module
   use m_ec_typedefs
   use m_ec_connection
   use m_ec_converter
   use m_ec_elementSet
   use m_ec_field
   use m_ec_item
   use m_ec_quantity
   use m_ec_filereader
   use m_ec_filereader_read
   use m_ec_bcreader
   use m_ec_bccollect
   use m_ec_instance
   use m_ec_message
   use m_ec_support
   use m_ec_provider
   
   implicit none
     
   !> The EC-module contains a single public pointer to a tEcInstance that is visible/usable by all kernels.
   !! Kernels can have their own private pointers to tEcInstances, which are only visible/usable by them.
   !! All these pointers should dynamically allocate memory for their tEcInstances, making the tEcInstances unnamed data objects.
   !! This is achieved by calling the interface procedure ecInstanceCreate(type(tEcInstance), pointer).
   !! All these pointers should therefor be deallocated, not directly nullified.
   !! This is achieved by calling the interface procedure ecInstanceFree(type(tEcInstance), pointer).
   type(tEcInstance), pointer, save :: public_EC_ptr => null() !< No kernel should ever call ecInstanceFree(public_EC_ptr).
   
   ! ===== constructors =====
   
   !> Construct and initialize a new EC-module instance.
   interface ecCreateInstance
      module procedure ecInstanceCreate
   end interface ecCreateInstance
   
   !> Construct and initialize a new FileReader, including its full range of source Items.
   interface ecCreateFileReader
      module procedure ecInstanceCreateFileReader
   end interface ecCreateFileReader
   
   !> Construct and initialize a new BCBlock-instance
   interface ecCreateBCBlock
      module procedure ecInstanceCreateBCBlock
   end interface ecCreateBCBlock

   !> Construct and initialize a new NetCDF-instance
   interface ecCreateNetCDF
      module procedure ecInstanceCreateNetCDF
   end interface ecCreateNetCDF
   
   !> Construct and initialze a new Quantity, returning a unique integer identifier.
   interface ecCreateQuantity
      module procedure ecInstanceCreateQuantity
   end interface ecCreateQuantity
   
   !> Construct and initialze a new ElementSet, returning a unique integer identifier.
   interface ecCreateElementSet
      module procedure ecInstanceCreateElementSet
   end interface ecCreateElementSet
   
   !> Construct and initialze a new Field, returning a unique integer identifier.
   interface ecCreateField
      module procedure ecInstanceCreateField
   end interface ecCreateField
   
   !> Construct and initialze a new Item, returning a unique integer identifier.
   interface ecCreateItem
      module procedure ecInstanceCreateItem
   end interface ecCreateItem
   
   !> Construct and initialze a new Converter, returning a unique integer identifier.
   interface ecCreateConverter
      module procedure ecInstanceCreateConverter
   end interface ecCreateConverter
   
   !> Construct and initialze a new Connection, returning a unique integer identifier.
   interface ecCreateConnection
      module procedure ecInstanceCreateConnection
   end interface ecCreateConnection
   
   ! ===== destructor =====
   
   !> Recursively free and destruct the entire data tree, which is an existing EC-module instance.
   interface ecFreeInstance
      module procedure ecInstanceFree
   end interface ecFreeInstance
   
   ! ===== getters =====
   
   !> 
   interface ecGetValues
      module procedure ecItemGetValuesMJD
      module procedure ecItemGetValues
   end interface ecGetValues

   interface ecGetQHtable
      module procedure ecItemGetQHtable
   end interface ecGetQHtable

   interface ecFindItem
      module procedure ecFileReaderFindItem
   end interface ecFindItem
   
   interface ecFindItemInFileReader
      module procedure ecFileReaderFindItem
   end interface ecFindItemInFileReader
   
   interface ecGetFileReaderNumberOfItems
      module procedure ecFileReaderGetNumberOfItems
   end interface ecGetFileReaderNumberOfItems
   
   interface ecFindXthItemInFileReader
      module procedure ecFileReaderGetItem
   end interface ecFindXthItemInFileReader

   ! ===== setters =====
   ! No convenience methods defined yet.
   
   ! Quantity
   
   interface ecSetQuantity
      module procedure ecQuantitySet
   end interface ecSetQuantity
   
   interface ecSetQuantityUnitsFillScaleOffsetFromNcidVarid
      module procedure ecQuantitySetUnitsFillScaleOffsetFromNcidVarid
   end interface ecSetQuantityUnitsFillScaleOffsetFromNcidVarid

   
   ! ElementSet
   
   interface ecSetElementSetName
      module procedure ecElementSetSetName
   end interface ecSetElementSetName

   interface ecSetElementSetType
      module procedure ecElementSetSetType
   end interface ecSetElementSetType
   
   interface ecSetElementSetXArray
      module procedure ecElementSetSetXArray
   end interface ecSetElementSetXArray
   
   interface ecSetElementSetYArray
      module procedure ecElementSetSetYArray
   end interface ecSetElementSetYArray
   
   interface ecSetElementSetZArray
      module procedure ecElementSetSetZArray
   end interface ecSetElementSetZArray

   interface ecSetElementSetvptyp
      module procedure ecElementSetSetvptyp
   end interface ecSetElementSetvptyp
   
   interface ecSetElementSetX0Dx
      module procedure ecElementSetSetX0Dx
   end interface ecSetElementSetX0Dx
   
   interface ecSetElementSetY0Dy
      module procedure ecElementSetSetY0Dy
   end interface ecSetElementSetY0Dy
   
   interface ecSetElementSetDirectionArray
      module procedure ecElementSetSetDirectionArray
   end interface ecSetElementSetDirectionArray
   
   interface ecSetElementSetLat0Dlat
      module procedure ecElementSetSetLat0Dlat
   end interface ecSetElementSetLat0Dlat
   
   interface ecSetElementSetLon0Dlon
      module procedure ecElementSetSetLon0Dlon
   end interface ecSetElementSetLon0Dlon
   
   interface ecSetElementSetMaskArray
      ! Error: Ambiguous interfaces 'ecelementsetsetmaskarray' and 'ecelementsetsetmaskpointer' in 
      ! generic interface 'ecsetelementsetmaskarray' at (1)
      
      ! module procedure ecElementSetSetMaskPointer
      module procedure ecElementSetSetMaskArray
   end interface ecSetElementSetMaskArray
   
   interface ecSetElementSetNumberOfCoordinates
      module procedure ecElementSetSetNumberOfCoordinates
   end interface ecSetElementSetNumberOfCoordinates
   
   interface ecSetElementSetSouthPoleLatitude
      module procedure ecElementSetSetSouthPoleLatitude
   end interface ecSetElementSetSouthPoleLatitude
   
   interface ecSetElementSetSouthPoleLongitude
      module procedure ecElementSetSetSouthPoleLongitude
   end interface ecSetElementSetSouthPoleLongitude
   
   interface ecSetElementSetLocations
      module procedure ecElementSetSetLocations
   end interface ecSetElementSetLocations
   
   interface ecSetElementSetRadius
      module procedure ecElementSetSetRadius
   end interface ecSetElementSetRadius
   
   interface ecSetElementSetRowsCols
      module procedure ecElementSetSetRowsCols
   end interface ecSetElementSetRowsCols
   
   interface ecSetElementSetXyen
      module procedure ecElementSetSetXyen
   end interface ecSetElementSetXyen

   interface ecSetElementSetKbotKtop
      module procedure ecElementSetSetKbotKtop
   end interface ecSetElementSetKbotKtop

   ! Field
   
   interface ecSetFieldMissingValue
      module procedure ecFieldSetMissingValue
   end interface ecSetFieldMissingValue
   
   interface ecSetFieldScalarPointer
      module procedure ecFieldSetScalarPointer 
   end interface ecSetFieldScalarPointer

   interface ecSetFieldArrayPointer
      module procedure ecFieldSet1dArrayPointer 
   end interface ecSetFieldArrayPointer
   
   interface ecSetFieldDataPtr
      module procedure ecFieldSetScalarPointer
      module procedure ecFieldSet1dArrayPointer
   end interface ecSetFieldDataPtr

   interface ecSetField1dArray
      module procedure ecFieldSet1dArray
      module procedure ecFieldCreate1dArray
   end interface ecSetField1dArray   

   ! Item

   interface ecSetItemProperty
      module procedure ecItemSetProperty
   end interface ecSetItemProperty

   interface ecCopyItemProperty
      module procedure ecItemCopyProperty
   end interface ecCopyItemProperty
   
   interface ecSetItemRole
      module procedure ecItemSetRole
   end interface ecSetItemRole
   
   interface ecSetItemType
      module procedure ecItemSetType
   end interface ecSetItemType
   
   interface ecSetItemQuantity
      module procedure ecItemSetQuantity
   end interface ecSetItemQuantity
   
   interface ecSetItemElementSet
      module procedure ecItemSetElementSet
   end interface ecSetItemElementSet
   
   interface ecSetItemSourceT0Field
      module procedure ecItemSetSourceT0Field
   end interface ecSetItemSourceT0Field
   
   interface ecSetItemSourceT1Field
      module procedure ecItemSetSourceT1Field
   end interface ecSetItemSourceT1Field
   
   interface ecSetItemTargetField
      module procedure ecItemSetTargetField
   end interface ecSetItemTargetField
   
   interface ecAddItemConnection
      module procedure ecItemAddConnection
   end interface ecAddItemConnection

   interface ecEstimateItemresultSize
      module procedure ecItemEstimateResultSize
   end interface ecEstimateItemresultSize
   
   ! Converter
   
   interface ecSetConverterType
      module procedure ecConverterSetType
   end interface ecSetConverterType
   
   interface ecSetConverterOperand
      module procedure ecConverterSetOperand
   end interface ecSetConverterOperand

   interface ecSetConverterInputPointer
      module procedure ecConverterSetInputPointer
   end interface ecSetConverterInputPointer

   interface ecSetConverterMask
      module procedure ecConverterSetMask
   end interface ecSetConverterMask

   interface ecSetConverterInterpolation
      module procedure ecConverterSetInterpolation
   end interface ecSetConverterInterpolation
   
   interface ecSetConverterElement
      module procedure ecConverterSetElement
   end interface ecSetConverterElement
   
   ! Connection
   
   interface ecSetConnectionConverter
      module procedure ecConnectionSetConverter
   end interface ecSetConnectionConverter
   
   interface ecAddConnectionSourceItem
      module procedure ecConnectionAddSourceItem
   end interface ecAddConnectionSourceItem
   
   interface ecAddConnectionTargetItem
      module procedure ecConnectionAddTargetItem
   end interface ecAddConnectionTargetItem
   
   interface ecSetConnectionIndexWeights
      module procedure ecConnectionSetIndexWeights
   end interface ecSetConnectionIndexWeights
   
   ! FileReader
   interface ecAddItemToFileReader
      module procedure ecFileReaderAddItem
   end interface ecAddItemToFileReader

   ! Provider
   interface ecCreateTimeInterpolatedItem
      module procedure ecProviderCreateTimeInterpolatedItem
   end interface ecCreateTimeInterpolatedItem

   interface ecCreateInitializeBCFileReader
      module procedure ecProviderCreateInitializeBCFileReader
   end interface ecCreateInitializeBCFileReader

   ! Support
   interface ecFindItem
      module procedure ecSupportFindItem
   end interface ecFindItem

   interface ecFindItemByQuantityLocation
      module procedure ecSupportFindItemByQuantityLocation
   end interface ecFindItemByQuantityLocation

   interface ecFindFileReader
      module procedure ecSupportFindFileReader
      module procedure ecSupportFindFileReaderByFileName
   end interface ecFindFileReader

   
    contains  
   
      ! ==========================================================================
      !> Replacement function for FM's meteo1 'addtimespacerelation' function.
      !> Calls to this routine should be embedded as follows:
      !> * Ensure that target items are created or re-used,
      !> * Call this routine which includes 
      !>      the array of TARGET ITEM ID's prepared on the caller site
      !>      the array of SOURCE QUANTITY NAMES to be sought in the FileReader
      function ecModuleAddTimeSpaceRelation(instancePtr, name, x, y, vectormax, filename, filetype, &
                                            method, operand, tgt_refdate, tgt_tzone, tgt_tunit, &
                                            jsferic, missing_value, itemIDs, &
                                            mask, xyen, z, pzmin, pzmax, pkbot, pktop, &
                                            targetIndex, forcingfile, srcmaskfile, dtnodal) &
                                            result (success)
   !     use m_ec_module, only: ecFindFileReader ! TODO: Refactor this private data access (UNST-703).
         use m_ec_filereader_read, only: ecParseARCinfoMask
         use m_ec_support
         use time_module, only: ymd2modified_jul
 
         type(tEcInstance), pointer :: instancePtr !< intent(in)
         character(len=*),                         intent(inout) :: name         !< Name for the target Quantity, possibly compounded with a tracer name.
         real(hp), dimension(:),                   intent(in)    :: x            !< Array of x-coordinates for the .
         real(hp), dimension(:),                   intent(in)    :: y            !< Array of y-coordinates for the target ElementSet.
         logical,                                  intent(in)    :: jsferic      !< Sferic coordinates
         integer,                                  intent(in)    :: vectormax    !< Vector max (length of data values at each element location).
         character(len=*),                         intent(in)    :: filename     !< File name of meteo data file.
         integer,                                  intent(in)    :: filetype     !< filetype enumeration.
         integer,                                  intent(in)    :: method       !< method enumeration.
         integer,                                  intent(in)    :: operand      !< operand enumeration.
         integer,                                  intent(in)    :: tgt_refdate
         real(kind=hp),                            intent(in)    :: tgt_tzone
         integer,                                  intent(in)    :: tgt_tunit
         real(kind=hp),                            intent(in)    :: missing_value
         integer, dimension(:),                    intent(inout) :: itemIDs      !<  Connection available outside to which one can connect target items
   
         integer,  dimension(:), optional,         intent(in)    :: mask         !< Array of masking values for the target ElementSet.
         real(hp),               optional,         intent(in)    :: xyen(:,:)    !< distance tolerance / cellsize of ElementSet.
         real(hp), dimension(:), optional, target, intent(in)    :: z            !< array of z/sigma coordinates
         real(hp), dimension(:), optional, pointer               :: pzmin        !< array of minimal z coordinate
         real(hp), dimension(:), optional, pointer               :: pzmax        !< array of maximum z coordinate
         integer,  dimension(:), optional, pointer               :: pkbot  
         integer,  dimension(:), optional, pointer               :: pktop  
         integer,                optional,         intent(in)    :: targetIndex  !< target position or rank of (complete!) vector in target array
         character(len=*),       optional,         intent(in)    :: forcingfile  !< file containing the forcing data for pli-file 'filename'
         character(len=*),       optional,         intent(in)    :: srcmaskfile  !< file containing mask applicable to the arcinfo source data 
         real(hp),               optional,         intent(in)    :: dtnodal      !< update interval for nodal factors
         !
         integer :: convtype !< EC-module's convType_ enumeration.
         !
         integer :: fileReaderId   !< Unique FileReader id.
         integer :: quantityId     !< Unique Quantity id.
         integer :: elementSetId   !< Unique ElementSet id.
         integer :: converterId    !< Unique Converter id.
         integer :: connectionId    !< Unique Converter id.
         !
         type(tEcFileReader)   , pointer :: fileReaderPtr  => null() !< 
         
         logical                   :: success
         integer, external         :: findname
         type (tEcMask)            :: srcmask
         logical                   :: res
         integer                   :: i, itgt
         integer                   :: fieldId
         real(hp)                  :: tgt_mjd
   
      success = .false.
   
 ! ============================== Setting up the SOURCE side of the connection ===================================
         ! Construct the FileReader, which constructs the source Items.
         fileReaderPtr => ecSupportFindFileReaderByFileName(instancePtr, fileName)
         if (associated(fileReaderPtr) .and. fileType==provFile_spiderweb) then                    ! double file access not allowed when using the Gnu compiler 
            fileReaderId = fileReaderPtr%id 
         else
            fileReaderId = ecInstanceCreateFileReader(instancePtr)
            fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
            fileReaderPtr%vectormax = vectormax
            
            success = ymd2modified_jul(tgt_refdate, tgt_mjd) ! TODO: handle time zone (and time?)

            if (present(forcingfile)) then
               if (present(dtnodal)) then
                  res = ecProviderInitializeFileReader(instancePtr, fileReaderId, filetype, filename, tgt_mjd, tgt_tzone, tgt_tunit, name, forcingfile=forcingfile, dtnodal=dtnodal)
               else
                  res = ecProviderInitializeFileReader(instancePtr, fileReaderId, filetype, filename, tgt_mjd, tgt_tzone, tgt_tunit, name, forcingfile=forcingfile)
               end if
               if (.not. res) return
               if (ecAtLeastOnePointIsCorrection) then       ! TODO: Refactor this shortcut (UNST-180).
                     ecAtLeastOnePointIsCorrection = .false. ! TODO: Refactor this shortcut (UNST-180).
               success = .true.
                     return
               end if
            else
               if (present(dtnodal)) then
                  res = ecProviderInitializeFileReader(instancePtr, fileReaderId, filetype, filename, tgt_mjd, tgt_tzone, tgt_tunit, name, dtnodal=dtnodal)
               else
                  res = ecProviderInitializeFileReader(instancePtr, fileReaderId, filetype, filename, tgt_mjd, tgt_tzone, tgt_tunit, name)
               end if
               if (.not. res) return
            end if
         end if

         ! Construct the target Quantity.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. ecQuantitySet(instancePtr, quantityId, name=name, units=' ', vectormax=vectormax)) return
   
         ! Construct the target ElementSet.
         elementSetId = ecInstanceCreateElementSet(instancePtr)
   
         if (filetype == provFile_poly_tim) then
               res = ecElementSetSetType(instancePtr, elementSetId, elmSetType_polytim)
         else
            if (jsferic) then
               res = ecElementSetSetType(instancePtr, elementSetId, elmSetType_spheric)
            else
               res = ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)
            end if
         end if
         if (.not. res) return
   
         if (.not.ecElementSetSetXArray(instancePtr, elementSetId, x)) return
         if (.not.ecElementSetSetYArray(instancePtr, elementSetId, y)) return
         if (present(mask)) then
            if(.not.ecElementSetSetMaskArray(instancePtr, elementSetId, mask))return
         endif
         if (.not.ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, size(x))) return
         if (present(xyen)) then
            if (.not.ecElementSetSetXyen(instancePtr, elementSetId, xyen)) return
         end if
         
         if (present(z)) then ! 3D
            if (present(pzmin) .and. present(pzmax)) then        ! implicitly means: target elt z-type == SIGMA
               if (.not.ecElementSetSetZArray(instancePtr, elementSetId, z, pzmin=pzmin, pzmax=pzmax, Lpointer_=.true.)) return
               if (.not.ecElementSetSetvptyp(instancePtr, elementSetID, BC_VPTYP_PERCBED)) return ! sigma layers
            else if (present(pkbot) .and. present(pktop)) then   ! implicitly means: target elt z-type == Z WITH sparse kbot/ktop storage
               if (.not.ecElementSetSetZArray(instancePtr, elementSetId, z, Lpointer_=.true.)) return
               if (.not.ecElementSetSetKbotKtop(instancePtr, elementSetId, pkbot, pktop, Lpointer_=.true.)) return
               if (.not.ecElementSetSetvptyp(instancePtr, elementSetID, BC_VPTYP_ZDATUM)) return ! z-layers
            else
               ! ERROR .. TODO: LR
               continue
            end if
         end if
         
         ! Construct a new Converter.
         convtype = ec_filetype_to_conv_type(filetype, name)
         if (convtype == convType_undefined) then
            call setECMessage("Unsupported converter for file '"//filename//"'.")
            return
         end if
         
         converterId = ecInstanceCreateConverter(instancePtr)
         if (present(srcmaskfile)) then 
            if (filetype == provFile_arcinfo .or. filetype == provFile_curvi) then
               if (.not.ecParseARCinfoMask(srcmaskfile, srcmask, fileReaderPtr)) then
                  !LC: to substitute with setECMessage?
                  !call setMessage("Error while reading mask file '"//trim(srcmaskfile)//"'.")
                  return
               endif 
               if (.not.ecConverterInitialize(instancePtr, converterId, convtype, operand, method, srcmask=srcmask)) then 
                  !LC: to substitute with setECMessage?
                  !call setMessage("Error while setting mask to converter (file='"//trim(srcmaskfile)//      &
                  !                "', associated with meteo file '"//trim(filename)//"'.")
                  return
               end if 
            end if
         else
            if (.not.ecConverterInitialize(instancePtr, converterId, convtype, operand, method)) return
         end if
         
 ! ============================== Setting up the TARGET side of the connection ===================================
         
         quantityId = ecCreateQuantity(instancePtr)
         if (.not.ecSetQuantity(instancePtr, quantityId, name=name, units=' ', vectormax=vectormax)) return
   
         elementSetId = ecCreateElementSet(instancePtr)
   
         if (filetype == provFile_poly_tim) then
            if (.not.ecSetElementSetType(instancePtr, elementSetId, elmSetType_polytim)) return
         else
            if (jsferic) then
               if (.not.ecSetElementSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
            else
               if (.not.ecSetElementSetType(instancePtr, elementSetId, elmSetType_spheric)) return
            end if
         end if
         
         if (.not.ecSetElementSetXArray(instancePtr, elementSetId, x)) return
         if (.not.ecSetElementSetYArray(instancePtr, elementSetId, y)) return
!        if (.not.ecSetElementSetMaskArray(instancePtr, elementSetId, mask)) return
         if (.not.ecSetElementSetNumberOfCoordinates(instancePtr, elementSetId, size(x))) return

         if (present(xyen)) then
            if (.not.ecSetElementSetXyen(instancePtr, elementSetId, xyen)) return
         end if
         
         if (present(z)) then ! 3D
            if (present(pzmin) .and. present(pzmax)) then       ! implicitly means: target elt z-type == SIGMA
               if (.not.ecSetElementSetZArray(instancePtr, elementSetId, z, pzmin=pzmin, pzmax=pzmax, Lpointer_=.true.)) return
               if (.not.ecSetElementSetvptyp(instancePtr, elementSetID, BC_VPTYP_PERCBED)) return ! sigma layers
            else if (present(pkbot) .and. present(pktop))  then ! implicitly means: target elt z-type == Z WITH sparse kbot/ktop storage
               if (.not.ecSetElementSetZArray(instancePtr, elementSetId, z, Lpointer_=.true.)) return
               if (.not.ecSetElementSetKbotKtop(instancePtr, elementSetId, pkbot, pktop, Lpointer_=.true.)) return
               if (.not.ecSetElementSetvptyp(instancePtr, elementSetID, BC_VPTYP_ZDATUM)) return ! z-layers
            else
               ! ERROR .. TODO: LR
            end if
         
            ! add 3D settings if needed
            if (filetype == provFile_poly_tim) then 
!            .and. (target_name == 'salinitybnd' .or. target_name == 'temperaturebnd' .or. target_name == 'tracerbnd')) then   ! TODO JRE sediment    
               if (.not.ecSetElementSetMaskArray(instancePtr, elementSetId, mask)) return
               if (.not.ecSetElementSetNumberOfCoordinates(instancePtr, elementSetId, size(x))) return
            end if
         end if
         
         do itgt = 1,size(itemIDs)
            fieldId = ecCreateField(instancePtr)
            if (.not.ecSetFieldMissingValue(instancePtr, fieldId, ec_undef_hp)) return

            if (itemIDs(itgt) == ec_undef_int) then                ! if Target Item already exists, do NOT create a new one ... 
               itemIDs(itgt) = ecCreateItem(instancePtr)
               if (.not.ecSetItemRole(instancePtr, itemIDs(itgt), itemType_target)) return
               if (.not.ecSetItemQuantity(instancePtr, itemIDs(itgt), quantityId)) return
            end if
            if (.not.ecSetItemElementSet(instancePtr, itemIDs(itgt), elementSetId)) return
            if (.not.ecSetItemTargetField(instancePtr, itemIDs(itgt), fieldId)) return    
         enddo
         
         converterId = ecCreateConverter(instancePtr)
         if (.not.ecConverterSetType(instancePtr, converterId, convtype)) return
         if (.not.ecConverterSetOperand(instancePtr, converterId, operand)) return
         if (.not.ecConverterSetInterpolation(instancePtr, converterId, method)) return
         
         connectionId = ecCreateConnection(instancePtr)
         if (.not. ecSetConnectionConverter(instancePtr, connectionId, converterId)) return

         ! Connect the source items to the connector
         do i=1,fileReaderPtr%nItems
            if (.not.ecAddConnectionSourceItem(instancePtr, connectionId, fileReaderPtr%items(i)%ptr%id)) return
         enddo

         ! Connect the target items to the connector
         ! Loop over the given item id list for the TARGET side 
         do i=1,size(itemIDs)
            if (.not.ecAddConnectionTargetItem(instancePtr, connectionId, itemIDs(i))) return
            if (.not.ecAddItemConnection(instancePtr, itemIDs(i), connectionId)) return 
         enddo

         if (.not. ecSetConnectionIndexWeights(InstancePtr, connectionId)) return

      success = .true.
      end function ecModuleAddTimeSpaceRelation
                                               
                                               
   !> Replacement function for FM's meteo1 'gettimespacevalue' function.
   function ec_gettimespacevalue_by_itemID(instancePtr, itemId, tgt_refdate, tgt_tzone, tgt_tunit, timesteps, target_array) result(success)
      use time_module
      use time_class
      logical                                                 :: success      !< function status
      type(tEcInstance),                        pointer       :: instancePtr  !< intent(in)
      integer,                                  intent(in)    :: itemID       !< unique Item id
      integer,                                  intent(in)    :: tgt_refdate
      real(kind=hp),                            intent(in)    :: tgt_tzone
      integer,                                  intent(in)    :: tgt_tunit
      real(hp),                                 intent(in)    :: timesteps    !< time
      real(hp), dimension(:), target, optional, intent(inout) :: target_array !< kernel's data array for the requested values

      type(c_time)                                            :: ecReqTime    !< time stamp for request to EC
      real(hp)                                                :: tUnitFactor  !< factor for time step unit
      real(hp)                                                :: tgt_mjd      !< current time in modified julian days
      character(len=20) :: datestring
      integer           :: ierr
      

      if (itemId == ec_undef_int) then       ! We isolate the case that itemId was uninitialized,
         success = .true.                    ! in which case we simply ignore the Get-request
         return
      else
         call clearECMessage()
         tUnitFactor = ecSupportTimeUnitConversionFactor(tgt_tunit)
         success = ymd2modified_jul(tgt_refdate, tgt_mjd)
         success = .false.
         call ecReqTime%set2(tgt_mjd, timesteps * tUnitFactor / 86400.0_hp - tgt_tzone / 24.0_hp)        
         if (.not. ecGetValues(instancePtr, itemId, ecReqTime, target_array)) then
            datestring = datetime_to_string(ecReqTime%mjd(), ierr)
            if (ierr==0) then
               call setECMessage('Requested time was: '//datestring//' ! ')
            end if
            return
         end if
         success = .true.
      end if
   end function ec_gettimespacevalue_by_itemID 
                                               
      ! =======================================================================
      !> Helper function for initializing a Connection.
      function ecModuleConnectItem(instancePtr, connectionId, sourceItemId, targetItemId) result(success)
         logical                          :: success      !< function status
         type(tEcInstance), pointer       :: instancePtr  !< 
         integer,           intent(inout) :: connectionId !< 
         integer,           intent(inout) :: sourceItemId !< 
         integer,           intent(inout) :: targetItemId !< 
         !
         success              = ecConnectionAddSourceItem(instancePtr, connectionId, sourceItemId)
         if (success) success = ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)
         if (success) success = ecItemAddConnection(instancePtr, targetItemId, connectionId)
      end function ecModuleConnectItem

      ! ==========================================================================
      
      !> Given the quantity names and the filereader ID, search for source items provided by the filereader with these quantity names and associate them with the connection.
      function ecModuleConnectSrc(instancePtr, fileReaderId, connectionId, qnames) result (success)
         implicit none
         logical                                     :: success      !< function status
         type(tEcInstance),               pointer    :: instancePtr  !< instance of the EC module
         integer,                         intent(in) :: fileReaderId !< identifier of the filereader
         integer,                         intent(in) :: connectionId !< identifier of the connection
         character(len=*), dimension(:),  intent(in) :: qnames       !< list of quantity names 
         integer :: sourceItemId
         integer :: i, isrc
         !
      success = .false.
         do isrc = 1, size(qnames)
            sourceItemId = ecFindItemInFileReader(instancePtr, fileReaderId, trim(qnames(i)))
            if (sourceItemId==ec_undef_int) then
               return
            endif
            if (.not.ecAddConnectionSourceItem(instancePtr, connectionId, sourceItemId)) return
         enddo
      success = .true.
      end function ecModuleConnectSrc      
      
      ! ==========================================================================

      !> Translate EC's 'filetype' to EC's 'convType' enum.
      function ec_filetype_to_conv_type(filetype, quantity) result (convtype)
      integer                          :: convtype !< converter type number (m_ec_parameters)
      integer, intent(in)              :: filetype !< file type (m_ec_parameters)
      character(len=*), intent(in)     :: quantity !< quantity name
      !
      select case (filetype)
      case (provFile_bc)
         select case (quantity)
         case ('qhbnd')    
            convtype = convType_qhtable
         case default    
            convtype = convType_uniform
         end select
      case (provFile_uniform)
         convtype = convType_uniform
      case (provFile_fourier)
         convtype = convType_fourier
      case (provFile_unimagdir)
         convtype = convType_unimagdir
      case (provFile_svwp)
         convtype = convType_undefined ! not yet implemented
      case (provFile_arcinfo)
         convtype = convType_arcinfo
      case (provFile_spiderweb)
         convtype = convType_spiderweb
      case (provFile_curvi)
         convtype = convType_curvi
      case (provFile_samples)
         convtype = convType_samples
      case (provFile_triangulationmagdir)
         convtype = convType_undefined ! not yet implemented
      case (provFile_poly_tim)
         convtype = convType_polytim
      case (provFile_netcdf)
         convtype = convType_netcdf
      case (provFile_qhtable)
         convtype = convType_qhtable
      case default
         convtype = convType_undefined
      end select
      end function ec_filetype_to_conv_type

      ! ==========================================================================

end module m_ec_module
