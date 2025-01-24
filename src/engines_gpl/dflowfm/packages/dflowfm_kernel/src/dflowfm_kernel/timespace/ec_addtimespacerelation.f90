!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

submodule(m_meteo) m_ec_addtimespacerelation

   implicit none

contains
   ! ==========================================================================
   !> Replacement function for FM's meteo1 'addtimespacerelation' function.
   module logical function ec_addtimespacerelation(name, x, y, mask, vectormax, filename, filetype, method, operand, &
                                                   xyen, z, pzmin, pzmax, pkbot, pktop, targetIndex, forcingfile, srcmaskfile, &
                                                   dtnodal, quiet, varname, varname2, targetMaskSelect, &
                                                   tgt_data1, tgt_data2, tgt_data3, tgt_data4, &
                                                   tgt_item1, tgt_item2, tgt_item3, tgt_item4, &
                                                   multuni1, multuni2, multuni3, multuni4)
      use m_fm_wq_processes_sub, only: get_waqinputname
      use m_ec_module, only: ecFindFileReader, ec_filetype_to_conv_type ! TODO: Refactor this private data access (UNST-703).
      use m_ec_filereader_read, only: ecParseARCinfoMask
      use m_flowparameters, only: jawave
      use m_sferic, only: jsferic
      use m_missing, only: dmiss
      use m_flowtimes, only: refdate_mjd
      use string_module, only: str_upper
      use timespace_parameters
      use timespace
      use fm_external_forcings_utils, only: get_tracername, get_sedfracname
      use timespace_read, only: maxnamelen
      use precision, only: dp
      use unstruc_messages, only: callback_msg

      character(len=*), intent(in) :: name !< Name for the target Quantity, possibly compounded with a tracer name.
      real(kind=dp), dimension(:), intent(in) :: x !< Array of x-coordinates for the target ElementSet.
      real(kind=dp), dimension(:), intent(in) :: y !< Array of y-coordinates for the target ElementSet.
      integer, intent(in) :: vectormax !< Vector max (length of data values at each element location).
      integer, dimension(:), intent(in) :: mask !< Array of masking values for the target ElementSet.
      character(len=*), intent(in) :: filename !< File name of meteo data file.
      integer, intent(in) :: filetype !< FM's filetype enumeration.
      integer, intent(in) :: method !< FM's method enumeration.
      character(len=1), intent(in) :: operand !< FM's operand enumeration.
      real(kind=dp), optional, intent(in) :: xyen(:, :) !< FM's distance tolerance / cellsize of ElementSet.
      real(kind=dp), dimension(:), optional, intent(in), target :: z !< FM's array of z/sigma coordinates
      real(kind=dp), dimension(:), optional, pointer :: pzmin !< FM's array of minimal z coordinate
      real(kind=dp), dimension(:), optional, pointer :: pzmax !< FM's array of maximum z coordinate
      integer, dimension(:), intent(in), optional, pointer :: pkbot
      integer, dimension(:), intent(in), optional, pointer :: pktop
      integer, optional, intent(in) :: targetIndex !< target position or rank of (complete!) vector in target array
      character(len=*), optional, intent(in) :: forcingfile !< file containing the forcing data for pli-file 'filename'
      character(len=*), optional, intent(in) :: srcmaskfile !< file containing mask applicable to the arcinfo source data
      real(kind=dp), optional, intent(in) :: dtnodal !< update interval for nodal factors
      logical, optional, intent(in) :: quiet !< When .true., in case of errors, do not write the errors to screen/dia at the end of the routine.
      character(len=*), optional, intent(in) :: varname !< variable name within filename
      character(len=*), optional, intent(in) :: varname2 !< variable name within filename
      character(len=1), optional, intent(in) :: targetMaskSelect !< 'i'nside (default) or 'o'utside mask polygons
      real(kind=dp), dimension(:), optional, pointer :: tgt_data1 !< optional pointer to the storage location for target data 1 field
      real(kind=dp), dimension(:), optional, pointer :: tgt_data2 !< optional pointer to the storage location for target data 2 field
      real(kind=dp), dimension(:), optional, pointer :: tgt_data3 !< optional pointer to the storage location for target data 3 field
      real(kind=dp), dimension(:), optional, pointer :: tgt_data4 !< optional pointer to the storage location for target data 4 field
      integer, optional, intent(inout), target :: tgt_item1 !< optional target item ID 1
      integer, optional, intent(inout), target :: tgt_item2 !< optional target item ID 2
      integer, optional, intent(inout), target :: tgt_item3 !< optional target item ID 3
      integer, optional, intent(inout), target :: tgt_item4 !< optional target item ID 4
      integer, optional, intent(inout), target :: multuni1 !< multiple uni item ID 1
      integer, optional, intent(inout), target :: multuni2 !< multiple uni item ID 2
      integer, optional, intent(inout), target :: multuni3 !< item ID 3
      integer, optional, intent(inout), target :: multuni4 !< item ID 4
      !
      integer :: ec_filetype !< EC-module's enumeration.
      integer :: ec_convtype !< EC-module's convType_ enumeration.
      integer :: ec_method !< EC-module's interpolate_ enumeration.
      integer :: ec_operand !< EC-module's operand_ enumeration.
      !
      integer :: fileReaderId !< Unique FileReader id.
      integer :: quantityId !< Unique Quantity id.
      integer :: elementSetId !< Unique ElementSet id.
      integer :: fieldId !< Unique Field id.
      integer :: fieldId_2 !< Unique Field id.
      integer :: fieldId_3 !< Unique Field id.
      integer :: fieldId_4 !< Unique Field id.
      integer :: converterId !< Unique Converter id.
      integer :: connectionId !< Unique Connection id.
      integer :: sourceItemId !< Unique source item id.
      integer :: sourceItemId_2 !< Unique additional second source item id.
      integer :: sourceItemId_3 !< Unique additional third source item id.
      integer :: sourceItemId_4 !< Unique additional fourth source item id.
      integer :: ndx
      !
      character(len=maxnamelen) :: sourceItemName !< name of source item (as created by provider)
      character(len=maxnamelen) :: target_name !< Unstruc target name derived from user-specified name
      character(len=maxnamelen) :: location !< location (name) as specified in the LOCATION field of the new EXT-file
      integer, pointer :: targetItemPtr1 => null() !< pointer to the target item id
      integer, pointer :: targetItemPtr2 => null() !< pointer to optional second target item id (e.g. in case of windxy)
      integer, pointer :: targetItemPtr3 => null() !< pointer to optional third target item id (e.g. in case of spiderweb)
      integer, pointer :: targetItemPtr4 => null() !< pointer to optional fourth target item id (e.g. in case of hacs)
      real(kind=dp), dimension(:), pointer :: dataPtr1 => null() !< Pointer to FM's 1D data arrays.
      real(kind=dp), dimension(:), pointer :: dataPtr2 => null() !< Pointer to FM's optional extra 1D data array (e.g. in case of windxy)
      real(kind=dp), dimension(:), pointer :: dataPtr3 => null() !< Pointer to FM's optional third 1D data array (e.g. in case of spiderweb)
      real(kind=dp), dimension(:), pointer :: dataPtr4 => null() !< Pointer to FM's optional fourth 1D data array (e.g. in case of hacs)
      type(tEcFileReader), pointer :: fileReaderPtr => null() !<

      logical :: success
      logical :: quiet_
      character(len=NAMTRACLEN) :: trname, sfname, qidname
      character(len=20) :: waqinput
      integer, external :: findname
      type(tEcMask) :: srcmask
      integer :: itargetMaskSelect !< 1:targetMaskSelect='i' or absent, 0:targetMaskSelect='o'
      logical :: exist, opened, withCharnock, withStress

      real(kind=dp) :: relrow, relcol
      real(kind=dp), allocatable :: transformcoef(:)
      integer :: row0, row1, col0, col1, ncols, nrows, issparse, Ndatasize
      character(len=128) :: txt1, txt2, txt3
      real(kind=dp), pointer :: inputptr => null()

      call clearECMessage()
      ec_addtimespacerelation = .false.
      if (present(quiet)) then
         quiet_ = quiet
      else
         quiet_ = .false. ! Default: print errors at the end of routine, if no success
      end if

      ndx = size(x)

      ! ========================================================
      ! Translate FM's enumerations to EC-module's enumerations.
      ! ========================================================
      call filetype_fm_to_ec(filetype, ec_filetype)
      if (ec_filetype == provFile_undefined) then
         write (msgbuf, '(a,i0,a)') 'm_meteo::ec_addtimespacerelation: Unsupported filetype ''', filetype, &
            ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if
      call method_fm_to_ec(method, ec_method)
      if (ec_method == interpolate_unknown) then
         write (msgbuf, '(a,i0,a)') 'm_meteo::ec_addtimespacerelation: Unsupported method ''', method, &
            ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if
      call operand_fm_to_ec(operand, ec_operand)
      if (ec_operand == operand_undefined) then
         write (msgbuf, '(a,a,a)') 'm_meteo::ec_addtimespacerelation: Unsupported operand ''', operand, &
            ''' for quantity '''//trim(name)//''' and file '''//trim(filename)//'''.'
         call err_flush()
         return
      end if

      ! =================================================
      ! Convert ext file names to accepted Unstruc names.
      ! =================================================
      ! Name conversion: (targetname=qidname==name for all names, except name=tracerbndfoo --> qidname=tracerbnd)
      qidname = name
      call get_tracername(name, trname, qidname)
      call get_sedfracname(name, sfname, qidname)
      call get_waqinputname(name, waqinput, qidname)
      target_name = qidname

      call clearECMessage()

      ! ============================================================
      ! If BC-Type file, create filereader and source items here
      ! ============================================================
      location = filename
      if (ec_filetype == provFile_bc) then
         if (.not. ecCreateInitializeBCFileReader(ecInstancePtr, forcingfile, location, qidname, &
                                                  refdate_mjd, tzone, ec_second, fileReaderId)) then

            if (.not. quiet_) then
               message = dumpECMessageStack(LEVEL_WARN, callback_msg)
            end if
            message = 'Boundary '''//trim(qidname)//''', location='''//trim(location)//''', file='''//trim(forcingfile)//''' failed!'
            call mess(LEVEL_ERROR, message)
         end if
      else
         !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile, dtnodal=dtnodal)
         !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile)
         ! ============================================================
         ! For the remaining types, construct the fileReader and source Items here.
         ! ============================================================
         ! first see if the file has already been opened
         inquire (file=trim(fileName), exist=exist, opened=opened)
         if (opened .and. ec_fileType == provFile_spiderweb) then ! double file access not allowed when using the Gnu compiler
            fileReaderPtr => ecFindFileReader(ecInstancePtr, fileName)
            if (.not. associated(fileReaderPtr)) then
               continue
            end if
            fileReaderId = fileReaderPtr%id
         else
            !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, dtnodal=dtnodal, varname=varname)
            fileReaderId = ecCreateFileReader(ecInstancePtr)

            fileReaderPtr => ecFindFileReader(ecInstancePtr, fileReaderId) ! TODO: Refactor this private data access (UNST-703).

            fileReaderPtr%vectormax = vectormax

            if (present(forcingfile)) then
               if (present(dtnodal)) then
                  success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile, dtnodal=dtnodal / 86400.d0)
               else
                  success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, forcingfile=forcingfile)
               end if
               !message = dumpECMessageStack(LEVEL_WARN,callback_msg)
               if (.not. success) then
                  goto 1234
               end if
               if (ecAtLeastOnePointIsCorrection) then ! TODO: Refactor this shortcut (UNST-180).
                  ecAtLeastOnePointIsCorrection = .false. ! TODO: Refactor this shortcut (UNST-180).
                  ec_addtimespacerelation = .true.
                  return
               end if
            else
               !success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, varname=varname)
               if (name == 'qhbnd') then
                  ec_filetype = provFile_qhtable
                  success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename(1:index(filename, '.'))//'qh', refdate_mjd, tzone, ec_second, name)
               else
                  if (present(dtnodal)) then
                     success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, dtnodal=dtnodal / 86400.d0, varname=varname)
                  else
                     if (present(varname2)) then
                        success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, varname=varname, varname2=varname2)
                     else
                        success = ecSetFileReaderProperties(ecInstancePtr, fileReaderId, ec_filetype, filename, refdate_mjd, tzone, ec_second, name, varname=varname)
                     end if
                  end if
                  if (.not. success) then
                     ! message = ecGetMessage()
                     ! message = dumpECMessageStack(LEVEL_WARN,callback_msg)
                     ! NOTE: do all error dumping (if any) at the end of this routine at label 1234

                     ! NOTE: in relation to WAVE: all calling WAVE-related routines now pass quiet=.true. to this addtimespace routine.
                     ! When running online with WAVE and the first WAVE calculation is after the first DFlowFM calculation,
                     ! this message will be generated. This must be a warning: notify the user that DFlowFM is going to do
                     ! a calculation with zero wave values. This message should be written every time step, until proper
                     ! wave data is available. The user has to check whether this behaviour is as expected.
                     goto 1234
                  end if
               end if
            end if
         end if
      end if

      ! ==============================
      ! Construct the target Quantity.
      ! ==============================
      quantityId = ecCreateQuantity(ecInstancePtr)
      if (.not. ecSetQuantity(ecInstancePtr, quantityId, name=target_name, units=' ', vectormax=vectormax)) then
         goto 1234
      end if

      ! ================================
      ! Construct the target ElementSet.
      ! ================================
      elementSetId = ecCreateElementSet(ecInstancePtr)

      if (ec_filetype == provFile_poly_tim) then
         success = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_polytim)
      else
         if (jsferic == 0) then
            success = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_cartesian)
         else
            success = ecSetElementSetType(ecInstancePtr, elementSetId, elmSetType_spheric)
         end if
      end if

      if (success) success = ecSetElementSetXArray(ecInstancePtr, elementSetId, x)
      if (success) success = ecSetElementSetYArray(ecInstancePtr, elementSetId, y)
      if (success) success = ecSetElementSetMaskArray(ecInstancePtr, elementSetId, mask)
      if (success) success = ecSetElementSetNumberOfCoordinates(ecInstancePtr, elementSetId, size(x))
      if (present(xyen)) then
         if (success) success = ecSetElementSetXyen(ecInstancePtr, elementSetId, xyen)
      end if

      if (present(z)) then ! 3D
         if (present(pzmin) .and. present(pzmax)) then ! implicitly means: target elt z-type == SIGMA
            if (success) success = ecSetElementSetZArray(ecInstancePtr, elementSetId, z, pzmin=pzmin, pzmax=pzmax, Lpointer_=.true.)
            if (success) success = ecSetElementSetvptyp(ecInstancePtr, elementSetID, BC_VPTYP_PERCBED) ! sigma layers
         else if (present(pkbot) .and. present(pktop)) then ! implicitly means: target elt z-type == Z WITH sparse kbot/ktop storage
            if (success) success = ecSetElementSetZArray(ecInstancePtr, elementSetId, z, Lpointer_=.true.)
            if (success) success = ecSetElementSetKbotKtop(ecInstancePtr, elementSetId, pkbot, pktop, Lpointer_=.true.)
            if (success) success = ecSetElementSetvptyp(ecInstancePtr, elementSetID, BC_VPTYP_ZDATUM) ! z-layers
         else
            ! ERROR .. TODO: LR
         end if

         ! add 3D settings if needed
         if (ec_filetype == provFile_poly_tim .and. (target_name == 'salinitybnd' .or. target_name == 'temperaturebnd' .or. target_name == 'tracerbnd' .or. target_name == 'sedfracbnd')) then ! TODO JRE sediment
            if (success) success = ecSetElementSetMaskArray(ecInstancePtr, elementSetId, mask)
            if (success) success = ecSetElementSetNumberOfCoordinates(ecInstancePtr, elementSetId, size(x))
         end if
      end if

      if (.not. success) then
         goto 1234
      end if

      ! ==============================================
      ! Construct the target field and the target item
      ! ==============================================
      ! determine which target item (id) will be created, and which FM data array has to be used
      if (.not. fm_ext_force_name_to_ec_item(trname, sfname, waqinput, qidname, &
                                             targetItemPtr1, targetItemPtr2, targetItemPtr3, targetItemPtr4, &
                                             dataPtr1, dataPtr2, dataPtr3, dataPtr4)) then
         return
      end if
      continue

      ! Overrule hard-coded pointers to target data by optional pointers passed in the call
      if (present(tgt_data1)) dataPtr1 => tgt_data1
      if (present(tgt_data2)) dataPtr2 => tgt_data2
      if (present(tgt_data3)) dataPtr3 => tgt_data3
      if (present(tgt_data4)) dataPtr4 => tgt_data4

      ! Overrule hard-coded pointers to target items by optional pointers passed in the call
      if (present(tgt_item1)) targetItemPtr1 => tgt_item1
      if (present(tgt_item2)) targetItemPtr2 => tgt_item2
      if (present(tgt_item3)) targetItemPtr3 => tgt_item3
      if (present(tgt_item4)) targetItemPtr4 => tgt_item4

      ! Create the field and the target item, and if needed additional ones.
      fieldId = ecCreateField(ecInstancePtr)
      success = ecSetField1dArray(ecInstancePtr, fieldId, dataPtr1)
      if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId, dmiss)
      if (success) success = createItem(ecInstancePtr, targetItemPtr1, quantityId, elementSetId, fieldId)
      if (present(multuni1)) then ! if multiple-uni item(s) specified:
         if (multuni1 < 0) then
            multuni1 = ecInstanceCreateItem(ecInstancePtr)
            if (.not. ecSetItemRole(ecInstancePtr, multuni1, itemType_target)) return
         end if
         connectionId = ecCreateConnection(ecInstancePtr)
         if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr1)) return ! connecting source to new converter
         if (.not. ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni1)) return ! connecting multuni1 as target item to the new converter
         if (.not. ecCopyItemProperty(ecInstancePtr, multuni1, targetItemPtr1, 'quantityPtr')) return ! copying the quantity pointer to the multi uni item
         if (.not. ecAddItemConnection(ecInstancePtr, multuni1, connectionId)) return ! adding the new converter to multuni1
      end if
      if (associated(targetItemPtr2)) then
         ! second field (e.g. for 'windxy')
         fieldId_2 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_2, dataPtr2)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_2, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr2, quantityId, elementSetId, fieldId_2)
         if (present(multuni2)) then ! if multiple-uni item(s) specified:
            if (multuni2 < 0) then
               multuni2 = ecInstanceCreateItem(ecInstancePtr)
               if (.not. ecSetItemRole(ecInstancePtr, multuni2, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr2)) return ! connecting source to new converter
            if (.not. ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni2)) return ! connecting multuni1 as target item to the new converter
            if (.not. ecCopyItemProperty(ecInstancePtr, multuni2, targetItemPtr2, 'quantityPtr')) return ! copying the quantity pointer to the multi uni item
            if (.not. ecAddItemConnection(ecInstancePtr, multuni2, connectionId)) return ! adding the new converter to multuni1
         end if
      end if
      if (associated(targetItemPtr3)) then
         ! third field (e.g. for 'airpressure_windx_windy'
         fieldId_3 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_3, dataPtr3)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_3, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr3, quantityId, elementSetId, fieldId_3)
         if (present(multuni3)) then ! if multiple-uni item(s) specified:
            if (multuni3 < 0) then
               multuni3 = ecInstanceCreateItem(ecInstancePtr)
               if (.not. ecSetItemRole(ecInstancePtr, multuni3, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr3)) return ! connecting source to new converter
            if (.not. ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni3)) return ! connecting multuni1 as target item to the new converter
            if (.not. ecCopyItemProperty(ecInstancePtr, multuni3, targetItemPtr3, 'quantityPtr')) return ! copying the quantity pointer to the multi uni item
            if (.not. ecAddItemConnection(ecInstancePtr, multuni3, connectionId)) return ! adding the new converter to multuni1
         end if
      end if
      if (associated(targetItemPtr4)) then
         ! fourth field (e.g. for 'humidity_airtemperatur_cloudiness_solarradiation'
         fieldId_4 = ecCreateField(ecInstancePtr)
         if (success) success = ecSetField1dArray(ecInstancePtr, fieldId_4, dataPtr4)
         if (success) success = ecSetFieldMissingValue(ecInstancePtr, fieldId_4, dmiss)
         if (success) success = createItem(ecInstancePtr, targetItemPtr4, quantityId, elementSetId, fieldId_4)
         if (present(multuni4)) then ! if multiple-uni item(s) specified:
            if (multuni4 < 0) then
               multuni4 = ecInstanceCreateItem(ecInstancePtr)
               if (.not. ecSetItemRole(ecInstancePtr, multuni4, itemType_target)) return
            end if
            connectionId = ecCreateConnection(ecInstancePtr)
            if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, targetItemPtr4)) return ! connecting source to new converter
            if (.not. ecAddConnectionTargetItem(ecInstancePtr, connectionId, multuni4)) return ! connecting multuni1 as target item to the new converter
            if (.not. ecCopyItemProperty(ecInstancePtr, multuni4, targetItemPtr4, 'quantityPtr')) return ! copying the quantity pointer to the multi uni item
            if (.not. ecAddItemConnection(ecInstancePtr, multuni4, connectionId)) return ! adding the new converter to multuni1
         end if
      end if

      if (.not. success) then
         goto 1234
      end if

      ! ==========================
      ! Construct a new Converter.
      ! ==========================
      ec_convtype = ec_filetype_to_conv_type(ec_filetype, name)
      if (ec_convtype == convType_undefined) then
         call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported converter.')
         return
      end if

      converterId = ecCreateConverter(ecInstancePtr)

      select case (target_name)
      case ('shiptxy', 'movingstationtxy', 'discharge_salinity_temperature_sorsin', 'pump', 'valve1D', 'damlevel', 'gateloweredgelevel', 'generalstructure', 'lateral_discharge', 'dambreakLevelsAndWidths')
         ! for the FM 'target' arrays, the index is provided by the caller
         if (.not. present(targetIndex)) then
            message = 'Internal program error: missing targetIndex for quantity '''//trim(target_name)
            call mess(LEVEL_ERROR, message)
            return
         end if
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, operand_replace_element, ec_method)
         if (success) success = ecSetConverterElement(ecInstancePtr, converterId, targetIndex)
      case ('qhbnd')
         ! count qh boundaries
         n_qhbnd = n_qhbnd + 1
         inputptr => atqh_all(n_qhbnd)
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, operand_replace_element, interpolate_passthrough, inputptr=inputptr)
         if (success) success = ecSetConverterElement(ecInstancePtr, converterId, n_qhbnd)
         ! Each qhbnd polytim file replaces exactly one element in the target data array.
         ! Converter will put qh value in target_array(n_qhbnd)
      case ('windx', 'windy', 'windxy', 'stressxy', 'airpressure', 'atmosphericpressure', 'airpressure_windx_windy', 'airdensity', &
            'airpressure_windx_windy_charnock', 'charnock', 'airpressure_stressx_stressy', 'humidity', 'dewpoint', 'airtemperature', 'cloudiness', 'solarradiation', 'longwaveradiation')
         if (present(srcmaskfile)) then
            if (ec_filetype == provFile_arcinfo .or. ec_filetype == provFile_curvi) then
               if (.not. ecParseARCinfoMask(srcmaskfile, srcmask, fileReaderPtr)) then
                  write (msgbuf, '(3a)') 'Error while reading mask file ''', trim(srcmaskfile), '''.'
                  call err_flush()
                  return
               end if
               if (.not. initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method, srcmask=srcmask)) then
                  write (msgbuf, '(5a)') 'Error while setting mask to converter (file=''', trim(srcmaskfile), ''', associated with meteo file ''', trim(filename), '''.'
                  call err_flush()
                  return
               end if
            end if
         else
            if (ec_filetype == provFile_bc .and. target_name == 'windxy') then
               ec_convtype = convType_unimagdir
            end if
            success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         end if
      case ('rainfall')
         if (present(srcmaskfile)) then
            if (allocated(srcmask%msk)) deallocate (srcmask%msk)
            allocate (srcmask%msk(ndx))
            if (allocated(transformcoef)) deallocate (transformcoef)
            allocate (transformcoef(1))
            if (present(targetMaskSelect)) then
               if (targetMaskSelect == 'i') then
                  itargetMaskSelect = 1
               else
                  itargetMaskSelect = 0
               end if
            else
               itargetMaskSelect = 1
            end if
            if (itargetMaskSelect == 1) then
               transformcoef = 1.0d0
               srcmask%msk = 0
            else
               transformcoef = 0.0d0
               srcmask%msk = 1
            end if

            success = timespaceinitialfield_int(x, y, srcmask%msk, ndx, srcmaskfile, inside_polygon, operand, transformcoef) ! zie meteo module
            if (.not. success) then
               write (msgbuf, '(3a)') 'Error while reading mask file ''', trim(srcmaskfile), '''.'
               call err_flush()
               return
            end if
            if (.not. initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method, srcmask=srcmask)) then
               write (msgbuf, '(5a)') 'Error while setting mask to converter (file=''', trim(srcmaskfile), ''', associated with meteo file ''', trim(filename), '''.'
               call err_flush()
               return
            end if
            if (allocated(srcmask%msk)) deallocate (srcmask%msk)
            if (allocated(transformcoef)) deallocate (transformcoef)
         else
            success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         end if
      case default
         success = initializeConverter(ecInstancePtr, converterId, ec_convtype, ec_operand, ec_method)
         if (present(targetindex)) then
            success = ecSetConverterElement(ecInstancePtr, converterId, targetindex)
         end if
      end select

      if (.not. success) then
         goto 1234
      end if

      ! ================================================================
      ! Construct a new Connection, and connect source and target Items.
      ! ================================================================
      connectionId = ecCreateConnection(ecInstancePtr)

      if (.not. ecSetConnectionConverter(ecInstancePtr, connectionId, converterId)) then
         goto 1234
      end if

      ! determine the source item's name
      ! note 1: this can be determined (and be improved) when creating the file reader
      ! note 2: the source item's name is set in the select case switch below. In some cases
      !         of this switch ('special cases') the source-target connections is established
      !         immediatly, and sourceItemName is NOT set.
      !         So the generic 'connect source and target' statements after the switch are
      !         only executed if sourceItemName IS set.
      !
      sourceItemName = ' '

      sourceItemId = 0
      sourceItemId_2 = 0
      sourceItemId_3 = 0
      sourceItemId_4 = 0

      select case (target_name)
      case ('shiptxy', 'movingstationtxy', 'discharge_salinity_temperature_sorsin')
         if (checkFileType(ec_filetype, provFile_uniform, target_name)) then
            ! the file reader will have created an item called 'uniform_item'
            sourceItemName = 'uniform_item'
         else if (checkFileType(ec_filetype, provFile_bc, target_name)) then
            sourceItemName = target_name
         else
            return
         end if

      case ('pump', 'generalstructure', 'damlevel', 'valve1D', 'gateloweredgelevel', 'lateral_discharge', 'dambreakLevelsAndWidths')
         if (checkFileType(ec_filetype, provFile_uniform, target_name)) then
            !
            ! *.tim file
            !
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            if (sourceItemId == ec_undef_int) then
               ! Add something to the EC message stack about missing source item
               return
            end if
            if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return
         else if (checkFileType(ec_filetype, provFile_bc, target_name)) then
            !
            ! *.bc file
            !
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, target_name)
            if (sourceItemId == ec_undef_int) then
               ! Add something to the EC message stack about missing source item
               return
            end if
            if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return
         else if (checkFileType(ec_filetype, provFile_fourier, target_name)) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'period')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'magnitude')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'phase')
            if ((sourceItemId == ec_undef_int) .or. (sourceItemId_2 == ec_undef_int) .or. (sourceItemId_3 == ec_undef_int)) then
               ! Add something to the EC message stack about missing source item
               return
            else
               if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)) return
               if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)) return
               if (.not. ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)) return
            end if
         else if (checkFileType(ec_filetype, provFile_poly_tim, target_name)) then
            sourceItemName = 'polytim_item'
         else
            ! Add something to the EC message stack about mismatching filetype bla bla
            return
         end if
         if (.not. ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)) return
         if (.not. ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)) return
      case ('qhbnd')
         if ((.not. checkFileType(ec_filetype, provFile_poly_tim, target_name)) .and. &
             (.not. checkFileType(ec_filetype, provFile_qhtable, target_name)) .and. &
             (.not. checkFileType(ec_filetype, provFile_bc, target_name))) then
            return
         end if
         if (ec_filetype == provFile_poly_tim) then
            sourceItemName = 'polytim_item'
         else if (ec_filetype == provFile_bc .or. ec_filetype == provFile_qhtable) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'discharge')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'waterlevel')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'slope')
            sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'crossing')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)
            if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)
            if (.not. success) then
               goto 1234
            end if
         end if
      case ('velocitybnd', 'dischargebnd', 'waterlevelbnd', 'salinitybnd', 'tracerbnd', &
            'neumannbnd', 'riemannbnd', 'absgenbnd', 'outflowbnd', &
            'temperaturebnd', 'sedimentbnd', 'tangentialvelocitybnd', 'uxuyadvectionvelocitybnd', &
            'normalvelocitybnd', 'criticaloutflowbnd', 'weiroutflowbnd', 'sedfracbnd', 'riemannubnd')
         if ((.not. checkFileType(ec_filetype, provFile_poly_tim, target_name)) .and. &
             (.not. checkFileType(ec_filetype, provFile_bc, target_name))) then
            return
         end if
         if (ec_filetype == provFile_poly_tim) then
            sourceItemName = 'polytim_item'
         else if (ec_filetype == provFile_bc) then
            sourceItemName = name
            call str_upper(sourceItemName)
         end if
      case ('rainfall')
         ! the name of the source item depends on the file reader
         if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else if (ec_filetype == provFile_bc) then
            sourceItemName = 'RAINFALL'
         else if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'precipitation_amount'
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity rainfall.')
            return
         end if
         if (.not. (ecQuantitySet(ecInstancePtr, quantityId, timeint=timeint_rainfall))) return
      case ('rainfall_rate')
         ! the name of the source item depends on the file reader
         if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else if (ec_filetype == provFile_bc) then
            sourceItemName = 'RAINFALL_RATE'
         else if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'rainfall_rate'
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity rainfall_rate.')
            return
         end if
      case ('hrms', 'tp', 'tps', 'rtp', 'dir', 'fx', 'fy', 'wsbu', 'wsbv', 'mx', 'my', 'dissurf', 'diswcap', 'ubot')
         ! the name of the source item created by the file reader will be the same as the ext.force. quant name
         sourceItemName = target_name
         ! this file contains wave data
         if (jawave == 3) then
            ! wave data is read from a com.nc file produced by D-Waves which contains one time field only
            fileReaderPtr%one_time_field = .true.
         end if
      case ('wavesignificantheight', 'waveperiod', 'xwaveforce', 'ywaveforce', &
            'wavebreakerdissipation', 'whitecappingdissipation', 'totalwaveenergydissipation')
         ! the name of the source item created by the file reader will be the same as the ext.force. var name
         sourceItemName = varname
      case ('airpressure', 'atmosphericpressure')
         if (ec_filetype == provFile_arcinfo) then
            sourceItemName = 'wind_p'
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else if (ec_filetype == provFile_spiderweb) then
            sourceItemName = 'p_drop'
         else if (ec_filetype == provFile_netcdf) then
            ! the arc-info file contains 'air_pressure', which is also the standard_name
            sourceItemName = 'air_pressure'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity wind_p.')
            return
         end if
      case ('windx')
         ! the name of the source item depends on the file reader
         if (ec_filetype == provFile_arcinfo) then
            sourceItemName = 'wind_u'
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'eastward_wind'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windx.')
            return
         end if
      case ('windy')
         ! the name of the source item depends on the file reader
         if (ec_filetype == provFile_arcinfo) then
            sourceItemName = 'wind_v'
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'northward_wind'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windy.')
            return
         end if
      case ('stressx')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'surface_downward_eastward_stress'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressx only implemented for NetCDF.')
            return
         end if
      case ('stressy')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'surface_downward_northward_stress'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressy only implemented for NetCDF.')
            return
         end if
      case ('stressxy')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_eastward_stress')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_northward_stress')
            if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int) then
               goto 1234
            end if
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: stressxy only implemented for NetCDF.')
            return
         end if
         success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_stressxy_x)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_stressxy_y)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_stressxy_x, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_stressxy_y, connectionId)
      case ('charnock')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'charnock')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_charnock)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_charnock, connectionId)
      case ('friction_coefficient_time_dependent')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'friction_coefficient'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: friction_coefficient_time_dependent only implemented for NetCDF.')
            return
         end if
      case ('windxy')
         ! special case: m:n converter, (for now) handle here in case switch
         if (ec_filetype == provFile_unimagdir) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            success = (sourceItemId /= ec_undef_int)
            if (.not. success) then
               goto 1234
            end if
         else if (ec_filetype == provFile_uniform) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            success = (sourceItemId /= ec_undef_int)
            if (.not. success) then
               goto 1234
            end if
         else if (ec_filetype == provFile_bc) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'WINDXY')
            success = (sourceItemId /= ec_undef_int)
            if (.not. success) then
               goto 1234
            end if
         else if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'eastward_wind')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'northward_wind')
            success = (sourceItemId /= ec_undef_int .and. sourceItemId_2 /= ec_undef_int)
            if (.not. success) then
               goto 1234
            end if
         else if (ec_filetype == provFile_spiderweb) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'windspeed')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'winddirection')
            success = (sourceItemId /= ec_undef_int .and. sourceItemId_2 /= ec_undef_int)
            if (.not. success) then
               goto 1234
            end if
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity windxy.')
            return
         end if
         if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         if (sourceItemId_2 > 0) then
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_windxy_x)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_windxy_y)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_windxy_x, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_windxy_y, connectionId)
      case ('airpressure_windx_windy', 'airpressure_windx_windy_charnock', 'airpressure_stressx_stressy')
         withCharnock = (target_name == 'airpressure_windx_windy_charnock')
         withStress = (target_name == 'airpressure_stressx_stressy')
         ! special case: m:n converter, (for now) handle seperately
         if (ec_filetype == provFile_curvi) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
         else if (ec_filetype == provFile_spiderweb) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'windspeed')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'winddirection')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'p_drop')
         else if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_pressure')
            if (.not. withStress) then
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'eastward_wind')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'northward_wind')
            else
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_eastward_stress')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_downward_northward_stress')
            end if
            if (withCharnock) then
               sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'charnock')
               if (sourceItemId_4 == ec_undef_int) goto 1234
            end if
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
            goto 1234
         end if
         success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
         if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
         if (success .and. withCharnock) then
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
         end if
         if (ec_filetype == provFile_curvi .or. ec_filetype == provFile_netcdf) then
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_p)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_x)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_y)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_p, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_x, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_y, connectionId)
            if (withCharnock) then
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_c)
               if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_c, connectionId)
            end if
         else if (ec_filetype == provFile_spiderweb) then
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_x)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_y)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_apwxwy_p)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_x, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_y, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_apwxwy_p, connectionId)
         end if
         if (.not. success) then
            goto 1234
         end if
      case ('humidity_airtemperature_cloudiness')
         ! special case: m:n converter, (for now) handle seperately
         if (ec_filetype == provFile_curvi .or. ec_filetype == provFile_uniform .or. ec_filetype == provFile_netcdf) then
            if (ec_filetype == provFile_curvi) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
               if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
                  goto 1234
               end if
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            else if (ec_filetype == provFile_uniform) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
               if (sourceItemId == ec_undef_int) then
                  goto 1234
               end if
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            else if (ec_filetype == provFile_netcdf) then
               sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'relative_humidity')
               sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
               sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
               if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. sourceItemId_3 == ec_undef_int) then
                  goto 1234
               end if
               success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            end if
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_humidity)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_airtemperature)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hac_cloudiness)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_humidity, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_airtemperature, connectionId)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_hac_cloudiness, connectionId)
            if (.not. success) then
               goto 1234
            end if
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity humidity_airtemperature_cloudiness.')
            return
         end if
      case ('humidity_airtemperature_cloudiness_solarradiation')
         ! special case: m:n converter, (for now) handle seperately
         if (ec_filetype == provFile_curvi) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_1')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_2')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_3')
            sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'curvi_source_item_4')
            if (sourceItemId == ec_undef_int .or. sourceItemId_2 == ec_undef_int .or. &
                sourceItemId_3 == ec_undef_int .or. sourceItemId_4 == ec_undef_int) then
               goto 1234
            end if
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
         else if (ec_filetype == provFile_uniform) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            if (sourceItemId == ec_undef_int) then
               goto 1234
            end if
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         else if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'humidity')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
            sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_net_downward_shortwave_flux')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if

         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_humidity)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_airtemperature)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_cloudiness)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_hacs_solarradiation)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_humidity, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_airtemperature, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_cloudiness, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_hacs_solarradiation, connectionId)
         if (.not. success) then
            goto 1234
         end if
      case ('dewpoint_airtemperature_cloudiness')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'dew_point_temperature')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (.not. success) goto 1234
         else if (ec_filetype == provFile_uniform) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            success = (sourceItemId /= ec_undef_int)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_dewpoint)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_airtemperature)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dac_cloudiness)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_dewpoint, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_airtemperature, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dac_cloudiness, connectionId)
      case ('dewpoint_airtemperature_cloudiness_solarradiation')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'dew_point_temperature')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
            sourceItemId_3 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'cloud_area_fraction')
            sourceItemId_4 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'surface_net_downward_shortwave_flux')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
         else if (ec_filetype == provFile_uniform) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            success = (sourceItemId /= ec_undef_int)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_dewpoint)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_airtemperature)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_cloudiness)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_dacs_solarradiation)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_dewpoint, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_airtemperature, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_cloudiness, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_dacs_solarradiation, connectionId)
      case ('humidity')
         sourceItemName = 'relative_humidity'
      case ('dewpoint')
         sourceItemName = 'dew_point_temperature'
      case ('airtemperature')
         if (ec_filetype == provFile_uniform) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'uniform_item')
            if (sourceItemId == ec_undef_int) then
               goto 1234
            end if
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airtemperature)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_airtemperature, connectionId)
         elseif (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_temperature')
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airtemperature)
            if (success) success = ecAddItemConnection(ecInstancePtr, item_airtemperature, connectionId)
            if (.not. success) then
               goto 1234
            end if
         else
            sourceItemName = 'air_temperature'
         end if
      case ('cloudiness')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'cloud_area_fraction'
         else
            sourceItemName = 'cloudiness'
         end if
      case ('airdensity')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'air_density')
            success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_airdensity)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_airdensity, connectionId)
      case ('solarradiation')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = 'surface_net_downward_shortwave_flux'
         else
            sourceItemName = 'sw_radiation_flux'
         end if
      case ('longwaveradiation')
         sourceItemName = 'surface_net_downward_longwave_flux'
      case ('nudge_salinity_temperature')
         if (ec_filetype == provFile_netcdf) then
            sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'sea_water_potential_temperature')
            sourceItemId_2 = ecFindItemInFileReader(ecInstancePtr, fileReaderId, 'sea_water_salinity')
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(target_name)//'.')
            return
         end if
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_nudge_tem)
         if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, item_nudge_sal)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_nudge_tem, connectionId)
         if (success) success = ecAddItemConnection(ecInstancePtr, item_nudge_sal, connectionId)
      case ('waqfunction')
         if (.not. checkFileType(ec_filetype, provFile_uniform, target_name)) then
            return
         end if
         ! the file reader will have created an item called 'polytim_item'
         sourceItemName = 'uniform_item'
      case ('waqsegmentfunction')
         ! the name of the source item depends on the file reader
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = name
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '''//trim(name)//'''')
            return
         end if
      case ('initialtracer')
         if (ec_filetype == provFile_netcdf) then
            sourceItemName = name(14:)
         end if
      case ('bedrock_surface_elevation', 'sea_ice_area_fraction', 'sea_ice_thickness')
         if (ec_filetype == provFile_arcinfo) then
            sourceItemName = name
         else if (ec_filetype == provFile_curvi) then
            sourceItemName = 'curvi_source_item_1'
         else if (ec_filetype == provFile_netcdf) then
            sourceItemName = name
         else if (ec_filetype == provFile_uniform) then
            sourceItemName = 'uniform_item'
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported filetype for quantity '//trim(name)//'.')
            return
         end if
      case default
         fileReaderPtr => ecFindFileReader(ecInstancePtr, fileReaderId)
         if (fileReaderPtr%nitems >= 1) then
            sourceItemId = fileReaderPtr%items(1)%ptr%id
            if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId)
            if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr1)
            if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr1, connectionId)
            if (fileReaderPtr%nitems >= 2) then
               sourceItemId_2 = fileReaderPtr%items(2)%ptr%id
               if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_2)
               if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr2)
               if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr2, connectionId)
               if (fileReaderPtr%nitems >= 3) then
                  sourceItemId_3 = fileReaderPtr%items(3)%ptr%id
                  if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_3)
                  if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr3)
                  if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr3, connectionId)
                  if (fileReaderPtr%nitems >= 4) then
                     sourceItemId_4 = fileReaderPtr%items(4)%ptr%id
                     if (success) success = ecAddConnectionSourceItem(ecInstancePtr, connectionId, sourceItemId_4)
                     if (success) success = ecAddConnectionTargetItem(ecInstancePtr, connectionId, targetItemPtr4)
                     if (success) success = ecAddItemConnection(ecInstancePtr, targetItemPtr4, connectionId)
                  end if
               end if
            end if
            if (success) then
               ! all statements executed successfully ... this must be good
               ec_addtimespacerelation = .true.
            else
               call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Error while default processing of ext-file (connect source and target) for : '//trim(target_name)//'.')
            end if
         else
            call mess(LEVEL_FATAL, 'm_meteo::ec_addtimespacerelation: Unsupported quantity specified in ext-file (connect source and target): '//trim(target_name)//'.')
         end if
      end select

      if (sourceItemName /= ' ') then
         ! not a special case, connect source and target
         sourceItemId = ecFindItemInFileReader(ecInstancePtr, fileReaderId, sourceItemName)
         if (sourceItemId == ec_undef_int) then
            goto 1234
         end if
         if (.not. initializeConnection(ecInstancePtr, connectionId, sourceItemId, targetItemPtr1)) then
            goto 1234
         end if
         if (present(targetIndex)) then
            if (.not. checkVectorMax(ecInstancePtr, sourceItemId, targetItemPtr1)) then
               goto 1234
            end if
         end if
      end if

      success = ecSetConnectionIndexWeights(ecInstancePtr, connectionId)

      if (target_name == 'nudge_salinity_temperature') then
         call ecConverterGetBbox(ecInstancePtr, SourceItemID, 0, col0, col1, row0, row1, ncols, nrows, issparse, Ndatasize)
         relcol = dble(col1 - col0 + 1) / dble(ncols)
         relrow = dble(row1 - row0 + 1) / dble(nrows)
         write (txt1, "('nudge_salinity_temperature: bounding box')")
         write (txt2, "('col0-col1 X row0-row1 = ', I0, '-', I0, ' X ', I0, '-', I0, ', ncols X nrows = ', I0, ' X ', I0)") col0, col1, row0, row1, ncols, nrows
         write (txt3, "('relcol X relrow = ', F4.2, ' X ', F4.2, ' = ', F4.2)") relcol, relrow, relcol * relrow
         call mess(LEVEL_INFO, trim(txt1)//' '//trim(txt2)//', '//trim(txt3))

         if (issparse == 1) then
            write (txt1, "('sparse: data size = ', I0, ', ncols X nrows = ', I0, ' X ', I0, ' = ', I0)") Ndatasize, ncols, nrows, ncols * nrows
            write (txt2, "('factor = ', F4.2)") dble(Ndatasize) / dble(Ncols * Nrows)
            call mess(LEVEL_INFO, trim(txt1)//' '//trim(txt2))
         end if
      end if

      ec_addtimespacerelation = .true.
      return

      ! Error handling.
1234  continue
      ec_addtimespacerelation = .false.
!     message = ecGetMessage()

      if (.not. quiet_) then
         ! TODO: AvD: I'd rather have a full message stack that will combine EC + meteo + dflowfm, and any caller may print any pending messages.
         ! For now: Print the EC message stack here, and leave the rest to the caller.
         ! TODO: RL: the message below is from m_meteo::message, whereas timespace::getmeteoerror() returns timespace::errormessage. So now this message here is lost/never printed at call site.
         message = dumpECMessageStack(LEVEL_WARN, callback_msg)
         ! Leave this concluding message for the caller to print or not. (via getmeteoerror())
      end if
      message = 'm_meteo::ec_addtimespacerelation: Error while initializing '''//trim(name)//''' from file: '''//trim(filename)//''''
      if (present(forcingfile)) then
         message = trim(message)//' ('''//trim(forcingfile)//''')'
      end if

   end function ec_addtimespacerelation

end submodule m_ec_addtimespacerelation
