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

!> This module instantiates FileReaders and their source Items, just as a GUI/framework instantiates kernels and their target Items.
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author robert.leander@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_provider
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_support
   use m_ec_elementSet
   use m_ec_field
   use m_ec_item
   use m_ec_connection
   use m_ec_converter
   use m_ec_filereader
   use m_ec_filereader_read
   use m_ec_quantity
   use m_ec_bcreader
   use time_module
   use m_ec_instance
   use m_ec_message
   use m_ec_parameters
   use precision
   use string_module
   use netcdf
   use multi_file_io

   implicit none

   private

   public :: ecSetFileReaderProperties
   public :: ecProviderInitializeFileReader
   public :: ecProviderCreateInitializeBCFileReader
   public :: ecProviderCreateUniformItems
   public :: ecProviderCreateQhtableItems
   public :: ecProviderCreateTimeInterpolatedItem
   public :: ecProviderInitializeTimeFrame
   public :: items_from_bc_quantities

   interface ecSetFileReaderProperties
      module procedure ecProviderInitializeFileReader
   end interface ecSetFileReaderProperties

   public :: ecAtLeastOnePointIsCorrection ! TODO: Refactor this shortcut (UNST-180).
   logical :: ecAtLeastOnePointIsCorrection = .false. ! TODO: Refactor this shortcut (UNST-180).

contains

   ! =======================================================================

   !> Create and Initialize BC instance, yielding a file reader with items, returning the fileReaderID
   function ecProviderCreateInitializeBCFileReader(instancePtr, forcingfile, location, quantity, k_refdat, k_tzone, k_tsunit, fileReaderId, funtype) result(success)
      use m_ec_support
      implicit none
      logical :: success
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      character(len=*), intent(in) :: forcingfile
      character(len=*), intent(in) :: location
      character(len=*), intent(in) :: quantity
      real(hp), intent(in) :: k_refdat !< kernel ref date
      real(hp), intent(in) :: k_tzone !< kernel time zone
      integer, intent(in) :: k_tsunit !< kernel timestep unit (1=sec, 2=min, 3=hour)
      integer, intent(out) :: fileReaderId !< unique fileReader id
      character(len=*), optional, intent(in) :: funtype !< matching function in the BC-block header
      !
      integer :: istat
      integer :: forcingfiletype
      integer :: bcBlockId
      type(tEcBCBlock), pointer :: bcBlockPtr
      logical :: file_exists
      success = .false.

      inquire (FILE=forcingfile, EXIST=file_exists)
      if (.not. file_exists) then
         call setECMessage("Forcing file ("//trim(forcingfile)//") not found.")
         return
      end if
      if (index(trim(forcingfile)//'|', '.bc|') > 0) then ! ASCII: bc-format  : detection is extension-based
         forcingfiletype = BC_FTYPE_ASCII
      else if (index(trim(forcingfile)//'|', '.nc|') > 0) then ! NETCDF: nc-format
         forcingfiletype = BC_FTYPE_NETCDF
      else
         call setECMessage("Forcing file ("//trim(forcingfile)//") should either have extension .nc (netcdf timeseries file) or .bc (ascii BC-file).")
         return
      end if

      bcBlockId = ecInstanceCreateBCBlock(instancePtr)
      bcBlockPtr => ecSupportFindBCBlock(instancePtr, bcBlockId)
      if (.not. ecProviderInitializeBCBlock(instancePtr, bcBlockId, &
                                            k_refdat, k_tzone, k_tsunit, fileReaderId, forcingfile, forcingfiletype, quantity, location, istat, funtype=funtype)) then
         ! TODO: handle exception
         return
         continue
      end if
      success = .true.
   end function ecProviderCreateInitializeBCFileReader

   ! =======================================================================

   !> Initialize a new BCBlock item, which in turn constructs and initializes a filereader

   function ecProviderInitializeBCBlock(instancePtr, bcBlockId, k_refdat, k_tzone, k_tsunit, fileReaderId, fileName, fileType, quantityName, &
                                        plilabel, istat, dtnodal, funtype) result(success)
      use m_ec_netcdf_timeseries
      use m_ec_alloc
      implicit none
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: bcBlockId !< unique bcBlock id
      real(hp), intent(in) :: k_refdat !< kernel ref date
      real(hp), intent(in) :: k_tzone !< kernel time zone
      integer, intent(in) :: k_tsunit !< kernel timestep unit (1=sec, 2=min, 3=hour)
      integer, intent(out) :: fileReaderId !< unique fileReader id
      character(*), intent(in) :: fileName !< relative path of data file
      integer, intent(in) :: fileType !< type of the BC-file (ascii=bc, netcdf=nc)
      character(*), intent(in) :: quantityName !< name of quantity, needed for structured input files (NetCDF and BC)
      character(*), intent(in) :: plilabel !< identify a (set of) pli-points
      real(hp), optional, intent(in) :: dtnodal !< Nodal factors in astronomical bc update interval
      integer, intent(out) :: istat !< Detailed result status. \see{m_ec_parameters}.
      character(len=*), optional, intent(in) :: funtype !< Function type requested to match; the value for the keyword 'FUNCTION' in the bc-headers
      !< passing funtype narrows down the search for blocks to blocks with the requested function

      type(tEcBCBlock), pointer :: bcBlockPtr !< BCBlock corresponding to bcBlockId
      type(tEcFileReader), pointer :: fileReaderPtr !< FileReader associated with the BC instance
      integer :: iostat

      integer :: netCDFId

      success = .false.
      istat = EC_UNKNOWN_ERROR

      bcBlockPtr => ecSupportFindBCBlock(instancePtr, bcBlockId)
      if (.not. associated(bcBlockPtr)) return

      bcBlockPtr%ftype = fileType
      select case (bcBlockPtr%ftype)
      case (BC_FTYPE_ASCII)
         bcBlockPtr%bcFilePtr => ecSupportFindBCFileByFilename(instancePtr, fileName) ! was this BC-file already opened?
         if (.not. associated(bcBlockPtr%bcFilePtr)) then ! if not, create anew
            ! ensure capacity
            if (instancePtr%nBCFiles == size(instancePtr%ecBCFilesPtr)) then
               if (.not. ecArrayIncrease(instancePtr%ecBCFilesPtr, instancePtr%nBCFiles)) then
                  return
               end if
            end if
            instancePtr%nBCFiles = instancePtr%nBCFiles + 1

            allocate (bcBlockPtr%bcFilePtr)
            bcBlockPtr%bcFilePtr%bcfilename = fileName
            instancePtr%ecBCFilesPtr(instancePtr%nBCFiles)%Ptr => bcBlockPtr%bcFilePtr
         end if
      case (BC_FTYPE_NETCDF)
         !if (index(plilabel,'_')<=0) then
         !   return                                                              ! If this was not pli-label  bla_0001 then its is a qhbnd
         !endif                                                                  ! not supported in combination with netcdf-files
         ! This is something dirty, which deserves refactoring
         ! but no alternative for it as we speak
         bcBlockPtr%ncptr => ecSupportFindNetCDFByFilename(instancePtr, fileName) ! is there a netCDF instance with this file ?
         if (.not. associated(bcBlockPtr%ncptr)) then ! if not ...
            netCDFId = ecInstanceCreateNetCDF(instancePtr)
            bcBlockPtr%ncptr => ecSupportFindNetCDF(instancePtr, netCDFId)
            if (.not. ecNetCDFInit(fileName, bcBlockPtr%ncptr, iostat)) then
               bcBlockPtr%ncptr => null()
               return
            end if
         end if

         bcBlockPtr%ftype = BC_FTYPE_NETCDF
         bcBlockPtr%vptyp = bcBlockPtr%ncptr%vptyp
         if (allocated(bcBlockPtr%ncptr%vp)) then
            bcBlockPtr%vp => bcBlockPtr%ncptr%vp
            bcBlockPtr%numlay = bcBlockPtr%ncptr%nLayer
         end if
      end select

      if (.not. ecBCInit(instancePtr, filename, quantityName, plilabel, bcBlockPtr, iostat, funtype=funtype)) return

      ! Every BC block (instance) needs an associated filereader referring to it
      fileReaderId = ecInstanceCreateFileReader(instancePtr)
      fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
      if (.not. associated(fileReaderPtr)) return

      fileReaderPtr%bc => bcBlockPtr
      fileReaderPtr%filename = fileName
      fileReaderPtr%ofType = provFile_bc

      if (present(dtnodal)) then
         if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, k_refdat, k_tzone, k_tsunit, dtnodal)) return
      else
         if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, k_refdat, k_tzone, k_tsunit)) return
      end if

      select case (fileReaderPtr%bc%func)
      case (BC_FUNC_TSERIES, BC_FUNC_CONSTANT)
         success = ecProviderCreateUniformItems(instancePtr, fileReaderPtr)
      case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO)
         success = ecProviderCreateFourierItems(instancePtr, fileReaderPtr)
      case (BC_FUNC_HARMOCORR, BC_FUNC_ASTROCORR)
         success = ecApplyCorrectionToCmp(instancePtr, fileReaderPtr)
      case (BC_FUNC_QHTABLE)
         success = ecProviderCreateQhtableItems(instancePtr, fileReaderPtr)
      case (BC_FUNC_TIM3D)
         success = ecProviderCreatet3DItems(instancePtr, fileReaderPtr)
      case default
         call setECMessage("ERROR: unknown function type.") ! RL666 Todo: expand info on which file this is ...
      end select
   end function ecProviderInitializeBCBlock
   ! =======================================================================

   !> Initialize a new FileReader, by constructing the complete tree of source Items.
      !! On the opposite end of the EC-module is a kernel, which constructs the complete tree of target Items.
   recursive function ecProviderInitializeFileReader(instancePtr, fileReaderId, fileType, fileName, refdat, tzone, tsunit, quantityName, forcingFile, dtnodal, varname, varname2) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer, intent(in) :: fileReaderId !< unique FileReader id
      integer, intent(in) :: fileType !< type of data file, see provFile enumeration
      character(len=*), intent(in) :: fileName !< relative path of data file
      real(kind=hp), intent(in) :: refdat !< Kernel's reference date, format: Gregorian yyyymmdd
      real(kind=hp), intent(in) :: tzone !< Kernel's timezone.
      integer, intent(in) :: tsunit !< Kernel's timestep unit (1=sec 2=min 3=sec).
      character(len=*), optional, intent(in) :: quantityName !< name of quantity, needed for structured input files (NetCDF and BC)
      character(len=*), optional, intent(in) :: forcingFile !< name of the forcing file (if quantityName is given)
      real(kind=hp), optional, intent(in) :: dtnodal !< Nodal factors update interval
      character(len=*), optional, intent(in) :: varname !< variable name within filename
      character(len=*), optional, intent(in) :: varname2 !< variable name 2 within filename
      !
      type(tEcFileReader), pointer :: fileReaderPtr !< FileReader corresponding to fileReaderId
      character(len=:), allocatable :: l_quantityName !< local string with quantityName
      !
      success = .false.
      fileReaderPtr => null()
      !
      if (len_trim(fileName) > maxFileNameLen) then
         call setECMessage("ERROR: ec_provider::ecProviderInitializeFileReader: The filename string is too long.")
         return
      end if
      !
      fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
      if (associated(fileReaderPtr)) then
         fileReaderPtr%ofType = fileType
         fileReaderPtr%fileName = fileName
         fileReaderPtr%fileHandle = ec_undef_int ! The filereader itself has now an invalid filehandle

         if (.not. ecSupportOpenExistingFile(fileReaderPtr%fileHandle, fileReaderPtr%fileName)) return
         select case (fileReaderPtr%ofType) ! Inventory of the opened netcdf-file
         case (provFile_netcdf)
            if (.not. ecProviderNetcdfReadvars(fileReaderPtr)) then
               ! todo: error handling with message
               return
            end if
            if (size(fileReaderPtr%dim_length) >= 5) then
               if (fileReaderPtr%relndx > fileReaderPtr%dim_length(3)) then
                  call setECMessage("ERROR: ec_provider::ecProviderInitializeFileReader: Number of realizations is outside the ensemble size.")
                  return
               end if
            end if
         end select

         if (present(dtnodal)) then
            if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, refdat, tzone, tsunit, dtnodal)) return
         else
            if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, refdat, tzone, tsunit)) return
         end if

         fileReaderPtr%nItems = 0

         ! Create source Items and their contained types, based on file type and file header.

         if (present(quantityName) .and. present(varname) .and. present(varname2)) then
            l_quantityName = trim(quantityName)
            if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr, forcingFile, l_quantityName, varname, varname2)) return
         else if (present(quantityName) .and. present(varname)) then
            l_quantityName = trim(quantityName)
            if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr, forcingFile, l_quantityName, varname)) return
         else if (present(quantityName)) then
            l_quantityName = trim(quantityName)
            if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr, forcingFile, l_quantityName)) return
         else if (present(varname)) then
            if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr, varname=varname)) return
         else
            if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr)) return
         end if
      end if
      success = .true.

   end function ecProviderInitializeFileReader

   ! =======================================================================

   !> Create source Items and their contained types, based on file type and file header.
   function ecProviderCreateItems(instancePtr, fileReaderPtr, bctfilename, quantityname, varname, varname2) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      character(len=*), intent(in), optional :: quantityname !< Names of the quantities read from file, needed for structured files (NetCDF),
      !< but also for bct-file
      character(len=*), intent(in), optional :: bctfilename !< file name of bct-file with data
      character(len=*), intent(in), optional :: varname !< variable name within filename
      character(len=*), intent(in), optional :: varname2 !< variable name 2 within filename
      !
      success = .false.
      select case (fileReaderPtr%ofType)
      case (provFile_undefined)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
      case (provFile_uniform, provFile_unimagdir)
         success = ecProviderCreateUniformItems(instancePtr, fileReaderPtr)
      case (provFile_svwp)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type: svwp.")
      case (provFile_svwp_weight)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type: svwp_weight.")
      case (provFile_arcinfo)
         success = ecProviderCreateArcinfoItems(instancePtr, fileReaderPtr)
      case (provFile_spiderweb)
         success = ecProviderCreateSpiderwebItems(instancePtr, fileReaderPtr)
      case (provFile_curvi)
         success = ecProviderCreateCurviItems(instancePtr, fileReaderPtr)
      case (provFile_curvi_weight)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type: curvi_weight.")
      case (provFile_samples)
         success = ecProviderCreateSampleItems(instancePtr, fileReaderPtr)
      case (provFile_triangulationmagdir)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type: triangulation_magdir.")
      case (provFile_qhtable)
         success = ecProviderCreateQhtableItems(instancePtr, fileReaderPtr)
      case (provFile_poly_tim)
         if (present(bctfilename)) then
            if (.not. present(quantityname)) then
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: BC type, but no quantity name.")
               return
            end if
            success = ecProviderCreatePolyTimItemsBC(instancePtr, fileReaderPtr, bctfilename, quantityname)
         else
            success = ecProviderCreatePolyTimItems(instancePtr, fileReaderPtr)
         end if
      case (provFile_fourier)
         success = ecProviderCreateFourierItems(instancePtr, fileReaderPtr)
      case (provFile_grib)
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type: grib.")
      case (provFile_netcdf)
         if (present(quantityname)) then
            select case (quantityname)
            case ("ERA_Interim_Dataset")
               success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname, varname)
            case ("rainfall", &
                  "rainfall_rate", &
                  "airpressure_windx_windy", "airpressure_windx_windy_charnock", &
                  "airpressure_stressx_stressy", "charnock", &
                  "windxy", "stressxy", "windx", "windy", "stressx", "stressy", &
                  "nudge_salinity_temperature", &
                  "airpressure", "atmosphericpressure", &
                  "airtemperature", "humidity", "dewpoint", "cloudiness", &
                  "wind_speed", "wind_from_direction", &
                  "airdensity", &
                  "humidity_airtemperature_cloudiness", &
                  "humidity_airtemperature_cloudiness_solarradiation", &
                  "dewpoint_airtemperature_cloudiness", &
                  "dewpoint_airtemperature_cloudiness_solarradiation", &
                  "sea_ice_area_fraction", "sea_ice_thickness", &
                  "solarradiation", "longwaveradiation", "wavesignificantheight", &
                  "waveperiod", "wavedirection", "friction_coefficient_time_dependent", &
                  "xwaveforce", "ywaveforce", &
                  "wavebreakerdissipation", "whitecappingdissipation", "totalwaveenergydissipation")
               success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname, varname)
            case ("hrms", "tp", "tps", "rtp", "dir", "fx", "fy", "wsbu", "wsbv", "mx", "my", "dissurf", "diswcap", "ubot")
               success = ecProviderCreateWaveNetcdfItems(instancePtr, fileReaderPtr, quantityname)
            case default
               if (index(quantityName, 'waqsegmentfunction') == 1) then
                  success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname, varname)
               else if (index(quantityName, 'initialtracer') == 1) then
                  success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname, varname)
               else
                  call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported quantity name '" &
                                    //trim(quantityname)//"', file='"//trim(fileReaderPtr%filename)//"'.")
                  return
                  ! TODO: user defined quantity name
                  !success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname, varname)
               end if
            end select
         else
            call setECMessage("ERROR: ec_provider::ecProviderCreateItems: NetCDF requires a quantity name.")
         end if
      case (provFile_t3D)
         success = ecProviderCreatet3DItems(instancePtr, fileReaderPtr)
      case default
         call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unknown file type.")
      end select
   end function ecProviderCreateItems

   ! =======================================================================

   !> Create source Items and their contained types, from a qh-table file.
   function ecProviderCreateQhtableItems(instancePtr, fileReaderPtr, use_std_names) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      logical, optional :: use_std_names !< us
      !
      real(hp), dimension(:), allocatable :: discharges !< the table's discharge values
      real(hp), dimension(:), allocatable :: waterlevels !< the table's water level values
      integer :: nr_rows !< the number of rows in the table
      integer :: itemId !< helper variable
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      type(tEcItem), pointer :: item_discharge !< Item
      type(tEcItem), pointer :: item_waterlevel !< Item
      type(tEcItem), pointer :: item_slope !< Item
      type(tEcItem), pointer :: item_crossing !< Item
      integer :: i !< loop counter
      integer :: n1, n2 !< helper variables
      character(len=:), allocatable :: elementSetName
      character(len=maxNameLen) :: quantityName
      character(len=maxMessageLen) :: msgstr

      !
      success = .true.
      item_discharge => null()
      item_waterlevel => null()
      item_slope => null()
      item_crossing => null()
      n1 = 12345 ! arbitrary large number

      ! Find elementset name (=location name), later store it into the elementset
      select case (fileReaderPtr%ofType)
      case (provFile_qhtable)
         elementSetName = fileReaderPtr%fileName
         if (index(elementSetName, '.') > 0) then
            elementSetName = elementSetName(1:index(elementSetName, '.'))
         end if
      case (provFile_bc)
         elementSetName = fileReaderPtr%bc%bcname
      end select

      ! Determine the number of rows and read the data.
      success = ecQhtableReadAll(fileReaderPtr, discharges, waterlevels, nr_rows)
      if (.not. success) return
      ! Create the item 'discharge'.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      quantityName = 'discharge'
      if (present(use_std_names)) then
         if (use_std_names) then
            quantityName = 'water_discharge'
         end if
      end if
      if (.not. (ecQuantitySet(instancePtr, quantityId, name=quantityName))) then
         success = .false.
      end if

      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName) .and. &
                 ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                 ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
         success = .false.
      end if

      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item_discharge => ecSupportFindItem(instancePtr, itemId)
      end if
      ! Create the item 'waterlevel'.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      quantityName = 'waterlevel'
      if (present(use_std_names)) then
         if (use_std_names) then
            quantityName = 'water_level'
         end if
      end if
      if (.not. (ecQuantitySet(instancePtr, quantityId, name=quantityName))) then
         success = .false.
      end if
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName) .and. &
                 ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                 ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item_waterlevel => ecSupportFindItem(instancePtr, itemId)
      end if
      ! Create the item 'slope'.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='slope'))) then
         success = .false.
      end if
      if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName) .and. &
                 ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                 ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item_slope => ecSupportFindItem(instancePtr, itemId)
      end if
      !
      ! Create the item 'crossing'.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='crossing'))) then
         success = .false.
      end if
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName) .and. &
                 ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                 ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item_crossing => ecSupportFindItem(instancePtr, itemId)
      end if
      ! ===== finish initialization of Fields =====
      ! This FileReader has constant values, so we store them in T0.
      ! The values in T1 are never used, so to comply with the uniform EC-module mechanism, it is set to a large, increasable value.
      ! Rewind the file
      if (success) then
         item_slope%sourceT0FieldPtr%arr1dPtr = 0.0_hp
         item_crossing%sourceT0FieldPtr%arr1dPtr = 0.0_hp
         do i = 1, nr_rows
            ! initialize Field T0
            item_discharge%sourceT0FieldPtr%arr1dPtr(i) = discharges(i)
            item_waterlevel%sourceT0FieldPtr%arr1dPtr(i) = waterlevels(i)
            n2 = i - 1
            if (i > 1) then
               if (discharges(i) > 0 .and. discharges(i) <= discharges(n2)) then
                  write (msgstr, '(a,i0,a,i0,a,f0.5,a,f0.5)') "  On rows ", i, " and ", n2, ":", discharges(i), " >= ", discharges(n2)
                  call setECMessage(msgstr)
                  call setECMessage("  "//trim(fileReaderPtr%bc%fname)//", location = "//trim(fileReaderPtr%bc%bcname))
                  call setECMessage("First column in QH-table should be strictly increasing if negative.")
                  success = .false.
                  return
               end if
               if (discharges(i) < 0 .and. discharges(i) >= discharges(n2)) then
                  write (msgstr, '(a,i0,a,i0,a,f0.5,a,f0.5)') "  On rows ", i, " and ", n2, ":", discharges(i), " >= ", discharges(n2)
                  call setECMessage(msgstr)
                  call setECMessage("  "//trim(fileReaderPtr%bc%fname)//", location = "//trim(fileReaderPtr%bc%bcname))
                  call setECMessage("First column in QH-table should be strictly decreasing if negative.")
                  success = .false.
                  return
               end if
               item_slope%sourceT0FieldPtr%arr1dPtr(n2) = (waterlevels(i) - waterlevels(n2)) / (discharges(i) - discharges(n2))
               item_crossing%sourceT0FieldPtr%arr1dPtr(n2) = waterlevels(n2) - item_slope%sourceT0FieldPtr%arr1dPtr(n2) * discharges(n2)
            end if
         end do
         item_discharge%sourceT0FieldPtr%timesteps = 0.0_hp
         item_waterlevel%sourceT0FieldPtr%timesteps = 0.0_hp
         item_slope%sourceT0FieldPtr%timesteps = 0.0_hp
         item_crossing%sourceT0FieldPtr%timesteps = 0.0_hp
         ! initialize Field T1 (arbitrary far future)
         item_discharge%sourceT1FieldPtr%timesteps = 10000.0_hp
         item_waterlevel%sourceT1FieldPtr%timesteps = 10000.0_hp
         item_slope%sourceT1FieldPtr%timesteps = 10000.0_hp
         item_crossing%sourceT1FieldPtr%timesteps = 10000.0_hp
      end if
      ! Add successfully created source Items to the FileReader

      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_discharge%id)
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_waterlevel%id)
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_slope%id)
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_crossing%id)
      ! finalize
      if (allocated(discharges)) deallocate (discharges)
      if (allocated(waterlevels)) deallocate (waterlevels)
   end function ecProviderCreateQhtableItems

   ! =======================================================================

   !> Create source Items and their contained types, based on Fourier file header.
   function ecProviderCreateFourierItems(instancePtr, fileReaderPtr) result(success)
      use m_ec_message
      implicit none
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      integer :: i !< loop counter
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      integer :: istat !< error status
      !
      integer :: nPeriods !< number of periods
      type(tEcItem), pointer :: itemPeriod !< Item containing the Fourier period
      type(tEcItem), pointer :: itemMagnitude !< Item containing the Fourier magnitude
      type(tEcItem), pointer :: itemPhase !< Item containing the Fourier phase
      real(hp), dimension(:), allocatable :: periods !< Fourier components transformed into periods
      character(len=8), dimension(:), allocatable :: components !< Astro component names read from file
      real(hp), dimension(:), allocatable :: magnitudes !< seed values for the magnitudes of the Fourier components
      real(hp), dimension(:), allocatable :: phases !< seed values for the phases of the Fourier components
      !
      success = .true.
      itemPeriod => null()
      itemMagnitude => null()
      itemPhase => null()
      !
      if (ecFourierReadAll(fileReaderPtr, periods, components, magnitudes, phases, nPeriods)) then
         ! ===== all periods =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. ecQuantitySet(instancePtr, quantityId, name='period', units='minute')) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            itemPeriod => ecSupportFindItem(instancePtr, itemId)
            if (allocated(components)) then
               if (allocated(itemPeriod%sourceT0FieldPtr%astro_components)) then
                  deallocate (itemPeriod%sourceT0FieldPtr%astro_components)
               end if
               allocate (itemPeriod%sourceT0FieldPtr%astro_components(nPeriods))
               if (allocated(itemPeriod%sourceT1FieldPtr%astro_components)) then
                  deallocate (itemPeriod%sourceT1FieldPtr%astro_components)
               end if
               allocate (itemPeriod%sourceT1FieldPtr%astro_components(nPeriods))
            end if
         end if
         ! ===== magnitude =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySet(instancePtr, quantityId, name='magnitude', units='m'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            itemMagnitude => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== phase =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySet(instancePtr, quantityId, name='phase', units='degree'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            itemPhase => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== finish initialization of Fields =====
         ! This FileReader calculates it's values, rather then reading them in, so we store the seed values in T0 and T1.
         ! The values in T1 are used to cache the original values, while the values in T0 are updated with asc every dtnodal cycle.
         ! The actual calculation is performed by the Converter.
         if (success) then
            istat = 0
            do i = 1, nPeriods
               ! initialize Field T0
               if (allocated(periods)) then
                  itemPeriod%sourceT0FieldPtr%arr1dPtr(i) = periods(i)
                  itemPeriod%sourceT1FieldPtr%arr1dPtr(i) = periods(i)
               end if
               itemMagnitude%sourceT0FieldPtr%arr1dPtr(i) = magnitudes(i)
               itemMagnitude%sourceT1FieldPtr%arr1dPtr(i) = magnitudes(i)
               itemPhase%sourceT0FieldPtr%arr1dPtr(i) = phases(i)
               itemPhase%sourceT1FieldPtr%arr1dPtr(i) = phases(i)

               if (allocated(components)) then
                  itemPeriod%sourceT0FieldPtr%astro_components(i) = components(i)
                  itemPeriod%sourceT1FieldPtr%astro_components(i) = components(i)
               end if
            end do

            if (istat /= 0) then
               if (associated(fileReaderPtr%bc)) then
                  call setECMessage("Error in file "//trim(fileReaderPtr%bc%fname))
               else
                  call setECMessage("Error in file "//trim(fileReaderPtr%fileName))
               end if

               success = .false.
               return
            end if
            itemPeriod%sourceT0FieldPtr%timesteps = 0.0_hp
            itemMagnitude%sourceT0FieldPtr%timesteps = 0.0_hp
            itemPhase%sourceT0FieldPtr%timesteps = 0.0_hp
            itemPeriod%sourceT1FieldPtr%timesteps = 0.0_hp
            itemMagnitude%sourceT1FieldPtr%timesteps = 0.0_hp
            itemPhase%sourceT1FieldPtr%timesteps = 0.0_hp
         end if
         ! Add successfully created source Items to the FileReader
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPeriod%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemMagnitude%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPhase%id)
      else
         success = .false.
      end if
   end function ecProviderCreateFourierItems

   ! =======================================================================

   !> Create source Item and its contained types, for timeseries source data,
      !! which may come from a time series file ('uniform'), or a BC-file block.
      !! Uniform file records (rows) are treated as vector quantities, reading all physical quantities (columns) into a single Item.
   function ecProviderCreateUniformItems(instancePtr, fileReaderPtr) result(success)
      use m_ec_message
      implicit none
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      character(len=:), allocatable :: rec !< first data line in file
      integer :: n_quantities !< number of quantities in the file
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      type(tEcItem), pointer :: item !< Item containing all components
      character(len=:), allocatable :: elementSetName
      character(len=:), allocatable :: quantityName
      !
      success = .false.
      item => null()
      !
      select case (fileReaderPtr%ofType)
      case (provFile_uniform, provFile_unimagdir)
         rec = ecUniReadFirstLine(fileReaderPtr)
         n_quantities = count_words(rec) - 1
      case (provFile_bc)
         n_quantities = fileReaderPtr%bc%quantity%vectormax
      end select
      ! time [minute], quantities, elementsetname
      quantityId = ecInstanceCreateQuantity(instancePtr)
      select case (fileReaderPtr%ofType)
      case (provFile_uniform, provFile_unimagdir)
         if (.not. ecQuantitySet(instancePtr, quantityId, name='uniform_item')) then
            return
         end if
         elementSetName = fileReaderPtr%fileName
         if (index(elementSetName, '.') > 0) then
            elementSetName = elementSetName(1:index(elementSetName, '.'))
         end if
      case (provFile_bc)
         quantityName = fileReaderPtr%bc%quantity%name
         if (.not. ecQuantitySet(instancePtr, quantityId, name=quantityName)) then
            return
         end if
         if (.not. ecQuantitySetTimeint(instancePtr, quantityId, fileReaderPtr%bc%timeint, &
                                        periodic=fileReaderPtr%bc%periodic, &
                                        constant=(fileReaderPtr%bc%func == BC_FUNC_CONSTANT))) then
            return
         end if
         elementSetName = fileReaderPtr%bc%bcname
      end select

      ! N_quantities number of scalar quantities.
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName))) then
         return
      end if
      if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_scalar))) then
         return
      end if
      ! N_quantities scalars in a Field array.
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_quantities))) then
         return
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_quantities))) then
         return
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. ecItemSetRole(instancePtr, itemId, itemType_source)) return
      if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
      if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
      if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
      if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id)) return
      if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id)) return
      item => ecSupportFindItem(instancePtr, itemId)

      ! ===== finish initialization of Fields =====
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.

      select case (fileReaderPtr%ofType)
      case (provFile_uniform, provFile_unimagdir)
         rewind (unit=fileReaderPtr%fileHandle)
         if (.not. ecUniReadBlock(fileReaderPtr, item%sourceT0FieldPtr%timesteps, item%sourceT0FieldPtr%arr1dPtr)) return
         if (.not. ecUniReadBlock(fileReaderPtr, item%sourceT1FieldPtr%timesteps, item%sourceT1FieldPtr%arr1dPtr)) return
      case (provFile_bc)
         item%sourceT0FieldPtr%timesteps = 54321.0d+10
         if (.not. ecBCReadBlock(fileReaderPtr, item%sourceT0FieldPtr%timesteps, item%sourceT0FieldPtr%arr1dPtr)) then
            call setECMessage("Failed reading timelevel 0 from file '" &
                              //trim(fileReaderPtr%bc%fname)//"', location:'"//trim(fileReaderPtr%bc%bcname)//"'.")
            return
         end if
         item%sourceT1FieldPtr%arr1dPtr = item%sourceT0FieldPtr%arr1dPtr
         if (fileReaderPtr%bc%func /= BC_FUNC_CONSTANT) then
            ! read second line for T1-Field
            if (.not. ecBCReadBlock(fileReaderPtr, item%sourceT1FieldPtr%timesteps, item%sourceT1FieldPtr%arr1dPtr)) then
               if (.not. fileReaderPtr%bc%timeint == BC_TIMEINT_LIN_EXTRAPOL) then
                  call setECMessage("Failed reading timelevel 1 from file '" &
                                    //trim(fileReaderPtr%bc%fname)//"', location:'"//trim(fileReaderPtr%bc%bcname)//"'.")
                  return
               end if
            end if
         end if
      end select
      ! Add successfully created source Item to the FileReader
      if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)) return
      item%quantityPtr%vectorMax = n_quantities
      success = .true.
   end function ecProviderCreateUniformItems

   ! =======================================================================

   !> Create source Items and their contained types, based on Arcinfo file header.
   function ecProviderCreateArcinfoItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      integer :: n_cols !< number of columns
      integer :: n_rows !< number of rows
      real(hp) :: x0 !< seed x-coordinate
      real(hp) :: y0 !< seed y-coordinate
      real(hp) :: dxa !< step size in x
      real(hp) :: dya !< step size in y
      real(hp) :: dmiss !< missing data value
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      type(tEcItem), pointer :: item !< ec item
      character(len=maxFileNameLen) :: lc_filename !< filename (lowercase)
      !
      success = .true.
      item => null()
      ! Read relevant information from the first arcinfo file's header.
      if (readarcinfoheader(fileReaderPtr%fileHandle, n_cols, n_rows, x0, y0, dxa, dya, dmiss)) then
         ! One common ElementSet.
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian_equidistant) .and. &
                    ecElementSetSetX0Dx(instancePtr, elementSetId, x0, dxa) .and. &
                    ecElementSetSetY0Dy(instancePtr, elementSetId, y0, dya) .and. &
                    ecElementSetSetRowsCols(instancePtr, elementSetId, n_rows, n_cols))) then
            success = .false.
         end if
         !
         lc_filename = fileReaderPtr%fileName
         call str_lower(lc_filename, maxFileNameLen)
         if (index(lc_filename, '.amu') /= 0) then
            ! ===== quantity: wind component u (usually == x) =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='wind_u', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.amv') /= 0) then
            ! ===== quantity: wind component v (usually == y) =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='wind_v', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.amp') /= 0) then
            ! ===== quantity: wind component p =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='wind_p', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.amh') /= 0) then
            ! ===== quantity: rhum
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='relative_humidity', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.amt') /= 0) then
            ! ===== quantity: tair
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='air_temperature', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.amc') /= 0) then
            ! ===== quantity: clou
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='cloudiness', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if

         else if (index(lc_filename, '.ams') /= 0) then
            ! ===== quantity: solrad
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='sw_radiation_flux', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if

         else if (index(lc_filename, '.amr') /= 0) then
            ! ===== quantity: rainfall =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='rainfall', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.sdu') /= 0) then
            ! ===== quantity: bedrock surface timeseries =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='bedrock_surface_elevation', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.aice') /= 0) then
            ! ===== quantity: sea ice area fraction timeseries =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='sea_ice_area_fraction', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else if (index(lc_filename, '.hice') /= 0) then
            ! ===== quantity: sea ice thickness timeseries =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySet(instancePtr, quantityId, name='sea_ice_thickness', &
                                     units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
               success = .false.
            end if
         else
            call setECMessage('extension not recognized in '//trim(fileReaderPtr%fileName))
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols * n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field0Id, dmiss))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols * n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field1Id, dmiss))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item => ecSupportFindItem(instancePtr, itemId)
         end if
      else
         success = .false. ! failed reading header
      end if
      ! ===== finish initialization of Fields =====
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
      if (success) then
         success = ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)
      end if
      if (success) then
         success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, n_cols, n_rows, item)
      end if
      if (success) then
         success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1, n_cols, n_rows, item)
      end if
      ! Add successfully created source Item to the FileReader
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)
   end function ecProviderCreateArcinfoItems

   ! =======================================================================

   !> Construct an ElementSet from a curvilinear grid file.
      !! meteo1: reaarc_curv_tim
   function ecProviderCreateCurviElementSet(instancePtr, fileReaderPtr, elementSetId, n_cols, n_rows) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      integer, intent(out) :: elementSetId !< if of new ElementSet
      integer, intent(out) :: n_cols, n_rows
      !
      character(len=:), allocatable :: rec !< a read line
      character(len=maxFileNameLen) :: grid_file !< file name of curvilinear grid
      integer :: minp !< IO unit number
      integer :: mx, grid_width !< n_clos, n_rows
      real(hp), dimension(:), allocatable :: x, y !< coordinate arrays
      integer :: i, j !< loop counters
      character(len=10) :: dummy !< helper variable for ignored data
      integer :: istat !< status of operation
      integer :: elmSetType !< spherical (elmSetType_spheric) or not (elmSetType_Cartesian)
      !
      success = .false.
      ! Find and open the curvilinear grid file.
      grid_file = adjustl(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'grid_file'))
      success = ecSupportOpenExistingFile(minp, grid_file)
      if (.not. success) return
      ! Read the file header.
      !
      ! There is the RGF4.0 format description, and there is reality in the testbench:
      ! -There can be no comments, or comments
      ! -There can be RGF3.0 files or older with the SPHERICAL keyword in a comment
      ! -There can be no, 1, or 2 keywords
      elmSetType = elmSetType_Cartesian
20    call GetLine(minp, rec, istat)
      !
      ! handle comment lines
      if (index(rec, '*') > 0) then ! read comment lines where spherical might occur

         ! Backwards compatible: first line could contain spherical keyword
         if (index(rec, 'Spherical') >= 1 .or. &
             index(rec, 'SPHERICAL') >= 1) then
            ! grid has spherical coordinates.
            elmSetType = elmSetType_spheric
         end if

         goto 20
         ! no comment, but keyword
      elseif (index(rec, '=') >= 1) then
         if (index(rec, 'Spherical') >= 1 .or. &
             index(rec, 'SPHERICAL') >= 1) then
            ! grid has spherical coordinates.
            elmSetType = elmSetType_spheric
         end if
         !
         if (index(rec, 'Missing') >= 1) then
            ! do nothing
         end if
         goto 20
         ! Comments and keywords done, read ncols and nrows from last record read
      else
         read (rec, *) mx, grid_width
         read (minp, *) ! skips line with 0 0 0
      end if

      ! Read the file body.
      allocate (x(mx * grid_width))
      allocate (y(mx * grid_width))
      ! Read the data row-by-row into a 1D array.
      do j = 0, grid_width - 1
         read (minp, *, iostat=istat) dummy, dummy, (x(j * mx + i), i=1, mx)
      end do
      do j = 0, grid_width - 1
         read (minp, *, iostat=istat) dummy, dummy, (y(j * mx + i), i=1, mx)
      end do
      close (minp)
      if (istat /= 0) then
         call setECMessage("ERROR: ec_provider::ecProviderCreateCurviElementSet: Unable to read data block from curvilinear grid file.")
         success = .false.
         return
      end if
      !
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType) .and. &
                 ecElementSetSetXArray(instancePtr, elementSetId, x) .and. &
                 ecElementSetSetYArray(instancePtr, elementSetId, y) .and. &
                 ecElementSetSetRowsCols(instancePtr, elementSetId, grid_width, mx))) then
         success = .false.
      end if
      deallocate (x)
      deallocate (y)
      n_cols = mx
      n_rows = grid_width
   end function ecProviderCreateCurviElementSet

   ! =======================================================================

   !> Create source Items with their contained types, based on curvilinear file header.
      !! metteo1: reaarc_curv_tim
   function ecProviderCreateCurviItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      integer :: quantityId !< id of new Quantity
      integer :: elementSetId !< id of new ElementSet
      integer :: field0Id !< id of new Field
      integer :: field1Id !< id of new Field
      integer :: itemId !< id of new Item
      character(len=:), allocatable :: rec !< a read line
      real(hp) :: missingValue !< helper variable
      integer :: n_cols, n_rows
      integer :: n_quantity
      integer :: i
      character(len=1) :: postfix
      !
      success = .false.
      !
      ! ==================================
      ! Interrogate the curvi file header.
      ! ==================================
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'NODATA_value')
      if (len_trim(rec) /= 0) then
         read (rec, *) missingValue
      else
         missingValue = -9999.0_hp
      end if
      !
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_quantity')
      if (len_trim(rec) == 0) then
         call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: Missing keyword n_quantity in file header: "//trim(fileReaderPtr%fileName))
         return
      end if
      read (rec, *) n_quantity
      if (n_quantity < 1) then
         call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: Less then one quantity specified in header: "//trim(fileReaderPtr%fileName))
         return
      end if
      if (n_quantity > 9) then
         call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: More then nine quantities specified in header: "//trim(fileReaderPtr%fileName))
         return
      end if
      !
      ! ============================================================
      ! Construct a source Item for each quantity in the curvi file.
      ! ============================================================
      ! Use a common ElementSet for the Items.
      if (.not. ecProviderCreateCurviElementSet(instancePtr, fileReaderPtr, elementSetId, n_cols, n_rows)) return
      !
      do i = 1, n_quantity
         write (postfix, '(I1)') i
         ! Quantity.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. ecQuantitySet(instancePtr, quantityId, name='curvi_source_item_'//postfix)) return
         ! Field for T0.
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. ecFieldCreate1dArray(instancePtr, field0Id, n_cols * n_rows)) return
         if (.not. ecFieldSetMissingValue(instancePtr, field0Id, missingValue)) return
         ! Field for T1.
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. ecFieldCreate1dArray(instancePtr, field1Id, n_cols * n_rows)) return
         if (.not. ecFieldSetMissingValue(instancePtr, field1Id, missingValue)) return
         ! Item.
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ecItemSetRole(instancePtr, itemId, itemType_source)) return
         if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
         if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
         if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
         if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id)) return
         if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id)) return
         ! Add successfully created source Items to the FileReader.
         if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemId)) return
      end do
      ! ===== finish initialization of Fields =====
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
      if (.not. ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)) return
      if (.not. ecCurviReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0)) return
      if (.not. ecCurviReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1)) return
      !
      success = .true.
   end function ecProviderCreateCurviItems

   ! =======================================================================

   !> Create source Items and their contained types, based on the 'sample' file format.
   function ecProviderCreateSampleItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      real(hp), dimension(:), allocatable :: xs
      real(hp), dimension(:), allocatable :: ys
      real(hp), dimension(:, :), allocatable :: zs

      integer :: nSamples !< number of samples
      integer :: kx !< vector max of values at sample points

      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      type(tEcItem), pointer :: item
      integer :: i !< loop counter
      !
      success = .false.
      item => null()

      if (.not. ecSampleReadAll(fileReaderPtr, xs, ys, zs, nSamples, kx)) return

      ! Construct the samples Item.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='samples_item'))) return

      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_samples)) return
      if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
      if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
      if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nSamples)) return

      field0Id = ecInstanceCreateField(instancePtr)
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, kx * nSamples))) return

      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. ecItemSetRole(instancePtr, itemId, itemType_source)) return
      if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
      if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
      if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
      if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id)) return
      if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id)) return

      item => ecSupportFindItem(instancePtr, itemId)
      item%quantityPtr%vectorMax = kx

      do i = 1, nSamples
         ! initialize Field T1 (only one containing sample data)
         item%sourceT1FieldPtr%arr1dPtr((i - 1) * kx + 1:i * kx) = zs(1:kx, i)
      end do

      item%sourceT0FieldPtr%timesteps = 0.0_hp
      item%sourceT1FieldPtr%timesteps = huge(0.0_hp)

      if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)) return

      success = .true.
   end function ecProviderCreateSampleItems

   ! =======================================================================

   !> Create source Items with their contained types, based on t3D file header.
   function ecProviderCreatet3DItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      integer :: quantityId !< id of new Quantity
      integer :: elementSetId !< id of new ElementSet
      integer :: field0Id !< id of new Field
      integer :: field1Id !< id of new Field
      integer :: itemId !< id of new Item
      integer :: vptyp !< type of layer
      integer :: zInterpolationType !< vertical interpolation type
      type(tEcItem), pointer :: valueptr !< Item containing z/sigma-dependent values
      type(tEcBCBlock), pointer :: bcptr
      character(len=:), allocatable :: rec !< a read line
      integer, parameter :: MAXSTRLEN = 128 !<
      integer, parameter :: MAXLAY = 256 !<
      integer :: numlay, i !<
      real(hp), dimension(:), allocatable :: xws !< x-values
      real(hp), dimension(:), allocatable :: yws !< y-values
      real(hp), dimension(:), allocatable :: zws !< z-values of vertical velocities
      real(hp), dimension(MAXLAY) :: a !<

      integer :: vectormax, iostat
      !
      success = .false.
      valueptr => null()
      vectormax = 1 ! assumed scalar if vector dimensions not made explicit
      zInterpolationType = zinterpolate_unknown
      select case (fileReaderPtr%ofType)
      case (provFile_t3D)
         zInterpolationType = zinterpolate_linear
         a = ec_undef_hp
         !
         ! check if these are z-layers or sigma-layers
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'LAYER_TYPE')
         if (len_trim(rec) == 0) then
            call setECMessage("Unable to find LAYER_TYPE in file header.")
            return
         end if
         !
         call str_lower(rec)
         if (index(rec, 'sigma') /= 0) then
            vptyp = BC_VPTYP_PERCBED
         else if (index(rec, 'z') /= 0) then
            vptyp = BC_VPTYP_ZBED
         else
            call setECMessage("Invalid LAYER_TYPE specified in header.")
            return
         end if
         !
         ! Acquire the number of layers by the number of readable columns in LAYERS field of header
         rec = ect3DFindInFile(fileReaderPtr%fileHandle, 'LAYERS')
         if (len_trim(rec) == 0) then
            call setECMessage("Unable to find LAYER in file header.")
            return
         end if
         !
         read (rec, *, end=666) (a(i), i=1, MAXLAY)
666      continue

         ! Acquire the number of vector elements (i.e. vectormax) from the header in case it is specified explicitly
         rec = ect3DFindInFile(fileReaderPtr%fileHandle, 'VECTORMAX')
         if (len_trim(rec) > 0) then
            read (rec, *, iostat=iostat) vectormax
         end if

         !
         numlay = 0
         do i = 1, MAXLAY
            if (a(i) /= ec_undef_hp) then
               numlay = numlay + 1
            end if
         end do
         !
         ! allocate x and y array of size 1, currently not used, but its size is inquired lateron
         allocate (xws(1))
         allocate (yws(1))
         ! allocate vertical mesh array
         if (numlay > 0) then
            allocate (zws(numlay))
         else
            call setECMessage("No LAYERS found in header")
            return
         end if
         !
!           xws = (/ (-1.0) /)
!           yws = (/ (-1.0) /)
         zws = (/(a(i), i=1, numlay)/)
         rewind (fileReaderPtr%fileHandle)

      case (provFile_bc)
         ! Check if these are z-layers or sigma-layers
         if (.not. associated(fileReaderPtr%bc)) then
            call setECMessage("BC-filetype, but no bc instance associated to filereader")
            return
         end if
         bcptr => fileReaderPtr%bc
         vptyp = bcptr%vptyp
         zInterpolationType = bcptr%zInterpolationType
         numlay = bcptr%numlay
         vectormax = bcptr%quantity%vectormax

         ! Prepare the z-coordinates for the levels in zws(dimension=numlay)
         ! Corresponds with the SELECTED vertical positions in vp(:)
         ! allocate x and y array of size 1, currently not used, but its size is inquired lateron
         allocate (xws(1))
         allocate (yws(1))
         ! allocate vertical mesh array
         if (numlay > 0) then
            allocate (zws(numlay))
         else
            call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: no layers found in header")
            return
         end if
         xws = (/(-1.0)/)
         yws = (/(-1.0)/)
         zws = (/(bcptr%vp(i), i=1, numlay)/)

      case default
         call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: Unknown file type.")
         return
      end select
      !
      ! ===== single quantity: quant ===== (Further code in this sub independent of file type)
      quantityId = ecInstanceCreateQuantity(instancePtr)

      if (.not. ecQuantitySet(instancePtr, quantityId, name='uniform_item', units=' ', vectormax=vectormax)) return

      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
      if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xws)) return
      if (.not. ecElementSetSetYArray(instancePtr, elementSetId, yws)) return
      if (.not. ecElementSetSetZArray(instancePtr, elementSetId, zws)) return
!        if (.not.ecElementSetSetVType(instancePtr, elementSetId, vptyp))           return
!        if (.not.ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, 1))      return
      if (.not. ecElementSetSetProperties(instancePtr, elementSetId, vptyp=vptyp, &
                                          ncoords=1)) return

      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, numlay * vectormax))) return
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, numlay * vectormax))) return
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. ecItemSetRole(instancePtr, itemId, itemType_source)) return
      if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
      if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
      if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
      if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id)) return
      if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id)) return
      valueptr => ecSupportFindItem(instancePtr, itemId)

      ! ===== finish initialization of Fields =====
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
      select case (fileReaderPtr%ofType)
      case (provFile_t3D)
         if (.not. ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, numlay * vectormax, 1, valueptr)) return
         if (.not. ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1, numlay * vectormax, 1, valueptr)) return
      case (provFile_bc)
         if (.not. ecBCReadLine(fileReaderPtr, valueptr%sourceT0FieldPtr%arr1dPtr, valueptr%sourceT0FieldPtr%timesteps)) return
         if (.not. ecBCReadLine(fileReaderPtr, valueptr%sourceT1FieldPtr%arr1dPtr, valueptr%sourceT1FieldPtr%timesteps)) return
      case default
         call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: Unknown file type.")
         return
      end select

      ! In bc-files only, time-interpolation properties for T3D data can be customized
      if (fileReaderPtr%ofType == provFile_bc) then
         if (.not. ecQuantitySetTimeint(instancePtr, quantityId, fileReaderPtr%bc%timeint, &
                                        periodic=fileReaderPtr%bc%periodic, &
                                        constant=(fileReaderPtr%bc%func == BC_FUNC_CONSTANT))) then
            return
         end if
      end if

      ! Assign vertical interpolation type
      valueptr%quantityPtr%zInterpolationType = zInterpolationType

      ! Add successfully created source Items to the FileReader
      if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, valueptr%id)) return
      success = .true.
   end function ecProviderCreatet3DItems

   !> Create subproviders, which create source Items and their contained types.
      !! meteo1.f90: read1polylin
   function ecProviderCreatePolyTimItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)

      !
      real(hp), dimension(:), allocatable :: xs !< x-coordinates of support points
      real(hp), dimension(:), allocatable :: ys !< y-coordinates of support points
      integer, dimension(:), allocatable :: mask !< support point mask array (for polytime ElementSet)
      integer :: n_points !< number of support points
      integer :: n_signals !< Number of forcing signals created (at most n_signals==n_points, but warn if n_signals==0)
      character(len=:), allocatable :: rec !< a read line
      integer :: i !< loop counters
      integer :: istat !< status of read operation
      integer :: L !< helper index
      character(len=maxFileNameLen) :: filename !< helper string containing subprovider file name.
      character(len=maxFileNameLen) :: plipointlbl !< temporary name of current pli-point in bct context
      character(len=maxFileNameLen), &
      &   dimension(:), allocatable :: plipointlbls !< user-specified name for all pli-point in bct context
      character(len=maxFileNameLen) :: polyline_name !< polyline name read from pli-file
      logical :: exists !< helper boolean, indicating file existence
      integer :: id !< dummy, catches ids which are not used
      integer :: quantityId, elementSetId, fieldId, itemId
      integer :: maxLay
      type(tEcItem), pointer :: itemPT
      type(tEcItem), pointer :: itemt3D
      type(tEcItem), pointer :: sourceItem
      integer, dimension(:), allocatable :: itemIDList
      integer :: vectormax

      integer :: signaltype
      logical :: has_label
      integer :: lblstart
      type(tEcFileReader), pointer :: fileReaderPtr2
      !

!        initialization
      success = .false.
      itemPT => null()
      sourceItem => null()
      itemt3D => null()
      maxlay = 0
      vectormax = 1
      !
      ! Skip the lead comment lines plus one additional line.
      do
         call GetLine(fileReaderPtr%fileHandle, rec, istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unexpected end of file.")
            return
         end if
         if (rec(1:1) /= '*') exit
      end do
      ! Read the polyline name from the first line of a tekal-block
      polyline_name = trim(adjustl(rec))
      ! Read the number of support points.
      read (fileReaderPtr%fileHandle, *, iostat=istat) n_points
      if (istat /= 0) then
         call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unable to read the number of points.")
         return
      end if
      ! Sanity check
      if (n_points < 2) then
         call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Less then two support points found.")
         return
      end if
      ! Read the support point coordinate pairs.
      allocate (xs(n_points))
      allocate (ys(n_points))
      allocate (mask(n_points))
      mask = 1
      allocate (itemIDList(n_points))
      itemIDList = ec_undef_int
      allocate (plipointlbls(n_points))
      plipointlbls = ''

      do i = 1, n_points
         call GetLine(fileReaderPtr%fileHandle, rec, istat)
         if (index(rec, '!') > 0) rec = rec(1:index(rec, '!') - 1) ! trim commented  (!)
         if (index(rec, '#') > 0) rec = rec(1:index(rec, '#') - 1) ! trim commented  (#)
         if (len(trim(rec)) == 0) cycle ! skip empty lines     (or commented-out coordinate pairs)
         istat = 0
         read (rec, *, iostat=istat) xs(i), ys(i) ! see if we can at least read a coordinate pair
         if (istat /= 0) then
            call setECMessage("   '"//trim(rec)//"'")
            call setECMessage("Unable to read a coordinate pair from file "//trim(fileReaderPtr%fileName))
            return
         end if
         !lblstart = index(rec,'{')
         !lblend = index(rec,'}')
         !plipointlbls(i) = ''
         !if (lblstart>0 .and. lblend>lblstart) then
         !   plipointlbls  = rec(lblstart+1:lblend-1)
         !endif
         lblstart = index(rec, 'label=')
         plipointlbls(i) = ''
         if (lblstart > 0) then
            read (rec(lblstart + 6:len_trim(rec)), *, iostat=istat) plipointlbls(i)
         end if
      end do
      ! close pli file
      close (fileReaderPtr%fileHandle, iostat=istat)

      ! Construct the poly_tim Item.
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='polytim_item'))) return

      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
      if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
      if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
      if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n_points)) return

      fieldId = ecInstanceCreateField(instancePtr)
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. ecItemSetRole(instancePtr, itemId, itemType_target)) return
      if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
      if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
      if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
      if (.not. ecItemSetTargetField(instancePtr, itemId, fieldId)) return

      itemPT => ecSupportFindItem(instancePtr, itemId)

      ! Determine the end of the base of the fileName.
      L = index(fileReaderPtr%fileName, '.', back=.true.) - 1
      ! Create providers at each support point, depending on the availability of specific files.
      n_signals = 0 ! Record whether at least one child provider is created for this polytim.

      do i = 1, n_points
         ! plipoint labels read from the third column in the pli-file. Currently this goes wrong if in the test third-column labels are not unique
         if (len_trim(plipointlbls(i)) == 0) then
            write (plipointlbl, '(a,i4.4)') fileReaderPtr%fileName(1:L)//'_', i
            has_label = .false.
         else
            plipointlbl = trim(plipointlbls(i))
            has_label = .true.
         end if

         filename = trim(plipointlbl)//'.tim'
         inquire (file=trim(filename), exist=exists)
         if (exists) then
            id = ecInstanceCreateFileReader(instancePtr)
            if (id == ec_undef_int) return
            fileReaderPtr2 => ecSupportFindFileReader(instancePtr, id)
            fileReaderPtr2%vectormax = fileReaderPtr%vectormax ! TODO copy timeframe
            if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_uniform, filename, fileReaderPtr%tframe%k_refdate, &
                                                      fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit))) return
            signaltype = BC_FUNC_TSERIES
         else
            filename = trim(plipointlbl)//'.cmp'
            inquire (file=trim(filename), exist=exists)
            if (exists) then
               id = ecInstanceCreateFileReader(instancePtr)
               if (id == ec_undef_int) return
               fileReaderPtr2 => ecSupportFindFileReader(instancePtr, id)
               fileReaderPtr2%vectormax = fileReaderPtr%vectormax
               if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_fourier, filename, fileReaderPtr%tframe%k_refdate, &
                                                         fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit, dtnodal=fileReaderPtr%tframe%dtnodal))) return

               signaltype = BC_FUNC_HARMONIC
            else
               filename = trim(plipointlbl)//'.t3d'
               inquire (file=trim(filename), exist=exists)
               if (exists) then
                  id = ecInstanceCreateFileReader(instancePtr)
                  if (id == ec_undef_int) return
                  fileReaderPtr2 => ecSupportFindFileReader(instancePtr, id)
                  fileReaderPtr2%vectormax = fileReaderPtr%vectormax
                  if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_t3D, filename, fileReaderPtr%tframe%k_refdate, &
                                                            fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit))) return

                  signaltype = BC_FUNC_TIM3D
               else ! No file with data for this point
                  if (has_label) then ! Report explicitly labelled point without data
                     call setECMessage("No .tim, .cmp or .t3d file found for labelled point '" &
                                       //trim(plipointlbl)//"' (required).")
                     return
                  end if ! labelled point ?
                  signaltype = -1
               end if ! tim3d-file ?
            end if ! cmp file ?
         end if ! tim-file ?
         if (.not. ecProviderConnectSourceItemsToTargets(instancePtr, signaltype, id, itemId, i, &
                                                         n_signals, maxlay, itemIDList)) then
            ! No sub-FileReader made.
            mask(i) = 0
         end if
      end do ! loop over support points
      if (n_signals <= 0) then
         call setECMessage("ERROR: ec_provider::ecProviderPolyTimItems: No forcing signals (.tim/.cmp/.t3d/.qh) could be attached for polyline file '"//trim(fileReaderPtr%filename)//"'.")
         return
      end if

      ! itemID refers to the source item (providing to the polytim item) for the last support point we came across in the above loop.
      if (.not. ecProvider3DVectmax(instancePtr, itemPT, mask, maxlay, n_points, itemIDList)) return

      ! Since the main FileReader's Item is a target, the TimeFrame is not set.
      ! Add successfully created source Item to the main FileReader
      if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPT%id)) return
      !
      success = .true.
   end function ecProviderCreatePolyTimItems

!==============================================================================================================

   !> Create subproviders, which create source Items and their contained types.
      !! meteo1.f90: read1polylin
   function ecProviderCreatePolyTimItemsBC(instancePtr, fileReaderPtr, bctfilename, quantityname) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      character(len=*), intent(in) :: bctfilename !< in case of bct-data, we neeed the explicit filename
      character(len=*), intent(in) :: quantityname !< in case of bct-data, we neeed the explicit quantityname
      !
      real(hp), dimension(:), allocatable :: xs !< x-coordinates of support points
      real(hp), dimension(:), allocatable :: ys !< y-coordinates of support points
      integer, dimension(:), allocatable :: mask !< support point mask array (for polytime ElementSet)
      integer :: n_points !< number of support points
      integer :: n_signals !< Number of forcing signals created (at most n_signals==n_points, but warn if n_signals==0)
      character(len=:), allocatable :: rec !< a read line
      integer :: i !< loop counters
      integer :: istat !< status of read operation
      character(len=:), allocatable :: plipointlbl !< temporary name of current pli-point in bct context
      type(VLSType), dimension(:), allocatable :: plipointlbls !< user-specified name for all pli-point in bct context
      character(len=:), allocatable :: polyline_name !< polyline name read from pli-file
      character(len=4) :: cnum !< temp integer converted to a string
      integer :: id !< dummy, catches ids which are not used
      integer :: quantityId, elementSetId, fieldId, itemId, BCBlockID
      integer :: maxLay
      type(tEcItem), pointer :: itemPT
      type(tEcItem), pointer :: itemt3D
      type(tEcItem), pointer :: sourceItem
      integer, dimension(:), allocatable :: itemIDList
      integer :: vectormax
      type(tEcBCBlock), pointer :: bcBlockPtr
      logical :: all_points_are_corr
      logical :: has_label
      integer :: lblstart
      integer :: bctfiletype
      logical :: file_exists
      !

!        initialization
      success = .false.
      itemPT => null()
      sourceItem => null()
      itemt3D => null()
      maxlay = 0
      vectormax = 1
      !
      ! Skip the lead comment lines plus one additional line.
      do
         call GetLine(fileReaderPtr%fileHandle, rec, istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItemsBC: Unexpected end of file.")
            return
         end if
         if (rec(1:1) /= '*') exit
      end do
      ! Read the polyline name from the first line of a tekal-block
      polyline_name = trim(adjustl(rec))
      ! Read the number of support points.
      read (fileReaderPtr%fileHandle, *, iostat=istat) n_points
      if (istat /= 0) then
         call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItemsBC: Unable to read the number of points.")
         return
      end if
      ! Sanity check
      if (n_points < 2) then
         call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItemsBC: Less then two support points found.")
         return
      end if
      ! Read the support point coordinate pairs.
      allocate (xs(n_points), ys(n_points), mask(n_points), itemIDList(n_points), plipointlbls(n_points), stat=istat)
      if (istat /= 0) then
         call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItemsBC: allocation error. N_points = ", n_points)
      end if
      mask = 1
      itemIDList = ec_undef_int
      do i = 1, n_points
         call GetLine(fileReaderPtr%fileHandle, rec, istat)
         if (index(rec, '!') > 0) rec = rec(1:index(rec, '!') - 1) ! trim commented  (!)
         if (index(rec, '#') > 0) rec = rec(1:index(rec, '#') - 1) ! trim commented  (#)
         if (len(trim(rec)) == 0) cycle ! skip empty lines     (or commented-out coordinate pairs)
         istat = 0
         read (rec, *, iostat=istat) xs(i), ys(i) ! see if we can at least read a coordinate pair
         if (istat /= 0) then
            call setECMessage("   '"//trim(rec)//"'")
            call setECMessage("Unable to read a coordinate pair from file "//trim(fileReaderPtr%fileName))
            return
         end if
         lblstart = index(rec, 'label=')

         if (lblstart > 0) then
            plipointlbls(i)%s = rec(lblstart + 6:)
         end if
      end do

      ! Construct the poly_tim Item
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='polytim_item'))) return

      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
      if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
      if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
      if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n_points)) return

      fieldId = ecInstanceCreateField(instancePtr)
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. ecItemSetRole(instancePtr, itemId, itemType_target)) return
      if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
      if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
      if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
      if (.not. ecItemSetTargetField(instancePtr, itemId, fieldId)) return

      itemPT => ecSupportFindItem(instancePtr, itemId)

      inquire (FILE=bctfilename, EXIST=file_exists)
      if (.not. file_exists) then
         call setECMessage("Forcing file ("//trim(bctfilename)//") not found.")
         return
      end if
      if (index(trim(bctfilename)//'|', '.bc|') > 0) then ! ASCII: bc-format  : detection is extension-based
         bctfiletype = BC_FTYPE_ASCII
      else if (index(trim(bctfilename)//'|', '.nc|') > 0) then ! NETCDF: nc-format
         bctfiletype = BC_FTYPE_NETCDF
      else
         call setECMessage("Forcing file ("//trim(bctfilename)//") should either have extension .nc (netcdf timeseries file) or .bc (ascii BC-file).")
         return
      end if

      all_points_are_corr = .true.
      ! Init BCBlock for (global) qh-bound
      bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
      bcBlockPtr => ecSupportFindBCBlock(instancePtr, bcBlockId)
      plipointlbl = polyline_name
      call str_upper(quantityname)
      n_signals = 0 ! Record whether at least one child provider is created for this polytim.
      do i = 1, n_points
         ! Process a *.tim file.
         bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
         bcBlockPtr => ecSupportFindBCBlock(instancePtr, bcBlockId)
         ! id van de filereader

         ! plipoint labels read from the third column in the pli-file. Currently this goes wrong if in the test third-column labels are not unique
         if (.not. allocated(plipointlbls(i)%s)) then
            write (cnum, '(i4.4)') i
            plipointlbl = polyline_name//'_'//cnum ! using polyline_name from tekal-block
            has_label = .false.
         else
            plipointlbl = trim(plipointlbls(i)%s)
            has_label = .true.
         end if

         if (.not. ecProviderInitializeBCBlock(InstancePtr, bcBlockId, fileReaderPtr%tframe%k_refdate, fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit, &
                                               id, bctfilename, bctfiletype, quantityname, plipointlbl, istat, dtnodal=fileReaderPtr%tframe%dtnodal)) then
            if (has_label) then ! for explicitly labelled pli-points, require data
               call setECMessage("BC-File "//trim(bctfilename)//" contains no data for labelled point '" &
                                 //trim(plipointlbl)//"' and quantity '"//trim(quantityname)//"' (required).")
               return
            else
               mask(i) = 0
               cycle
            end if
         end if
         if (bcBlockPtr%func == BC_FUNC_HARMOCORR .or. bcBlockPtr%func == BC_FUNC_ASTROCORR) then
            ecAtLeastOnePointIsCorrection = .true. ! TODO: Refactor this shortcut (UNST-180).
!                 n_signals = n_signals + 1
            cycle
         else
            all_points_are_corr = .false.
            ! For non-harmonic, check if the source side vectormax equals the one requested on the target side
            if (fileReaderPtr%vectormax /= bcBlockPtr%quantity%vectormax) then
               call setECMessage("BC-File '"//trim(bctfilename)//"' quantity '"//trim(quantityname) &
                                 //"' has the wrong vector rank, please check the vector definition.")
               mask(i) = 0
               exit
            end if
         end if
         if (.not. ecProviderConnectSourceItemsToTargets(instancePtr, bcBlockPtr%func, id, itemId, i, &
                                                         n_signals, maxlay, itemIDList, qname=quantityname)) then
            !
            ! No sub-FileReader made.
            mask(i) = 0
         end if
      end do ! loop over support points

      if (n_signals > 0) then
         if (.not. ecQuantitySet(instancePtr, quantityId, &
                                 fillvalue=bcBlockPtr%quantity%missing, &
                                 offset=bcBlockPtr%quantity%offset, &
                                 factor=bcBlockPtr%quantity%factor)) then
            return
         end if
      end if

      if (ecAtLeastOnePointIsCorrection) then ! TODO: Refactor this shortcut (UNST-180).
         if (all_points_are_corr) then
            success = .true.
         else
            ! TODO: error: bc file currently should only contain corrections
         end if
         return
      end if

      if (n_signals <= 0) then
         call setECMessage("    for polyline "//trim(polyline_name)//" and quantity "//trim(quantityname)//".")
         call setECMessage("No signals for polyline file "//trim(fileReaderPtr%filename)//" found in "//trim(bctfilename))
         success = .false.
         return
      end if

      ! itemID refers to the source item (providing to the polytim item) for the last support point we came across in the above loop.
      if (.not. ecProvider3DVectmax(instancePtr, itemPT, mask, maxlay, n_points, itemIDList)) return

      ! Since the main FileReader's Item is a target, the TimeFrame is not set.
      ! Add successfully created source Item to the main FileReader
      if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPT%id)) return
      !
      ! close pli file
      close (fileReaderPtr%fileHandle, iostat=istat)
      success = .true.
   end function ecProviderCreatePolyTimItemsBC

   !TODO: pass vertical interpolation type from source item to target
   function ecProvider3DVectmax(instancePtr, itemPT, mask, maxlay, n_points, itemIDList) result(success)
      implicit none

      logical :: success

      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcItem), pointer :: itemPT
      integer, intent(in) :: mask(:)
      integer, intent(in) :: maxlay
      integer, intent(in) :: n_points
      integer, dimension(:), allocatable :: itemIDList

      real(hp), dimension(:), allocatable :: zs !< z/sigma-coordinates of support points
      integer :: i, magnitude, vectormax, zInterpolationType
      type(tEcItem), pointer :: itemt3D, itemSRC
      integer :: elementSetId
      integer :: fieldId
      integer :: vptyp
      integer :: iconn, isrc
      vectormax = 1
      zInterpolationType = 0

      fieldId = itemPT%targetFieldPtr%id
      elementSetId = itemPT%elementSetPtr%id
      success = ecElementSetSetMaskArray(instancePtr, elementSetId, mask)

      if (maxlay > 0) then ! t3D type
         allocate (zs(maxlay * n_points))
         zs = ec_undef_hp
         do i = 1, n_points
            magnitude = itemIDList(i)
            if (magnitude /= ec_undef_int) then
               Itemt3D => ecSupportFindItem(instancePtr, magnitude)
               zs((i - 1) * maxlay + 1:(i - 1) * maxlay + size(itemt3D%elementSetPtr%z)) = itemt3D%elementSetPtr%z
               vptyp = itemt3D%elementSetPtr%vptyp
            end if
         end do
         if (success) success = ecElementSetSetZArray(instancePtr, elementSetId, zs)

         if (.not. ecElementSetSetProperties(instancePtr, elementSetId, vptyp=vptyp)) return

         if (.not. ecFieldCreate1dArray(instancePtr, fieldId, n_points * maxlay)) then
            success = .false.
         end if
      end if
      ! Determine vectormax.
      itemPT%quantityptr%vectormax = 0
      do iconn = 1, itemPT%nConnections
         if (itemPT%connectionsptr(iconn)%ptr%nSourceItems > 0) then ! assume those of the first source item ....
            itemSRC => itemPT%connectionsptr(iconn)%ptr%sourceItemsptr(1)%ptr
            zInterpolationType = itemSRC%quantityptr%zInterpolationType
            vectormax = itemSRC%quantityptr%vectormax
            itemPT%quantityptr%zInterpolationType = zInterpolationType
            itemPT%quantityptr%vectormax = vectormax
         end if
         do isrc = 1, itemPT%connectionsptr(iconn)%ptr%nSourceItems ! ... check with all followinf source items
            itemSRC => itemPT%connectionsptr(iconn)%ptr%sourceItemsptr(isrc)%ptr
            if (itemPT%quantityptr%vectormax /= vectormax) then
               call setECMessage("ERROR: ec_provider::ecProvider3DVectmax: Polytim subitems have different vectormax.")
               continue
            end if
            if (maxlay > 0) then
               if (itemPT%quantityptr%zInterpolationType /= zInterpolationType) then
                  call setECMessage("ERROR: ec_provider::ecProvider3DVectmax: Polytim subitems have different interpolation types.")
                  continue
               end if
            end if
         end do
      end do

      ! Allocate the target Field.
      if (.not. ecFieldCreate1dArray(instancePtr, fieldId, n_points * max(maxlay, 1) * vectormax)) then
         success = .false.
      end if
   end function ecProvider3DVectmax

!==============================================================================================================

   function ecProviderConnectSourceItemsToTargets(instancePtr, signaltype, fileReaderId, targetItemId, targetIndex, n_signals, maxlay, itemIDList, qname) result(itemFound)
      logical :: itemFound
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      integer :: fileReaderId ! file reader id
      integer :: targetItemId ! target item id
      integer :: targetIndex ! index in target item's values
      integer :: n_signals, maxlay ! INOUT
      integer, dimension(:) :: itemIDList ! INOUT
      integer :: signaltype
      integer :: subconverterId, magnitude, j, connectionId, nr_fourier_items, anItemId
      type(tEcItem), pointer :: itemt3D
      character(len=*), optional :: qname

      itemFound = .false.

      select case (signaltype)
      case (BC_FUNC_TSERIES, BC_FUNC_CONSTANT)
         ! Construct a new Converter.
         subconverterId = ecInstanceCreateConverter(instancePtr)
         ! Determine the source Items.
         if (present(qname)) then
            magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, trim(qname))
            if (magnitude == ec_undef_int) then ! new BC-format has items labelled with the quantity
               call setECMessage("ecProviderConnectSourceItemsToTargets: cannot find filereader item with quantity "//trim(qname)//".")
               magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'uniform_item')
            end if
         else
            magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'uniform_item')
         end if

         if (magnitude == ec_undef_int) then
            call setECMessage("ecProviderConnectSourceItemsToTargets: cannot find filereader item with quantity 'uniform_item'.")
            return
         end if
         ! Initialize the new Converter.
         if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_uniform) .and. &
                    ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                    ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_timespace) .and. &
                    ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
         ! Construct a new Connection.
         connectionId = ecInstanceCreateConnection(instancePtr)
         if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
         ! Initialize the new Connection.
         if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, magnitude)) return
         if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
         if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
         n_signals = n_signals + 1
         itemFound = .true.
      case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO)
         ! Construct a new Converter.
         subconverterId = ecInstanceCreateConverter(instancePtr)
         ! Determine the source Items.
         nr_fourier_items = ecFileReaderGetNumberOfItems(instancePtr, fileReaderId)
         ! Initialize the new Converter.
         if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_fourier) .and. &
                    ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                    ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_passthrough) .and. &
                    ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
         ! Construct a new Connection.
         connectionId = ecInstanceCreateConnection(instancePtr)
         if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
         ! Initialize the new Connection.
         do j = 1, nr_fourier_items
            anItemId = ecFileReaderGetItem(instancePtr, fileReaderId, j)
            if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, anItemId)) return
         end do
         if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
         if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
         n_signals = n_signals + 1
         itemFound = .true.
      case (BC_FUNC_TIM3D)
         ! Construct a new Converter.
         subconverterId = ecInstanceCreateConverter(instancePtr)

         ! Determine the source Items.
         if (present(qname)) then
            magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, trim(qname))
            if (magnitude == ec_undef_int) then ! new BC-format has items labelled with the quantity
               call setECMessage("ecProviderConnectSourceItemsToTargets: cannot find filereader item with quantity "//trim(qname)//".")
               magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'uniform_item')
            end if
         else
            magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'uniform_item')
         end if

         if (magnitude == ec_undef_int) then
            call setECMessage("ecProviderConnectSourceItemsToTargets: cannot find filereader item with quantity 'uniform_item'.")
            return
         end if

         ! update maximum number of layers
         Itemt3D => ecSupportFindItem(instancePtr, magnitude)
         maxlay = max(maxlay, size(Itemt3D%elementSetPtr%z))
         ItemIDList(targetIndex) = magnitude
         ! Initialize the new Converter.
         if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_uniform) .and. &
                    ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                    ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_timespace) .and. &
                    ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
         ! Construct a new Connection.
         connectionId = ecInstanceCreateConnection(instancePtr)
         if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
         ! Initialize the new Connection.
         if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, magnitude)) return
         if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
         if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
         n_signals = n_signals + 1
         itemFound = .true.
      end select

   end function ecProviderConnectSourceItemsToTargets
   ! itemIDList

   ! =======================================================================

   !> Create source Items and their contained types, based on a spiderweb file's header.
      !! meteo1.f90: reaspwheader
   function ecProviderCreateSpiderwebItems(instancePtr, fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      !
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      integer :: n_cols !< helper variable
      integer :: n_rows !< helper variable
      real(hp) :: missingValue !< helper variable
      real(hp) :: radius !< helper variable
      real(hp) :: spw_merge_frac !< helper variable
      character(len=maxNameLen) :: radius_unit !< helper variable
      type(tEcItem), pointer :: item1 !< Item containing quantity1
      type(tEcItem), pointer :: item2 !< Item containing quantity2
      type(tEcItem), pointer :: item3 !< Item containing quantity3
      character(len=maxNameLen) :: rec !< helper variable
      !
      success = .false.
      item1 => null()
      item2 => null()
      item3 => null()
      !
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_cols')
      read (rec, *) n_cols
      n_cols = n_cols + 1 ! an additional column for 360 == 0 degrees
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_rows')
      read (rec, *) n_rows
      n_rows = n_rows + 1 ! an additional row for the eye
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'NODATA_value')
      read (rec, *) missingValue
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'spw_radius')
      read (rec, *) radius
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'spw_rad_unit')
      read (rec, *) radius_unit
      spw_merge_frac = 0.5
      rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'spw_merge_frac')
      if (len_trim(rec) > 0) read (rec, *) spw_merge_frac
      !
      ! One common ElementSet.
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_spw) .and. &
                 ecElementSetSetRadius(instancePtr, elementSetId, radius, spw_merge_frac, radius_unit) .and. &
                 ecElementSetSetRowsCols(instancePtr, elementSetId, n_rows, n_cols))) then
         success = .false.
      end if
      !
      ! ===== quantity1: wind_speed =====
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='windspeed', &
                               units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item1 => ecSupportFindItem(instancePtr, itemId)
      end if
      ! ===== quantity2: wind_from_direction =====
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='winddirection', &
                               units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit2'))))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item2 => ecSupportFindItem(instancePtr, itemId)
      end if
      ! ===== quantity3: p_drop =====
      quantityId = ecInstanceCreateQuantity(instancePtr)
      if (.not. (ecQuantitySet(instancePtr, quantityId, name='p_drop', &
                               units=trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit3'))))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols * n_rows) .and. &
                 ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item3 => ecSupportFindItem(instancePtr, itemId)
      end if
      ! ===== finish initialization of Fields =====
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
      success = ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)
      if (success) then
         success = ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, 0, n_cols, n_rows)
      end if
      if (success) then
         success = ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, 1, n_cols, n_rows)
      end if
      ! Add successfully created source Items to the FileReader
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item1%id)
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item2%id)
      if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item3%id)
   end function ecProviderCreateSpiderwebItems

   ! =======================================================================
   function ecProviderCreateTimeInterpolatedItem(instancePtr, sourceItemId, tgtNdx, arrayPtr, scalarPtr) result(itemId)
      use m_ec_item
      use m_ec_converter, only: ecConverterSetType, ecConverterSetInterpolation, ecConverterSetOperand, ecConverterSetElement
      use m_ec_instance, only: ecInstanceCreateConverter, ecInstanceCreateConnection, ecInstanceCreateItem, ecInstanceCreateField, ecInstanceCreateQuantity
      use m_ec_connection, only: ecConnectionAddTargetItem, ecConnectionAddSourceItem, ecConnectionSetConverter
      use m_ec_quantity, only: ecQuantitySet
      use m_ec_field, only: ecFieldCreate1dArray
      use m_ec_item

      type(tEcInstance), pointer :: instancePtr !< EC-instance
      integer, intent(in) :: sourceItemId !< Source item id, before temporal interpolation
      integer, intent(in), optional :: tgtNdx !< Optional target index, 1 is assumed as default
      real(hp), pointer, optional :: arrayPtr(:) !< Optional pointer to a target array somewhere else
      real(hp), pointer, optional :: scalarPtr !< Optional pointer to a target scalar somewhere else
      integer :: targetItemId !< Target item id, after temporal interpolation
      integer :: itemId !< returned  target item ID, if successful, otherwise -1
      type(tECItem), pointer :: sourceItemPtr => null()
      character(len=:), allocatable :: quantityName
      logical :: quantityPeriodic, quantityConstant
      integer :: arraySize

      integer :: targetIndex
      integer :: converterId, connectionId, quantityId, elementSetId, fieldId

      if (present(tgtNdx)) then
         targetIndex = tgtNdx
      else
         targetIndex = 1
      end if

      sourceItemPtr => ecSupportFindItem(instancePtr, sourceItemId)

      ! TODO: create target item:
      !       . elementset-name = source_item's elementset-name
      !       . quantity-name = source_item's quantity-name + '-interpolated'
      itemId = -1

      ! Set up the target item
      targetItemId = ecInstanceCreateItem(instancePtr)
      fieldId = ecInstanceCreateField(instancePtr)

      arraySize = size(sourceItemPtr%sourceT0FieldPtr%arr1d)
      if (present(arrayPtr)) then
         if (.not. associated(arrayPtr)) then
            call setECMessage("ecProviderCreateTimeInterpolatedItem: pointer to target array unassociated.")
            return
         end if
         if (size(arrayPtr) /= arraySize) then ! check size
            call setECMessage("ecProviderCreateTimeInterpolatedItem: reserved target size and source size do not match.")
            return
         end if
         if (.not. ecFieldSet1dArrayPointer(instancePtr, fieldId, arrayPtr)) then
            call setECMessage("ecProviderCreateTimeInterpolatedItem: setting pointer to target array failed.")
            return
         end if
      else
         if (.not. (ecFieldCreate1dArray(instancePtr, fieldId, arraySize))) return
      end if

      if (present(scalarPtr)) then
         if (.not. associated(scalarPtr)) then
            call setECMessage("ecProviderCreateTimeInterpolatedItem: pointer to target scalar unassociated.")
            return
         end if
         if (.not. ecFieldSetScalarPointer(instancePtr, fieldId, scalarPtr)) then
            call setECMessage("ecProviderCreateTimeInterpolatedItem: setting pointer to target scalar failed.")
            return
         end if
      end if

      if (.not. ecItemSetRole(instancePtr, targetItemId, itemType_target)) return
      if (.not. ecItemSetTargetField(instancePtr, targetItemId, fieldId)) return
      if (.not. ecItemSetType(instancePtr, targetItemId, accessType_evaluate)) return
      quantityId = ecInstanceCreateQuantity(instancePtr)
      quantityName = trim(sourceItemPtr%quantityPtr%name)
      quantityPeriodic = sourceItemPtr%quantityPtr%periodic
      quantityConstant = sourceItemPtr%quantityPtr%constant
      if (.not. ecItemSetQuantity(instancePtr, targetItemId, quantityId)) return
      if (.not. (ecQuantitySet(instancePtr, quantityId, name=quantityName//'_interpolated'))) return
      if (.not. (ecQuantitySet(instancePtr, quantityId, periodic=quantityPeriodic, constant=quantityConstant))) return
      elementSetId = sourceItemPtr%elementSetPtr%id
      if (.not. ecItemSetElementSet(instancePtr, targetItemId, elementSetId)) return

      ! Construct a new Converter.
      converterId = ecInstanceCreateConverter(instancePtr)

      ! Initialize the new Converter.
      if (.not. (ecConverterSetType(instancePtr, converterId, convType_uniform))) return
      if (.not. (ecConverterSetOperand(instancePtr, converterId, operand_replace_element))) return
      if (.not. (ecConverterSetInterpolation(instancePtr, converterId, interpolate_time_extrapolation_ok))) return
      if (.not. (ecConverterSetElement(instancePtr, converterId, targetIndex))) return

      ! Construct a new Connection.
      connectionId = ecInstanceCreateConnection(instancePtr)
      if (.not. ecConnectionSetConverter(instancePtr, connectionId, converterId)) return

      ! Initialize the new Connection.
      if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, sourceItemId)) return
      if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
      if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
      itemId = targetItemId
   end function ecProviderCreateTimeInterpolatedItem

   ! =======================================================================

   !> Create source Items and their contained types, based on a NetCDF file's header.
   function ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityName, varname) result(success)
      use transform_poleshift
      use m_ec_message
      use m_alloc
      implicit none
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      character(len=*), intent(in) :: quantityName !< name of quantity to read
      character(len=*), optional, intent(in) :: varname !< name of variabele (ignored if = ' ')

      integer :: ierror !< return value of NetCDF function calls
      integer :: idvar !< variable id of the forcing variable
      integer :: varid !< dummy variable id of coordinate variable
      integer :: ndims !< number of dimensions within NetCDF or for the current variable
      integer :: idims !< helper variables
      integer :: ifgd, isgd !< helper variables
      integer, dimension(:), allocatable :: dimids !< ids of a variable's dimensions
      integer, dimension(:), allocatable :: coordids !< helper variable
      integer :: i, j !< loop counter
      integer :: grid_mapping_id !< id of the applied grid mapping
      integer :: fgd_id !< var_id for elementset X or latitude
      integer :: sgd_id !< var_id for elementset Y or longitude
      integer :: tgd_id !< var_id for elementset Z or depth
      integer :: fgd_grid_type !< helper variable for consistency check on grid_type
      integer :: sgd_grid_type !< helper variable for consistency check on grid_type
      integer :: grid_type !< elmSetType enum
      integer :: vptyp !< interpretation of the vertical coordinate
      character(len=NF90_MAX_NAME) :: z_positive !< which direction of z is positive ?
      character(len=NF90_MAX_NAME) :: z_standardname !< which direction of z is positive ?
      real(hp) :: gnplon, gnplat !< coordinates of shifted north pole obtained from gridmapping
      real(hp) :: gsplon, gsplat !< coordinates of shifted south pole obtained from gridmapping
      real(hp), dimension(:, :), allocatable :: fgd_data !< coordinate data along first dimension's axis
      real(hp), dimension(:), allocatable :: fgd_data_1d !< coordinate data along first dimension's axis
      real(hp), dimension(:), allocatable :: fgd_data_trans !< coordinate data along first dimension's axis transformed, rotating pole
      real(hp), dimension(:, :), allocatable :: sgd_data !< coordinate data along second dimension's axis
      real(hp), dimension(:), allocatable :: sgd_data_1d !< coordinate data along second dimension's axis
      real(hp), dimension(:), allocatable :: sgd_data_trans !< coordinate data along first dimension's axis transformed, rotating pole
      real(hp), dimension(:), allocatable :: tgd_data_1d !< coordinate data along third dimension's axis
      real(hp), dimension(:), allocatable :: pdiri !<
      real(hp) :: var_miss !< missing data value in second dimension
      character(len=NF90_MAX_NAME) :: grid_mapping !< name of the applied grid mapping
      character(len=NF90_MAX_NAME) :: units !< helper variable for variable's units
      character(len=NF90_MAX_NAME) :: coord_name !< helper variable
      character(len=NF90_MAX_NAME), dimension(:), allocatable :: coord_names !< helper variable
      character(len=NF90_MAX_NAME) :: name !< helper variable
      character(len=NF90_MAX_NAME), dimension(4) :: ncstdnames !< helper variable : temp. list of standard names to search for in netcdf
      character(len=NF90_MAX_NAME), dimension(1) :: ncstdnames_fallback !< idem
      character(len=NF90_MAX_NAME), dimension(:), allocatable :: ncvarnames !< helper variable : temp. list of variable names to search for in netcdf
      character(len=NF90_MAX_NAME), dimension(:), allocatable :: nccustomnames !< helper variable : temp. list of user-defined variables names to search for
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      type(tEcItem), pointer :: itemPtr !< Item containing quantity
      logical :: dummy !< temp
      character(len=50) :: attstr
      logical :: rotate_pole
      integer :: lon_varid, lon_dimid, lat_varid, lat_dimid, tim_varid, tim_dimid
      integer :: grid_lon_varid, grid_lat_varid
      integer :: x_varid, x_dimid, y_varid, y_dimid, z_varid, z_dimid, nod_varid, nod_dimid
      integer :: realization_varid, realization_dimid, dim_offset

      integer, dimension(:, :), allocatable :: crd_dimids, crd_dimlen
      integer :: timeint
      integer :: expectedLength
      character(len=:), allocatable :: nameVar ! variable name in error message
      character(len=2) :: cnum1, cnum2 ! 1st and 2nd number converted to string for error message
      integer :: nrow, ncol, nlay, nrel
      !
      success = .false.
      itemPtr => null()
      fgd_grid_type = ec_undef_int
      sgd_grid_type = ec_undef_int
      grid_type = ec_undef_int
      units = ''
      coord_name = ''
      name = ''
      ndims = 0
      rotate_pole = .false.

      ! =============================================================================
      ! Find the Quantity corresponding to quantityName. (configurable in the future)
      ! =============================================================================
      ! TODO: Check on standard variable name.
      ! TODO: Check on variable's long_name.
      ! Check for variable named "rainfall". FEWS calls this "Rainfall"

      ! quantityName -> set of netcdf names -> set of varids
      ! TODO: map var id's to standard names in the netcdf (loop)
      ! TODO: quantity names ARE standardnames
      ! TODO: loop over id's

      ! ncstdnames now provided with the standard names, used to label the netcdf quantities as well as search for the varids
      ! (already stored in the filereader)
      ! For now assuming the MATROOS-definitions of variables, listed at
      ! https://publicwiki.deltares.nl/display/NETCDF/Matroos+Standard+names
      ncstdnames(:) = ''
      allocate (ncvarnames(4)) ! todo: error handling
      ncvarnames(:) = ''
      ncstdnames_fallback = ' '
      idvar = -1
      select case (trim(quantityName))
      case ('rainfall')
         ncvarnames(1) = 'rainfall'
         ncstdnames(1) = 'precipitation_amount'
      case ('rainfall_rate')
         ncvarnames(1) = 'rainfall'
         ncstdnames(1) = 'rainfall_rate'
      case ('windx')
         ncvarnames(1) = 'u10' ! 10 meter eastward wind
         ncstdnames(1) = 'eastward_wind'
      case ('windy')
         ncvarnames(1) = 'v10' ! 10 meter northward wind
         ncstdnames(1) = 'northward_wind'
      case ('windxy')
         ncvarnames(1) = 'u10' ! 10 meter eastward wind
         ncstdnames(1) = 'eastward_wind'
         ncvarnames(2) = 'v10' ! 10 meter northward wind
         ncstdnames(2) = 'northward_wind'
      case ('stressx')
         ncvarnames(1) = 'tauu' ! eastward wind stress
         ncstdnames(1) = 'surface_downward_eastward_stress'
      case ('stressy')
         ncvarnames(1) = 'tauv' ! northward wind stress
         ncstdnames(1) = 'surface_downward_northward_stress'
      case ('stressxy')
         ncvarnames(1) = 'tauu' ! eastward wind stress
         ncstdnames(1) = 'surface_downward_eastward_stress'
         ncvarnames(2) = 'tauv' ! northward wind stress
         ncstdnames(2) = 'surface_downward_northward_stress'
      case ('airpressure', 'atmosphericpressure')
         ncvarnames(1) = 'msl' ! mean sea-level pressure
         ncstdnames(1) = 'air_pressure'
      case ('airdensity')
         ! UNST-6593: airdensity has variable name p140209 and no standard_name, will be changed in the future according to ECMWF.
         ncvarnames(1) = 'p140209' ! air density above sea
         ncstdnames(1) = 'air_density'
      case ('airpressure_windx_windy')
         ncvarnames(1) = 'msl' ! mean sea-level pressure
         ncstdnames(1) = 'air_pressure'
         ncvarnames(2) = 'u10' ! 10 meter eastward wind
         ncstdnames(2) = 'eastward_wind'
         ncvarnames(3) = 'v10' ! 10 meter northward wind
         ncstdnames(3) = 'northward_wind'
      case ('airpressure_stressx_stressy')
         ncvarnames(1) = 'msl' ! mean sea-level pressure
         ncstdnames(1) = 'air_pressure'
         ncvarnames(2) = 'tauu' ! eastward wind stress
         ncstdnames(2) = 'surface_downward_eastward_stress'
         ncvarnames(3) = 'tauv' ! northward wind stress
         ncstdnames(3) = 'surface_downward_northward_stress'
      case ('airpressure_windx_windy_charnock')
         ncvarnames(1) = 'msl' ! mean sea-level pressure
         ncstdnames(1) = 'air_pressure'
         ncvarnames(2) = 'u10' ! 10 meter eastward wind
         ncstdnames(2) = 'eastward_wind'
         ncvarnames(3) = 'v10' ! 10 meter northward wind
         ncstdnames(3) = 'northward_wind'
         ncvarnames(4) = 'c' ! space varying Charnock coefficients
         ncstdnames(4) = 'charnock'
      case ('charnock')
         ncvarnames(1) = 'c' ! space varying Charnock coefficients
         ncstdnames(1) = 'charnock'
      case ('airtemperature')
         ncvarnames(1) = 't2m' ! 2-meter air temperature
         ncstdnames(1) = 'air_temperature'
      case ('cloudiness')
         ncvarnames(1) = 'tcc' ! cloud cover (fraction)
         ncstdnames(1) = 'cloud_area_fraction'
      case ('humidity')
         ncstdnames(1) = 'humidity'
         ncstdnames_fallback(1) = 'relative_humidity'
      case ('dewpoint')
         ncvarnames(1) = 'd2m' ! dew-point temperature
         ncstdnames(1) = 'dew_point_temperature'
      case ('wind_speed')
         ncstdnames(1) = 'wind_speed'
      case ('wind_from_direction')
         ncstdnames(1) = 'wind_from_direction'
      case ('humidity_airtemperature_cloudiness')
         ncvarnames(1) = 'rhum' ! relative_humidity
         ncstdnames(1) = 'relative_humidity'
         ncvarnames(2) = 't2m' ! 2-meter air temperature
         ncstdnames(2) = 'air_temperature'
         ncvarnames(3) = 'tcc' ! cloud cover (fraction)
         ncstdnames(3) = 'cloud_area_fraction'
      case ('dewpoint_airtemperature_cloudiness')
         ncvarnames(1) = 'd2m' ! dew-point temperature
         ncstdnames(1) = 'dew_point_temperature'
         ncvarnames(2) = 't2m' ! 2-meter air temperature
         ncstdnames(2) = 'air_temperature'
         ncvarnames(3) = 'tcc' ! cloud cover (fraction)
         ncstdnames(3) = 'cloud_area_fraction'
      case ('dewpoint_airtemperature_cloudiness_solarradiation')
         ncvarnames(1) = 'd2m' ! dew-point temperature
         ncstdnames(1) = 'dew_point_temperature'
         ncvarnames(2) = 't2m' ! 2-meter air temperature
         ncstdnames(2) = 'air_temperature'
         ncvarnames(3) = 'tcc' ! cloud cover (fraction)
         ncstdnames(3) = 'cloud_area_fraction'
         ncvarnames(4) = 'ssr' ! outgoing SW radiation at the top-of-the-atmosphere
         ncstdnames(4) = 'surface_net_downward_shortwave_flux'
      case ('solarradiation')
         ncvarnames(1) = 'ssr' ! outgoing SW radiation at the top-of-the-atmosphere
         ncstdnames(1) = 'surface_net_downward_shortwave_flux'
         ncstdnames_fallback(1) = 'solar_irradiance'
      case ('longwaveradiation')
         ncvarnames(1) = 'strd' ! outgoing long wave radiation
         ncstdnames(1) = 'surface_net_downward_longwave_flux'
      case ('nudge_salinity_temperature')
         ncvarnames(1) = 'thetao' ! temperature
         ncstdnames(1) = 'sea_water_potential_temperature'
         ncvarnames(2) = 'so' ! salinity
         ncstdnames(2) = 'sea_water_salinity'
      case ('sea_ice_area_fraction', 'sea_ice_thickness')
         ncstdnames(1) = quantityName
      case ('friction_coefficient_time_dependent')
         ncvarnames(1) = 'friction_coefficient'
         ncstdnames(1) = 'friction_coefficient'
      case ('wavesignificantheight')
         ncvarnames(1) = 'hs' ! significant wave height
         ncstdnames(1) = 'sea_surface_wave_significant_height'
      case ('waveperiod')
         ncvarnames(1) = varname ! wave period
         ncstdnames(1) = varname
      case ('wavedirection')
         ncvarnames(1) = 'theta0'
         ncstdnames(1) = 'sea_surface_wave_from_direction'
         ncvarnames(2) = 'hs'
         ncstdnames(2) = 'sea_surface_wave_significant_height'
      case ('xwaveforce')
         ncvarnames(1) = 'xfor'
         ncstdnames(1) = 'eastward_wave_force'
      case ('ywaveforce')
         ncvarnames(1) = 'yfor'
         ncstdnames(1) = 'northward_wave_force'
      case ('wavebreakerdissipation')
         ncvarnames(1) = 'ssurf'
         ncstdnames(1) = 'depth_induced_surf_breaking_energy_dissipation'
      case ('whitecappingdissipation')
         ncvarnames(1) = 'swcap'
         ncstdnames(1) = 'whitecapping_energy_dissipation'
      case ('totalwaveenergydissipation')
         ncvarnames(1) = 'dissip'
         ncstdnames(1) = 'total_energy_dissipation'
      case default ! experiment: gather miscellaneous variables from an NC-file,
         if (index(quantityName, 'waqsegmentfunction') == 1) then
            ncvarnames(1) = quantityName
            ncstdnames(1) = quantityName
         else if (index(quantityName, 'initialtracer') == 1) then
            ncvarnames(1) = quantityName(14:)
            ncstdnames(1) = quantityName(14:)
         else
            ! we have faulty
            call setECMessage("Quantity '"//trim(quantityName)//"', requested from file "//trim(fileReaderPtr%filename)//", unknown.")
            !TODO: user defined quantity name
            !ncvarnames(1) = varname
            !ncstdnames(1) = varname
         end if
      end select

      ! ------------------------------------------------------------------------------------------------
      ! Inquiry of the dimids and the varids of lon/lat/time coordinate according to the CF-convention
      ! Lateron we can match the dimids to the dimids of the variable

      ! For now not sure yet if we need this call.
      if (.not. ecSupportNCFindCFCoordinates(fileReaderPtr%fileHandle, lon_varid, lon_dimid, lat_varid, lat_dimid, &
                                             grid_lon_varid, grid_lat_varid, &
                                             x_varid, x_dimid, y_varid, y_dimid, &
                                             z_varid, z_dimid, &
                                             tim_varid, tim_dimid, &
                                             nod_varid, nod_dimid, &
                                             realization_varid, realization_dimid)) then
         ! Exception: inquiry of id's of required coordinate variables failed
         return
      end if

      ! if no varid id for stations was found through the cf_role=timeseriesid criterion there is an alternative
      ! way to discover timeseries. Remove this to make it more strict: always demand a cf_role attribute
      if (nod_dimid < 0) then
         if (lon_dimid > 0 .and. lon_dimid == lat_dimid) nod_dimid = lon_dimid ! stations with lon/lat
         if (x_dimid > 0 .and. x_dimid == y_dimid) nod_dimid = x_dimid ! stations with x/y
      end if

      expectedLength = count(ncstdnames > ' ')

      ! Fill a string array with user-defined variable names
      if (len_trim(varname) > 0) then
         if (index(trim(varname), ' ') > 0) then
            call strsplit(varname, 1, nccustomnames, 1)
         else
            call realloc(nccustomnames, 1)
            nccustomnames(1) = varname
         end if

         if (size(nccustomnames) /= expectedLength) then
            write (cnum1, '(i2)') expectedLength
            write (cnum2, '(i2)') size(ncvarnames)
            call setECMessage("Quantity '"//trim(quantityName)//"' should have"//cnum1//' sub-names, but found'//cnum2//' in ext-file.')
         end if
      end if

      do i = 1, expectedLength
         call ecProviderSearchStdOrVarnames(fileReaderPtr, i, idvar, ncstdnames, ncvarnames, uservarnames=nccustomnames)
         if (idvar <= 0 .and. ncstdnames_fallback(1) /= ' ') then
            call ecProviderSearchStdOrVarnames(fileReaderPtr, i, idvar, ncstdnames_fallback, ncvarnames)
         end if
         if (idvar <= 0) then ! Variable not found among standard names and variable names either
            if (allocated(nccustomnames)) then
               nameVar = trim(nccustomnames(i))
            else
               nameVar = trim(ncvarnames(i))
            end if
            call setECMessage("Variable '"//nameVar//"' not found in NetCDF file '"//trim(fileReaderPtr%filename))
            return
         end if
         fileReaderPtr%standard_names(idvar) = ncstdnames(i) ! overwrite the standardname by the one required

         ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, idvar, ndims=ndims) ! get the number of dimensions
         if (allocated(coordids)) then
            deallocate (coordids) ! allocate space for the variable id's
         end if
         allocate (coordids(ndims)) ! .. representing the var's coordinates
         coordids = -1
         if (allocated(dimids)) then
            deallocate (dimids)
         end if
         allocate (dimids(ndims))
         ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, idvar, dimids=dimids) ! get dimension ID's

         do idims = 1, ndims ! JRE to check
            coordids(idims) = fileReaderPtr%dim_varids(dimids(idims))
         end do

         fgd_id = -1
         sgd_id = -1
         if (instancePtr%coordsystem == EC_COORDS_CARTESIAN) then
            if (nod_dimid > 0) then
               grid_type = elmSetType_samples
            else
               grid_type = elmSetType_cartesian
            end if
            if (x_varid > 0) then
               fgd_id = x_varid
            end if
            if (y_varid > 0) then
               sgd_id = y_varid
            end if
         else if (instancePtr%coordsystem == EC_COORDS_SFERIC) then
            if (nod_dimid > 0) then
               grid_type = elmSetType_samples
            else
               grid_type = elmSetType_spheric
            end if
            if (lon_varid > 0) then
               fgd_id = lon_varid
            end if
            if (lat_varid > 0) then
               sgd_id = lat_varid
            end if
            if ((grid_lon_varid > 0) .and. (grid_lat_varid > 0) .and. (fgd_id < 0 .or. sgd_id < 0)) then ! ... then try relative (rotated-pole-) lon and lat
               fgd_id = grid_lon_varid
               sgd_id = grid_lat_varid
               grid_mapping = ''
               ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar, "grid_mapping", grid_mapping) ! check if there is a gridmapping variable for this var
               if (len_trim(grid_mapping) > 0) then
                  ierror = nf90_inq_varid(fileReaderPtr%fileHandle, grid_mapping, grid_mapping_id)
                  if (ierror == NF90_NOERR) then
                     gsplon = -999.9
                     gsplat = -999.9
                     gnplon = -999.9
                     gnplat = -999.9
                     attstr = ''
                     ierror = nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_mapping_name", attstr)
                     if (attstr == 'rotated_latitude_longitude') then
                        if (.not. (nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_north_pole_longitude", gnplon) == NF90_NOERR)) gnplon = -999.9
                        if (.not. (nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_north_pole_latitude", gnplat) == NF90_NOERR)) gnplat = -999.9
                        if (.not. (nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_south_pole_longitude", gsplon) == NF90_NOERR)) gsplon = -999.9
                        if (.not. (nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_south_pole_latitude", gsplat) == NF90_NOERR)) gsplat = -999.9
                        if ((gnplon > -900.d0) .and. (gnplat > -900.d0)) then ! northpole given
                           gsplon = gnplon + 180.d0
                           gsplat = -gnplat
                        end if
                        if ((gsplon > -900.d0) .and. (gsplat > -900.d0)) then ! southpole given
                           rotate_pole = .true.
                        end if
                     end if
                  end if
               end if
            end if
         else
            call setECMessage("coordsystem is not set for EC instance")
            return
         end if

         tgd_id = z_varid

         ! If we failed to read all coordinate variable id's from the dimension variable id's,
         ! inspect the coordinate attribute string
         ! The contents of the coordinate string OVERRULE the id's of coordinate variables (i.e. fgd_id, sgd_id, tgd_id set above)
         coord_name = ''
         ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar, "coordinates", coord_name) ! get coordinates attribute
         if (len_trim(coord_name) > 0) then
            if (allocated(coord_names)) deallocate (coord_names)
            call strsplit(coord_name, 1, coord_names, 1)
            do j = 1, size(coord_names)
               if (len_trim(coord_names(j)) > 0) then
                  call ecProviderSearchStdOrVarnames(fileReaderPtr, j, varid, ncvarnames=coord_names, ignore_case=.true.)
                  if (varid < 0) then
                     call setECMessage("Variable '"//trim(ncstdnames(i))//"' in NetCDF file '"//trim(fileReaderPtr%filename) &
                                       //' coordinates variable '//trim(coord_names(2))//' referenced but not found')
                  else
                     if (instancePtr%coordsystem == EC_COORDS_CARTESIAN) then
                        if (strcmpi(fileReaderPtr%standard_names(varid), 'projection_x_coordinate')) then
                           fgd_id = varid
                        end if
                        if (strcmpi(fileReaderPtr%standard_names(varid), 'projection_y_coordinate')) then
                           sgd_id = varid
                        end if
                     end if
                     if (instancePtr%coordsystem == EC_COORDS_SFERIC) then
                        if (strcmpi(fileReaderPtr%standard_names(varid), 'longitude')) then
                           fgd_id = varid
                        end if
                        if (strcmpi(fileReaderPtr%standard_names(varid), 'latitude')) then
                           sgd_id = varid
                        end if
                     end if
                  end if
               end if
            end do
         end if ! has non-empty coordinates attribute

         if (fgd_id < 0 .or. sgd_id < 0) then
            if (instancePtr%coordsystem == EC_COORDS_CARTESIAN) then
               call setECMessage("Variable '"//trim(ncstdnames(i))//"' in NetCDF file '"//trim(fileReaderPtr%filename) &
                                 //' requires ''projection_x_coordinate'' and ''projection_y_coordinate''.')
            end if
            if (instancePtr%coordsystem == EC_COORDS_SFERIC) then
               call setECMessage("Variable '"//trim(ncstdnames(i))//"' in NetCDF file '"//trim(fileReaderPtr%filename) &
                                 //' either requires ''latitude'' and ''longitude'' or ''grid_latitude'' and ''grid_longitude''.')
            end if
            return
         end if

         ! =========================================
         ! Create the ElementSet for this quantity
         ! =========================================
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (grid_type == ec_undef_int) then
            dummy = ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, 0)
         else
            if (allocated(fgd_data)) deallocate (fgd_data)
            if (allocated(sgd_data)) deallocate (sgd_data)
            if (allocated(fgd_data_1d)) deallocate (fgd_data_1d)
            if (allocated(sgd_data_1d)) deallocate (sgd_data_1d)

            call realloc(crd_dimids, ndims, 3)
            call realloc(crd_dimlen, ndims, 3)
            crd_dimids = 0
            crd_dimlen = 0

            ! Dimensions ID's and dimension lengths of the FIRST coordinate variable
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, fgd_id, ndims=ndims)
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, fgd_id, dimids=crd_dimids(1:ndims, 1)) ! count dimensions of the first coordinate variable
            do idims = 1, ndims
               crd_dimlen(idims, 1) = fileReaderPtr%dim_length(crd_dimids(idims, 1))
            end do

            ! Dimensions ID's and dimension lengths of the SECOND coordinate variable
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, sgd_id, ndims=ndims)
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, sgd_id, dimids=crd_dimids(1:ndims, 2)) ! count dimensions of the first coordinate variable
            do idims = 1, ndims
               crd_dimlen(idims, 2) = fileReaderPtr%dim_length(crd_dimids(idims, 2))
            end do

            ! Dimensions ID's and dimension lengths of the THIRD coordinate variable
            if (crd_dimids(idims, 3) == 0) then
               crd_dimlen(:, 3) = 0
            else
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, tgd_id, ndims=ndims)
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, tgd_id, dimids=crd_dimids(1:ndims, 3)) ! count dimensions of the first coordinate variable
               do idims = 1, ndims
                  crd_dimlen(idims, 3) = fileReaderPtr%dim_length(crd_dimids(idims, 3))
               end do
            end if

            ! Check if the dimension(sizes) of the 1st and 2nd coordinate variable agree
            if (any(crd_dimlen(1:ndims, 2) /= crd_dimlen(1:ndims, 2))) then
               return
               ! TODO: error message
            end if

            ! notation: crd_dimlen(k,l) holds the size of the k-th dimension in the variable holding the l-th coordinate
            ! similar for crd_dimids

            if (ndims == 2 .or. rotate_pole) then
               allocate (fgd_data(crd_dimlen(1, 1), crd_dimlen(2, 1)))
               allocate (sgd_data(crd_dimlen(1, 2), crd_dimlen(2, 2)))
               allocate (fgd_data_1d(crd_dimlen(1, 1) * crd_dimlen(2, 1)))
               allocate (sgd_data_1d(crd_dimlen(1, 2) * crd_dimlen(2, 2)))
            end if
            if (ndims == 1) then
               allocate (fgd_data_1d(crd_dimlen(1, 1)))
               allocate (sgd_data_1d(crd_dimlen(1, 2)))
               if (grid_type == elmSetType_spheric) grid_type = elmSetType_spheric_ortho
               if (grid_type == elmSetType_Cartesian) grid_type = elmSetType_Cartesian_ortho
            end if

            if (ndims == 2) then
               ierror = nf90_get_var(fileReaderPtr%fileHandle, fgd_id, fgd_data, start=(/1, 1/), count=crd_dimlen(1:2, 1))
               ierror = nf90_get_var(fileReaderPtr%fileHandle, sgd_id, sgd_data, start=(/1, 1/), count=crd_dimlen(1:2, 2))
               fgd_data_1d = reshape(fgd_data, (/crd_dimlen(1, 1) * crd_dimlen(2, 1)/)) ! transform fgd and sgd here if necessary
               sgd_data_1d = reshape(sgd_data, (/crd_dimlen(1, 2) * crd_dimlen(2, 2)/))
            else if (ndims == 1) then
               ierror = nf90_get_var(fileReaderPtr%fileHandle, fgd_id, fgd_data_1d(1:crd_dimlen(1, 1)), start=(/1/), count=(/crd_dimlen(1, 1)/))
               ierror = nf90_get_var(fileReaderPtr%fileHandle, sgd_id, sgd_data_1d(1:crd_dimlen(1, 2)), start=(/1/), count=(/crd_dimlen(1, 2)/))
               ! Make a crossproduct array
               if (rotate_pole) then
                  do ifgd = 1, crd_dimlen(1, 1)
                     do isgd = 1, crd_dimlen(1, 2)
                        sgd_data(ifgd, isgd) = sgd_data_1d(isgd)
                        fgd_data(ifgd, isgd) = fgd_data_1d(ifgd)
                     end do
                  end do
                  fgd_data_1d = reshape(fgd_data, (/crd_dimlen(1, 1) * crd_dimlen(2, 1)/)) ! transform fgd and sgd here if necessary
                  sgd_data_1d = reshape(sgd_data, (/crd_dimlen(1, 2) * crd_dimlen(2, 2)/))
               end if
            else
               ! Something wrong with the coordinate dimensions
            end if

            ! Scale metric coordinates if needed:
            units = ''
            ierror = nf90_get_att(fileReaderPtr%fileHandle, fgd_id, 'units', units)
            if (ierror == nf90_noerr .and. (strcmpi(units, 'km') .or. strcmpi(units, 'kilometer') .or. strcmpi(units, 'kilometre'))) then
               fgd_data_1d = fgd_data_1d * 1000d0
            end if
            units = ''
            ierror = nf90_get_att(fileReaderPtr%fileHandle, sgd_id, 'units', units)
            if (ierror == nf90_noerr .and. (strcmpi(units, 'km') .or. strcmpi(units, 'kilometer') .or. strcmpi(units, 'kilometre'))) then
               sgd_data_1d = sgd_data_1d * 1000d0
            end if

            if (.not. ecElementSetSetType(instancePtr, elementSetId, grid_type)) then
               return
            end if

            if (realization_dimid > 0) then
               dim_offset = 1
               nrel = fileReaderPtr%dim_length(dimids(1))
            else
               dim_offset = 0
               nrel = 0
            end if

            ! this goes wrong when time is defined before space in nc file
            if (grid_type == elmSetType_samples) then
               ncol = fileReaderPtr%dim_length(dimids(1))
               nrow = 1
               nlay = crd_dimlen(1, 3)
            else
               ncol = fileReaderPtr%dim_length(fileReaderPtr%lonx_id)
               nrow = 1
               nlay = 0
               if (size(dimids) >= 2) then
                  nrow = fileReaderPtr%dim_length(fileReaderPtr%laty_id)
                  if (size(dimids) > 3 + dim_offset) then
                     nlay = fileReaderPtr%dim_length(dimids(3 + dim_offset))
                  end if
               end if
            end if
            if (.not. ecElementSetSetRowsColsLayers(instancePtr, elementSetId, nrow, ncol, nlay)) then
               return
            end if
            if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nrow * ncol)) then
               return
            end if
            if (.not. ecElementSetSetType(instancePtr, elementSetId, grid_type)) then
               return
            end if
            select case (grid_type)
            case (elmSetType_samples)
               if (.not. (ecElementSetSetXArray(instancePtr, elementSetId, fgd_data_1d) .and. &
                          ecElementSetSetYArray(instancePtr, elementSetId, sgd_data_1d))) then
                  return
               end if
            case (elmSetType_cartesian, elmSetType_cartesian_ortho)
               if (.not. (ecElementSetSetXArray(instancePtr, elementSetId, fgd_data_1d) .and. &
                          ecElementSetSetYArray(instancePtr, elementSetId, sgd_data_1d))) then
                  return
               end if
            case (elmSetType_spheric, elmSetType_spheric_ortho)
               if (allocated(fgd_data_trans)) deallocate (fgd_data_trans)
               if (allocated(sgd_data_trans)) deallocate (sgd_data_trans)
               if (allocated(pdiri)) deallocate (pdiri)
               allocate (fgd_data_trans(crd_dimlen(1, 1) * crd_dimlen(2, 1)))
               allocate (sgd_data_trans(crd_dimlen(1, 2) * crd_dimlen(2, 2)))
               if (.not. ecElementSetSetType(instancePtr, elementSetId, grid_type)) then
                  call setECMessage("Setting element type failed for "//trim(fileReaderPtr%filename)//".")
                  return
               end if

               if (rotate_pole) then
                  if (allocated(pdiri)) deallocate (pdiri)
                  allocate (pdiri(size(fgd_data_1d)))
                  call gb2lla(fgd_data_1d, sgd_data_1d, fgd_data_trans, sgd_data_trans, pdiri, size(fgd_data_1d), &
                              gsplon, gsplat, 0.0_hp, 0.0_hp, -90.0_hp, 0.0_hp)
                  if (.not. ecElementSetSetXArray(instancePtr, elementSetId, fgd_data_trans)) then
                     call setECMessage("Setting latitude array failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
                  if (.not. ecElementSetSetYArray(instancePtr, elementSetId, sgd_data_trans)) then
                     call setECMessage("Setting longitude array failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
                  if (.not. ecElementSetSetDirectionArray(instancePtr, elementSetId, pdiri)) then
                     call setECMessage("Setting rotation array for vector for transformed vector quantities failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
               else
                  if (.not. ecElementSetSetXArray(instancePtr, elementSetId, fgd_data_1d)) then
                     call setECMessage("Setting longitude array failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
                  if (.not. ecElementSetSetYArray(instancePtr, elementSetId, sgd_data_1d)) then
                     call setECMessage("Setting latitude array failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
               end if
            end select

            if (nlay > 0) then
               if (allocated(tgd_data_1d)) deallocate (tgd_data_1d)
               allocate (tgd_data_1d(nlay))
               ierror = nf90_get_var(fileReaderPtr%fileHandle, tgd_id, tgd_data_1d, start=(/1/), count=(/nlay/))
               z_positive = ''
               ierror = nf90_get_att(fileReaderPtr%fileHandle, tgd_id, "positive", z_positive)
               z_standardname = fileReaderPtr%standard_names(z_varid)
               call str_lower(z_standardname)
               call str_lower(z_positive)
               ! Set the vptyp of the elementset
               select case (z_standardname)
               case ('depth') ! absolute depth below geoid
                  vptyp = BC_VPTYP_ZDATUM
                  if (z_positive == 'down') vptyp = BC_VPTYP_ZDATUM_DOWN
               case ('ocean_sigma_coordinate', 'ocean_sigma_z_coordinate') ! relative vertical coordinate
                  vptyp = BC_VPTYP_PERCBED
                  if (z_positive == 'down') vptyp = BC_VPTYP_PERCSURF
               case ('hybrid_height')
               case default
                  call setECMessage("Setting Z-array failed for "//trim(fileReaderPtr%filename)//".")
                  return
               end select
               if (.not. ecElementSetSetProperties(instancePtr, elementSetId, vptyp=vptyp)) then
                  return
               end if
               if (.not. ecElementSetSetZArray(instancePtr, elementSetId, tgd_data_1d)) then
                  call setECMessage("Setting Z-array failed for "//trim(fileReaderPtr%filename)//".")
                  return
               end if
            end if
         end if

         ! -------------------------------------------------------------------------------------------------

         ! ===================
         ! Create the Quantity
         ! ===================
         quantityId = ecInstanceCreateQuantity(instancePtr)
         select case (quantityName)
         case ('rainfall')
            timeint = timeint_rainfall
         case default
            timeint = timeint_lin
         end select
         if (.not. (ecQuantitySet(instancePtr, quantityId, name=ncstdnames(i), ncid=idvar, timeint=timeint))) return
         if (.not. (ecQuantitySetUnitsFillScaleOffsetFromNcidVarid(instancePtr, quantityId, fileReaderPtr%fileHandle, idvar))) return

         ! ========================
         ! Create the source Fields
         ! ========================
         !  --- Determine ssmissingDataValue ---
         if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, idvar, "_FillValue", var_miss), "reading _FillValue", fileReaderPtr%fileName)) then
            if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, NF90_GLOBAL, "NF90_FILL_DOUBLE", var_miss), "reading _FillValue", fileReaderPtr%fileName)) then
               var_miss = ec_undef_hp
            end if
         end if
         !
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. ecFieldSetMissingValue(instancePtr, field0Id, var_miss)) return
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. ecFieldSetMissingValue(instancePtr, field1Id, var_miss)) return

         ! ==================
         ! Create source Item
         ! ==================
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            return
         else
            itemPtr => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== finish initialization of Fields =====
         itemPtr%sourceT0FieldPtr%timesteps = ec_undef_hp
         itemPtr%sourceT1FieldPtr%timesteps = ec_undef_hp

         ! Add successfully created source Item to the FileReader
         if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPtr%id)) then
            return
         end if
      end do !                i = 1, size(ncstdnames) quantities in requested set of quantities

      success = .true.

   end function ecProviderCreateNetcdfItems

   !> Search for a single variabele index in a (NetCDF) dataset, using standard_name values, hardcoded values, or user-defined values.
   subroutine ecProviderSearchStdOrVarnames(fileReaderPtr, ncIndex, id, ncstdnames, ncvarnames, uservarnames, ignore_case)
      type(tEcFileReader), intent(in) :: fileReaderPtr !< used for input standard and variable names
      integer, intent(in) :: ncIndex !< index in list(s) ncstdnames, ncvarnames, uservarnames
      integer, intent(out) :: id !< found index in data set variable list.
      character(len=*), intent(in), optional :: ncstdnames(:) !< list with standard names to compare with. Gets last priority.
      character(len=*), intent(in), optional :: ncvarnames(:) !< list with predefined variable names to compare with. Gets second priority.
      character(len=*), intent(in), optional, allocatable :: uservarnames(:) !< list with user-specified variable names to compare with. Gets first priority.
      logical, intent(in), optional :: ignore_case !< optionally perform a case INsensitive lookup

      logical :: ic
      integer :: idvar ! loop counter
      integer :: nvar ! number/loopvariable of varids in this netcdf file

      id = -999
      ic = .false.
      if (present(ignore_case)) then
         ic = ignore_case
      end if

      nvar = size(fileReaderPtr%standard_names, dim=1)

      ! Match substituted variable names:
      if (present(uservarnames)) then
         if (allocated(uservarnames)) then
            do idvar = 1, nvar
               if (match_strings(uservarnames(ncIndex), fileReaderPtr%variable_names(idvar), ic)) then
                  id = idvar
                  return
               end if
            end do
         end if
      end if
      ! Match standard names:
      if (present(ncstdnames)) then
         do idvar = 1, nvar
            if (match_strings(ncstdnames(ncIndex), fileReaderPtr%standard_names(idvar), ic)) then
               id = idvar
               return
            end if
         end do
      end if
      ! Match variable names:
      if (present(ncvarnames)) then
         do idvar = 1, nvar
            if (match_strings(ncvarnames(ncIndex), fileReaderPtr%variable_names(idvar), ic)) then
               id = idvar
               return
            end if
         end do
      end if

   contains

      !> Determines whether two strings are equal, optionally case-INsensitive.
      function match_strings(s1, s2, ic) result(match)
         implicit none
         logical :: match
         character(len=*), intent(in) :: s1, s2 !< Input strings to be compared.
         logical, intent(in) :: ic !< Whether or not case-INsensitive comparison should be cone.
         if (ic) then
            match = strcmpi(trim(s1), trim(s2))
         else
            match = (s1 == s2)
         end if
      end function

   end subroutine ecProviderSearchStdOrVarnames

   ! =======================================================================

   !> Create source Items and their contained types, based on NetCDF file header.
   function ecProviderCreateWaveNetcdfItems(instancePtr, fileReaderPtr, quantityName) result(success)
      logical :: success !< function status
      type(tEcInstance), pointer :: instancePtr !< intent(in)
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      character(len=*), intent(in) :: quantityName !< name of quantity to read
      !
      integer :: ierror !< return value of function calls
      integer :: iddim_netelem !< id as obtained from NetCDF
      integer :: idvar_q !< id as obtained from NetCDF
      integer :: n !< number of values
      integer :: quantityId !< helper variable
      integer :: elementSetId !< helper variable
      integer :: field0Id !< helper variable
      integer :: field1Id !< helper variable
      integer :: itemId !< helper variable
      integer :: t0t1 !< indicates whether the 0 or the 1 field is read. -1: choose yourself
      logical :: local_success !< when the return flag should not be influenced
      real(hp) :: dmiss !< missing data value
      type(tEcItem), pointer :: item
      character(NF90_MAX_NAME) :: string !< read from NetCDF file
      character(300) :: message

      !
      ! body
      success = .false.
      item => null()
      dmiss = -999.0_hp
      elementSetId = ecInstanceCreateElementSet(instancePtr)
      !
      ! Cartesian or spheric
      ! WARNING: elementSetType must be set before elementSetNumberOfCoordinates (why???)
      !
      string = ' '
      ierror = nf90_get_att(fileReaderPtr%fileHandle, nf90_global, 'grid_mapping', string); success = ecSupportNetcdfCheckError(ierror, "get_att global grid_mapping", fileReaderPtr%filename)
      if (string == 'wgs84') then
         success = ecElementSetSetType(instancePtr, elementSetId, elmSetType_spheric)
      else
         success = ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)
      end if
      !
      ! Read the number of values on the NetCDF file; this should exactly match the number of cells in dflowfm
      !
      ierror = nf90_inq_dimid(fileReaderPtr%fileHandle, 'nFlowElemWithBnd', iddim_netelem); success = ecSupportNetcdfCheckError(ierror, "inq_dimid netelem", fileReaderPtr%fileName)
      ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, iddim_netelem, string, n); success = ecSupportNetcdfCheckError(ierror, "inq_dim netelem", fileReaderPtr%fileName)
      success = ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n)
      !
      ! Use quantityName
      !
      string = ' '
      ierror = nf90_inq_varid(fileReaderPtr%fileHandle, quantityName, idvar_q); success = ecSupportNetcdfCheckError(ierror, "inq_varid "//quantityName, fileReaderPtr%filename)
      ! With the the quantity name interpreted as a standard name, inquire from the filereader instance the idvar_q
      do idvar_q = 1, size(fileReaderPtr%standard_names)
         if (strcmpi(fileReaderPtr%standard_names(idvar_q), quantityName)) exit
      end do
      if (idvar_q > size(fileReaderPtr%standard_names)) then
         ! ERROR: standard name not found in this filereader, Try the variable names
         call setECMessage("No standard_name='"//trim(quantityName)//"' found in file '"//trim(fileReaderPtr%filename)//"'.")
         do idvar_q = 1, size(fileReaderPtr%variable_names)
            if (strcmpi(fileReaderPtr%variable_names(idvar_q), quantityName)) exit
         end do
         if (idvar_q > size(fileReaderPtr%standard_names)) then
            ! ERROR: variable name not found in this filereader, TODO: handle exception
            call setECMessage("No variable_name='"//trim(quantityName)//"' found in file '"//trim(fileReaderPtr%filename)//"'.")
            return
         end if
      end if

      ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar_q, 'units', string); success = ecSupportNetcdfCheckError(ierror, "inq_att "//quantityName, fileReaderPtr%filename)
      if (.not. success) return
      !
      quantityId = ecInstanceCreateQuantity(instancePtr)

      if (.not. (ecQuantitySet(instancePtr, quantityId, name=quantityName, units=trim(string), ncid=idvar_q))) then
         success = .false.
      end if
      field0Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n) .and. &
                 ecFieldSetMissingValue(instancePtr, field0Id, dmiss))) then
         success = .false.
      end if
      field1Id = ecInstanceCreateField(instancePtr)
      if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n) .and. &
                 ecFieldSetMissingValue(instancePtr, field1Id, dmiss))) then
         success = .false.
      end if
      itemId = ecInstanceCreateItem(instancePtr)
      if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                 ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                 ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                 ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                 ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                 ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
         success = .false.
      else
         item => ecSupportFindItem(instancePtr, itemId)
      end if
      !
      ! finish initialization of Fields
      ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr
      !
      if (success) then
         t0t1 = 0
         success = ecNetcdfReadBlock(fileReaderPtr, item, t0t1, n)
      end if
      if (success) then
         t0t1 = 1
         local_success = ecNetcdfReadBlock(fileReaderPtr, item, t0t1, n)
      end if
      ! Add successfully created source Item to the FileReader
      if (success) then
         success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)
      end if
      if (.not. success) then
         write (message, '(2a)') "ERROR: ec_provider::ecProviderCreateWaveNetcdfItems: unable to create sourceItem for ", quantityName
         call setECMessage(trim(message))
      end if
   end function ecProviderCreateWaveNetcdfItems

   ! =======================================================================

   !> Set the TimeFrame's properties, based on a file header when present, or to refdat and hardcoded values otherwise.
   function ecProviderInitializeTimeFrame(fileReaderPtr, k_refdate, k_timezone, k_timestep_unit, dtnodal) result(success)
      logical :: success !< function status
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
      real(hp), intent(in) :: k_refdate !< Kernel's reference date as MJD
      real(hp), intent(in) :: k_timezone !< Kernel's timezone.
      integer, intent(in) :: k_timestep_unit !< Kernel's time step unit (1=seconds, 2=minutes, 3=hours)
      real(hp), optional, intent(in) :: dtnodal !< Nodal factors update interval
      !
      success = .false.
      !
      if (k_refdate > -1) then

         fileReaderPtr%tframe%k_refdate = k_refdate
         fileReaderPtr%tframe%k_timezone = k_timezone
         fileReaderPtr%tframe%k_timestep_unit = k_timestep_unit

         fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
         fileReaderPtr%tframe%ec_timezone = fileReaderPtr%tframe%k_timezone
         fileReaderPtr%tframe%ec_timestep_unit = fileReaderPtr%tframe%k_timestep_unit

         fileReaderPtr%tframe%dtnodal = 1e+20_hp
         if (present(dtnodal)) then
            if (dtnodal /= 0.0_hp) then
               fileReaderPtr%tframe%dtnodal = dtnodal
            end if
         end if

      else
         ! no kernel ref date defined
         fileReaderPtr%tframe%k_refdate = -1
      end if

      !
      select case (fileReaderPtr%ofType)
      case (provFile_undefined)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: No file type specified.")
      case (provFile_uniform, provFile_unimagdir)
         success = ecUniInitializeTimeFrame(fileReaderPtr)
      case (provFile_svwp)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
      case (provFile_svwp_weight)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
      case (provFile_arcinfo)
         success = ecDefaultInitializeTimeFrame(fileReaderPtr)
      case (provFile_spiderweb)
         success = ecDefaultInitializeTimeFrame(fileReaderPtr)
      case (provFile_curvi)
         success = ecDefaultInitializeTimeFrame(fileReaderPtr)
      case (provFile_curvi_weight)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
      case (provFile_samples)
         ! Time independent: ec timeframe params were already defaulted to kernel's timeframe params (above).
         success = .true.
      case (provFile_triangulationmagdir)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
      case (provFile_qhtable)
         fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
         ! The nr_timesteps is infinity.
         success = .true.
      case (provFile_poly_tim)
         ! TODO : For now the top-level fileReader (*.pli) needs no TimeFrame.
         fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
         ! TODO : The total number of available timesteps is not yet known here.
         success = .true.
      case (provFile_fourier)
         ! Boundary condition components file containing: either period[minute] or component name, amplitude[m], phase[degree].
         ! Time_unit explicit in the Converter, as values at a moment in time are calculated from the seeds in the file, not read from file as records.
         fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
         ! The nr_timesteps is infinity, as values at a moment in time are calculated from the seeds in the file, not read from file as records.
         success = .true.
      case (provFile_grib)
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
      case (provFile_netcdf)
         success = ecNetcdfInitializeTimeFrame(fileReaderPtr)
         if (.not. success) then
            success = ecNetcdfInitializeHarmonicsFrame(fileReaderPtr)
         end if
         if (.not. success) then
            call setECMessage('ERROR: ec_provider::ecProviderInitializeTimeFrame: Failed to initialize from NetCDF file.')
         end if
      case (provFile_t3D)
         success = ecDefaultInitializeTimeFrame(fileReaderPtr)
      case (provFile_bc)
         ! Filereader was created thru a BC-instance. This instance has a timeunit (netcdf-style) property
         if (fileReaderPtr%bc%func == BC_FUNC_TSERIES .or. fileReaderPtr%bc%func == BC_FUNC_TIM3D) then
            success = ecSupportTimestringToUnitAndRefdate(fileReaderPtr%bc%timeunit, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate, &
                                                          tzone=fileReaderPtr%tframe%ec_timezone)
         else
            success = .true.
         end if
      case default
         call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unknown file type.")
      end select
   end function ecProviderInitializeTimeFrame

   ! =======================================================================

   !> Set the TimeFrame's properties, based on a default time string format.
      !! meteo1.f90: reaspwtim
   function ecDefaultInitializeTimeFrame(fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
      !
      character(len=maxNameLen) :: keyword !< helper variable
      character(len=maxNameLen) :: rec !< helper variable
      character(len=maxNameLen) :: prev_rec !< helper variable
      !
      success = .false.
      keyword = 'TIME'
      prev_rec = ' '
      !
      rewind (unit=fileReaderPtr%fileHandle)
      !
      rec = ecFindInFile(fileReaderPtr%fileHandle, keyword)
      if (len_trim(rec) > 0) then
         ! Determine the timestep unit and reference date for the time data in the file.
         if (.not. ecSupportTimestringToUnitAndRefdate(rec, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate, &
                                                       tzone=fileReaderPtr%tframe%ec_timezone)) return
      else
         call setECMessage("ERROR: ec_provider::ecDefaultInitializeTimeFrame: Unable to identify the first data block.")
         return
      end if
      ! =========================================
      ! Determine the total number of time steps.
      ! =========================================
         !!! This read action is too slow on multi-gigabyte files. Commented for now.
         !!!do
         !!!   prev_rec = rec
         !!!   rec = ecFindInFile(fileReaderPtr%fileHandle, keyword)
         !!!   if (len_trim(rec) == 0) exit
         !!!end do
         !!!if (len_trim(prev_rec) /= 0) then
         !!!   ! Read and convert the timesteps to seconds.
         !!!   if (.not. ecGetTimesteps(prev_rec, time_steps, .false.)) return
         !!!   ! Store the total number of available timesteps in the ecFileReader's ecTimeFrame.
         !!!   fileReaderPtr%tframe%nr_timesteps = time_steps
         !!!else
         !!!   call setECMessage("ERROR: ec_provider::ecDefaultInitializeTimeFrame: Unable to read the stop time.")
         !!!end if
      success = .true.
   end function ecDefaultInitializeTimeFrame

   ! =======================================================================

   !> Set the TimeFrame's properties, based on a uni* file's header.
      !! Headerless column-based ASCII file.
      !! uniform: time[minute], wind_x[m s-1](, wind_y[m s-1])
      !! unimagdir: time[minute], windspeed[m s-1], winddirection[degree]
   function ecUniInitializeTimeFrame(fileReaderPtr) result(success)
      logical :: success !< function status
      type(tEcFileReader), pointer :: fileReaderPtr !< FileReader to initialize
      !
      success = .false.
      fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
      ! Obtain the total number of timesteps in this file.
      rewind (unit=fileReaderPtr%fileHandle)
      success = .true.
   end function ecUniInitializeTimeFrame

   ! =======================================================================

   !> Set the TimeFrame's properties, based on a NetCDF file.
   function ecNetcdfInitializeTimeFrame(fileReaderPtr) result(success)
      use netcdf
      !
      logical :: success !< function status
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
      !
      integer :: nVariables !< number of variables in NetCDF file
      integer :: time_id !< integer id of variable with standard_name "time"
      integer :: i !< loop counter
      character(len=NF90_MAX_NAME) :: units !< units attribute of a variable
      integer, dimension(1) :: dimid !< integer id of time variable's dimension variable
      integer :: length !< number of time steps
      integer :: istat !< status of allocation operation
      !
      success = .false.
      nVariables = 0
      time_id = ec_undef_int
      !
      if (.not. ecSupportNetcdfCheckError(nf90_inquire(fileReaderPtr%fileHandle, nVariables=nVariables), "obtain nVariables", fileReaderPtr%fileName)) return
      !
      ! Inspect the standard_name attribute of all variables to find "time" and store that variable's id.
      nVariables = size(fileReaderPtr%variable_names)
      do i = 1, nVariables ! check the standard names for TIME
         if (strcmpi(fileReaderPtr%standard_names(i), 'TIME')) then
            time_id = i
            exit
         end if
      end do
      if (i > nVariables) then
         do i = 1, nVariables ! .... if not found, check variable names for TIME ....
            if (strcmpi(fileReaderPtr%variable_names(i), 'TIME')) then
               time_id = i
               exit
            end if
         end do
         if (i > nVariables) then ! .... if still not found, you are out of luck !
            return
         end if
      end if
      !
      ! Determine the timestep unit and reference date for the time data in the NetCDF file.
      ! Surprisingly, the reference date is part of the "units" attribute.
      units = '' ! NetCDF does not completely overwrite a string, so re-initialize.
      if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, time_id, "units", units), "obtain units", fileReaderPtr%fileName)) return
      if (.not. ecSupportTimestringToUnitAndRefdate(units, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate, &
                                                    tzone=fileReaderPtr%tframe%ec_timezone)) return
      !
      ! Determine the total number of timesteps.
      if (.not. ecSupportNetcdfCheckError(nf90_inquire_variable(fileReaderPtr%fileHandle, time_id, dimids=dimid), "obtain time dimension ids", fileReaderPtr%fileName)) return
      if (.not. ecSupportNetcdfCheckError(nf90_inquire_dimension(fileReaderPtr%fileHandle, dimid(1), len=length), "obtain time dimension length", fileReaderPtr%fileName)) return
      fileReaderPtr%tframe%nr_timesteps = length
      allocate (fileReaderPtr%tframe%times(length), stat=istat)
      ! Store the times at which data is available.
      if (.not. ecSupportNetcdfCheckError(nf90_get_var(fileReaderPtr%fileHandle, time_id, fileReaderPtr%tframe%times, start=(/1/), count=(/length/)), "obtain time data", fileReaderPtr%fileName)) return
      !
      success = .true.
   end function ecNetcdfInitializeTimeFrame

   ! =======================================================================

   !> Get index of supplied variable name
   function ecNetcdfFindVariableId(fileReaderPtr, variable_name) result(variable_id)
      use netcdf
      !
      integer :: variable_id !< variable id of name in file, ec_undef_int if not found.
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(in) File being read.
      character(*), intent(in) :: variable_name !< variable name to find.
      !
      integer :: nVariables !< number of variables in NetCDF file
      integer :: i !< loop variable
      !
      variable_id = ec_undef_int
      !
      if (.not. ecSupportNetcdfCheckError(nf90_inquire(fileReaderPtr%fileHandle, nVariables=nVariables), "obtain nVariables", fileReaderPtr%fileName)) return
      !
      ! Inspect the standard_name attribute of all variables to find and store that variable's id.
      !
      ! check the standard names for the requested name
      nVariables = size(fileReaderPtr%standard_names)
      do i = 1, nVariables
         if (strcmpi(fileReaderPtr%standard_names(i), variable_name)) then
            variable_id = i
            return
         end if
      end do
      ! .... if not found, check variable names ....
      nVariables = size(fileReaderPtr%variable_names)
      do i = 1, nVariables
         if (strcmpi(fileReaderPtr%variable_names(i), variable_name)) then
            variable_id = i
            return
         end if
      end do
   end function ecNetcdfFindVariableId

   !> Set the HarmonicsFrame's properties, based on a NetCDF file.
   function ecNetcdfInitializeHarmonicsFrame(fileReaderPtr) result(success)
      use netcdf
      !
      logical :: success !< function status
      type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
      !
      integer :: phase_id !< integer id of variable with standard_name "phase"
      character(len=NF90_MAX_NAME) :: units !< units attribute of a variable
      character(len=NF90_MAX_NAME) :: attrstring !< global attribute
      integer :: period !< period value in seconds.
      integer, dimension(2) :: dimids !< integer id's of amplitude/phase variable's dimension variables eg: phase(y,x) -> id's of y and x.
      integer :: numids !< number of variable id's of amplitude/phase (we expect 2: y,x)
      integer, dimension(2) :: phase_dims !< number of points in Y,X directions.
      integer :: istat !< status of allocation operation
      !
      success = .false.
      !
      ! Find the required 'phase' variable.
      phase_id = ecNetcdfFindVariableId(fileReaderPtr, 'PHASE')
      if (phase_id == ec_undef_int) return

      ! Set reference date
      attrstring = '' ! NetCDF does not completely overwrite a string, so re-initialize.
      if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, 0, "reference_time_of_component_phase", attrstring), &
                                          "obtain reference time", fileReaderPtr%fileName)) return
      if (.not. ecSupportTimestringToRefdate(attrstring, fileReaderPtr%tframe%ec_refdate, tzone=fileReaderPtr%tframe%ec_timezone)) return

      ! Extract period from attributes.
      ! TODO: Make this an OPTIONAL thing and maybe revert to astro as a fallback. (for now, we expect it to be there)
      attrstring = '' ! NetCDF does not completely overwrite a string, so re-initialize.
      if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, 0, "component_period_in_seconds", attrstring), &
                                          "obtain component period", fileReaderPtr%fileName)) return
      allocate (fileReaderPtr%hframe, stat=istat)
      if (istat /= 0) return

      ! We assume the unit to be seconds for now.
      ! TODO: Add iostat and support other units? (See ecGetTimesteps() for inspiration)
      read (attrstring, *) period
      fileReaderPtr%hframe%ec_period = period

      ! Determine number of timesteps: For harmonic components, the number of timesteps is not properly defined.
      fileReaderPtr%tframe%nr_timesteps = 0

      ! Get phase units
      units = '' ! NetCDF does not completely overwrite a string, so re-initialize.
      if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, phase_id, "units", units), "obtain phase units", fileReaderPtr%fileName)) return
      if (units /= 'deg') then
         call setECMessage('ERROR: Phase unit is not degrees.')
         return
      end if

      ! Get phase dimension variables
      if (.not. ecSupportNetcdfCheckError(nf90_inquire_variable(fileReaderPtr%fileHandle, phase_id, ndims=numids, dimids=dimids), "obtain phase dimension ids", fileReaderPtr%fileName)) return
      if (numids /= 2) then
         call setECMessage('ERROR: Phase does not have exactly two dimensions.')
         return
      end if

      ! Get phase dimension sizes
      if (.not. ecSupportNetcdfCheckError(nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(1), len=phase_dims(1)), "obtain first phase dimension length", fileReaderPtr%fileName)) return
      if (.not. ecSupportNetcdfCheckError(nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(2), len=phase_dims(2)), "obtain second phase dimension length", fileReaderPtr%fileName)) return
      fileReaderPtr%hframe%phase_dims = phase_dims

      ! Allocate required buffer.
      allocate (fileReaderPtr%hframe%phases(phase_dims(1), phase_dims(2)), stat=istat)
      if (istat /= 0) then
         call setECMessage('ERROR: Failed to allocate phase array.')
         return
      end if

      ! Load phase data.
      if (.not. ecSupportNetcdfCheckError(nf90_get_var(fileReaderPtr%fileHandle, phase_id, fileReaderPtr%hframe%phases, start=(/1, 1/), count=phase_dims), "obtain phase data", fileReaderPtr%fileName)) return

      success = .true.
   end function ecNetcdfInitializeHarmonicsFrame

   ! =======================================================================

   !> Read the header of a Arcinfo file.
      !! Does not check whether all expected information is present in the header.
      !! meteo1:: readarcinfoheader
   function readarcinfoheader(minp, num_columns, num_rows, x0, y0, dxa, dya, dmiss) result(success)
      logical :: success !< function status
      integer :: minp !<
      integer, intent(out) :: num_columns !<
      integer, intent(out) :: num_rows !<
      real(hp), intent(out) :: x0 !<
      real(hp), intent(out) :: y0 !<
      real(hp), intent(out) :: dxa !<
      real(hp), intent(out) :: dya !<
      real(hp), intent(out) :: dmiss !<
      !
      integer :: jacornerx
      integer :: jacornery
      integer :: k
      integer :: l
      integer :: equal_sign_index
      character(len=maxNameLen) :: rec
      !
      success = .false.
      jacornerx = 0
      jacornery = 0
      !
      rewind (unit=minp)
10    continue
      read (minp, '(A)', end=100) rec
      num_columns = -1
      num_rows = -1
      if (index(rec, '### START OF HEADER') > 0) then ! new d3dflow header
20       continue
         read (minp, '(A)', end=100) rec
         equal_sign_index = index(rec, '=')
         L = equal_sign_index + 1
         !
         if (index(rec, 'NODATA_value') > 0 .and. index(rec, 'NODATA_value') < equal_sign_index) then
            read (rec(L:), *, err=106) dmiss
         end if
         !
         if (index(rec, 'n_cols') > 0 .and. index(rec, 'n_cols') < equal_sign_index) then
            read (rec(L:), *, err=101) num_columns
         end if
         !
         if (index(rec, 'n_rows') > 0 .and. index(rec, 'n_rows') < equal_sign_index) then
            read (rec(L:), *, err=102) num_rows
         end if
         !
         if (index(rec, 'x_llcenter') > 0 .and. index(rec, 'x_llcenter') < equal_sign_index) then
            read (rec(L:), *, err=103) x0
         end if
         !
         if (index(rec, 'x_llcorner') > 0 .and. index(rec, 'x_llcorner') < equal_sign_index) then
            read (rec(L:), *, err=103) x0
            jacornerx = 1
         end if
         !
         if (index(rec, 'y_llcenter') > 0 .and. index(rec, 'y_llcenter') < equal_sign_index) then
            read (rec(L:), *, err=104) y0
         end if
         !
         if (index(rec, 'y_llcorner') > 0 .and. index(rec, 'y_llcorner') < equal_sign_index) then
            read (rec(L:), *, err=104) y0
            jacornery = 1
         end if
         !
         if (index(rec, 'dx') > 0 .and. index(rec, 'dx') < equal_sign_index) then
            read (rec(L:), *, err=105) dxa
         end if
         !
         if (index(rec, 'dy') > 0 .and. index(rec, 'dy') < equal_sign_index) then
            read (rec(L:), *, err=105) dya
         end if
         !
         if (index(rec, '### END OF HEADER') == 0) then ! new d3dflow header
            goto 20
         end if
      else
         !
         if (rec(1:1) == '*' .or. rec(2:2) == '*') goto 10
         read (rec(13:), *, err=101) num_columns
         read (minp, '(A)', end=100) rec
         read (rec(13:), *, err=102) num_rows
         !
         read (minp, '(A)', end=100) rec
         read (rec(13:), *, err=103) x0
         jacornerx = 0
         if (index(rec, 'corner') /= 0) jacornerx = 1
         !
         read (minp, '(A)', end=100) rec
         read (rec(13:), *, err=104) y0
         jacornery = 0
         if (index(rec, 'corner') /= 0) jacornery = 1
         !
         read (minp, '(A)', end=100) rec
         l = index(rec, 'cellsize') + 8
         k = numbersonline(rec(l:))
         if (k == 1) then
            read (rec(13:), *, err=105) dxa
            dya = dxa
            jacornery = jacornerx
         else
            read (rec(13:), *, err=105) dxa, dya
         end if
         read (minp, '(A)', end=100) rec
         read (rec(13:), *, err=106) dmiss
         !
         ! Data in an arcinfo grid file is always defined in the cell centres.
         ! Data is assumed to be given at the points x0+i*dx,y0+j*dy
         ! If the x0/y0 line contains the word corner, the corner coordinates
         ! have been specified in the file. Therefore, shift x0 and y0 by half
         ! a grid cell to the cell centres.
         !
      end if

      if (num_columns < 0) then
         call setECMessage('ERROR: Could not find n_cols/num_columns value in header of arcinfo file')
         goto 999
      end if
      if (num_rows < 0) then
         call setECMessage('ERROR: Could not find n_rows/num_rows value in header of arcinfo file')
         goto 999
      end if

      !
      if (jacornerx == 1) x0 = x0 + dxa / 2
      if (jacornery == 1) y0 = y0 + dya / 2
      !
      success = .true.
      return
      !
      ! error handling
      !
100   continue
      call setECMessage('ERROR: Unexpected end of file while reading header of arcinfo file')
      goto 999
101   continue
      call setECMessage('ERROR: Looking for ncols (arc-info), but getting: ', trim(rec))
      goto 999
102   continue
      call setECMessage('ERROR: Looking for nrows (arc-info), but getting: ', trim(rec))
      goto 999
103   continue
      call setECMessage('ERROR: Looking for xll (arc-info), but getting: ', trim(rec))
      goto 999
104   continue
      call setECMessage('ERROR: Looking for yll (arc-info), but getting: ', trim(rec))
      goto 999
105   continue
      call setECMessage('ERROR: Looking for cellsize (dx, dy) (arc-info), but getting: ', trim(rec))
      goto 999
106   continue
      call setECMessage('ERROR: Looking for missing value (arc-info), but getting: ', trim(rec))
      goto 999
999   continue
      success = .false.
   end function readarcinfoheader

   ! =======================================================================

   function numbersonline(rec)
      integer :: numbersonline
      character(*) :: rec
      !
      integer :: i
      integer :: istarti
      integer :: leeg
      integer :: lend
      !
      numbersonline = 0
      leeg = 1
      lend = len_trim(rec)
      do i = 1, lend
         if (index(rec(i:i), ' ') == 0) then
            ! there is nothing here
            if (leeg == 1) then
               leeg = 0
               istarti = i
               numbersonline = numbersonline + 1
            end if
         else
            leeg = 1
         end if
      end do
   end function numbersonline

!============================================================================================================================
! Items_from_bc_quantities: Produce a separate (source) item from every quantity in a bcblock and connect it to the filereader.
   function items_from_bc_quantities(instancePtr, fileReaderPtr) result(success)
!  use m_ec_provider
      implicit none

      type(tEcBCBlock), pointer :: bcPtr !< Pointer to a BCBlock instance
      type(tEcFileReader), pointer :: fileReaderPtr !< Pointer to a filereader instance
      type(tEcInstance), pointer :: instancePtr

      integer :: ic, nc
      logical :: success

      success = .false.
      bcPtr => fileReaderPtr%bc
      nc = bcPtr%numcols
      allocate (bcPtr%columns(bcPtr%numcols))
      select case (bcPtr%func)
      case (BC_FUNC_TSERIES, BC_FUNC_CONSTANT)
         do ic = 1, nc
            if (ic /= bcPtr%timecolumn) then
               bcPtr%quantity => bcPtr%quantities(ic)
               if (.not. (ecProviderCreateUniformItems(instancePtr, fileReaderPtr))) then
                  return
               end if
            end if
         end do
      case (BC_FUNC_QHTABLE)
         if (.not. (ecProviderCreateQhtableItems(instancePtr, fileReaderPtr, use_std_names=.true.))) return
         ! This step produces four items for this filereader with quantities named 'waterlevel', 'discharge', 'slope' and 'crossing',
         ! each having a fieldT0%arr1D holding the table column values
      end select
      success = .true.
   end function items_from_bc_quantities

   function ecProviderNetcdfReadvars(fileReaderPtr) result(success)
      use m_alloc
      implicit none
      type(tECFileReader), pointer :: fileReaderPtr
      logical :: success
      integer :: ivar, idim, ierror
      integer :: nvar, ndim, name_len
      integer :: dimid(1), dim_size
      character(len=NF90_MAX_NAME) :: dim_name
      ! Make a list of standard names of variables available in the netcdf file
      nvar = 0
      ierror = nf90_inquire(fileReaderPtr%fileHandle, nvariables=nvar)
      if (ierror /= NF90_NOERR) then
         ! todo: error handling with message
         call setECMessage('ecProviderNetcdfReadvars: '//nf90_strerror(ierror))
         return
      end if

      ierror = nf90_inquire(fileReaderPtr%fileHandle, nDimensions=ndim)
      if (ndim > 0) then
         dim_name = ''
         allocate (fileReaderPtr%dim_length(ndim))
         allocate (fileReaderPtr%dim_varids(ndim))
         fileReaderPtr%dim_varids = -1
         fileReaderPtr%dim_length = -1
         fileReaderPtr%time_id = -1
         fileReaderPtr%lonx_id = -1
         fileReaderPtr%laty_id = -1

         do idim = 1, ndim
            ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, idim, len=fileReaderPtr%dim_length(idim))
            ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, idim, name=dim_name)
            ! Find dimension matching columns and rows
            select case (trim(dim_name))
            case ('x', 'longitude', 'projected_x', 'xc', 'grid_longitude', 'projection_x_coordinate')
               fileReaderPtr%lonx_id = idim
            case ('y', 'latitude', 'projected_y', 'yc', 'grid_latitude', 'projection_y_coordinate')
               fileReaderPtr%laty_id = idim
            end select
         end do ! idim
      else
         ! no dimensions in the file or netcdf inquiry error .... handle exception
         return
      end if

      ! Collects names and standard names of variables in the netcdf as well as the varids associated with dimids
      ! The latter is used later to guess coordinates belonging to a variable
      if (nvar > 0) then
         call realloc(fileReaderPtr%standard_names, nvar) ! Note: one of these may be obsolete (if we only check standard names)
         call realloc(fileReaderPtr%variable_names, nvar)
         fileReaderPtr%standard_names = ''
         fileReaderPtr%variable_names = ''
         do ivar = 1, nvar
            fileReaderPtr%standard_names(ivar) = ''
            ierror = nf90_get_att(fileReaderPtr%fileHandle, ivar, 'standard_name', fileReaderPtr%standard_names(ivar))
            call str_lower(fileReaderPtr%standard_names(ivar))
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, name=fileReaderPtr%variable_names(ivar))
            call str_lower(fileReaderPtr%variable_names(ivar))
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, nDims=ndim)
            dim_name = ''
            name_len = 0
            if (ndim == 1) then ! Is this variable a 1D coordinate variable
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, dimids=dimid)
               ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimid(1), name=dim_name, len=dim_size)
               call str_lower(dim_name)
               if (trim(dim_name) == trim(fileReaderPtr%variable_names(ivar))) then
                  fileReaderPtr%dim_varids(dimid(1)) = ivar ! connects a varid to a dimid
               end if
            end if
         end do
      else
         ! no variables in the file or netcdf inquiry error .... handle exception
         return
      end if
      success = .true.
   end function ecProviderNetcdfReadvars

end module m_ec_provider
