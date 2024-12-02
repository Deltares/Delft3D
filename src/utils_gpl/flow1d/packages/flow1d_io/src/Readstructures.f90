module m_readstructures
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
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
!-------------------------------------------------------------------------------
!  
!  
!-------------------------------------------------------------------------------

   use MessageHandling
   use precision_basics, only: dp
   use string_module
   use m_network
   use m_CrossSections
   use m_1d_structures
   use m_Universal_Weir
   use m_Culvert
   use m_Bridge
   use m_pump
   use m_General_Structure
   use m_Dambreak

   use properties
   use m_hash_search
   use m_tables

   implicit none

   private

   public readStructures
   public readPump
   public readDambreak
   public allowedFlowDirtoInt
   public get_value_or_addto_forcinglist

   !> The file version number of the structure file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Structure file current version: 1.00
   integer, parameter :: StructureFileMajorVersion = 3
   integer, parameter :: StructureFileMinorVersion = 1
   
   ! History structure file versions:

   ! 0.00 (pre-2019  ): Unversioned flow1d/SOBEK3 version of *.ini type structure file.
   ! 1.00 (2019-07-05): Consistent renaming,
   !                    * pumps: nrStages -> numStages, reductionFactorLevels -> numReductionLevels
   !                    * culverts: lossCoeffCount -> numLossCoeff
   !                    * universal weir: levelsCount -> numLevels
   ! 2.00 (2019-07-22): Consistent renaming,
   ! 2.01 (2020-10-20): Added new type=longCulvert.
   ! 3.00 (2021-06-17): Bridge field 'bedLevel' removed and replaced by 'shift' (UNST-5177).
   ! 3.01 (2024-11-25): Added new type=gate, polylineFile replaced by locationFile.
   

   contains

   !> Read structure.ini file(s).
   subroutine readStructures(network, structureFiles)
      use string_module, only: str_token

      type(t_network),  intent(inout)    :: network        !< The network data structure into whose Structure Set the file(s) will be read.
      character(len=*), intent(in   )    :: structurefiles !< File name(s) to be read. Separate multiple files by semicolon: "file with spaces 1.ini;file2.ini;file 3.ini".
   
      character(len=IdLen) :: file
      character(len=IdLen) :: inputFiles
      
      inputFiles = structurefiles
      do while (len_trim(inputfiles) > 0) 
         call str_token(inputfiles, file, DELIMS=';')
         call readStructureFile(network, adjustl(trim(file)))
      enddo

      if (.not. allocated(network%sts%restartData) .and. (network%sts%count > 0)) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif
      ! Fill indirection tables and set indices for compoundss
      if (network%sts%currentFileVersion >= 2) then
         call finishReading(network%sts, network%cmps, network%crs)
      else
         ! fill the hashtable for searching on Id's
         call fill_hashtable(network%sts)
      endif
      
   end subroutine readStructures

   !> read a single ini file and add the structures to the structure sets
   subroutine readStructureFile(network, structureFile)
      use m_GlobalParameters
      use m_1d_Structures
      use m_compound
      
      implicit none
      
      type(t_network), intent(inout) :: network              !< Network pointer
      character(len=*), intent(in)      :: structureFile        !< Name of the structure file

      logical                                                :: success, success1
      type(tree_data), pointer                               :: md_ptr 
      integer                                                :: istat
      integer                                                :: numstr
      integer                                                :: i
      character(len=:), allocatable                          :: str_buf

      character(len=IdLen)                                   :: typestr
      character(len=IdLen)                                   :: st_id
      character(len=IdLen)                                   :: branchID
      
      integer                                                :: iStrucType
      type(t_structure), pointer                             :: pstru
      integer                                                :: major, minor, ierr
      character(len=IdLen)                                   :: filename
      
      success = .true.

      call tree_create(trim(structurefile), md_ptr, maxlenpar)
      call prop_file('ini',trim(structurefile),md_ptr,istat)
      
      msgbuf = 'Reading '//trim(structurefile)//'.'
      call msg_flush()
      
      ! check FileVersion
      ierr = 0
      major = 1
      minor = 0
      call get_version_number(md_ptr, major = major, minor = minor, success = success1)
      if (.not. success1) then
         msgbuf = 'Early return, file '//trim(structurefile)//' is a 2D3D structure file, it will be read by function flow_init_structurecontrol.'
         call msg_flush()
         msgbuf = 'This functionality is deprecated, please convert this file to the current version.'
         call warn_flush()
         return
      endif
      ! For now majorVersion = 2.xx is supported for all structures, except for the bridge. 
      if ((major /= StructureFileMajorVersion .and. major /= 2) .or. (major == StructureFileMajorVersion .and. minor > StructureFileMinorversion)) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of structure file detected in '''//trim(structurefile)//''': v', major, minor, '. Current format: v',StructureFileMajorVersion,StructureFileMinorVersion,'. Ignoring this file.'
         call err_flush()
         ierr = 1
      end if

      if (ierr /= 0) then
         goto 999
      end if

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      if (network%sts%count == 0) then
         network%sts%currentFileVersion = major
      else
         network%sts%currentFileVersion = min(major, network%sts%currentFileVersion)
      endif

      do i = 1, numstr
         success = .true.
         if (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'Structure')) then
            
            if (network%sts%count+1 > network%sts%Size) then
               call realloc(network%sts)
            endif
            pstru => network%sts%struct(network%sts%count+1)
            ! Read Common Structure Data
            
            ! TODO: UNST-2799: temporary check on polylinefile to prevent stopping on old (1.00) structure files. They will be read by dflowfm kernel itself.

            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'id', st_id, success1)
            if (.not. success1) then
               write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ', id is missing.'
               call err_flush()
               success = .false.
            end if

            pstru%id = st_id

            pstru%name = pstru%id
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'name', pstru%name)
            
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'type', typestr, success1)
            success = success .and. check_input_result(success1, st_id, 'type')
      
            iStrucType = GetStrucType_from_string(typestr)
            pstru%type = iStrucType
            if (iStrucType == ST_COMPOUND) then
               call readCompound(network%cmps, md_ptr%child_nodes(i)%node_ptr, success)
               cycle
            endif
            
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchID, success1)
            if (success1) call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', pstru%chainage, success1)

            pstru%numCoordinates = 0
            if (success1) then
               pstru%ibran = hashsearch(network%brs%hashlist, branchID)
               if (pstru%ibran <= 0) then
                  write (msgbuf, '(a)') 'Error Reading Structure '''//trim(st_id)//''' from '''//trim(structureFile)//''', branchId '''//trim(branchID)//''' not found.'
                  call err_flush()
                  success = .false.
               endif
            endif
            if (.not. success1) then
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'numCoordinates', pstru%numCoordinates, success1)
               if (success1) then
                  allocate(pstru%xCoordinates(pstru%numCoordinates), pstru%yCoordinates(pstru%numCoordinates), stat = istat)
                  call aerr( 'readStructureFile:pstru%x/y coordinates', istat, 2*pstru%numCoordinates)

                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'xCoordinates', pstru%xCoordinates, &
                                pstru%numCoordinates, success1)
                  success = success .and. check_input_result(success1, st_id, 'xCoordinates')
                  
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'yCoordinates', pstru%yCoordinates, &
                                pstru%numCoordinates, success1)
                  success = success .and. check_input_result(success1, st_id, 'yCoordinates')
               endif
            endif

            if (.not. success1) then
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'locationFile', filename, success1)
               if (.not. success1) then
                  ! For backward compatibility: if locationfile is not found, try polylinefile.
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'polylineFile', filename, success1)
                  if (success1) then
                     msgbuf = 'Error Reading Structure '''//trim(st_id)//''' from '''//trim(structureFile)//''', polylineFile is found. Please use locationFile instead.'
                     call warn_flush()
                  end if
                  
               end if
               if (success1) then
                  call read_polyfile(filename, pstru%xCoordinates, pstru%yCoordinates, pstru%numCoordinates, success1)
               end if

               success = success .and. check_input_result(success1, st_id, 'branchId, numCoordinates and locationfile')  
            end if         
            pstru%compound = 0

            select case (iStrucType)
            case (ST_WEIR)
               ! support for version >= 2.0 structure files weirs as general structure
               call readWeirAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_UNI_WEIR)
               call readUniversalWeir(pstru%uniweir, md_ptr%child_nodes(i)%node_ptr, st_id, success)
            case (ST_CULVERT)
               call readCulvert(pstru%culvert, network, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_BRIDGE)
               if (major /= StructureFileMajorVersion) then
                  call SetMessage(LEVEL_ERROR, 'The file version in ' // structureFile //' should be at least 3.0 or higher' )
                  call SetMessage(-LEVEL_ERROR, 'when using bridges (parameter bedLevel must be changed to a shift).' )
               else
                  call readBridge(pstru%bridge, network, md_ptr%child_nodes(i)%node_ptr, st_id, success)
               endif 
            case (ST_PUMP)
               call readPump(pstru%pump, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_ORIFICE)
               ! support for version >= 2.0 structure files weirs as general structure
               call readOrificeAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_GATE)
               ! support for version >= 2.0 structure files weirs as general structure
               call readGateAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_GENERAL_ST)
               call readGeneralStructure(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_DAMBREAK)
               call readDambreak(pstru%dambreak, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_COMPOUND)
               ! Compound structures have been cycled above already.
               continue
            case default
               if (strcmpi(typestr, 'longCulvert')) then
                  cycle ! NOTE: UNST-4328: reading of culverts done in kernel.
               else 
                  call setmessage(LEVEL_ERROR,  'Structure type: '//trim(typestr)//' not supported, see '//trim(pstru%id))
                  success = .false.
               end if
            end select
            
            if (success) then
               network%sts%Count = network%sts%Count + 1
               call incStructureCount(network%sts, iStrucType)
            endif
         
         endif

         
      end do

999   continue
      call tree_destroy(md_ptr)

   end subroutine readStructureFile
   
   !> Read the data for a compound structure and store the ids of the structure  elements
   subroutine readCompound(cmps, md_ptr, success)
   
      use messageHandling
      
      type(t_compoundSet),                intent(inout) :: cmps         !< compound data set
      type(tree_data), pointer,           intent(in   ) :: md_ptr       !< ini tree pointer with user input.
      logical,                            intent(  out) :: success 

      type(t_compound), pointer           :: pcompound
      integer                             :: i
      character(len=IdLen)                :: st_id
      logical                             :: success1
      
      if (cmps%count+1 > cmps%Size) then
         call realloc(cmps)
      endif

      success = .true.
      pcompound => cmps%compound(cmps%count+1)
      call prop_get(md_ptr, '', 'id', st_id, success1)
      if (.not. success1) then
         write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ', id is missing.'
         call err_flush()
         success = .false.
      end if

      pcompound%id = st_id
      pcompound%name = pcompound%id
      call prop_get(md_ptr, '', 'name', pcompound%name)
            
      call prop_get(md_ptr, '', 'numStructures', pcompound%numstructs, success1)
      success = success .and. check_input_result(success1, st_id, 'numStructures')
      
      if (.not. success) then
         return
      endif
      
      allocate(pcompound%structureIds(pcompound%numstructs))

      call prop_get(md_ptr, '', 'structureIds', pcompound%numstructs, pcompound%structureIds, success1)
      success = success .and. check_input_result(success1, st_id, 'structureIds')
      if (.not. success) then
         ! Stop processing this structure
         return
      endif
            
      allocate(pcompound%structure_indices(pcompound%numstructs))
      if (success) then
         cmps%Count = cmps%Count + 1
      endif
      
   end subroutine readCompound
   
   !> At the end of the reading of all structure files, fill the indices arrays for the different
   !! structure types. And find the integer indices for all compound structures
   subroutine finishReading(sts, cmps, crs)
      type(t_structureSet),               intent(inout) :: sts    !< structure data set
      type(t_compoundSet),                intent(inout) :: cmps   !< compound data set
      type(t_CrossSectionSet),            intent(inout) :: crs    !< cross section set
   
      integer :: i
      integer :: icross
      integer :: istru
      integer :: nweir
      integer :: nculvert
      integer :: norifice
      integer :: nbridge
      integer :: ngate
      integer :: ngenstru
      integer :: nuniweir
      integer :: ndambreaklinks
      integer :: npump
      integer,          dimension(:), pointer :: indices
      character(len=IdLen), dimension(:), pointer :: ids
      
      ! Set counters for number of weirs, culverts, etc
      sts%numweirs    = 0
      sts%numculverts = 0
      sts%numPumps    = 0
      sts%numOrifices = 0
      sts%numBridges  = 0
      sts%numGates    = 0
      sts%numGeneralStructures = 0
      sts%numUniWeirs = 0
      sts%numweirs    = sts%countByType(ST_WEIR)
      sts%numculverts = sts%countByType(ST_CULVERT)
      sts%numPumps    = sts%countByType(ST_PUMP)
      sts%numOrifices = sts%countByType(ST_ORIFICE)
      sts%numBridges  = sts%countByType(ST_BRIDGE)
      sts%numGates    = sts%countByType(ST_GATE)
      sts%numGeneralStructures = sts%countByType(ST_GENERAL_ST)
      sts%numUniWeirs = sts%countByType(ST_UNI_WEIR)
      sts%numDambreaks = sts%countByType(ST_DAMBREAK)
      allocate(sts%weirIndices(sts%numweirs))
      allocate(sts%culvertIndices(sts%numCulverts))
      allocate(sts%pumpIndices(sts%numPumps))
      allocate(sts%orificeIndices(sts%numOrifices))
      allocate(sts%gateIndices(sts%numGates))
      allocate(sts%bridgeIndices(sts%numBridges))
      allocate(sts%generalStructureIndices(sts%numGeneralStructures))
      allocate(sts%uniWeirIndices(sts%numUniWeirs))
      allocate(sts%dambreakIndices(sts%numDambreaks))

      !set structure indices for different structure types
      nweir = 0
      nculvert = 0
      norifice = 0
      ngenstru = 0
      nbridge = 0
      ngate = 0
      nuniweir = 0
      ndambreaklinks = 0
      npump = 0
      do istru = 1, sts%Count
         select case (sts%struct(istru)%type)
         case (ST_WEIR)
            nweir = nweir+1
            sts%weirIndices(nweir) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_CULVERT)
            nculvert = nculvert + 1
            sts%culvertIndices(nculvert) = istru

            ! Extra step for culvert: re-pointer to the cross section (as the network%crs may have been reallocted during reading).
            icross = sts%struct(istru)%culvert%crosssectionnr
            if (icross > 0) then
               sts%struct(istru)%culvert%pcross => crs%cross(icross)
            end if
         case (ST_ORIFICE)
            norifice = norifice + 1
            sts%orificeIndices(norifice) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_GATE)
            ngate = ngate + 1
            sts%gateIndices(ngate) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_BRIDGE)
            nbridge = nbridge + 1
            sts%bridgeIndices(nbridge) = istru

            ! Extra step for bridge: re-pointer to the cross section (as the network%crs may have been reallocted during reading).
            icross = sts%struct(istru)%bridge%crosssectionnr
            if (icross > 0) then
               sts%struct(istru)%bridge%pcross => crs%cross(icross)
            end if
         case (ST_GENERAL_ST)
            ngenstru = ngenstru + 1
            sts%generalStructureIndices(ngenstru) = istru
         case (ST_UNI_WEIR)
            nuniweir = nuniweir + 1
            sts%uniWeirIndices(nuniweir) = istru
         case (ST_DAMBREAK)
            ndambreaklinks = ndambreaklinks + 1
            sts%dambreakIndices(ndambreaklinks) = istru
         case (ST_PUMP)
            npump = npump+1
            sts%pumpIndices(npump) = istru
            sts%struct(istru)%type = ST_PUMP
         end select
      enddo
      
      call fill_hashtable(sts)
            
      ! Initialise compounds and searc for all structure elements
      do istru = 1, cmps%Count
         indices => cmps%compound(istru)%structure_indices
         ids =>  cmps%compound(istru)%structureIds
         do i = 1,  cmps%compound(istru)%numstructs
            indices(i) = hashsearch(sts%hashlist_structure, ids(i))
            if (indices(i) <=0) then
               msgbuf = 'Error in compound '''//trim(cmps%compound(istru)%id)//''' structure element with id '''//trim(ids(i))//&
                        ''' was not found in the structure list'
               call err_flush()
            else
               sts%struct(indices(i))%compound = 1 ! mark the structure that belongs to a compound structure
            endif
         enddo
         
      enddo
   end subroutine finishreading
 

   !> Read specific data for the universal weir structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readUniversalWeir(uniweir, md_ptr, st_id, success)
   
      type(t_uni_weir), pointer, intent(inout) :: uniweir    !< Universal weir structure to be read into.
      type(tree_data),  pointer, intent(in   ) :: md_ptr     !< ini tree pointer with user input.
      character(IdLen),          intent(in   ) :: st_id      !< Structure character Id.
      logical,                   intent(  out) :: success    !< Result status, whether reading of the structure was successful.

      integer                                      :: istat
      integer                                      :: i, ilowest
      real(kind = dp)                             :: lowestz
      character(len=Idlen)                         :: txt
      logical                                      :: success1

      success = .true.
      allocate(uniweir)
      
      call prop_get(md_ptr, '', 'crestLevel', uniweir%crestlevel, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')
      
      call prop_get(md_ptr, '', 'dischargeCoeff', uniweir%dischargecoeff, success1)
      success = success .and. check_input_result(success1, st_id, 'dischargeCoeff')
      
      call prop_get(md_ptr, '', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      
      uniweir%allowedflowdir = allowedFlowDirToInt(txt)
      
      call prop_get(md_ptr, '', 'numLevels', uniweir%yzcount, success1) 
      success = success .and. check_input_result(success1, st_id, 'numLevels')

      if (success) then
         call realloc(uniweir%y, uniweir%yzcount, stat=istat)
         if (istat == 0) call realloc(uniweir%z, uniweir%yzcount, stat=istat)
         if (istat .ne. 0) then
            call SetMessage(LEVEL_ERROR, 'Reading Universal Weir: '''//trim(st_id)//'''. Error Allocating Y/Z Arrays')
            success = .false.
            return
         endif

         call prop_get(md_ptr, '', 'yValues', uniweir%y, uniweir%yzcount, success1)
         success = success .and. check_input_result(success1, st_id, 'yValues')
      
         ! Y values must be in non-descending order.
         do i = 1, uniweir%yzcount-1
            if (uniweir%y(i+1) < uniweir%y(i)) then
               call SetMessage(LEVEL_ERROR, 'Reading Universal Weir: '''//trim(st_id)//'''. yValues are not in ascending order.')
               success = .false. 
               return
            endif
         enddo
         call prop_get(md_ptr, '', 'zValues', uniweir%z, uniweir%yzcount, success1)
         success = success .and. check_input_result(success1, st_id, 'zValues')
      endif
      
      ! The z-values contains a relative height with respect to the crest level.
      ! As a result the minimal value for Z must be 0. Shift-user-specified values to 0.
      lowestz = huge(1.0_dp)
      do i = 1, uniweir%yzcount
         if (lowestz >  uniweir%z(i)) then
            lowestz = uniweir%z(i)
            ilowest = i
         endif
      enddo
      do i = 1, uniweir%yzcount
         uniweir%z(i) = uniweir%z(i) - lowestz
      enddo
      
      ! Check whether a possible zero flow area can occurr
      if ((uniweir%y(max(1,ilowest-1)) == uniweir%y(ilowest)) .and. (uniweir%y(ilowest) == uniweir%y(min(ilowest+1, uniweir%yzcount)))) then
         call SetMessage(LEVEL_ERROR, 'Reading Universal Weir: '''//trim(st_id)//'''.  The flow area at the lowest point to the next point is 0.')
         success = .false. 
         return
      endif
   end subroutine readUniversalWeir


   !> Read the culvert specific data.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readCulvert(culvert, network, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_culvert), pointer,           intent(inout) :: culvert         !< Culvert structure to be read into.
      type(t_network),                    intent(inout) :: network         !< Network data structure, to which a crosssection may be added for the culvert.
      type(tree_data), pointer,           intent(in   ) :: md_ptr          !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id           !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist     !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                            intent(  out) :: success         !< Result status, whether reading of the structure was successful.
 
      character(len=IdLen)                        :: CrsDefID, txt
      integer                                     :: CrsDefIndx
      integer                                     :: icross
      
      integer                                     :: bedFrictionType
      real(kind = dp)                            :: bedFriction
      integer                                     :: groundFrictionType
      real(kind = dp)                            :: groundFriction
      integer                                     :: valveonoff
      
      integer                                     :: lossCoeffCount
      integer                                     :: istat
      real(kind = dp), allocatable, dimension(:) :: relOpen
      real(kind = dp), allocatable, dimension(:) :: lossCoeff
      logical                                     :: success1 
      character(len=IdLen)                        :: subtype 

      success = .true.
      allocate(culvert)

      call prop_get(md_ptr, '', 'csDefId', CrsDefID, success1)
      success = success .and. check_input_result(success1, st_id, 'csDefId')
      
      if (success) then
         CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
         if (CrsDefIndx <= 0) then
            call setMessage(LEVEL_ERROR, 'Error Reading Culvert '''//trim(st_id)//''': Cross-Section Definition '''//trim(CrsDefID)//''' not found.')
            success = .false.
         endif
      endif
   
      call prop_get(md_ptr, '', 'bedFrictionType', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'bedFrictionType')
      if (success) then
         call frictionTypeStringToInteger(txt, bedFrictionType)
         if (bedFrictionType < 0) then
            call setMessage(LEVEL_ERROR, 'Error Reading Culvert '''//trim(st_id)//''': invalid bedFrictionType '''//trim(txt)//'''.')
            success = .false.
         end if
      end if

      call prop_get(md_ptr, '', 'bedFriction', bedFriction, success1)
      success = success .and. check_input_result(success1, st_id, 'bedFriction')
      
      groundFrictionType = 0
      groundFriction = 45.0_dp
      call prop_get(md_ptr, '', 'groundFrictionType', groundFrictionType)
      call prop_get(md_ptr, '', 'groundFriction', groundFriction)
         
      icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0_dp, CrsDefIndx, 0.0_dp, &
                               bedFrictionType, bedFriction, groundFrictionType, groundFriction)
      network%crs%cross(icross)%csid = 'CS_Culvert_'//trim(st_id)
      network%crs%cross(icross)%branchid = -1
         
      culvert%pcross         => network%crs%cross(icross)
      culvert%crosssectionnr = icross
      
      call prop_get(md_ptr, '', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      if (success) culvert%allowedflowdir = allowedFlowDirToInt(txt)
      
      call prop_get(md_ptr, '', 'length', culvert%length, success1) 
      success = success .and. check_input_result(success1, st_id, 'length')
      
      call prop_get(md_ptr, '', 'leftLevel', culvert%leftlevel, success1) 
      success = success .and. check_input_result(success1, st_id, 'leftLevel')
      
      call prop_get(md_ptr, '', 'rightLevel', culvert%rightlevel, success1) 
      success = success .and. check_input_result(success1, st_id, 'rightLevel')
      
      call prop_get(md_ptr, '', 'inletLossCoeff', culvert%inletlosscoeff, success1) 
      success = success .and. check_input_result(success1, st_id, 'inletLossCoeff')
      
      call prop_get(md_ptr, '', 'outletLossCoeff', culvert%outletlosscoeff, success1) 
      success = success .and. check_input_result(success1, st_id, 'outletLossCoeff')

      subtype = 'culvert'
      call prop_get(md_ptr, '', 'subType', subtype)
      call prop_get(md_ptr, '', 'bendLossCoeff', culvert%bendLossCoeff, success1)
      call str_lower(subtype)
      select case(str_tolower(trim(subtype)))
      case ('invertedsiphon')
         if (.not. success1) then
            call SetMessage(LEVEL_ERROR, 'Parameter bendLossCoeff is missing for culvert ''' // trim(st_id) // '''.')
         endif
         if (culvert%bendLossCoeff < 0.0_dp) then
            call SetMessage(LEVEL_ERROR, 'Parameter bendLossCoeff is less than 0 for culvert '''  // trim(st_id) // '''.')
         endif
         culvert%isInvertedSiphon = .true.
      case ('culvert')   
         if (success1) then
            call SetMessage(LEVEL_ERROR, 'The use of bendLossCoeff is only allowed for subtype invertedSiphon, please check ''' // trim(st_id) // '''.')
         endif
         culvert%isInvertedSiphon = .false.
      case default
         call SetMessage(LEVEL_ERROR, 'Incorrect subType (= '''//trim(subtype) // ''') found for culvert ''' // trim(st_id) // '''.')
      end select

      call prop_get(md_ptr, '', 'valveOnOff', valveonoff, success1)
      success = success .and. check_input_result(success1, st_id, 'valveOnOff')
      
      if (valveonoff == 1) then
         
         culvert%has_valve = .true.
         
         call get_value_or_addto_forcinglist(md_ptr, 'valveOpeningHeight', culvert%valveOpening, st_id, ST_CULVERT, forcinglist, success1)
         success = success .and. check_input_result(success1, st_id, 'valveOpeningHeight')
         
         call prop_get(md_ptr, '', 'numLossCoeff', lossCoeffCount, success1) ! UNST-2710: new consistent keyword
         success = success .and. check_input_result(success1, st_id, 'numLossCoeff')
         if (success1) then   
            call realloc(relOpen, lossCoeffCount, stat=istat)
            if (istat == 0) call realloc(lossCoeff, lossCoeffCount, stat=istat)
            if (istat /= 0) then
               call SetMessage(LEVEL_ERROR, 'Reading Culvert: Error Allocating Valve Loss Arrays')
               success = .false.
            endif

            call prop_get(md_ptr, '', 'relOpening', relOpen, lossCoeffCount, success1)
            success = success .and. check_input_result(success1, st_id, 'relOpening')
            
            call prop_get(md_ptr, '', 'lossCoeff', lossCoeff, lossCoeffCount, success1)
            success = success .and. check_input_result(success1, st_id, 'lossCoeff')
         
            call setTable(culvert%lossCoeff, 0, relOpen, lossCoeff, lossCoeffCount)
            ! Clear Valve Loss Arrays
            istat = 0
            if (allocated(relOpen)) deallocate(relOpen, stat=istat)
            if (istat == 0 .and. allocated(lossCoeff)) deallocate(lossCoeff, stat=istat)
            if (istat .ne. 0) then
               call SetMessage(LEVEL_ERROR, 'Reading Culvert: Error Deallocating Valve Loss Arrays')
               success = .false.
            endif
         endif
         
      else
         culvert%has_valve = .false.
         culvert%valveOpening = 0.0_dp
      endif
   
   end subroutine readCulvert
   
   !> Read the bridge specific data for a bridge structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readBridge(bridge,network, md_ptr, st_id, success)
      
      type(t_bridge),  pointer,  intent(inout) :: bridge          !< Bridge structure to be read into.
      type(t_network),           intent(inout) :: network         !< Network data structure, to which a crosssection may be added for the bridge.
      type(tree_data), pointer,  intent(in   ) :: md_ptr          !< ini tree pointer with user input.
      character(IdLen),          intent(in   ) :: st_id           !< Structure character Id.
      logical,                   intent(  out) :: success         !< Result status, whether reading of the structure was successful.

      character(len=IdLen)                       :: txt
      character(len=IdLen)                       :: CrsDefID
      integer                                    :: CrsDefIndx
      integer                                    :: icross
      logical                                    :: success1
      real(kind = dp)                           :: shift  
      
      
      success = .true.
      allocate(bridge)
      
      bridge%bedLevel           = 0.0_dp
      bridge%pillarwidth        = 0.0_dp
      bridge%formfactor         = 0.0_dp
      bridge%allowedflowdir     = 0
      bridge%useOwnCrossSection = .false.
      bridge%pcross             => null()
      bridge%crosssectionnr     = 0
      bridge%bedFrictionType    = 0
      bridge%bedFriction        = 0.0_dp
      bridge%length             = 0.0_dp
      bridge%inletlosscoeff     = 0.0_dp
      bridge%outletlosscoeff    = 0.0_dp

      call prop_get(md_ptr, 'structure', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      if (success) bridge%allowedflowdir = allowedFlowDirToInt(txt)
      
      ! Make distinction between a pillar bridge and a standard bridge
      
      call prop_get(md_ptr, '', 'pillarWidth', bridge%pillarwidth, success1)
      if (success1) then
         ! pillar bridge
         call prop_get(md_ptr, '', 'formFactor', bridge%formfactor, success1)
         success = success .and. check_input_result(success1, st_id, 'formFactor')
      endif
      
      ! Standard bridge
      call prop_get(md_ptr, '', 'csDefId', CrsDefID, success1)
      if (success1) then
         CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
         if (CrsDefIndx <= 0) then
            call setMessage(LEVEL_ERROR, 'Error Reading Bridge '''//trim(st_id)//''': Cross-Section Definition '''//trim(CrsDefID)// ''' not found.')
            success = .false.
         endif
         
         call prop_get(md_ptr, '', 'frictionType', txt, success1)
         success = success .and. check_input_result(success1, st_id, 'frictionType')
         if (success) then
            call frictionTypeStringToInteger(txt, bridge%bedFrictionType)
            if (bridge%bedFrictionType < 0) then
               call setMessage(LEVEL_ERROR, 'Error Reading Bridge '''//trim(st_id)//''': invalid frictionType '''//trim(txt)//'''.')
               success = .false.
            end if
         end if
         
         call prop_get(md_ptr, '', 'friction', bridge%bedFriction, success1)
         success = success .and. check_input_result(success1, st_id, 'friction')
         
         if (success) then
            icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0_dp, CrsDefIndx, 0.0_dp, &
                                     bridge%bedFrictionType, bridge%bedFriction, groundFrictionType = -1, groundFriction = -1.0_dp)
            network%crs%cross(icross)%branchid = -1
            network%crs%cross(icross)%csid = 'CS_Bridge_'//trim(st_id)

            bridge%useOwnCrossSection = .true.
            bridge%pcross             => network%crs%cross(icross)
            bridge%crosssectionnr     = icross
            if (network%crs%cross(icross)%crossType == cs_YZ_Prof) then
               bridge%pcross%convtab1 => null()
               call CalcConveyance(network%crs%cross(icross))
            endif

         endif
         
         call prop_get(md_ptr, '', 'shift', shift, success1)
         success = success .and. check_input_result(success1, st_id, 'shift')
         bridge%bedLevel = bridge%pcross%bedlevel + shift
         
         call prop_get(md_ptr, '', 'length', bridge%length, success1)
         success = success .and. check_input_result(success1, st_id, 'length')
         
         call prop_get(md_ptr, '', 'inletLossCoeff', bridge%inletlosscoeff, success1)
         success = success .and. check_input_result(success1, st_id, 'inletLossCoeff')
         
         call prop_get(md_ptr, '', 'outletLossCoeff', bridge%outletlosscoeff, success1)
         success = success .and. check_input_result(success1, st_id, 'outletLossCoeff')

      endif
      
   end subroutine readBridge


   !> Read the dambreak specific data for a dambreak structure.
   !! The common fields for the structure (e.g. x/yCoordinates) must have been read elsewhere.
   subroutine readDambreak(dambr, md_ptr, st_id, forcinglist, success)
   
      type(t_dambreak), pointer,    intent(inout) :: dambr       !< Dambreak structure to be read into.
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters. (only for uniform interface now, later: to which dambreak forcing will be added if needed.)
      logical,                      intent(  out) :: success     !< Result status, whether reading of the structure was successful.

      
      logical :: localsuccess

      allocate(dambr)

      call prop_get(md_ptr, 'Structure', 'StartLocationX',  dambr%startLocationX, localsuccess)
      success = success .and. check_input_result(localsuccess, st_id, 'StartLocationX')
      if (.not. success) return

      call prop_get(md_ptr, 'Structure', 'StartLocationY',  dambr%startLocationY, localsuccess)
      success = success .and. check_input_result(localsuccess, st_id, 'StartLocationY')
      if (.not. success) return

      call prop_get(md_ptr, 'Structure', 'Algorithm', dambr%algorithm, localsuccess)
      success = success .and. check_input_result(localsuccess, st_id, 'Algorithm')
      if (.not. success) return

      call prop_get(md_ptr, 'Structure', 'CrestLevelIni', dambr%crestLevelIni, localsuccess)
      success = success .and. check_input_result(localsuccess, st_id, 'CrestLevelIni')
      if (.not. success) return
         
      if (dambr%algorithm == 2) then
         
         call prop_get(md_ptr, 'Structure', 'BreachWidthIni', dambr%breachWidthIni, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'BreachWidthIni')
         if (.not. success) return

         call prop_get(md_ptr, 'Structure', 'CrestLevelMin', dambr%crestLevelMin, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'CrestLevelMin')
         if (.not. success) return

         call prop_get(md_ptr, 'Structure', 'TimeToBreachToMaximumDepth', dambr%timeToBreachToMaximumDepth, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'TimeToBreachToMaximumDepth')
         if (.not. success) return

         call prop_get(md_ptr, 'Structure', 'F1', dambr%f1, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'F1')
         if (.not. success) return

         call prop_get(md_ptr, 'Structure', 'F2', dambr%f2, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'F2')
         if (.not. success) return

         call prop_get(md_ptr, 'Structure', 'Ucrit', dambr%ucrit, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'Ucrit')
         if (.not. success) return
         
         ! optional extra fields
         call prop_get(md_ptr, 'Structure', 'waterLevelUpstreamNodeId ', dambr%waterLevelUpstreamNodeId, localsuccess)
         if (.not. localsuccess) then
            call prop_get(md_ptr, 'Structure', 'WaterLevelUpstreamLocationX', dambr%waterLevelUpstreamLocationX, localsuccess)
            call prop_get(md_ptr, 'Structure', 'WaterLevelUpstreamLocationY', dambr%waterLevelUpstreamLocationY, localsuccess)
         end if

         call prop_get(md_ptr, 'Structure', 'waterLevelDownstreamNodeId ', dambr%waterLevelDownstreamNodeId, localsuccess)
         if (.not. localsuccess) then
            call prop_get(md_ptr, 'Structure', 'WaterLevelDownstreamLocationX', dambr%waterLevelDownstreamLocationX, localsuccess)
            call prop_get(md_ptr, 'Structure', 'WaterLevelDownstreamLocationY', dambr%waterLevelDownstreamLocationY, localsuccess)
         end if
      endif
      
      ! get the name of the tim file 
      if (dambr%algorithm == 3) then
         ! UNST-3308: NOTE that only the .tim filename is read below. It is NOT added to the network%forcinglist.
         !            All time-space handling of the dambreak is still done in kernel.
         
         call prop_get(md_ptr, 'Structure', 'DambreakLevelsAndWidths', dambr%levelsAndWidths, localsuccess)
         success = success .and. check_input_result(localsuccess, st_id, 'DambreakLevelsAndWidths')
         if (.not. success) return         
      endif

      call prop_get(md_ptr, 'Structure', 'T0', dambr%t0, localsuccess)
      success = success .and. check_input_result(localsuccess, st_id, 'T0')
      if (.not. success) return
      
      call setCoefficents(dambr)
      
   end subroutine readDambreak

   !> Read the pump specific data for a pump structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readPump(pump, md_ptr, st_id, forcinglist, success)
   
      type(t_pump), pointer,        intent(inout) :: pump        !< Pump structure to be read into.
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                      intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      integer                                      :: tabsize
      integer                                      :: istat
      real(kind = dp), allocatable, dimension(:)  :: head
      real(kind = dp), allocatable, dimension(:)  :: redfac
      integer :: numcap, numred, iside
      logical :: success1
      character(len=IdLen) :: txt

      success = .true.
      allocate(pump)

      ! Compute the pump%direction from two parts: orientation and optionally controlSide.
      txt = 'positive'
      call prop_get(md_ptr, '', 'orientation', txt)
      pump%direction = orientationToInt(txt) ! will become +1 or -1
      if (abs(pump%direction) /= 1) then
         call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': orientation has invalid value '''//trim(txt)// '''.')
         success = .false.
      end if

      pump%nrstages = 0
      call prop_get(md_ptr, '', 'numStages', pump%nrstages) ! UNST-2709: new consistent keyword

      numcap = max(1, pump%nrstages)
      allocate(pump%capacity(numcap), stat=istat)
      ! NOTE: numStages may be 0, but always read at least 1 capacity value.
      if (istat == 0) allocate(pump%ss_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_trigger(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_trigger(pump%nrstages), stat=istat)

      
      if (pump%nrstages == 0) then
         ! In case of 0 stages, capacity is either scalar double, or filename, or 'realtime'.
         call get_value_or_addto_forcinglist(md_ptr, 'capacity', pump%capacity(1), st_id, ST_PUMP, forcinglist, success1)
         ! addtimespace gaat qid='pump' gebruiken, de bc provider moet via FM juiste name doorkrijgen (==structureid)
         success = success .and. check_input_result(success1, st_id, 'capacity')
      else
         ! Stages with control: only support table with real(kind = dp) values.
         pump%capacity = -1.0_dp
         call prop_get(md_ptr, '', 'capacity', pump%capacity, pump%nrstages, success1)
         success = success .and. check_input_result(success1, st_id, 'capacity')
         if (any(pump%capacity < 0)) then
            call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': a staged pump (numStages > 0) must have nonnegative capacity, and cannot be combined with time series or realtime setting.')
            success = .false.
         end if

         txt = '' ! No default controlSide
         call prop_get(md_ptr, '', 'controlSide', txt, success1)
         success = success .and. check_input_result(success1, st_id, 'controlSide')
         iside = controlSideToInt(txt) ! will become +1 or -1
         if (.not. (iside >= 1 .and. iside <= 3)) then
            call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': controlSide has invalid value '''//trim(txt)// '''.')
            success = .false.
         end if

         pump%direction = pump%direction * iside  ! (+/-1 * 1 or 2 or 3)

         if (abs(pump%direction) == 1 .or. abs(pump%direction) == 3) then
            call prop_get(md_ptr, '', 'startLevelSuctionSide', pump%ss_onlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'startLevelSuctionSide')
            call prop_get(md_ptr, '', 'stopLevelSuctionSide', pump%ss_offlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'stopLevelSuctionSide')
         end if
      
         if (abs(pump%direction) == 2 .or. abs(pump%direction) == 3) then
            call prop_get(md_ptr, '', 'startLevelDeliverySide', pump%ds_onlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'startLevelDeliverySide')
            call prop_get(md_ptr, '', 'stopLevelDeliverySide', pump%ds_offlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'stopLevelDeliverySide')
         end if
      end if
      
      if (.not. success) return

      pump%ss_trigger = .true.
      pump%ds_trigger = .true.

      ! Reduction Table
      tabsize = 0
      call prop_get(md_ptr, '', 'numReductionLevels', tabsize) ! UNST-2709: new consistent keyword

      numred = max(1, tabsize)
      call realloc(head, numred, stat=istat)
      if (istat == 0) call realloc(redfac, numred, stat=istat)
      if (istat /= 0) then
         call SetMessage(LEVEL_ERROR, 'Error Reading Pump: Error Allocating Reduction Factor Arrays.')
         success = .false.
         return
      end if

      if (tabsize > 0) then
         call prop_get(md_ptr, '', 'head', head, tabsize, success1)
         success = success .and. check_input_result(success1, st_id, 'head')

         call prop_get(md_ptr, '', 'reductionFactor', redfac, tabsize, success1)
         success = success .and. check_input_result(success1, st_id, 'reductionFactor')
      else
         ! When no reduction table given in input, always create one dummy entry in table with 100% pump capacity.
         tabsize   = 1
         head(1)   = 0.0_dp
         redfac(1) = 1.0_dp
      endif

      call setTable(pump%reducfact, 0, head, redfac, tabsize)

      ! Clear Arrays
      istat = 0
      if (allocated(head)) deallocate(head, stat=istat)
      if (istat == 0 .and. allocated(redfac)) deallocate(redfac, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_ERROR, 'Error Reading Pump: Error Dellocating Reduction Factor Arrays.')
      endif
      
      ! Initialize Parameters for Pump
      pump%actual_stage      = 0
      pump%discharge         = 0.0_dp
      pump%is_active         = .true.
      pump%pump_head         = 0.0_dp
      pump%reduction_factor  = 1.0_dp
      pump%ss_level          = 0.0_dp
      pump%ds_level          = 0.0_dp
      pump%current_capacity  = 0.0_dp
      
   end subroutine readPump


   !> Either retrieve a constant value for parameter KEY, or get the filename for the time series.
   subroutine get_value_or_addto_forcinglist(md_ptr, key, value, st_id, st_type, forcinglist, success)
      use m_forcinglist
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(len=*),             intent(in   ) :: key         !< name of the item in the input file
      real(kind = dp), target,     intent(  out) :: value       !< The variable into which the read value may be stored.
                                                                 !< In case of a time series/realtime forcing, this variable will be pointed to in the forcinglist.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      integer,                      intent(in   ) :: st_type     !< structure type
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which structure forcing will be added if needed.
      logical,          optional,   intent(inout) :: success     
      
      integer           :: istat   
      character(IdLen) :: tmpstr, structuretype
      logical           :: success1
      
      call prop_get(md_ptr, '', key, tmpstr, success1)
      if (success1) then
         read(tmpstr, *, iostat = istat) value
         if (istat /= 0 .or. index(tmpstr,'/') == 1) then ! No number or a string starting with '/': assume it was a filename
            forcinglist%Count = forcinglist%Count+1
            if (forcinglist%Count > forcinglist%Size) then
               call realloc(forcinglist)
            end if
            forcinglist%forcing(forcinglist%Count)%object_id      = st_id
            call GetStrucType_from_int(st_type, structuretype)
            forcinglist%forcing(forcinglist%Count)%object_type = trim(structuretype)
            forcinglist%forcing(forcinglist%Count)%quantity_id = trim(structuretype)//'_'//trim(key)
            forcinglist%forcing(forcinglist%Count)%param_name = key
            forcinglist%forcing(forcinglist%Count)%targetptr  => value
            forcinglist%forcing(forcinglist%Count)%filename   = tmpstr
         end if
      endif
      
      if (present(success)) then
         success = success1
      endif
      
   end subroutine get_value_or_addto_forcinglist
   
   !> Read the weir parameters and define a general structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readWeirAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into.
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which weir forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      logical           :: success1
      character(len=Idlen) :: dirString
      
      success = .true.
      allocate(generalst)

      generalst%ws = 1.0e10_dp
      call prop_get(md_ptr, '', 'crestWidth', generalst%ws)

      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_WEIR, forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')

      generalst%mugf_pos = 1.0_dp
      if (success) call prop_get(md_ptr, '', 'corrCoeff',  generalst%cgf_pos)

      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)
      
      dirString = 'both'
      call prop_get(md_ptr, '', 'allowedFlowDir', dirString, success1)
      generalst%allowedflowdir = allowedFlowDirToInt(dirString)
   
      ! all levels are set to -1e-10. In the time loop these parameters will be set to the bed level.
      generalst%zu1                = -1.0e10_dp
      generalst%zu2                = -1.0e10_dp
      generalst%zd1                = -1.0e10_dp
      generalst%zd2                = -1.0e10_dp
      generalst%wu1                = generalst%ws
      generalst%wu2                = generalst%ws
      generalst%wd1                = generalst%ws
      generalst%wd2                = generalst%ws
      generalst%gateLowerEdgeLevel = 1.0e10_dp
      generalst%cgf_pos            = generalst%cgf_pos
      generalst%cgd_pos            = generalst%cgf_pos
      generalst%cwf_pos            = generalst%cgf_pos
      generalst%cwd_pos            = generalst%cgf_pos
      generalst%cgf_neg            = generalst%cgf_pos
      generalst%cgd_neg            = generalst%cgf_pos
      generalst%cwf_neg            = generalst%cgf_pos
      generalst%cwd_neg            = generalst%cgf_pos
      generalst%mugf_neg           = 1.0_dp
      generalst%mugf_pos           = 1.0_dp
      generalst%extraresistance    = 0.0_dp
      generalst%gatedoorheight     = 1.0e10_dp
      generalst%gateopeningwidth   = generalst%ws
      generalst%crestlength        = 0.0_dp
      generalst%openingDirection   = GEN_SYMMETRIC

   end subroutine readWeirAsGenStru
 
   !> Read the orifice or gate parameters and define a general structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readOrificeAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into. 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which orifice forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      character(len=Idlen) :: dirString
      logical              :: success1
      
      success = .true.
      allocate(generalst)

      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_ORIFICE, forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')

      generalst%mugf_pos = 1.0_dp
      call prop_get(md_ptr, '', 'corrCoeff',  generalst%cgf_pos)

      generalst%ws = 1.0e10_dp
      call prop_get(md_ptr, '', 'crestWidth',  generalst%ws)
      
      call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_ORIFICE, &
                                                       forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'gateLowerEdgeLevel')

      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)
      
      dirString = 'both'
      call prop_get(md_ptr, '', 'allowedFlowDir', dirString, success1)
      generalst%allowedflowdir = allowedFlowDirToInt(dirString)

      generalst%uselimitFlowPos = .false.
      call prop_get(md_ptr, ' ', 'useLimitFlowPos', generalst%uselimitFlowPos)
      if (generalst%uselimitFlowPos) then
         if (generalst%allowedflowdir /= 0 .and. generalst%allowedflowdir /= 1) then
            write (msgbuf, '(a,a,a,a,a)') 'Structure ''', trim(st_id), ''': useLimitFlowPos can not be combined with allowedFlowDir=', &
               allowedFlowDirToString(generalst%allowedflowdir), '. Ignoring limitFlowPos.'
            call warn_flush()
         else
            call prop_get(md_ptr, ' ', 'limitFlowPos', generalst%limitFlowPos, success1)
            success = success .and. check_input_result(success1, st_id, 'limitFlowPos')
         end if
      end if

      generalst%uselimitFlowNeg = .false.
      call prop_get(md_ptr, ' ', 'useLimitFlowNeg', generalst%uselimitFlowNeg)
      if (generalst%uselimitFlowNeg) then
         if (generalst%allowedflowdir /= 0 .and. generalst%allowedflowdir /= 2) then
            write (msgbuf, '(a,a,a,a,a)') 'Structure ''', trim(st_id), ''': useLimitFlowNeg can not be combined with allowedFlowDir=', &
               allowedFlowDirToString(generalst%allowedflowdir), '. Ignoring limitFlowNeg.'
            call warn_flush()
         else
            call prop_get(md_ptr, ' ', 'limitFlowNeg', generalst%limitFlowNeg, success1)
            success = success .and. check_input_result(success1, st_id, 'limitFlowNeg')
         end if
      end if

      ! Set default/standard values for orifice
      ! all levels are set to -1e-10. In the time loop these parameters will be set to the bed level.e
      generalst%zu1                = -1.0e10_dp
      generalst%zu2                = -1.0e10_dp
      generalst%zd1                = -1.0e10_dp
      generalst%zd2                = -1.0e10_dp
      generalst%wu1                = generalst%ws
      generalst%wu2                = generalst%ws
      generalst%wd1                = generalst%ws
      generalst%wd2                = generalst%ws
      generalst%cgf_pos            = generalst%cgf_pos
      generalst%cgd_pos            = generalst%cgf_pos
      generalst%cwf_pos            = generalst%cgf_pos
      generalst%cwd_pos            = generalst%cgf_pos
      generalst%cgf_neg            = generalst%cgf_pos
      generalst%cgd_neg            = generalst%cgf_pos
      generalst%cwf_neg            = generalst%cgf_pos
      generalst%cwd_neg            = generalst%cgf_pos
      generalst%mugf_neg           = 1.0_dp
      generalst%mugf_pos           = 1.0_dp
      generalst%extraresistance    = 0.0_dp
      generalst%gatedoorheight     = 1.0e10_dp
      generalst%gateopeningwidth   = 0.0_dp
      generalst%crestlength        = 0.0_dp
      generalst%openingDirection   = GEN_SYMMETRIC ! TODO: once 2D structures are being read by this reader, also support fromleft and fromright

   end subroutine readOrificeAsGenStru

   !> Read the gate parameters and define a general structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readGateAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into. 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which orifice forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      character(len=Idlen) :: dirString
      logical              :: success1
      
      success = .true.
      allocate(generalst)

      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_GATE, forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')

      generalst%ws = 1e10_dp
      call get_value_or_addto_forcinglist(md_ptr, 'crestWidth',  generalst%ws, st_id, ST_GATE, forcinglist, success1)
      
      call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GATE, &
      forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'gateLowerEdgeLevel')
      
      call prop_get(md_ptr, '', 'gateHeight',  generalst%gatedoorheight, success1)
      success = success .and. check_input_result(success1, st_id, 'gateHeight')
      
      generalst%gateOpeningWidth = 0.0_dp
      call get_value_or_addto_forcinglist(md_ptr, 'gateOpeningWidth',  generalst%gateOpeningWidth, st_id, ST_GATE, &
      forcinglist, success1)
      
      generalst%openingDirection   = GEN_SYMMETRIC
      call prop_get(md_ptr, '', 'gateOpeningHorizontalDirection',  generalst%openingDirection)
      
      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)
      
      ! Set default/standard values for orifice
      ! all levels are set to -1e-10. In the time loop these parameters will be set to the bed level.
      generalst%zu1                = -1.0e10_dp
      generalst%zu2                = -1.0e10_dp
      generalst%zd1                = -1.0e10_dp
      generalst%zd2                = -1.0e10_dp
      generalst%wu1                = generalst%ws
      generalst%wu2                = generalst%ws
      generalst%wd1                = generalst%ws
      generalst%wd2                = generalst%ws
      generalst%cgf_pos            = 1.0_dp
      generalst%cgd_pos            = 1.0_dp
      generalst%cwf_pos            = 1.0_dp
      generalst%cwd_pos            = 1.0_dp
      generalst%cgf_neg            = 1.0_dp
      generalst%cgd_neg            = 1.0_dp
      generalst%cwf_neg            = 1.0_dp
      generalst%cwd_neg            = 1.0_dp
      generalst%mugf_neg           = 1.0_dp
      generalst%mugf_pos           = 1.0_dp
      generalst%extraresistance    = 0.0_dp
      generalst%crestlength        = 1.0_dp
      generalst%allowedflowdir = FLOWDIR_BOTH
      generalst%uselimitFlowPos = .false.
      generalst%uselimitFlowNeg = .false.

   end subroutine readGateAsGenStru

   !> Helper routine to check the result status of a read/prop_get action.
   !! Checks if success is true or false, when false generate an error message.
   !! Result value is the original success value.
   function check_input_result(success, st_id, key) result (res)
      logical         , intent(in   )    :: success   !< Result value of the prop_get subroutine.
      character(len=*), intent(in   )    :: st_id     !< Id of the current structure.
      character(len=*), intent(in   )    :: key       !< Key of the input value.
      logical                            :: res       !< Result status, is equal to the original success value.
                                                      !< Recommended use: successall = successall .and. check_input_result(success, ..)

      if (.not. success) then
         write (msgbuf, '(a,a,a,a,a)') 'Error Reading Structure ''', trim(st_id), ''', ''', trim(key), ''' is missing.'
         call err_flush()
      endif
      res = success
      return 
   end function check_input_result


   !> Read the general structure parameters.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readGeneralStructure(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into.
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which general structure forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      character(len=Idlen) :: dirString
      logical              :: success1

      success = .true.
      allocate(generalst)

      generalst%wu1                = 10.0_dp
      call prop_get(md_ptr, '', 'upstream1Width', generalst%wu1, success1)
      generalst%wu2                = 10.0_dp
      call prop_get(md_ptr, '', 'upstream2Width',  generalst%wu2, success1)
      generalst%ws                 = 10.0_dp
      call get_value_or_addto_forcinglist(md_ptr, 'crestWidth', generalst%ws, st_id, ST_GENERAL_ST, forcinglist)
      generalst%wd1                = 10.0_dp
      call prop_get(md_ptr, '', 'downstream1Width', generalst%wd1, success1)
      generalst%wd2                = 10.0_dp
      call prop_get(md_ptr, '', 'downstream2Width',   generalst%wd2, success1)

      generalst%zu1                = 0.0_dp
      call prop_get(md_ptr, '', 'upstream1Level',   generalst%zu1, success1)
      generalst%zu2                = 0.0_dp
      call prop_get(md_ptr, '', 'upstream2Level',  generalst%zu2, success1)
      generalst%zs                 = 0.0_dp
      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel',    generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success1)
      generalst%zd1                = 0.0_dp
      call prop_get(md_ptr, '', 'downstream1Level', generalst%zd1, success1)
      generalst%zd2                = 0.0_dp
      call prop_get(md_ptr, '', 'downstream2Level',  generalst%zd2, success1)

      generalst%gateLowerEdgeLevel = 1.0e10_dp
      call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GENERAL_ST, forcinglist, success1)
      generalst%crestlength        = 0.0_dp
      call prop_get(md_ptr, '', 'crestLength',   generalst%crestlength)
      generalst%gatedoorheight     = 1.0e10_dp
      call prop_get(md_ptr, '', 'gateHeight',   generalst%gatedoorheight, success1)
      generalst%gateopeningwidth   = 0.0_dp
      call get_value_or_addto_forcinglist(md_ptr, 'gateOpeningWidth', generalst%gateopeningwidth, st_id, ST_GENERAL_ST, forcinglist, success1)

      dirString = 'symmetric'
      call prop_get(md_ptr, '', 'gateOpeningHorizontalDirection',   dirString)
      generalst%openingDirection = openingDirectionToInt(dirString)
      
      dirString = 'both'
      call prop_get(md_ptr, '', 'allowedFlowDir', dirString, success1)
      generalst%allowedflowdir = allowedFlowDirToInt(dirString)
      
      generalst%cgf_pos            = 1.0_dp
      call prop_get(md_ptr, '', 'posFreeGateflowCoeff',  generalst%cgf_pos)
      generalst%cgd_pos            = 1.0_dp
      call prop_get(md_ptr, '', 'posDrownGateFlowCoeff', generalst%cgd_pos)
      generalst%cwf_pos            = 1.0_dp
      call prop_get(md_ptr, '', 'posFreeWeirFlowCoeff',  generalst%cwf_pos)
      generalst%cwd_pos            = 1.0_dp
      call prop_get(md_ptr, '', 'posDrownWeirFlowCoeff', generalst%cwd_pos)
      generalst%mugf_pos           = 1.0_dp  
      call prop_get(md_ptr, '', 'posContrCoefFreeGate',  generalst%mugf_pos)
      
      generalst%cgf_neg            = 1.0_dp
      call prop_get(md_ptr, '', 'negFreeGateFlowCoeff',  generalst%cgf_neg)
      generalst%cgd_neg            = 1.0_dp
      call prop_get(md_ptr, '', 'negDrownGateFlowCoeff', generalst%cgd_neg)
      generalst%cwf_neg            = 1.0_dp
      call prop_get(md_ptr, '', 'negFreeWeirFlowCoeff',  generalst%cwf_neg)
      generalst%cwd_neg            = 1.0_dp
      call prop_get(md_ptr, '', 'negDrownWeirFlowCoeff', generalst%cwd_neg)
      generalst%mugf_neg           = 1.0_dp
      call prop_get(md_ptr, '', 'negContrCoefFreeGate',  generalst%mugf_neg)
      
      generalst%extraresistance    = 0.0_dp
      call prop_get(md_ptr, '', 'extraResistance', generalst%extraresistance)
      
      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)

   end subroutine readGeneralStructure


   !> Parses a (pump's) orientation string into an integer
   !! that can be used in its direction field.
   function orientationToInt(orientationString) result (res)
      character(len=*), intent(in) :: orientationString !< Orientation value as given in input file.
      integer                      :: res               !< The returned orientation integer code. +1 for positive, -1 for negative, 0 for invalid input.

      select case(str_tolower(trim(orientationString)))
      case('positive')
         res = 1
      case('negative')
         res = -1
      case default
         res = 0
      end select
      
   end function orientationToInt


   !> Parses a (pump's) controlSide string into an integer
   !! that can be used as part of its direction field.
   function controlSideToInt(controlSideString) result (res)
      character(len=*), intent(in) :: controlSideString !< controlSide value as given in input file.
      integer                      :: res               !< The returned controlSide integer code. 1 for suctionSide, 2 for deliverySide, 3 for both, 0 for invalid input.

      select case(str_tolower(trim(controlSideString)))
      case('suctionside')
         res = 1
      case('deliveryside')
         res = 2
      case('both')
         res = 3
      case default
         res = 0
      end select
      
   end function controlSideToInt


   integer function  openingDirectionToInt(dirString)
      character(len=*), intent(inout) :: dirString
      call str_lower(dirString)
      select case(dirString)
      case('symmetric')
         openingDirectionToInt = GEN_SYMMETRIC
      case('fromleft')
         openingDirectionToInt = GEN_FROMLEFT
      case('fromright')
         openingDirectionToInt = GEN_FROMRIGHT
      case default
         openingDirectionToInt = GEN_SYMMETRIC
      end select
      
   end function  openingDirectionToInt
   
   !> Gives the integer parameter constant for an 'allowedFlowDir' string value.
   !! An unknown/invalid value defaults to 0 (both).
   integer function allowedFlowDirToInt(flowdirString)
      character(len=*), intent(inout) :: flowdirString !< String value of the allowedFlowDir parameter.

      call str_lower(flowdirString)
      select case(flowdirString)
      case('both')
         allowedFlowDirToInt = FLOWDIR_BOTH
      case('positive')
         allowedFlowDirToInt = FLOWDIR_POSITIVE
      case('negative')
         allowedFlowDirToInt = FLOWDIR_NEGATIVE
      case('none')
         allowedFlowDirToInt = FLOWDIR_NONE
      case default
         allowedFlowDirToInt = FLOWDIR_BOTH
      end select
      
   end function  allowedFlowDirToInt


   !> Gives the string value for an 'allowedFlowDir' integer value.
   function allowedFlowDirToString(flowDirInt)
      integer,          intent(in   ) :: flowDirInt             !< Input integer value of the allowedFlowDir parameter.
      character(len=:), allocatable   :: allowedFlowDirToString !< String value for the given allowedFlowDir integer value.

      select case (flowDirInt)
      case (0)
         allowedFlowDirToString = 'both'
      case (1)
         allowedFlowDirToString = 'positive'
      case (2)
         allowedFlowDirToString = 'negative'
      case (3)
         allowedFlowDirToString = 'none'
      case default
         allowedFlowDirToString = 'invalid'
      end select
      
   end function allowedFlowDirToString

   !> Read the poly file and return the x and y coordinates.
   subroutine read_polyfile(filename, xs, ys, ns, success)
      use m_alloc, only: reallocP
      use messagehandling, only: err, IdLen
      character(len=*), intent(in) :: filename!< Name of the poly file to read.
      real(kind = dp), pointer, dimension(:), intent(out) :: xs(:) !< x-coordinates read from file
      real(kind = dp), pointer, dimension(:), intent(out) :: ys(:) !< y-coordinates read from file
      integer, intent(out) :: ns !< Number of pli-points read
      logical, intent(out) :: success !< Success status of the read operation.
      
      character(len=Idlen) :: rec
      integer :: minp 
      integer :: k
      integer :: ierr

      ns = 0

      success = .true.
      open (newunit=minp, file=filename, iostat=ierr)
      if (ierr /= 0) then
         call err('File: unable to open ', trim(filename), ' ')
         return
      end if

      rec(1:1) = '*'
      do while (rec(1:1) == '*')
         read (minp, '(a)', end=999) rec
      end do

      read (minp, '(a)', end=999) rec
      read (rec, *, err=888) ns

      call reallocP(xs, ns, keepExisting=.false.)
      call reallocP(ys, ns, keepExisting=.false.)

      do k = 1, ns
         read (minp, '(a)', end=999) rec
         read (rec, *, err=777) xs(k), ys(k)
      end do

      call doclose(minp)
      return

999   call eoferror(minp)
      success =  .false.
      return

888   call readerror('reading nrows but getting ', rec, minp)
      success =  .false.
      return

777   call readerror('reading x, y  but getting ', rec, minp)
      success =  .false.
      return

   end subroutine read_polyfile

end module m_readstructures
