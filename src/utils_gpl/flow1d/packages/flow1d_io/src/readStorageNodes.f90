module m_readStorageNodes
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
   use m_network
   use m_Storage
   use m_tables

   use properties
   use m_hash_search
   use string_module, only: str_tolower, strcmpi


   implicit none

   private

   public readStorageNodes
   
   ! Storage nodes file current version: 2.02
   integer, parameter :: storgNodesFileMajorVersion = 2
   integer, parameter :: storgNodesFileMinorVersion = 2
   
   ! History storage nodes file versions:

   ! 2.02 (2023-11-01): added various manhole loss coefficients for nodeType=compartment, both [Global] and per [StorageNode].
   ! 2.01 (2022-05-11): added branchId+chainage as possible location.
   ! 2.00 (2019-08-27): renamed to storage nodes, added x/y as possible location, added storage table option.
   ! 1.00 (2018-08-13): initial "urban" version of storage nodes ('retentions').

   contains

   !> Read storage nodes file, giving the file name
   subroutine readStorageNodes(network, storgNodesFile)
      use m_array_predicates, only: is_monotonically_increasing

      implicit none
      
      type(t_network), intent(inout)                :: network
      character*(*),   intent(in   )                :: storgNodesFile

      logical                                       :: success
      logical                                       :: success1, success2
      type(tree_data), pointer                      :: md_ptr
      type(tree_data), pointer                      :: node_ptr
      integer                                       :: istat
      integer                                       :: numstr
      integer                                       :: i

      character(len=IdLen)                          :: blockname
      character(len=IdLen)                          :: fileType
      character(len=IdLen)                          :: storgNodeId
      character(len=IdLen)                          :: storgNodeName
      character(len=IdLen)                          :: nodeId, branchId
      character(len=IdLen)                          :: sStorageType
      integer                                       :: storageType
      logical                                       :: useTable1
      character(len=IdLen)                          :: node_type
      
      double precision                              :: x, y
      double precision                              :: chainage
      integer                                       :: branchIdx
      integer                                       :: nodeIdx
      type(t_storage), pointer                      :: pSto
      
      double precision, allocatable, dimension(:)   :: streetLevel
      double precision, allocatable, dimension(:)   :: streetStorageArea
      integer                                       :: numLevels
      character(len=IdLen)                          :: sInterpolate
      integer                                       :: interpol
      
      logical                                       :: useStreetStorage
      double precision, allocatable, dimension(:)   :: storagelevels
      double precision, allocatable, dimension(:)   :: storageAreas
      integer                                       :: major, minor
      integer                                       :: jaxy, jageneral

      type(t_table), pointer  :: angle_loss_global        !< [Global] value for the lookup table that connects an angle to an energy loss coefficient
      double precision        :: entrance_loss_global     !< [Global] value for the entrance loss coefficient
      double precision        :: exit_loss_global         !< [Global] value for the exit loss coefficient
      double precision        :: expansion_loss_global    !< [Global] value for the loss coefficient for expansion or contraction

      call tree_create(trim(storgNodesFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(storgNodesFile),md_ptr, istat)

      msgbuf = 'Reading '//trim(storgNodesFile)//'.'
      call msg_flush()
      
      ! check FileVersion
      major = 0
      minor = 0
      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success1)
      if (.not. success1 .or. major < storgNodesFileMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of storage nodes file detected in '''//trim(storgNodesFile)//''': v', major, minor, '. Current format: v',storgNodesFileMajorVersion,storgNodesFileMinorVersion,'. Ignoring this file.'
         call warn_flush()
         goto 999
      end if

      ! check optional [Global] values for junction loss parameters
      call read_all_loss_values(md_ptr, 'Global', 'section [Global]', angle_loss_global, &
         entrance_loss_global, exit_loss_global, expansion_loss_global, success)

      if (.not. success) then
         goto 999
      end if

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      jageneral = 0
      success = .true.
      ! Read each block
      do i = 1, numstr
         node_ptr => md_ptr%child_nodes(i)%node_ptr
         blockname = tree_get_name(node_ptr)
         
         if (strcmpi(blockname, 'general')) then  ! Read [General] block
            if (jageneral > 0) then
               write(msgbuf, '(3a)') 'Found more than one [General] blocks in file ''', trim(storgNodesFile), '''. Only the first [General] block is read, others are ignored.'
               call warn_flush()
               cycle
            end if
               
            ! read fileType
            call prop_get(node_ptr, '', 'fileType', fileType, success)
            if ((.not. success) .or. (.not. strcmpi(fileType,'storagenodes'))) then
               write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''fileType'' is missing or not correct. Support fileType = storagenodes. Ignoring this file'
               call warn_flush()
               cycle
            endif
            
            ! read useStreetStorage
            call prop_get(node_ptr, '', 'useStreetStorage', useStreetStorage, success)
            if (.not. success) then
               useStreetStorage = .true.
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''useStreetStorage'' is missing. Use default value useStreetStorage = true.'
               call warn_flush()
            endif
            jageneral = 1
            cycle
         else if (strcmpi(blockname,'StorageNode')) then   ! Read [StorageNode] block

            nodeIdx = -1
            branchIdx = -1
            success = .true.
            jaxy    = 0
            ! read id
            call prop_get(node_ptr, '', 'id', storgNodeId, success1)
            if (.not. success1) then
               write (msgbuf, '(a,i0,a)') 'Error Reading storage node #', network%storS%Count + 1, ', id is missing.'
               call err_flush()
               cycle
            end if
            
            ! read name
            call prop_get(node_ptr, '', 'name', storgNodeName, success1)
            success = success .and. check_input(success1, storgNodeId, 'name')

            ! read optional node type
            node_type = ''
            call prop_get(node_ptr, '', 'nodeType', node_type, success1)

            ! read location
            call prop_get(node_ptr, '', 'nodeId', nodeId, success1)
            call prop_get(node_ptr, '', 'branchId', branchId, success2)
            if (success1 .and. success2) then
               ! The input can contain only a nodeId or a branchId, chainage specification.
               write(msgbuf, '(3a)') 'Inconsistent block in: [', trim(storgNodeId),  &
                              ']. Either "nodeId" or "branchId, chainage" must be specified (and not both).'
               call err_flush
               cycle
            else if (success1) then
               ! Location specification by nodeId.
               nodeIdx = hashsearch(network%nds%hashlist, nodeId)
               if (nodeIdx <= 0) Then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Storage Node '''//trim(storgNodeID)//''': node: '''//trim(nodeID)//''' not Found in network.')
                  cycle
               endif
            else if (success2) then
               ! Location specification by branchId, chainage.
               branchIdx = hashsearch(network%brs%hashlist, branchId)
               if (branchIdx <= 0) Then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Storage Node '''//trim(storgNodeID)//''': branch: '''//trim(branchId)//''' not Found in network.')
                  cycle
               endif
               
               call prop_get(node_ptr, '', 'chainage', chainage, success1)
               success = check_input(success1, trim(storgNodeId), 'chainage')
               if (.not. success) then 
                  cycle
               endif

            else
               ! read x-, y-coordinates
               call prop_get(node_ptr, '', 'x', x, success1)
               success = success .and. success1
               call prop_get(node_ptr, '', 'y', y, success1)
               success = success .and. success1
               if (.not. success) then
                  write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Either "nodeId", "branchId, chainage" or "x, y" must be specified.'
                  call err_flush()
                  cycle
               else
                  jaxy      = 1
               end if
            end if
            
            ! read useTable
            call prop_get(node_ptr, '', 'useTable', useTable1, success1)
            success = success .and. check_input(success1, storgNodeId, 'useTable')
            
            if (.not. useTable1) then
               numLevels = 1
            else
               ! read numLevels
               call prop_get(node_ptr, '', 'numLevels', numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'numLevels')
            end if
            
            ! Allocate Arrays storageLevels, storageAreas
            call realloc(storageLevels, numLevels, stat=istat)
            if (istat == 0) then
               call realloc(storageAreas, numLevels, stat=istat, fill = 0d0)
            else
               call SetMessage(LEVEL_FATAL, 'Reading storage nodes: Error Allocating Arrays')
            endif
            
            ! read data
            if (.not. useTable1) then
               call realloc(streetLevel, numLevels, stat=istat)
               if (istat == 0) then
                  call realloc(streetStorageArea, numLevels, stat=istat, fill = 0d0)
               else
                  call SetMessage(LEVEL_FATAL, 'Reading storage nodes: Error Allocating Arrays')
               endif
            
               sInterpolate = 'block'
               call interpolateStringToInteger(sInterpolate, interpol)
               ! read bedLevel
               call prop_get(node_ptr, '', 'bedLevel', storageLevels(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'bedLevel', storageLevels(1))
               
               ! read area
               call prop_get(node_ptr, '', 'area', storageAreas(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'area')   
               
               ! read streetLevel
               call prop_get(node_ptr, '', 'streetLevel', streetLevel(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'streetLevel')
               
               ! read storageType
               call prop_get(node_ptr, '', 'storageType', sStorageType, success1)
               if (.not. success1) then 
                  sStorageType = 'reservoir'
               end if
               
               call storageTypeStringToInteger(sStorageType, StorageType)
               if (StorageType /= nt_Reservoir .and. StorageType /= nt_Closed) then
                  write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''storageType'' is not correct. Supported values are "reservoir" and "closed".'
                  call err_flush()
               end if
               
               ! read streetStorageArea
               if (useStreetStorage) then
                  if (strcmpi(sStorageType, 'reservoir')) then
                     call prop_get(node_ptr, '', 'streetStorageArea', streetStorageArea(1), success1)
                     success = success .and. check_input(success1, storgNodeId, 'streetStorageArea')
                  else if (strcmpi(sStorageType, 'closed')) then
                     streetStorageArea(1) = slot_area
                  end if
               end if
            else
               ! read levels
               call prop_get(node_ptr, '', 'levels', storageLevels, numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'levels')
               
               ! read storageArea
               call prop_get(node_ptr, '', 'storageArea', storageAreas, numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'storageArea')
               
               ! read interpolate
               call prop_get(node_ptr, '', 'interpolate', sInterpolate, success1)
               if (.not. success1) then
                  sInterpolate = 'linear'
                  write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''interpolate'' is missing. Use default value interpolate = linear.'
                  call warn_flush()
               end if
               if ((.not. strcmpi(sInterpolate, 'linear')) .and. (.not. strcmpi(sInterpolate, 'block'))) then
                  success1 = .false.
                  write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''interpolate'' is not correct. Supported values are "linear" and "block".'
                  call err_flush()
               end if
               success = success .and. success1
               call interpolateStringToInteger(sInterpolate, interpol)
            end if
            
            if (storageLevels(1) == -999d0) then
               call SetMessage(LEVEL_ERROR, 'Bed Level for storage node ' // trim(storgNodeId) // ' was set to missing value -999.0, which is not supported for storage nodes. Please enter an actual value.')
            endif
            if (storageAreas(1) <= 0d0) then
               call setMessage(LEVEL_ERROR, 'Area at Bed Level for storage node ' // trim(storgNodeId) // ' <= 0.0. Please enter a positive value')
            endif

            if (success) then ! If reading variables are successful, then store the obtained info. to the corresponding places
               network%storS%Count = network%storS%Count + 1
               if (network%storS%Count > network%storS%Size) then
                  call realloc(network%storS)
               endif
      
               pSto => network%storS%stor(network%storS%Count)
               nullify(pSto%storage_area)
               nullify(pSto%street_area)

               pSto%id           = storgNodeId
               pSto%name         = storgNodeName
               pSto%node_index   = -1
               pSto%branch_index = -1
               if (nodeIdx > 0) then
                  pSto%node_id    = nodeId
                  pSto%node_index = nodeIdx
               else if (branchIdx > 0) then
                  psto%branch_index = branchIdx
                  pSto%chainage = chainage
               else
                  network%storS%Count_xy = network%storS%Count_xy + 1
                  pSto%x         = x
                  pSto%y         = y
                  pSto%node_index = -1 ! node_index will be computed later when calling subroutine set_node_numbers_for_storage_nodes 
               end if
               pSto%use_street_storage = useStreetStorage
               pSto%use_table          = useTable1
            
            
               ! setTable
               call setTable(pSto%storage_area, interpol, storageLevels, storageAreas, numLevels)
               if (.not. useTable1) then
                  pSto%storage_type = storageType
                  if (storageType == nt_Closed) then
                     network%storS%Count_closed = network%storS%Count_closed + 1
                  end if
                  if (useStreetStorage) then
                     call setTable(pSto%street_area, interpol, streetLevel, streetStorageArea, numLevels)
                  end if
               else
                  pSto%storage_type = nt_Reservoir
               end if               
            endif

            if (strcmpi(node_type, 'compartment')) then
               ! Set manhole loss coefficients

               ! Each storage node starts with default values coming from the [Global] values:
               pSto%angle_loss     => angle_loss_global
               pSto%entrance_loss  =  entrance_loss_global
               pSto%exit_loss      =  exit_loss_global
               pSto%expansion_loss =  expansion_loss_global

               ! Override the coefficients that are set for this particular storage node.
               call read_all_loss_values(node_ptr, '', 'StorageNode id = '//trim(pSto%id), pSto%angle_loss, &
                  pSto%entrance_loss, pSto%exit_loss, pSto%expansion_loss, success)
            end if
         end if
         
      end do
      
      ! Clear Arrays
      istat = 0
      if (allocated(storageLevels)) deallocate(storageLevels, stat=istat)
      if (istat == 0 .and. allocated(storageAreas)) deallocate(storageAreas, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_ERROR, 'Reading storage nodes file: Error Deallocating Arrays')
      endif
      
      write(msgbuf,'(a,a,a,i0,a)') 'Done reading storage nodes file ''', trim(storgNodesFile), ''', ', network%storS%Count, ' storage nodes have been read.'
      call msg_flush()

      call fill_hashtable(network%storS)
999   continue
      call tree_destroy(md_ptr)

   contains
      !> Helper subroutine to read all loss coefficients (angle loss table +
      !! scalar coefficients), either from [Global] data, or for a specific StorageNode.
      subroutine read_all_loss_values(tree_ptr, chapter_name, section_string, angle_loss, entrance_loss, exit_loss, expansion_loss, success)
         use m_tables, only : t_table, realloc
         type(tree_data), pointer     , intent(in   ) :: tree_ptr       !< The input tree to read from.
         character(len=*),              intent(in   ) :: chapter_name   !< Which chapter to read from (use 'Global' for global reading, or '' when tree_ptr already contains a single specific storage node).
         character(len=*),              intent(in   ) :: section_string !< Character string used only in error messages, describing in which input section faulty input was read.
         type(t_table), pointer       , intent(inout) :: angle_loss     !< Table with angle-loss coefficient values, will be pointed to newly allocated memory of correct length,
                                                                        !< *if* new table data has been read, otherwise the original pointer is left unchanged.
                                                                        !< Any table data originally pointed to will intentionally be left intact, as that may be the [Global] table.
         double precision             , intent(inout) :: entrance_loss  !< Value for the entrance loss coefficient
         double precision             , intent(inout) :: exit_loss      !< Value for the exit loss coefficient
         double precision             , intent(inout) :: expansion_loss !< Value for the loss coefficient for expansion or contraction
         logical,                       intent(  out) :: success        !< Success status (.false. if something went wrong, check log messages)

         integer :: num_angles
         type(t_table), pointer :: new_angle_loss !< New table with angle-loss coefficient values, the dummy angle_loss table will be re-pointered to this new one.

         success = .true.

         num_angles     = 0
         entrance_loss  = 0d0
         exit_loss      = 0d0
         expansion_loss = 0d0
         call prop_get(tree_ptr, chapter_name, 'angleCount', num_angles)
         if (num_angles > 0) then
            nullify(new_angle_loss)
            call realloc(new_angle_loss, num_angles)

            call prop_get(tree_ptr, chapter_name, 'angles', new_angle_loss%x, num_angles, success)
            if (.not. success) then
               write(msgbuf, '(5a,i0,a)') 'Incorrect input for angles in ''', trim(storgNodesFile), ''', ', trim(section_string), '. Expecting ', num_angles, ' values.'
               call err_flush()
               goto 888
            endif
            if (.not. is_monotonically_increasing(new_angle_loss%x, num_angles)) then
               write(msgbuf, '(5a)') 'Incorrect input for angles in ''', trim(storgNodesFile), ''', ', trim(section_string), '. Angles should be monotonically increasing.'
               call err_flush()
               goto 888
            endif

            call prop_get(tree_ptr, chapter_name, 'angleLossCoefficient', new_angle_loss%y, num_angles, success)
            if (.not. success) then
               write(msgbuf, '(5a,i0,a)') 'Incorrect input for angleLossCoefficient in ''', trim(storgNodesFile), ''', ', trim(section_string), '. Expecting ', num_angles, ' values.'
               call err_flush()
               goto 888
            endif
            
            new_angle_loss%length = num_angles
            
            ! Angle loss table successfully read, direct the input table pointer to the new one.
            angle_loss => new_angle_loss
         end if

         call prop_get(tree_ptr, chapter_name, 'entranceLossCoefficient',  entrance_loss)
         call prop_get(tree_ptr, chapter_name, 'exitLossCoefficient',      exit_loss)
         call prop_get(tree_ptr, chapter_name, 'expansionLossCoefficient', expansion_loss)

         ! Return with success
         return

888      success = .false.
         ! Some error occurred
         return
      end subroutine read_all_loss_values

   end subroutine readStorageNodes
   
   !> Converts interpolate type string to an integer.
   !! Returns -1 when an invalid type string is given.
   subroutine interpolateStringToInteger(sinterpol, interpol)
      implicit none
      character(len=*), intent(in   ) :: sinterpol        !< interpolate type string
      integer,          intent(  out) :: interpol         !< interpolate type integer
      
      select case (trim(str_tolower(sinterpol)))
         case ('linear')
            interpol = 0
         case ('block')
            interpol = 1
         case default
            interpol = -1
      end select
      return

   end subroutine interpolateStringToInteger
   
   !> Converts storage type string to an integer.
   !! Returns nt_None when an invalid type string is given.
   subroutine storageTypeStringToInteger(sStorgType, storgType)
      implicit none
      character(len=*), intent(in   ) :: sStorgType        !< storage type string
      integer,          intent(  out) :: storgType         !< storage type integer
      
      character(len=:), allocatable :: sStorgType_ 
      sStorgType_ = sStorgType
      call str_lower(sStorgType_)
      select case (trim(sStorgType_))
         case ('reservoir')
            storgType = nt_Reservoir
         case ('closed')
            storgType = nt_Closed
         case default
            storgType = nt_None
      end select
      return

   end subroutine storageTypeStringToInteger

   !> Helper routine to check the result status of a read/prop_get action.
   !! Checks if success is true or false, when false generate an error message.
   !! Result value is the original success value.
   !! Moreover, as an optional choice, it checks if the input value (double precision) equals to dmiss(-999). If yes,
   !! then a warning message will be written.
   function check_input(success, st_id, key, val) result (res)
      use m_missing, only: dmiss
      implicit none
      logical         ,           intent(in   ) :: success !< Result value of the prop_get subroutine.
      character(len=*),           intent(in   ) :: st_id   !< Id of the current storage node.
      character(len=*),           intent(in   ) :: key     !< Key of the input value.
      logical                                   :: res     !< Result status, is equal to the original success value.
                                                           !< Recommended use: successall = successall .and. check_input_result(success, ..)
      double precision, optional, intent(in   ) :: val     !< The input value to be checked

      if (.not. success) then
         write (msgbuf, '(a,a,a,a,a)') 'Error Reading storage Node ''', trim(st_id), ''', ''', trim(key), ''' is missing.'
         call err_flush()
      endif
      res = success

      if (present(val)) then
         if (abs(val-dmiss)<1d-8) then
            write(msgbuf, '(a,a,a,a,a)') 'Reading storage Node ''', trim(st_id), ''', the input value for ', trim(key), ' is -999, which can cause problems in computation.'
            call warn_flush()
         endif
      end if

      return 
   end function check_input
end module m_readStorageNodes