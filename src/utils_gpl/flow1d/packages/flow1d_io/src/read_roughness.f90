module m_read_roughness
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
   
   use m_Branch
   use m_GlobalParameters
   use m_hash_search
   use m_network
   use m_Roughness
   use m_spatial_data
   use properties
   use string_module
   use messagehandling

   implicit none

   private

   public roughness_reader
   public frictionTypeStringToInteger

   !> The file version number of the roughness file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Roughness file current version: 3.00
   integer, parameter, public       :: RoughFileMajorVersion = 3
   integer, parameter, public       :: RoughFileMinorVersion = 0
   
   ! History roughness file versions:

   ! 3.00 (2019-06-18): Use strings, instead of integers, for "frictionType" and "functionType".
   ! 2.00 (2019-05-31): A completely new description of roughness file, see issue UNST-2388.
   ! 1.01 (2019-03-12): First version of *.ini type roughness file.

contains

   !> Read all roughness ini-files
   subroutine roughness_reader(network, roughnessfiles, md_ptr)
      type(t_network), intent(inout), target :: network                !< Network structure
      character(len=*), intent(in)           :: roughnessfiles         !< separated list of roughness files
      type(tree_data), pointer, intent(in), optional   :: md_ptr       !< treedata pointer to model definition file

      type(t_RoughnessSet), pointer          :: rgs
      type(t_branchSet), pointer             :: brs
      type(t_spatial_dataSet) , pointer      :: spData
      
      character(len=1024)                    :: inputfiles
      integer                                :: ifrst
      integer                                :: isemi
      integer                                :: count
      integer                                :: def_type
      logical                                :: success
      character(len=IdLen)                  :: file
      double precision                       :: default

      inputfiles = roughnessfiles
      default = 60d0
      def_type = 1
      def_type = 1
      network%rgs%roughnessFileMajorVersion = RoughFileMajorVersion
      !> Check if the model definition file contains global values for roughness
      if (present(md_ptr)) then
         call prop_get(md_ptr, 'GlobalValues', 'roughness', default, success)
         if (success) then
            call prop_get(md_ptr, 'GlobalValues', 'roughnessType', def_type, success)
         endif
         if (.not. success) then
            def_type = R_CHEZY
            default = 45
         endif
      endif
      
      rgs    => network%rgs
      brs    => network%brs
      spdata => network%spdata
      
      ! initialize hash_search
      ifrst = 1
      isemi = 1
      count = 1
      do while (len_trim(inputfiles(ifrst:)) > 0) 
         isemi = scan(inputfiles(ifrst:), ';')
         if (isemi ==0) then
            isemi = len_trim(inputfiles(ifrst:))+1
         endif
         isemi = ifrst+isemi - 1
         ifrst = isemi+1
         count = count+1
      enddo
   
      call hashfill_init(rgs%hashlist, count)
      call realloc(rgs%hashlist%id_list, count)
      call realloc(rgs)
   
      ! now start reading individual files
      do while (len_trim(inputfiles) > 0) 
         isemi = scan(inputfiles, ';')
         if (isemi ==0) then
            isemi = len_trim(inputfiles)+1
         endif
         
         file = inputfiles(1:isemi-1)
         inputfiles = inputfiles(isemi+1:)
            
         call remove_leading_spaces(trim(file))
         call read_roughnessfile(rgs, brs, spdata, file, default, def_type)
      enddo

      if (getMaxErrorLevel() >= LEVEL_ERROR) then
         call setmessage(LEVEL_FATAL, 'Errors found during reading of roughness files, please review earlier error messages for details')
      endif

      call add_timeseries_to_forcinglist(rgs, network%forcinglist)
   end subroutine roughness_reader

   !> scan all roughness sections for timeseries and subsequently register them in the forcinglist
   subroutine add_timeseries_to_forcinglist(rgs, forcinglist)
      use m_roughness
      
      type (t_RoughnessSet), intent(inout) :: rgs            !< Roughness set
      type (t_forcinglist), intent(inout)  :: forcinglist    !< Forcing list

      type(t_roughness), pointer          :: prgh
      integer                             :: i, j, count
      integer                             :: ibr
      
      do i = 1, rgs%count
         prgh => rgs%rough(i)
         if (prgh%timeSeriesIds%id_count > 0) then
            count = prgh%timeSeriesIds%id_count 
            rgs%timeseries_defined = .true.
            call realloc(prgh%currentValues, count)
            call realloc(prgh%timeDepValues, (/ count, 2 /))
            prgh%timeDepValues = -10d0

            do j = 1, count

               ! Extend forcinglist by one and reallocate in case of insufficient space
               forcinglist%Count = forcinglist%Count+1
               if (forcinglist%Count > forcinglist%Size) then
                  call realloc(forcinglist)
               end if

               ! For correct roughness type we need a branch index:
               do ibr = 1, size(prgh%timeSeriesIndexes)
                  if (prgh%timeSeriesIndexes(ibr) == j) then
                     exit
                  endif
               enddo
               forcinglist%forcing(forcinglist%Count)%object_id   = prgh%timeSeriesIds%id_list(j)
               forcinglist%forcing(forcinglist%Count)%quantity_id = 'friction_coefficient_'//         &
                                             trim(frictionTypeIntegerToString(prgh%rgh_type_pos(ibr)))
               forcinglist%forcing(forcinglist%Count)%param_name  = frictionTypeIntegerToString(prgh%rgh_type_pos(ibr))
               forcinglist%forcing(forcinglist%Count)%targetptr  => prgh%timeDepValues(j,2)
               forcinglist%forcing(forcinglist%Count)%filename    = prgh%frictionValuesFile
               forcinglist%forcing(forcinglist%Count)%object_type = 'friction_coefficient'
   
            enddo
         endif

      enddo

   end subroutine add_timeseries_to_forcinglist

   !> Read a specific roughness file, taking the file version into account.
   subroutine read_roughnessfile(rgs, brs, spdata, inputfile, default, def_type)
   
      type(t_roughnessSet), intent(inout)    :: rgs        !< Roughness set
      type(t_branchSet), intent(in)          :: brs        !< Branches
      type(t_spatial_dataSet), intent(inout) :: spdata     !< Spatial data set
      character(len=IdLen), intent(in)      :: inputfile  !< Name of the input file
      double precision, intent(inout)        :: default    !< Default friction parameter
      integer, intent(inout)                 :: def_type   !< Default friction type
   
      integer                                :: major
      integer                                :: minor
      integer                                :: istat
      logical                                :: success
      type(tree_data), pointer               :: tree_ptr
   
      ! create and fill tree
      call tree_create(trim(inputfile), tree_ptr, maxlenpar)
      call prop_file('ini',trim(inputfile),tree_ptr,istat)

      msgbuf = 'Reading '//trim(inputfile)//'.'
      call msg_flush()

      if (istat /= 0) then
         call setmessage(LEVEL_ERROR, 'Roughness file '''//trim(inputfile)//''' could not be opened.')
         return
      end if

      call get_version_number(tree_ptr, major = major, minor = minor, success = success)
      if (.not. success) then
         major = 1
         minor = 0
      endif
      if (rgs%version == -1) then
         rgs%version = major
      else if (rgs%version /= major) then
         call setmessage(LEVEL_ERROR, 'Roughness files with different versions are not allowed in one model')
         return
      endif
      
      select case(major)
      case(RoughFileMajorVersion)
         call scan_roughness_input(tree_ptr, rgs, brs, spdata, inputfile, default, def_type)
      case default
         call SetMessage(LEVEL_ERROR,'Unsupported fileVersion for roughness file: '//trim(inputfile))
         return
      end select
   end subroutine read_roughnessfile


   !> Reads a single roughness file of current version.
   !! File must already have been opened into an ini tree.
   subroutine scan_roughness_input(tree_ptr, roughness_set, branches, spdata, inputfile, default, def_type)
      use m_tablematrices
      use m_alloc
      use string_module, only: strcmpi
      
      type(tree_data), pointer, intent(in)   :: tree_ptr   !< treedata pointer to input file, must already be created.
      type(t_roughnessSet), intent(inout)    :: roughness_set
      type(t_branchSet), intent(in)          :: branches
      type(t_spatial_dataSet), intent(inout) :: spdata     !< Spatial data set
      character(len=IdLen), intent(in)      :: inputfile  !< Name of the input file
      double precision, intent(inout)        :: default    !< Default friction parameter
      integer, intent(inout)                 :: def_type   !< Default friction type
      
      integer                                :: count
      integer                                :: numlocations
      integer                                :: numlevels
      integer                                :: irgh
      integer                                :: ibr
      integer                                :: i
      integer                                :: numSections
      integer                                :: maxlocations
      integer                                :: maxlevels
      logical                                :: success
      logical                                :: branchdef
      type(t_roughness), pointer             :: rgh
      character(len=Idlen)                   :: frictionId
      character(len=Idlen)                   :: branchid
      character(len=Idlen)                   :: timeseriesId
      double precision, allocatable          :: levels(:)
      double precision, allocatable          :: locations(:)
      double precision, allocatable          :: values(:)
   
      character(len=Idlen)                   :: fricType
      character(len=Idlen)                   :: funcType
      character(len=IdLen)                  :: frictionValuesFileName
     
      count = 0
      if (associated(tree_ptr%child_nodes)) then
            count = size(tree_ptr%child_nodes)
      end if
   
      !Scan for global sections 
      numSections = 0
      branchdef = .false.
      do i = 1, count
         if (strcmpi(tree_get_name(tree_ptr%child_nodes(i)%node_ptr), 'Global')) then
            numsections = numSections+1
         elseif (strcmpi(tree_get_name(tree_ptr%child_nodes(i)%node_ptr), 'Branch')) then
            branchdef = .true.
         endif
      enddo
      
      if (numsections >=2 .and. branchdef) then
         call setmessage(LEVEL_ERROR, 'In inputfile '//trim(inputfile)// ' more than 1 Global section is found, together with a Branch section, this is not allowed')
         return
      endif
         
      frictionValuesFileName = trim(inputfile)
      
      !> when branches are defined, the friction can be defined per branch, then additional arrays are required
      if (branchdef) then
         
         ! *If* there's [Branch] blocks, then there will be one and only one [Global] block.
         call prop_get(tree_ptr, 'Global', 'frictionId', frictionId, success)
         if (.not. success) then
            call setmessage(LEVEL_ERROR, 'frictionId not found in roughness definition file: '//trim(inputfile))
            return
         endif

         call prop_get(tree_ptr, 'General', 'frictionValuesFile', frictionValuesFileName, success)

         irgh = hashsearch(roughness_set%hashlist, frictionId)
         if (irgh /= -1) then
            call setmessage(LEVEL_ERROR, 'frictionId '''//trim(frictionId)//&
               ''' in roughness definition file '//trim(inputfile)//&
               ' was already defined. Please provide a unique frictionId')
            return
         endif
         irgh = hashsearch_or_add(roughness_set%hashlist, frictionId)
         if (irgh > roughness_set%size) then
            call realloc(roughness_set)
         endif
         rgh => roughness_set%rough(irgh)
         if (irgh == roughness_set%count+1) then
            ! Create a new Roughness section.
            roughness_set%count = irgh
            rgh%id           = frictionId
            allocate(rgh%rgh_type_pos(branches%Count))
            allocate(rgh%fun_type_pos(branches%Count))
            allocate(rgh%table(branches%Count))
         else
            ! Initialize an existing Roughness section.
            if (.not. associated(rgh%rgh_type_pos))   allocate(rgh%rgh_type_pos(branches%Count))
            if (.not. associated(rgh%fun_type_pos))   allocate(rgh%fun_type_pos(branches%Count))
            if (.not. associated(rgh%table))          allocate(rgh%table(branches%Count))
         endif         
   
         if (.not. associated(rgh%timeSeriesIndexes)) then
            allocate(rgh%timeSeriesIndexes(branches%Count))
            rgh%timeSeriesIndexes = -1
         endif

         rgh%rgh_type_pos = -1
         rgh%fun_type_pos = -1
         do i = 1, branches%count
            rgh%table(i)%lengths = -1
         enddo
      endif
      
      maxlevels    = 0
      maxlocations = 0
      ! Now scan the complete input
      do i = 1, count
         if (strcmpi(tree_get_name(tree_ptr%child_nodes(i)%node_ptr), 'Global')) then
            ! Get section id
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionId', frictionId, success)
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'frictionId not found in roughness definition file: '//trim(inputfile))
            endif
            ! Look if section Id is already defined, otherwise add it to the list
            irgh = hashsearch_or_add(roughness_set%hashlist, frictionId)
            if (irgh == roughness_set%count+1) then
               roughness_set%count = irgh
               if (roughness_set%count > roughness_set%size) then
                  call realloc(roughness_set)
               endif
            endif
            
            roughness_set%rough(irgh)%useGlobalFriction  = .not. branchdef
            roughness_set%rough(irgh)%frictionValuesFile = frictionValuesFileName
            roughness_set%rough(irgh)%id                 = frictionId
            fricType = ''
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionType', fricType, success)
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'frictionType not found in roughness definition file '''//trim(inputfile)//''' for frictionId='//trim(frictionId)//'.')
            end if
            call frictionTypeStringToInteger(fricType, roughness_set%rough(irgh)%frictionType)
            if (roughness_set%rough(irgh)%frictionType < 0) then
               call setmessage(LEVEL_ERROR, 'frictionType '''//trim(fricType)//''' invalid in roughness definition file '''//trim(inputfile)//''' for frictionId='//trim(frictionId)//'.')
            end if
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionValue', roughness_set%rough(irgh)%frictionValue, success)
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'frictionValue not found/valid in roughness definition file '''//trim(inputfile)//''' for frictionId='//trim(frictionId)//'.')
            end if
         else if (strcmpi(tree_get_name(tree_ptr%child_nodes(i)%node_ptr), 'Branch')) then
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchid, success)
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'branchId not found in chapter Branch of input file: '//trim(inputfile))
               cycle
            endif
            
            ibr = hashsearch(branches%hashlist, branchid)
            if (ibr <= 0 .or. ibr > branches%count) then
               call setmessage(LEVEL_ERROR, 'branchId '//trim(branchid)//' does not exist in network, see input file: '//trim(inputfile))
               cycle
            endif
            
            fricType = ''
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionType', fricType, success)
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'Missing frictionType for branchId '//trim(branchid)//', see input file: '//trim(inputfile))
               cycle
            end if
            call frictionTypeStringToInteger(fricType, rgh%rgh_type_pos(ibr))
            if (rgh%rgh_type_pos(ibr) < 0) then
               call setmessage(LEVEL_ERROR, 'frictionType '''//trim(fricType)//''' invalid for branchId '//trim(branchid)//', see input file: '//trim(inputfile))
               cycle
            endif

            funcType = 'constant'
            call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'functionType', funcType, success)
            call functionTypeStringToInteger(funcType, rgh%fun_type_pos(ibr))
            if (rgh%fun_type_pos(ibr) < 0) then
               call setmessage(LEVEL_ERROR, 'functionType '''//trim(funcType)//''' invalid for branchId '//trim(branchid)//', see input file: '//trim(inputfile))
               cycle
            endif
            
            if (rgh%fun_type_pos(ibr) == R_FunctionTimeseries) then
               call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'timeSeriesId', timeseriesId, success)   
               if (.not. success) then
                  call setmessage(LEVEL_ERROR, 'timeSeriesId is required for functionType='//trim(funcType)//', but was not found in the input for branchId '//trim(branchid)//', see input file: '//trim(inputfile))
                  cycle
               endif
               rgh%timeSeriesIndexes(ibr) = hashsearch_or_add(rgh%timeSeriesIds, timeseriesId)
               numlevels = 0
               maxlevels = 1
               numlocations = 0
               maxlocations = 1
            else
               numlevels = 0
               call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'numLevels', numlevels, success)
               numlocations = 0
               call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'numLocations', numlocations, success)
               success = .true.

               maxlevels    = max(1, maxlevels,    numlevels)
               maxlocations = max(1, maxlocations, numlocations)
            endif

            call realloc(levels,    maxlevels,              keepExisting=.false.)
            call realloc(locations, maxlocations,           keepExisting=.false.)
            call realloc(values,    maxlevels*maxlocations, keepExisting=.false.)

            if (numlevels > 0) then
               call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'levels', levels, numlevels, success)
            else
               numlevels = 1
               levels(1) = 0d0
            endif
            
            if (success .and. numlocations > 0) then
               call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'chainage', locations, numlocations, success)
            else 
               numlocations = 1
               locations(1) = 0d0
            endif
            
            if (success) then
               if (rgh%fun_type_pos(ibr) == R_FunctionTimeseries) then
                  call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionValue', values, numlevels*numlocations, success)
               else
                  call prop_get(tree_ptr%child_nodes(i)%node_ptr, '', 'frictionValues', values, numlevels*numlocations, success)
               endif
               
            endif
            
            if (.not. success) then
               call setmessage(LEVEL_ERROR, 'Missing data for branchid '//trim(branchid)//' see input file: '//trim(inputfile))
               cycle
            endif

            call setTableMatrix(rgh%table(ibr), locations, levels, (/numlocations, numlevels/), linear=values)
         endif
      enddo   
  
   end subroutine scan_roughness_input

end module m_read_roughness
