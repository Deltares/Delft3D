module m_readCrossSections
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

   use M_newcross
   use m_CrossSections
   use MessageHandling
   use properties
   use m_network
   use m_GlobalParameters
   use m_hash_search
   use m_spatial_data
   use string_module, only: strcmpi


   implicit none

   private
   
   public readCrossSectionDefinitions
   public readCrossSectionLocationFile
   public finalizeCrs
   !public write_cross_section_definition_cache
   !public write_cross_section_cache
   !public write_convtab
   !public read_convtab

   !> The file version number of the cross section definition file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.

   ! Cross section definition file current version: 3.00
   integer, parameter :: CrsDefFileMajorVersion      = 3
   integer, parameter :: CrsDefFileMinorVersion      = 0

   double precision, parameter :: position_tolerance = 1e-4

   ! History cross section definition file versions:
   
   ! 3.00 (2019-06-18): use strings, instead of integers, for "closed" and "frictionType(s)".
   ! 2.00 (2019-05-29): A completely new description of cross section definition file. See more details in issue UNST-2387.
   ! 1.01 (2019-03-12): First version of *.ini type cross section definition file.

   contains
    
   !> Read the cross section location file
   subroutine readCrossSectionLocationFile(network, CrossSectionfile)
      use m_CrossSections
      use m_network
      type(t_network), intent(inout) :: network                 !< Network structure
      character(len=*), intent(in)   :: CrossSectionFile        !< name of the crossection location input file 

      type(tree_data), pointer       :: md_ptr
      integer                        :: istat
      integer                        :: numstr
      integer                        :: i
      character(len=IdLen)           :: branchid
      character(len=IdLen)           :: defid
      integer                        :: iref
      integer                        :: inext
      integer                        :: indx
      logical                        :: success
      type(t_CrossSection), pointer  :: pCrs
      integer                        :: numcrs
      integer                        :: maxErrorLevel


      
      call tree_create(trim(CrossSectionfile), md_ptr, maxlenpar)
      call prop_file('ini',trim(CrossSectionfile),md_ptr,istat)

      msgbuf = 'Reading '//trim(CrossSectionfile)//'.'
      call msg_flush()

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      success = .true.
      numcrs = 0
      do i = 1, numstr
         if (network%crs%count+1 > network%crs%size) then
            call realloc(network%crs)
         endif
         inext = network%crs%count+1
         pCrs => network%crs%cross(inext)
         
         if (.not. strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'CrossSection')) then
            cycle
         else
            numcrs = numcrs + 1
         endif
         
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'id', pCrs%csid, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection #', numcrs, &
               ' in '''//trim(CrossSectionFile)//'''. No id was given.'
            call err_flush()
            cycle
         endif

         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchid, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection id '''//trim(pCrs%csid)// &
               ''' in '''//trim(CrossSectionFile)//'''. No branchId was given.')
            cycle
         endif
         
         indx = hashsearch(network%brs%hashlist, branchid)
         pCrs%branchid = indx
         call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', pCrs%chainage, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection on branch '//trim(branchid)// &
               '. No chainage was given.')
            cycle
         endif
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'definitionId', defid, success)
         if (.not. success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'definition', defid, success) ! Backwards compatibility
         if (success) then
            iref = hashsearch(network%CSDefinitions%hashlist, defid)
            if (iref < 1) then
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '''//trim(pCrs%csid)//''' on branch '''//trim(branchid)// &
                     '''. Specified definitionId '''//trim(defid)//''' was not found in definitions.')
               cycle
            endif
         else
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '''//trim(pCrs%csid)//''' on branch '''//trim(branchid)// &
                  '''. No definitionId was given.')
            cycle
         end if

         pCrs%bedLevel = 0.0d0
         call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'shift', pCrs%shift, success)
         if (.not. success) pCrs%shift = 0.0d0

         ! Stop in case of errors
         maxErrorLevel = getMaxErrorLevel()
        call finalizeCrs(network,pCrs,iref,inext)
         
      end do

      call tree_destroy(md_ptr)
      
   end subroutine readCrossSectionLocationFile
   
    
   subroutine finalizeCrs(network,pCrs,iref,inext)
   
      use m_CrossSections
      use m_network
      use m_hash_search
      type(t_network), intent(inout) :: network                 !< Network structure
      type(t_CrossSection), pointer, intent(inout)  :: pCrs
      integer, intent(in)            :: iref
      integer, intent(in)            :: inext
      
         pCrs%itabDef             = iref
         pCrs%tabDef              => network%CSDefinitions%CS(iref)
         
         pCrs%shift = pCrs%shift + pCrs%tabDef%bedLevel
         call SetParsCross(network%CSDefinitions%CS(iref), network%crs%cross(inext))
         pCrs => network%crs%cross(inext)
         
         allocate(pCrs%frictionTypePos(pCrs%tabDef%frictionSectionsCount))        !< Friction type for positive flow direction
         allocate(pCrs%frictionTypeNeg(pCrs%tabDef%frictionSectionsCount))        !< Friction type for negative flow direction
         call realloc(pCrs%frictionValuePos, pCrs%tabDef%frictionSectionsCount, fill=-999d0) !< Friction value for positive flow direction
         call realloc(pCrs%frictionValueNeg, pCrs%tabDef%frictionSectionsCount, fill=-999d0) !< Friction value for negative flow direction
         ! Allocate and Copy Section Data form Definition
         if (.not. pCrs%tabDef%frictionSectionID(pCrs%tabDef%frictionSectionsCount) == '') then
            allocate(pCrs%frictionSectionID(pCrs%tabDef%frictionSectionsCount))      !< Friction Section Identification
            allocate(pCrs%frictionSectionFrom(pCrs%tabDef%frictionSectionsCount))    !<
            allocate(pCrs%frictionSectionTo(pCrs%tabDef%frictionSectionsCount))      !<
            call realloc(pCrs%tabdef%frictionSectionFrom, pCrs%tabDef%frictionSectionsCount)
            call realloc(pCrs%tabdef%frictionSectionto, pCrs%tabDef%frictionSectionsCount)

            pCrs%frictionSectionsCount = pCrs%tabDef%frictionSectionsCount
            pCrs%frictionSectionID     = pCrs%tabDef%frictionSectionID
            pCrs%frictionSectionFrom   = pCrs%tabDef%frictionSectionFrom
            pCrs%frictionSectionTo     = pCrs%tabDef%frictionSectionTo
            
                     ! Retrieve Roughness for Profile from Spatial Data
         call GetRoughnessForProfile(network, network%crs%cross(inext))
         else 
           pCrs%frictiontypepos(1) = network%csdefinitions%cs(iref)%frictiontype(1)
           pCrs%frictiontypeneg(1) = network%csdefinitions%cs(iref)%frictiontype(1)
           pCrs%frictionvaluepos(1) = network%csdefinitions%cs(iref)%frictionvalue(1)
           pCrs%frictionvalueneg(1) = network%csdefinitions%cs(iref)%frictionvalue(1) 
         endif
         
         network%crs%count = inext
         
         !check of fricion section count > 0, zo ja getrougness, zo nee vul frictionype frictionvalue uit def

         if (network%CSDefinitions%CS(iref)%crossType == cs_YZ_Prof) then
            ! Prematurely to facilitate Conveyance Data to Delta Shell

            pCrs%convtab1 => null()
            pCrs%hasTimeDependentConveyance = .false. ! until proven otherwise
            call CalcConveyance(network%crs%cross(inext))
         endif
         
   end subroutine finalizeCrs
   
   !> Read the cross section definitions file, taking the file version into account.
   subroutine readCrossSectionDefinitions(network, CrossSectionDefinitionFile)

      type(t_network), target, intent(inout) :: network                          !< network structure
      character(len=*), intent(in)           :: CrossSectionDefinitionFile       !< name of the cross section definition file
      type(tree_data), pointer  :: md_ptr
      integer :: istat
      logical :: success
      character(len=Idlen)          :: fileVersion
      integer                       :: major 
      integer                       :: minor
      
      if (len_trim(CrossSectionDefinitionFile) == 0) then
         return
      endif
      
      call tree_create(trim(CrossSectionDefinitionFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(CrossSectionDefinitionFile),md_ptr,istat)
      msgbuf = 'Reading '//trim(CrossSectionDefinitionFile)//'.'
      call msg_flush()

      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success)
      if (.not. success) then
         major = 1
         minor = 0
         return
      endif
      
      select case (major)
      case (CrsDefFileMajorVersion)
         call parseCrossSectionDefinitionFile(md_ptr, network)
      case default
         call SetMessage(LEVEL_FATAL,'Unsupported fileVersion for cross section definition file:'//trim(fileVersion))
      end select

      call tree_destroy(md_ptr)
      call fill_hashtable(network%CSDefinitions)
      
   end subroutine readCrossSectionDefinitions
      
      
   !> Parse cross section definition file of the current version.
   !! file must already have been read into an ini tree.
   subroutine parseCrossSectionDefinitionFile(md_ptr, network)
      use m_hash_search
      use string_module, only: strcmpi
      use m_read_roughness, only: frictionTypeStringToInteger
      !use m_globalparameters, only: summerDikeTransitionHeight
   
      type(t_network), target,  intent(inout)   :: network        !< network structure
      type(tree_data), pointer, intent(in   )   :: md_ptr         !< treedata pointer to cross section definitions, already created.
   
      !integer :: istat
      integer :: numstr
      integer :: i, j
      integer :: crosstype
      logical :: success
      character(len=IdLen) :: id
      character(len=IdLen) :: typestr
      character(len=10) :: msgstr = ''
      double precision :: diameter
      integer :: numLevels
      double precision, allocatable :: level(:)
      double precision, allocatable :: width(:)
      double precision              :: plains(3)
      double precision              :: crestLevel
      double precision              :: baseLevel
      double precision              :: flowArea
      double precision              :: totalArea
      logical                       :: closed
      
      
      logical                       :: groundlayerUsed = .false.
      double precision              :: groundlayer
      double precision              :: height
      integer                       :: inext
      logical                       :: multiple_friction_inputs !< frictionId or frictionIds
      character(len=1)              :: plural_string
      type(t_CSType), pointer       :: pCS
      character(len=IdLen), allocatable :: fricTypes(:)
      integer                       :: maxnumsections ! Max number of friction sections, to realloc some arrays
      integer                       :: jaFricId
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      maxnumsections = 3

  crs:do i = 1, numstr
         
         ! block [Global]
         if (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'Global')) then
             call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'leveeTransitionHeight',summerDikeTransitionHeight, success)
             
         ! block [Definition]   
         elseif (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'Definition')) then
         
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'id', id, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection #', i, '. No id was given.'
            call err_flush()
            cycle
         endif
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'type', typestr, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '//trim(id)// &
               '. No type was given.')
            cycle
         endif
         crossType = getCrosstype(typestr)

         inext = network%CSDefinitions%count + 1
         if (network%CSDefinitions%count + 1 > network%CSDefinitions%size) then
            call realloc(network%CSDefinitions)
         endif
         
         multiple_friction_inputs = .false.

         pCS => network%CSDefinitions%CS(inext)
         pCS%id = id
         pCS%crossType = crosstype
         pCS%bedLevel = 0d0
         
         select case (crossType)
         case(CS_TABULATED)
            
            if (strcmpi(typestr, 'zwRiver')) then
               multiple_friction_inputs = .true.
            endif
            success = readTabulatedCS(pCS, md_ptr%child_nodes(i)%node_ptr) 
            
         case(CS_RECTANGLE)
            
            numlevels = 1
            allocate(level(numlevels + 2))
            allocate(width(numlevels + 2))
            level(numlevels) = 0.0d0
            call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '', 'width', width, numlevels, success)
            if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. No width was given.')
            endif
            if (width(1) == 0d0) then
               ! THe width of a rectangular cross section must be > 0
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. width = 0.00 was found in the input.')
            endif
            call prop_get_logical(md_ptr%child_nodes(i)%node_ptr, '', 'closed', closed, success)
            if (.not. success) closed = .true. ! Default
            
            if (closed) then
               call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'height', height, success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                  '. No height was given.')
                  cycle
               elseif (height == 0d0) then
                  ! THe height of a rectangular cross section must be > 0
                  call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                  '. height = 0.00 was found in the input.')
               endif
               numlevels = numlevels + 1
               level(numLevels) = height
               width(numLevels) = width(1)
               
            endif

            plains     = 0.0d0
            crestLevel = 0.0d0
            baseLevel  = 0.0d0
            flowArea   = 0.0d0
            totalArea  = 0.0d0

            pCs%frictionSectionsCount = 1
                
            inext = AddCrossSectionDefinition(network%CSDefinitions, id, numLevels, level, width,               &
                                              width, plains, crestLevel, baseLevel, flowArea, totalArea,        &
                                              closed, groundlayerUsed, groundlayer)
           
            deallocate(level, width)            
         
         case(CS_CIRCLE, CS_EGG)
            success = .true.
            ! use analytical description of circle and egg profile
            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'diameter', diameter, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. No diameter was given.')
            elseif (diameter == 0d0) then
               ! The diameter of a circular cross sections must be > 0
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. diameter = 0.00 was found in the input.')
            endif
   
            pCs%frictionSectionsCount = 1
            inext = AddCrossSectionDefinition(network%CSDefinitions, id, diameter, crossType, groundlayerUsed, groundlayer)
            
         case(CS_YZ_PROF)
            multiple_friction_inputs = .true.
            success = readYZCS(pCS, md_ptr%child_nodes(i)%node_ptr, network%sferic) 
            
         case default
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection type for CrossSection Definition with given type '//trim(typestr)//' and id: '//trim(id))
            success = .false.
            
         end select
            
         allocate(pCs%frictionSectionID  (pCs%frictionSectionsCount))      !< Friction Section Identification
         allocate(pCS%frictionSectionIndex(pCs%frictionSectionsCount))
         allocate(pCS%frictionType       (pCs%frictionSectionsCount))
         maxnumsections = max(maxnumsections, pCs%frictionSectionsCount)
         call realloc(fricTypes, maxnumsections, keepExisting = .false.)
         allocate(pCS%frictionValue      (pCs%frictionSectionsCount))

         if (multiple_friction_inputs) then
            plural_string = 's'
         else
            plural_string = ''
         end if
         call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionId'//trim(plural_string), pCs%frictionSectionsCount, pCS%frictionSectionID, success)
         call check_prop_get_wrong_singular_or_plural_keyword(md_ptr%child_nodes(i)%node_ptr, '', multiple_friction_inputs, 'frictionIds', 'frictionId', success, trim(id))


         if (.not. success) then
            call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionType'//trim(plural_string), pCs%frictionSectionsCount, fricTypes, success)
            call check_prop_get_wrong_singular_or_plural_keyword(md_ptr%child_nodes(i)%node_ptr, '', multiple_friction_inputs, 'frictionTypes', 'frictionType', success, trim(id))

            if (success) then
               do j = 1, pCs%frictionSectionsCount
                  call frictionTypeStringToInteger(fricTypes(j), pCS%frictionType(j))
                  if (pCS%frictionType(j) < 0) then
                     write(msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                                               '. frictionType '''//trim(fricTypes(j))//''' is wrong in section #', j, '.'
                     call err_flush()
                     cycle crs ! Skip this entire cross section
                  endif
               end do
               
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'frictionValue'//trim(plural_string), pCS%frictionValue, pCs%frictionSectionsCount, success)
               call check_prop_get_wrong_singular_or_plural_keyword(md_ptr%child_nodes(i)%node_ptr, '', multiple_friction_inputs, 'frictionValues', 'frictionValue', success, trim(id))
            endif
               
            if (.not. success) then
               pCs%frictionSectionID(1) = 'Main'
               if (pCs%frictionSectionsCount >=2) then
                  pCs%frictionSectionID(2) = 'FloodPlain1'
               endif
               if (pCs%frictionSectionsCount ==3) then
                  pCs%frictionSectionID(3) = 'FloodPlain2'
               endif
            else
               do j = 1, pCs%frictionSectionsCount
                  pCS%frictionSectionID(j) = ''
               enddo
            endif
         else
            jaFricId = 0
            do j = 1, pCs%frictionSectionsCount
               if (len_trim(pCS%frictionSectionID(j)) > 1) then
                  jaFricId = 1
                  exit
               end if
            end do
            if (jaFricId == 0) then
               write(msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                                               '. frictionId (or frictionIds)is not specified in section #', j, '.'
               call err_flush()
            end if
         endif
         success = .true.
         
         if (success) then
            network%CSDefinitions%count = inext
         endif
         
         do j = 1, pCs%frictionSectionsCount
            pCs%frictionSectionIndex(j) = hashsearch(network%rgs%hashlist, pCS%frictionSectionID(j))
         enddo
         
         endif !block test
         
      enddo crs

      if (anySummerDike) then 
          if (summerDikeTransitionHeight == 0.5) then 
              msgstr = '(default)'
          endif
          write(msgbuf,'(a,F6.3,a,a)') 'Levee transition height (summerdike) = ', summerDikeTransitionHeight, ' m ', msgstr 
          call msg_flush()
      endif 

   end subroutine parseCrossSectionDefinitionFile

   !> Checks whether a wrong singular or plural keyword was given after an earlier prop_get() call,
   !! which should have been done already at the call site.
   !! If user has supplied wrong keyword, a warning message is printed, and input value will not be read.
   subroutine check_prop_get_wrong_singular_or_plural_keyword(prop_ptr, chapname, plural, key_plural, key_singular, success_get, id)
      type(TREE_DATA),  pointer       :: prop_ptr     !< Pointer to a property tree in which the prop_get will be done.
      character(len=*), intent(in   ) :: chapname     !< The name of the chapter under which the input key is searched (may be empty '').
      logical,          intent(in   ) :: plural       !< Whether or not the plural keyword is the correct one.
      character(len=*), intent(in   ) :: key_plural   !< The plural version of the keyword.
      character(len=*), intent(in   ) :: key_singular !< The singular version of the keyword.
      logical,          intent(in   ) :: success_get  !< Result status of an earlier prop_get call of the expected keyword.
      character(len=*), intent(in   ) :: id           !< The object id, can be used in the warning message
      character(len=IdLen) :: tmpstr

      character(len=max(len(key_plural), len(key_singular))) :: key_expected, key_alt
      logical :: success_alt

      if (success_get) then
         ! User already gave the correct keyword
         return
      else
         if (plural) then
            key_expected = key_plural
            key_alt      = key_singular
         else
            key_expected = key_singular
            key_alt      = key_plural
         end if

         ! If expected keyword was *not* given, double check whether the alternative keyword *was* given.
         ! If so: print a warning, so user can correct their input.
         call prop_get_string(prop_ptr, chapname, key_alt, tmpstr, success_alt)
         if (success_alt) then
            write (msgbuf,'(a,a,a,a,a,a,a)') 'Incorrect keyword ''', trim(key_alt), ''' found for id: ', &
               trim(id), '. Did you mean ''', trim(key_expected), '''?'
            call warn_flush()
         end if
      end if
   end subroutine check_prop_get_wrong_singular_or_plural_keyword

   !> read tabulated cross section definition from treedata input
   logical function readTabulatedCS(pCS, node_ptr)  
   
      use precision_basics
      use m_array_predicates, only: is_monotonically_increasing
      
      type(t_CSType), pointer, intent(inout) :: pCS           !< cross section definition
      type(tree_data), pointer, intent(in)   :: node_ptr      !< treedata node pointer to current cross section definition
      
      integer          :: numlevels, level_index_intersect
      logical          :: success
      double precision :: crestLevel
      double precision :: baseLevel
      double precision :: maxFlowWidth
      double precision :: Main
      double precision :: FP1
      double precision :: FP2
      double precision :: flowArea
      double precision :: totalArea
      double precision :: wintersect
      double precision :: factor
      double precision, dimension(:), allocatable :: height
      double precision, dimension(:), allocatable :: width
      double precision, dimension(:), allocatable :: totalwidth
      integer :: i, j, k
      double precision, parameter   :: eps = 1d-5
      character(len=IdLen) :: typestr

      numlevels = 0
      readTabulatedCS= .false.
      call prop_get_integer(node_ptr, '', 'numLevels', numlevels, success)
      if (numlevels == 0) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with id: '//trim(pCS%id)//'. NumLevels should be > 0.')
            return
      end if

      ! reserve space for extra support points at main - floodplain1 - floodplain2 transitions
      allocate(height(numlevels+2))
      allocate(width(numlevels+2))
      allocate(TotalWidth(numlevels+2))
   !
      pCS%levelsCount = numlevels
      
      if (success) then
         call prop_get_doubles(node_ptr, '', 'levels', height, numlevels, success)
      endif
      if (success) then
         call prop_get_doubles(node_ptr, '', 'flowWidths', width, numlevels, success)
      endif
      if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition id: '//trim(pCS%id)//'. Invalid levels/widths.')
            return
      endif
      call prop_get_double(node_ptr, '', 'totalWidths', totalWidth(1), success)
      if (success) then
         call prop_get_doubles(node_ptr, '', 'totalWidths', totalWidth, numlevels, success)
      else
         totalWidth = width
      endif

      call prop_get_string(node_ptr, '', 'type', typestr, success)
      if (.not. is_monotonically_increasing(height, numlevels)) then
         call SetMessage(LEVEL_WARN, 'Incorrect input for tabulated Cross-Section Definition id: '//trim(pCS%id)//'. Levels should be monotonically increasing.')
      endif
      if (strcmpi(typestr, 'zwRiver')) then ! only for zwRiver does width need to be monotonically increasing
         if (.not. is_monotonically_increasing(width, numlevels)) then
            call SetMessage(LEVEL_WARN, 'Incorrect input for tabulated Cross-Section Definition id: '//trim(pCS%id)//'. flowWidths should be monotonically increasing.')
         endif
         if (.not. is_monotonically_increasing(totalWidth, numlevels)) then
            call SetMessage(LEVEL_WARN, 'Incorrect input for tabulated Cross-Section Definition id: '//trim(pCS%id)//'. totalWidths should be monotonically increasing.')
         endif
      endif
   
      ! summerdike
      
      call prop_get_double(node_ptr, '', 'leveeCrestLevel', crestLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'leveeBaseLevel', baseLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'leveeFlowArea',  flowArea,  success)
      if (success) call prop_get_double(node_ptr, '', 'leveeTotalArea', totalArea, success)
      if (success .and. flowArea > totalArea) then
            call SetMessage(LEVEL_WARN, 'Total area behind levee should be larger then flow area behind levee. Cross-Section Definition id: '//trim(pCS%id)//'.')
      endif      
      if (success) then
         if (flowArea > 1.0d-5 .or. totalArea > 1.0d-5) then
            allocate(pCS%summerdike)
            anySummerDike = .true.
            pCS%summerdike%crestLevel = crestLevel
            pCS%summerdike%baseLevel  = baseLevel
            pCS%summerdike%flowArea   = flowArea
            pCS%summerdike%totalArea  = totalArea
         endif
      endif
      
      ! Initialize groundlayer information of the newly added cross-section
      allocate(pCS%groundlayer)
      
      pCS%closed = .false.
            
      pCS%plains = 0.0d0
      
      maxFlowWidth = width(1)
      do i = 2, numlevels
         maxFlowWidth = max( maxFlowWidth, width(i))
      enddo
      
      call prop_get_double(node_ptr, '', 'mainWidth', Main, success)
      if (.not. success)  Main = maxFlowWidth
      call prop_get_double(node_ptr, '', 'fp1Width', FP1, success)
      if (.not. success)  FP1 = 0.0d0
      call prop_get_double(node_ptr, '', 'fp2Width', FP2, success)
      if (.not. success)  FP2 = 0.0d0

      ! Check and Make Consistent if Needed
      if ((Main + FP1 + FP2) < (maxFlowWidth) - 0.001d0) then
            call SetMessage(LEVEL_ERROR, 'Sum of all Sections less than Flow Width for Cross-Section Definition ID: '//trim(pCS%id))
      endif
         
      if (FP1 <= 0.0d0 .and. FP2 > 0) then
            call SetMessage(LEVEL_ERROR, 'Floodplain2 only allowed when Floodplain1 exists for Cross-Section Definition ID: '//trim(pCS%id))
      endif
         
      ! Compensate for round off if needed
      if ((Main + FP1 + FP2) < maxFlowWidth) then
         Main = Main + 0.001d0
      endif 
            
      pCS%plains(1) = Main
      pCS%plains(2) = FP1
      pCS%plains(3) = FP2
                  
      if ( (pCS%plains(2) == 0.0d0) .and. (pCS%plains(3) == 0.0d0) ) then
         pCs%plainsLocation(1) = numlevels
         pCs%plainsLocation(2) = 0
         pCs%plainsLocation(3) = 0
      else
       
         ! make sure transitions main - floodplain1 and floodplain1 - floodplain2 are always in table
        
         wintersect = 0d0
         do i = 1, 2
            wintersect = wintersect + pCs%plains(i)
            level_index_intersect = 0
            do j = 1, numlevels
               if (wintersect+1d-5 < width(j)) then
                  ! found an intersection
                  level_index_intersect = j
                  exit
               endif
            enddo
            if ( level_index_intersect /= 0) then
               if (j == 1) then
                  pCs%plains(1) = width(1)
               elseif ( abs(wintersect - width(level_index_intersect-1) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect-1
               elseif ( abs(wintersect - width(level_index_intersect) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect
               else
                  ! extra level needed.
                  factor = (wintersect - width(level_index_intersect-1))/(width(level_index_intersect) - width(level_index_intersect-1))
                  do k = numlevels+1, level_index_intersect, -1
                     width(k) = width(k-1)
                     height(k) = height(k-1)
                     totalwidth(k) = totalwidth(k-1)
                  enddo
                  width(level_index_intersect)      = factor * width(level_index_intersect+1)      + (1d0-factor) * width(level_index_intersect)
                  height(level_index_intersect)     = factor * height(level_index_intersect+1)     + (1d0-factor) * height(level_index_intersect)
                  totalwidth(level_index_intersect) = factor * totalwidth(level_index_intersect+1) + (1d0-factor) * totalwidth(level_index_intersect)
                  pCs%plainsLocation(i) = level_index_intersect
                  numlevels = numlevels+1
               endif
            elseif (comparereal(wintersect, width(numlevels), eps) == 0) then
                pCs%plainsLocation(i) = numlevels
            endif
         
         enddo
         pCs%plainsLocation(3) = numlevels
      endif
      
      call realloc(pCS%height, numlevels)
      call realloc(pCS%flowWidth, numlevels)
      call realloc(pCS%totalWidth, numlevels)

      call realloc(pCS%af_sub, 3, numlevels)
      call realloc(pCS%width_sub, 3, numlevels)
      call realloc(pCS%perim_sub, 3, numlevels)
      call realloc(pCS%flowArea, numlevels)
      call realloc(pCS%wetPerimeter, numlevels)
      call realloc(pCS%totalArea, numlevels)
      call realloc(pCS%area_min, numlevels)
      call realloc(pCS%width_min, numlevels)
      
      pCs%levelsCount = numlevels
      pCS%height      = height(1:numlevels)
      pCS%flowWidth   = width(1:numlevels)

      ! Add Preisman slot to totalwidth for closed cross sections.
      ! The flow width remains unchanged.
      if (totalwidth(numlevels) <= sl) then
         pcs%closed = .true.
         do i = 2, numlevels
            totalwidth(i) = max(totalwidth(i), sl)
         enddo
      endif
      
      pCS%totalWidth  = totalwidth(1:numlevels)
      
      if (pCs%plains(3) > 0.0d0) then
         pCs%frictionSectionsCount = 3
      elseif (pCs%plains(2) > 0.0d0) then
         pCs%frictionSectionsCount = 2
      else
         pCs%frictionSectionsCount = 1
      endif
            
      ! Create Interpolation Tables
      call createTablesForTabulatedProfile(pCs)
      
      deallocate(height)
      deallocate(width)
      deallocate(TotalWidth)

      readTabulatedCS =  .true.
      
   end function readTabulatedCS

   !> Snap the edges of the position range to the boundaries of the y_coordinates,
   !> since this is assumed later on
   subroutine snap_friction_position_edges(positions, y_coordinates)
      double precision, intent(inout) :: positions(:)       !< friction y-positions, size > 0, monotonically increasing
      double precision, intent(in   ) :: y_coordinates(:)   !< y-coordinates of cross-section specification, size > 0, monotonically increasing

      integer                         :: i
      integer                         :: friction_positions_size
      integer                         :: num_levels

      friction_positions_size = size(positions)
      num_levels = size(y_coordinates)

      ! Set the upper boundary, and ensure that positions is still monotonically increasing
      positions(friction_positions_size) = y_coordinates(num_levels)
      i = friction_positions_size - 1
      do while (i >= 1 .and. positions(i) > positions(i + 1))
         positions(i) = positions(i + 1)
         i = i - 1
      end do

      ! Set the lower boundary, and ensure that positions is still monotonically increasing
      positions(1) = y_coordinates(1)
      i = 2
      do while (i <= friction_positions_size .and. positions(i) < positions(i - 1))
         positions(i) = positions(i - 1)
         i = i + 1
      end do
   end subroutine snap_friction_position_edges

   !> read YZ cross section from ini file
   logical function readYZCS(pCS, node_ptr, sferic) 
      use precision
      use physicalconsts, only: earth_radius
      use m_array_predicates, only: is_monotonically_increasing
      
      type(t_CSType), pointer,  intent(inout) :: pCS             !< cross section item
      type(tree_data), pointer, intent(in)    :: node_ptr        !< treedata pointer to input for cross section
      logical                 , intent(in)    :: sferic          !< indicates whether spherical coordinates are used or metric
      integer :: numlevels
      integer :: frictionCount
      logical :: success, sferic_local
      double precision, allocatable, dimension(:) :: positions
      double precision, allocatable, dimension(:) :: xcoordinates, ycoordinates, ycoordinates_help, zcoordinates
      integer,          allocatable, dimension(:) :: segmentToSectionIndex
      integer          :: i
      double precision :: locShift
      logical          :: xyz_cross_section 
      character(len=idlen) :: conv_text

      
      readYZCS = .false.
      sferic_local = .false.
      call prop_get_integer(node_ptr, '', 'yzCount', numlevels, success)
      if (.not. success) then
         xyz_cross_section = .true.
         call prop_get_integer(node_ptr, '', 'xyzCount', numlevels, success)
         ! only for xyz cross sections the coordinates may be spherical 
         sferic_local = sferic
      else
         xyz_cross_section = .false.
      endif
      
      if (.not. success .or. numLevels <= 0 ) then
            call SetMessage(LEVEL_ERROR, 'Error while reading number of levels/sections for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            return
      endif

      frictionCount = 1
      call prop_get_integer(node_ptr, '', 'sectionCount', frictionCount, success)
      if (frictioncount <=0 ) then
         call SetMessage(LEVEL_ERROR, 'Error while reading number of frictionlevels Definition ID: '//trim(pCS%id)//'the frictionCount must be larger than 0.' )
      endif  

      pCS%conveyanceType = CS_VERT_SEGM
      conv_text = 'segmented'
      call prop_get(node_ptr, '', 'conveyance', conv_text)
      if (trim(conv_text) =='segmented') then
         pCS%conveyanceType = CS_VERT_SEGM
      elseif(trim(conv_text) =='lumped') then
         pCS%conveyanceType = CS_LUMPED
      else
         msgbuf = 'Incorrect conveyance type for cross section definition '//trim(pCS%id)//': '//trim(conv_text)
      endif
         
      if (frictionCount > 1 .and. pCS%conveyanceType == CS_LUMPED) then
         msgbuf = 'In cross section definition '//trim(pCS%id)//' lumped conveyance in combination with multiple friction sections is used, this is not allowed'
         call err_flush()
      endif
      
      pCS%frictionSectionsCount = frictionCount
      pCS%storLevelsCount       = 0
      
      call realloc(xcoordinates, numlevels+frictionCount)
      call realloc(ycoordinates, numlevels+frictionCount)
      call realloc(ycoordinates_help, numlevels+frictionCount)
      call realloc(zcoordinates, numlevels+frictionCount)
      call realloc(segmentToSectionIndex, numlevels+frictionCount)
      call realloc(pCS%storLevels, 2)
      call realloc(pCS%YZstorage, 2)
      call realloc(pCS%frictionSectionFrom, frictionCount)
      call realloc(pCS%frictionSectionTo, frictionCount)
      allocate(positions(frictionCount+1))
      
      xcoordinates = 0d0
      call prop_get_doubles(node_ptr, '', 'xCoordinates', xcoordinates, numlevels, success)
      call prop_get_doubles(node_ptr, '', 'yCoordinates', ycoordinates, numlevels, success)
      if (success) call prop_get_doubles(node_ptr, '', 'zCoordinates', zcoordinates, numlevels, success)
      if (.not. success) then
          call SetMessage(LEVEL_ERROR, 'Error while reading number of yz-levels for YZ-Cross-Section Definition ID: '//trim(pCS%id))
      endif
      
      ycoordinates_help = ycoordinates
      if (xyz_cross_section) then
         ycoordinates(1) = 0d0
         do i = 2, numlevels
            call distance(sferic_local, xcoordinates(i-1), ycoordinates_help(i-1), xcoordinates(i), ycoordinates_help(i), ycoordinates(i), earth_radius)
            ycoordinates(i) = ycoordinates(i-1) + ycoordinates(i) 
         enddo
      endif
      
      pCS%storLevels = 0
      
      call prop_get_doubles(node_ptr, '', 'frictionPositions', positions, frictionCount+1, success)
      
      if (success) then
         if (.not. is_monotonically_increasing(positions, frictionCount + 1)) then
            write (msgbuf, '(a)') 'frictionPositions for (X)YZ-Cross-Section Definition ID: '//trim(pCS%id)// &
               ' do not increase monotonically.'
            call err_flush()
         endif
         ! Check Consistency of Rougness Positions
         if (comparereal(positions(1), ycoordinates(1), position_tolerance) /= 0 .or. comparereal(positions(frictionCount + 1), ycoordinates(numLevels), position_tolerance) /= 0) then
            if (positions(1) == 0.0d0  .and. comparereal(positions(frictionCount+1), ycoordinates(numLevels) - ycoordinates(1), 1d-6) == 0) then
               ! Probably lined out wrong because of import from SOBEK2
               locShift = positions(frictionCount + 1) - ycoordinates(numLevels)
               !do i = 1, frictionCount + 1
                  i = frictionCount + 1
                  positions(i) = positions(i) - locShift
               !enddo
               call SetMessage(LEVEL_WARN, 'Friction sections corrected for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            else
               write (msgbuf, '(a,f16.10,a,f16.10,a,f16.10,a,f16.10,a)') 'Section data not consistent for (X)YZ-Cross-Section Definition ID: '//trim(pCS%id)// &
                  ', friction section y-range (', positions(1), ', ', positions(frictionCount+1), &
                  ') differs from cross section y-range (', ycoordinates(1), ', ', ycoordinates(numLevels), ').'
               call err_flush()
            endif
         endif
         call snap_friction_position_edges(positions, ycoordinates(1:numlevels))

      elseif (.not.success .and. frictionCount==1) then
         positions(1) = ycoordinates(1)
         positions(2) = ycoordinates(numLevels)
         success = .true.
      endif

      pCS%frictionSectionFrom = positions(1:frictionCount)
      pCS%frictionSectionTo = positions(2:frictionCount+1)

      allocate(pCS%groundlayer)
      pCS%groundlayer%used      = .false.
      pCS%groundlayer%thickness = 0.0d0

      ! Actions: 
      ! * remove double points
      ! * prevent horizontal segments
      ! * add extra points (if necessary) at frictionsection transitions
      ! * generate segmentToSectionIndex
      call regulate_yz_coordinates(ycoordinates, zcoordinates, pcs%bedlevel, segmentToSectionIndex, numlevels, pCS%frictionSectionFrom, &
                                   frictionCount)

      call realloc(pCS%y, numlevels)
      call realloc(pCS%z, numlevels)
      call realloc(pCS%segmentToSectionIndex, numlevels)

      pCS%levelsCount           = numLevels
      pCS%y(1:numlevels) = ycoordinates(1:numlevels)
      pCS%z(1:numlevels) = zcoordinates(1:numlevels)
      pCS%segmentToSectionIndex(1:numlevels) = segmentToSectionIndex(1:numlevels)
      
      deallocate(positions)
      readYZCS = success
   end function readYZCS
   
end module m_readCrossSections
