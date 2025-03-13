!> Define cross-sections
module m_CrossSections
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
   use m_alloc
   use M_newcross
   use m_branch
   use m_GlobalParameters
   use m_hash_search
   use string_module

   implicit none

   private

   public realloc
   public dealloc
   public AddCrossSection
   public AddCrossSectionDefinition
   public CalcConveyance
   public createTablesForTabulatedProfile
   public fill_hashtable
   public getBob
   public GetCriticalDepth
   public GetCrossType
   public GetCSParsFlow
   public GetCSParsTotal
   public getGroundLayer
   public SetParsCross
   public useBranchOrders
   public write_crosssection_data
   public getYZConveyance
   public getHighest1dLevel
   public getSummerDikeData
   double precision, public :: default_width

   interface fill_hashtable
      module procedure fill_hashtable_csdef
   end interface
   
   interface getHighest1dLevel
      module procedure getHighest1dLevelInterpolate
      module procedure getHighest1dLevelSingle
   end interface getHighest1dLevel
   
   interface getBob
      module procedure getBobInterpolate
      module procedure getBobSingle
   end interface getBob
   
   interface GetCSParsFlow
      module procedure GetCSParsFlowInterpolate
      module procedure GetCSParsFlowCross
   end interface GetCSParsFlow

   interface GetCSParsTotal
      module procedure GetCSParsTotalInterpolate
      module procedure GetCSParsTotalCross
   end interface GetCSParsTotal
   
   !> Add cross section definition for complete tabulated or YZ-cross-sections.For YZ-profiles also just a location definition is possible
   interface AddCrossSectionDefinition
      module procedure AddRoundCrossSectionDefinition
      module procedure AddTabCrossSectionDefinition
      module procedure AddYZCrossSectionDefinition
   end interface

   interface AddCrossSection
      module procedure AddCrossSectionByVariables
      module procedure AddCrossSectionByCross
   end interface AddCrossSection

   interface useBranchOrders
      module procedure useBranchOrdersCrs
   end interface useBranchOrders

   !> Realloc memory cross-section definition or cross-sections
   interface realloc
      module procedure reallocCSDefinitions
      module procedure reallocCrossSections
   end interface

   !> Free the memory of cross-section definition or cross-sections
   interface dealloc
      module procedure deallocCrossDefinition
      module procedure deallocCSDefinitions
      module procedure deallocCrossSection
      module procedure deallocCrossSections
   end interface dealloc

   integer, public, parameter :: CS_TABULATED      =    1 !< Tabulated type cross section definition
   integer, public, parameter :: CS_CIRCLE         =    2 !< Circle type cross section definition
   integer, public, parameter :: CS_EGG            =    3 !< Egg type cross section definition
   integer, public, parameter :: CS_RECTANGLE      =    4 !< Rectangular type cross section definition
   integer, public, parameter :: CS_TRAPEZIUM      =    5 !< Trapezium type cross section definition
   integer, public, parameter :: CS_YZ_PROF        =   10 !< YZ type cross section definition
   integer, public, parameter :: CS_TYPE_NORMAL    =    1 !< Ordinary flow area computation
   integer, public, parameter :: CS_TYPE_PREISMAN  =    2 !< Ordinary total area computation, with possible Preisman lock on top
   integer, public, parameter :: CS_TYPE_PLUS      =    3 !< Total area for only the expanding part of the cross section (Nested Newton method)
   integer, public, parameter :: CS_TYPE_MIN       =    4 !< Total area for only the narrowing part of the cross section (Nested Newton method)
   
   double precision, parameter:: eps = 1d-3               !< accuracy parameter for determining wetperimeter == 0d0

   double precision           :: thresholdForSummerdike = 0.4d0
   character (len=13), public, dimension(10) :: CSTypeName = (/&
   '1:TABULATED ',&
   '2:CIRCLE    ',&
   '3:EGG       ',&
   '            ',&
   '            ',&
   '            ',&
   '            ',&
   '            ',&
   '            ',&
   '10:YZ       '/)

   type  :: t_groundlayer                               !< Derived type containing groundlayer data
      logical          :: used      = .false.           !< Flag
      double precision :: thickness = 0.0d0             !< Thickness of the ground layer
      double precision :: area      = 0.0d0             !< area occupied by the ground layer
      double precision :: perimeter = 0.0d0             !< wet perimeter occupied by the groundlayer
      double precision :: width     = 0.0d0             !< width at the top of the groundlayer
   end type t_groundlayer

   type, public  :: t_summerdike                 !< Derived type containing summerdike data
      double precision :: crestLevel = 0.0d0     !< Crest height of the summerdike
      double precision :: baseLevel  = 0.0d0     !< Base level of the summerdike
      double precision :: flowArea   = 0.0d0     !< Flow area of the summerdike
      double precision :: totalArea  = 0.0d0     !< Total area, i.e. Flow area + storage area
   end type t_summerdike

   !> Derived type for defining one cross-section type
   type, public :: t_CSType
       character(IdLen)             :: id                !< unique identification
       integer                      :: crossType         !< Type of cross section
       integer                      :: levelsCount = 0  !< number of levels in tabulated cross section

       !*** data for tabulated cross sections ***
       double precision, allocatable, dimension(:)   :: height       !< Specified height of the cross-section
       double precision, allocatable, dimension(:)   :: flowWidth    !< Specified flow width of the cross-section
       double precision, allocatable, dimension(:)   :: totalWidth   !< Specified total width of the cross-section
       logical                                       :: closed       !< Flag to determine if cross-section is closed

       ! Pre-Calculated Table for Tabulated/River Profiles
       double precision, allocatable, dimension(:,:) :: af_sub        !< Flow Areas for Sub-Sections (Main, FP1 and FP2)
       double precision, allocatable, dimension(:,:) :: width_sub     !< Width for Sub-Sections (Main, FP1 and FP2)
       double precision, allocatable, dimension(:,:) :: perim_sub     !< Wetted Perimeter for Sub-Sections (Main, FP1 and FP2)
       double precision, allocatable, dimension(:)   :: flowArea      !< Flow Areas
       double precision, allocatable, dimension(:)   :: wetPerimeter  !< Wet Perimeters
       double precision, allocatable, dimension(:)   :: totalArea     !< Total Areas
       double precision, allocatable, dimension(:)   :: area_min      !< Area for a narrowing part of a cross section (Nested Newton)
       double precision, allocatable, dimension(:)   :: width_min     !< Width for a narrowing part of a cross section (Nested Newton)
       
       !--- additional data for river profiles
       double precision                         :: plains(3) = 0.0d0  !< 1: main channel, 2: floopplain 1, 3: floodplain 2
       integer                                  :: plainsLocation(3)  !< locations (widths) of the main channel and floodplains
       !--- information for Summerdikes
       type(t_summerdike),pointer               :: summerdike => null()   !< pointer to the summerdike data

       !*** data for yz cross sections
       double precision, allocatable            :: y(:)                 !< tranversal co-ordinate
       double precision, allocatable            :: z(:)                 !< z-co-ordinate (shifted, with base level = 0)
       double precision                         :: bedLevel             !< lowest point in cross section
       integer, allocatable                     :: segmentToSectionIndex(:) !< Returns friction section index for segment i
       integer                                  :: conveyanceType       !< Conveyance type possible values CS_LUMPED or CS_VERT_SEGM
       integer                                  :: storLevelsCount = 0  !< Number of actual storage levels
       double precision, allocatable            :: storLevels(:)        !< Storage levels
       double precision, allocatable            :: YZstorage(:)         !< Storage levels
       integer                                  :: storageType     = 0  !< Storage type (default no storage level)

       !** Circle and egg profile data
       double precision                         :: diameter             !< diameter of a circle type or egg type profile

       !*** ground layer data
       type(t_groundlayer),pointer              :: groundlayer => null()      !< pointer to groundlayer data
       
       ! Friction Data 
       integer                                  :: frictionSectionsCount = 0  !< Number of actual friction sections
       character(IdLen), allocatable            :: frictionSectionID(:)       !< Friction Section Identification
       integer         , allocatable            :: frictionSectionIndex(:)    !< Friction Section index
       double precision, allocatable            :: frictionSectionFrom(:)     !< For YZ: Start point (y) of friction section.
       double precision, allocatable            :: frictionSectionTo(:)       !< For yz: End point (y) of friction section.
       integer, allocatable                     :: frictionType(:)            !< Friction type  (Only when FrictionSectionID not specified).
       double precision, allocatable            :: frictionValue(:)           !< Friction value  (Only when FrictionSectionID not specified).

   end type t_CSType

   !> Derived type to store the cross-section definition, grouped according to the types
   type, public :: t_CSDefinitionSet
      integer                                   :: size    = 0     !< Size of the cross-section set
      integer                                   :: growsBy = 2000  !< Increment for the cross-section type
      integer                                   :: count   = 0     !< Actual number of cross-sections
      type(t_CSType), pointer, dimension(:)     :: CS              !< Cross-section derived type
      type(t_hashlist)                          :: hashlist        !< hash table for searching cross section definition on id
   end type

   !---------------------------------------------------------
   !> Derived type to store the cross-section
   type, public :: t_CrossSection

       character (len=IdLen)        :: csid                 !< User-defined ID of this crossection
       integer                      :: crossIndx            !< Index to check on same cross-section
                                                            !! To Prevent unecessary interpolations.
       logical                      :: IsCopy = .false.     !< Flag to determine if Cross-Section is a copy
       
       integer                      :: crossType            !< Type of cross section

       integer                      :: branchid = -1        !< Integer branch id
       double precision             :: chainage             !< Offset in meters along reach
       double precision             :: bedLevel             !< Bed level of the cross section
       double precision             :: shift                !< Bed level = reference level cross section definition + SHIFT
       double precision             :: surfaceLevel         !< Highest level of cross section
       double precision             :: charHeight           !< Characteristic height
       double precision             :: charWidth            !< Characteristic width
       logical                      :: closed               !< Flag indicates whether the cross section is closed
       integer                      :: groundFrictionType   !< Type of ground friction
       double precision             :: groundFriction       !< Ground friction parameter
       integer                      :: iTabDef              !< Temporary item to save ireference number to cross section definition
                                                            !! Necessary for reallocation of arrays
       type(t_CSType), pointer      :: tabDef => null()
       logical                      :: hasTimeDependentConveyance !< Flag indicates whether the cross section has time dependent roughness
       type(t_convtab), pointer        :: convTab1 => null()   !< Conveyance table for YZ-cross sections
       type(t_convtab), pointer        :: convTab2 => null()   !< Conveyance table for YZ-cross sections at new time level, in case the friction
                                                            !< for this cross section is time dependent
       
       integer                                  :: frictionSectionsCount = 0 !< Number of actual friction sections
       character(IdLen), allocatable            :: frictionSectionID(:)      !< Friction Section Identification
       double precision, allocatable            :: frictionSectionFrom(:)    !< Start point of friction section
       double precision, allocatable            :: frictionSectionTo(:)      !< End point of friction section
       integer, allocatable                     :: frictionTypePos(:)        !< Friction type for positive flow direction
       double precision, allocatable            :: frictionValuePos(:)       !< Friction value for positive flow direction
       integer, allocatable                     :: frictionTypeNeg(:)        !< Friction type for negative flow direction
       double precision, allocatable            :: frictionValueNeg(:)       !< Friction value for negative flow direction

    contains
      procedure, pass :: hasSummerDike   => hasSummerDike                    !< Indicates if the cross  section has a summer dike
  


    end type

   !> Derived type to store the cross-section set
   type, public :: t_CrossSectionSet
      integer                                                :: maxNumberOfConnections=0  !< maximum nr of connections to a node
      integer                                                :: Size = 0                  !< Actual size of cross-section set
      integer                                                :: growsBy = 2000            !< Increment for cross-section set
      integer                                                :: Count= 0                  !< Actual number of cross-section sets
      type(t_CrossSection), pointer, dimension(:)            :: cross                     !< Current cross-section
      integer, pointer, dimension(:)                         :: CrossSectionIndex =>null()!< When sorting cross sections, the index of structure related cross sections must be reindexed.
   end type t_CrossSectionSet

contains

!> Indicates if the cross  section has a summer dike.
logical function hasSummerDike(this)
   class(t_CrossSection) :: this       !< Current cross section
   if (associated(this%tabDef) ) then
      hasSummerDike = associated(this%tabDef%summerdike)
   else 
      hasSummerDike = .false.
   endif
end function hasSummerDike



!> Free the memory used by a cross-section definition
subroutine deallocCrossDefinition(CrossDef)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CSType), intent(inout)                            :: CrossDef !< Current cross-section definition

   ! Local variables
   if (allocated(CrossDef%height))                   deallocate(CrossDef%height)
   if (allocated(CrossDef%flowWidth))                deallocate(CrossDef%flowWidth)
   if (allocated(CrossDef%totalWidth))               deallocate(CrossDef%totalWidth)

   if (allocated(CrossDef%af_sub))                   deallocate(CrossDef%af_sub)
   if (allocated(CrossDef%width_sub))                deallocate(CrossDef%width_sub)
   if (allocated(CrossDef%perim_sub))                deallocate(CrossDef%perim_sub)
   if (allocated(CrossDef%flowArea))                 deallocate(CrossDef%flowArea)
   if (allocated(CrossDef%wetPerimeter))             deallocate(CrossDef%wetPerimeter)
   if (allocated(CrossDef%totalArea))                deallocate(CrossDef%totalArea)
   if (allocated(CrossDef%area_min))                 deallocate(CrossDef%area_min)
   if (allocated(CrossDef%width_min))                deallocate(CrossDef%width_min)

   if (allocated(CrossDef%y))                        deallocate(CrossDef%y)
   if (allocated(CrossDef%z))                        deallocate(CrossDef%z)
   if (allocated(CrossDef%frictionSectionID))        deallocate(CrossDef%frictionSectionID)
   if (allocated(CrossDef%frictionSectionFrom))      deallocate(CrossDef%frictionSectionFrom)
   if (allocated(CrossDef%frictionSectionTo))        deallocate(CrossDef%frictionSectionTo)
   if (associated(CrossDef%summerdike))              deallocate(CrossDef%summerdike)
   if (associated(CrossDef%groundlayer))             deallocate(CrossDef%groundlayer)
   
   CrossDef%summerdike => null()
   CrossDef%groundlayer => null()
   
end subroutine deallocCrossDefinition

!> Free the memory used by a cross-section definition set
subroutine deallocCSDefinitions(CSdef)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CSDefinitionSet), intent(inout)   :: CSdef !< Cross-section definition set

   ! Local variables
   integer length, i

   ! Program code
   if (associated(CSdef%CS)) then
      length = size(CSdef%CS)
      do i = 1, length
         call dealloc(CSdef%CS(i))
      enddo
      deallocate(CSdef%CS)
      CSdef%CS =>null()
      CSDef%size            = 0
      CSDef%count           = 0
   endif
   call dealloc(CSDef%hashlist)
end subroutine deallocCSDefinitions

!> Increase the memory used by a cross-section definition
subroutine reallocCSDefinitions(CSDef)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CSDefinitionSet), intent(inout)   :: CSdef !< Current cross-section definition
   

   ! Local variables
   integer                   :: ierr
   type(t_CSType), pointer, dimension(:)    :: oldDefs

   ! Program code

   if (CSDef%Size > 0) then
      oldDefs=>CSDef%CS
   endif

   if (CSDef%growsBy <=0) then
      CSDef%growsBy = 200
   endif
   allocate(CSDef%CS(CSDef%Size+CSDef%growsBy),stat=ierr)
   call aerr('CSDef%CS(CSDef%Size+CSDef%growsBy)',ierr,CSDef%Size+CSDef%growsBy)

   if (CSDef%size > 0) then
      CSDef%CS(1:CSDef%size) = oldDefs(1:CSDef%size)
      deallocate(oldDefs)
   endif
   CSDef%Size = CSDef%Size+CSDef%growsBy
end subroutine

!> Retrieve the cross section type number by character string
integer function GetCrossType(string)

   character(len=*), intent(in) :: string   !< name of the cross section type    

   select case(str_tolower(trim(string)))
      case ('tabulated')
         GetCrossType = CS_TABULATED ! v1
      case ('trapezium')
         GetCrossType = CS_TRAPEZIUM ! v1, v2
      case ('circle')
         GetCrossType = CS_CIRCLE    ! v1, v2
      case ('egg')
         GetCrossType = CS_EGG       ! v1
      case ('yz', 'xyz')
         GetCrossType = CS_YZ_PROF   ! v1, v2
      case ('rectangle')
         GetCrossType = CS_RECTANGLE ! v1, v2
      case ('ellipse')
         GetCrossType = CS_TABULATED ! v1
      case ('arch')
         GetCrossType = CS_TABULATED ! v1
      case ('cunette')
         GetCrossType = CS_TABULATED ! v1
      case ('steelcunette')
         GetCrossType = CS_TABULATED ! v1
      case ('zw')
         GetCrossType = CS_TABULATED !     v2
      case ('zwriver')
         GetCrossType = CS_TABULATED !     v2
      case default
         GetCrossType = -1
   end select

end function GetCrossType


!> integer function AddRoundCrossSectionDefinition adds an CSType (cross section definition type) element to the
!! set CSDefinitions of type t_CSDefinitionSet and initializes the added element using the parameters passed.
integer function AddRoundCrossSectionDefinition(CSDefinitions, id, diameter, shape, groundlayerUsed, groundlayer)

   implicit none
   ! Input/output parameters
   type(t_CSDefinitionSet), intent(inout) :: CSDefinitions      !< cross section definition set
   character(len=*), intent(in) :: id                           !< id of the cross section definition
   double precision, intent(in)     :: diameter                 !< diameter of the cross section definition
   integer, intent(in)              :: shape                    !< shape can be CS_CIRCLE or CS_EGG
   logical, intent(in)              :: groundlayerUsed          !< flag indicating whether a ground layer is used
   double precision, intent(in)     :: groundLayer              !< Thickness of the groundlayer

   ! local variables
   integer     :: i

   ! program code
   CSDefinitions%count = CSDefinitions%count+1

   if (CSDefinitions%count > CSDefinitions%size) then   ! skipping real allocation
      call realloc(CSDefinitions)
   endif

   i = CSDefinitions%count

   if (shape /= CS_CIRCLE .and. shape /= CS_EGG) then
      call setmessage(LEVEL_ERROR, 'INTERNAL ERROR: incorrect type of cross section.')
      return
   endif

   CSDefinitions%CS(i)%id           = id 
   CSDefinitions%CS(i)%crossType    = shape
   CSDefinitions%CS(i)%diameter     = diameter
   CSDefinitions%CS(i)%closed       = .true.
   
   allocate(CSDefinitions%CS(i)%groundlayer)
   if (groundlayerUsed) anyGroundLayer       = .true.
   CSDefinitions%CS(i)%groundlayer%used      = groundlayerUsed
   CSDefinitions%CS(i)%groundlayer%thickness = groundlayer
!
   AddRoundCrossSectionDefinition = i

end function AddRoundCrossSectionDefinition


!> integer function AddTabCrossSectionDefinition adds an CSType (cross section definition type) element to the
!! set CSDefinitions of type t_CSDefinitionSet and initializes the added element using the parameters passed.

integer function AddTabCrossSectionDefinition(CSDefinitions , id, numLevels, level, flowWidth, totalWidth, plains,  &
                                            & crestLevel, baseLevel, flowArea, totalArea, closed,              &
                                            & groundlayerUsed, groundlayer)
   ! Modules
   use m_alloc
   use m_GlobalParameters, only : ThresholdForPreismannLock
   implicit none

   ! Input/output parameters
   type(t_CSDefinitionSet), intent(inout) :: CSDefinitions
   character(len=*), intent(in) :: id
   integer, intent(in)              :: numLevels                !< length of the arrays: level, flowWidth and totalWidth
   double precision, intent(in)     :: level(numLevels)         !< heights at which widths are defined
   double precision, intent(in)     :: flowWidth(numLevels)     !< flow width as a function of level
   double precision, intent(in)     :: totalWidth(numLevels)    !< total width as a function of level
   double precision, intent(in)     :: plains(3)                !< width of main section, flood plane 1 and flood plane 2
   double precision, intent(in)     :: crestLevel               !< level height of summerdike
   double precision, intent(in)     :: baseLevel                !< level height of base of summerdike channel
   double precision, intent(in)     :: flowArea                 !< effective flow area of summer dike channel
   double precision, intent(in)     :: totalArea                !< the total area of summer dike channel
   logical, intent(in)              :: closed                   !< flag indicating whether a cross section is closed
   logical, intent(in)              :: groundlayerUsed          !< flag indicating whether a ground layer is used
   double precision, intent(in)     :: groundLayer              !< Thickness of the groundlayer

   ! Local variables
   integer i, j, length
!
   ! Program code
   CSDefinitions%count = CSDefinitions%count+1

   if (CSDefinitions%count > CSDefinitions%size) then
      call realloc(CSDefinitions)
   endif
!
   i = CSDefinitions%count
   if (closed) then
      length = numLevels+1
   else
      length = numLevels
   endif
   call realloc(CSDefinitions%CS(i)%height, length)
   call realloc(CSDefinitions%CS(i)%flowWidth, length)
   call realloc(CSDefinitions%CS(i)%totalWidth, length)

   call realloc(CSDefinitions%CS(i)%af_sub, 3, length)
   call realloc(CSDefinitions%CS(i)%width_sub, 3, length)
   call realloc(CSDefinitions%CS(i)%perim_sub, 3, length)
   call realloc(CSDefinitions%CS(i)%flowArea, length)
   call realloc(CSDefinitions%CS(i)%wetPerimeter, length)
   call realloc(CSDefinitions%CS(i)%totalArea, length)
   call realloc(CSDefinitions%CS(i)%area_min, length)
   call realloc(CSDefinitions%CS(i)%width_min, length)   
   
   CSDefinitions%CS(i)%id          = id 
   CSDefinitions%CS(i)%crossType   = cs_tabulated
   CSDefinitions%CS(i)%levelsCount = length
   CSDefinitions%CS(i)%plains      = plains
   do j = 1, numlevels
      CSDefinitions%CS(i)%height(j)  = level(j)
      CSDefinitions%CS(i)%flowWidth(j)   = flowWidth(j)
      CSDefinitions%CS(i)%totalWidth(j) = max(ThresholdForPreismannLock, totalWidth(j), flowWidth(j))
   enddo
   if (closed) then
      CSDefinitions%CS(i)%height(numLevels+1) = CSDefinitions%CS(i)%height(numLevels)+1d-7
      CSDefinitions%CS(i)%flowWidth(numLevels+1)  = 0d0
      CSDefinitions%CS(i)%totalWidth(numLevels+1) = ThresholdForPreismannLock
   endif
   CSDefinitions%CS(i)%closed = closed
   
   call createTablesForTabulatedProfile(CSDefinitions%CS(i))
!
   ! Initialize the summer dike information of the newly added cross-section
   if (flowArea > 1.0d-5 .or. totalArea > 1.0d-5) then
      allocate(CSDefinitions%CS(i)%summerdike)
      CSDefinitions%CS(i)%summerdike%crestLevel = crestLevel
      CSDefinitions%CS(i)%summerdike%baseLevel  = baseLevel
      CSDefinitions%CS(i)%summerdike%flowArea   = flowArea
      CSDefinitions%CS(i)%summerdike%totalArea  = totalArea
   endif
   
   ! Initialize groundlayer information of the newly added cross-section
   allocate(CSDefinitions%CS(i)%groundlayer)
   if (groundlayerUsed) anyGroundLayer       = .true.
   CSDefinitions%CS(i)%groundlayer%used      = groundlayerUsed
   CSDefinitions%CS(i)%groundlayer%thickness = groundlayer

   AddTabCrossSectionDefinition = i
end function AddTabCrossSectionDefinition


!> Adds YZ cross-sectin definition to the set of cross-section (complete with all attributes).
integer function AddYZCrossSectionDefinition(CSDefinitions, id, Count, y, z, frictionCount, frictionSectionID,           &
                                             frictionSectionFrom, frictionSectionTo, levelsCount, storageLevels, storage)
   use m_alloc

   type(t_CSDefinitionSet) CSDefinitions
   character(len=*), intent(in) :: id
   integer, intent(in)                        :: Count                              !< Size of arrays: y and z
   integer, intent(in)                        :: frictionCount                      !< Number of friction settings
   integer, intent(in)                        :: levelsCount                        !< Number of levels
   double precision, intent(in)               :: y(count)                           !< transversal y-co-ordinate
   double precision, intent(in)               :: z(count)                           !< level z-co-ordinate
   character(IdLen), intent(in)               :: frictionSectionID(frictionCount)   !< Friction Section Identification
   double precision, intent(in)               :: frictionSectionFrom(frictionCount) !< Start point of friction section
   double precision, intent(in)               :: frictionSectionTo(frictionCount)   !< End point of friction section
   double precision, intent(in)               :: storageLevels(levelsCount)         !< Storage levels
   double precision, intent(in)               :: storage(levelsCount)               !< Storage values

   integer                                    :: i

   CSDefinitions%count = CSDefinitions%count+1
   if (CSDefinitions%count > CSDefinitions%size) then
      call realloc(CSDefinitions)
   endif

   i = CSDefinitions%count

   CSDefinitions%CS(i)%id                    = id 
   CSDefinitions%CS(i)%levelsCount           = Count
   CSDefinitions%CS(i)%crossType             = cs_YZ_Prof
   CSDefinitions%CS(i)%frictionSectionsCount = frictionCount
   CSDefinitions%CS(i)%storLevelsCount       = levelsCount

   call realloc(CSDefinitions%CS(i)%y, Count)
   call realloc(CSDefinitions%CS(i)%z, Count)
   call realloc(CSDefinitions%CS(i)%storLevels, max(levelsCount,2))
   call realloc(CSDefinitions%CS(i)%YZstorage, max(levelsCount,2))
   call realloc(CSDefinitions%CS(i)%frictionSectionID, frictionCount)
   call realloc(CSDefinitions%CS(i)%frictionSectionFrom, frictionCount)
   call realloc(CSDefinitions%CS(i)%frictionSectionTo, frictionCount)

   CSDefinitions%CS(i)%y = y
   CSDefinitions%CS(i)%z = z

   CSDefinitions%CS(i)%storLevels = storageLevels
   if (levelsCount==1)then
      CSDefinitions%CS(i)%storLevels(2) = storageLevels(1)+1.0
      CSDefinitions%CS(i)%YZstorage(1) = storage(1)
      CSDefinitions%CS(i)%YZstorage(2) = storage(1)
   else
      CSDefinitions%CS(i)%YZstorage = storage
   endif

   CSDefinitions%CS(i)%frictionSectionID   = frictionSectionID
   CSDefinitions%CS(i)%frictionSectionFrom = frictionSectionFrom
   CSDefinitions%CS(i)%frictionSectionTo   = frictionSectionTo
   
   allocate(CSDefinitions%CS(i)%groundlayer)
   CSDefinitions%CS(i)%groundlayer%used      = .false.
   CSDefinitions%CS(i)%groundlayer%thickness = 0d0

   AddYZCrossSectionDefinition = i
end function AddYZCrossSectionDefinition

!> Free the memory used by cross-section
subroutine deallocCrossSection(cross)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CrossSection), intent(inout) :: cross !< Current cross-section

   ! Local variables

   ! Program code

   if (.not. cross%IsCopy) then
   
      if (associated(cross%convTab1)) then
         call dealloc(cross%convTab1)
      endif
      if (associated(cross%convTab2)) then
         call dealloc(cross%convTab2)
      endif
      
      if (associated(cross%tabDef)) then
         call dealloc(cross%tabDef)
      endif
      
      if (allocated(cross%frictionSectionID))   deallocate(cross%frictionSectionID)      !< Friction Section Identification
      if (allocated(cross%frictionSectionFrom)) deallocate(cross%frictionSectionFrom)    !<
      if (allocated(cross%frictionSectionTo))   deallocate(cross%frictionSectionTo)      !<
      if (allocated(cross%frictionTypePos))     deallocate(cross%frictionTypePos)        !< Friction type for positive flow direction
      if (allocated(cross%frictionValuePos))    deallocate(cross%frictionValuePos)       !< Friction value for positive flow direction
      if (allocated(cross%frictionTypeNeg))     deallocate(cross%frictionTypeNeg)        !< Friction type for negative flow direction
      if (allocated(cross%frictionValueNeg))    deallocate(cross%frictionValueNeg)       !< Friction value for negative flow direction

   endif

   cross%convTab1 => null()
   cross%convTab2 => null()
   cross%tabDef => null()

end subroutine deallocCrossSection

!> Free the memory used by the cross-sections
subroutine deallocCrossSections(crs)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CrossSectionSet), intent(inout) :: crs !< Current cross-section set

   ! Local variables
   integer i

   ! Program code
   if (associated(crs%cross)) then

      do i = 1, crs%count
         if(.not. crs%cross(i)%IsCopy) then
            call dealloc(crs%cross(i))
         endif
      enddo

      if (associated(crs%crossSectionIndex)) then
         deallocate(crs%crossSectionIndex)
      endif
      
      deallocate(crs%cross)
      crs%crossSectionIndex => null()
      crs%cross => null()
      crs%size  = 0
      crs%count = 0
   endif
   
end subroutine deallocCrossSections

!> Allocate memory for cross-sections
subroutine reallocCrossSections(crs)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_CrossSectionSet), intent(inout)      :: crs !< Current cross-section set

   ! Local variables
   type(t_CrossSection), pointer, dimension(:) :: oldCrs
   integer :: ierr

   ! Program code

   if (crs%size > 0) then
      oldCrs=>crs%cross
   endif

   if (crs%growsBy <=0) then
      crs%growsBy = 200
   endif
   allocate(crs%cross(crs%size+crs%growsBy), stat=ierr)
   if (ierr /=0) then
      call setMessage(LEVEL_FATAL, 'allocation error')
   endif
   
   if (crs%size > 0) then
      crs%cross(1:crs%size) = oldCrs(1:crs%size)
      deallocate(oldCrs)
   endif
   crs%size = crs%size+crs%growsBy
end subroutine

!> Add a cross-section on an reach
integer function  AddCrossSectionByVariables(crs, CSDef, branchid, chainage, iref, bedLevel, bedFrictionType, &
                  bedFriction, groundFrictionType, groundFriction)

   type(t_CrossSectionSet)          :: crs                !< Cross-section set
   type(t_CSDefinitionSet)          :: CSDef              !< Cross-section definition set
   integer, intent(in)              :: branchid           !< Reach id (integer)
   double precision, intent(in)     :: chainage           !< chainage on the reach
   integer, intent(in)              :: iref               !< Current cross-section
   double precision, intent(in)     :: bedLevel           !< bed level of the cross section
   integer, intent(in)              :: bedFrictionType    !< bed friction type
   double precision, intent(in)     :: bedFriction        !< Bed friction value
   integer, intent(in)              :: groundFrictionType !< Groundlayer fricton type
   double precision, intent(in)     :: groundFriction     !< Groundlayer friction value

   integer                          :: i, j

   crs%count = crs%count+1
   !! THREEDI-278
   !! NOTE: below is a dangerous realloc, see THREEDI-278
   !! By reallocating here, all pointers that used to point to input crs set (e.g. pcross in t_culvert),
   !! will afterwards point to undefined memory, since it was reallocated elsewhere.
   !! Update Dec 9, 2019: this problem is fixed for structures in finishReading().
   if (crs%count > crs%size) then
      call realloc(crs)
   endif

   i = crs%count

   if (branchid/=0) then
      crs%cross(i)%branchid = branchid
   endif

   crs%cross(i)%chainage            = chainage
   crs%cross(i)%bedLevel            = bedLevel
   crs%cross(i)%shift               = bedlevel
   crs%cross(i)%frictionSectionsCount = 1
   crs%cross(i)%crossType = CSDef%CS(iref)%crossType
   allocate(crs%cross(i)%frictionTypePos(1), crs%cross(i)%frictionTypeNeg(1), crs%cross(i)%frictionValuePos(1), crs%cross(i)%frictionValueNeg(1))
   allocate(crs%cross(i)%frictionSectionFrom(1), crs%cross(i)%frictionSectionTo(1))
   
   if (crs%cross(i)%crossType == CS_TABULATED) then
      crs%cross(i)%frictionSectionFrom(1) = 0d0
      crs%cross(i)%frictionSectionTo(1) = CSDef%CS(iref)%totalWidth(1)
      do j = 2, CSDef%CS(iref)%levelsCount
         crs%cross(i)%frictionSectionTo(1) = max(crs%cross(i)%frictionSectionTo(1),CSDef%CS(iref)%totalWidth(j))
      enddo
   elseif (crs%cross(i)%crossType == CS_YZ_PROF) then
      crs%cross(i)%frictionSectionFrom(1) = CSDef%CS(iref)%y(1)
      crs%cross(i)%frictionSectionTo(1) = CSDef%CS(iref)%y(1)
      do j = 2, CSDef%CS(iref)%levelsCount
         crs%cross(i)%frictionSectionfrom(1) = min(crs%cross(i)%frictionSectionFrom(1),CSDef%CS(iref)%y(j))
         crs%cross(i)%frictionSectionTo(1) = max(crs%cross(i)%frictionSectionTo(1),CSDef%CS(iref)%y(j))
      enddo
   endif
      
   crs%cross(i)%frictionTypePos(1)  = bedFrictionType
   crs%cross(i)%frictionTypeNeg(1)  = bedFrictionType
   crs%cross(i)%frictionValuePos(1) = bedFriction
   crs%cross(i)%frictionValueNeg(1) = bedFriction
   crs%cross(i)%groundFrictionType  = groundFrictionType
   crs%cross(i)%groundFriction      = groundFriction
   crs%cross(i)%itabDef             = iref
   crs%cross(i)%tabDef              => CSDef%CS(iref)
   
   if (crs%cross(i)%crossType == CS_YZ_PROF) then
      crs%cross(i)%convtab1 => null()
      call CalcConveyance(crs%cross(i))    
   end if
   call SetParsCross(CSDef%CS(iref), crs%cross(i))
   
   AddCrossSectionByVariables = i
   
end function  AddCrossSectionByVariables
                
!> set additional cross section parameters  
subroutine SetParsCross(CrossDef, cross)
   type(t_CStype), intent(in)          :: CrossDef        !< cross section definition
   type(t_CrossSection), intent(inout) :: Cross           !< cross section 
      
   double precision                 :: surfLevel
   double precision                 :: bedlevel
   integer :: j
   type(t_CrossSection)             :: crossSection
   
   cross%crossType = Crossdef%crossType
   cross%closed    = Crossdef%closed


   if (Crossdef%crossType == cs_tabulated) then
      cross%surfaceLevel = Crossdef%height(Crossdef%levelsCount) + cross%shift
      cross%bedLevel     = Crossdef%height(1) + cross%shift
   elseif (Crossdef%crossType == CS_YZ_PROF) then
      surfLevel = -huge(1d0)
      bedlevel  = huge(1d0)
      do j = 1, Crossdef%levelsCount
         if (surfLevel < Crossdef%z(j)) then
            surfLevel = Crossdef%z(j)
         endif
         if (bedlevel > Crossdef%z(j)) then
            bedLevel = Crossdef%z(j)
         endif
      enddo
      bedlevel = bedlevel + Crossdef%bedlevel
      cross%surfaceLevel = cross%shift + surflevel
      cross%bedLevel     = cross%shift + bedlevel
   else
      cross%surfaceLevel = cross%shift + cross%tabDef%diameter
      cross%bedLevel     = cross%shift
   endif
   
   ! Get Characteristic Height and Width
   call SetCharHeightWidth(cross)
   
   if (cross%tabDef%groundlayer%used .and. (cross%tabDef%groundlayer%Area < 1.0d-8) ) then

      crossSection = CopyCross(cross)
      
      if (associated(crossSection%tabDef%summerdike)) then
         deallocate(crossSection%tabDef%summerdike)
         crossSection%tabDef%summerdike => null()
      endif
      
      crossSection%tabDef%groundlayer%used      = .false.
      crossSection%tabDef%groundlayer%thickness = 0.0d0
      
      call GetCSParsFlowCross(crossSection,  cross%tabDef%groundlayer%thickness, &
                              cross%tabDef%groundlayer%area, cross%tabDef%groundlayer%perimeter, &
                              cross%tabDef%groundlayer%width)
      
      ! Deallocate Temporary Copy
      call dealloc(crossSection)
      
   endif                        
end subroutine SetParsCross

!> Add a cross-section on a reach, using a cross section defined on another reach
integer function AddCrossSectionByCross(crs, cross, branchid, chainage)

   type(t_CrossSectionSet)          :: crs       !< Cross-section set
   type(t_CrossSection), intent(in) :: cross     !< Cross-section
   integer, intent(in)              :: branchid  !< Reach id
   double precision, intent(in)     :: chainage  !< chainage on the reach
   
   integer                          :: i

   crs%count = crs%count+1
   
   if (crs%count > crs%size) then
      call realloc(crs)
   endif

   i = crs%count

   crs%cross(i) = cross
   crs%cross(i)%branchid = branchid
   crs%cross(i)%chainage = chainage

   AddCrossSectionByCross = i

end function AddCrossSectionByCross

!> interpolation of width arrays.
subroutine interpolateWidths(height1, width1, levelsCount1, height2, width2, levelsCount2, height, width, levelsCount, f)
   ! modules

   implicit none

   ! variables
   integer, intent(in)                    :: levelsCount1               !< number of levels in height1, width1
   integer, intent(in)                    :: levelsCount2               !< number of levels in height2, width2
   integer, intent(in)                    :: levelsCount                !< number of levels in height, width
   double precision, intent(in)           :: height1(levelsCount1)      !< levels (z) of width array at location 1
   double precision, intent(in)           :: width1(levelsCount1)       !< widths as function of height at location 1
   double precision, intent(in)           :: height2(levelsCount2)      !< levels (z) of width array at location 2
   double precision, intent(in)           :: width2(levelsCount2)       !< widths as function of height at location 2
   double precision, intent(in)           :: height(levelsCount)        !< levels (z) of interpolated width array at location
   double precision, intent(out)          :: width(levelsCount)         !< interpolated widths as function of height
   double precision, intent(in)           :: f

   ! local variables
   integer              :: i
   integer              :: i1
   integer              :: i2
   double precision     :: frac
   double precision     :: w1
   double precision     :: w2
   !program code
   i1 = 1
   i2 = 1
   do i = 1, levelscount
      ! Fill first part of width array, and interpolate missing values
      if (height1(i1) > height(i) .and. i1 == 1 ) then
         w1 = width1(i1)
      elseif (height1(i1) == height(i) ) then
         w1 = width1(i1)
         i1 = min(levelsCount1,i1 + 1)
      elseif(height1(i1) < height(i)) then
         w1 = width1(i1)
      else
         frac = (height(i) - height1(i1 - 1)) / (height1(i1) - height1(i1 - 1))
         w1 = (1.0d0 - frac) * width1(i1 - 1) + frac * width1(i1)
      endif
      if (height2(i2) > height(i) .and. i2==1 ) then
         w2 = width2(i2)
      elseif (height2(i2) == height(i) ) then
         w2 = width2(i2)
         i2 = min(levelsCount2,i2 + 1)
      elseif(height2(i2) < height(i)) then
         w2 = width2(i2)
      else
         frac = (height(i) - height2(i2 - 1)) / (height2(i2) - height2(i2 - 1))
         w2 = (1.0d0 - frac) * width2(i2 - 1) + frac  *width2(i2)
      endif

      width(i) = (1d0 - f) * w1 + f * w2

   enddo

end subroutine interpolateWidths

!> this subroutine uses the branchOrders from the input to add extra cross sections  \n
!! e.g assume a model network, where * represents a gridpoint, C represents a cross section, # represents a connection node \n
!! branch1: #---C---*------*------*---C---#                                             \n
!! branch2:                               #------*---C---*------*------*------*---C---# \n
!! This will result in:                                                                 \n
!! branch1: #---C---*------*------*---C---#----------C                                  \n
!! branch2:                           C---#------*---C---*------*------*------*---C---# \n
!! As a result the interpolation can be restricted to a branch
subroutine useBranchOrdersCrs(crs, brs)
   ! modules
   use messageHandling
   use stdlib_kinds, only: int32
   use stdlib_sorting, only: sort_index
   implicit none
   ! variables
   type(t_CrossSectionSet), intent(inout)          :: crs       !< Current cross-section set
   type(t_branchSet)      , intent(in   )          :: brs       !< Set of reaches

   ! local variables
   integer  ibr
   integer  ics, iorder, minOrderNumber, OrderNumberCount
   integer  crsCount
   integer, allocatable, dimension(:,:)         :: orderNumber       !< first index contains orderNumber, second contains start position for this ordernumber
   double precision, allocatable, dimension(:)  :: crsData
   integer(int32), allocatable, dimension(:)    :: crsIndices
   type(t_CrossSection)                         :: cross
   type(t_CrossSectionSet)                      :: tempset
   integer                                      :: maxBranchId, maxBranchOrder
   double precision                             :: maxChainage
   double precision                             :: F1, F2 !< sorting multiplication factors

   crsCount = crs%count
   tempset%size = crsCount
   tempset%count = crsCount
   
   maxBranchId    = max(1,maxval(crs%cross(:)%branchId))
   maxBranchOrder = max(1,maxval(brs%branch(:)%ordernumber)+1)
   maxChainage    = maxval(crs%cross(:)%chainage)

   ! Multiplication factors for sorting 
   F2 = maxChainage+1
   F1 = (maxBranchId+1)*F2

   allocate(crsData(crsCount),crs%crossSectionIndex(crscount),crsIndices(crsCount),tempset%cross(crsCount),orderNumber(maxBranchOrder+2,2))
   ! We want to sort the array by branch order and chainage, but in groups:
   ! first cross sections that are not on any branch (order by crs index),
   ! then cross sections on branches that have no branch order (order by chainage),
   ! and finally by both order number and chainage.
   ! To this end we multiply branch id by F1 and branch order by F2 to be able to sort them all at once.
   ! See: UNST-3680
   do ics = 1, crsCount
      ibr = crs%cross(ics)%branchid
      if (ibr <= 0) then ! crs without branch first
         crsData(ics) = crs%cross(ics)%chainage
      else if (getOrderNumber(brs, ibr) < 0) then
         crsData(ics) = crs%cross(ics)%branchid*F2 + crs%cross(ics)%chainage
      else   
         crsData(ics) = (getOrderNumber(brs, ibr)+1)*F1 + crs%cross(ics)%branchid*F2 + crs%cross(ics)%chainage
      endif
   enddo

   call sort_index(crsData,crsIndices)

   do ics = 1, crsCount !copy data to temp array
      tempset%cross(ics) = crs%cross(crsIndices(ics))
      crs%crossSectionIndex(crsIndices(ics)) = ics 
   enddo
   crs%cross(:) = tempset%cross(:) !copy temp array to real array
   
   !ENABLE THIS LOOP TO DEBUG SORTED BRANCH ORDERS IN CASE OF PROBLEMS IN USEBRANCHORDERSCRS
   !do ics = 1, crsCount 
   !   crsData(ics) = getOrderNumber(brs,crs%cross(ics)%branchId)    
   !enddo
   
   !check for multiple crossSections on a branch and fill OrderNumber array
   minordernumber = -1
   ordernumbercount = 1
   orderNumber(1,1) = -1
   orderNumber(1,2) = 1
   do ics = 1, crsCount
      cross = crs%cross(ics)
      if (ics > 1) then
         if ( crs%cross(ics)%branchid > 0 .and. (crs%cross(ics-1)%branchid == crs%cross(ics)%branchid) .and. (crs%cross(ics-1)%chainage == crs%cross(ics)%chainage) ) then
            msgbuf = 'Cross section ''' // trim(crs%cross(ics-1)%csid) // ''' and ''' // trim(crs%cross(ics)%csid) // ''' are exactly at the same location.'
            call err_flush()
         endif
      endif
      if (crs%cross(ics)%branchid > 0) then
      minOrderNumber = max(minOrderNumber,getOrderNumber(brs, crs%cross(ics)%branchid))
      endif
      if (orderNumber(orderNumberCount,1) /= minOrderNumber) then
         orderNumberCount = orderNumberCount + 1
         orderNumber(orderNumberCount, 1) = minOrderNumber
         orderNumber(orderNumberCount, 2) = ics
      endif
   enddo

   orderNumber(orderNumberCount+1,1) = -999
   orderNumber(orderNumberCount+1,2) = crsCount+1
   ! Now check all cross sections on branches of the same order (-1 orders can be skipped)
   do iorder = 2, orderNumberCount
      ics = orderNumber(iorder, 2)
      do while (ics < orderNumber(iorder+1,2) )
         ! ics is now the first cross section on the branch
         ibr = crs%cross(ics)%branchid
         
         ! because crs%cross can be reallocated a copy of the cross section has to be made
         cross = crs%cross(ics)
         cross%IsCopy = .true.
         call findNeighbourAndAddCrossSection(brs, crs, ibr, cross, cross%chainage, .true., orderNumber, orderNumberCount)
         ! find last cross section on branch
         do while (ics < crs%count .and. crs%cross(ics+1)%branchid == ibr)
            ics = ics+1
         enddo
         ! The last cross section in CRS is the last cross section on a branch
         if (ics < crs%count) then
            if (crs%cross(ics+1)%branchid == ibr) then
               ics = ics +1
            endif
         endif
         
         ! because crs%cross can be reallocated a copy of the cross section has to be made
         cross = crs%cross(ics)
         cross%IsCopy = .true.
         call findNeighbourAndAddCrossSection(brs, crs, ibr, cross, cross%chainage, .false., orderNumber, orderNumberCount)
         ics = ics +1
      enddo
   enddo
   deallocate(orderNumber, crsData, crsIndices)

end subroutine useBranchOrdersCrs

!> returns the order number of a given branch index
integer function getOrderNumber(brs, ibr)
   type(t_branchSet), intent(in)    :: brs       !< Set of reaches
   integer, intent(in)              :: ibr       !< branch index

   if (ibr <= brs%count .and. ibr > 0) then
      getOrderNumber = brs%branch(ibr)%orderNumber
   else
      getOrderNumber = -1
   endif
end function getOrderNumber

!> The first cross section and the last cross section on a branch (B1), with an order number > 0
!! can be used to interpolate over a connection node. The algorithm is to locate a neighbouring
!! branch (same begin or end node) with the same order number (B2). When found the cross section
!! is added to the branch B2, with an offset outside the branch (either -OFFSET, or LENGTH+OFFSET).
recursive subroutine findNeighbourAndAddCrossSection(brs, crs, branchid, cross, offset, beginNode, orderNumber, orderNumberCount)
   ! modules

   implicit none

   ! variables
   integer  :: orderNumberCount
   type(t_CrossSectionSet), intent(inout)          :: crs            !< Current cross-section set
   type(t_branchSet)      , intent(in   )          :: brs            !< Set of reaches
   integer                , intent(in   )          :: branchid       !< branch for which a neighbour is requested
   type(t_CrossSection)   , intent(in   )          :: cross          !< cross section
   double precision       , intent(inout)          :: offset         !< chainage of cross section on branch
   logical                , intent(in   )          :: beginNode      !< indicates whether the begin or end node is to be used of the branch
   integer, dimension(:,:), intent(in   )          :: orderNumber    !< first index contains orderNumber, second contains start position for this ordernumber

   ! local variables
   integer                          :: nodeIndex
   integer                          :: ibr
   integer                          :: i
   integer                          :: iorder
   integer                          :: j
   integer                          :: idum
   logical                          :: found
   logical                          :: nextBeginNode
   double precision                 :: distanceFromNode
   double precision                 :: chainage
   type(t_branch), pointer          :: branch

   !program code
   if (brs%count==0) then
      return
   endif

   branch => brs%branch(branchid)

   ibr = branch%index
   if (beginNode) then
      nodeIndex = branch%FromNode%index
      distanceFromNode = offset
   else
      nodeIndex = branch%ToNode%index
      distanceFromNode = branch%length - offset
   endif
   found = .false.
   do i = 1, brs%count
      if ( (brs%branch(i)%orderNumber == branch%orderNumber) .and. (i /= ibr ) ) then
         if (brs%branch(i)%ToNode%index == nodeIndex) then
            found = .true.
            ! interpolation is required over begin node of reach i
            chainage = brs%branch(i)%length + distanceFromNode
            nextBeginNode = .true.              ! NOTE: since the end node of the reach is connected to a node of the first reach, now check the begin node
            exit
         elseif (brs%branch(i)%FromNode%index == nodeIndex) then
            ! interpolation is required over end node of reach i
            found = .true.
            chainage = - distanceFromNode
            nextBeginNode = .false.             ! NOTE: since the begin node of the reach is connected to a node of the first reach, now check the end node
            exit
         endif
      endif
   enddo

   if (found) then
      idum = AddCrossSection(crs, cross, i, chainage)
      ! look in crs if a cross section is placed on brs%branch(i)
      found = .false.
      do iorder = 1, orderNumberCount
         if (orderNumber(iorder,1) == brs%branch(i)%orderNumber) then
            exit
         endif
      enddo

      do j = orderNumber(iorder,2), orderNumber(iorder+1,2)-1
         if (brs%branch(crs%cross(j)%branchid)%index ==i) then
            found = .true.
            exit
         endif
      enddo
      if (.not. found) then
         ! Now check for another neighbouring branch
         call findNeighbourAndAddCrossSection(brs, crs, i, cross, chainage, nextBeginNode, orderNumber, orderNumberCount)
      endif
   endif

end subroutine findNeighbourAndAddCrossSection

!> Get the flow area, wet perimeter and flow width located on a link
subroutine GetCSParsFlowInterpolate(line2cross, cross, dpt, flowArea, wetPerimeter, flowWidth, maxFlowWidth, af_sub, perim_sub)

   use m_GlobalParameters
   
   implicit none

   type(t_chainage2cross),intent(in)         :: line2cross     !< cross section indirection
   type(t_CrossSection), target, intent(in)  :: cross(:)      !< array containing cross section information
   double precision, intent(in   )           :: dpt            !< water depth at cross section
   double precision, intent(  out)           :: flowArea       !< flow area for given DPT
   double precision, intent(  out)           :: wetPerimeter   !< wet perimeter for given DPT
   double precision, intent(  out)           :: flowWidth      !< flow width of water surface
   double precision, intent(  out), optional :: maxFlowWidth   !< maximum flow width of wet flow area
   double precision, intent(  out), optional :: af_sub(3)      !< flow area for main, floodplain1 and floodplain2
   double precision, intent(  out), optional :: perim_sub(3)   !< wet perimeter for main, floodplain1 and floodplain2   

   type (t_CrossSection), pointer        :: cross1         !< cross section
   type (t_CrossSection), pointer        :: cross2         !< cross section
   double precision                      :: f              !< cross = (1-f)*cross1 + f*cross2
   double precision                      :: flowArea1
   double precision                      :: flowArea2
   double precision                      :: wetPerimeter1
   double precision                      :: wetPerimeter2
   double precision                      :: flowWidth1   
   double precision                      :: flowWidth2   
   double precision                      :: maxFlowWidth1   
   double precision                      :: maxFlowWidth2   
   type (t_CrossSection), save           :: crossi         !< intermediate virtual crosssection     

   double precision                      :: af_sub_local1(3), af_sub_local2(3)      
   double precision                      :: perim_sub_local1(3), perim_sub_local2(3)

   if (line2cross%c1 <= 0) then
      ! no cross section defined on branch, use default definition
      flowArea = default_width* dpt
      flowWidth = default_width
      if (present(maxFlowWidth)) maxFlowWidth= flowWidth
      wetPerimeter = default_width + 2d0*dpt
      if (present(af_sub)) then
         af_sub = 0d0
         af_sub(1) = flowArea
      endif
      if (present(perim_sub)) then
         perim_sub = 0d0
         perim_sub(1) = wetPerimeter
      endif
      return
   endif

   cross1 => cross(line2cross%c1)
   cross2 => cross(line2cross%c2)
   f =  line2cross%f

   if(cross1%crossIndx == cross2%crossIndx) then
      ! Same Cross-Section, no interpolation needed 
      call GetCSParsFlowCross(cross1, dpt, flowArea, wetPerimeter, flowWidth, maxFlowWidth1, af_sub_local1, perim_sub_local1)
      if (present(af_sub)   ) af_sub    = af_sub_local1
      if (present(perim_sub)) perim_sub = perim_sub_local1
      if (present(maxFlowWidth)) maxFlowWidth= maxFlowWidth1
   else
      select case (cross1%crosstype)
         
         case (CS_CIRCLE, CS_EGG)
            ! Create an interpolate cross-section here and call GetCSParsFlowCross for intermediate cross-section
            if(.not.associated(crossi%tabDef)) then 
               allocate(crossi%tabDef)
               if(.not.associated(crossi%tabDef%groundlayer)) then 
                  allocate(crossi%tabDef%groundlayer)
               endif 
            endif 
         
            crossi%crosstype          = cross1%crosstype   
            crossi%bedLevel           = (1.0d0 - f) * cross1%bedLevel + f * cross2%bedLevel
            crossi%surfaceLevel       = (1.0d0 - f) * cross1%surfaceLevel + f * cross2%surfaceLevel
            ! for circular cross sections there is only one roughness value set in bedfriction
            crossi%groundFriction     = (1.0d0 - f) * cross1%groundFriction + f * cross2%groundFriction
            crossi%groundFrictionType = cross1%groundFrictionType
            crossi%tabDef%diameter    = (1.0d0 - f) * cross1%tabDef%diameter + f * cross2%tabDef%diameter
         
            call GetCSParsFlowCross(crossi, dpt, flowArea, wetPerimeter, flowWidth, maxFlowWidth1, af_sub_local1, &
                     perim_sub_local1)
            if (present(af_sub)   ) af_sub    = af_sub_local1
            if (present(perim_sub)) perim_sub = perim_sub_local1
            if (present(maxFlowWidth)) maxFlowWidth= maxFlowWidth1
            
         case default                             ! Call GetCSParsFlowCross twice and interpolate the results 

            call GetCSParsFlowCross(cross1, dpt, flowArea1, wetPerimeter1, flowWidth1, maxFlowWidth1, af_sub_local1, &
                     perim_sub_local1, .true.)
            call GetCSParsFlowCross(cross2, dpt, flowArea2, wetPerimeter2, flowWidth2, maxFlowWidth2, af_sub_local2, &
                     perim_sub_local2, .true.)
        
            flowArea     = (1.0d0 - f) * flowArea1     + f * flowArea2
            flowWidth    = (1.0d0 - f) * flowWidth1    + f * flowWidth2
            if (present(maxFlowWidth)) then
               maxFlowWidth = (1.0d0 - f) * maxFlowWidth1 + f * maxFlowWidth2
            endif
            wetPerimeter = (1.0d0 - f) * wetPerimeter1 + f * wetPerimeter2

            ! compute average chezy 
            if (present(af_sub)) then
               af_sub = (1.0d0 - f) * af_sub_local1     + f * af_sub_local2
            endif
            if (present(perim_sub)) then
               perim_sub = (1.0d0 - f) * perim_sub_local1     + f * perim_sub_local2
            endif
         
      end select
      
   endif

end subroutine GetCSParsFlowInterpolate

!> Get flow area, wet perimeter and flow width at cross section location
subroutine GetCSParsFlowCross(cross, dpt, flowArea, wetPerimeter, flowWidth, maxFlowWidth, af_sub, perim_sub, doSummerDike)   

   use m_GlobalParameters
   use precision_basics
   use m_Roughness

   type (t_CrossSection), intent(in)          :: cross          !< cross section definition
   double precision, intent(in   )            :: dpt            !< water depth at cross section
   double precision, intent(  out)            :: flowArea       !< flow area for given DPT
   double precision, intent(  out)            :: wetPerimeter   !< wet perimeter for given DPT
   double precision, intent(  out)            :: flowWidth      !< flow width of water surface
   double precision, intent(  out), optional  :: maxFlowWidth   !< maximum flow width of wet flow area
   logical,          intent(in   ), optional  :: doSummerDike   !< Switch to calculate Summer Dikes or not
   double precision, intent(  out), optional  :: af_sub(3)      !< flow area for main, floodplain1 and floodplain2
   double precision, intent(  out), optional  :: perim_sub(3)   !< wet perimeter for main, floodplain1 and floodplain2      

   type(t_CSType), pointer           :: crossDef
   double precision                  :: widgr
   double precision                  :: maxFlowWidth1
   logical                           :: getSummerDikes
   double precision                  :: af_sub_local(3)      
   double precision                  :: perim_sub_local(3)      
   logical                           :: hysteresis =.true.   ! hysteresis is a dummy variable at this location, since this variable is only used for total areas
   perim_sub_local = 0d0
   if (dpt < 0.0d0) then
      flowArea     = 0.0
      wetPerimeter = 0.0
      flowWidth    = sl
      return
   endif

!   call system_clock(countstart)
   crossDef => cross%tabDef

   select case(cross%crosstype)
   case (CS_TABULATED)
      if (present(doSummerDike))then
         getSummerdikes = doSummerdike
      else
         getSummerDikes = .true.
      endif
      call TabulatedProfile(dpt, cross, .true., getSummerDikes, flowArea, flowWidth, maxFlowWidth1, wetPerimeter, af_sub_local, perim_sub_local, CS_TYPE_NORMAL, hysteresis)
   case (CS_CIRCLE)
      call CircleProfile(dpt, crossDef%diameter, flowArea, flowWidth, maxFlowWidth1, wetPerimeter, CS_TYPE_NORMAL)
   case (CS_EGG)
      call EggProfile(dpt, crossDef%diameter, flowArea, flowWidth, wetPerimeter, CS_TYPE_NORMAL)
      maxFlowWidth1 = flowWidth
   case (CS_YZ_PROF)
      ! Also in case a conveyance table is time dependent, the geometry remains constant, so no need for interpolation of time dependent conveyance tables is required.
      call YZProfile(dpt, cross%convtab1, 0, flowArea, flowWidth, maxFlowWidth1, wetPerimeter)
   case default
      call SetMessage(LEVEL_ERROR, 'INTERNAL ERROR: Unknown type of cross section')
   end select

   if (cross%crosstype /= CS_TABULATED ) then
      af_sub_local = 0d0
      af_sub_local(1) = flowArea
      
      perim_sub_local = 0d0
      perim_sub_local(1) = wetPerimeter
      
   endif
   ! correction for groundlayers
   if (associated(crossDef)) then
      if (crossDef%groundlayer%used) then
         widGr = crossDef%groundlayer%width
         flowArea = flowArea - crossDef%groundlayer%area
         af_sub_local(1) = af_sub_local(1) - crossDef%groundlayer%area

         wetPerimeter = wetPerimeter - crossDef%groundlayer%perimeter + widGr
         perim_sub_local(1) = perim_sub_local(1) - crossDef%groundlayer%perimeter + widGr
      endif
   endif

   
   if (present(af_sub)) then
      af_sub = af_sub_local
   endif
   if (present(perim_sub)) then
      perim_sub = perim_sub_local
   endif
   if (present(maxFlowWidth)) then
      maxFlowWidth = maxFlowWidth1
   endif

end subroutine GetCSParsFlowCross

!> Get total area and total width for given location and water depth
subroutine GetCSParsTotalInterpolate(line2cross, cross, dpt, totalArea, totalWidth, calculationOption, hysteresis, doSummerDike)
   use m_GlobalParameters
   
   implicit none

   type(t_chainage2cross),intent(in)         :: line2cross     !< cross section indirection
   type(t_CrossSection), target, intent(in)  :: cross(:)       !< array containing cross section information
   double precision, intent(in)              :: dpt            !< water depth at cross section
   double precision, intent(out)             :: totalArea      !< total area for given DPT
   double precision, intent(out)             :: totalWidth     !< total width of water surface
                                                               !> type of total area computation, possible values:\n
                                                               !! CS_TYPE_PREISMAN  Ordinary total area computation, with possible Preisman lock on top\n
                                                               !! CS_TYPE_PLUS      Total area for only the expanding part of the cross section (Nested Newton method)\n
                                                               !! CS_TYPE_MIN       Total area for only the narrowing part of the cross section (Nested Newton method)
   integer, intent(in)                       :: calculationOption 
   logical, intent(in), optional             :: doSummerDike   !< Switch to calculate Summer Dikes or not
   logical, intent(inout), optional          :: hysteresis(2)  !< hysteresis information for summer dikes
   double precision                      :: f              !< cross = (1-f)*cross1 + f*cross2
   double precision                      :: totalArea1
   double precision                      :: totalArea2
   double precision                      :: totalWidth1   
   double precision                      :: totalWidth2   
   type (t_CrossSection), save           :: crossi         !< intermediate virtual crosssection     
   type (t_CrossSection), pointer        :: cross1         !< cross section
   type (t_CrossSection), pointer        :: cross2         !< cross section

   if (line2cross%c1 <= 0) then
      ! no cross section defined on branch, use default definition
      totalArea = default_width* dpt
      totalWidth = default_width
      return
   endif

   cross1 => cross(line2cross%c1)
   cross2 => cross(line2cross%c2)
   f =  line2cross%f

   if(cross1%crossIndx == cross2%crossIndx) then
      ! Same Cross-Section, no interpolation needed 
      call GetCSParsTotalCross(cross1, dpt, totalArea, totalWidth, calculationOption, hysteresis(1))
   else
      select case (cross1%crosstype)
      case (CS_CIRCLE, CS_EGG)
         ! Create an interpolate cross-section here and call GetCSParstotalCross for intermediate cross-section
         if(.not.associated(crossi%tabDef)) then 
            allocate(crossi%tabDef)
            if(.not.associated(crossi%tabDef%groundlayer)) then 
               allocate(crossi%tabDef%groundlayer)
            endif 
         endif 
         
         crossi%crosstype          = cross1%crosstype   
         crossi%bedLevel           = (1.0d0 - f) * cross1%bedLevel + f * cross2%bedLevel
         crossi%surfaceLevel       = (1.0d0 - f) * cross1%surfaceLevel + f * cross2%surfaceLevel
         crossi%groundFriction     = (1.0d0 - f) * cross1%groundFriction + f * cross2%groundFriction
         crossi%groundFrictionType = cross1%groundFrictionType
         crossi%tabDef%diameter    = (1.0d0 - f) * cross1%tabDef%diameter + f * cross2%tabDef%diameter
         
         call GetCSParsTotalCross(crossi, dpt, totalArea, totalWidth, calculationOption, hysteresis(1))
         
      case default                             ! Call GetCSParstotalCross twice and interpolate the results 
         if (present(doSummerDike)) then
             call GetCSParsTotalCross(cross1, dpt, totalArea1, totalWidth1, calculationOption, hysteresis(1), doSummerDike = doSummerDike)
             call GetCSParsTotalCross(cross2, dpt, totalArea2, totalWidth2, calculationOption, hysteresis(2), doSummerDike = doSummerDike)
         else    
             call GetCSParsTotalCross(cross1, dpt, totalArea1, totalWidth1, calculationOption, hysteresis(1))
             call GetCSParsTotalCross(cross2, dpt, totalArea2, totalWidth2, calculationOption, hysteresis(2))
         endif         
         
         totalArea     = (1.0d0 - f) * totalArea1     + f * totalArea2
         totalWidth    = (1.0d0 - f) * totalWidth1    + f * totalWidth2
        
      end select
      
   endif
   
end subroutine GetCSParsTotalInterpolate

!> Get total area and total width for given cross section location and water depth
subroutine GetCSParsTotalCross(cross, dpt, totalArea, totalWidth, calculationOption, hysteresis, doSummerDike)

   use m_GlobalParameters
   
   ! Global Variables
   type (t_CrossSection), intent(in) :: cross           !< cross section
   double precision, intent(in)      :: dpt             !< water depth at cross section
   double precision, intent(out)     :: totalArea       !< total area for given DPT
   double precision, intent(out)     :: totalWidth      !< total width of water surface
                                                        !> type of total area computation, possible values:\n
                                                        !! CS_TYPE_PREISMAN  Ordinary total area computation, with possible Preisman lock on top\n
                                                        !! CS_TYPE_PLUS      Total area for only the expanding part of the cross section (Nested Newton method)\n
                                                        !! CS_TYPE_MIN       Total area for only the narrowing part of the cross section (Nested Newton method)
   integer, intent(in)               :: calculationOption 
   logical, intent(in), optional     :: doSummerDike    !< Switch to calculate Summer Dikes or not
   logical, intent(inout)            :: hysteresis!< Switch to calculate Summer Dikes or not
   ! Local Variables
   type(t_CSType), pointer           :: crossDef
   double precision                  :: wetperimeter
   double precision                  :: maxwidth        !< Maximal width for wet area
   double precision                  :: wlev            !< water level at cross section
   logical                           :: getSummerDikes
   double precision                  :: af_sub(3), perim_sub(3)

   if (dpt < 0.0d0) then
      totalArea = 0.0d0
      totalWidth = sl
      return
   endif

   !call system_clock(countstart)
   crossDef => cross%tabdef

   select case(cross%crosstype)
      case (CS_TABULATED, CS_RECTANGLE)
         wlev = cross%bedlevel + dpt
         if (present(doSummerDike))then
            getSummerdikes = doSummerdike
         else
            getSummerDikes = .true.
         endif
         call TabulatedProfile(dpt, cross, .false., getSummerDikes, totalArea, totalWidth, maxwidth, wetPerimeter, af_sub, perim_sub, calculationOption, hysteresis)
      case (CS_CIRCLE)
         call CircleProfile(dpt, crossDef%diameter, totalArea, totalWidth, maxwidth, wetPerimeter, calculationOption)
      case (CS_EGG)
         !TODO:
         call EggProfile(dpt, crossDef%diameter, totalArea, totalWidth, wetPerimeter, calculationOption)
      case (CS_YZ_PROF)
         if (calculationOption == CS_TYPE_MIN) then
            ! YZ-profiles do not have declining cross sections
            totalArea = 0d0
            totalWidth = 0d0
            wetPerimeter = 0d0
         else
            
         ! Also in case a conveyance table is time dependent, the geometry remains constant, so no need for interpolation of time dependent conveyance tables required.
            call YZProfile(dpt, cross%convtab1, 1, totalArea, totalWidth, maxwidth, wetPerimeter)
            if (totalWidth < sl) then
               ! Assume a rectangular profile for widths < sl, in order to 
               ! prevent "no convergence" in the non-linear solver. 
               totalWidth= sl
               totalArea = sl*dpt
            endif
         endif         
      case default
         call SetMessage(LEVEL_ERROR, 'INTERNAL ERROR: Unknown type of cross section')
   end select
 
end subroutine GetCSParsTotalCross

!> Get area, width and perimeter for a tabulated profile
subroutine TabulatedProfile(dpt, cross, doFlow, getSummerDikes, area, width, maxWidth, perimeter, af_sub, perim_sub, calculationOption, hysteresis)

   use m_GlobalParameters

   implicit none

   double precision, intent(in)      :: dpt                 !< Water depth at cross section location
   type (t_CrossSection), intent(in) :: cross               !< Cross section
   logical, intent(in)               :: doFlow              !< True: Flow, otherwise Total
   logical, intent(in)               :: getSummerDikes      !< Use summerdike data
   double precision, intent(out)     :: width               !< Width at water surface
   double precision, intent(out)     :: maxwidth            !< Maximal width for wet surface area
   double precision, intent(out)     :: area                !< Wet area
   double precision, intent(out)     :: perimeter           !< Wet perimeter
   double precision, intent(out)     :: af_sub(3)           !< Wet area, split up to main, floodlain1 and floodplain2
   double precision, intent(out)     :: perim_sub(3)        !< Wet perimeter, split up to main, floodlain1 and floodplain2
   integer, intent(in)               :: calculationOption   !< Defines the calculation option for closed profiles: Preisman, Plus or min the latter two are used for Nested Newton
   logical, intent(inout)            :: hysteresis          !< Flag for hysteresis of summerdike

   ! local parameters
   type(t_CSType), pointer           :: crossDef
   type(t_summerdike), pointer       :: summerdike
  
   double precision  :: wlev
   double precision  :: sdArea
   double precision  :: sdWidth
   integer           :: section

   crossDef => cross%tabDef

   call GetTabulatedSizes(dpt, crossDef, doFlow, area, width, maxWidth, perimeter, af_sub, perim_sub, calculationOption)
   
   if (associated(crossDef%summerdike) .and. getSummerDikes .and. calculationOption /= CS_TYPE_MIN) then
   
      wlev = cross%bedlevel + dpt
      
      allocate(summerdike)
      summerdike%crestLevel = crossDef%summerdike%crestLevel + cross%shift
      summerdike%baseLevel  = crossDef%summerdike%baseLevel + cross%shift
      summerdike%flowArea   = crossDef%summerdike%flowArea
      summerdike%totalArea  = crossDef%summerdike%totalArea
      
      ! Summerdike Adjusting
      if (doFlow) then
      
         ! Get Summer Dike Flow Data
         call GetSummerDikeFlow(summerdike, wlev, sdArea, sdWidth)
      
      else
      
         ! Get Summer Dike Total Data   
         call GetSummerDikeTotal(summerdike, wlev, sdArea, sdWidth, hysteresis)
      
      endif
      
      deallocate(summerdike)
      summerdike => null()
      
      area  = area  + sdArea
      width = width + sdWidth
      maxwidth = maxwidth + sdWidth
      
      if (sdArea > 0d0) then
         do section = 3, 1, -1
            if (af_sub(section) > thresholdForSummerdike) then
               af_sub(section) = af_sub(section) + sdarea
               exit
            endif
         enddo
      endif
   endif
end subroutine TabulatedProfile

!> Get area, width and perimeter for a tabulated profile definition
subroutine GetTabSizesFromTables(dpt, pCSD, doFlow, area, width, perimeter, af_sub, perim_sub, calculationOption)

   use m_GlobalParameters

   implicit none

   double precision, intent(in)                 :: dpt                  !< Water depth at cross section location
   type (t_CSType), pointer, intent(in)         :: pCSD                 !< Cross section definition
   logical, intent(in)                          :: doFlow               !< True: Flow, otherwise Total
   double precision, intent(out)                :: width                !< Width at water surface
   double precision, intent(out)                :: area                 !< Wet area
   double precision, intent(out)                :: perimeter            !< Wet perimeter
   double precision, intent(out)                :: af_sub(3)            !< Wet area, split up to main, floodlain1 and floodplain2
   double precision, intent(out)                :: perim_sub(3)         !< Wet perimeter, split up to main, floodlain1 and floodplain2
   integer, intent(in)                          :: calculationOption    !< Defines the calculation option for closed profiles: Preisman, Plus or min the latter two are used for Nested Newton

   ! local parameters
   integer                                      :: levelsCount
   double precision, dimension(:), pointer      :: heights
   
   ! Pre-Calculated Table for Tabulated/River Profiles
   double precision, dimension(:,:), pointer    :: af_sub_tab        !< Flow Areas for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:,:), pointer    :: width_sub_tab     !< Width for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:,:), pointer    :: perim_sub_tab     !< Wetted Perimeter for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:), pointer      :: flowArea_tab      !< Flow Areas
   double precision, dimension(:), pointer      :: flowWidth_tab     !< Flow Widths
   double precision, dimension(:), pointer      :: wetPerimeter_tab  !< Wet Perimeters
   double precision, dimension(:), pointer      :: totalArea_tab     !< Total Areas
   double precision, dimension(:), pointer      :: totalWidth_tab    !< Total Widths
   double precision, dimension(:), pointer      :: area_min_tab      !< Area for a narrowing part of a cross section (Nested Newton)
   double precision, dimension(:), pointer      :: width_min_tab     !< Width for a narrowing part of a cross section (Nested Newton)
   
   integer, dimension(:), pointer               :: plainsLocation
  
   double precision  :: d1
   double precision  :: d2
   double precision  :: dTop
   double precision  :: eTop
   integer           :: ilev, isec
   
   double precision  :: factor
   double precision  :: area_plus
   double precision  :: a
   double precision  :: b
   
   levelsCount = pCSD%levelsCount
   heights => pCSD%height   
   dTop = heights(levelsCount) - heights(1)

   af_sub    = 0.0d0
   perim_sub = 0.0d0
   width     = 0.0d0
   area      = 0.0d0
   perimeter = 0.0d0

   if (doFlow) then
      
      af_sub_tab       => pCSD%af_sub
      width_sub_tab    => pCSD%width_sub
      perim_sub_tab    => pCSD%perim_sub
      flowArea_tab     => pCSD%flowArea
      flowWidth_tab    => pCSD%flowWidth
      wetPerimeter_tab => pCSD%wetPerimeter
      plainsLocation   => pCSD%plainsLocation

      if (dpt > dTop) then
      
         do isec = 1,3
            af_sub(isec) = af_sub_tab(isec, levelsCount) + (dpt - dTop) * width_sub_tab(isec, levelsCount)
            perim_sub(isec) =  perim_sub_tab(isec, levelsCount)
         enddo
         
         area      = flowArea_tab(levelsCount)
         perimeter = wetPerimeter_tab(levelsCount)
         width     = flowWidth_tab(levelsCount)
         eTop      = flowWidth_tab(levelsCount)

         if ((eTop > ThresholdForPreismannLock)) then  !hk: add only when this part is meant to carry water

            if (af_sub(3) > 0d0) then
               isec = 3
            elseif (af_sub(2) > 0d0) then
               isec = 2
            else 
               isec = 1
            endif
            perim_sub(isec) = perim_sub_tab(isec, levelsCount) + 2.0d0 * (dpt - dTop)

            area = area + (dpt - dTop) * eTop
            perimeter = perimeter + 2.0d0 * (dpt - dTop)
            
         endif
         
      else

         do ilev = 2, levelsCount
         
            d1 = heights(ilev - 1) - heights(1)
            d2 = heights(ilev) - heights(1)
            
            if (dpt == d2) then

               do isec = 1,3
                  af_sub(isec)    = af_sub_tab(isec, ilev)
                  perim_sub(isec) = perim_sub_tab(isec, ilev)
               enddo
               
               area = flowArea_tab(ilev)
               perimeter = wetPerimeter_tab(ilev)
               width = flowWidth_tab(ilev)
            
            elseif (dpt > d1 .and. dpt <= d2) then
               
               if ((d2 - d1) > 0.0d0) then
                  
                  factor = (dpt - d1) / (d2 - d1)
                  
                  do isec = 1,3
                     
                     if (plainsLocation(isec) == 0) cycle
                     
                     width = width_sub_tab(isec, ilev - 1) + (width_sub_tab(isec, ilev) - width_sub_tab(isec, ilev - 1)) * factor
                     
                     area_plus = (width_sub_tab(isec, ilev - 1) + width) * (dpt - d1) * 0.5d0
                     af_sub(isec)    = af_sub_tab(isec, ilev - 1) + area_plus
                     
                     a = width - width_sub_tab(isec, ilev - 1)
                     b = dpt - d1
                     perim_sub(isec) = perim_sub_tab(isec, ilev - 1) + 2.0d0 * sqrt(0.25d0 * a * a + b * b)
                     
                  enddo
                  
                  width = flowWidth_tab(ilev - 1) + (flowWidth_tab(ilev) - flowWidth_tab(ilev - 1)) * factor
                  
                  area_plus = (flowWidth_tab(ilev - 1) + width) * (dpt - d1) * 0.5d0
                  area      = flowArea_tab(ilev - 1) + area_plus
                  
                  a = width - flowWidth_tab(ilev - 1)
                  b = dpt - d1
                  perimeter = wetPerimeter_tab(ilev - 1) + 2.0d0 * sqrt(0.25d0 * a * a + b * b)
                  
               else
                  
                  do isec = 1,3
                     af_sub(isec)    = af_sub_tab(isec, ilev - 1)
                     perim_sub(isec) = perim_sub_tab(isec, ilev - 1)
                  enddo

                  area = flowArea_tab(ilev - 1)
                  perimeter = wetPerimeter_tab(ilev - 1)
                  width = flowWidth_tab(ilev - 1)
                  
               endif
            
            endif
            
         enddo
         
      endif
   
 
   else

      ! Calculation for Storage
      totalWidth_tab => pCSD%totalWidth
      totalArea_tab  => pCSD%totalArea
      area_min_tab   => pCSD%area_min
      width_min_tab  => pCSD%width_min
   
      if (dpt > dTop) then
         
         area  = totalArea_tab(levelsCount)
         width = totalWidth_tab(levelsCount)
         eTop  = totalWidth_tab(levelsCount)

         if ((eTop > ThresholdForPreismannLock)) then  !hk: add only when this part is meant to carry water
            area = area + (dpt - dTop) * eTop
         endif
         
      else

         do ilev = 2, levelsCount
         
            d1 = heights(ilev - 1) - heights(1)
            d2 = heights(ilev) - heights(1)
            
            if (dpt == d2) then

               if (.not. calculationOption == CS_TYPE_MIN) then               
                  area = totalArea_tab(ilev)
                  width = totalWidth_tab(ilev)
               else
                  area =area_min_tab(ilev)
                  width = width_min_tab(ilev)
               endif
            
            elseif (dpt > d1) then
               
               d1 = heights(ilev - 1) - heights(1)
               d2 = heights(ilev) - heights(1)
               
               if ((d2 - d1) > 0.0d0) then
                  
                  factor = (dpt - d1) / (d2 - d1)
                  
                  width = totalWidth_tab(ilev - 1) + (totalWidth_tab(ilev) - totalWidth_tab(ilev - 1)) * factor
                  area_plus = (totalWidth_tab(ilev - 1) + width) * (dpt - d1) * 0.5d0
                   
                  if (.not. calculationOption == CS_TYPE_MIN) then
                     area = totalArea_tab(ilev - 1) + area_plus
                  else
                     
                     if (width < totalWidth_tab(ilev - 1)) then                       
                        area = area_min_tab(ilev - 1) + area_plus
                     else
                        area = area_min_tab(ilev - 1)
                     endif
                     
                     width = width_min_tab(ilev - 1) + totalWidth_tab(ilev - 1) - width
                     
                  endif
                  
               else
                  
                  if (.not. calculationOption == CS_TYPE_MIN) then               
                     area = totalArea_tab(ilev - 1)
                     width = totalWidth_tab(ilev - 1)
                  else
                     area = area_min_tab(ilev - 1)
                     width = width_min_tab(ilev - 1)
                  endif
                  
               endif
            
            endif
            
         enddo
         
      endif

   endif
   
end subroutine GetTabSizesFromTables

!> Get area, width and perimeter from cross section definition.\n
!! This subroutine is called for either flow or total widths and areas (see logical DOFLOW).\n
!! For total areas the width is always larger than the Preisman slot width. Which is set in AddTabulatedCrossSection.\n
!! For flow area calculation the Preisman slot is not used. 
subroutine GetTabulatedSizes(dpt, crossDef, doFlow, area, width, maxwidth, perimeter, af_sub, perim_sub, calculationOption)

   use m_GlobalParameters

   implicit none

   double precision, intent(in)                 :: dpt                 !< Water depth at cross section location
   type (t_CSType), pointer, intent(in)         :: crossDef            !< Cross section definition
   logical, intent(in)                          :: doFlow              !< True: Flow, otherwise Total
   double precision, intent(out)                :: width               !< Width at water surface
   double precision, intent(out)                :: maxwidth            !< maximum width for wet area
   double precision, intent(out)                :: area                !< Wet area
   double precision, intent(out)                :: perimeter           !< Wet perimeter
   double precision, intent(out)                :: af_sub(3)           !< Wet area, split up to main, floodlain1 and floodplain2
   double precision, intent(out)                :: perim_sub(3)        !< Wet perimeter, split up to main, floodlain1 and floodplain2
   integer, intent(in)                          :: calculationOption   !< Defines the calculation option for closed profiles: Preisman, Plus or min the latter two are used for Nested Newton

   ! local parameters
   integer                                      :: levelsCount
   double precision, dimension(:), pointer      :: heights
   double precision, dimension(:), pointer      :: widths
   double precision, dimension(4)               :: widthplains
   double precision, dimension(3)               :: w_section
  
   double precision  :: d1
   double precision  :: d2
   double precision  :: dTop
   double precision  :: e1
   double precision  :: e2
   double precision  :: eTop
   double precision  :: wl
   double precision  :: areal
   double precision  :: wll
   integer           :: ilev, isec, istart
   double precision  :: width_min
   double precision  :: area_min
   double precision  :: widthl, fullwidth

   maxwidth = 0d0
   levelsCount = crossDef%levelsCount
   heights => crossDef%height
   if (doFlow) then
      if (crossDef%plains(1) == 0d0) then
         crossDef%plains(1) = huge(1d0)
         crossDef%plainslocation(1) = crossDef%levelsCount
      endif
      
      widths => crossDef%flowWidth
      
      widthplains(1) = 0d0
      do isec = 1, 3
         widthplains(isec+1) = widthplains(isec) + crossDef%plains(isec)
      enddo
   else
      widths => crossDef%TotalWidth
      widthplains(1) = 0d0
      widthplains(2:4) = 0.5d0*huge(1d0)
   endif

   area      = 0.0D0
   d2        = 0.0D0
   e2        = widths(1)
   wl        = 0d0
   area_min  = 0d0
   width_min = 0d0
   !
   istart = 1
   do isec = 1, 3
      af_sub(isec) = 0d0
      w_section(isec) = 0d0
      perim_sub(isec) = min(max(0d0, widths(1)-widthplains(isec)), widthplains(isec+1)-widthplains(isec))
      if (doflow) then
         levelsCount = crossDef%plainslocation(isec)
      elseif (isec>=2) then
         ! total width is only first section
         levelscount = 0
      endif
      if (levelsCount==0) then
         cycle
      endif
      
      ! ISTART starts at 1 and is set to levelscount at the end of this loop.
      ! LEVELSCOUNT is the location, where the cross section intersects with a transition from main to floodplain1 or from floodplain1 to floodplain2
      ! this transition is always present in the table
      !
      do ilev = istart, levelsCount-1
         ! D1 is the level at ILEV, D2 is the level at D+1
         ! WIDTHS(ilev) and WIDTHS(ilev+1) are the corresponding widths at D1 and D2
         ! E1 is the width for subsection isec at D1 (since ISTART is at the start of subsection ISEC, E1 is garanteed > 0)
         ! E2 is the width for subsection isec at D2 (since ISTART is at the start of subsection ISEC, E2 is garanteed > 0)
         
         d1 = heights(ilev) - heights(1)
         d2 = heights(ilev + 1) - heights(1)
         e1 = min(widths(ilev)-widthplains(isec), widthplains(isec+1)-widthplains(isec))
         maxwidth = max(maxwidth , widths(ilev))
         e2 = min(widths(ilev + 1)-widthplains(isec), widthplains(isec+1)-widthplains(isec))
         if (dpt > d2) then
            af_sub(isec)    = af_sub(isec) + 0.5d0 * (d2 - d1) * (e1 + e2)
            perim_sub(isec) = perim_sub(isec) + 2.0d0 * sqrt(0.25d0 * (e2 - e1)**2 + (d2 - d1)**2)
            
            ! calculate decreasing area:
            ! area = width_min*height subsection + 0.5 * decrease in width * height subsection
            ! width_min = width_min + decrease width
            area_min = area_min + (width_min + 0.5d0*max(e1-e2,0d0))*(d2-d1)
            width_min = width_min + max(e1-e2,0d0)
         elseif (dpt >= d1) then
            call trapez(dpt, d1, d2, e1, e2, areal, w_section(isec), wll)
            if (isec==1) then
               ! Calculate the full width (not corrected for the subsections) 
               call trapez(dpt, d1, d2, widths(ilev), widths(ilev+1), areal, fullwidth, wll)
               maxwidth = max(maxwidth, fullWidth)
            endif
            
            af_sub(isec) = af_sub(isec) + areal
            perim_sub(isec) = perim_sub(isec) + wll
            
            ! calculate decreasing area 
            ! area = width_min*(local water depth in subsection) + trapez(dpt and decrease in width)
            ! width_min = width_min + widthl (= decrease in width)
            if (e2 < e1) then
               call trapez(dpt, d1, d2, 0d0, e1-e2, areal, widthl, wll)
               area_min = area_min + areal + width_min*(dpt-d1)
               width_min = width_min + widthl
            endif
            exit
         endif
      enddo
   
      istart = levelsCount
      ! Extend above table if necessary (for the subsections this is also the case for a local extension of a subsection.
      ! To prevent double additions, exclude the wet perimeter. And add this part later
      dTop = heights(levelsCount) - heights(1)
      if (dpt > dTop) then
      
         eTop = min(widths(levelsCount)-widthplains(isec), widthplains(isec+1)-widthplains(isec))
   
         af_sub(isec) = af_sub(isec) + (dpt - dTop) * eTop
         area_min = area_min + (dpt - dTop) * width_min
         w_section(isec) = eTop
      endif
   enddo

   
   levelsCount = crossDef%levelsCount
   dTop = heights(levelsCount) - heights(1)
   if (dpt > dTop) then
      eTop = widths(levelsCount)
      if ((eTop > ThresholdForPreismannLock)) then
         if (af_sub(3) > 0d0) then
            isec = 3
         elseif (af_sub(2) > 0d0) then
            isec = 2
         else 
            isec = 1
         endif
         perim_sub(isec) = perim_sub(isec) + 2.0d0 * (dpt - dTop)  ! Here we take the wet perimeter into account
      endif
   endif

   area      = 0d0
   width     = 0d0   
   perimeter = 0d0
   do isec = 1, 3
      area      = area      + af_sub(isec)
      width     = width     + w_section(isec)
      perimeter = perimeter + perim_sub(isec)
   enddo
      
   maxwidth = max(maxwidth, width)
 
   select case (calculationOption)
   case(CS_TYPE_PLUS)
      area  = area  + area_min
      width = width + width_min
   case(CS_TYPE_MIN)
      area  = area_min
      width = width_min
   case default
      ! CS_TYPE_NORMAL and CS_TYPE_PREISMAN need no special treatment: Preissmann slot already handled in AddTabulatedCrossSection()
      continue   
   end select
   
   
end subroutine GetTabulatedSizes

!> Get area, width and perimeter from cross section definition
subroutine GetTabFlowSectionFromTables(dpt, pCross, isector, area, width, perimeter)

   use m_GlobalParameters

   implicit none

   double precision, intent(in)                 :: dpt          !< Water depth
   type (t_CrossSection), pointer, intent(in)   :: pCross       !< Pointer to cross section
   integer, intent(in)                          :: isector      !< Section number
   double precision, intent(out)                :: width        !< Width at water surface
   double precision, intent(out)                :: area         !< Wet area
   double precision, intent(out)                :: perimeter    !< Wet perimeter

   ! local parameters
   integer                                      :: levelsCount
   double precision, dimension(:), pointer      :: heights
   integer, dimension(:), pointer               :: plainsLocation                              
   type(t_CSType), pointer                      :: pCSD
   
   ! Pre-Calculated Table for Tabulated/River Profiles
   double precision, dimension(:,:), pointer    :: af_sub_tab        !< Flow Areas for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:,:), pointer    :: width_sub_tab     !< Width for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:,:), pointer    :: perim_sub_tab     !< Wetted Perimeter for Sub-Sections (Main, FP1 and FP2)
   double precision, dimension(:), pointer      :: wetPerimeter_tab  !< Wet Perimeters
   
  
   double precision  :: d1
   double precision  :: d2
   double precision  :: dTop
   double precision  :: eTop
   integer           :: ilev
   
   double precision  :: factor
   double precision  :: area_plus
   double precision  :: a
   double precision  :: b
   
   pCSD => pCross%tabDef
   
   levelsCount    =  pCSD%levelsCount
   heights        => pCSD%height
   plainsLocation => pCSD%plainsLocation
   dTop           =  heights(levelsCount) - heights(1)

   width     = 0.0d0
   area      = 0.0d0
   perimeter = 0.0d0

   af_sub_tab       => pCSD%af_sub
   width_sub_tab    => pCSD%width_sub
   perim_sub_tab    => pCSD%perim_sub
   wetPerimeter_tab => pCSD%wetPerimeter

   if (dpt > dTop) then
      
      width     =  width_sub_tab(isector, levelsCount)
         
      eTop = pCSD%flowWidth(levelsCount)

      if ((eTop > ThresholdForPreismannLock)) then  !hk: add only when this part is meant to carry water

         area = af_sub_tab(isector, levelsCount) + (dpt - dTop) * width_sub_tab(isector, levelsCount)

         if (levelsCount <= plainsLocation(isector)) then
            perimeter = perim_sub_tab(isector, levelsCount) + 2.0d0 * (dpt - dTop)
         else
            perimeter = perim_sub_tab(isector, levelsCount)
         endif

      else
         
         area      = af_sub_tab(isector, levelsCount)
         perimeter =  perim_sub_tab(isector, levelsCount)
         
      endif
         
   else

      do ilev = 2, levelsCount
         
         d1 = heights(ilev - 1) - heights(1)
         d2 = heights(ilev) - heights(1)
            
         if (dpt == d2) then

            area      = af_sub_tab(isector, ilev)
            perimeter = perim_sub_tab(isector, ilev)
            width     = width_sub_tab(isector, ilev)
               
         elseif (dpt > d1 .and. dpt <= d2) then
               
            if ((d2 - d1) > 0.0d0) then
                  
               if (plainsLocation(isector) > 0)then
                     
                  factor = (dpt - d1) / (d2 - d1)
               
                  width = width_sub_tab(isector, ilev - 1) + (width_sub_tab(isector, ilev) - width_sub_tab(isector, ilev - 1)) * factor
                     
                  area_plus = (width_sub_tab(isector, ilev - 1) + width) * (dpt - d1) * 0.5d0
                  area      = af_sub_tab(isector, ilev - 1) + area_plus
                  
                  if (ilev <= plainsLocation(isector)) then
                     a = width - width_sub_tab(isector, ilev - 1)
                     b = dpt - d1
                     perimeter = perim_sub_tab(isector, ilev - 1) + 2.0d0 * sqrt(0.25d0 * a * a + b * b)
                  else
                     perimeter = perim_sub_tab(isector, ilev - 1)
                  endif
                  
               else
                  area      = 0.0d0
                  perimeter = 0.0d0
                  width     = 0.0d0
               endif
                  
            else
                  
               area      = af_sub_tab(isector, ilev - 1)
               perimeter = perim_sub_tab(isector, ilev - 1)
               width     = width_sub_tab(isector, ilev - 1)

            endif
            
         endif
            
      enddo
         
   endif
   
end subroutine GetTabFlowSectionFromTables

!> Get flow parameters for summerdike
subroutine GetSummerDikeFlow(summerdike, wlev, sdArea, sdWidth)

   implicit none

   type(t_summerdike), pointer, intent(in)      :: summerdike     !< summerdike data (note: baseLevel and crestlevel should already be absolute levels)
   double precision, intent(in)                 :: wlev           !< water level at cross section
   double precision, intent(out)                :: sdArea         !< area for summerdike
   double precision, intent(out)                :: sdWidth        !< width for summerdike

   ! Local Parameters
   double precision                             :: sdtr
   double precision                             :: aflw
   double precision                             :: dflw
   double precision                             :: htop
   double precision                             :: hbas
   
   if (.not. associated(summerdike)) then
      sdWidth = 0.0d0
      sdArea  = 0.0d0
      return
   endif
   
   sdtr = summerDikeTransitionHeight
   
   aflw = summerdike%flowArea
   
   if (aflw > 0.0d0) then
   
      dflw = aflw / sdtr
      
      htop = summerdike%crestLevel
      hbas = summerdike%baseLevel
      
      if (wlev > (htop + sdtr)) then
         sdWidth = 0.0
         sdArea  = aflw
      elseif (wlev > (htop + 0.5d0 * sdtr)) then
         sdWidth = (htop + sdtr - wlev) * 4.0d0 * dflw / sdtr
         sdArea  = aflw - 0.5d0 * (htop + sdtr - wlev) * sdWidth
      elseif (wlev > htop) then
         sdWidth = (wlev - htop) * 4.0d0 * dflw / sdtr
         sdArea = 0.5d0 * (wlev - htop) * sdWidth
      else
         sdWidth = 0.0d0
         sdArea  = 0.0d0
      endif
   else
      sdWidth = 0.0d0
      sdArea  = 0.0d0
   endif
          
end subroutine GetSummerDikeFlow

!> Get total parameers for summerdike
subroutine GetSummerDikeTotal(summerdike, wlev, sdArea, sdWidth, hysteresis)

   implicit none

   type(t_summerdike), pointer                  :: summerdike      !< summerdike data (note: baseLevel and crestlevel should already be absolute levels)
   double precision, intent(in)                 :: wlev            !< water level at cross section
   double precision, intent(out)                :: sdArea          !< area for summerdike
   double precision, intent(out)                :: sdWidth         !< width for summerdike
   logical, intent(inout)                       :: hysteresis      !< hysteresis parameter for rising or falling water

   ! Local Parameters
   double precision                             :: sdtr
   double precision                             :: atot
   double precision                             :: htop
   double precision                             :: hbas
   double precision                             :: dtot

   if (.not. associated(summerdike)) then
      sdWidth = 0.0d0
      sdArea  = 0.0d0
      return
   endif
   
   sdtr = summerDikeTransitionHeight
   
   atot       = summerdike%totalArea
   
   if (atot > 0.0d0) then
   
      htop = summerdike%crestLevel
      hbas = summerdike%baseLevel
      
      if (wlev >= (htop + sdtr)) then
         hysteresis = .false.
      elseif (wlev <= hbas) then
         hysteresis = .true.
      else
      endif

      
      if (hysteresis) then
      
         dtot = atot / sdtr
         
         if (wlev > (htop + sdtr)) then
            sdWidth = 0.0d0
            sdArea  = atot
         elseif (wlev > (htop + 0.5d0 * sdtr)) then
            sdWidth = (htop + sdtr - wlev) * 4.0d0 * dtot / sdtr 
            sdArea  = atot - 0.5d0 * (htop + sdtr - wlev) * sdWidth
         elseif (wlev>htop) then
            sdWidth = (wlev - htop) * 4.0d0 * dtot / sdtr
            sdArea  = 0.5d0 * (wlev - htop) * sdWidth
         else
            sdWidth = 0.0d0
            sdArea  = 0.0d0
         endif
         
      else
      
         dtot = atot / (htop + sdtr - hbas)
         
         if (wlev > (htop + sdtr)) then
            sdWidth = 0.0d0
            sdArea  = atot
         elseif (wlev > (hbas + 0.5d0 * (htop + sdtr - hbas))) then
            sdWidth = (htop + sdtr - wlev) * 4.0d0 * dtot / (htop + sdtr - hbas)
            sdArea  = atot - 0.5d0*(htop + sdtr - wlev) * sdWidth
         elseif (wlev > hbas) then
            sdWidth = (wlev - hbas) * 4.0d0 * dtot / (htop + sdtr - hbas)
            sdArea  = 0.5d0 * (wlev - hbas) * sdWidth
         else
            sdWidth = 0.0d0
            sdArea  = 0.0d0
         endif
         
      endif
      
   else
      sdWidth = 0.0d0
      sdArea  = 0.0d0
   endif
   
end subroutine GetSummerDikeTotal

!> calculate flow area and width for given trapezium
subroutine trapez(dpt, d1, d2, w1, w2, area, width, perimeter)
   implicit none

   double precision, intent(in)     :: dpt
   double precision, intent(in)     :: d1
   double precision, intent(in)     :: d2
   double precision, intent(in)     :: w1
   double precision, intent(in)     :: w2
   double precision, intent(out)    :: area
   double precision, intent(out)    :: width
   double precision, intent(out)    :: perimeter
   !
   width= w1 + (dpt - d1) * (w2 - w1) / (d2 - d1)
   area = (dpt - d1) * 0.5d0 * (width + w1)
   perimeter = 2.0d0 * sqrt(0.25d0 * (width - w1)**2 + (dpt - d1)**2)
   
end subroutine trapez

!> Calculate area, width and perimeter for a circle type cross section.\n
!! CALCULATIONOPTION determines the type of calculation, possibilities are:\n
!! * CS_TYPE_NORMAL   : the (flow) area and (flow) width is calculated. In this case the Preisman slot is not taken into account.\n
!! * CS_TYPE_PREISMAN : The (total) area and (flow) width is calculated, including a Preisman slot at the top, the bottomwidth is also set to the Preisman slot width.
!!   The latter is to assure A1 > 0.
!! * CS_TYPE_PLUS     : See CS_TYPE_PREISMAN, with non-decreasing width
!! * CS_TYPE_MIN      : non-increasing width
subroutine CircleProfile(dpt, diameter, area, width, maxwidth, perimeter, calculationOption)
   use m_GlobalParameters

   implicit none

   double precision, intent(in)        :: dpt                !< water depth
   double precision, intent(in)        :: diameter           !< diameter of circle
   double precision, intent(out)       :: width              !< width at given water depth
   double precision, intent(out)       :: area               !< wet area
   double precision, intent(out)       :: perimeter          !< wet perimeter
   double precision, intent(out)       :: maxwidth           !< Maximal width for wet surface area
   integer, intent(in)                 :: calculationOption  !< calculation option for cross section

!
! Local variables
!
   double precision               :: fi
   double precision               :: ra
   double precision               :: sq
   double precision               :: area_plus
   double precision               :: width_plus
   double precision               :: dpt2
   integer :: i
   
!
!! executable statements -------------------------------------------------------
!
   !
   !
   !
   if (diameter > 0d0) then
      
      ra = 0.5*diameter

      ! normal circle profile
      do i = 1, 2
         if (i==1) then
            dpt2 = min(dpt, ra)  ! first step only increasing part.
         else
            dpt2 = dpt              ! second step full egg profile up to water depth
         endif
         fi = acos(max((ra-dpt2)/ra, -1d0))
         sq = sqrt(max(dpt2*(diameter - dpt2),0d0))
      
         if (dpt2<diameter) then
            area      = abs(fi*ra*ra - sq*(ra-dpt2))
            perimeter       = abs(2d0*fi*ra)
            width     = 2d0*sq
         else
            area      = pi*ra*ra
            perimeter       = 2d0*pi*ra
            width     = 0d0
         endif
      
         if (i==1) then
            area_plus = area + width*(dpt-dpt2)
            width_plus = width
         endif
      
      enddo
   else
      area = 0d0
      area_plus = 0d0
      perimeter = sl
      width = sl
      width_plus = sl
   endif
   
   
   select case(calculationOption)
   case(CS_TYPE_NORMAL)
      area = area 
      width = width 
   case(CS_TYPE_PREISMAN)
      area = area    + sl*max(dpt-diameter, 0d0)
      width = max(width, sl)
   case(CS_TYPE_PLUS)
      area      = area_plus
      width     = max(width_plus, sl)
   case(CS_TYPE_MIN)
      area      = area    + sl*max(dpt-diameter, 0d0)
      width = max(width, sl)
      width_plus = max(width_plus, sl)
      
      area      = area_plus - area
      width     = width_plus - width
   end select
   maxwidth = width_plus
   
end subroutine CircleProfile

!> Calculate area, width and perimeter for a circle type cross section 
subroutine EggProfile(dpt, diameter, area, width, perimeter, calculationOption)
   use m_GlobalParameters
   use precision_basics

   implicit none

   double precision, intent(in)        :: dpt                !< water depth
   double precision, intent(in)        :: diameter           !< diameter of circle
   double precision, intent(out)       :: width              !< width at given water depth
   double precision, intent(out)       :: area               !< wet area
   double precision, intent(out)       :: perimeter          !< wet perimeter
   integer, intent(in)                 :: calculationOption  !< calculation option for cross section

   double precision                    :: r
   double precision                    :: dpt2
   double precision                    :: e
   double precision                    :: area_plus
   double precision                    :: width_plus
   integer :: i

   r = 0.5*diameter
   do i = 1, 2
      if (i==1) then
         dpt2 = min(dpt, 2d0*r)  ! first step only increasing part.
      else
         dpt2 = dpt              ! second step full egg profile up to water depth
      endif
      
      if ((dpt2>0) .and. (dpt2<=.2*r)) then
         perimeter = 2*r*(.5*atan((sqrt(r*dpt2 - dpt2*dpt2)/(.5*r - dpt2))))
         width = r*sin(atan(sqrt(dpt2*r - dpt2*dpt2)/(.5*r - dpt2)))
         e = .25*r*r*atan((dpt2 - .5*r)/(sqrt(dpt2*r - dpt2*dpt2))) + .392699082*r*r +       &
            & (dpt2 - .5*r)*sqrt(dpt2*r - dpt2*dpt2)
         area = e

      elseif ((dpt2>.2*r) .and. (dpt2<=2.*r)) then
         perimeter = 2*r*(2.39415093538065 -                                          &
               & 3.*atan((2*r - dpt2)/(sqrt(5.*r*r + 4*r*dpt2 - dpt2*dpt2))))
         width = 2.*((sqrt(5.*r*r + 4.*r*dpt2 - dpt2*dpt2)) - 2.*r)
         e = 9.*r*r*atan((dpt2 - 2*r)/(sqrt(9.*r*r - ((dpt2 - 2*r)**2)))) + (dpt2 - 2*r)&
            & *sqrt(9*r*r - ((dpt2 - 2*r)**2)) - 4.*r*(dpt2 - .2*r)                   &
            & + 10.11150997913956*r*r
         area = .11182380480168*r*r + e

      elseif ((dpt2>2*r) .and. (comparereal(dpt2,3*r, 1d-6) < 0)) then
         perimeter = 2.*r*(2.39415093538065 +                                         &
               & atan((dpt2 - 2*r)/(sqrt( - 3.*r*r + 4*r*dpt2 - dpt2*dpt2))))
         width = 2*r*cos(atan((dpt2 - 2*r)/(sqrt( - 3.*r*r + 4*dpt2*r - dpt2*dpt2))))
         e = r*r*(atan((dpt2 - 2*r)/(sqrt(r*r - ((dpt2 - 2*r)**2))))) + (dpt2 - 2*r)   &
               & *sqrt(r*r - ((dpt2 - 2*r)**2))
         area = 3.02333378394124*r*r + e

      else 
         perimeter = 7.92989452435109*r
         width = 0d0
         area = 4.59413011073614*r*r 
      endif
      
      if (i==1) then
         area_plus = area + width*max(0d0, dpt - 2d0*r)
         width_plus = width
      endif
      
   enddo
   
   select case(calculationOption)
   case(CS_TYPE_NORMAL)
      area = area 
      width = width 
   case(CS_TYPE_PREISMAN)
      area = area    + sl*dpt
      width = width  + sl
   case(CS_TYPE_PLUS)
      area      = area_plus + dpt*sl
      width     = width_plus + sl
   case(CS_TYPE_MIN)
      area      = area_plus - area
      width     = width_plus - width
   end select
   
end subroutine EggProfile

!> Calculate area, width and perimeter for a circle type cross section 
subroutine YZProfile(dpt, convtab, i012, area, width, maxwidth, perimeter, u1, cz, conv, frictionType, frictionValue)
   use m_GlobalParameters
   use m_Roughness
   implicit none

   double precision, intent(in)              :: dpt            !< Water depth
   integer,          intent(in)              :: i012           !< 0: use u point, 1: use water level point 1, 2: use water level point 2
   type(t_convtab),     intent(inout)        :: convtab        !< Conveyance table
   double precision, intent(out)             :: width          !< Width at given water depth
   double precision, intent(out)             :: maxwidth       !< Maximum width for wetted area
   double precision, intent(out)             :: area           !< Wet area
   double precision, intent(out)             :: perimeter      !< Wet perimeter
   double precision, intent(in)   , optional :: u1             !< Flow velocity
   double precision, intent(inout), optional :: cz             !< Chezy value, when a lumped definition would be used
   double precision, intent(out),   optional :: conv           !< Calculated conveyance 
   integer,          intent(out),   optional :: frictionType   !< friction type
   double precision, intent(out),   optional :: frictionValue  !< friction value
   
! locals
integer            :: nr, i1, i2, i
double precision   :: a1, a2, c1, c2, z1, z2, hu1, hu2, hh1, hh2, dh1, dh2
double precision   :: c_a, c_b !< variables related to extrapolation
                              !! above defined profile

double precision   :: r3, ar1, ar2

maxwidth = 0d0
if (convtab%last_position == 0) then
   width = 0d0
   area  = 0d0
else if (i012 == 0) then                                ! look at u points, mom. eq.

   nr = convtab%levelscount                                    ! number of table entries
   i  = convtab%last_position                                ! last index found

   do while ( i + 1 < nr .and. convtab%height(i+1) < dpt ) ! look up, imax = nr - 1
      i = i + 1
   enddo

   do while ( i     > 1  .and. convtab%height(i)   > dpt ) ! look down, imin = 1
      i = i - 1   
   enddo
   convtab%iolu = i                                    ! and store last index found

   do i1 = 1, i
      maxwidth = max(maxwidth, convtab%flow_width(i1))
   enddo
   
   i1  = i                                            ! so i1, i2 always inside table
   i2  = i+1
   hu2 = convtab%height(i2) ; dh2 = hu2 - dpt
   if (dpt .LE. convtab%height(i2) ) then !  .and. convtab%jopen .eq. 0) then     ! weightfactors. If profile closed no extrapolation
      hu1 = convtab%height(i1) ; dh1 = dpt - hu1
      a1  = dh2 / ( hu2-hu1)                          ! eis parser: hu = wel monotoon stijgend
      a2 = 1d0 - a1
      !
      c1    = convtab%perimeter(i1) ; c2 = convtab%perimeter(i2)
      perimeter = a1*c1 + a2*c2
      !
      c1    = convtab%flow_width(i1) ; c2 = convtab%flow_width(i2)
      width = a1*c1 + a2*c2
      maxwidth = max(maxwidth, width)
      !
      z1    = convtab%flow_area(i1) ; z2 = convtab%flow_area(i2)
      ar1   = 0.5d0*dh1*(c1 + width)                     ! area above i1
      ar2   = 0.5d0*dh2*(c2 + width)                     ! area below i2
      area  = a1*(z1+ar1)   + a2*(z2-ar2)
      !
      if (present(conv)) then
         !
         z1 = convtab%chezy(i1)
         z2 = convtab%chezy(i2)   ! positive flow direction
         if (convtab%conveyType==CS_VERT_SEGM) then
            c1 = convtab%conveyance(i1)
            c2 = convtab%conveyance(i2)
         endif
         !
         if (convtab%conveyType==CS_LUMPED) then
            cz = getchezy(frictionType, frictionValue, area/perimeter, dpt, 1d0)
            conv = (cz)*area*sqrt(area/perimeter)
         elseif (convtab%conveyType==CS_VERT_SEGM) then
            conv  = a1*c1 + a2*c2
         endif
      
         if (area == 0d0) then
            r3 = 0d0
            convtab%chezy_act = 0d0
         else
            r3 = area / perimeter                            ! actual hydraulic radius
            convtab%chezy_act = conv / (area * sqrt(r3))  ! Used in function ChezyFromConveyance
         endif
         cz = convtab%chezy_act
         !
      endif
      
   ELSE                                               ! when above profile
!(*)!  document SOBEK-21942: Change of roughness formulations in "Y-Z" and
   ! "Asymetrical Trapezium" profiles, Author:     Thieu van Mierlo
   !                                   Programmer: Daniel Abel
      WIDTH = convtab%flow_width (i2)
      perimeter = convtab%perimeter (i2)
      AREA  = convtab%flow_area (i2)
      if (present(conv)) then
         CONV  = convtab%conveyance(i2)
      endif
      
      !
      if ( convtab%jopen .eq. 1) then ! for open profiles add extra conveyance
         AR2   = WIDTH*(DPT-HU2)
         AREA  = AREA + AR2
         perimeter = perimeter + 2*(DPT-HU2)
         if (present(conv)) then
            r3    = AREA/perimeter                        ! actual hydraulic radius

            if (convtab%conveyType==CS_VERT_SEGM) then
               c_b = convtab%b_extr
               c_a = convtab%a_extr
               !
               conv = c_a*((dpt)**(c_b))
               ! Actual Chezy Value for ChezyFromConveyance
               convtab%chezy_act = conv / (AREA * sqrt(r3))
               cz = convtab%chezy_act
            else
               cz = getchezy(frictionType, frictionValue, area/perimeter, dpt, 0d0)
               conv = cz*area*sqrt(area/perimeter)
            endif
         endif
      endif

   endif

else                                                      ! look at left or right h, cont. eq.

   nr = convtab%levelscount                                  ! number of entries
   i  = convtab%last_position                                  ! last found

   do while ( i + 1 < nr .and. convtab%height(i+1) < dpt ) ! look up
      i = i + 1
   enddo

   do while ( i     > 1  .and. convtab%height(i) > dpt ) ! look down
      i = i - 1
   enddo
   convtab%last_position = i

   i1 = i                                                  ! so i1, i2 always inside table
   i2 = i+1
   hh2 = convtab%height(i2) ; dh2 = hh2 - dpt
   if (i2 .eq. nr .and. dpt .ge. convtab%height(i2) ) then ! Weightfactors. If profile closed no extrapolation
      a1 = 0d0 ; dh1 = 0
   else
      hh1 = convtab%height(i1) ; dh1 = dpt - hh1
      a1  = dh2 / ( hh2-hh1 )                              ! parser: hh = wel monotoon stijgend
   endif
   a2 = 1d0 - a1

   c1    = convtab%total_width(i1) ; c2 = convtab%total_width(i2)
   z1    = convtab%total_area(i1) ; z2 = convtab%total_area(i2)

   width = a1*c1 + a2*c2

   ar1   = 0.5d0*dh1*(c1 + width)                          ! area above i1
   ar2   = 0.5d0*dh2*(c2 + width)                          ! area below i2
   area  = a1*(z1+ar1)   + a2*(z2-ar2)

endif

end subroutine YZProfile

!> Generate conveyance table. This subroutine should only be called once at initialisation and 
!! during the simulation as soon as a new update of the friction values is performed.
subroutine CalcConveyance(crs)

use M_newcross
   !use m_parseConveyance
   !use modelGlobalData
   !use convTables
use messageHandling

   implicit none

   type(t_CrossSection), intent(inout)    :: crs   !< cross section
   
   type(t_convtab), pointer   :: convTab
   integer                 :: i
   allocate(convtab)

   ! Check if type is not equal to walLawNikuradse (type=2), since this option is not implemented yet
   do i = 1, crs%frictionSectionsCount
      if (crs%frictionTypePos(i) == 2 .or. crs%frictionTypeNeg(i) == 2 ) then
         ! TODO: make this error message obsolete
         msgbuf = 'Friction type wallLawNikuradse is not (yet) implemented for vertically segmented conveyance, see cross section: '//trim(crs%csid)
         call err_flush()
      endif
   enddo

   if (associated(crs%tabDef)) then
      call generateConvtab(convtab, crs%tabDef%levelsCount, crs%csid, crs%tabdef%y, crs%tabdef%z,     &
                        crs%tabDef%segmentToSectionIndex, crs%frictionTypePos,              &
                        crs%frictionValuePos)
      
      convTab%conveyType = crs%tabDef%conveyanceType
   end if

   convTab%last_position = 1

   if (associated(crs%convtab1)) then
      if (associated(crs%convTab2)) then
         call dealloc(crs%convTab1)
         crs%convTab1 => crs%convTab2
      endif
      crs%convTab2 => convTab
   else
      crs%convTab1 => convTab
   endif
      
end subroutine CalcConveyance

!> Get the highest level for the two cross sections and interpolate, using weighing factor F
double precision function getHighest1dLevelInterpolate(c1, c2, f)
   type (t_CrossSection), intent(in)     :: c1      !< cross section definition
   type (t_CrossSection), intent(in)     :: c2      !< cross section definition
   double precision, intent(in)          :: f       !< cross = (1-f)*cross1 + f*cross2
   
   double precision level1, level2
   
   level1 = getHighest1dLevel(c1)
   level2 = getHighest1dLevel(c2)
   
   getHighest1dLevelInterpolate = (1.0d0 - f) * level1 + f * level2
   
end function getHighest1dLevelInterpolate
   
!> Get the highest level for the cross section
double precision function getHighest1dLevelSingle(cross)

   type (t_CrossSection), intent(in)     :: cross      !< cross section
   
   integer levelsCount
   
   select case(cross%crosstype)
      case (CS_TABULATED)
         getHighest1dLevelSingle = cross%tabdef%height(cross%tabdef%levelscount) +cross%shift
      case (CS_CIRCLE)
         getHighest1dLevelSingle = cross%tabdef%diameter + cross%bedlevel
      case (CS_EGG)
         getHighest1dLevelSingle = 1.5d0 * cross%tabdef%diameter + cross%bedlevel
      case (CS_YZ_PROF)
         levelsCount = cross%convtab1%levelscount
         getHighest1dLevelSingle = cross%convtab1%height(levelsCount) + cross%bedlevel
      case default
         call SetMessage(LEVEL_ERROR, 'INTERNAL ERROR: Unknown type of cross-section in getHighest1dLevelSingle')
   end select
 
end function getHighest1dLevelSingle

!> set characteristic height and width of the cross section
subroutine SetCharHeightWidth(cross)

type(t_CrossSection), intent(inout)       :: cross        !< Cross section

   ! Code
   select case(cross%crosstype)
      case (CS_TABULATED)
            cross%charHeight = maxval(cross%tabDef%height) - minval(cross%tabDef%height)
            cross%charWidth  = maxval(cross%tabDef%flowWidth)
      case (CS_CIRCLE)
            cross%charHeight = cross%tabDef%diameter
            cross%charWidth  = cross%tabDef%diameter
      case (CS_EGG)
            cross%charHeight = 1.5d0 * cross%tabDef%diameter
            cross%charWidth  = cross%tabDef%diameter
      case (CS_YZ_PROF)
            cross%charHeight = maxval(cross%tabDef%z) - minval(cross%tabDef%z)
            cross%charWidth  = maxval(cross%tabDef%y) - minval(cross%tabDef%y)
      case default
         call SetMessage(LEVEL_ERROR, 'INTERNAL ERROR: Unknown type of cross-section in SetCharHeightWidth')
   end select
   
end subroutine SetCharHeightWidth

!> Returns a Copy from the given CrossSection
type(t_CrossSection) function CopyCross(CrossFrom)

   type(t_CrossSection)      :: CrossFrom   !< Cross section

   CopyCross%crossType          = CrossFrom%crossType
   CopyCross%branchid           = CrossFrom%branchid
   CopyCross%chainage           = CrossFrom%chainage
   CopyCross%bedLevel           = CrossFrom%bedLevel
   CopyCross%shift              = CrossFrom%shift
   CopyCross%surfaceLevel       = CrossFrom%surfaceLevel
   CopyCross%charHeight         = CrossFrom%charHeight
   CopyCross%charWidth          = CrossFrom%charWidth
   CopyCross%closed             = CrossFrom%closed
   CopyCross%groundFrictionType = CrossFrom%groundFrictionType
   CopyCross%groundFriction     = CrossFrom%groundFriction
   CopyCross%iTabDef            = CrossFrom%iTabDef
   CopyCross%csid               = CrossFrom%csid
   
   if (associated(CrossFrom%tabDef)) then
      allocate(CopyCross%tabDef)
      CopyCross%tabDef = CopyCrossDef(CrossFrom%tabDef)
   endif

   if (associated(CrossFrom%convTab1)) then
      allocate(CopyCross%convTab1)
      CopyCross%convTab1 = CopyCrossConv(CrossFrom%convTab1)
   endif
   if (associated(CrossFrom%convTab2)) then
      allocate(CopyCross%convTab2)
      CopyCross%convTab2 = CopyCrossConv(CrossFrom%convTab2)
   endif

   end function CopyCross

!> Returns a Copy from the given CrossSection Definition
type(t_CSType) function CopyCrossDef(CrossDefFrom)

   type(t_CSType) :: CrossDefFrom             !< cross section definition
   
   CopyCrossDef%crossType   = CrossDefFrom%crossType
   CopyCrossDef%levelsCount = CrossDefFrom%levelsCount

   !*** data for tabulated cross sections ***
   if (allocated(CrossDefFrom%height)) then
      allocate(CopyCrossDef%height(CopyCrossDef%levelsCount))
      CopyCrossDef%height = CrossDefFrom%height
   endif
   if (allocated(CrossDefFrom%flowWidth)) then
      allocate(CopyCrossDef%flowWidth(CopyCrossDef%levelsCount))
      CopyCrossDef%flowWidth = CrossDefFrom%flowWidth
   endif
   if (allocated(CrossDefFrom%totalWidth)) then
      allocate(CopyCrossDef%totalWidth(CopyCrossDef%levelsCount))
      CopyCrossDef%totalWidth = CrossDefFrom%totalWidth
   endif
   
   if (allocated(CrossDefFrom%af_sub)) then
      allocate(CopyCrossDef%af_sub(3, CopyCrossDef%levelsCount))
      CopyCrossDef%af_sub = CrossDefFrom%af_sub
   endif
   if (allocated(CrossDefFrom%width_sub)) then
      allocate(CopyCrossDef%width_sub(3, CopyCrossDef%levelsCount))
      CopyCrossDef%width_sub = CrossDefFrom%width_sub
   endif
   if (allocated(CrossDefFrom%perim_sub)) then
      allocate(CopyCrossDef%perim_sub(3, CopyCrossDef%levelsCount))
      CopyCrossDef%perim_sub = CrossDefFrom%perim_sub
   endif
   if (allocated(CrossDefFrom%flowArea)) then
      allocate(CopyCrossDef%flowArea(CopyCrossDef%levelsCount))
      CopyCrossDef%flowArea = CrossDefFrom%flowArea
   endif
   if (allocated(CrossDefFrom%wetPerimeter)) then
      allocate(CopyCrossDef%wetPerimeter(CopyCrossDef%levelsCount))
      CopyCrossDef%wetPerimeter = CrossDefFrom%wetPerimeter
   endif
   if (allocated(CrossDefFrom%totalArea)) then
      allocate(CopyCrossDef%totalArea(CopyCrossDef%levelsCount))
      CopyCrossDef%totalArea = CrossDefFrom%totalArea
   endif
   if (allocated(CrossDefFrom%area_min)) then
      allocate(CopyCrossDef%area_min(CopyCrossDef%levelsCount))
      CopyCrossDef%area_min = CrossDefFrom%area_min
   endif
   if (allocated(CrossDefFrom%width_min)) then
      allocate(CopyCrossDef%width_min(CopyCrossDef%levelsCount))
      CopyCrossDef%width_min = CrossDefFrom%width_min
   endif
   
   CopyCrossDef%plains       = CrossDefFrom%plains
   
   !*** data for yz cross sections
   if (allocated(CrossDefFrom%y)) then
      allocate(CopyCrossDef%y(CopyCrossDef%levelsCount))
      CopyCrossDef%y = CrossDefFrom%y
   endif
   if (allocated(CrossDefFrom%z)) then
      allocate(CopyCrossDef%z(CopyCrossDef%levelsCount))
      CopyCrossDef%z = CrossDefFrom%z
   endif
   
   CopyCrossDef%storageType     = CrossDefFrom%storageType
   CopyCrossDef%storLevelsCount = CrossDefFrom%storLevelsCount

   if (allocated(CrossDefFrom%storLevels)) then
      allocate(CopyCrossDef%storLevels(max(CopyCrossDef%storLevelsCount, 2)))
      CopyCrossDef%storLevels = CrossDefFrom%storLevels
   endif
   if (allocated(CrossDefFrom%YZstorage)) then
      allocate(CopyCrossDef%YZstorage(CopyCrossDef%storLevelsCount))
      CopyCrossDef%YZstorage = CrossDefFrom%YZstorage
   endif

   CopyCrossDef%frictionSectionsCount = CrossDefFrom%frictionSectionsCount
   if (CopyCrossDef%frictionSectionsCount > 0) then
   
      allocate (CopyCrossDef%frictionSectionID(CopyCrossDef%frictionSectionsCount))
      allocate (CopyCrossDef%frictionSectionFrom(CopyCrossDef%frictionSectionsCount))
      allocate (CopyCrossDef%frictionSectionTo(CopyCrossDef%frictionSectionsCount))
      allocate (CopyCrossDef%frictionSectionIndex(CopyCrossDef%frictionSectionsCount))
   
      CopyCrossDef%frictionSectionID = CrossDefFrom%frictionSectionID
      CopyCrossDef%frictionSectionFrom = CrossDefFrom%frictionSectionFrom
      CopyCrossDef%frictionSectionTo   = CrossDefFrom%frictionSectionTo
     
   endif
   
   !** Circle and egg profile data
   CopyCrossDef%diameter = CrossDefFrom%diameter

   !--- information for Summerdikes
   if (associated(CrossDefFrom%summerdike)) then
      allocate(CopyCrossDef%summerdike)
      CopyCrossDef%summerdike%crestLevel = CrossDefFrom%summerdike%crestLevel
      CopyCrossDef%summerdike%baseLevel  = CrossDefFrom%summerdike%baseLevel
      CopyCrossDef%summerdike%flowArea   = CrossDefFrom%summerdike%flowArea
      CopyCrossDef%summerdike%totalArea  = CrossDefFrom%summerdike%totalArea
   endif
       
   !*** ground layer data
   allocate(CopyCrossDef%groundlayer)
   CopyCrossDef%groundlayer%used      = CrossDefFrom%groundlayer%used
   CopyCrossDef%groundlayer%thickness = CrossDefFrom%groundlayer%thickness
   CopyCrossDef%groundlayer%area      = CrossDefFrom%groundlayer%area
   CopyCrossDef%groundlayer%perimeter = CrossDefFrom%groundlayer%perimeter
   CopyCrossDef%groundlayer%width     = CrossDefFrom%groundlayer%width
end function CopyCrossDef

!> Returns a Copy from the given CrossSection Conveyance
!! DEALLOCATE this Copy after Use!!!!!!!!!!!!!!!!!!!!!!!!!!
type(t_convtab) function CopyCrossConv(CrossConvFrom)


   type(t_convtab) :: CrossConvFrom        !< conveyance table
   
   CopyCrossConv%jopen      = CrossConvFrom%jopen
   CopyCrossConv%iolu       = CrossConvFrom%iolu
   CopyCrossConv%conveyType = CrossConvFrom%conveyType

   CopyCrossConv%a_extr = CrossConvFrom%a_extr
   CopyCrossConv%b_extr = CrossConvFrom%b_extr

   CopyCrossConv%last_position = CrossConvFrom%last_position
   CopyCrossConv%bedlevel      = CrossConvFrom%bedlevel
      
   CopyCrossConv%chezy_act  = CrossConvFrom%chezy_act
      
   CopyCrossConv%levelscount        = CrossConvFrom%levelscount

   if (CrossConvFrom%levelscount > 0) then
      
      if (allocated(CrossConvFrom%height)) then
         allocate(CopyCrossConv%height(CrossConvFrom%levelscount))
         CopyCrossConv%height = CrossConvFrom%height
      endif
         
      if (allocated(CrossConvFrom%flow_area)) then
         allocate(CopyCrossConv%flow_area(CrossConvFrom%levelscount))
         CopyCrossConv%flow_area = CrossConvFrom%flow_area
      endif
         
      if (allocated(CrossConvFrom%flow_width)) then
         allocate(CopyCrossConv%flow_width(CrossConvFrom%levelscount))
         CopyCrossConv%flow_width = CrossConvFrom%flow_width
      endif
         
      if (allocated(CrossConvFrom%perimeter)) then
         allocate(CopyCrossConv%perimeter(CrossConvFrom%levelscount))
         CopyCrossConv%perimeter = CrossConvFrom%perimeter
      endif
         
      if (allocated(CrossConvFrom%conveyance)) then
         allocate(CopyCrossConv%conveyance(CrossConvFrom%levelscount))
         CopyCrossConv%conveyance = CrossConvFrom%conveyance
      endif
         
      if (allocated(CrossConvFrom%chezy)) then
         allocate(CopyCrossConv%chezy(CrossConvFrom%levelscount))
         CopyCrossConv%chezy = CrossConvFrom%chezy
      endif
         
      if (allocated(CrossConvFrom%height)) then
         allocate(CopyCrossConv%height(CrossConvFrom%levelscount))
         CopyCrossConv%height = CrossConvFrom%height
      endif
         
      if (allocated(CrossConvFrom%total_area)) then
         allocate(CopyCrossConv%total_area(CrossConvFrom%levelscount))
         CopyCrossConv%total_area = CrossConvFrom%total_area
      endif
         
      if (allocated(CrossConvFrom%total_width)) then
         allocate(CopyCrossConv%total_width(CrossConvFrom%levelscount))
         CopyCrossConv%total_width = CrossConvFrom%total_width
      endif
         
   endif

   
end function CopyCrossConv

!> Get the interpolated BOB between two cross sections  
double precision function getBobInterpolate(cross1, cross2, factor)

   type (t_CrossSection), intent(in)     :: cross1       !< cross section definition
   type (t_CrossSection), intent(in)     :: cross2       !< cross section definition
   double precision, intent(in)          :: factor       !< cross = (1 - f) * cross1 + f * cross2
   
   double precision bob1, bob2
   
   bob1 = getBobSingle(cross1)
   bob2 = getBobSingle(cross2)
   
   getBobInterpolate = (1d0 - factor) * bob1 + factor * bob2
   
end function getBobInterpolate
   
!> Get the BOB of a cross section
double precision function getBobSingle(cross)

   type (t_CrossSection), intent(in)     :: cross      !< cross section
   
   type(t_CSType), pointer               :: pCrossDef
   
   getBobSingle = cross%bedLevel

   pCrossDef => cross%tabDef
   if (associated(pCrossDef%groundlayer)) then
     if (pCrossDef%groundlayer%used) then
        getBobSingle = getBobSingle + pCrossDef%groundlayer%thickness
     endif
   endif
end function getBobSingle

!> Get the groundlayer thickness
double precision function getGroundLayer(cross)

   type (t_CrossSection), intent(in)     :: cross      !< cross section
   
   type(t_CSType), pointer               :: pCrossDef
   
   pCrossDef => cross%tabDef
   if (associated(pCrossDef%groundlayer)) then
      if (pCrossDef%groundlayer%used) then
         getGroundLayer = pCrossDef%groundlayer%thickness
      else
         getGroundLayer = 0.0d0
      endif
   else
      getGroundLayer = 0.0d0
   endif

end function getGroundLayer

!> Compute the critical depth for a cross section
double precision function GetCriticalDepth(q, cross)

   implicit none

   type(t_crosssection)            :: cross !< cross section
   double precision, intent(in)    :: q     !< Discharge for which the critical depth should be computed.
   !
   ! Local variables
   !
   double precision               :: depth
   double precision               :: dummy
   double precision               :: wWidth
   double precision               :: groundLayer
   double precision               :: step
   double precision               :: wArea 
   logical                        :: first
   double precision, parameter    :: eps = 0.0001d0

   !! executable statements -------------------------------------------------------
   !
   !
   !     dlg, pjo, 1999-07-07
   !     Compute the critical depth according to the discharge through
   !     the culvert with an iteration loop
   !
   !     Include Pluvius data space
   !
   !     argument
   !     local variables
   !
   !     pjo, 13-04-2000, ars 4952
   !------------------------------------------------------------------------------------------
   ! Iterations, finds dtpc 
   !------------------------------------------------------------------------------------------
   groundLayer = getGroundLayer(cross)
   if (abs(q) < eps) then 
      depth = 0d0
      GetCriticalDepth = depth - groundLayer
      return
   endif
   
   depth  = cross%charHeight * 0.5d0
   step  = depth
   first = .true.

   do while (first .or. step > eps)
   
      first = .false.
      
      call GetCSParsFlow(cross, depth, wArea, dummy, wWidth)        
      wwidth = warea/depth
      
      step = 0.5d0 * step
      
      if ((q * q * wWidth) - (wArea * wArea * wArea * gravity) > 0.0d0) then 
         depth = depth + step
      else
         depth = depth - step
      endif
   
   enddo

   GetCriticalDepth = depth - groundLayer
    
end function GetCriticalDepth

!> Update cross section definition administration
subroutine admin_crs_def(definitions)
   
   type(t_CSDefinitionSet), intent(inout), target :: Definitions      !< Cross section definitions
      
   integer i
   character(len=idlen), dimension(:), pointer :: ids
      
   allocate(definitions%hashlist%id_list(definitions%Count))
   definitions%hashlist%id_count = definitions%Count
   ids => definitions%hashlist%id_list
      
   do i= 1, definitions%count
      ids(i) = definitions%CS(i)%id
   enddo
      
   call hashfill(definitions%hashlist)
      
end subroutine admin_crs_def
 
!> Fill the hashtable for the cross section definitions
subroutine fill_hashtable_csdef(definitions)
   
   type(t_CSDefinitionSet), intent(inout), target :: definitions          !< Cross section definitions
      
   integer i
   character(len=idlen), dimension(:), pointer :: ids
      
   allocate(definitions%hashlist%id_list(definitions%Count))
   definitions%hashlist%id_count = definitions%Count
   ids => definitions%hashlist%id_list
      
   do i= 1, definitions%count
      ids(i) = definitions%CS(i)%id
   enddo
      
   call hashfill(definitions%hashlist)

end subroutine fill_hashtable_csdef

!> Create predefined tables for speeding up the calculation of cross section data
subroutine createTablesForTabulatedProfile(crossDef)
   
   type(t_CStype), intent(inout) :: crossDef       !< cross section definition
   
   ! local parameters
   integer                                           :: levelsCount
   double precision, dimension(crossDef%levelsCount) :: heights
   double precision, dimension(crossDef%levelsCount) :: widths
   double precision, dimension(0:3)                  :: widthplains
  
   double precision  :: d1
   double precision  :: d2
   double precision  :: e1
   double precision  :: e2
   double precision  :: wl
   integer           :: ilev, isec

   crossDef%width_sub    = 0.0d0
   crossDef%af_sub       = 0.0d0
   crossDef%perim_sub    = 0.0d0
   crossDef%flowArea     = 0.0d0
   crossDef%wetPerimeter = 0.0d0

   crossDef%totalArea = 0.0d0
   crossDef%area_min  = 0.0d0
   crossDef%width_min = 0.0d0
   
   levelsCount = crossDef%levelsCount
   heights = crossDef%height   
   
   widthplains = 0d0

   if (crossDef%plains(1) == 0d0) then
      crossDef%plains(1) = huge(1d0)
      crossDef%plainsLocation(1) = crossDef%levelsCount
   endif
   widths = crossDef%flowWidth
   do isec = 1, 3
      widthplains(isec) = widthplains(isec-1) + crossDef%plains(isec)
   enddo
   
   d2 = 0.0D0
   e2 = widths(1)
   crossDef%af_sub = 0.0d0
   crossDef%perim_sub = 0.0d0
  
   do isec = 1, 3
      crossDef%af_sub(isec, 1) = 0d0
      crossDef%width_sub(isec, 1) = min(max(0d0, widths(1)-widthplains(isec-1)), widthplains(isec)-widthplains(isec-1))
      crossDef%perim_sub(isec, 1) = min(max(0d0, widths(1)-widthplains(isec-1)), widthplains(isec)-widthplains(isec-1))
      
      if (crossDef%plains(isec) <= 0.0d0) cycle

      do ilev = 2, levelsCount
      
         d1 = heights(ilev - 1) - heights(1)
         d2 = heights(ilev) - heights(1)
         e1 = min(max(0d0, widths(ilev - 1)-widthplains(isec-1)), widthplains(isec)-widthplains(isec-1))
         e2 = min(max(0d0, widths(ilev)-widthplains(isec-1)), widthplains(isec)-widthplains(isec-1))

         crossDef%width_sub(isec, ilev) = e2
         crossDef%af_sub(isec, ilev)    = crossDef%af_sub(isec, ilev - 1) + 0.5d0 * (d2 - d1) * (e1 + e2)
         
         if (crossDef%plainsLocation(2) > 0) then
            if (widths(ilev - 1) < widthplains(isec - 1)) then
               crossDef%perim_sub(isec, ilev) = 0.0d0
            elseif (widths(ilev - 1) >= widthplains(isec)) then 
               crossDef%perim_sub(isec, ilev) = crossDef%perim_sub(isec, ilev - 1)
            else
               crossDef%perim_sub(isec, ilev) = crossDef%perim_sub(isec, ilev - 1) + 2.0d0 * sqrt(0.25d0 * (e2 - e1)**2 + (d2 - d1)**2)
            endif
         else
            crossDef%perim_sub(isec, ilev) = crossDef%perim_sub(isec, ilev - 1) + 2.0d0 * sqrt(0.25d0 * (e2 - e1)**2 + (d2 - d1)**2)
         endif
            
      enddo

   enddo
   
   ! Totalize
   do isec = 1, 3
      do ilev = 1, crossDef%levelsCount
         crossDef%flowArea(ilev)  = crossDef%flowArea(ilev)  + crossDef%af_sub(isec, ilev)
         crossDef%wetPerimeter(ilev) = crossDef%wetPerimeter(ilev) + crossDef%perim_sub(isec, ilev)
      enddo
   enddo
      

   ! Calculation of Total Area
   levelsCount = crossDef%levelsCount
   heights = crossDef%height   
   widths = crossDef%totalWidth
   
   wl = widths(1)
   d2 = 0.0D0
   e2 = widths(1)
   
   crossDef%totalArea = 0.0d0
   crossDef%area_min  = 0.0d0
   crossDef%width_min = 0.0d0
   
   do ilev = 2, levelsCount
   
      d1 = heights(ilev - 1) - heights(1)
      d2 = heights(ilev) - heights(1)
      e1 = widths(ilev - 1)
      e2 = widths(ilev)

      crossDef%totalArea(ilev) = crossDef%totalArea(ilev - 1) + 0.5d0 * (d2 - d1) * (e1 + e2)
      
      crossDef%area_min(ilev)  = crossDef%area_min(ilev - 1) + (crossDef%width_min(ilev - 1) + 0.5d0 * max(e1- e2, 0.0d0)) * (d2 - d1)
      crossDef%width_min(ilev) = crossDef%width_min(ilev - 1) + max(e1 - e2, 0.0d0)

   enddo
   
   end subroutine createTablesForTabulatedProfile

   !> write cross section data to dia file
   subroutine write_crosssection_data(crs, brs)
      use M_newcross
   
      type(t_CrossSectionSet), intent(in)    :: crs      !< cross sections
      type(t_branchSet), intent(in)          :: brs      !< branches
      
      integer                       :: icross
      integer                       :: n
      integer                       :: count
      type(t_CrossSection), pointer :: cross
      character(len=20)             :: cstype
      
      count = crs%count
      do icross = 1, count
         cross => crs%cross(icross)
         msgbuf = '  '
         call msg_flush()      
         write(msgbuf, '(137a1)') ('=', n=1,137)
         call msg_flush()

         msgbuf = 'Id                   = '// trim(cross%csid)
         call msg_flush()
         select case(cross%crossType)
         case(CS_TABULATED) 
            cstype = 'tabulated'
         case(CS_CIRCLE   ) 
            cstype = 'circle'
         case(CS_EGG      )
            cstype = 'egg'
         case(CS_YZ_PROF  ) 
            cstype = 'yz'
         case default
            cstype = '***UNKNOWN***'
         end select
         msgbuf = 'Type                 = '// trim(cstype)
         call msg_flush()
         msgbuf = 'Branch id            = '// trim(brs%branch(cross%branchid)%id)
         call msg_flush()
         write(msgbuf, '(''Chainage             = '', f14.2)') cross%chainage
         call msg_flush()
         write(msgbuf, '(''Bed level            = '', f14.2)') cross%bedlevel
         call msg_flush()
         
      enddo
          msgbuf = '  '
         call msg_flush()
         call msg_flush()
 
   
   end subroutine write_crosssection_data

   !> Get the flow area, wet perimeter and flow width located on a link
   subroutine getYZConveyance(line2cross, cross, dpt, u1, cz, conv, factor_time_interpolation)

      use m_GlobalParameters
   
      implicit none

      type(t_chainage2cross),       intent(in)        :: line2cross     !< cross section indirection
      type(t_CrossSection), target, intent(in)        :: cross(:)       !< array containing cross section information
      double precision,             intent(in)        :: dpt            !< water depth at cross section
      double precision,             intent(in)        :: u1             !< water velocity at cross section
      double precision,             intent(  out)     :: cz             !< computed Chezy value
      double precision,             intent(  out)     :: conv           !< conveyance
      double precision,             intent(in)        :: factor_time_interpolation    !< Factor for interpolation of time dependent conveyance tables
                                                                                       !< conveyance = (1-factor)*conv1 + factor*conv2

      type (t_CrossSection), pointer        :: cross1         !< cross section
      type (t_CrossSection), pointer        :: cross2         !< cross section
      double precision                      :: f              !< cross = (1-f)*cross1 + f*cross2
      double precision                      :: area 
      double precision                      :: width 
      double precision                      :: maxwidth
      double precision                      :: perimeter 
      double precision                      :: cz1, cz2
      double precision                      :: conv1, conv2, conv3
      

      cross1 => cross(line2cross%c1)
      cross2 => cross(line2cross%c2)
      f = line2cross%f

      if(cross1%crossIndx == cross2%crossIndx) then
         ! Same Cross-Section, no interpolation needed 
         call YZProfile(dpt, cross1%convTab1, 0, area, width, maxwidth, perimeter, u1, cz, conv, cross1%frictionTypePos(1), cross1%frictionValuePos(1))
         if (cross1%hasTimeDependentConveyance) then
            ! time interpolation is required, compute the conveyance, using the conveyance table, at the end of the interval
            call YZProfile(dpt, cross1%convTab2, 0, area, width, maxwidth, perimeter, u1, cz, conv1, cross1%frictionTypePos(1), cross1%frictionValuePos(1))
            ! Interpolate to the correct time level
            conv = (1d0-factor_time_interpolation) * conv + factor_time_interpolation * conv1
         endif
      else
         if (cross1%crosstype /= cross2%crosstype) then
            write(msgbuf, '(a,a,a,a,a)') 'getYZConveyance: cross sections ''', trim(cross1%csid), ''' and ''', trim(cross2%csid), &
               ''' are incompatible: cannot interpolate between YZ-cross section and non-YZ cross section.'
            call err_flush()
            return
         end if
         call YZProfile(dpt, cross1%convTab1, 0, area, width, maxwidth, perimeter, u1, cz1, conv1, cross1%frictionTypePos(1), cross1%frictionValuePos(1))
         if (cross1%hasTimeDependentConveyance) then
            ! time interpolation is required, compute the conveyance, using the conveyance table, at the end of the interval
            call YZProfile(dpt, cross1%convTab2, 0, area, width, maxwidth, perimeter, u1, cz1, conv3, cross1%frictionTypePos(1), cross1%frictionValuePos(1))
            conv1 = (1d0-factor_time_interpolation) * conv1 + factor_time_interpolation * conv3
         endif

         call YZProfile(dpt, cross2%convTab1, 0, area, width, maxwidth, perimeter, u1, cz2, conv2, cross2%frictionTypePos(1), cross2%frictionValuePos(1))
         if (cross2%hasTimeDependentConveyance) then
            ! time interpolation is required, compute the conveyance, using the conveyance table, at the end of the interval
            call YZProfile(dpt, cross2%convTab2, 0, area, width, maxwidth, perimeter, u1, cz2, conv3, cross2%frictionTypePos(1), cross2%frictionValuePos(1))
            conv2 = (1d0-factor_time_interpolation) * conv2 + factor_time_interpolation * conv3
         endif
         cz   = (1.0d0 - f) * cz1   + f * cz2
         conv = (1.0d0 - f) * conv1 + f * conv2
      endif

    end subroutine getYZConveyance

    !> retrieve the interpolated summerdike  data
    subroutine getSummerDikeData(line2cross, cross, crestLevel, baseLevel)
       use m_missing
       
       type(t_chainage2cross),intent(in)         :: line2cross     !< cross section indirection
       type(t_CrossSection), target, intent(in)  :: cross(:)       !< array containing cross section information
       double precision,      intent(  out)      :: crestLevel     !< Crest level of the summerdike at the given location
       double precision,      intent(  out)      :: baseLevel      !< Base level of the summerdike at the given location  

       double precision :: f
       type (t_CrossSection), pointer        :: cross1         !< cross section
       type (t_CrossSection), pointer        :: cross2         !< cross section

       cross1 => cross(line2cross%c1)
       cross2 => cross(line2cross%c2)
       f = line2cross%f

        if (cross1%hasSummerDike() .and. cross2%hasSummerDike()) then
          crestLevel = (1.0d0 - f) * (cross1%tabDef%summerdike%crestLevel + cross1%shift) + f * (cross2%tabDef%summerdike%crestLevel + cross2%shift)
          baseLevel  = (1.0d0 - f) * (cross1%tabDef%summerdike%baseLevel + cross1%shift)  + f * (cross2%tabDef%summerdike%baseLevel + cross2%shift)
       else if (cross1%hasSummerDike()) then
         crestLevel = cross1%tabDef%summerdike%crestLevel + cross1%shift
         baseLevel  = cross1%tabDef%summerdike%baseLevel  + cross1%shift 
       else if (cross2%hasSummerDike()) then
         crestLevel = cross2%tabDef%summerdike%crestLevel + cross2%shift
         baseLevel  = cross2%tabDef%summerdike%baseLevel  + cross2%shift
       else
         crestLevel = dmiss
         baseLevel  = dmiss
       endif

   end subroutine getSummerDikeData
   
end module m_CrossSections

