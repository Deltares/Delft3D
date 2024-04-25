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
module dhydrology_reader_module

   use m_ec_module

   implicit none

   public:: dhydrologyReadConfiguration
   type(tEcInstance), pointer, save            :: ecInstancePtr =>null()
   integer, parameter                          :: nameLenght = 128

   type ec_module_query
      character(len=nameLenght)                :: name
      character(len=nameLenght)                :: testpathname = '.'         ! chdir to this directory when started
      character(len=nameLenght)                :: quantityname
      character(len=nameLenght)                :: locationname
      character(len=nameLenght)                :: refFilename
      character(len=nameLenght)                :: outFilename
      character(len=nameLenght)                :: inFilename
      character(len=nameLenght)                :: forcingfile = ''
      character(len=nameLenght)                :: varname = ''
      integer                                  :: inFiletype
      integer                                  :: method
      integer                                  :: npoint, ntimes
      real(hp), dimension(:), allocatable      :: x,y,t
      real(hp), dimension(:), allocatable      :: x2,y2
      real(hp)                                 :: dtnodal
      integer                                  :: quantitynr = 1
      integer                                  :: operand
      integer                                  :: vectormax
      integer                                  :: tgt_refdate
      real(kind=hp)                            :: tgt_tzone
      integer                                  :: tgt_tunit
      logical                                  :: jasferic = .False.
      real(kind=hp)                            :: missing_value = ec_undef_hp
      integer                                  :: targetItem = ec_undef_int
      real(kind=hp), dimension(:), allocatable :: resultVector
      real(kind=hp), dimension(:), allocatable :: referenceVector
      real(kind=hp)                            :: tolerance
      integer                                  :: ndxstart = -1
      integer                                  :: ndxend= -1
      logical                                  :: useBcBlockApproach = .false.

   end type ec_module_query

   type dhydrologyConfigurationFile
      !> [framework]
      character(len=nameLenght)                        :: configurationFileName
      character(len=nameLenght)                        :: netcdfInputFileName
   end type dhydrologyConfigurationFile

   ! static instances
   type(dhydrologyConfigurationFile)                  :: configurationFile

   contains

   !< reads entries of a dhydrology configuration file
   function dhydrologyReadConfiguration(filename) result (ierr)

   use tree_data_types
   use messageHandling
   use tree_structures
   use properties

   implicit none

   integer                                 :: ierr
   character(len=nameLenght), intent(in)   :: filename
   type(tree_data), pointer                :: configPtr => null()
   logical                                 :: success
   character(len=nameLenght)               :: netcdfInputFileName

   ierr = - 1
   call tree_create(trim(filename), configPtr)
   call prop_inifile(filename , configPtr, ierr)

   if ( ierr.ne.0 ) then
      call mess(LEVEL_ERROR, 'Error opening file ', trim(filename), '.')
   endif
   configurationFile%configurationFileName = filename

   call prop_get_string(configPtr, 'framework', 'netcdfinput', netcdfInputFileName, success)
   configurationFile%netcdfInputFileName = netcdfInputFileName

   ! clean up tree
   call tree_destroy(configPtr)

   end function dhydrologyReadConfiguration
   !-----------------------------------------------------------------------------------!
   !< reads a map from a file and perform space and time interpolation
   function dhydrologyReadMap(query) result (ierr)

   use messageHandling
   use m_alloc
   use m_ec_module
   use m_ec_parameters

   implicit none

   type(tEcInstance), pointer, save      :: ecInstancePtr !< Local EC instance
   type(ec_module_query)                 :: query
   real(hp), dimension(:,:), allocatable :: xyen
   logical                               :: success
   integer                               :: ierr
   integer, dimension(:), allocatable    :: itemIDs
   integer                               :: ntgt, it
   integer                               :: result_size
   real(kind=hp), dimension(:),   allocatable :: targetArray

   ! Create and initialize EC instance
   if (.not.associated(ecInstancePtr)) then
      success = ecCreateInstance(ecInstancePtr)
   endif

   ecInstancePtr%coordsystem = EC_COORDS_CARTESIAN

   ! find out what the number of target items should be or from config
   ntgt = 1
   call realloc(itemIDs,ntgt)
   itemIDs = ec_undef_int

   ! UNST-3838: TODO: update this call to match ec_module's changed API (ec_module commit #63478).
   success = .false.
   ! success = ecModuleAddTimeSpaceRelation(ecInstancePtr, query%quantityname, &
   !    query%x, query%y, query%vectormax, &
   !    query%inFilename, query%inFiletype, &
   !    query%method, query%operand, &
   !    query%tgt_refdate, query%tgt_tzone, query%tgt_tunit, &
   !    query%jasferic, query%missing_value, itemIDs, varname = query%varname, dtnodal = query%dtnodal)

   if(.not.success) ierr = -1

   ! try to estimate the result size
   result_size = ecEstimateItemresultSize(ecInstancePtr, itemIDs(query%quantitynr))
   if (result_size>0) then
   else
      return
   endif
   call realloc(targetArray,result_size)
   if (query%ndxstart==-1) query%ndxstart = 1
   if (query%ndxend==-1) query%ndxend = result_size


   if (query%quantitynr<=size(itemIDs)) then
      if (query%useBcBlockApproach) then
         ! UNST-3838: TODO: update this call to match ec_module's changed API (ec_module commit #62379).
         success = .false.
         ! success = ecItemGetValues(ecInstancePtr, itemIDs(query%quantitynr),46800.d0, targetArray)
      else
         ! UNST-3838: TODO: update this call to match ec_module's changed API (ec_module commit #62379).
         success = .false.
         ! success = ec_gettimespacevalue_by_itemID(ecInstancePtr, itemIDs(query%quantitynr), 46800.d0, targetArray)
      endif
   endif

   end function dhydrologyReadMap

end module dhydrology_reader_module