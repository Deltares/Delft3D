module m_Storage
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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
   use m_tables
   use m_hash_search

   implicit none
   
   private 
     
   ! storage types
   integer, public, parameter :: nt_None        = 0
   integer, public, parameter :: nt_Reservoir   = 2
   integer, public, parameter :: nt_Closed      = 3
   double precision, public, parameter:: slot_area = 1d-2 ! value to be used for storageStreetArea when storageType is "closed"
   
   public realloc
   public dealloc

   public getVolume
   public getSurface
   public fill_hashtable
   public getTopLevel

   interface GetVolume
      module procedure GetVolumeByStorNode
   end interface GetVolume
   
   interface GetSurface
      module procedure GetSurfaceByStorNode
   end interface GetSurface

   interface fill_hashtable
      module procedure fill_hashtable_sto
   end interface 

   interface realloc
      module procedure reallocStorage
   end interface

   interface dealloc
      module procedure deallocStorage
   end interface dealloc

    !---------------------------------------------------------
  
   type, public :: t_storage
      character(len=idlen)    :: id                      !< unique id of storage area
      character(len=idlen)    :: nodeId                  !< (optional) Node Id
      character(len=idlen)    :: name                    !< Long name in the user interface
      integer                 :: gridPoint               !< gridpoint index
      integer                 :: node_index              !< connection node index
      integer                 :: branch_index            !< branch index
      double precision        :: chainage                !< location of the storage node w.r.t the start of the branch
      integer                 :: storageType             !< type of storage on street\n
                                                         !! 0: no storage\n
                                                         !! 2: reservoir storage\n
                                                         !! 3: closed manhole
      type(t_table), pointer  :: storageArea             !< table containing storage area and levels
      type(t_table), pointer  :: streetArea              !< table containing storage area and levels on street level
      logical                 :: useStreetStorage        !< flag indicating whether streetstorage is to be used
      double precision        :: x                       !< (optional) x-coordinate
      double precision        :: y                       !< (optional) y-cooridnate
      logical                 :: useTable                !< flag indicating whether table is to be used
   end type
   
   type, public :: t_storageSet
      integer                                               :: Size = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0
      integer                                               :: Count_xy = 0 ! Number of storage nodes that are defined by x-, y-coordinates
      integer                                               :: Count_closed = 0 ! Number of storage nodes with storageType "closed"
      type(t_storage), pointer, dimension(:)                :: stor
      type(t_hashlist)                                      :: hashlist
   end type t_storageSet
   
contains
    
   !> deallocate storage array
   subroutine deallocStorage(storS)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storageSet)            :: storS
      
      ! Local variables
      integer                       :: i
   
      ! Program code
      if (storS%count > 0) then
         if (associated(storS%stor)) then 
            do i = 1, storS%Count
               if (associated(storS%stor(i)%storageArea)) then
                  call dealloc(storS%stor(i)%storageArea)
                  storS%stor(i)%storageArea => null()
               endif
               if (associated(storS%stor(i)%streetArea)) then
                  call dealloc(storS%stor(i)%streetArea)
                  storS%stor(i)%streetArea => null()
               endif
            enddo
            deallocate(storS%stor)
         endif
      endif
      storS%stor => null()
      storS%Size  = 0
      storS%Count = 0
      call dealloc(storS%hashlist)

   end subroutine
!
   !> Resize storage array.  
   subroutine reallocStorage(storS)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storageSet), intent(inout)          :: storS
      
      ! Local variables
      type(t_storage), pointer, dimension(:)     :: oldStorS
      
      ! Program code
      
      if (storS%Size > 0) then
         oldStorS=>storS%stor
      endif
      
      if (storS%growsBy <=0) then
         storS%growsBy = 200
      endif
      allocate(storS%stor(storS%Size+storS%growsBy))
      
      if (storS%Size > 0) then
         storS%stor(1:storS%Size) = oldstorS(1:storS%Size)
         deallocate(oldstorS)
      endif
      storS%Size = storS%Size+storS%growsBy
   end subroutine
   
   !> Retrieve the surface area, using the storage node.
   double precision function getSurfaceByStorNode(storage, level)
      ! Modules
   
      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      if (storage%useStreetStorage .and. (.not. storage%useTable)) then
         ! check if water level is above street level
         if (level >= storage%streetArea%x(1) ) then
            getSurfaceByStorNode = interpolate(storage%streetArea, level)
            ! finished
            return
         endif
      endif
      ! else : calculate well storage:
      if (storage%storageType /= nt_None .and. level >= storage%storageArea%x(1)-1d-4 ) then
         getSurfaceByStorNode = interpolate(storage%storageArea, level)
      else
         getSurfaceByStorNode = 0d0
      endif
      
   end function getSurfaceByStorNode

   !> Retrieve the volume, using the storage node.
   double precision function getVolumeByStorNode(storage, level)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      ! Local variables
      double precision  :: level2         !< level2 is the level to which the normal storageArea must be calculated, which is the street level or the actual water level
   
      ! Program code
      !         Check if storage on street is to be calculated:
   
      if (storage%storagetype/=nt_none) then
         level2 = level
         getVolumeByStorNode = 0d0
         if (storage%useStreetStorage .and. (.not. storage%useTable)) then
            ! check if water level is above street level
            if (level > storage%streetArea%x(1)) then
               getVolumeByStorNode= integrate(storage%streetArea, level)
               level2 = storage%streetArea%x(1)
               ! finished
            endif
         endif
         
         ! else : calculate well storage:
         getVolumeByStorNode = getVolumeByStorNode+ integrate(storage%storageArea, level2)
      else 
         getVolumeByStorNode = 0d0
      endif
   end function getVolumeByStorNode

   !> Get the top level of the storage node
   double precision function getTopLevel(storage)
      type(t_storage), intent(in)            :: storage
      if (storage%useStreetStorage .and. (.not. storage%useTable)) then
         getTopLevel = storage%streetArea%x(storage%streetArea%length)
      else
         getTopLevel = storage%storageArea%x(storage%storageArea%length)
      endif
   end function getTopLevel
   
   !> Fill the hash table
   subroutine fill_hashtable_sto(storS)
   
      type (t_storageSet), intent(inout), target   :: storS
      
      integer                                      :: ist
      character(len=idlen), dimension(:), pointer  :: ids
      
      allocate(storS%hashlist%id_list(storS%Count))
      storS%hashlist%id_count = storS%Count
      ids => storS%hashlist%id_list
      
      do ist = 1, storS%count
         ids(ist) = storS%stor(ist)%id
      enddo
      
      call hashfill(storS%hashlist)
      
   end subroutine fill_hashtable_sto

   
end module m_Storage
