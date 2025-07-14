module m_Storage
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

   public get_volume
   public get_surface
   public fill_hashtable
   public get_top_level

   interface get_volume
      module procedure get_volume_by_stor_node
   end interface get_volume

   interface get_surface
      module procedure get_surface_by_stor_node
   end interface get_surface

   interface fill_hashtable
      module procedure fill_hashtable_stor
   end interface

   interface realloc
      module procedure realloc_storage
   end interface

   interface dealloc
      module procedure dealloc_storage
   end interface dealloc

    !---------------------------------------------------------
  
   type, public :: t_storage
      character(len=idlen)    :: id                      !< unique id of storage area
      character(len=idlen)    :: node_id                 !< (optional) Node Id
      character(len=idlen)    :: name                    !< Long name in the user interface
      integer                 :: grid_point              !< gridpoint index
      integer                 :: node_index              !< connection node index
      integer                 :: branch_index            !< branch index
      double precision        :: chainage                !< location of the storage node w.r.t the start of the branch
      integer                 :: storage_type            !< type of storage on street\n
                                                         !! 0: no storage\n
                                                         !! 2: reservoir storage\n
                                                         !! 3: closed manhole
      type(t_table), pointer  :: storage_area            !< table containing storage area and levels
      type(t_table), pointer  :: street_area             !< table containing storage area and levels on street level
      logical                 :: use_street_storage      !< flag indicating whether streetstorage is to be used
      double precision        :: x                       !< (optional) x-coordinate
      double precision        :: y                       !< (optional) y-coordinate
      logical                 :: use_table               !< flag indicating whether table is to be used
      type(t_table), pointer  :: angle_loss     => null()!< lookup table that connects an angle to an energy loss coefficient
      double precision        :: entrance_loss  = 0d0    !< entrance loss coefficient
      double precision        :: exit_loss      = 0d0    !< exit loss coefficient
      double precision        :: expansion_loss = 0d0    !< loss coefficient for expansion or contraction
   end type
   
   type, public :: t_storage_set
      integer                                               :: Size = 0
      integer                                               :: grows_by = 2000
      integer                                               :: Count= 0
      integer                                               :: Count_xy = 0 ! Number of storage nodes that are defined by x-, y-coordinates
      integer                                               :: Count_closed = 0 ! Number of storage nodes with storageType "closed"
      type(t_storage), pointer, dimension(:)                :: stor
      type(t_hashlist)                                      :: hash_list
   end type t_storage_set
   
contains
    
   !> deallocate storage array
   subroutine dealloc_storage(stor_set)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storage_set)           :: stor_set
      
      ! Local variables
      integer                       :: i
   
      ! Program code
      if (stor_set%count > 0) then
         if (associated(stor_set%stor)) then
            do i = 1, stor_set%Count
               if (associated(stor_set%stor(i)%storage_area)) then
                  call dealloc(stor_set%stor(i)%storage_area)
                  stor_set%stor(i)%storage_area => null()
               endif
               if (associated(stor_set%stor(i)%street_area)) then
                  call dealloc(stor_set%stor(i)%street_area)
                  stor_set%stor(i)%street_area => null()
               endif
               if (associated(stor_set%stor(i)%angle_loss)) then
                  call dealloc(stor_set%stor(i)%angle_loss)
                  stor_set%stor(i)%angle_loss => null()
               endif
            enddo
            deallocate(stor_set%stor)
         endif
      endif
      stor_set%stor => null()
      stor_set%Size  = 0
      stor_set%Count = 0
      call dealloc(stor_set%hash_list)

   end subroutine
!
   !> Resize storage array.  
   subroutine realloc_storage(stor_set)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_storage_set), intent(inout)         :: stor_set
      
      ! Local variables
      type(t_storage), pointer, dimension(:)     :: old_stor_set
      
      ! Program code
      
      if (stor_set%Size > 0) then
         old_stor_set=>stor_set%stor
      endif
      
      if (stor_set%grows_by <=0) then
         stor_set%grows_by = 200
      endif
      allocate(stor_set%stor(stor_set%Size+stor_set%grows_by))
      
      if (stor_set%Size > 0) then
         stor_set%stor(1:stor_set%Size) = old_stor_set(1:stor_set%Size)
         deallocate(old_stor_set)
      endif
      stor_set%Size = stor_set%Size+stor_set%grows_by
   end subroutine
   
   !> Retrieve the surface area, using the storage node.
   double precision function get_surface_by_stor_node(storage, level) result(res)
      ! Modules
   
      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      if (storage%use_street_storage .and. (.not. storage%use_table)) then
         ! check if water level is above street level
         if (level >= storage%street_area%x(1) ) then
            res = interpolate(storage%street_area, level)
            ! finished
            return
         endif
      endif
      ! else : calculate well storage:
      if (storage%storage_type /= nt_None .and. level >= storage%storage_area%x(1)-1d-4 ) then
         res = interpolate(storage%storage_area, level)
      else
         res = 0d0
      endif
      
   end function get_surface_by_stor_node

   !> Retrieve the volume, using the storage node.
   double precision function get_volume_by_stor_node(storage, level) result(res)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_storage), intent(in)            :: storage
      double precision, intent(in)           :: level

      ! Local variables
      double precision  :: level2         !< level2 is the level to which the normal storageArea must be calculated, which is the street level or the actual water level
   
      ! Program code
      !         Check if storage on street is to be calculated:
   
      if (storage%storage_type /= nt_none) then
         level2 = level
         res = 0d0
         if (storage%use_street_storage .and. (.not. storage%use_table)) then
            ! check if water level is above street level
            if (level > storage%street_area%x(1)) then
               res= integrate(storage%street_area, level)
               level2 = storage%street_area%x(1)
               ! finished
            endif
         endif
         
         ! else : calculate well storage:
         res = res + integrate(storage%storage_area, level2)
      else 
         res = 0d0
      endif
   end function get_volume_by_stor_node

   !> Get the top level of the storage node
   double precision function get_top_level(storage) result(res)
      type(t_storage), intent(in)            :: storage
      if (storage%use_street_storage .and. (.not. storage%use_table)) then
         res = storage%street_area%x(storage%street_area%length)
      else
         res = storage%storage_area%x(storage%storage_area%length)
      endif
   end function get_top_level
   
   !> Fill the hash table
   subroutine fill_hashtable_stor(stor_set)
   
      type (t_storage_set), intent(inout), target  :: stor_set
      
      integer                                      :: ist
      character(len=idlen), dimension(:), pointer  :: ids
      
      allocate(stor_set%hash_list%id_list(stor_set%Count))
      stor_set%hash_list%id_count = stor_set%Count
      ids => stor_set%hash_list%id_list
      
      do ist = 1, stor_set%count
         ids(ist) = stor_set%stor(ist)%id
      enddo
      
      call hashfill(stor_set%hash_list)
      
   end subroutine fill_hashtable_stor
   
end module m_Storage
