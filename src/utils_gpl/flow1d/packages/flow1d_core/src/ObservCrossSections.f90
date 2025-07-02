!> Define observation cross-sections
module m_ObservCrossSections
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
   use M_newcross
   use m_branch
   use m_GlobalParameters
   use m_hash_search
   use string_module

   implicit none

   private

   public realloc
   public dealloc


   interface realloc
      module procedure reallocObservCrossSections
   end interface realloc
   
   !> Free the memory of observation cross-sections
   interface dealloc
      module procedure deallocObservCrossSections
   end interface dealloc

   !> Derived type for defining one observation cross-section type
   type, public :: t_ObservCrossSection
      character(IdLen)              :: name             !< name
         
      character(IdLen)              :: branchid         !< branch id
      double precision              :: chainage         !< offset in meters along branch
      integer                       :: branchIdx = 0    !< branch index
      integer                       :: numValues        !< (optional) number of values in xy-coordinates
      double precision, allocatable :: x(:)             !< (optional) x-coordinates of crosssection line. (size=numValues)
      double precision, allocatable :: y(:)             !< (optional) y-coordinates of crosssection line. (size=numValues)
   end type t_ObservCrossSection

   !> Derived type to store the observation cross-section set
   type, public :: t_ObservCrossSectionSet
      integer                                                :: maxNumberOfConnections=0 !< maximum nr of connections to a node
      integer                                                :: Size = 0                 !< Actual size of observation cross-section set
      integer                                                :: growsBy = 2000           !< Increment for observation cross-section set
      integer                                                :: Count= 0                 !< Actual number of observation cross-section sets
      type(t_ObservCrossSection), pointer, dimension(:)      :: Observcross => null()    !< Current observation cross-section
      
      type(t_hashlist)                                       :: hashlist
   end type t_ObservCrossSectionSet

contains

!> Free the memory used by a cross-section definition
subroutine deallocObservCrossSections(ObservCrossSet)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_ObservCrossSectionSet)                           :: ObservCrossSet !< Current observation cross-section

   

   ! Program code
   if (associated(ObservCrossSet%ObservCross)) then
     
      deallocate(ObservCrossSet%ObservCross)
      ObservCrossSet%ObservCross =>null()
      ObservCrossSet%size     = 0
      ObservCrossSet%count    = 0
   endif
end subroutine deallocObservCrossSections


!> Increase the memory used by a observation cross-section
subroutine reallocObservCrossSections(ObservCrossSet)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_ObservCrossSectionSet)        :: ObservCrossSet!< Current observation cross-section
   integer                   :: ierr

   ! Local variables
   ! Local variables
   type(t_ObservCrossSection), pointer, dimension(:)    :: oldObservs

   ! Program code

   if (ObservCrossSet%Size > 0) then
      oldObservs=>ObservCrossSet%ObservCross
   endif

   if (ObservCrossSet%growsBy <=0) then
      ObservCrossSet%growsBy = 200
   endif
   allocate(ObservCrossSet%ObservCross(ObservCrossSet%Size+ObservCrossSet%growsBy),stat=ierr)
   call aerr('ObservCrossSet%ObservCross(ObservCrossSet%Size+ObservCrossSet%growsBy)',ierr,ObservCrossSet%Size+ObservCrossSet%growsBy)

   if (ObservCrossSet%size > 0) then
      ObservCrossSet%ObservCross(1:ObservCrossSet%size) = oldObservs(1:ObservCrossSet%size)
      deallocate(oldObservs)
   endif
   ObservCrossSet%Size = ObservCrossSet%Size+ObservCrossSet%growsBy
end subroutine reallocObservCrossSections

end module m_ObservCrossSections
