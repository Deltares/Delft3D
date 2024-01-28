!> Define grid values on the grid (level, discharge, salinity, dispersion or Windshield)
module m_spatial_data
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

   use m_alloc
   use m_tables
   use MessageHandling
   use m_GlobalParameters
   implicit none
   
   private

   public realloc
   public dealloc
   public getValueAtLocation
   
   !> Realloc grid values set
   interface realloc
      module procedure reallocspatial_data
   end interface

   !> Dealloc grid values set
   interface dealloc
      module procedure deallocspatial_data
   end interface dealloc

   !> Derived type containg the grid values
   type, public:: t_spatial_data                               !< Derived type for spatial varying data values
      integer                         :: numValues             !< Array size
      integer                         :: def_type              !< Default type
                                                               
      integer, allocatable            :: brIndex(:)            !< Array containing indices to the branches for each input value (length NUMVALUES)
      double precision, allocatable   :: chainage(:)           !< Array containing chainage on the branch for each input value (length NUMVALUES)
      double precision, allocatable   :: valuesOnLocation(:)   !< Array containing input values (length NUMVALUES)
                                  
      double precision                :: default               !< Default or global value
      integer                         :: quantity              !< Name (integer type) of the quantity
      integer, allocatable            :: tblIndex(:)           !< Indirection table from grid point to table index
      double precision, allocatable   :: values(:)             !< Values of the quantity
      
      type(t_tableSet)                :: tables                !< tables for water level or discharge dependent values
      
   end type    

   !> Derived type containing the grid values set
   type, public   :: t_spatial_dataSet
      integer                                       :: size   = 0           !< current length of array quant
      integer                                       :: growsBy = 2000       !< used increment for extending array quant
      integer                                       :: count   = 0          !< number of registered quantial Conditions
      type(t_spatial_data), pointer, dimension(:)   :: quant   => null()            
   end type
      
   type, public :: t_ptable
      type(t_table), pointer :: p
   end type

contains

   !> Free the spatial_data set
   subroutine deallocspatial_data(spData)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet), intent(inout)          :: spData        !< Spatial data set

      ! Local variables
      integer                       :: i
      integer                       :: length
  
      ! Program code
      if (associated(spData%quant)) then
        length = size(spData%quant)
        do i = 1, length
           if (allocated(spData%quant(i)%values)) deallocate(spData%quant(i)%values)
           if (allocated(spData%quant(i)%brIndex)) deallocate(spData%quant(i)%brIndex)
           if (allocated(spData%quant(i)%chainage)) deallocate(spData%quant(i)%chainage)
           if (allocated(spData%quant(i)%valuesOnLocation)) deallocate(spData%quant(i)%valuesOnLocation)
           if (allocated(spData%quant(i)%tblIndex)) deallocate(spData%quant(i)%tblIndex)
           call dealloc(spData%quant(i)%tables)
           spData%quant(i)%numvalues = 0
        enddo   
        deallocate(spData%quant)
      endif
      spData%quant => null()
      spData%Size  = 0
      spData%Count = 0
   end subroutine
!
!  !> Realloc spatial_data 
   subroutine reallocspatial_data(spData)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet), intent(inout)          :: spData         !< Spatial data set
      
      ! Local variables
      type(t_spatial_data), pointer, dimension(:)     :: oldspData
      
      ! Program code
      
      if (spData%Size > 0) then
        oldspData=>spData%quant
      endif
      
      if (spData%growsBy <=0) then
        spData%growsBy = 200
      endif
      allocate(spData%quant(spData%Size+spData%growsBy))
      
      if (spData%Size > 0) then
        spData%quant(1:spData%Size) = oldspData(1:spData%Size)
        deallocate(oldspData)
      endif
      spData%Size = spData%Size+spData%growsBy
   end subroutine
 
   !> Get the function value at a given location (branchid, chainage)
   integer function getValueAtLocation(sp, branchidx, chainage, value, valuetype)
   
      ! Return Values: 0 = Value found at Location
      !                1 = Default Value
      !               -1 = Error, No Value determined
   
      type(t_spatial_data), intent(in)            :: sp         !< spatial data 
      integer, intent(in)                         :: branchidx  !< branch index
      double precision, intent(in)                :: chainage   !< chainage
      double precision, intent(out)               :: value      !< value
      integer, intent(out)                        :: valuetype  !< valuetype
      
      integer                 :: i
      integer                 :: icount
      double precision        :: chainages(2) = 0.0d0
      double precision        :: values(2)   = 0.0d0
      
      getValueAtLocation = -1
      value              = 0.0d0
      valuetype          = 0
      
      icount = 0
      do i = 1, sp%numvalues
         
         if (sp%brIndex(i) == branchidx) then
         
            if (icount == 0) then
               icount = 1
               chainages = sp%chainage(i)
               values   = sp%valuesOnLocation(i)
               if (chainages(1) >= chainage) exit
            else
               icount = icount + 1
               chainages(1) = chainages(2)
               values  (1) = values(2)
               chainages(2) = sp%chainage(i)
               values(2)   = sp%valuesOnLocation(i)
               if (chainages(2) >= chainage) exit
            endif   

         else
            if (icount > 0 ) exit
         endif
      
      enddo
      
      if (icount == 0) then
         
         ! No Data at Location found, use Default and Default Type
         value = sp%default
         valuetype = sp%def_type
         getValueAtLocation = 1
         
      elseif (icount == 1) then
      
         ! Only one Value Found on Branch, no interpolation needed
         value = values(1)
         getValueAtLocation = 0
      
      elseif (chainages(1) >= chainage) then
      
         ! chainage before first point
         value = values(1)
         getValueAtLocation = 0
      
      elseif (chainages(2) <= chainage) then
      
         ! chainage after last point
         value = values(2)
         getValueAtLocation = 0
      
      else
      
         ! Now we need interpolation
         if (chainages(2) > chainages(1)) then
            value = values(1) + (values(2) - values(1)) * (chainage - chainages(1)) / (chainages(2) - chainages(1))
         else
            ! Prevent zero devide
            value = (values(1) + values(2)) * 0.50d0
         endif
         
         getValueAtLocation = 0
         
      endif
      
   
   end function getValueAtLocation
   
end module m_spatial_data
