!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Reading + initializing of initial and parameter fields.
!! The IniFieldFile from the MDU is the successor of the old
!! *.ext file for quantities such as initialwaterlevel,
!! frictioncoefficient, etc.
module unstruc_extfile

use unstruc_messages
use properties
use string_module, only: str_lower, strcmpi

public :: readFieldProvider, fileTypeStringToInteger, methodStringToInteger, averagingTypeStringToInteger

! History versions:

! 1.01 (2019-03-12): First version of *.ini type initial fields and parameters file.


type ext_rec
   character(len=:), allocatable :: groupname
   character(len=:), allocatable :: quantity
   character(len=:), allocatable :: varname
   character(len=:), allocatable :: locationfile
   integer                       :: locationtype
   character(len=:), allocatable :: forcingfile
   double precision              :: returntime
   double precision              :: openboundarytolerance
   character(len=:), allocatable :: id
   character(len=:), allocatable :: operand
   double precision              :: averagingrelsize
   integer                       :: averagingnummin
   integer                       :: averagingtype
   double precision              :: averagingpercentile
   integer                       :: interpmethod
   integer                       :: extrapmethod       = 0
   character(len=:), allocatable :: datafile
   integer                       :: datafiletype
   integer                       :: lateraltype
   double precision              :: lateralflow
   double precision              :: value
   double precision, pointer     :: transformcoef(:)    !< Transformation coefficients
end type ext_rec   



contains


!> Converts fileType string to an integer.
!! Returns -1 when an invalid type string is given.
subroutine fileTypeStringToInteger(sFileType, iFileType)
   use timespace_parameters
   implicit none
   character(len=*), intent(in   ) :: sFileType        !< file type string
   integer,          intent(  out) :: iFileType        !< file type integer
   
   call str_lower(sFileType)
   select case (trim(sFileType))
      case ('arcinfo')
         iFileType = arcinfo
      case ('sample')
         iFileType = triangulation
      case ('1dfield')
         iFileType = field1D
      case ('polygon')
         iFileType = inside_polygon
      case default
         iFileType = -1
   end select
end subroutine fileTypeStringToInteger

!> Converts lateralType string to an integer.
!! Returns -1 when an invalid type string is given.
subroutine lateralTypeStringToInteger(sLateralType, iLateralType)
   use timespace_parameters
   use string_module
   use m_wind
   implicit none
   character(len=*), intent(in   ) :: sLateralType        !< lateral type string
   integer,          intent(  out) :: iLateralType        !< lateral type integer
   
   select case (str_tolower(trim(sLateralType)))
   case ('1d')
      iLateralType = ILATTP_1D
   case ('2d')
      iLateralType = ILATTP_2D
   case ('1d2d')
      iLateralType = ILATTP_ALL
   case default
      iLateralType = ILATTP_ALL
   end select
end subroutine lateralTypeStringToInteger

!> Converts interpolationMethod string to an integer.
!! Returns -1 when an invalid type string is given.
subroutine methodStringToInteger(sMethod, iMethod)
   implicit none
   character(len=*), intent(in   ) :: sMethod        !< method string
   integer,          intent(  out) :: iMethod        !< method integer

   call str_lower(sMethod)
   select case (trim(sMethod))
      case ('constant')
         iMethod = 4
      case ('triangulation')
         iMethod = 5
      case ('averaging')
         iMethod = 6
      case default
         iMethod = -1
   end select
end subroutine methodStringToInteger


!> Converts averaging type string to an integer value.
!! Returns -1 when an invalid type string is given.
subroutine averagingTypeStringToInteger(sAveragingType, iAveragingType)
   implicit none
   character(len=*), intent(in   ) :: sAveragingType        ! averaging type string
   integer,          intent(  out) :: iAveragingType        ! averaging type integer
   
   call str_lower(sAveragingType)
   select case (trim(sAveragingType))
      case ('mean')
         iAveragingType = 1
      case ('nearestnb')
         iAveragingType = 2
      case ('max')
         iAveragingType = 3
      case ('min')
         iAveragingType = 4
      case ('invdist')
         iAveragingType = 5
      case ('minabs')
         iAveragingType = 6
      case default
         iAveragingType = -1
   end select
end subroutine averagingTypeStringToInteger

!> Converts location type string to an integer value.
!! Returns -1 when an invalid type string is given.
subroutine locationTypeStringToInteger(sLocationType, iLocationType)
   use string_module   
   use m_wind
   implicit none
   character(len=*), intent(in   ) :: sLocationType        ! averaging type string
   integer,          intent(  out) :: iLocationType        ! averaging type integer
   
   select case (trim(str_tolower(sLocationType)))
      case ('1d')
         iLocationType = ILATTP_1D
      case ('2d')
         iLocationType = ILATTP_2D
      case ('1d2d')
         iLocationType = ILATTP_ALL
      case default
         iLocationType = ILATTP_ALL
   end select
end subroutine locationTypeStringToInteger



function readFieldProvider(extrecord, basedir, extname, node_ptr) result (success)
   use messageHandling
   use string_module
   implicit none
   logical                                    :: success
   type (ext_rec), pointer                    :: extrecord           !< 
   character (len=*), intent(   in)           :: basedir             !< path to the ext-file
   character (len=*), intent(   in)           :: extname             !< name of the ext-file, without path
   type(tree_data), pointer                   :: node_ptr            !< The tree structure containing a single ini-file chapter/block.

   type (ext_rec), pointer                    :: self
   integer, parameter           :: ini_key_len   = 32  !
   integer, parameter           :: ini_value_len = 256 !
   character(len=ini_value_len) :: property_name
   character(len=ini_value_len) :: property_value
   character(len=ini_key_len)   :: groupname           !
   integer                      :: j, istat, iextrap
   logical                      :: retVal
   type(tree_data), pointer     :: block_ptr           !
   logical                      :: group_ok
   
   success = .False.
   self => extrecord
   self%groupname = tree_get_name(node_ptr)

   ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingfile=...
   do j=1,size(node_ptr%child_nodes)
      block_ptr => node_ptr%child_nodes(j)%node_ptr
      ! todo: read multiple quantities
      property_name = tree_get_name(block_ptr)
      call tree_get_data_string(block_ptr, property_value, retVal)
      istat = 0
      select case (property_name)
      case ('quantity')
         self%quantity = trim(property_value)
      case ('locationfile')
         call resolvePath(property_value, basedir, property_value)
         self%locationfile = trim(property_value)
      case ('forcingfile')
         call resolvePath(property_value, basedir, property_value)
         self%forcingfile = trim(property_value)
      case ('returntime', 'return_time')
         read(property_value,*,iostat=istat) self%returntime
      case ('openboundarytolerance')
         read(property_value,*,iostat=istat) self%openboundarytolerance
      case ('nodeid','id')
         self%id = trim(property_value)
      case ('averagingrelsize')
         read(property_value,*,iostat=istat) self%averagingrelsize
      case ('averagingnummin')
         read(property_value,*,iostat=istat) self%averagingnummin
      case ('averagingtype')
         call averagingTypeStringToInteger(property_value, self%averagingtype)
      case ('averagingpercentile')
         read(property_value,*) self%averagingpercentile
      case ('locationtype')
         call locationTypeStringToInteger(property_value, self%locationtype)
      case ('extrapolationmethod')
         read(property_value,*,iostat=istat) iextrap 
         self%extrapmethod = self%extrapmethod + 100 * iextrap
      case ('interpolationmethod')
         call methodStringToInteger(property_value, self%interpmethod)
      case ('datafile')
         self%datafile = trim(property_value)
      case ('datafiletype')
         call fileTypeStringToInteger(property_value, self%datafiletype)
      case ('value')
         read(property_value,*,iostat=istat) self%value
      case ('operand')
         self%operand = trim(property_value)
      case ('varname')
         self%varname = trim(property_value)
      case ('lateraltype')    
         call lateralTypeStringToInteger(property_value, self%lateraltype)
      case ('flow')    
         read(property_value,*,iostat=istat) self%lateralflow
      case default
         ! handle exception or issue warning 
         cycle
      end select
      if (istat/=0) then
         if (istat>0) then
            continue    ! tbd: exception handling failing to read property_value into an int, double, etc 
         end if   
         if (istat<0) then
            continue    ! tbd 
         end if   
      endif
   end do
   success = .True.

end function readFieldProvider
   

end module unstruc_extfile







