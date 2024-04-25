module m_Roughness
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
   use m_branch
   use m_tables
   use m_tablematrices
   use m_spatial_data
   use m_hash_search
   
   implicit none
   
   private
   
   public realloc
   public dealloc
   public GetChezy
   public getFrictionParameters
   public frictionTypeStringToInteger
   public functionTypeStringToInteger
   public frictionTypeIntegerToString

   
   double precision :: vonkar      = 0.41        !< von Karman constant ()
   double precision :: ag          = 9.81d0     !< gravity acceleration
   double precision :: sag                      !< = sqrt(ag)  
   double precision :: ee                       !< natural e ()

!   public setCrossSectionIncrement
!   
   interface realloc
      module procedure reallocRoughness
   end interface
   
   interface dealloc
      module procedure deallocRoughness
   end interface dealloc
   
 
   !> roughness definition
   
   !> Roughness definition
   type, public :: t_Roughness                           !< Derived type for roughness data 
   
      character(len=idLen)              :: id            !< section id/name
       
                                                         !> Section Type
                                                         !! 1 = Main
                                                         !! 2 = Floodplain1
                                                         !! 3 = Floodplan2
                                                         !! Any other = Other section (Default 0)
      integer                           :: iSection = 0
      type(t_tablematrix), pointer      :: table(:) => null()              !< table for space and parameter dependent roughness
      logical                           :: useGlobalFriction               !< Flag indicates to use frictionValue and frictionType or to use the table
      double precision                  :: frictionValue                   !< Global friction Value
      integer                           :: frictionType                    !< Global friction Type
      integer, pointer                  :: rgh_type_pos(:) => null()       !< Roughness type for positive flow direction at a branch
      integer, pointer                  :: fun_type_pos(:) => null()       !< Roughness parameter value for positive flow direction at a branch     
      character(len=IdLen)             :: frictionValuesFile              !< *.bc file containing the timeseries with friction values
      type(t_hashlist)                  :: timeSeriesIds                   !< Hashlist containing ids of the timeseries
      integer, pointer                  :: timeSeriesIndexes(:) => null()  !< Get index in timeSeriesIds and/or frictionvalues based on branch index 
      double precision, allocatable     :: currentValues(:)                !< Time Interpolated Friction values of time dependent items (same index as timeSeriesIds).
      double precision, allocatable     :: timeDepValues(:,:)                 !< Friction values of time dependent items (same index as timeSeriesIds).

      integer                           :: spd_pos_idx       !< Index to Spatial Data for parameter values
      
   end type
   
   type, public :: t_RoughnessSet                                           !< Roughness set for roughness sections
      integer                                           :: version =-1      !< Version number for roughness input
      integer                                           :: Size = 0         !< Current size
      integer                                           :: growsBy = 2000   !< Increment for growing array
      integer                                           :: Count= 0         !< Number of elements in array
      type(t_Roughness), pointer, dimension(:)          :: rough => null()  !< Array containing roughness sections
      type(t_tableSet)                                  :: tables           !< Array with tables for flow or water level dependend parameter values
      type(t_hashlist)                                  :: hashlist         !< hashlist for fast searching.
      logical                                           :: timeseries_defined = .false.   !< Indicates whether time dependent roughnesses are defined
      integer                                           :: roughnessFileMajorVersion      !< current major version number of the roughness ini files
   end type t_RoughnessSet

   integer, parameter, public                           :: R_FunctionConstant = 0      !< Constant type roughness function
   integer, parameter, public                           :: R_FunctionDischarge = 1     !< Discharge dependent roughness 
   integer, parameter, public                           :: R_FunctionLevel = 2         !< Water level dependent roughness
   integer, parameter, public                           :: R_FunctionTimeSeries = 3    !< Time dependent roughness
   integer, parameter, public                           :: R_Chezy = 0                 !< Chezy type roughness
   integer, parameter, public                           :: R_Manning = 1               !< Manning  roughness formula
   integer, parameter, public                           :: R_Nikuradse = 7             !< Nikuradse roughness formula
   integer, parameter, public                           :: R_Strickler = 8             !< Strickler roughness formula
   integer, parameter, public                           :: R_WhiteColebrook = 3        !< White Colebrook roughness formula
   integer, parameter, public                           :: R_BosBijkerk = 9            !< Bos en Bijkerk roughness formula 
   double precision, parameter, public                  :: sixth = 1.d0/6.d0           !< 1/6
   double precision, parameter, public                  :: third = 1.d0/3.d0           !< 1/3
   double precision, parameter, public                  :: chlim = 0.001d0             !< Lowest Chezy value

contains
   
!> Reallocate roughness array, while keeping the original values in place   
subroutine reallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet), intent(inout)           :: rgs    !< roughness set
   
   ! Local variables
   type(t_Roughness), pointer, dimension(:)    :: oldrough
   
   ! Program code
   
   if (rgs%Size > 0) then
      allocate(oldrough(rgs%Size))
      oldrough=rgs%rough
      deallocate(rgs%rough)
   else
      ! set some parameters (not the correct location)
      ee = exp(1d0)
      sag = sqrt(ag)
      rgs%version = -1
   endif
   
   if (rgs%growsBy <=0) then
      rgs%growsBy = 200
   endif
   allocate(rgs%rough(rgs%Size+rgs%growsBy))
   
   if (rgs%size > 0) then
      rgs%rough(1:rgs%size) = oldrough(1:rgs%size)
      deallocate(oldrough)
   endif
   
   rgs%Size = rgs%Size+rgs%growsBy

end subroutine reallocRoughness
   
!> Deallocate roughness set   
subroutine deallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet), intent(inout)    :: rgs          !< roughness set
   
   ! Local variables
   integer                 :: i
   
   ! Program code
   if (rgs%count > 0) then
      do i = 1, rgs%count

         if (associated(rgs%rough(i)%rgh_type_pos))  deallocate(rgs%rough(i)%rgh_type_pos) 
         if (associated(rgs%rough(i)%fun_type_pos))  deallocate(rgs%rough(i)%fun_type_pos)           
      
         rgs%rough(i)%rgh_type_pos  => null()
         rgs%rough(i)%fun_type_pos  => null()
         
      enddo
      
      call dealloc(rgs%tables)
      call dealloc(rgs%hashlist)
         
      deallocate(rgs%rough)
      rgs%rough => null()
         
      rgs%size=0
      rgs%count=0

   endif

end subroutine deallocRoughness

   !> Converts a friction type as text string into the integer parameter constant.
   !! E.g. R_Manning, etc. If input string is invalid, -1 is returned.
   subroutine frictionTypeStringToInteger(sfricType, ifricType)
      use string_module, only:str_lower
      implicit none
      character(len=*), intent(in   ) :: sfricType !< Friction type string.
      integer,          intent(  out) :: ifricType !< Friction type integer. When string is invalid, -1 is returned.
      
      call str_lower(sfricType)
      select case (trim(sfricType))
         case ('chezy')
            ifricType = R_Chezy
         case ('manning')
            ifricType = R_Manning
         case ('walllawnikuradse')
            ifricType = 2 ! TODO: JN: White-Colebrook $k_n$ (m) -- Delft3D style not available yet, no PARAMETER.
         case ('whitecolebrook')
            ifricType = R_WhiteColebrook
         case ('stricklernikuradse')
            ifricType = R_Nikuradse
         case ('strickler')
            ifricType = R_Strickler
         case ('debosbijkerk')
            ifricType = R_BosBijkerk
         case default
            ifricType = -1
      end select
      return
   
   end subroutine frictionTypeStringToInteger
   
   !> Converts a friction integer type to a text string 
   !! E.g. 'Manning' -> R_Manning, etc. 
   function frictionTypeIntegerToString(ifricType)
      use string_module, only:str_lower
      implicit none
      integer,          intent(in   ) :: ifricType !< Friction type integer. When string is invalid, 'unknown' is returned.
      character(:), allocatable :: frictionTypeIntegerToString
      
      select case (ifricType)
         case(R_Chezy)
            frictionTypeIntegerToString = 'Chezy'
         case(R_Manning)
            frictionTypeIntegerToString = 'Manning'
         case(2)
            frictionTypeIntegerToString = 'WallLawNikuradse'
         case(R_WhiteColebrook)
            frictionTypeIntegerToString = 'WhiteColebrook'
         case(R_Nikuradse)
            frictionTypeIntegerToString = 'StricklerNikuradse'
         case(R_Strickler)
            frictionTypeIntegerToString = 'Strickler'
         case(R_BosBijkerk)
            frictionTypeIntegerToString = 'deBosBijkerk'
         case default
            frictionTypeIntegerToString = 'unknown'
      end select
      return
   
   end function frictionTypeIntegerToString
   
   !> Converts a (friction) function type as text string into the integer parameter constant.
   !! E.g. R_FunctionConstant, etc. If input string is invalid, -1 is returned.
   subroutine functionTypeStringToInteger(sfuncType, ifuncType)
      use string_module, only:str_lower
      implicit none
      character(len=*), intent(in   ) :: sfuncType !< Function type string.
      integer,          intent(  out) :: ifuncType !< Function type integer. When string is invalid, -1 is returned.
      
      call str_lower(sfuncType)
      select case (trim(sfuncType))
         case ('constant')
            ifuncType = R_FunctionConstant
         case ('absdischarge')
            ifuncType = R_FunctionDischarge
         case ('waterlevel')
            ifuncType = R_FunctionLevel
         case ('timeseries')
            ifuncType = R_FunctionTimeseries
         case default
            ifuncType = -1
      end select
      return
   
   end subroutine functionTypeStringToInteger

!> Get the Chezy value for a given friction type and parameter value
double precision function GetChezy(frictType, cpar, rad, dep, u)

   implicit none
   double precision, intent(in)   :: dep                   !< water depth
   double precision, intent(in)   :: rad                   !< hydraulic radius
   double precision, intent(in)   :: cpar                  !< parameter value
   double precision, intent(in)   :: u                     !< velocity
   integer,          intent(in)   :: frictType             !< friction type (e.g., R_Manning, etc.)
   !
   !     Declaration of Parameters:
   !
   double precision        :: rad0
   double precision        :: z0
   double precision        :: sqcf
   
   rad0 = max(rad,1d-4)
   
   select case(frictType)
   case(0)
      !           Chezy value
      GetChezy = cpar
   case (1) 
      !           Manning-formula
      GetChezy = rad0**sixth/cpar
   case (2) 
     !            White Colebrook Delft3D
     z0        = min( cpar / 30d0 , rad0*0.3d0)
     sqcf      = vonkar/log( rad0/(ee*z0) )
     getChezy  = sag/sqcf
   case (3) 
      !           White Colebrook WAQUA / Nikuradse-formula
      GetChezy = 18.0d0*log10(12.d0*rad0/cpar)
   case (7) 
      !           Strickler-1 formula
      GetChezy = 25.0d0*(rad0/cpar)**sixth
   case (8) 
      !           Strickler-2 formula
      GetChezy = cpar*rad0**sixth
   case (9) 
      !           Bos Bijkerk formula
      GetChezy = cpar*dep**third*rad0**sixth
   case default
   end select
   !
   !
   !        The obtained Chezy value may never be less then 10. in
   !        the main channel (Civil engineers common sense)
   !        and less then 1. in the flood plains
   !
   if (GetChezy<chlim) then
      GetChezy = chlim
   endif

end function GetChezy

!> Retrieve the friction parameter for given branchIndex/chainage
subroutine getFrictionParameters(rgh, ibranch, chainage, c_type, c_par)

   use m_tables
   use m_tablematrices

   implicit none
   !
   ! Global variables
   !
   type(t_Roughness), intent(in   )   :: rgh         !< Roughness data
   integer,           intent(in   )   :: ibranch     !< branch index
   double precision,  intent(in   )   :: chainage    !< chainage (location on branch)
   integer,           intent(  out)   :: c_type      !< friction type
   double precision,  intent(  out)   :: c_par       !< friction parameter value
    
   !
   !
   ! Local variables
   !
   integer                         :: timeseries_index
   double precision                :: ys
   integer, dimension(:), pointer  :: rgh_type
   integer, dimension(:), pointer  :: fun_type
   !     Explanation:
   !     -----------
   !
   !     1. Each Chezy formula, apart from Engelund bed friction, is defined
   !        by 1 constant parameter. This constant is stored in bfricp.
   !        An exception is the Engelund bed friction defined by 10 parameters.
   !     2. For the Engelund bed friction the specific parameters are stored
   !        in the array engpar.
   !
   !
   !     Prevention against zero hydraulic radius and depth
   !
   
   if (rgh%useGlobalFriction)  then
      c_par = rgh%frictionValue
      c_type = rgh%frictionType
   else
      rgh_type  => rgh%rgh_type_pos 
      fun_type  => rgh%fun_type_pos 
      if (rgh_type(ibranch) ==-1)  then
         c_par = rgh%frictionValue
         c_type = rgh%frictionType
      else
         ys = 0d0
         c_par = interpolate(rgh%table(ibranch), chainage, ys)
         c_type = rgh_type(ibranch)
      endif

      ! In case of a time dependent roughness, overwrite the friction parameter
      if (fun_type(ibranch) ==R_FunctionTimeSeries) then
         ! There is a time dependency
         timeseries_index = rgh%timeSeriesIndexes(ibranch) 
         if (rgh%timeDepValues(timeseries_index,2) > 0d0) then
            ! The values are set 
            ! This subroutine is used for filling the YZ-cross section with new friction parameters
            ! As a result the value at the latest time instance is required
            c_par = rgh%timeDepValues(timeseries_index,2)
         endif
      endif
   endif

end subroutine getFrictionParameters

end module m_Roughness
