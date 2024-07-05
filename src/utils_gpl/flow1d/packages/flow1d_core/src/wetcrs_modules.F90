module M_newcross                                                ! new type conveyance table crossections
                                                                 ! all data is attached to pluv u nr
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

implicit none

private

public generateConvtab
public dealloc
public regulate_yz_coordinates

integer, parameter, public                             :: CS_LUMPED    = 0 !< Lumped option for yz-type conveyance calculation.
integer, parameter, public                             :: CS_VERT_SEGM = 1 !< Vertically segmented option for yz-type conveyance calculation.

interface dealloc
   module procedure deallocConvtab
end interface dealloc
interface realloc
   module procedure reallocConvtab
end interface realloc

!> Conveyance tabel definition.
type, public :: t_convtab
   integer                                         :: jopen                      !< Open/closed profile, 1/0.
   integer                                         :: levelscount                        !< Number of levels in u tables.
   integer                                         :: iolu   = 1                 !< Latest level found, initialise total_area bottom.
   integer                                         :: conveyType = CS_VERT_SEGM  !< Calculation type for conveyance (lumped or vertically segmented).
   double precision                                :: a_extr                     !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: b_extr                     !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: bedlevel                   !< Lowest point of the YZ cross setction.
   double precision, allocatable                   :: height(:)                  !< Heights for next (u) tables:.
   double precision, allocatable                   :: flow_area(:)               !< Flow area  ft of h, for all sections.
   double precision, allocatable                   :: flow_width(:)              !< Flow width (not double precisionly needed).
   double precision, allocatable                   :: perimeter(:)               !< Wet perimeter (only postpro).
   double precision, allocatable                   :: conveyance(:)              !< Conveyance positive flow direction.
   double precision, allocatable                   :: chezy(:)                   !< Chezy.
   double precision, allocatable                   :: total_area(:)              !< Total area .
   double precision, allocatable                   :: total_width(:)             !< Total width.
   integer                                         :: last_position              !< Last position used for interpolation.
   double precision                                :: chezy_act                  !< Actual Chezy.
                                                                                 !< Here stored for output.
                                                                                 !< Used in function ChezyFromConveyance.
end type t_convtab

type(t_convtab), pointer         :: convtab_help => null()
double precision, allocatable :: friction_value_per_segment(:)
double precision, allocatable :: y(:)
integer, allocatable          :: frictype(:)

contains

!> Deallocate conveyance table
subroutine deallocConvtab(convtab)
   ! Modules
   
   implicit none
   
   ! Input/output parameters
   type(t_convtab), pointer, intent(inout)     :: convtab    !< conveyance table
   
   ! Local variables

   ! Program code
   if (associated(convtab)) then
      if (allocated(convtab%flow_area ))      deallocate(convtab%flow_area )
      if (allocated(convtab%flow_width ))     deallocate(convtab%flow_width )
      if (allocated(convtab%perimeter ))      deallocate(convtab%perimeter )
      if (allocated(convtab%conveyance))      deallocate(convtab%conveyance)
      if (allocated(convtab%chezy))           deallocate(convtab%chezy)
      if (allocated(convtab%height ))         deallocate(convtab%height )
      if (allocated(convtab%total_area ))     deallocate(convtab%total_area )
      if (allocated(convtab%total_width ))    deallocate(convtab%total_width )
      deallocate(convtab)
      convtab => null()
   endif
      
end subroutine deallocConvtab

!> Reallocate conveyance table
subroutine reallocConvtab(convtab, levelscount)
   ! Modules
   use m_alloc
   implicit none
   
   ! Input/output parameters
   type(t_convtab), pointer, intent(inout)     :: convtab    !< conveyance table
   integer, intent(in) :: levelscount
   
   ! Local variables

   ! Program code
   call realloc(convtab%height,      levelscount)
   call realloc(convtab%flow_area,   levelscount)
   call realloc(convtab%flow_width,  levelscount)
   call realloc(convtab%perimeter,   levelscount)
   call realloc(convtab%conveyance,  levelscount)
   call realloc(convtab%chezy,       levelscount)
   call realloc(convtab%total_area,  levelscount)
   call realloc(convtab%total_width, levelscount)
   convtab%levelscount = levelscount
      
end subroutine reallocConvtab

!> Generate conveyance table.
subroutine generateConvtab(convtab, levelscount_csdef, crosssection_id, &
                           yin, z, segmentToSectionIndex, frictionType, &
                           frictionValue)

   use MessageHandling
   use m_alloc
   implicit none

   type (t_convtab), pointer, intent(inout)     :: convtab                          !< Conveyance table.
   integer               , intent(in   )     :: levelscount_csdef                !< Number of levels in yz cross section definition.
   double precision      , intent(in   )     :: yin(:)                           !< Y-offsets in cross section definition.
   double precision      , intent(in   )     :: z(:)                             !< Z-levels in cross section definition (positive upwards).
   character(len=*)      , intent(in   )     :: crosssection_id                  !< Id of the cross section. This is used for an error message.
   integer               , intent(in   )     :: segmentToSectionIndex(:)         !< Table returns frictionIndex for segment (i).
   integer               , intent(in   )     :: frictionType(:)                  !< Friction type for segment.
   double precision      , intent(in   )     :: frictionValue(:)                 !< Friction value for segment.
   
   integer                       :: levelscount_convtab, ierr, lcnv_max, lcnv_current
   integer                       :: yzcount_max_default = 400
   ! Variables for calculating extrapolation coefficients:
   integer              :: i1 ! one but last index
   integer              :: i2 ! last index
   double precision     :: h_1
   double precision     :: h_2
   double precision     :: K_1
   double precision     :: K_2
   
   if (.not. associated(convtab)) then
      allocate(convtab)
   endif

   if (allocated(friction_value_per_segment)) then
      lcnv_current = size(friction_value_per_segment)
   else
      lcnv_current = 0
   end if

   lcnv_max = max(yzcount_max_default, levelscount_csdef * 4)
   if (.not. associated(convtab_help) .or. lcnv_max > lcnv_current) then
      if (.not. associated(convtab_help)) then
         allocate(convtab_help)
      end if
      call reallocConvtab(convtab_help, lcnv_max)

      call realloc(friction_value_per_segment, lcnv_max, stat=ierr)
      call realloc(y, lcnv_max, stat=ierr)
      call realloc(frictype, lcnv_max, stat=ierr)
   endif
   
   convtab_help%height         = 0d0  
   convtab_help%flow_area      = 0d0     
   convtab_help%flow_width     = 0d0     
   convtab_help%perimeter      = 0d0     
   convtab_help%conveyance     = 0d0           
   convtab_help%chezy          = 0d0     
   convtab_help%total_area     = 0d0     
   convtab_help%total_width    = 0d0        
   
   levelscount_convtab = 2

   call CalcConveyanceTable(convtab_help, frictype, friction_value_per_segment, yin, z, levelscount_csdef,  &
                            segmentToSectionIndex, frictionType, frictionValue, &
                            levelscount_convtab)

   if (levelscount_convtab > convtab_help%levelscount) then
      msgbuf = 'Error while generating conveyance table for: '''//trim(crosssection_id)//'''. Please check your input for inconsistencies.'
      call err_flush()
   endif
   convTab%jopen  = 1
   convTab%levelscount    = levelscount_convtab   
   convTab%iolu   = 1
   
   call realloc(convtab, levelscount_convtab)
   
   convTab%height          = convtab_help%height(1:levelscount_convtab)
   convTab%flow_area       = convtab_help%flow_area(1:levelscount_convtab)
   convTab%flow_width      = convtab_help%flow_width(1:levelscount_convtab)
   convTab%perimeter       = convtab_help%perimeter(1:levelscount_convtab)
   convTab%conveyance      = convtab_help%conveyance(1:levelscount_convtab)
   convTab%chezy           = convtab_help%chezy(1:levelscount_convtab)
   convTab%total_area      = convtab_help%total_area(1:levelscount_convtab)
   convTab%total_width     = convtab_help%total_width(1:levelscount_convtab)
   
   ! calculate the coefficients for extrapolation of conveyance above specified profile
  !(*)   !  document SOBEK-21942: Change of roughness formulations in "Y-Z" and
   ! "Asymetrical Trapezium" profiles, Author:     Thieu van Mierlo
   !                                   Programmer: Daniel Abel
   i1  = convTab%levelscount - 1     ! so i1, i2 always inside table
   i2  = i1 + 1
   !
   h_1 = convTab%height(i1)
   h_2 = convTab%height(i2)
   !
   K_1 = convTab%conveyance(i1)
   K_2 = convTab%conveyance(i2)
   !
   ! dlog (h1/h2) is almost zero, however, h1 and h2 
   ! always differ enough (h2-h1 ~> 1e-5) s.t. the operation is stable and
   ! accurate
   !
   convTab%b_extr = dlog(K_1/K_2) / (dlog(h_1/h_2))
   convTab%a_extr = K_1*(h_1**(-convTab%b_extr))
   !

end subroutine generateConvtab

!> Calculate the entities for the conveyance table
subroutine CalcConveyanceTable(convtab, frictype, friction_value_per_segment, y, z, levelscount_csdef, &
                               segmentToSectionIndex, frictionType,                   &
                               frictionValue, levelscount_convtab)
   use MessageHandling
   
   implicit none
   
   ! Input output
   type(t_convtab), pointer,             intent(  out)     :: convtab                     !< Conveyance table for storing the results.
   integer,                           intent(  out)     :: frictype(:)                 !< Friction type at the segments (segment i is between y(i) and y(i+1)).
   integer,                           intent(in   )     :: levelscount_csdef           !< Number of values in y and z coordinates of the cross section definition.
   double precision, dimension(:),    intent(  out)     :: friction_value_per_segment  !< Friction value for each segment
   double precision, dimension(:),    intent(in   )     :: y                           !< y-coordinates of the cross section definition
   double precision, dimension(:),    intent(in   )     :: z                           !< z-coordinates of the cross section definition
   integer,          dimension(:),    intent(in   )     :: segmentToSectionIndex       !< Table returns frictionIndex for segment (i)
   integer,          dimension(:),    intent(in   )     :: frictionType                !< Friction type
   double precision, dimension(:),    intent(in   )     :: frictionValue               !< Friction value
   integer,                           intent(inout)     :: levelscount_convtab         !< Number of levels in new conveyance table.
   
   ! Local variables
   
   integer                        :: k, j, i, numlevels
   double precision               :: zh, dz
   double precision               :: flow_area
   double precision               :: flow_width, perimeter, conveyance, total_area, total_width, conveyance_interpolated, accuracy
   double precision               :: dif2
   double precision, pointer, dimension(:) :: levels
   ! A small profile might cause an overflow in calculating the extrapolation parameters for the conveyance table
   ! in that case raise the conveyance table in order to create a larger profile
   double precision, parameter :: EXTRA_HEIGHT = 0.5d0
   double precision, parameter :: MINCONV = 1e-3
   
   levels => convtab%height
   numlevels=levelscount_convtab
   
   do k = 1, levelscount_csdef
      i = segmentToSectionIndex(k)
      frictype(k) = frictionType(i)
      friction_value_per_segment(k) = frictionValue(i)
   enddo

   do k = 1,levelscount_csdef
      levels(k) = z(k)
   enddo          
   
   do k = 2,levelscount_csdef
      if ( z(k) .eq. z(k-1) ) then  ! floodplane, til verticaal tabelpunt iets op
         levels(k) = levels(k) + 1d-4      ! voor scherper zien van breekpunt in tabel
      endif
   enddo
   numlevels = levelscount_csdef

   call regulate_levels(levels,numlevels)
   
   ! Compute the conveyance for the levels in array levels.
   do j = 1, numlevels
      call ConveyYZ(levelscount_csdef,y,z,frictype,friction_value_per_segment,levels(j),flow_area,total_area,flow_width,total_width,perimeter,conveyance)
      convtab%flow_width(j)      = flow_width
      convtab%flow_area(j)       = flow_area
      convtab%perimeter(j)       = perimeter
      convtab%conveyance(j)      = conveyance
      convtab%total_area(j)      = total_area
      convtab%total_width(j)     = total_width
   enddo
   
   ! Check whether extra levels in the conveyance table are required. The error made by linear interpolation
   ! may only be 1 %. Assuming the largest error is made halfway between 2 levels in the conveyance table,
   ! the conveyance at levels(j) - levels(j-1) is calculated and compared to the interpolated value.
   ! If the accuracy is not met, a new level is added to the conveyance table.
   ! Another requirement is, that the difference between two levels in the conveyance table must
   ! in all cases be larger than 3.0d-3
    j = 2
    accuracy = 0.01d0

    do while (j .le. numlevels)
      dz = levels(j) - levels(j-1)
      
      if (dz .gt. 3.0d-3) then
         
         zh = ( levels(j)+levels(j-1) )*0.5d0
         call ConveyYZ(levelscount_csdef,y,z,frictype,friction_value_per_segment,zh,flow_area,total_area,flow_width,total_width,perimeter,conveyance)
         
         conveyance_interpolated = 0.5d0*convtab%conveyance(j) + 0.5d0*convtab%conveyance(j-1)
         conveyance = max(conveyance,1d-6)
         dif2 = abs(conveyance_interpolated - conveyance)/(conveyance)
         
         if ( dif2 .gt. accuracy ) then  
            
            do k = numlevels+1, j+1,-1
               levels (k) = levels(k-1)
               
               convtab%flow_width(k)      = convtab%flow_width(k-1)
               convtab%flow_area(k)       = convtab%flow_area(k-1)
               convtab%perimeter(k)       = convtab%perimeter(k-1)
               convtab%conveyance(k)      = convtab%conveyance(k-1)
               convtab%total_area(k)      = convtab%total_area(k-1)
               convtab%total_width(k)     = convtab%total_width(k-1)
            enddo
            
            levels(j) = zh
            numlevels = numlevels + 1
            
            convtab%flow_width(j)      = total_width
            convtab%flow_area(j)       = total_area
            convtab%perimeter(j)       = perimeter
            convtab%conveyance(j)      = conveyance
            convtab%total_area(j)      = total_area
            convtab%total_width(j)     = total_width
         else
            ! Goto next level
            j = j+1
         endif
         
      else
         ! Goto next level
         j = j+1
      endif
      
      if (j==numlevels) then
         ! check if previous conveyance is large enough
         
         if (j==1 .or. convtab%conveyance(max(1,j-1)) < MINCONV) then
            levels(j+1) = levels(j)+EXTRA_HEIGHT
            call ConveyYZ(levelscount_csdef,y,z,frictype,friction_value_per_segment,levels(j+1), &
                          flow_area,total_area,flow_width,total_width,perimeter,conveyance)
            convtab%flow_width(j+1)      = total_width
            convtab%flow_area(j+1)       = total_area
            convtab%perimeter(j+1)       = perimeter
            convtab%conveyance(j+1)       = conveyance
            convtab%total_area(j+1)      = total_area
            convtab%total_width(j+1)     = total_width
            numlevels = numlevels + 1
         endif
         
      endif
      
   enddo  ! end do while
   
   levelscount_convtab = max(numlevels,levelscount_convtab)
   
   return
    
end subroutine CalcConveyanceTable
   
!> Actions: 
!> * remove double points
!> * prevent horizontal segments
!> * add extra points (if necessary) at frictionsection transitions
!> * generate segmentToSectionIndex
subroutine regulate_yz_coordinates(y, z, bedlevel, segmentToSectionIndex, levelscount, frictionSectionFrom, sectionCount)
   implicit none

   double precision, dimension(:), intent(inout) :: y                      !< (Adapted) y-coordinates of the YZ cross section
   double precision, dimension(:), intent(inout) :: z                      !< (Adapted) z-coordinates of the YZ cross section
   double precision,               intent(  out) :: bedlevel               !< lowest level of the z-coordinates
   integer,                        intent(inout) :: levelscount            !< Number of (y,z) coordinates
   integer, dimension(:),          intent(  out) :: segmentToSectionIndex  !< Table returns frictionIndex for segment (i)
   double precision, dimension(:), intent(in   ) :: frictionSectionFrom    !< Start coordinate of the friction section   
   integer,                        intent(in   ) :: sectionCount           !< Number of friction sections
   
   integer           :: i, j, k, ncc, start, current
   double precision  :: zmin, grndlyr, zground, f, yy, a, yya, dda
   
   grndlyr = 0d0         ! Ground layers are not implemented (yet).
   
   ! Determine lowest Z-level
   bedlevel = z(1)
   do j = 1, levelscount
      if (z(j) < bedlevel) then 
         bedlevel = z(j)
      endif
   enddo
   
   ! Set Z-coordinates to a base level of 0.
   do j = 1, levelscount
      z(j) = z(j) - bedlevel
   enddo
   
   ! Remove double points.
   current = 1
   do j = 2, levelscount
      if (y(current) .ne. y(j) .or. z(current) .ne. z(j) ) then
         current = current + 1
         y(current) = y(j)
         z(current) = z(j)
      endif
   enddo
   
   ! Remove middle points at vertical parts
   current = 1
   do j = 2, levelscount-1
      if (y(current) .ne. y(j) .or. y(j) .ne. y(j+1) ) then
         current = current + 1
         y(current) = y(j)
         z(current) = z(j)
      endif
   enddo
   
   ! Adjust z to prevent horizontal segments 
   do j = 2, levelscount                 
      if (z(j) == z(j-1)) then
         z(j) = z(j) + 0.0011
      endif
   enddo                        
   
   ! Adjust for ground layer. 
   ! NOTE: This part of the code is not yet functional.
   zmin = 1e30
   do k = 1,levelscount
      zmin = min(zmin,z(k))
   enddo
   if (grndlyr .gt. 0.0d0) then
      zground = zmin + grndlyr !hk: is this o.k. ?
   else
      zground = -1e30
   endif
   
   ncc = levelscount
   
   do k = ncc-1,1,-1                   ! hk: eerst punten bijzetten
      if ( (z(k) .gt. zground .and. z(k+1) .lt. zground) .or. &
         (z(k) .lt. zground .and. z(k+1) .gt. zground) ) then
         f  = abs(zground-z(k))/abs(z(k+1) - z(k))
         yy = y(k) + f*(y(k+1)-y(k))
         levelscount = levelscount + 1
         do j = levelscount,k+2,-1
            y(j) = y(j-1)
            z(j) = z(j-1)
         enddo
         y(k+1) = yy
         z(k+1) = zground
      endif
   enddo

   j = 0                               !hk: dan weghalen wat onder zground zit

   do k = 1,levelscount
      if (z(k) .ge. zground) then
         j = j + 1
         y(j) = y(k)
         z(j) = z(k)   !hk: copie
      endif
   enddo

   levelscount = j

   ! Insert extra (y,z) coordinates at the friction section transistions
   start   = 0
   current = 0
   segmentToSectionIndex = 1
   do i = 2, sectionCount                          
      yya   = frictionSectionFrom(i)
      current = 0
      do k = 1,levelscount                            
         if (y(k) .lt. yya) current = k
      enddo
      if ((current .ne. 0 .and. current .ne. levelscount) .and. (abs(y(current+1) - yya) > 1d-4)) then
         ! Add an extra (y,z) location at to the list
         a   = (y(current+1) - yya)/(y(current+1) - y(current))
         dda = z(current)*a + z(current+1)*(1-a)

         levelscount  = levelscount + 1
         do k = levelscount, current+2,-1 
            y(k) = y(k-1)
            z(k) = z(k-1)
         enddo
         y(current+1) = yya
         z(current+1) = dda
      endif
      if (current > levelscount) then
         current = levelscount
      endif
      do k = start+1,current
         segmentToSectionIndex(k) = i-1
      enddo
      start = current
   enddo
   do k = start+1, levelscount
      segmentToSectionIndex(k) = i-1
   enddo
   

! Einde aanpassingen tbv frictie secties

end subroutine regulate_yz_coordinates

    
!> Sort the array levels and remove double entries.
subroutine regulate_levels(levels, numlevels)  

   use qsort
   
   double precision, dimension(:), intent(inout) :: levels        !< Array containing levels.
   integer,                        intent(inout) ::  numlevels    !< size of levels
   
   integer k, current

   call d_qsort(levels, numlevels)

   ! Remove double entries:
   current = 1
   do k = 2, numlevels
      if (abs(levels(k) - levels(current)) >= 1e-8) then
         current = current + 1
         levels(current) = levels(k)
      endif
   enddo
   numlevels = current

   return

end subroutine regulate_levels
    
   !!TODO >>
!> compute conveyance parameters for a given level.
subroutine ConveyYZ(numyz,y,z,frictype,friction_value,level,flow_area,total_area,flow_width,total_width,perimeter,conveyance) ! conveyance computation for a YZ profile

   integer,           intent(in  ):: numyz                     !< Number of profile points.
   double precision,  intent(in  ):: y(numyz), z(numyz)        !< Original YZ profile description (Z positive up).
                                                               !< Also breakpointed total_area friction segment points.
   integer,           intent(in  ):: frictype(numyz)           !< Friction types for segments (1, n-1).
   double precision,  intent(in  ):: friction_value(numyz)     !< Friction coefficients for segments (1, n-1).
   double precision,  intent(in  ):: level                     !< Water level for which conveyance data has to be calculated.
   double precision, intent(  out) :: flow_area                !< Flow area.
   double precision, intent(  out) :: total_area               !< Total area.
   double precision, intent(  out) :: flow_width               !< Flow width.
   double precision, intent(  out) :: total_width              !< Total width.
   double precision, intent(  out) :: perimeter                !< Perimeter.
   double precision, intent(  out) :: conveyance               !< Conveyance.

   !local
   integer          :: k
   double precision :: cfrictval                      ! friction coefficients for segments (1, n-1)
   double precision :: z0,z1,d0,d1,dz,y0,y1,bb,beta
   double precision :: aa,ww,pp,cc

   flow_area = 0 
   total_area = 0 
   flow_width = 0
   total_width = 0
   perimeter = 0
   conveyance  = 0

   do k = 1,numyz-1

      z0 = z(k)
      z1 = z(k+1)
      d0 = max(level - z0,1.0d-6)                                ! depth left
      d1 = max(level - z1,1.0d-6)                                ! depth right

      y0 = y(k)   - y(k)
      y1 = y(k+1) - y(k)
      bb = dabs(y1 - y0)         ! breedte segment

      cfrictval = max(friction_value(k),1.0e-10)
      if(.NOT. (d0.le.1.0d-6.and.d1.le.1.0d-6)) then
         call ConveySeg (d0,d1,z0, z1, beta,bb,frictype(k),cfrictval,aa,ww,pp,cc) ! segment routine
         total_area = total_area + aa
         total_width = total_width + ww
         flow_width = flow_width + ww
         flow_area = flow_area + aa
         perimeter = perimeter + pp
         conveyance = conveyance + cc   ! totals
      endif
   enddo

end subroutine ConveyYz


subroutine ConveySeg(d0, d1, z0, z1, beta,segwidth,friction_type,friction_value,area,width,perimeter,conveyance)  ! conveyance computation for a segment

   integer,          intent(in   ):: friction_type
   double precision, intent(in   ):: d0                     !< left waterdepth (m), always either hl > 0 or hr > 0
   double precision, intent(in   ):: d1                     !< right waterdepth (m), always either hl > 0 or hr > 0
   double precision, intent(in   ):: z0                     !< left bed level (m)
   double precision, intent(in   ):: z1                     !< right bed level (m)
   double precision, intent(in   ):: segwidth               !< width of the segment
   double precision, intent(in   ):: friction_value         !< roughness coefficient
   double precision, intent(  out):: area                   !< area       (m2)
   double precision, intent(  out):: width                  !< width      (m)
   double precision, intent(  out):: perimeter              !< perimeter  (m)
   double precision, intent(  out):: conveyance             !< conveyance (m2/s)

   ! locals
   double precision :: beta                   !< beta = tan(phi)
   double precision :: dz                     !< |z1-z0|

   double precision, parameter :: sixth = -1d0/6d0  ! for power law
   double precision, parameter :: s83   = 8d0/3d0   ! for power law
   double precision, parameter :: s52   = 5d0/2d0   ! for power law
   double precision, parameter :: s53   = 5d0/3d0   ! for power law
   double precision, parameter :: s32   = 3d0/2d0   ! for power law
   double precision, parameter :: s14   = 1d0/4d0   ! for power law
   double precision, parameter :: s25   = 2d0/5d0   ! for power law
   double precision            :: c1, c2, dcf, f1, f2    

   beta = 0.0d0
   if(segwidth.ne.0.d0) beta = (z1-z0)/segwidth ! beta = tan(phi)
   dz = dabs(z1 - z0)

   conveyance = 0.0d0
   dcf = friction_value
   if(friction_type.eq.3) then
      c1 = (1.0d0+beta**2)**s14*dlog(10.0d0)
      c2 = dcf/12.0d0
   endif
   !
   if(d0.lt.dz.or.d1.lt.dz) then   ! beta#0
      if(beta.lt.-0.01d0) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = 2.0d0*dcf/(5.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d1**s52
         case (1)                   ! Manning (n)
            conveyance = 3.0d0/(8.0d0*dcf*dabs(beta)*(1.0d0+beta**2)**s14)*d1**s83
         case (7)                   ! Strickler (kn)
            conveyance = 75.0d0*dcf**sixth/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d1**s83
         case (8)                   ! Strickler (ks)
            conveyance = 3.0d0*dcf/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d1**s83
         case (3)                   ! White-Colebrook (kn)
            if(d1/c2.le.1.495d0) then 
               f1 = 2.13d-3
            else
               f1 = dlog(d1/c2)-s25
            endif
            conveyance = 36.0d0/(5.0d0*dabs(beta)*c1)*d1**s52*f1
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/(3.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d1**3
         end select
      else if(beta.ge.-0.01.and.beta.lt.0) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = dcf/(((1.0d0+beta**2)**s14))*(d1/2.0d0)**s32*(-d1/beta)
         case (1)                   ! Manning (n)
            conveyance = 1/(dcf*(1.0d0+beta**2)**s14)*(d1/2.0d0)**s53*(-d1/beta)
         case (7)                   ! Strickler (kn)
            conveyance = 25.0d0*dcf**sixth/((1.0d0+beta**2)**s14)*(d1/2.0d0)**s53*(-d1/beta)
         case (8)                   ! Strickler (ks)
            conveyance = dcf/((1.0d0+beta**2)**s14)*(d1/2.0d0)**s53*(-d1/beta)
         case (3)                   ! White-Colebrook (kn)
            if(6.0d0*d1/dcf.le.1.0005d0) then
               f1 = 2.2d-4
            else
               f1 = dlog10(6.0d0*d1/dcf)
            endif
            conveyance = 18.0d0/((1.0d0+beta**2)**s14)*f1*(-d1/beta)*(d1/2.0d0)**s32
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/((1.0d0+beta**2)**s14)*(d1/2.0d0)**2*(-d1/beta)
         end select
     else if(beta.le.0.01.and.beta.gt.0) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = dcf/((1.0d0+beta**2)**s14)*(d0/2.0d0)**s32*(d0/beta)
         case (1)                   ! Manning (n)
            conveyance = 1/(dcf*(1.0d0+beta**2)**s14)*(d0/2.0d0)**s53*(d0/beta)
         case (7)                   ! Strickler (kn)
            conveyance = 25.0d0*dcf**sixth/((1.0d0+beta**2)**s14)*(d0/2.0d0)**s53*(d0/beta)
         case (8)                   ! Strickler (ks)
            conveyance = dcf/((1.0d0+beta**2)**s14)*(d0/2.0d0)**s53*(d0/beta)
         case (3)                   ! White-Colebrook (kn)
            if(6.0d0*d0/dcf.le.1.0005d0) then
               f1 = 2.2d-4
            else
               f1 = dlog10(6.0d0*d0/dcf)
            endif
            conveyance = 18.0d0/((1.0d0+beta**2)**s14)*f1*(d0/beta)*(d0/2.0d0)**s32
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/((1.0d0+beta**2)**s14)*(d0/2.0d0)**2*(d0/beta)
         end select
     else if(beta.gt.0.01) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = 2.0d0*dcf/(5.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d0**s52
         case (1)                   ! Manning (n)
            conveyance = 3.0d0/(8.0d0*dcf*dabs(beta)*(1.0d0+beta**2)**s14)*d0**s83
         case (7)                   ! Strickler (kn)
            conveyance = 75.0d0*dcf**sixth/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d0**s83
         case (8)                   ! Strickler (ks)
            conveyance = 3.0d0*dcf/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d0**s83
         case (3)                   ! White-Colebrook (kn)
            if(d0/c2.le.1.495d0) then 
               f1 = 2.13d-3
            else
               f1 = dlog(d0/c2)-s25
            endif
            conveyance = 36.0d0/(5.0d0*dabs(beta)*c1)*d0**s52*f1
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/(3.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*d0**3
         end select
     endif
   endif
   if(d0.ge.dz.or.d1.ge.dz) then   
     if(beta.ge.-0.01d0.and.beta.le.0.01d0) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = dcf/((1.0d0+beta**2)**s14)*((d0+d1)/2.0d0)**s32*segwidth
         case (1)                   ! Manning (n)
            conveyance = 1.0d0/(dcf*(1.0d0+beta**2)**s14)*((d0+d1)/2.0d0)**s53*segwidth
         case (7)                   ! Strickler (kn)
            conveyance = 25.0d0*dcf**sixth/((1.0d0+beta**2)**s14)*((d0+d1)/2.0d0)**s53*segwidth
         case (8)                   ! Strickler (ks)
            conveyance = dcf/((1.0d0+beta**2)**s14)*((d0+d1)/2.0d0)**s53*segwidth
         case (3)                   ! White-Colebrook (kn)
            if(6.0d0*(d0+d1)/dcf.le.1.0005d0) then
               f1 = 2.2d-4
            else
               f1 = dlog10(6.0d0*(d0+d1)/dcf)
            endif
            conveyance = 18.0d0/((1.0d0+beta**2)**s14)*f1*segwidth*((d0+d1)/2.0d0)**s32
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/((1.0d0+beta**2)**s14)*((d0+d1)/2.0d0)**2*segwidth
         end select
     elseif (dabs(beta) .gt. 0.01d0) then
         select case (friction_type)
         case (0)                   ! Chezy
            conveyance = 2.0d0*dcf/(5.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*dabs(d0**s52-d1**s52)
         case (1)                   ! Manning (n)
            conveyance = 3.0d0/(8.0d0*dcf*dabs(beta)*(1.0d0+beta**2)**s14)*dabs(d0**s83-d1**s83)
         case (7)                   ! Strickler (kn)
            conveyance = 75.0d0*dcf**sixth/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*dabs(d0**s83-d1**s83)
         case (8)                   ! Strickler (ks)
            conveyance = 3.0d0*dcf/(8.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*dabs(d0**s83-d1**s83)
         case (3)                   ! White-Colebrook (kn)
            if(d0/c2.le.1.495d0) then 
               f1 = 2.13d-3
            else
               f1 = dlog(d0/c2)-s25
            endif
            if(d1/c2.le.1.495d0) then 
               f2 = 2.13d-3
            else
               f2= dlog(d1/c2)-s25
            endif
            conveyance = 36.0d0/(5.0d0*dabs(beta)*c1)*dabs(d0**s52*f1-(d1**s52*f2))
         case (9)                   ! Bos&Bijkerk 
            conveyance = dcf/(3.0d0*dabs(beta)*(1.0d0+beta**2)**s14)*dabs(d0**3-d1**3)
         end select
     endif
   endif
   call compwap(segwidth,d0,d1,dz,area,width, perimeter)
end subroutine ConveySeg

!> Compute area and perimeter
subroutine compwap(segwidth,d0,d1,dz,area,width, perimeter)
   double precision, intent(in   ) :: segwidth   !< Width of the segment.
   double precision, intent(in   ) :: d0         !< Depth at the left side of the segment.
   double precision, intent(in   ) :: d1         !< Depth at the right side of the segment.
   double precision, intent(in   ) :: dz         !< Difference in depth.
   double precision, intent(  out) :: area       !< Wet area.
   double precision, intent(  out) :: width      !< Wet area.
   double precision, intent(  out) :: perimeter  !< Wet perimeter.
   
   
   double precision :: f
   double precision :: h1, h2, hl, hr
   double precision :: dh                         ! depth dif

   hL = d0
   hr = d1
   width = segwidth
   if (max(hL,hr) .gt. 1.0d-6) then               ! both wet or one wet
      if (min(hL,hr) .gt. 1.0d-6) then            ! both wet
!
      else if (hL .gt. 1.0d-6) then               ! only left wet
         f  = hl/dz
         width = f*width
         hr = 0d0
      else if (hr .gt. 1.0d-6) then               ! only rigth wet
         f  = hr/dz 
         width = f*width
         hl = 0d0
      endif
   endif

   if (hL .gt. hr) then
      h1 = hL
      h2 = hr
   else                                       
      h2 = hL
      h1 = hr
   endif

   dh = h1 - h2
   
   area  = 0.5d0*( h1 + h2 )*width
   perimeter  = dsqrt ( width*width + dh*dh )
end subroutine compwap

end module M_newcross           ! new type conveyance table crossections



