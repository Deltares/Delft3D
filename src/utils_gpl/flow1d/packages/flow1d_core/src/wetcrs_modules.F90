module M_newcross                                                ! new type conveyance table crossections
                                                                 ! all data is attached to pluv u nr
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

implicit none

private

public CalcConveyanceTables
public ConveyanceTables
public generateConvtab
public regulate_levels
public dealloc
public write_conv_tab
public regulate_yz_coordinates

 integer, parameter, public                             :: CS_LUMPED    = 0 !< Lumped option for yz-type conveyance calculation.
 integer, parameter, public                             :: CS_VERT_SEGM = 1 !< Vertically segmented option for yz-type conveyance calculation.
 integer, public     :: lcnvmax                                             !< Estimated maximum number of levels in a conveyance table.  
 
 interface dealloc
    module procedure deallocCru
 end interface dealloc

!> Conveyance tabel definition.
type, public :: t_crsu
   integer                                         :: jopen                      !< Open/closed profile, 1/0.
   integer                                         :: msec                       !< Number of friction sections (horreur only for postpro).
   integer                                         :: nru                        !< Number of levels in u tables.
   integer                                         :: iolu   = 1                 !< Latest level found, initialise total_area bottom.
   integer                                         :: negcon = 0                 !< Different conveyance for negative flow? 0/1.
   integer                                         :: conveyType = CS_VERT_SEGM  !< Calculation type for conveyance (lumped or vertically segmented).
   double precision                                :: a_pos_extr                 !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: a_neg_extr                 !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: b_pos_extr                 !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: b_neg_extr                 !< Extrapolation Factor for YZ-Profiles.
   double precision                                :: bedlevel                   !< Lowest point of the YZ cross setction.
   double precision, allocatable                   :: height (:)            !< Heights for next (u) tables:.
   double precision, allocatable                   :: flow_area (:)              !< Flow area  ft of h, for all sections.
   double precision, allocatable                   :: flow_width (:)             !< Flow width (not double precisionly needed).
   double precision, allocatable                   :: perimeter (:)              !< Wet perimeter (only postpro).
   double precision, allocatable                   :: conveyance_pos(:)          !< Conveyance positive flow direction.
   double precision, allocatable                   :: conveyance_neg(:)          !< Conveyance negative flow direction.
   double precision, allocatable                   :: chezy_pos(:)               !< Chezy posflow.
   double precision, allocatable                   :: chezy_neg(:)               !< Chezy negflow.
   double precision, allocatable                   :: total_area(:)              !< Total area .
   double precision, allocatable                   :: total_width(:)             !< Total width.

   integer                                         :: last_position              !< Last position used for interpolation.
   double precision                                :: chezy_act                  !< Actual Chezy.
                                                                                 !< Here stored for output.
                                                                                 !< Used in function ChezyFromConveyance.
   
end type t_crsu

double precision, allocatable, dimension(:), public   :: levels
double precision, allocatable, dimension(:), public   :: flow_widths
double precision, allocatable, dimension(:), public   :: flow_areas
double precision, allocatable, dimension(:), public   :: perimeters
double precision, allocatable, dimension(:), public   :: conveyances_posdir
double precision, allocatable, dimension(:), public   :: conveyances_negdir
double precision, allocatable, dimension(:), public   :: chezy_posdir
double precision, allocatable, dimension(:), public   :: chezy_negdir
double precision, allocatable, dimension(:), public   :: total_widths
double precision, allocatable, dimension(:), public   :: total_areas

integer, public                                        :: nupt
integer, public                                        :: msect

contains

!> Deallocate conveyance table
subroutine deallocCru(cru)
   ! Modules
   
   implicit none
   
   ! Input/output parameters
   type(t_crsu), pointer, intent(inout)     :: cru    !< conveyance table
   
   ! Local variables

   ! Program code
   if (associated(cru)) then
      if (allocated(cru%flow_area ))      deallocate(cru%flow_area )
      if (allocated(cru%flow_width ))     deallocate(cru%flow_width )
      if (allocated(cru%perimeter ))      deallocate(cru%perimeter )
      if (allocated(cru%conveyance_pos))  deallocate(cru%conveyance_pos)
      if (allocated(cru%conveyance_neg))  deallocate(cru%conveyance_neg)
      if (allocated(cru%chezy_pos))       deallocate(cru%chezy_pos)
      if (allocated(cru%chezy_neg))       deallocate(cru%chezy_neg)
      if (allocated(cru%height ))    deallocate(cru%height )
      if (allocated(cru%total_area ))     deallocate(cru%total_area )
      if (allocated(cru%total_width ))    deallocate(cru%total_width )
      deallocate(cru)
   endif
      
end subroutine deallocCru

!> Generate conveyance table.
subroutine generateConvtab(convtab, levelsCount, grndlyr, &
                           frictionSectionsCount, crosssection_id, &
                           yin, z, segmentToSectionIndex, frictionTypePos, &
                           frictionValuePos, frictionTypeNeg, frictionValueNeg)

   implicit none
   type (t_crsu), pointer, intent(inout)     :: convtab          !< Conveyance table.
   integer               , intent(in   )     :: levelsCount                 !< Number of levels in yz cross section definition.
   double precision      , intent(in   )     :: grndlyr                     !< Groundlayer thickness.
   double precision      , intent(in   )     :: yin(:)                      !< Y-offsets in cross section definition.
   double precision      , intent(in   )     :: z(:)                        !< Z-levels in cross section definition (positive upwards).
   integer               , intent(in   )     :: frictionSectionsCount       !< Number of friction sections.
   character(len=*)      , intent(in   )     :: crosssection_id             !< Id of the cross section. This is used for an error message
   integer               , intent(in   )     :: segmentToSectionIndex(:)    !< Table returns frictionIndex for segment (i)
   integer               , intent(in   )     :: frictionTypePos(:)          !< Friction type in positive flow direction for segment
   double precision      , intent(in   )     :: frictionValuePos(:)         !< Friction type in negative flow direction for segment
   integer               , intent(in   )     :: frictionTypeNeg(:)          !< Friction value in positive flow direction for segment
   double precision      , intent(in   )     :: frictionValueNeg(:)         !< Friction value in negative flow direction for segment
   
   double precision, allocatable :: friction_value_per_segment(:)
   double precision, allocatable :: y(:)
   integer, allocatable          :: frictype(:)
   integer                       ::  jgetlevels, nnlev, ierr
   integer                    :: nc                          !< 
   
   ! Variables for calculating extrapolation coefficients:
   integer              :: i1 ! one but last index
   integer              :: i2 ! last index
   double precision     :: h_1
   double precision     :: h_2
   double precision     :: K_1
   double precision     :: K_2
   
   nc = levelsCount
   if (.not. associated(convtab)) then
      allocate(convtab)
   endif

   if (.not. allocated(levels)) then
      lcnvmax = max(400, levelsCount * 4)
   
      allocate (levels(lcnvmax*2) ,      &
      flow_widths(lcnvmax) ,   &
      flow_areas(lcnvmax) ,   &
      perimeters(lcnvmax) ,   &
      chezy_posdir(lcnvmax) ,   &
      chezy_negdir(lcnvmax) ,   &
      conveyances_posdir(lcnvmax) ,   &
      conveyances_negdir(lcnvmax) ,   &
      total_widths(lcnvmax) ,    &
      total_areas(lcnvmax) ,    &
      stat=ierr )
      
      allocate (friction_value_per_segment(lcnvmax),  &
      y(lcnvmax),   &
      frictype(lcnvmax),  &
      stat=ierr )
      
   endif
   
   flow_widths          = 0
   flow_areas           = 0
   perimeters           = 0
   chezy_posdir         = 0
   chezy_negdir         = 0
   conveyances_posdir   = 0
   conveyances_negdir   = 0
   total_widths         = 0
   total_areas          = 0
   
   nnlev = 2
   jgetlevels = 1
   
   
   call calcConveyanceTables(frictype, friction_value_per_segment, yin, z, levels, nc,  jgetlevels, grndlyr, nbo,               &
   segmentToSectionIndex, frictionTypePos, frictionValuePos,  &
   frictionTypeNeg, frictionValueNeg, nhmax)


   convTab%jopen  = 1
   convTab%msec   = 1 
   convTab%nru    = nnlev   
   convTab%iolu   = 1
   convTab%negcon = 0
   
   
   allocate(convTab%height (nnlev)    )
   allocate(convTab%flow_area (nnlev)  )
   allocate(convTab%flow_width (nnlev)  )
   allocate(convTab%perimeter (nnlev)  )
   allocate(convTab%conveyance_pos(nnlev)  )
   allocate(convTab%conveyance_neg(nnlev)  )
   allocate(convTab%chezy_pos(nnlev)  )
   allocate(convTab%chezy_neg(nnlev)  )
   allocate(convTab%total_area(nnlev)   )
   allocate(convTab%total_width(nnlev)   )
   
   convTab%height          = levels(1:nnlev)
   convTab%flow_area       = flow_areas(1:nnlev)
   convTab%flow_width      = flow_widths(1:nnlev)
   convTab%perimeter       = perimeters(1:nnlev)
   convTab%conveyance_pos  = conveyances_posdir(1:nnlev)
   convTab%conveyance_neg  = conveyances_negdir(1:nnlev)
   convTab%chezy_pos       = chezy_posdir(1:nnlev)
   convTab%chezy_neg       = chezy_negdir(1:nnlev)
   convTab%total_area      = total_areas(1:nnlev)
   convTab%total_width     = total_widths(1:nnlev)
   
   ! calculate the coefficients for extrapolation of conveyance above specified profile
  !(*)   !  document SOBEK-21942: Change of roughness formulations in "Y-Z" and
   ! "Asymetrical Trapezium" profiles, Author:     Thieu van Mierlo
   !                                   Programmer: Daniel Abel
   i1  = convTab%nru - 1     ! so i1, i2 always inside table
   i2  = i1 + 1
   !
   h_1 = convTab%height(i1)
   h_2 = convTab%height(i2)
   !
   K_1 = convTab%conveyance_pos(i1)
   K_2 = convTab%conveyance_pos(i2)
   !
   ! dlog (h1/h2) is almost zero, however, h1 and h2 
   ! always differ enough (h2-h1 ~> 1e-5) s.t. the operation is stable and
   ! accurate
   !
   convTab%b_pos_extr = dlog(K_1/K_2) / (dlog(h_1/h_2))
   convTab%a_pos_extr = K_1*(h_1**(-convTab%b_pos_extr))
   !
   if (convTab%negcon .eq. 1) then
       K_1 = convTab%conveyance_neg(i1)
       K_2 = convTab%conveyance_neg(i2)
       !
       convTab%b_neg_extr = dlog(K_1/K_2) / (dlog(h_1/h_2))
       convTab%a_neg_extr = K_1*(h_1**(-convTab%b_neg_extr))
   else
       convTab%b_neg_extr = convTab%b_pos_extr
       convTab%a_neg_extr = convTab%a_pos_extr
   endif
   
   
   deallocate (levels ,     &
               flow_widths  ,     &
               flow_areas  ,     &
               perimeters  ,     &
               conveyances_posdir  ,     &
               conveyances_negdir  ,     &
               chezy_posdir  ,     &
               chezy_negdir  ,     &
               total_areas  ,     &
               total_widths  , stat=ierr )
   deallocate( friction_value_per_segment, y, frictype)

end subroutine generateConvtab

!!TODO >>
!> Calculate the con
subroutine CalcConveyanceTables(frictype, friction_value_per_segment, y, z, levels, nc,  jgetlevels,                        &
                                grndlyr, nbo, segmentToSectionIndex,                  &
                                frictionTypePos, frictionValuePos, frictionTypeNeg, frictionValueNeg,  &
                                nhmax)
    use MessageHandling
    
    implicit none
    
    ! Input output
    integer, intent(  out)            :: frictype(lcnvmax)        !< friction type at the segments (segment i is between y(i) and y(i+1))
    
    integer  nh,  jgetlevels
    integer          nc
    integer          nbo
    
    double precision           ::  friction_value_per_segment(lcnvmax)
    double precision           :: levels(lcnvmax*2), y(lcnvmax), z(lcnvmax*2)
    double precision           :: grndlyr
    integer                    :: segmentToSectionIndex(:)
    integer                    :: frictionTypePos(nbo)
    double precision           :: frictionValuePos(nbo)
    integer                    :: frictionTypeNeg(nbo)
    double precision           :: frictionValueNeg(nbo)

    ! Local variables

    integer numt, nump, nhi, nhmax
    integer ierr, k, j, i, io, num
    double precision               :: zh, dz
    double precision               :: a
    double precision               :: w, p, co, total_area, total_width, conv, accuracy
    double precision               :: dif2
    double precision               :: sl
    integer, allocatable           :: ik1(:)
    double precision,  allocatable :: yh(:), dh(:)
    ! A small profile might cause an overflow in calculating the extrapolation parameters for the conveyance table
    ! in that case raise the conveyance table in order to create a larger profile
    double precision, parameter :: EXTRA_HEIGHT = 0.5d0
    double precision, parameter :: MINCONV = 1e-3

    data numt /0/, nump /0/

    allocate ( ik1(lcnvmax*2), &
               yh(lcnvmax), &
               dh(lcnvmax*2), stat=ierr )
    if ( ierr .ne. 0 ) goto 9000
    nh=nhmax
    ik1 = 0
    yh = 0.0
    dh = 0.0

    sl = 0.

   ! compute conveyance (y/z profiles)

   do k = 1, nc                        !hk: do over the friction sections
      i = segmentToSectionIndex(k)
      frictype(k) = frictionTypePos(i)
      friction_value_per_segment(k)       = frictionValuePos(i)
   enddo

    if (jgetlevels .eq. 1) then ! hier wordt eerste hoogtetabel opgebouwd
      do k = 1,nc
        levels(k) = z(k)
      enddo          
      
      do k = 2,nc
        if ( z(k) .eq. z(k-1) ) then  ! floodplane, til verticaal tabelpunt iets op
          levels(k) = levels(k) + 1d-4      ! voor scherper zien van breekpunt in tabel
        endif
      enddo
      nh = nc
      call regulate_levels(levels,nh)
    endif

    nhi = nh

    flow_widths(:) = 0
    flow_areas(:) = 0
    perimeters(:) = 0
    conveyances_posdir(:) = 0
    conveyances_negdir(:) = 0
    total_areas(:)   = 0
    total_widths(:)   = 0

    num = 0
    j   = 0
    io  = 0

    do j = 1, nh
      call ConveyYZ(nc,y,z,frictype,friction_value_per_segment,levels(j),a,total_area,w,total_width,p,co)
      flow_widths(j) = total_width
      flow_areas(j) = total_area
      perimeters(j) = p
      conveyances_posdir(j) = co
      conveyances_negdir(j) = co
      total_areas(j) = total_area
      total_widths(j) = total_width
    enddo
    
    ! for the number of levels
    j = 2
    do while (j .le. nh)
      dz = levels(j) - levels(j-1)
      
      if (dz .gt. 3.0d-3) then
         
         zh = ( levels(j)+levels(j-1) )*0.5d0
         call ConveyYZ(nc,y,z,frictype,friction_value_per_segment,zh,a,total_area,w,total_width,p,co)

         conv = 0.5d0*conveyances_posdir(j) + 0.5d0*conveyances_posdir(j-1)
         co = max(co,1d-6)
         dif2 = abs(conv - co)/(co)
         accuracy = 0.01d0

         if ( dif2 .gt. accuracy ) then  ! hk: en zolang (nh .lt. size(levels) )

            do k = nh+1, j+1,-1
               levels (k) = levels(k-1)
               flow_widths(k) = flow_widths (k-1)
               flow_areas(k) = flow_areas (k-1)
               perimeters(k) = perimeters (k-1)
               conveyances_posdir(k) = conveyances_posdir (k-1)
               conveyances_negdir(k) = conveyances_negdir (k-1)
               total_areas(k) = total_areas (k-1) 
               total_widths(k) = total_widths (k-1) 
            enddo

            levels(j) = zh
            nh = nh + 1
            if (nh == nhmax) then
               ! no more space in conveyance table
               j = nh+1
            endif

            flow_widths(j) = total_width
            flow_areas(j) = total_area
            perimeters(j) = p
            conveyances_posdir(j) = co
            conveyances_negdir(j) = co
            total_areas(j) = total_area
            total_widths(j) = total_width
         else
            ! Goto next level
            j = j+1
         endif

      else
         ! Goto next level
         j = j+1
      endif
      
      if (j==nh) then
         ! check if previous conveyance is large enough
         
         if (j==1 .or. conveyances_posdir(max(1,j-1)) < MINCONV) then
            levels(j+1) = levels(j)+EXTRA_HEIGHT
            call ConveyYZ(nc,y,z,frictype,friction_value_per_segment,levels(j+1),a,total_area,w,total_width,p,co)
            flow_widths(j+1) = total_width
            flow_areas(j+1) = total_area
            perimeters(j+1) = p
            conveyances_posdir(j+1) = co
            conveyances_negdir(j+1) = co
            total_areas(j+1) = total_area
            total_widths(j+1) = total_width
            nh = nh+1
         endif
         
      endif
      
    enddo  ! end do while

    if (jgetlevels .eq. 1) then
      numt = numt + num
      nump = nump + 1
    endif


    nhmax = max(nh,nhmax)

    if (nh .gt. lcnvmax) then

      call SetMessage( LEVEL_ERROR, &
          'Conveyance: Dimension error: total number of levels exceeded' )

    endif



999   continue

   !hk : dus na conveyancetables staan de totalen op plek 1, moet in code vlak hierboven
   !     waarschijnlijk nog veranderd worden. voordeel is dat we die optelling nu maar 1 keer hoeven te doen

  deallocate ( ik1, &
               yh, &
               dh, stat=ierr )
  if ( ierr .ne. 0 ) goto 9010


    return


 9000 continue
      call SetMessage( LEVEL_ERROR, &
          'ConCnv: Error allocating array space')

 9010 continue
      call SetMessage( LEVEL_ERROR, &
          'ConCnv: Error deallocating array space')

end subroutine CalcConveyanceTables

subroutine regulate_yz_coordinates(y, z, bedlevel, segmentToSectionIndex, nc, frictionSectionFrom, frictionSectionTo, sectionCount)
   implicit none

   double precision, dimension(:), intent(inout) :: y                      !< (Adapted) y-coordinates of the YZ cross section
   double precision, dimension(:), intent(inout) :: z                      !< (Adapted) z-coordinates of the YZ cross section
   double precision,               intent(  out) :: bedlevel               !< lowest level of the z-coordinates
   integer,                        intent(inout) :: nc                     !< Number of (y,z) coordinates
   integer, dimension(:),          intent(  out) :: segmentToSectionIndex  !< Table returns frictionIndex for segment (i)
   double precision, dimension(:), intent(in   ) :: frictionSectionFrom    !< Start coordinate of the friction section   
   double precision, dimension(:), intent(in   ) :: frictionSectionTo      !< End coordinate of the friction section
   integer,                        intent(in   ) :: sectionCount           !< Number of friction sections

   integer           :: i, j, ja, k, ncc, k1
   double precision  :: zmin, grndlyr, zground, f, yy, a, yya, yyb, dda

   grndlyr = 0d0         ! Ground layers are not implemented (yet).

   ! Determine lowest Z-level
   bedlevel = z(1)
   do j = 1, nc
      if (z(j) < bedlevel) then 
         bedlevel = z(j)
      endif
   enddo
   ! Set Z-coordinates to a base level of 0.
   do j = 1, nc
      z(j) = z(j) - bedlevel
   enddo

   ! remove double points 
   ja  = 1                       
   do while (ja .eq. 1)
      ja = 0
      do k = 2,nc
         if (k .le. nc .and. y(k) .eq. y(k-1) .and. z(k) .eq. z(k-1) ) then
            ja  = 1
            nc  = nc - 1
            do j = k,nc
               y(j) = y(j+1)
               z(j) = z(j+1)
            enddo
         endif
      enddo
   enddo

   ! STEP 2 
   ja  = 1                                ! dan middelste y-punten weghalen uit verticale stukken
   do while (ja .eq. 1)
      ja = 0
      do k = 2,nc-1
         if (k .le. nc-1 .and. y(k) .eq. y(k-1) .and. y(k) .eq. y(k+1) ) then
            ja  = 1
            nc  = nc - 1
            do j = k,nc
            y(j) = y(j+1)
            z(j) = z(j+1)
            enddo
         endif
      enddo
   enddo

      ! STEP 1: Adjust z for reference level and prevent horizontal segments 
   do j = 1, nc                 
      if (j>1) then
         if (z(j) == z(j-1)) then
            z(j) = z(j) + 0.0011
         endif
      endif
   enddo                        

   ja  = 1                                ! dan middelste z-punten weghalen uit horizontale stukken
   do while (ja .eq. 1)
   ja = 0
   do k = 2,nc-1
      if (k .le. nc-1 .and.  z(k) .eq. z(k-1) .and. z(k) .eq. z(k+1) ) then
         ja  = 1
         nc  = nc - 1
         do j = k,nc
         y(j) = y(j+1)
         z(j) = z(j+1)
         enddo
      endif
   enddo
   enddo


   !hk: hier moet de groundlayer erin gezet worden. moet redelijk netjes gebeuren,
   !    dus niet alleen min/max functies zoals eerst, maar insnijden aan zijkant

   zmin = 1e30
   do k = 1,nc
   zmin = min(zmin,z(k))
   enddo
   if (grndlyr .gt. 0.0d0) then
   zground = zmin + grndlyr !hk: is this o.k. ?
   else
   zground = -1e30
   endif

   ncc = nc

   do k = ncc-1,1,-1                   ! hk: eerst punten bijzetten
   if ( (z(k) .gt. zground .and. z(k+1) .lt. zground) .or. &
         (z(k) .lt. zground .and. z(k+1) .gt. zground) ) then
      f  = abs(zground-z(k))/abs(z(k+1) - z(k))
      yy = y(k) + f*(y(k+1)-y(k))
      nc = nc + 1
      do j = nc,k+2,-1
         y(j) = y(j-1)
         z(j) = z(j-1)
      enddo
      y(k+1) = yy
      z(k+1) = zground
   endif
   enddo

   j = 0                               !hk: dan weghalen wat onder zground zit

   do k = 1,nc
   if (z(k) .ge. zground) then
      j = j + 1
      y(j) = y(k)
      z(j) = z(k)   !hk: copie
   endif
   enddo

   nc = j

   ! na inzetten grondlaag het level van het laagste profielpunt

   ! start friction secties invoegen op overgangen moeten (eventueel) extra 
   ! steunpunten worden aangemaakt

   do i = 1, sectionCount                          !hk: do over the friction sections
      yya   = frictionSectionFrom(i)
      yyb   = frictionSectionTo(i)
      k1 = 0
      do k = 1,nc                            !hk: zoek punt links van yya grens
         if (y(k) .lt. yya) k1 = k
      enddo
      if ((k1 .ne. 0 .and. k1 .ne. nc) .and. (abs(y(k1+1) - yya) > 1d-4)) then   !hk: als nodig bepaal tussenpunt
         a   = (y(k1+1) - yya)/(y(k1+1) - y(k1))
         dda = z(k1)*a + z(k1+1)*(1-a)
         nc  = nc + 1
         do k = nc, k1+2,-1                  !hk: dan opschuiven
            y(k) = y(k-1)
            z(k) = z(k-1)
         enddo                               ! en tussenpunt zetten
         y(k1+1) = yya
         z(k1+1) = dda
      endif
      do k = k1+1,nc
         ! zet frictietypes
         !  if (z(k) .eq. zground .and. z(k+1) .eq. zground) then ! grondlaag kan alleen horizontaal zijn
         !    !  NOT IMPLEMENTED
         !  else
         segmentToSectionIndex(k) = i
         !  endif
      enddo
   enddo

! Einde aanpassingen tbv frictie secties

end subroutine regulate_yz_coordinates

subroutine ConveyanceTables(grndlyr, yin, z, nbo, crosssection_id, segmentToSectionIndex, &
                            frictionTypePos, frictionValuePos,        &
                            frictionTypeNeg, frictionValueNeg, frictype, friction_value_per_segment, levels,        &
                            nc,  jgetlevels, nhmax)

   use MessageHandling
   implicit none
   
   double precision :: grndlyr
   double precision :: yin(:)
   double precision :: z(:)
   integer          :: nhmax
   integer          :: nbo
   character(len=*) :: crosssection_id
   double precision groundfriction
   
   integer                    :: frictionTypePos(:)
   double precision           :: frictionValuePos(:)
   integer                    :: frictionTypeNeg(:)
   double precision           :: frictionValueNeg(:)
   integer                    :: segmentToSectionIndex(:)

   integer nc, ind
   integer frictype(:), icrds
   integer jgetlevels
   double precision              :: friction_value_per_segment(:)
   double precision              :: levels(:)
   logical                       :: prtout

   double precision, parameter         :: eps = 1d-4

   icrds    = 0
   
   if (nbo .gt. 0) ind = 1
   nbo = max(nbo,1)  ! please check
   
   IF (NBO .LE. 0) THEN
      call setMessage (LEVEL_ERROR, 'Zero friction value for cross section: '// trim(crosssection_id))
      return
   ENDIF
   
      prtout = .false.
      call calcConveyanceTables(frictype, friction_value_per_segment, yin, z, levels, nc,  jgetlevels, grndlyr, nbo,               &
                                segmentToSectionIndex, frictionTypePos, frictionValuePos,  &
                                frictionTypeNeg, frictionValueNeg, nhmax)
   return

   end subroutine
                 
     
subroutine regulate_levels(levels, nh)  ! sorteren en dubbele entries weghalen

   use qsort

   integer nh, ja, k, j

   double precision levels(nh)

   call d_qsort(levels, nh)

   ja  = 1                                ! en dubbele entries weghalen
   do while (ja .eq. 1)
      ja = 0
      do k = 2,nh
      if (k .le. nh .and. abs(levels(k) - levels(k-1)) < 1e-8 ) then
         ja   = 1
         nh   = nh - 1
         do j = k,nh
            levels(j) = levels(j+1)
         enddo
      endif
      enddo
   enddo

   return

end subroutine regulate_levels
    
subroutine ConveyYZ(n,y,z,frictype,cf,hw,flow_area,total_area,w,total_width,p,co) ! conveyance computation for a YZ profile
! in
integer          :: n                          ! number of profile points
double precision :: y(n), z(n)                 ! original YZ profile description (Z positive up),
                                               ! also breakpointed total_area friction segment points. no more.......
integer          :: frictype(n)                      ! friction types for segments (1, n-1)
double precision :: cf(n)                      ! friction coefficients for segments (1, n-1)
double precision :: hw                         ! water level for which a,w,p,co must be calculated

! out
double precision :: flow_area, total_area                     ! area flow, total
double precision :: w, total_width                      ! flow width, total
double precision :: p                          ! perimeter
double precision :: co                         ! conveyance pos dir

!local
integer          :: k, ct
double precision :: cfv                      ! friction coefficients for segments (1, n-1)
double precision :: z0,z1,d0,d1,dz,y0,y1,bb,bt
double precision :: aa,ww,pp,cc

flow_area = 0 
total_area = 0 
w = 0
total_width = 0
p = 0
co  = 0

do k = 1,n-1

   z0 = z(k)
   z1 = z(k+1)
   d0 = max(hw - z0,1.0d-6)                                ! depth left
   d1 = max(hw - z1,1.0d-6)                                ! depth right
   dz = dabs(z1 - z0)

   y0 = y(k)   - y(k)
   y1 = y(k+1) - y(k)
   bb = dabs(y1 - y0)         ! breedte segment
   bt = 0.0d0
   if(bb.ne.0.d0) bt = (z1-z0)/(y1-y0) ! beta = tan(phi)

   ct = frictype(k) ! type
   cfv = max(cf(k),1.0e-10)
   if(.NOT. (d0.le.1.0d-6.and.d1.le.1.0d-6)) then
     call ConveySeg (y0,y1,d0,d1,bt,bb,dz,ct,cfv,aa,ww,pp,cc) ! segment routine
     total_area = total_area + aa
    total_width = total_width + ww
     if (cfv .ne. 0.0) then
         w = w + ww
       flow_area = flow_area + aa
       p = p + pp
       co = co + cc   ! totals
     endif
   endif
enddo

end subroutine ConveyYz


subroutine ConveySeg(y0,y1,d0,d1,bt,bb,dz,ct,cf,a,w,p,co)  ! conveyance computation for a segment
! in
integer          :: ct
double precision :: y0, y1                     ! left and right y values   (m)
double precision :: d0, d1                     ! left and right waterdepth (m), always either hl > 0 or hr > 0
double precision :: bt                         ! beta = tan(phi)
double precision :: bb                         ! y1 - y0 (m)
double precision :: dz                         ! |z1-z0|
double precision :: cf                         ! roughness coefficient

! out
double precision :: a                          ! area       (m2)
double precision :: w                          ! width      (m)
double precision :: p                          ! perimeter  (m)
double precision :: co                         ! conveyance (m2/s)

! locals

double precision, parameter :: sixth = -1d0/6d0  ! for power law
double precision, parameter :: s83   = 8d0/3d0   ! for power law
double precision, parameter :: s52   = 5d0/2d0   ! for power law
double precision, parameter :: s53   = 5d0/3d0   ! for power law
double precision, parameter :: s32   = 3d0/2d0   ! for power law
double precision, parameter :: s14   = 1d0/4d0   ! for power law
double precision, parameter :: s25   = 2d0/5d0   ! for power law
double precision            :: c1, c2, dcf, f1, f2    

co = 0.0d0
dcf = cf
if(ct.eq.3) then
   c1 = (1.0d0+bt**2)**s14*dlog(10.0d0)
   c2 = dcf/12.0d0
endif
!
if(d0.lt.dz.or.d1.lt.dz) then   ! beta#0
   if(bt.lt.-0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s52
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (3)                   ! White-Colebrook (kn)
         if(d1/c2.le.1.495d0) then 
            f1 = 2.13d-3
         else
            f1 = dlog(d1/c2)-s25
         endif
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*d1**s52*f1
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**3
      end select
   else if(bt.ge.-0.01.and.bt.lt.0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/(((1.0d0+bt**2)**s14))*(d1/2.0d0)**s32*(-d1/bt)
      case (1)                   ! Manning (n)
         co = 1/(dcf*(1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*d1/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*d1/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*(-d1/bt)*(d1/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*(d1/2.0d0)**2*(-d1/bt)
      end select
  else if(bt.le.0.01.and.bt.gt.0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s32*(d0/bt)
      case (1)                   ! Manning (n)
         co = 1/(dcf*(1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*d0/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*d0/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*(d0/bt)*(d0/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**2*(d0/bt)
      end select
  else if(bt.gt.0.01) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s52
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (3)                   ! White-Colebrook (kn)
         if(d0/c2.le.1.495d0) then 
            f1 = 2.13d-3
         else
            f1 = dlog(d0/c2)-s25
         endif
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*d0**s52*f1
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**3
      end select
  endif
endif
if(d0.ge.dz.or.d1.ge.dz) then   
  if(bt.ge.-0.01d0.and.bt.le.0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s32*bb
      case (1)                   ! Manning (n)
         co = 1.0d0/(dcf*(1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*(d0+d1)/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*(d0+d1)/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*bb*((d0+d1)/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**2*bb
      end select
  elseif (dabs(bt) .gt. 0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s52-d1**s52)
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
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
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*dabs(d0**s52*f1-(d1**s52*f2))
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**3-d1**3)
      end select
  endif
endif
call compwap(y0,y1,d0,d1,dz,w,a,p)
end subroutine ConveySeg

subroutine compwap(yL,yr,hL,hr,dz,w,a,p)
double precision :: hL,hr,dz,yL,yR,f,w,a,p
double precision :: h1, h2                     ! min, max depth
double precision :: dh                         ! depth dif

   if (max(hL,hr) .gt. 1.0d-6) then               ! both wet or one wet
      if (min(hL,hr) .gt. 1.0d-6) then            ! both wet
!
      else if (hL .gt. 1.0d-6) then               ! only left wet
         f  = hl/dz
         yr = f*yr
       hr = 0d0
      else if (hr .gt. 1.0d-6) then               ! only rigth wet
         f  = hr/dz 
         yr = f*yr
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

   w  = yr - yL
   a  = 0.5d0*( h1 + h2 )*w
   p  = dsqrt ( w*w  + dh*dh )
end subroutine compwap

subroutine write_conv_tab(convtab)
   use messagehandling
   
   type(t_crsu), intent(in)      :: convtab
   
   integer :: nlevels
   integer :: i
   
   write(msgbuf, '(''Number of levels in Conveyance table = '', i5)') convtab%nru
   call msg_flush()
   
   write(msgbuf,'(''Extrapolation factor a (positive direction)'', g14.6)') convtab%a_pos_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor a (negative direction)'', g14.6)') convtab%a_neg_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor b (positive direction)'', g14.6)') convtab%b_pos_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor b (negative direction)'', g14.6)') convtab%b_neg_extr
   call msg_flush()
   write(msgbuf,'(11a17)') 'Water_depth', 'Total_width', 'Flow_width', 'Total_Area', 'Flow_Area', 'Conv_pos_dir', &
               'Conv_neg_dir', 'Perimeter'
   call msg_flush()

   nlevels = convtab%nru
   do i = 1, nlevels
      write(msgbuf, '(11g17.6)') convtab%height (i) , convtab%total_width(i), convtab%flow_width (i), convtab%total_area(i), convtab%flow_area (i), &
                                 convtab%conveyance_pos(i), convtab%conveyance_neg(i), convtab%perimeter (i)
      call msg_flush()
   enddo 
   
end subroutine write_conv_tab

end module M_newcross           ! new type conveyance table crossections



