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
public regulatehlv
public dealloc
public write_conv_tab
public regulate_yz_coordinates

 integer, parameter                                     :: ncx = 144
 integer, parameter, public                             :: CS_LUMPED    = 0 !< Lumped option for yz-type conveyance calculation.
 integer, parameter, public                             :: CS_VERT_SEGM = 1 !< Vertically segmented option for yz-type conveyance calculation.
 integer, public     :: lcnvmax
 
 interface dealloc
    module procedure deallocCru
 end interface dealloc

type, public :: t_crsu
   integer                                         :: jopen                      !< open/closed profile, 1/0
   integer                                         :: msec                       !< number of friction sections (horreur only for postpro)
   integer                                         :: nru                        !< number of levels in u tables
   integer                                         :: iolu   = 1                 !< latest level found, initialise total_area bottom
   integer                                         :: negcon = 0                 !< different conveyance for negative flow? 0/1
   integer                                         :: conveyType = CS_VERT_SEGM  !< calculation type for conveyance (lumped or vertically segmented)
   double precision                                :: a_pos_extr                 !< Extrapolation Factor for YZ-Profiles
   double precision                                :: a_neg_extr                 !< Extrapolation Factor for YZ-Profiles
   double precision                                :: b_pos_extr                 !< Extrapolation Factor for YZ-Profiles
   double precision                                :: b_neg_extr                 !< Extrapolation Factor for YZ-Profiles
   double precision                                :: bedlevel                   !< Lowest point of the YZ cross setction
   double precision, allocatable                   :: water_depth (:)            !< heights for next (u) tables:
   double precision, allocatable                   :: flow_area (:)              !< flow area  ft of h, for all sections
   double precision, allocatable                   :: flow_width (:)             !< flow width (not double precisionly needed)
   double precision, allocatable                   :: perimeter (:)              !< wet perimeter (only postpro)
   double precision, allocatable                   :: conveyance_pos(:)          !< conveyance positive flow direction
   double precision, allocatable                   :: conveyance_neg(:)          !< conveyance negative flow direction
   double precision, allocatable                   :: chezy_pos(:)               !< chezy posflow
   double precision, allocatable                   :: chezy_neg(:)               !< chezy negflow
   double precision, allocatable                   :: total_area(:)              !< total area 
   double precision, allocatable                   :: total_width(:)             !< total width

   integer                                         :: last_position              !< last position used for interpolation
   double precision                                :: chezy_act                  !< Actual Chezy
                                                                                 !< here stored for output
                                                                                 !< used in function ChezyFromConveyance
   
end type t_crsu

double precision, allocatable, public::    &
         ihlev (:),      &
         iwft  (:),      &
         iaft  (:),      &
         ipft  (:),      &
         ikpt  (:),      &
         iknt  (:),      &
         ifcp  (:),      &
         ifcn  (:),      &
         iwtt  (:),      &
         iatt  (:),      &
         crsn_lc(:)
integer, allocatable, save, public :: cvip_cp(:,:)

integer, public                                        :: nupt
integer, public                                        :: msect

contains

subroutine deallocCru(cru)
   ! Modules
   
   implicit none
   
   ! Input/output parameters
   type(t_crsu), pointer, intent(inout)     :: cru
   
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
      if (allocated(cru%water_depth ))    deallocate(cru%water_depth )
      if (allocated(cru%total_area ))     deallocate(cru%total_area )
      if (allocated(cru%total_width ))    deallocate(cru%total_width )
      deallocate(cru)
   endif
      
end subroutine deallocCru

subroutine generateConvtab(convtab, levelsCount, href, grndlyr, typ, &
                           nc, nbo, branchid, bedFrictionType, groundFriction, &
                           yin, z, segmentToSectionIndex, frictionTypePos, &
                           frictionValuePos, frictionTypeNeg, frictionValueNeg)

   implicit none
   
   type (t_crsu), pointer, intent(inout) :: convtab
   integer, intent(in)        :: levelsCount
   double precision           :: href
   double precision           :: grndlyr
   double precision           :: yin(:)
   double precision           :: z(:)
   double precision           :: groundfriction
   integer                    :: typ
   integer                    :: nbo
   integer                    :: nc
   integer                    :: branchid
   integer                    :: bedfrictiontype
   integer                    :: segmentToSectionIndex(:)
   integer                    :: frictionTypePos(:)
   double precision           :: frictionValuePos(:)
   integer                    :: frictionTypeNeg(:)
   double precision           :: frictionValueNeg(:)
 
   double precision, allocatable :: vf(:), vg(:)
   double precision, allocatable :: y(:), d(:), hlv(:)
   integer, allocatable          :: frictype(:), jg(:)
   integer, allocatable          :: indx  (:)
   integer                       ::  jgetlevels, nnlev, ierr

   ! Variables for calculating extrapolation coefficients:
   integer              :: i1 ! one but last index
   integer              :: i2 ! last index
   double precision     :: h_1
   double precision     :: h_2
   double precision     :: K_1
   double precision     :: K_2

   double precision ::  zminprof ! absolute z level lowest profile point  
!
   if (.not. associated(convtab)) then
      allocate(convtab)
   endif

   lcnvmax = max(400, levelsCount * 4)
  
   allocate (ihlev(lcnvmax) ,      &
             iwft(lcnvmax) ,   &
             iaft(lcnvmax) ,   &
             ipft(lcnvmax) ,   &
             ifcp(lcnvmax) ,   &
             ifcn(lcnvmax) ,   &
             ikpt(lcnvmax) ,   &
             iknt(lcnvmax) ,   &
             iwtt(lcnvmax) ,    &
             iatt(lcnvmax) ,    &
             indx(lcnvmax) , stat=ierr )
   
   allocate (vf(lcnvmax),  &
             vg(lcnvmax),  &
             y(lcnvmax),   &
             d(lcnvmax*2), &
             frictype(lcnvmax),  &
             jg(lcnvmax),  &
             hlv(lcnvmax*2), &
             stat=ierr )

   iwft = 0 !-9
   iaft = 0 !-9
   ipft = 0 !-9
   ifcp = 0 !-9
   ifcn = 0 !-9
   ikpt = 0 !-9
   iknt = 0 !-9
   iwtt = 0 !-9
   iatt = 0 !-9
   indx = 9 !-9
   zminprof = 0

   nnlev = 2
   jgetlevels = 1


   call ConveyanceTables(href, grndlyr, typ, yin, z, nbo, branchid, bedFrictionType,   &
                         groundFriction, segmentToSectionIndex, frictionTypePos, frictionValuePos,         &
                         frictionTypeNeg, frictionValueNeg, frictype, vf, y, d, hlv,         &
                         nc,  jgetlevels, zminprof, nnlev)

   convTab%jopen  = 1
   convTab%msec   = 1 
   convTab%nru    = nnlev   
   convTab%iolu   = 1
   convTab%negcon = 0
   
   
   allocate(convTab%water_depth (nnlev)    )
   allocate(convTab%flow_area (nnlev)  )
   allocate(convTab%flow_width (nnlev)  )
   allocate(convTab%perimeter (nnlev)  )
   allocate(convTab%conveyance_pos(nnlev)  )
   allocate(convTab%conveyance_neg(nnlev)  )
   allocate(convTab%chezy_pos(nnlev)  )
   allocate(convTab%chezy_neg(nnlev)  )
   allocate(convTab%total_area(nnlev)   )
   allocate(convTab%total_width(nnlev)   )
   
   convTab%water_depth     = hlv(1:nnlev)
   convTab%flow_area       = iaft(1:nnlev)
   convTab%flow_width      = iwft(1:nnlev)
   convTab%perimeter       = ipft(1:nnlev)
   convTab%conveyance_pos  = ikpt(1:nnlev)
   convTab%conveyance_neg  = iknt(1:nnlev)
   convTab%chezy_pos       = ifcp(1:nnlev)
   convTab%chezy_neg       = ifcn(1:nnlev)
   convTab%total_area      = iatt(1:nnlev)
   convTab%total_width     = iwtt(1:nnlev)
   
   ! calculate the coefficients for extrapolation of conveyance above specified profile
  !(*)   !  document SOBEK-21942: Change of roughness formulations in "Y-Z" and
   ! "Asymetrical Trapezium" profiles, Author:     Thieu van Mierlo
   !                                   Programmer: Daniel Abel
   i1  = convTab%nru - 1     ! so i1, i2 always inside table
   i2  = i1 + 1
   !
   h_1 = convTab%water_depth(i1)
   h_2 = convTab%water_depth(i2)
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
   
   
   deallocate (ihlev ,     &
               iwft  ,     &
               iaft  ,     &
               ipft  ,     &
               ikpt  ,     &
               iknt  ,     &
               ifcp  ,     &
               ifcn  ,     &
               iatt  ,     &
               iwtt  , stat=ierr )
   deallocate( vf, vg, y, d, frictype, jg, hlv)

end subroutine generateConvtab

subroutine CalcConveyanceTables(frictype, vf, y, z, hlv, nc,  jgetlevels, zminpr,                        &
                                grndlyr, nbo, segmentToSectionIndex,                  &
                                frictionTypePos, frictionValuePos, frictionTypeNeg, frictionValueNeg,  &
                                cg, nhmax)
    use MessageHandling
    
    implicit none
    
    ! Input output
    integer, intent(  out)            :: frictype(lcnvmax)        !< friction type at the segments (segment i is between y(i) and y(i+1))
    
    integer  nh,  jgetlevels, lu_dump
    integer          nc
    integer          nbo
    
    double precision           ::  vf(lcnvmax)
    double precision           ::  cg(2)
    double precision           :: hlv(lcnvmax*2), y(lcnvmax), z(lcnvmax*2), zminpr
    double precision           :: grndlyr
    integer                    :: segmentToSectionIndex(:)
    integer                    :: frictionTypePos(nbo)
    double precision           :: frictionValuePos(nbo)
    integer                    :: frictionTypeNeg(nbo)
    double precision           :: frictionValueNeg(nbo)

    ! Local variables

    integer numt, nump, nhi, nhmax
    integer ierr, k, j, ncc, i, io, num
    integer ja, k2, numpunt, k1
    integer nbn
    double precision               :: yya, yyb
    double precision               :: cp(2), cn(2)
    double precision               :: zmin, zh, zl, zr, dy, dz, zground, yy, f
    double precision               :: area, ar1, ar2, a, dda
    double precision               :: w, p, co, total_area, total_width, conv, accuracy, width
    double precision               :: dif1, dif2, hlvz
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
      vf(k)       = frictionValuePos(i)
   enddo

    if (jgetlevels .eq. 1) then ! hier wordt eerste hoogtetabel opgebouwd
      do k = 1,nc
        hlv(k) = z(k)
      enddo          
      
      do k = 2,nc
        if ( z(k) .eq. z(k-1) ) then  ! floodplane, til verticaal tabelpunt iets op
          hlv(k) = hlv(k) + 1d-4      ! voor scherper zien van breekpunt in tabel
        endif
      enddo
      nh = nc
      call regulatehlv(hlv,nh)
    endif

    nhi = nh

    iwft(:) = 0
    iaft(:) = 0
    ipft(:) = 0
    ikpt(:) = 0
    iknt(:) = 0
    iatt(:)   = 0
    iwtt(:)   = 0

    num = 0
    j   = 0
    io  = 0

    do j = 1, nh
      call ConveyYZ(nc,y,z,frictype,vf,hlv(j),a,total_area,w,total_width,p,co)
      iwft(j) = total_width
      iaft(j) = total_area
      ipft(j) = p
      ikpt(j) = co
      iknt(j) = co
      iatt(j) = total_area
      iwtt(j) = total_width
    enddo
    
    ! for the number of levels
    j = 2
    do while (j .le. nh)
      dz = hlv(j) - hlv(j-1)
      
      if (dz .gt. 3.0d-3) then
         
         zh = ( hlv(j)+hlv(j-1) )*0.5d0
         call ConveyYZ(nc,y,z,frictype,vf,zh,a,total_area,w,total_width,p,co)

         conv = 0.5d0*ikpt(j) + 0.5d0*ikpt(j-1)
         co = max(co,1d-6)
         dif2 = abs(conv - co)/(co)
         accuracy = 0.01d0

         if ( dif2 .gt. accuracy ) then  ! hk: en zolang (nh .lt. size(hlv) )

            do k = nh+1, j+1,-1
               hlv (k) = hlv(k-1)
               iwft(k) = iwft (k-1)
               iaft(k) = iaft (k-1)
               ipft(k) = ipft (k-1)
               ikpt(k) = ikpt (k-1)
               iknt(k) = iknt (k-1)
               iatt(k) = iatt (k-1) 
               iwtt(k) = iwtt (k-1) 
            enddo

            hlv(j) = zh
            nh = nh + 1
            if (nh == nhmax) then
               ! no more space in conveyance table
               j = nh+1
            endif

            iwft(j) = total_width
            iaft(j) = total_area
            ipft(j) = p
            ikpt(j) = co
            iknt(j) = co
            iatt(j) = total_area
            iwtt(j) = total_width
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
         
         if (j==1 .or. ikpt(max(1,j-1)) < MINCONV) then
            hlv(j+1) = hlv(j)+EXTRA_HEIGHT
            call ConveyYZ(nc,y,z,frictype,vf,hlv(j+1),a,total_area,w,total_width,p,co)
            iwft(j+1) = total_width
            iaft(j+1) = total_area
            ipft(j+1) = p
            ikpt(j+1) = co
            iknt(j+1) = co
            iatt(j+1) = total_area
            iwtt(j+1) = total_width
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

subroutine ConveyanceTables(href, grndlyr, typ, yin, z, nbo, branchid, bedFrictionType,  &
                            groundFriction, segmentToSectionIndex, frictionTypePos, frictionValuePos,        &
                            frictionTypeNeg, frictionValueNeg, frictype, vf, y, d, hlv,        &
                            nc,  jgetlevels, zminpr, nhmax)

   use MessageHandling
   implicit none
   
   double precision :: href
   double precision :: grndlyr
   double precision :: yin(:)
   double precision :: z(:)
   integer          :: bedFrictionType
   integer          :: nhmax
   integer          :: nbo
   integer          :: branchid
   double precision groundfriction
   
   integer                    :: frictionTypePos(:)
   double precision           :: frictionValuePos(:)
   integer                    :: frictionTypeNeg(:)
   double precision           :: frictionValueNeg(:)
   integer                    :: segmentToSectionIndex(:)

   integer nc, ind, typ
   integer frictype(:), icrds
   integer j, jgetlevels
   double precision              :: vf(:)
   double precision              :: cg(2)
   double precision              :: hlv(:), d(:), y(:), zminpr
   logical                       :: prtout
   character*10                  :: id

   double precision, parameter         :: eps = 1d-4

   icrds    = 0
   
   if (nbo .gt. 0) ind = 1
   nbo = max(nbo,1)  ! please check
   
   IF (NBO .LE. 0) THEN
      write(id, '(i0)') branchid
      call setMessage (LEVEL_ERROR, 'Zero friction value in branch '// trim(id))
      return
   ENDIF
   
   cg(1) = 0.
   cg(2) = 0.
   
   if (grndlyr .gt. 0.5) then    ! ground layer coeffs
      cg(1) = bedFrictionType
      cg(2) = groundFriction
   endif
     
   if (typ.ge.10) then
      prtout = .false.
      call calcConveyanceTables(frictype, vf, yin, z, hlv, nc,  jgetlevels, zminpr, grndlyr, nbo,               &
                                segmentToSectionIndex, frictionTypePos, frictionValuePos,  &
                                frictionTypeNeg, frictionValueNeg, cg, nhmax)

   endif

   return

   end subroutine
                 
     
subroutine regulatehlv(hlv, nh)  ! sorteren en dubbele entries weghalen

   use qsort

   integer nh, ja, k, j

   double precision hlv(nh)

   call d_qsort(hlv, nh)

   ja  = 1                                ! en dubbele entries weghalen
   do while (ja .eq. 1)
      ja = 0
      do k = 2,nh
      if (k .le. nh .and. abs(hlv(k) - hlv(k-1)) < 1e-8 ) then
         ja   = 1
         nh   = nh - 1
         do j = k,nh
            hlv(j) = hlv(j+1)
         enddo
      endif
      enddo
   enddo

   return

end subroutine regulatehlv
    
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
      write(msgbuf, '(11g17.6)') convtab%water_depth (i) , convtab%total_width(i), convtab%flow_width (i), convtab%total_area(i), convtab%flow_area (i), &
                                 convtab%conveyance_pos(i), convtab%conveyance_neg(i), convtab%perimeter (i)
      call msg_flush()
   enddo 
   
end subroutine write_conv_tab

end module M_newcross           ! new type conveyance table crossections



