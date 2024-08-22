module geometry_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!!--description-----------------------------------------------------------------
!
!    Function: - Various geometrical routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   use MessageHandling, only: msgbox, mess, LEVEL_ERROR
   implicit none

   private
      
   !
   ! functions and subroutines
   !
   
   ! original function of the geometry_module
   public :: clockwise

   public :: pinpok   
   public :: dpinpok
   public :: dbdistance
   public :: getdx
   public :: getdy
   public :: getdxdy
   public :: sphertocart3D
   public :: cart3Dtospher
   public :: dbpinpol
   public :: cross
   public :: dbpinpol_optinside_perpol
   public :: dbpinpol_optinside_perpol2
   public :: get_startend
   public :: pinpok3D
   public :: cross3D
   public :: inprod
   public :: vecprod
   public :: matprod
   public :: gaussj
   public :: ave3D
   !from rest.F90 
   public :: crossinbox          ! line 38
   public :: dlinedis            ! line 3889 
   public :: dprodout            ! line 3941
   public :: dcosphi             ! line 3986
   public :: spher2locvec        ! line 4391
   public :: spher2locvec2       ! line 4391
   public :: normalin            ! line 4825
   public :: normalout           ! line 4884
   public :: normaloutchk        ! line 4948  
   public :: duitpl              ! line 4993
   public :: half                ! line 7454
   public :: xpav                ! line 7510
   !from net.F90 
   public :: comp_masscenter     ! line 32041
   public :: comp_masscenter2D   ! line 32063
   public :: comp_masscenter3D   ! line 32177
   public :: comp_circumcenter3D ! line 33419
   !from unstruct.f90
   public :: GETCIRCUMCENTER     ! line 18653
   public :: dotp                ! line 18843
   public :: circumcenter3       ! line 18851
   public :: comp_breach_point
   
   integer, public :: ipolyfound

   interface clockwise
      module procedure clockwise_sp
      module procedure clockwise_hp
   end interface clockwise

   contains

   !> projects a point to a polyline and finds the closest link.
   !! "Closest" is based on the intersection location of each flow link with the input polyline.
   subroutine comp_breach_point(startLocationX, startLocationY, xp, yp, np, xl, yl, Lstart, x_breach, y_breach, jsferic, jasfer3D, dmiss)

   implicit none

   !input
   double precision, intent(in   ) :: startLocationX, startLocationY !< Input coordinates of the start location of the breach.
   double precision, intent(in   ) :: xp(:), yp(:)       !< Dambreak polyline points.
   integer,          intent(in   ) :: np                 !< Number of input polyline points.
   double precision, intent(in   ) :: xl(:,:), yl(:,:)   !< (2,nlinks) Start-end points of the intersected flow links, used for selecting the nearest link to the start location.
   integer,          intent(  out) :: Lstart             !< Resulting index of the flow link closest to the startlocationX,Y.
   double precision, intent(  out) :: x_breach, y_breach !< Snapped x,y coordinates of the selected flow link's intersection with the polyline.
   double precision, intent(in   ) :: dmiss              !< Missing value used in the input polyline arrays.
   integer,          intent(in   ) :: jsferic, jasfer3D  !< Input coordinate type (sferic=1, cartesian=0)
   
   !locals
   integer                                   :: k, ja, i, jacros
   double precision                          :: dis, distemp, xn, yn, xntempa, yntempa, xc, yc, crpm, sm, sl
   

   ! Project the start of the breach on the polyline, find xn and yn
   !if(.not.allocated(xp)) return
   !if(.not.allocated(yp)) return
   xn = dmiss
   yn = dmiss
   dis = huge(dmiss)
   do k  = 1, np - 1
      call dlinedis(startLocationX, startLocationY, xp(k), yp(k), xp(k + 1), yp(k + 1), ja, distemp, xntempa, yntempa, jsferic, jasfer3D, dmiss)
      if (distemp <= dis ) then
         xn  = xntempa
         yn  = yntempa
         dis = distemp
      endif
   enddo

   ! Assign the flow links and the starting link of the breach
   dis = huge(dmiss)      
   do k = 1, size(xl, 1)
      ! calculate the coordinates of the intersection of the 1d2d link with the dambreak polygon
      do i = 1, np-1
         call cross (xp(i), yp(i), xp(i+1), yp(i+1), xl(k, 1), yl(k,1), xl(k,2), yl(k,2), jacros, sl, sm, xc, yc, crpm, jsferic, dmiss)
         if (jacros == 1) then
            exit
         end if
      enddo
    
      ! calculate the distance with projected start of the breach
      distemp = dbdistance(xn, yn, xc, yc, jsferic, jasfer3D, dmiss)
      ! identify the closest link to the projected point
      if (distemp <= dis) then
         Lstart   = k
         x_breach = xc
         y_breach = yc
         dis      = distemp
      endif
   enddo
   
   end subroutine comp_breach_point   

      !> Checks orientation of a polygon in single precision.
      function clockwise_sp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(sp), dimension(:), intent(in) :: x   !< x-coordinates
          real(sp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer                             :: n        !< number of points in polygon
          real(hp), dimension(:), allocatable :: xhp      !< temporary double precision x-coordinates
          real(hp), dimension(:), allocatable :: yhp      !< temporary double precision y-coordinates
          
          n = size(x)
          allocate(xhp(n),yhp(n))
          xhp = real(x,hp)
          yhp = real(y,hp)
          cw = clockwise_hp(xhp,yhp)
          deallocate(xhp,yhp)
      end function clockwise_sp

      !> Checks orientation of a polygon in high precision.
      function clockwise_hp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(hp), dimension(:), intent(in) :: x   !< x-coordinates
          real(hp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer :: i        !< loop variable
          integer :: i0       !< index of lowest left most point
          integer :: in       !< index of next point (not equal to x0,y0)
          integer :: ip       !< index of previous point (not equal to x0,y0)
          integer :: n        !< number of points in polygon
          real(hp) :: an      !< angle of next point compared to horizontal x-axis
          real(hp) :: ap      !< angle of previous point compared to horizontal x-axis
          real(hp) :: x0      !< x-coordinate of point i0
          real(hp) :: y0      !< y-coordinate of point i0
          
          !
          ! select lowest left most point
          !
          n = size(x)
          x0 = x(1)
          y0 = y(1)
          i0 = 1
          do i = 2, n
              if ( x(i)<x0 ) then
                  x0 = x(i)
                  y0 = y(i)
                  i0 = i
              elseif (x(i)==x0 .and. y(i)<y0) then
                  y0 = y(i)
                  i0 = i
              endif
          enddo
          !
          ! find point before
          ! note that this will give an infinite loop if n==1 or more generally if all(x==x0 and y==y0)
          !
          ip = i0
          do while (x(ip)==x0 .and. y(ip)==y0)
              if (ip==1) then
                  ip = n
              else
                  ip = ip-1
              endif
          enddo
          !
          ! find point after
          !
          in = i0
          do while (x(in)==x0 .and. y(in)==y0)
              if (in==n) then
                  in = 1
              else
                  in = in+1
              endif
          enddo
          !
          ! if "point after" lies above "point before" the orientation is clockwise ...
          !
          an = atan2(y(in)-y0,x(in)-x0)
          ap = atan2(y(ip)-y0,x(ip)-x0)
          cw = an>ap
      end function clockwise_hp
      
      !
      ! PINPOK
      !
      subroutine pinpok(XL, YL, N, X, Y, INSIDE, jins, dmiss) ! basic subroutine
      
      implicit none
      
      integer                      :: N, INSIDE
      integer, intent(in)          :: jins
      double precision, intent(in) :: dmiss
      double precision             :: X(N), Y(N), XL, YL

      integer          :: i, i1, i2, np
      double precision :: rechts, x1, x2, y1, y2, rm, rl

      IF (N .LE. 2) THEN
         INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
      ELSE
         NP = 0
5        CONTINUE
         NP = NP + 1
         IF (NP .LE. N) THEN
            IF ( X(NP) .NE. dmiss) GOTO 5
         ENDIF
         NP = NP - 1
         INSIDE = 0
         RECHTS = 0
         I = 0
10       CONTINUE
         I1 = I + 1  !MOD(I,NP) + 1
         I2 = I1 + 1 !MOD(I1,NP) + 1
         IF (I2 > NP) I2 = 1
         X1 = X(I1)
         X2 = X(I2)
         Y1 = Y(I1)
         Y2 = Y(I2)
         IF (XL .GE. MIN(X1,X2) .AND. XL .LE. MAX(X1,X2) ) THEN
            !           tussen of op lijnstuk
            IF (XL .EQ. X1 .AND. YL .EQ. Y1 .OR.                                     &
               !              op punt 1
               (X1 .EQ. X2 .AND. YL .GE. MIN(Y1,Y2) .AND. YL .LE. MAX(Y1,Y2) ) .OR.  &
               !              op verticale lijn
               (YL .EQ. Y1 .AND. Y1 .EQ. Y2)  ) THEN
            !              op horizontale lijn
            INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
            RETURN
            ELSE IF (X1 .NE. X2) THEN
               !              scheve lijn
               RL = ( XL - X1 )  / ( X2 - X1 )
               RM = ( Y1 - YL )  + RL * ( Y2 - Y1 )
               IF (RM .EQ. 0) THEN
                  !                 op scheve lijn
                  INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
                  RETURN
               ELSE IF (RM .GT. 0d0) THEN
                  !                 onder scheve lijn
                  IF (XL .EQ. X1 .OR. XL .EQ. X2) THEN
                     IF (X1 .GT. XL .OR. X2 .GT. XL) THEN
                        RECHTS = RECHTS + 1
                     ENDIF
                  ENDIF
                  INSIDE = 1 - INSIDE
               ENDIF
            ENDIF
         ENDIF
         I = I + 1
         IF (I .LT. NP) GOTO 10
         IF (MOD(RECHTS,2d0) .NE. 0) INSIDE = 1 - INSIDE
      ENDIF
      IF (jins .EQ. 0) INSIDE = 1 - INSIDE
      RETURN
      END SUBROUTINE PINPOK

      !
      ! DPINPOK
      !
      subroutine DPINPOK(X, Y, Z, NP, XP, YP, INSIDE, jins, dmiss)  ! multiple basic + jins (inside=1/outside=0) check
            
      implicit none
      
      double precision             :: X,Y,Z
      integer                      :: NP, INSIDE
      integer, intent(in)          :: jins
      double precision, intent(in) :: dmiss
      double precision             :: XP(NP), YP(NP)

      integer                                     :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                     :: istart, iend   ! point to start and and node of a polygon in polygon array respectively

      IF (NP .LE. 2) THEN
         INSIDE = 1
      ELSE

         ipoint = 1

         INSIDE = 0
         do while ( ipoint < NP )
            !        get polygon start and end pointer respectively
            call get_startend(NP-ipoint+1,xp(ipoint:NP),yp(ipoint:NP), istart, iend, dmiss)
            istart = istart+ipoint-1
            iend   = iend  +ipoint-1

            if ( istart >= iend .or. iend > NP ) exit ! done
            CALL PINPOK(X, Y, iend-istart+1, XP(istart:iend), YP(istart:iend), INSIDE, jins, dmiss)

            if ( INSIDE == 1 .and. JINS == 1 ) exit
            if ( INSIDE == 0 .and. JINS == 0 ) exit

            !        advance pointer
            ipoint = iend+2
         end do   ! do while ( ipoint < NP )
      ENDIF
      RETURN
      END SUBROUTINE DPINPOK

      !> compute distance from (x1,y1) to (x2,y2)
      double precision function dbdistance(x1,y1,x2,y2, jsferic, jasfer3D, dmiss)    ! distance point 1 -> 2
      implicit none
      double precision, intent(in) :: x1, y1, x2, y2
      ! locals
      double precision              :: ddx, ddy, rr
      double precision              :: xx1, yy1, zz1, xx2, yy2, zz2
      integer, intent(in)           :: jsferic, jasfer3D
      double precision, intent(in)  :: dmiss

      if ( x1 == dmiss .or. x2 == dmiss .or. y1 == dmiss .or. y2 == dmiss ) then
         dbdistance = 0d0
         return
      end if

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         call sphertocart3D(x1,y1,xx1,yy1,zz1)
         call sphertocart3D(x2,y2,xx2,yy2,zz2)
         dbdistance = sqrt( (xx2-xx1)**2 + (yy2-yy1)**2 + (zz2-zz1)**2 )
      else
         ddx = getdx(x1,y1,x2,y2,jsferic)
         ddy = getdy(x1,y1,x2,y2,jsferic)
         rr  = ddx*ddx + ddy*ddy
         if (rr == 0d0) then
            dbdistance = 0d0
         else
            dbdistance = sqrt(rr)
         endif
      endif

      end function dbdistance

      !
      ! getdx
      !
      double precision function getdx(x1,y1,x2,y2, jsferic)

      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole
      implicit none
      double precision                    :: x1, y1, x2, y2
      double precision                    :: xx1, yy1, xx2, yy2
      double precision                    :: diff1, diff2, dy, r, dx2
      integer, intent(in)                 :: jsferic
      double precision                    :: csphi

      if (jsferic == 1) then

         ! fix for poles
         diff1 = abs(abs(y1)-90d0)
         diff2 = abs(abs(y2)-90d0)
         if ( (diff1 <= dtol_pole .and. diff2 > dtol_pole) .or. &
            (diff1 > dtol_pole .and. diff2 <= dtol_pole) ) then
         getdx = 0d0
         return
         end if

         xx1   = x1
         xx2   = x2
         if      (xx1 - xx2 >  180d0) then
            xx1 = xx1 - 360d0
         else if (xx1 - xx2 < -180d0) then
            xx1 = xx1 + 360d0
         endif
         xx1   = xx1*degrad_hp
         xx2   = xx2*degrad_hp
         yy1   = y1 *degrad_hp
         yy2   = y2 *degrad_hp
         csphi = cos(0.5d0*(yy1+yy2))
         getdx = earth_radius*csphi*(xx2-xx1)
      else
         getdx = x2-x1
      endif
      end function getdx

      !
      ! getdy
      !
      double precision function getdy(x1,y1,x2,y2, jsferic)
      
      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole

      implicit none
      
      double precision :: x1, y1, x2, y2
      double precision :: xx1, yy1,yy2
      integer, intent(in)  :: jsferic

      if (jsferic == 1) then
         yy1   = y1*degrad_hp
         yy2   = y2*degrad_hp
         getdy = earth_radius*(yy2-yy1)
      else
         getdy = y2-y1
      endif
      end function getdy

      
      !
      ! getdxdy
      !
      subroutine getdxdy(x1,y1,x2,y2,dx,dy, jsferic)
      
      implicit none
      
      double precision :: x1, y1, x2, y2, dx, dy, dx2, dy2, dum
      integer, intent(in)  :: jsferic
      
      if (Jsferic == 1) then
         dx = getdx(x1,y1,x2,y2,jsferic)
         dy = getdy(x1,y1,x2,y2,jsferic)
      else
         dx = x2-x1
         dy = y2-y1
      endif

      end subroutine getdxdy
      
      !
      ! sphertocart3D
      !
      subroutine sphertocart3D(x1,y1,xx1,yy1,zz1)  !, jsferic) ! from spherical 2D to Cartesian 3D coordinates
      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole
      implicit none
      double precision     :: x1,y1,xx1,yy1,zz1,rr
      
!      if ( jsferic == 1 ) then
         zz1 = earth_radius*sin(y1*degrad_hp)
         rr  = earth_radius*cos(y1*degrad_hp)
         xx1 = rr*cos(x1*degrad_hp)
         yy1 = rr*sin(x1*degrad_hp)
!      else
!         zz1 = 0d0
!         xx1 = x1
!         yy1 = y1
!      end if
      
      end subroutine sphertocart3D

      !
      ! Cart3Dtospher
      !  
      !    transform 3D Cartesian coordinates to 2D spherical (jsferic=1) or 2D Cartesian (jsferic=0) coordinates
      !    x1 will be close to xref in spherical coordinates
      subroutine Cart3Dtospher(xx1,yy1,zz1,x1,y1,xref)
         use mathconsts, only: raddeg_hp
         implicit none
         
         double precision, intent(in)  :: xx1   !< 3D x-coordinate
         double precision, intent(in)  :: yy1   !< 3D y-coordinate
         double precision, intent(in)  :: zz1   !< 3D z-coordinate
         double precision, intent(out) :: x1    !< longitude (spherical) or x-coordinate (2D Cartesian)
         double precision, intent(out) :: y1    !< lattitude (spherical) or y-coordinate (2D Cartesian)
         double precision, intent(in)  :: xref  !< reference point longitude
         
         double precision              :: xx1_, yy1a
         
         double precision, parameter   :: dtol=1d-16
         
!         if ( jsferic == 1 ) then
            xx1_ = xx1
!            yy1a = abs(yy1)
!            if ( xx1 > -dtol*yy1a .and. xx1 < dtol*yy1a ) then
!               xx1_ = 0d0
!            end if
            x1 = atan2(yy1,xx1)*raddeg_hp
            y1 = atan2(zz1,sqrt(xx1**2+yy1**2))*raddeg_hp
            
!            if ( x1 /= DMISS ) then
               x1 = x1 + nint((xref-x1)/360d0) * 360d0
!            end if
!         else
!            x1 = xx1
!            y1 = yy1
!         end if
         
         return
      end subroutine Cart3Dtospher
      
      !
      ! CROSS
      !
      !> Checks whether lines 1-2 and 3-4 intersect.
      !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
      !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
      !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] xcr,ycr x-coord. of intersection point.
      subroutine CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
      use ieee_arithmetic, only: ieee_is_nan

      implicit none
       
      double precision, intent(inout) :: crp !< crp (in)==-1234 will make crp (out) non-dimensional
      double precision                :: det
      double precision                :: eps
      integer                         :: jacros, jamakenondimensional 
      double precision                :: sl
      double precision                :: sm
      double precision, intent(in)    :: x1, y1, x2, y2, x3, y3, x4, y4
      double precision                :: x21, y21, x31, y31, x43, y43, xcr, ycr
      integer, intent(in)             :: jsferic
      double precision, intent(in)    :: dmiss

     
!     safety check on crp (in)
      if ( ieee_is_nan(crp) ) then
         crp = 0d0
      end if

      ! Set defaults for no crossing at all:
      jamakenondimensional = 0
      if ( abs(crp+1234d0) < 0.5d0 ) then
         jamakenondimensional = 1
         crp = 0d0
      endif
      
      JACROS = 0
      EPS    = 0.00001d0
      SL     = DMISS
      SM     = DMISS
!     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
!     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR
      
      call getdxdy(x1,y1,x2,y2,x21,y21,jsferic)
      call getdxdy(x3,y3,x4,y4,x43,y43,jsferic)
      call getdxdy(x1,y1,x3,y3,x31,y31,jsferic)

      !X21 =  getdx(x1,y1,x2,y2)
      !Y21 =  getdy(x1,y1,x2,y2)
      !X43 =  getdx(x3,y3,x4,y4)
      !Y43 =  getdy(x3,y3,x4,y4)
      !X31 =  getdx(x1,y1,x3,y3)
      !Y31 =  getdy(x1,y1,x3,y3)

      DET =  X43*Y21 - Y43*X21

      EPS = max(EPS*MAXVAL((/ X21,Y21,X43,Y43 /)), tiny(0d0))
      IF (ABS(DET) .LT. EPS) THEN
         RETURN
      ELSE
         SM = (Y31*X21 - X31*Y21) / DET
         SL = (Y31*X43 - X31*Y43) / DET
         IF (SM .GE. 0d0 .AND. SM .LE. 1d0 .AND. &
             SL .GE. 0d0 .AND. SL .LE. 1d0) THEN
            JACROS = 1
         ENDIF
         XCR    = X1 + SL*(X2-X1)
         YCR    = Y1 + SL*(Y2-Y1)
         if ( jamakenondimensional == 1 ) then  ! make crp non-dimensional (for spline2curvi)
            CRP    = -DET / ( sqrt(x21**2+y21**2) * sqrt(x43**2 + y43**2) + 1d-8 )
         else
            CRP    = -DET
         end if
      ENDIF
      RETURN
      
      end subroutine CROSS
      
      subroutine cross3D(x1, y1, x2, y2, x3, y3, x4, y4, jacros, sL, sm, xcr, ycr, jsferic, dmiss)    ! ,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
      
         implicit none
         
         double precision, intent(in)   :: x1, y1 !< first  point coordinates
         double precision, intent(in)   :: x2, y2 !< second point coordinates
         double precision, intent(in)   :: x3, y3 !< third  point coordinates
         double precision, intent(in)   :: x4, y4 !< fourth point coordinates
         integer,          intent(out)  :: jacros !< line 1-2 crosses line 3-4 (1) or not (0)
         double precision, intent(out)  :: sL, sm
         double precision, intent(out)  :: xcr, ycr
         integer,          intent(in)   :: jsferic
         double precision, intent(in)   :: dmiss
         
         double precision, dimension(3) :: xx1, xx2, xx3, xx4
         double precision, dimension(3) :: xxcr
         
         double precision, dimension(3) :: n12, n34, n
         
         double precision               :: Det12, Det34, dum
         
         double precision, parameter    :: dtol = 1d-12

!        get 3D coordinates of four points
         call sphertocart3D(x1, y1, xx1(1), xx1(2), xx1(3))
         call sphertocart3D(x2, y2, xx2(1), xx2(2), xx2(3))
         call sphertocart3D(x3, y3, xx3(1), xx3(2), xx3(3))
         call sphertocart3D(x4, y4, xx4(1), xx4(2), xx4(3))
         
!        n12 = ( x1 X x2)
         n12 = vecprod(xx1, xx2)
         n12 = n12 / sqrt(inprod(n12,n12))
         
!        n34 = ( x3 X x4)
         n34 = vecprod(xx3, xx4)
         n34 = n34 / sqrt(inprod(n34,n34))
         
         sL = DMISS
         sm = DMISS
         jacros = 0
         
         dum = sqrt(abs(inprod(n12,n34)))
         
         if ( 1d0-dum > dtol ) then
!           3D         
            Det12 = inprod(xx2-xx1,n34)
            Det34 = inprod(xx4-xx3,n12)
         
            if ( abs(Det12) > dtol .and. abs(Det34) > dtol ) then
               SL = - inprod(xx1,n34) / Det12
               SM = - inprod(xx3,n12) / Det34
            end if
         else
!           2D
           
         end if
            
         if ( SM .GE. 0d0 .AND. SM .LE. 1d0 .AND. &
               SL .GE. 0d0 .AND. SL .LE. 1d0 ) THEN
            jacros = 1
         endif
         
         xxcr    = xx1 + SL*(xx2-xx1)
         call Cart3Dtospher(xxcr(1),xxcr(2),xxcr(3),xcr,ycr,max(x1, x2))
         
         return
      end subroutine cross3D

      ! compute average of coordinates in 3D
      subroutine ave3D(N,x,y,xu,yu,jsferic,jasfer3D)
      implicit none

      integer,                        intent(in)  :: N      !< number of points
      double precision, dimension(N), intent(in)  :: x, y   !< point coordinates
      double precision,               intent(out) :: xu, yu !< average coordinates
      integer,                        intent(in)  :: jsferic
      integer,                        intent(in)  :: jasfer3D

      double precision                            :: xx, yy, zz
      double precision                            :: xxu, yyu, zzu

      double precision                            :: dNi, x1

      integer                                     :: i

      if ( N < 1 ) return

      dNi = 1d0/N
      x1 = x(1)

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         xxu = 0d0
         yyu = 0d0
         zzu = 0d0
         do i=1,N
            call sphertoCart3D(x(i),y(i),xx,yy,zz)
            xxu = xxu + xx
            yyu = yyu + yy
            zzu = zzu + zz
            x1 = max(x1,x(i))
         end do
         xxu = xxu * dNi
         yyu = yyu * dNi
         zzu = zzu * dNi
         call Cart3Dtospher(xxu,yyu,zzu,xu,yu,x1)
      else
         xu = 0d0
         yu = 0d0
         do i=1,N
            xu = xu+x(i)
            yu = yu+y(i)
         end do
         xu = xu*dNi
         yu = yu*dNi
      end if

      return
      end subroutine ave3D

      !>    c = a X b
      function vecprod(a,b)
         implicit none
         double precision, dimension(3)             :: vecprod
         double precision, dimension(3), intent(in) :: a, b
         
         vecprod = (/ a(2)*b(3)-a(3)*b(2), a(3)*b(1)-a(1)*b(3), a(1)*b(2)-a(2)*b(1) /)
         
         return
      end function vecprod
      
!>    c = a.b
      double precision function inprod(a,b)
         implicit none
         
         double precision, dimension(3) :: a, b
         
         inprod = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      end function inprod
      
!>    c = A b
      function matprod(A,b)
         implicit none
         
         double precision, dimension(3)               :: matprod
         double precision, dimension(3,3), intent(in) :: A
         double precision, dimension(3),   intent(in) :: b
         
         integer                                      :: i
         
         matprod = (/ ( A(i,1) * b(1) + A(i,2) * b(2) + A(i,3) * b(3), i=1,3) /)
      end function matprod

      subroutine dbpinpol(xp,yp, in, dmiss, JINS, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN
      implicit none
      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(inout) :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon
      integer                                         :: num
      double precision, intent(in)                    :: dmiss
      integer, intent(in)                             :: JINS, NPL
      double precision, optional,intent(in)           :: xpl(:), ypl(:), zpl(:)
      
      if (NPL>0 .and. present(xpl)) then
         call dbpinpol_optinside_perpol(xp, yp, 0, 0, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)
      else
         call dbpinpol_optinside_perpol(xp, yp, 0, 0, in, num, dmiss, JINS, NPL)
      endif
      end subroutine dbpinpol

      !> The original dbpinpol routine, extended with an optional per-polygon-specified inside-mode.
      !! Used this for checking for many points whether they are inside the global polygons.
      !! Optionally, the global jins=1/other:inside/outside-mode can be replaced by an
      !! inside/outside mode per polygon: that should then be stored as a +1/-1 in the first
      !! zpl(istart) point of each polygon.
      subroutine dbpinpol_optinside_perpol(xp, yp, inside_perpol, iselect, in, numselect, dmiss, JINS, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN

      use m_alloc

      implicit none

      double precision,                 intent(in)    :: xp, yp        !< point coordinates
      integer,                          intent(in)    :: inside_perpol !< Specify whether or not (1/0) to use each polygon's first point zpl-value as the jins(ide)-option (only 0 or 1 allowed), or use the global JINS variable.
      integer,                          intent(in)    :: iselect       !< use all polygons (0), only first-zpl<0 polygons (-1), or all but first-zpl<0 polygons (1)
      integer,                          intent(inout) :: in            !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon
      integer,                          intent(inout) :: numselect     !< number of polygons of "iselect" type considered

      integer                                         :: MAXPOLY=1000 ! will grow if needed

      double precision, allocatable, save             :: xpmin(:), ypmin(:), xpmax(:), ypmax(:)
      integer,                       save             :: Npoly
      integer,          allocatable, save             :: iistart(:), iiend(:)

      integer                                         :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                         :: istart, iend   ! point to start and and node of a polygon in polygon array respectively
      integer                                         :: ipoly          ! polygon number

      logical                                         :: Linit          ! initialization of polygon bounds, and start and end nodes respectively

      integer :: jins_opt !< The actual used jins-mode (either global, or per poly)
      double precision, intent(in)                    :: dmiss
      integer, intent(in)                             :: JINS, NPL
      double precision, optional, intent(in)          :: xpl(NPL), ypl(NPL), zpl(NPL)
      integer                                         :: count
      numselect = 0

      if ( NPL == 0 ) then
         in = 1
         return
      end if

      Linit =  ( in < 0 )

      in = 0

      !     initialization
      if ( Linit ) then
         !         write(6,"('dbpinpol: init... ', $)")
         ipoint = 1
         ipoly = 0
         call realloc(xpmin, maxpoly, keepExisting=.false.)
         call realloc(xpmax, maxpoly, keepExisting=.false.)
         call realloc(ypmin, maxpoly, keepExisting=.false.)
         call realloc(ypmax, maxpoly, keepExisting=.false.)
         call realloc(iistart, maxpoly, keepExisting=.false.)
         call realloc(iiend, maxpoly, keepExisting=.false.)

         do while ( ipoint < NPL )
            ipoly = ipoly+1
            if (ipoly > maxpoly) then
               maxpoly = ceiling(maxpoly*1.1)
               call realloc(xpmin, maxpoly, keepExisting=.true.)
               call realloc(xpmax, maxpoly, keepExisting=.true.)
               call realloc(ypmin, maxpoly, keepExisting=.true.)
               call realloc(ypmax, maxpoly, keepExisting=.true.)
               call realloc(iistart, maxpoly, keepExisting=.true.)
               call realloc(iiend, maxpoly, keepExisting=.true.)
            end if

            !           get polygon start and end pointer respectively
            call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend, dmiss)
            istart = istart+ipoint-1
            iend   = iend  +ipoint-1

            if ( istart >= iend .or. iend > NPL ) exit ! done

            xpmin(ipoly) = minval(xpl(istart:iend))
            xpmax(ipoly) = maxval(xpl(istart:iend))
            ypmin(ipoly) = minval(ypl(istart:iend))
            ypmax(ipoly) = maxval(ypl(istart:iend))

            iistart(ipoly) = istart
            iiend(ipoly)   = iend

            !           advance pointer
            ipoint = iend+2
         end do   ! do while ( ipoint < NPL .and. ipoly < MAXPOLY )
         Npoly = ipoly

         !         write(6,"('done, Npoly=', I4)") Npoly
      end if
      count  = 0
      do ipoly=1,Npoly
         istart = iistart(ipoly)
         iend   = iiend(ipoly)

         !         write(6,"('dbpinpol: ipoly=', I4, ', istart=', I16, ', iend=', I16)") ipoly, istart, iend

         if ( istart >= iend .or. iend > NPL ) exit ! done

         if ( iselect == -1 .and. (zpl(istart) == DMISS .or.  zpl(istart) >= 0) ) cycle
         if ( iselect == 1 .and. (zpl(istart) /= DMISS .and. zpl(istart) < 0) ) cycle

         numselect = numselect+1

         if ( inside_perpol == 1 .and. zpl(istart) /= dmiss ) then   ! only if third column was actually supplied
            jins_opt = int(zpl(istart)) ! Use inside-option per each polygon.
         else
            jins_opt = JINS ! Use global inside-option.
         end if

         IF (jins_opt == 1) THEN  ! inside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
               if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
                  IN = 1-in   ! IN-1
               end if

               if (in == 1) then
                  count  = count + 1
               end if
            endif
         ELSE                 ! outside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
               if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
                  IN = 1-in   ! IN-1
               end if

               if (in == 1) then
                  continue
               else
                  count = count + 1
               end if
            !else
            !   in = 1 ! outside check succeeded (completely outside of polygon's bounding box), return 'true'.
            !   exit
            endif
         ENDIF
      end do   ! do ipoly=1,Npoly

      if (jins == 1) then !< NB: here, it is checked based on jins, while in the loop based on jins_opt, need to be fixed
         if (mod(count,2) == 1) then 
            in = 1
         else
            in = 0
         endif
      else
         if (mod(count,2) == 1) then
            in = 0
         else
            in = 1
         endif
      endif

      return
   end subroutine dbpinpol_optinside_perpol

   ! ==============================================================================================
   ! ==============================================================================================
   subroutine dbpinpol_optinside_perpol2(xp, yp, inside_perpol, iselect, in, numselect, dmiss, JINS, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN

      use m_alloc

      implicit none

      double precision,                 intent(in)    :: xp, yp        !< point coordinates
      integer,                          intent(in)    :: inside_perpol !< Specify whether or not (1/0) to use each polygon's first point zpl-value as the jins(ide)-option (only 0 or 1 allowed), or use the global JINS variable.
      integer,                          intent(in)    :: iselect       !< use all polygons (0), only first-zpl<0 polygons (-1), or all but first-zpl<0 polygons (1)
      integer,                          intent(inout) :: in            !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon
      integer,                          intent(inout) :: numselect     !< number of polygons of "iselect" type considered

      integer                                         :: MAXPOLY=1000 ! will grow if needed

      double precision, allocatable, save             :: xpmin(:), ypmin(:), xpmax(:), ypmax(:)
      integer,                       save             :: Npoly
      integer,          allocatable, save             :: iistart(:), iiend(:)

      integer                                         :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                         :: istart, iend   ! point to start and and node of a polygon in polygon array respectively
      integer                                         :: ipoly          ! polygon number

      logical                                         :: Linit          ! initialization of polygon bounds, and start and end nodes respectively

      integer :: jins_opt !< The actual used jins-mode (either global, or per poly)
      double precision, intent(in)                    :: dmiss
      integer, intent(in)                             :: JINS, NPL
      double precision, optional, intent(in)          :: xpl(NPL), ypl(NPL), zpl(NPL)
      
      numselect = 0

      if ( NPL == 0 ) then
         in = 1
         return
      end if

      Linit =  ( in < 0 )

      in = 0

      !     initialization
      if ( Linit ) then
         !         write(6,"('dbpinpol: init... ', $)")
         ipoint = 1
         ipoly = 0
         call realloc(xpmin, maxpoly, keepExisting=.false.)
         call realloc(xpmax, maxpoly, keepExisting=.false.)
         call realloc(ypmin, maxpoly, keepExisting=.false.)
         call realloc(ypmax, maxpoly, keepExisting=.false.)
         call realloc(iistart, maxpoly, keepExisting=.false.)
         call realloc(iiend, maxpoly, keepExisting=.false.)

         do while ( ipoint < NPL )
            ipoly = ipoly+1
            if (ipoly > maxpoly) then
               maxpoly = ceiling(maxpoly*1.1)
               call realloc(xpmin, maxpoly, keepExisting=.true.)
               call realloc(xpmax, maxpoly, keepExisting=.true.)
               call realloc(ypmin, maxpoly, keepExisting=.true.)
               call realloc(ypmax, maxpoly, keepExisting=.true.)
               call realloc(iistart, maxpoly, keepExisting=.true.)
               call realloc(iiend, maxpoly, keepExisting=.true.)
            end if

            !           get polygon start and end pointer respectively
            call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend, dmiss)
            istart = istart+ipoint-1
            iend   = iend  +ipoint-1

            if ( istart >= iend .or. iend > NPL ) exit ! done

            xpmin(ipoly) = minval(xpl(istart:iend))
            xpmax(ipoly) = maxval(xpl(istart:iend))
            ypmin(ipoly) = minval(ypl(istart:iend))
            ypmax(ipoly) = maxval(ypl(istart:iend))

            iistart(ipoly) = istart
            iiend(ipoly)   = iend

            !           advance pointer
            ipoint = iend+2
         end do   ! do while ( ipoint < NPL .and. ipoly < MAXPOLY )
         Npoly = ipoly

         !         write(6,"('done, Npoly=', I4)") Npoly
      end if

      do ipoly=1,Npoly
         istart = iistart(ipoly)
         iend   = iiend(ipoly)

         !         write(6,"('dbpinpol: ipoly=', I4, ', istart=', I16, ', iend=', I16)") ipoly, istart, iend

         if ( istart >= iend .or. iend > NPL ) exit ! done

         if ( iselect == -1 .and. (zpl(istart) == DMISS .or.  zpl(istart) >= 0) ) cycle
         if ( iselect == 1 .and. (zpl(istart) /= DMISS .and. zpl(istart) < 0) ) cycle

         numselect = numselect+1

         if ( inside_perpol == 1 .and. zpl(istart) /= dmiss ) then   ! only if third column was actually supplied
            jins_opt = int(zpl(istart)) ! Use inside-option per each polygon.
         else
            jins_opt = JINS ! Use global inside-option.
         end if

         IF (jins_opt == 1) THEN  ! inside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
            call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
            if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
               IN = 1-in   ! IN-1
            end if

            if ( in == 1 ) then
               exit
            end if
            endif
         ELSE                 ! outside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
            call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
            if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
               IN = 1-in   ! IN-1
            end if

            if ( in == 1 ) then
               exit ! outside check succeeded, return 'true'.
            end if
            else
               in = 1 ! outside check succeeded (completely outside of polygon's bounding box), return 'true'.
               exit
            endif
         ENDIF
      end do   ! do ipoly=1,Npoly

      if (in == 1) then ! and, even more handy: 
         ipolyfound = ipoly 
      else
         ipolyfound = 0
      endif

      return
   end subroutine dbpinpol_optinside_perpol2
      
      
   !>  get the start and end index of the first enclosed non-DMISS subarray
   subroutine get_startend(num, x, y, jstart, jend, dmiss)

      implicit none

      integer,                          intent(in)  :: num           !< array size
      double precision, dimension(num), intent(in)  :: x, y          !< array coordinates
      integer,                          intent(out) :: jstart, jend  !< subarray indices
      double precision, intent(in)                  :: dmiss

      !      find jstart and jend
      jend = 1
      jstart = jend

      if ( jend >= num ) return

      if ( x(jstart+1) == dmiss ) jstart = jstart+1
      if ( jstart >= num ) return

      do while( x(jstart) == dmiss )
         jstart = jstart+1
         if ( jstart == num ) exit
      end do
      if ( x(jstart) == dmiss ) return

      jend   = jstart
      if ( jend < num ) then
         do while( x(jend+1) /= dmiss )
            jend = jend+1
            if ( jend == num ) exit
         end do
      end if

      return
      end subroutine get_startend
      
!> determine if point is "inside" (first) polygon (1) or not (0)
   subroutine pinpok3D(xp, yp, N, x, y, inside, dmiss, jins, jsferic, jasfer3D, dfac, xz, yz)
!      use m_sferic
!      use m_missing
!      use geometry_module, only: sphertocart3D
      use mathconsts, only: degrad_hp
      implicit none

      double precision,               intent(in)  :: xp, yp !< point coordinates
      integer,                        intent(in)  :: N      !< polygon size
      double precision, dimension(N), intent(in)  :: x, y   !< polygon coordinates
      integer,                        intent(out) :: inside !< inside (1) or not (0)
      double precision,               intent(in)  :: dmiss  !< missing value
      integer,                        intent(in)  :: jins   !< global inside/outside
      integer,                        intent(in)  :: jsferic   !< spherical coordinates (1) or Cartesian (0)
      integer,                        intent(in)  :: jasfer3D
      double precision, optional,     intent(in)  :: dfac      !< polygon enlargement factor
      double precision, optional,     intent(in)  :: xz, yz    !< coordinates of enlargement center
      
      double precision, dimension(:), allocatable :: xx, yy, zz

      double precision, dimension(3)              :: xiXxip1 ! x_i X x_{i+1}
      double precision, dimension(3)              :: xpXe ! xp X e

      double precision                            :: xxp, yyp, zzp
      double precision                            :: xxz, yyz, zzz
      
      double precision                            :: D, Di
      double precision                            :: xi, eta, zeta
      double precision                            :: lambda

      integer                                     :: i, ip1, num

      double precision, dimension(3)              :: ee

      double precision,               parameter   :: dtol = 0d0

      if ( N < 3 ) then
         inside = 0
         if ( jins /= 1 ) inside = 1-inside
         goto 1234
      end if

      !     allocate
      allocate(xx(N), yy(N), zz(N))

      !     get 3D polygon coordinates
      num = 0
      do i=1,N
         if ( x(i) /= DMISS .and. y(i) /= DMISS ) then
            num = num+1
            call sphertocart3D(x(i),y(i),xx(num),yy(num),zz(num))
         else if ( num > 0 ) then
            exit
         end if
      end do

!     check if enlargement is required
      if ( present(dfac) .and. present(xz) .and. present(yz) ) then
!        enlarge
      
         call sphertocart3D(xz,yz,xxz,yyz,zzz)
         do i=1,N
            xx(i) = xxz + dfac*(xx(i)-xxz)
            yy(i) = yyz + dfac*(yy(i)-yyz)
            zz(i) = zzz + dfac*(zz(i)-zzz)
         end do
      end if
      
      if ( num < 3 ) then
         inside = 0
         if ( jins /= 1 ) inside=1-inside
         goto 1234  ! no valid polygon found
      end if

      call sphertocart3D(xp,yp,xxp,yyp,zzp)

      !     get test direction: e_lambda
      lambda = xp*degrad_hp   ! dg2rd
      ee = (/ -sin(lambda), cos(lambda), 0d0 /)

      !     loop over polygon sections
      inside = 0
      do i=1,num
         ip1 = i+1; if ( ip1 > num ) ip1=ip1-num

!         xiXxip1 = (/ yy(i)*zz(ip1) - zz(i)*yy(ip1),   &
!                      zz(i)*xx(ip1) - xx(i)*zz(ip1),   &
!                      xx(i)*yy(ip1) - yy(i)*xx(ip1) /)
         
         xiXxip1 = vecprod( (/ xx(i),yy(i),zz(i) /), (/ xx(ip1),yy(ip1),zz(ip1) /) )
                     
!         xpXe = (/ yyp*ee(3) - zzp*ee(2),  &
!                   zzp*ee(1) - xxp*ee(3),  &
!                   xxp*ee(2) - yyp*ee(1) /)
         
         xpXe = vecprod( (/ xxp,yyp,zzp /), ee )
                     
!         D = xiXxip1(1)*ee(1) + xiXxip1(2)*ee(2) + xiXxip1(3)*ee(3)

         D = inprod( xiXxip1, ee )
!         D = sign(1d0,D)
         
         if ( abs(D) > dtol ) then
            Di = 1d0/D
!            xi   = -( xpXe(1)*xx(ip1) + xpXe(2)*yy(ip1) + xpXe(3)*zz(ip1) ) * Di
!            eta  =  ( xpXe(1)*xx(i)   + xpXe(2)*yy(i)   + xpXe(3)*zz(i)   ) * Di
!            zeta = -( xiXxip1(1)*xxp  + xiXxip1(2)*yyp  + xiXxip1(3)*zzp  ) * Di
            
            xi   = -( inprod( xpXe, (/ xx(ip1),yy(ip1),zz(ip1) /) )  ) * Di
            eta  =  ( inprod( xpXe, (/ xx(i),yy(i),zz(i) /) )  ) * Di
            zeta = -( inprod( xiXxip1, (/ xxp,yyp,zzp /) )  ) * Di
         else
            !           enforce no intersection
            xi   = -1d0
            eta  = -1d0
            zeta = -1d0
         end if

         if ( zeta == 0d0 ) then
            inside=1
            goto 1234
         else if ( xi >= 0d0 .and. eta > 0d0 .and. zeta > 0d0 ) then
            inside = 1-inside
         end if

      end do

 1234 continue      
      
      if ( jins == 0 ) inside=1-inside
      
!     deallocate
      if ( allocated(xx) ) deallocate(xx)
      if ( allocated(yy) ) deallocate(yy)
      if ( allocated(zz) ) deallocate(zz)

      return
      end subroutine pinpok3D

      
      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
      implicit none

      integer          :: n,np,m,mp
      double precision :: a,b

      integer          :: ipiv, indxr, indxc, i, j, k, L, LL, irow, icol
      double precision :: big, dum, pivinv

      !      PARAMETER (num_rows=50)
      !      DIMENSION A(NP,NP),B(NP,MP),IPIV(num_rows),INDXR(num_rows),INDXC(num_rows)

      DIMENSION A(NP,NP),B(NP,MP),IPIV(NP),INDXR(NP),INDXC(NP) ! SPvdP: set NMAX to N
      do J=1,N
         IPIV(J)=0
      end do
      do I=1,N
         BIG=0.
         do J=1,N
            IF(IPIV(J).NE.1)THEN
               do K=1,N
                  IF (IPIV(K).EQ.0) THEN
                     IF (ABS(A(J,K)).GE.BIG)THEN
                        BIG=ABS(A(J,K))
                        IROW=J
                        ICOL=K
                     ENDIF
                  ELSE IF (IPIV(K).GT.1) THEN
                     WRITE(*,*) 'Singular matrix'
                  ENDIF
               end do
            ENDIF
         end do
         IPIV(ICOL)=IPIV(ICOL)+1
         IF (IROW.NE.ICOL) THEN
            do L=1,N
               DUM=A(IROW,L)
               A(IROW,L)=A(ICOL,L)
               A(ICOL,L)=DUM
            end do
            do L=1,M
               DUM=B(IROW,L)
               B(IROW,L)=B(ICOL,L)
               B(ICOL,L)=DUM
            end do
         ENDIF
         INDXR(I)=IROW
         INDXC(I)=ICOL
         IF (A(ICOL,ICOL).EQ.0.) WRITE(*,*) 'Singular matrix'
         PIVINV=1./A(ICOL,ICOL)
         A(ICOL,ICOL)=1.
         do L=1,N
            A(ICOL,L)=A(ICOL,L)*PIVINV
         end do
         do L=1,M
            B(ICOL,L)=B(ICOL,L)*PIVINV
         end do
         do LL=1,N
            IF(LL.NE.ICOL)THEN
               DUM=A(LL,ICOL)
               A(LL,ICOL)=0.
               do L=1,N
                  A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
               end do
               do L=1,M
                  B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
               end do
            ENDIF
         end do
      end do
      do L=N,1,-1
         IF(INDXR(L).NE.INDXC(L))THEN
            do K=1,N
               DUM=A(K,INDXR(L))
               A(K,INDXR(L))=A(K,INDXC(L))
               A(K,INDXC(L))=DUM
            end do
         ENDIF
      end do
      RETURN
      END SUBROUTINE GAUSSJ

      !> Checks whether lines 1-2 and 3-4 intersect.
      !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
      !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
      !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
      !! @param[out] xcr,ycr x-coord. of intersection point.
      subroutine crossinbox (x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)  ! only if overlap

      implicit none
      double precision, intent(inout) :: crp ! crp (in)==-1234 will make crp (out) non-dimensional
      integer                         :: jacros
      double precision, intent(in)    :: x1, y1, x2, y2, x3, y3, x4, y4
      double precision, intent(out)   :: SL,SM,XCR,YCR
      double precision                :: x1min, x1max, y1min, y1max, x3min, x3max, y3min, y3max
      integer, intent(in)             :: jsferic
      double precision, intent(in)    :: dmiss

      ! Set defaults for no crossing at all:
      JACROS = 0

      x1min = min(x1,x2); x1max = max(x1,x2)
      y1min = min(y1,y2); y1max = max(y1,y2)
      x3min = min(x3,x4); x3max = max(x3,x4)
      y3min = min(y3,y4); y3max = max(y3,y4)

      if (x1max < x3min) return
      if (x1min > x3max) return
      if (y1max < y3min) return
      if (y1min > y3max) return

      call CROSS (x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)

      RETURN
      end subroutine crossinbox

      !> Computes the perpendicular distance from point 3 to a line 1-2.
      subroutine dlinedis(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
      
      implicit none
      DOUBLE PRECISION, intent(in   ) :: X1,Y1,X2,Y2 !< x,y coordinates of the line between point 1 and 2.
      DOUBLE PRECISION, intent(in   ) :: X3,Y3       !< x,y coordinates of the point for which to compute the distance.
      integer         , intent(  out) :: ja          !< Whether or not (1/0) the computation was possible. If line points 1 and 2 coincide, ja==0, and distance is just Euclidean distance between 3 and 1.
      DOUBLE PRECISION, intent(  out) :: DIS         !< Perpendicular distance from point 3 and line 1-2.
      DOUBLE PRECISION, intent(  out) :: XN, YN      !< Coordinates of the projected point from point 3 onto line 1-2.
      integer,          intent(in   ) :: jsferic, jasfer3D
      double precision, intent(in   ) :: dmiss
      
      DOUBLE PRECISION :: R2,RL,X21,Y21,Z21,X31,Y31,Z31
      DOUBLE PRECISION :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn

!     korste afstand tot lijnelement tussen eindpunten
      JA  = 0
      
      if ( jsferic == 0 .or. jasfer3D == 0 ) then
         X21 = getdx(x1,y1,x2,y2,jsferic)
         Y21 = getdy(x1,y1,x2,y2,jsferic)
         X31 = getdx(x1,y1,x3,y3,jsferic)
         Y31 = getdy(x1,y1,x3,y3,jsferic)
         R2  = dbdistance(x2,y2,x1,y1,jsferic, jasfer3D, dmiss)
         R2  = R2*R2
!         IF (R2 .NE. 0) THEN
         IF (R2 .GT. 1D-8) THEN
            RL  = (X31*X21 + Y31*Y21) / R2
            RL  = MAX( MIN(1d0,RL) , 0d0)
            JA  = 1
            XN  = X1 + RL*(x2-x1)
            
!           fix for spherical, periodic coordinates
            if ( jsferic == 1 ) then
               if ( x2-x1 > 180d0 ) then
                  XN = XN - RL*360d0
               else if ( x2-x1 < -180d0 ) then
                  XN = XN + RL*360d0
               end if
            end if
            
            YN  = Y1 + RL*(y2-y1)
            DIS = dbdistance(x3,y3,xn,yn,jsferic, jasfer3D, dmiss)
         ELSE  ! node 1 -> node 2
            DIS = dbdistance(x3,y3,x1,y1,jsferic, jasfer3D, dmiss)
         ENDIF
      else
         call sphertocart3D(x1,y1,xx1,yy1,zz1)
         call sphertocart3D(x2,y2,xx2,yy2,zz2)
         call sphertocart3D(x3,y3,xx3,yy3,zz3)
         
         x21 = xx2-xx1
         y21 = yy2-yy1
         z21 = zz2-zz1
         x31 = xx3-xx1
         y31 = yy3-yy1
         z31 = zz3-zz1

         r2  = x21*x21 + y21*y21 + z21*z21      
         if (R2 .GT. 1D-8) then
            RL = (X31*X21 + Y31*Y21 + Z31*Z21) / R2
            RL  = MAX( MIN(1d0,RL) , 0d0)
            JA  = 1
            
            XXN  = xx1 + RL*x21 
            YYN  = yy1 + RL*y21
            ZZN  = zz1 + RL*z21
            x31 = xxn-xx3
            y31 = yyn-yy3
            z31 = zzn-zz3
            DIS = sqrt(x31*x31 + y31*y31 + z31*z31)      
            
            call Cart3Dtospher(xxn,yyn,zzn,xn,yn,maxval((/x1,x2,x3/)))
         else   
            DIS = dbdistance(x3,y3,x1,y1, jsferic, jasfer3D, dmiss)
         endif   
      end if
      
      RETURN
      END subroutine dlinedis

      double precision function dprodout(x1,y1,x2,y2,x3,y3,x4,y4, jsferic, jasfer3D)    ! out product of two segments
      implicit none
      double precision :: x1,y1,x2,y2,x3,y3,x4,y4
      double precision :: dx1,dy1,dx2,dy2
      double precision :: xx1, yy1, zz1
      double precision :: xx2, yy2, zz2
      double precision :: xx3, yy3, zz3
      double precision :: xx4, yy4, zz4
      double precision :: vxx, vyy, vzz
      integer, intent(in) :: jsferic
      integer, intent(in) :: jasfer3D

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         call sphertocart3D(x1,y1,xx1,yy1,zz1)
         call sphertocart3D(x2,y2,xx2,yy2,zz2)
         call sphertocart3D(x3,y3,xx3,yy3,zz3)
         call sphertocart3D(x4,y4,xx4,yy4,zz4)

         vxx = (yy2-yy1) * (zz4-zz3) - (zz2-zz1) * (yy4-yy3)
         vyy = (zz2-zz1) * (xx4-xx3) - (xx2-xx1) * (zz4-zz3)
         vzz = (xx2-xx1) * (yy4-yy3) - (yy2-yy1) * (xx4-xx3)

         dprodout = sqrt(vxx**2 + vyy**2 + vzz**2 )

         !   check if vector is pointing outwards of earth
         if ( vxx*xx1 + vyy*yy1 + vzz*zz1 < 0d0 ) then
            dprodout = -dprodout
         end if
      else
         dx1 = getdx(x1,y1,x2,y2,jsferic)
         dx2 = getdx(x3,y3,x4,y4,jsferic)

         dy1 = getdy(x1,y1,x2,y2,jsferic)
         dy2 = getdy(x3,y3,x4,y4,jsferic)

         dprodout = (dx1*dy2 - dy1*dx2)
      end if

      return
      end function dprodout

      !
      ! dcosphi
      !
      !> Normalized inner product of two segments
      !! NOTE that parallel lines may produce abs(dcosphi)=1+O(10^-16) > 1
      !! in Debug builds, crashes subsequent acos calls! (not in Release)
      double precision function dcosphi(x1,y1,x2,y2,x3,y3,x4,y4, jsferic, jasfer3D, dxymis)

      implicit none
      double precision     :: x1,y1,x2,y2,x3,y3,x4,y4
      double precision     :: dx1,dy1,dx2,dy2,r1,r2
      integer, intent(in)  :: jsferic, jasfer3D
      double precision, intent(in) :: dxymis

      double precision, dimension(4) :: xx, yy, zz
      double precision                :: dz1, dz2


      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         call sphertocart3D(x1, y1, xx(1), yy(1), zz(1))
         call sphertocart3D(x2, y2, xx(2), yy(2), zz(2))
         call sphertocart3D(x3, y3, xx(3), yy(3), zz(3))
         call sphertocart3D(x4, y4, xx(4), yy(4), zz(4))

         dx1 = xx(2)-xx(1)
         dy1 = yy(2)-yy(1)
         dz1 = zz(2)-zz(1)
         r1  = dx1**2 + dy1**2 + dz1**2

         dx2 = xx(4)-xx(3)
         dy2 = yy(4)-yy(3)
         dz2 = zz(4)-zz(3)
         r2  = dx2**2 + dy2**2 + dz2**2

         if ( r1 == 0d0 .or. r2 == 0d0 ) then
            dcosphi = dxymis
         else
            dcosphi = (dx1*dx2 + dy1*dy2 + dz1*dz2)/sqrt(r1*r2)
         endif
      else
         !call getdxdy(x1,y1,x2,y2,dx1,dy1)
         !call getdxdy(x3,y3,x4,y4,dx2,dy2)

         dx1 = getdx(x1,y1,x2,y2,jsferic)
         dx2 = getdx(x3,y3,x4,y4,jsferic)

         dy1 = getdy(x1,y1,x2,y2,jsferic)
         dy2 = getdy(x3,y3,x4,y4,jsferic)

         r1  = dx1*dx1+dy1*dy1
         r2  = dx2*dx2+dy2*dy2

         if (r1 == 0d0 .or. r2 == 0d0) then
            dcosphi = dxymis
         else
            dcosphi = (dx1*dx2 + dy1*dy2)/sqrt(r1*r2)
         endif

      end if

      dcosphi = max( min(dcosphi,1d0) , -1d0)

      return
      end function dcosphi



      !>    transform vector with componentis in global spherical coordinate directions (xglob,yglob) to local coordinate directions (xloc,yloc) around reference point (xref,yref)
      subroutine spher2locvec(xref,yref,N,xglob,yglob,vxglob,vyglob,vxloc,vyloc, jsferic, jasfer3D, dmiss)

      use mathconsts, only: degrad_hp

      implicit none

      double precision,               intent(in)  :: xref,  yref     !< global coordinates of reference point (longitude, latitude)
      integer,                        intent(in)  :: N               !< number of global coordinates
      double precision, dimension(N), intent(in)  :: xglob, yglob    !< global coordinates, (longitude, latitude)
      double precision, dimension(N), intent(in)  :: vxglob, vyglob !< vector components in global coordinates
      double precision, dimension(N), intent(out) :: vxloc,  vyloc   !< vector components in local coordinates

      double precision, dimension(3)              :: exxp, eyyp, ezzp   ! base vectors of rotated 3D Cartesian reference frame
      double precision, dimension(3)              :: elambda, ephi
      double precision, dimension(3)              :: elambdap, ephip
      double precision, dimension(3)              :: elambdaloc, ephiloc
      double precision                            :: vxx, vyy, vzz

      double precision                            :: xx, yy, zz     !  3D Cartesian coordinates
      double precision                            :: xxp, yyp, zzp  !  3D Cartesian coordinates in rotated frame
      double precision                            :: xloc, yloc

      double precision                            :: lambda0, phi0
      double precision                            :: lambda, phi
      double precision                            :: lambdap, phip

      integer                                     :: i
      integer, intent(in)                         :: jsferic
      integer, intent(in)                         :: jasfer3D
      double precision, intent(in)                :: dmiss


      if ( jsferic == 0 .or. jasfer3D == 0 ) then
         do i=1,N
            vxloc(i) = vxglob(i)
            vyloc(i) = vyglob(i)
         end do

      else
         phi0 = yref*degrad_hp
         lambda0 = xref*degrad_hp

         !           compute base vectors
         exxp = (/  cos(phi0) * cos(lambda0),  cos(phi0) * sin(lambda0), sin(phi0) /)
         eyyp = (/             -sin(lambda0),              cos(lambda0), 0d0       /)
         ezzp = (/ -sin(phi0) * cos(lambda0), -sin(phi0) * sin(lambda0), cos(phi0) /)

         do i=1,N
            lambda = xglob(i)*degrad_hp
            phi    = yglob(i)*degrad_hp

            !              get 3d-coordinates
            call sphertocart3d(xglob(i),yglob(i),xx,yy,zz)

            !              project to rotated frame
            xxp = exxp(1) * xx + exxp(2) * yy + exxp(3) * zz
            yyp = eyyp(1) * xx + eyyp(2) * yy + eyyp(3) * zz
            zzp = ezzp(1) * xx + ezzp(2) * yy + ezzp(3) * zz

            !              tranform to local spherical coordinates
            call Cart3Dtospher(xxp,yyp,zzp,xloc,yloc,xref)

            lambdap = xloc*degrad_hp
            phip    = yloc*degrad_hp

            !              compute global base vectors at other point in 3D (xx,yy,zz) frame
            elambda = (/          -sin(lambda),           cos(lambda), 0d0 /)
            ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

            !              compute vector in 3D (xx,yy,zz) frame
            vxx = vxglob(i) * elambda(1) + vyglob(i) * ephi(1)
            vyy = vxglob(i) * elambda(2) + vyglob(i) * ephi(2)
            vzz = vxglob(i) * elambda(3) + vyglob(i) * ephi(3)

            !              compute base vectors at other point in rotated 3D (xxp,yyp,zzp) frame
            elambdap = (/           -sin(lambdap),            cos(lambdap), 0d0 /)
            ephip    = (/ -sin(phip)*cos(lambdap), -sin(phip)*sin(lambdap), cos(phip) /)

            !              compute local base vectors in (xx,yy,zz) frame
            elambdaloc = exxp * elambdap(1) + eyyp * elambdap(2) + ezzp * elambda(3)
            ephiloc    = exxp * ephip(1)    + eyyp * ephip(2)    + ezzp * ephip(3)

            !              compute vectors in other point in local base (elambdaloc,ephiloc)
            vxloc = elambdaloc(1) * vxx + elambdaloc(2) * vyy + elambdaloc(3) * vzz
            vyloc = ephiloc(1)    * vxx + ephiloc(2)    * vyy + ephiloc(3)    * vzz
         end do

      end if

      return
      end subroutine spher2locvec 

            subroutine spher2locvec2(xref,yref,N,xglob,yglob,vxglob,vyglob,vxloc,vyloc, jsferic, jasfer3D, dmiss)

      use mathconsts, only: degrad_hp

      implicit none

      double precision,               intent(in)  :: xref,  yref     !< global coordinates of reference point (longitude, latitude)
      integer,                        intent(in)  :: N               !< number of global coordinates
      double precision, intent(in)  :: xglob, yglob    !< global coordinates, (longitude, latitude)
      double precision, intent(in)  :: vxglob, vyglob !< vector components in global coordinates
      double precision, dimension(N), intent(out) :: vxloc,  vyloc   !< vector components in local coordinates

      double precision, dimension(3)              :: exxp, eyyp, ezzp   ! base vectors of rotated 3D Cartesian reference frame
      double precision, dimension(3)              :: elambda, ephi
      double precision, dimension(3)              :: elambdap, ephip
      double precision, dimension(3)              :: elambdaloc, ephiloc
      double precision                            :: vxx, vyy, vzz

      double precision                            :: xx, yy, zz     !  3D Cartesian coordinates
      double precision                            :: xxp, yyp, zzp  !  3D Cartesian coordinates in rotated frame
      double precision                            :: xloc, yloc

      double precision                            :: lambda0, phi0
      double precision                            :: lambda, phi
      double precision                            :: lambdap, phip

      integer                                     :: i
      integer, intent(in)                         :: jsferic
      integer, intent(in)                         :: jasfer3D
      double precision, intent(in)                :: dmiss


      if ( jsferic == 0 .or. jasfer3D == 0 ) then
         do i=1,N
            vxloc(i) = vxglob
            vyloc(i) = vyglob
         end do

      else
         phi0 = yref*degrad_hp
         lambda0 = xref*degrad_hp

         !           compute base vectors
         exxp = (/  cos(phi0) * cos(lambda0),  cos(phi0) * sin(lambda0), sin(phi0) /)
         eyyp = (/             -sin(lambda0),              cos(lambda0), 0d0       /)
         ezzp = (/ -sin(phi0) * cos(lambda0), -sin(phi0) * sin(lambda0), cos(phi0) /)

         do i=1,N
            lambda = xglob*degrad_hp
            phi    = yglob*degrad_hp

            !              get 3d-coordinates
            call sphertocart3d(xglob,yglob,xx,yy,zz)

            !              project to rotated frame
            xxp = exxp(1) * xx + exxp(2) * yy + exxp(3) * zz
            yyp = eyyp(1) * xx + eyyp(2) * yy + eyyp(3) * zz
            zzp = ezzp(1) * xx + ezzp(2) * yy + ezzp(3) * zz

            !              tranform to local spherical coordinates
            call Cart3Dtospher(xxp,yyp,zzp,xloc,yloc,xref)

            lambdap = xloc*degrad_hp
            phip    = yloc*degrad_hp

            !              compute global base vectors at other point in 3D (xx,yy,zz) frame
            elambda = (/          -sin(lambda),           cos(lambda), 0d0 /)
            ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

            !              compute vector in 3D (xx,yy,zz) frame
            vxx = vxglob * elambda(1) + vyglob * ephi(1)
            vyy = vxglob * elambda(2) + vyglob * ephi(2)
            vzz = vxglob * elambda(3) + vyglob * ephi(3)

            !              compute base vectors at other point in rotated 3D (xxp,yyp,zzp) frame
            elambdap = (/           -sin(lambdap),            cos(lambdap), 0d0 /)
            ephip    = (/ -sin(phip)*cos(lambdap), -sin(phip)*sin(lambdap), cos(phip) /)

            !              compute local base vectors in (xx,yy,zz) frame
            elambdaloc = exxp * elambdap(1) + eyyp * elambdap(2) + ezzp * elambda(3)
            ephiloc    = exxp * ephip(1)    + eyyp * ephip(2)    + ezzp * ephip(3)

            !              compute vectors in other point in local base (elambdaloc,ephiloc)
            vxloc = elambdaloc(1) * vxx + elambdaloc(2) * vyy + elambdaloc(3) * vzz
            vyloc = ephiloc(1)    * vxx + ephiloc(2)    * vyy + ephiloc(3)    * vzz
         end do

      end if

      return
      end subroutine spher2locvec2

      
      !
      ! normalin
      !
      !> Normalized vector in direction 1 -> 2, in the orientation of (xu,yu)
      subroutine normalin(x1,y1,x2,y2,xn,yn,xu,yu, jsferic, jasfer3D, dxymis)
      use mathconsts, only: degrad_hp
      implicit none
      double precision :: x1, y1, x2, y2, xn, yn, xu, yu
      ! locals
      double precision             :: ddx, ddy, rr
      double precision, intent(in) :: dxymis

      double precision, dimension(3) :: xx1
      double precision, dimension(3) :: xx2
      double precision, dimension(3) :: xxu
      double precision, dimension(3) :: elambda
      double precision, dimension(3) :: ephi

      double precision :: lambda, phi
      integer, intent(in)          :: jsferic, jasfer3D

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         !    call qnerror('normalin: reference probably not set', ' ', ' ')

         !   compute 3D coordinates
         call sphertoCart3D(x1,y1,xx1(1),xx1(2),xx1(3))
         call sphertoCart3D(x2,y2,xx2(1),xx2(2),xx2(3))

         !   compute base vectors in reference point
         lambda = xu*degrad_hp
         phi    = yu*degrad_hp
         elambda = (/ -sin(lambda),                    cos(lambda), 0d0 /)
         ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

         !   project vector in local base
         ddx = sum((xx2-xx1)*elambda)
         ddy = sum((xx2-xx1)*ephi)
      else
         ddx = getdx(x1,y1,x2,y2,jsferic)
         ddy = getdy(x1,y1,x2,y2,jsferic)
      end if

      rr  = ddx*ddx + ddy*ddy
      if (rr == 0d0) then
         xn  = dxymis
         yn  = dxymis
      else
         rr  = sqrt(rr)
         xn  = ddx / rr
         yn  = ddy / rr
      endif
      end subroutine normalin

      !> Creates the relative unit normal vector to edge 1->2
      !!
      !! Vector is of unit length in Cartesian world.
      !! Vector is almost unit length in spherical world, but its
      !! x-component is scaled 1/cos(phi) such that in later uses:
      !! (theta_B, theta_A) = (theta_A, phi_A) + alpha*(theta_n, phi_n)
      !! the vectors 1->2 and A->B are perpendicular in Cartesian world,
      !! not in spherical world. NOTE: the LENGTH of A->B in Cartesian
      !! world implictly contains the earth radius and dg2rd already,
      !! so make sure your alpha is in degrees.
      subroutine normalout(x1,y1,x2,y2,xn,yn, jsferic, jasfer3D, dmiss, dxymis)             ! normals out edge 1  2

      use mathconsts, only: degrad_hp

      implicit none
      double precision :: x1, y1, x2, y2, xn, yn
      ! locals
      double precision :: ddx, ddy, rr

      double precision, dimension(3) :: xx1
      double precision, dimension(3) :: xx2
      double precision, dimension(3) :: xxu
      double precision, dimension(3) :: elambda
      double precision, dimension(3) :: ephi
      double precision                :: xu, yu
      double precision                :: lambda, phi
      integer, intent(in)             :: jsferic, jasfer3D
      double precision, intent(in)    :: dmiss, dxymis

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         !   get local coordinates w.r.t. (xn,yn)
         call half(x1,y1,x2,y2,xn,yn, jsferic, jasfer3D)

         !   compute 3D coordinates
         call sphertoCart3D(x1,y1,xx1(1),xx1(2),xx1(3))
         call sphertoCart3D(x2,y2,xx2(1),xx2(2),xx2(3))

         !   compute midpoint
         xxu = 0.5d0*(xx1+xx2)
         call Cart3Dtospher(xxu(1),xxu(2),xxu(3),xu,yu,max(x1,x2))

         !   compute base vectors at midpoint
         lambda = xu*degrad_hp
         phi    = yu*degrad_hp
         elambda = (/ -sin(lambda),                    cos(lambda), 0d0 /)
         ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)

         !   project vector in local base
         ddx = sum((xx2-xx1)*elambda)
         ddy = sum((xx2-xx1)*ephi)
      else
         ddx = getdx(x1,y1,x2,y2,jsferic)
         ddy = getdy(x1,y1,x2,y2,jsferic)
      end if

      rr  = ddx*ddx + ddy*ddy
      if (rr == 0d0) then
         xn  = dxymis
         yn  = dxymis
      else
         rr  =  sqrt(rr)
         xn  =  ddy / rr
         yn  = -ddx / rr
      endif
      if (jsferic == 1 .and. jasfer3D == 0) then
         xn = xn / cos(degrad_hp*0.5d0*(y1+y2) )
         yn = yn
      endif

      end subroutine normalout

      !
      ! normaloutchk
      !
      !> Computes the normal vector to a line 1-2, which is *outward* w.r.t.
      !! an 'inside' point 3. Similar to normalout, except that the normal
      !! vector may be flipped based on the 'inside' point.
      subroutine normaloutchk(x1, y1, x2, y2, x3, y3, xn, yn, jaflip, jsferic, jasfer3D, dmiss, dxymis)

      use physicalconsts, only: earth_radius
      use mathconsts, only: degrad_hp

      implicit none
      double precision, intent(in)  :: x1, y1 !< First point of line
      double precision, intent(in)  :: x2, y2 !< Second point of line
      double precision, intent(in)  :: x3, y3 !< Point that is considered 'inside'.
      double precision, intent(out) :: xn, yn !< Output normal vector
      integer,          intent(out) :: jaflip !< Indicates whether normal was flipped (1) or not (0).
      double precision :: din
      double precision, external :: dprodin

      double precision, dimension(1) :: xnloc, ynloc
      double precision               :: xref, yref
      double precision               :: x4, y4
      integer, intent(in)            :: jsferic, jasfer3D
      double precision, intent(in)   :: dmiss, dxymis

      call normalout(x1,y1,x2,y2,xn,yn, jsferic, jasfer3D, dmiss, dxymis)
      jaflip = 0
      !din  = dprodin(x1, y1, x1+xn, y1+yn, x1, y1, x3, y3)
      !if (din > 0d0) then   ! Check whether normal vector points really outward
      !   xn = -xn           ! Using the previously stored internal point x4.
      !   yn = -yn
      !end if

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         !   x4 = x1+xn
         call half(x1,y1,x2,y2,xref,yref, jsferic, jasfer3D)
         call spher2locvec(x1,y1,1,(/xref/),(/yref/),(/xn/),(/yn/),xnloc,ynloc, jsferic, jasfer3D, dmiss)
         call xpav(x1,y1,earth_radius*degrad_hp,xnloc(1),ynloc(1),x4,y4,jsferic, jasfer3D)  ! 1 degree "distance"
      else
         x4 = x1+xn
         y4 = y1+yn
      end if

      if ( dprodout(x1, y1, x4, y4, x1, y1, x2, y2, jsferic, jasfer3D)*dprodout(x1, y1, x3, y3, x1, y1, x2, y2, jsferic, jasfer3D) > 0d0 ) then
         xn = -xn           ! Using the previously stored internal point x4.
         yn = -yn
         jaflip = 1
      else
         jaflip = 0
      endif

      end subroutine normaloutchk

      !
      ! DUITPL
      !
      SUBROUTINE DUITPL(X1,Y1,X2,Y2,X3,Y3,X4,Y4,RU, jsferic)

      implicit none
      double precision             :: X1,Y1,X2,Y2,X3,Y3,X4,Y4,RU
      double precision             :: X12, y12, x34, y34
      integer, intent(in)          :: jsferic

      X12 = GETDX(X1,Y1,X2,Y2,jsferic)
      Y12 = GETDY(X1,Y1,X2,Y2,jsferic)
      X34 = GETDX(X3,Y3,X4,Y4,jsferic)
      Y34 = GETDY(X3,Y3,X4,Y4,jsferic)
      RU  = X12*Y34 - Y12*X34
      RU  = SIGN(1d0,RU)
      RETURN
      END SUBROUTINE DUITPL

      !
      ! half
      !
      ! compute coordinates (xu, yu) halfway between (x1,y1) and (x2,y2)
      subroutine half(x1,y1,x2,y2,xu,yu, jsferic, jasfer3D)

      implicit none

      double precision, intent(in)  :: x1, y1
      double precision, intent(in)  :: x2, y2
      double precision, intent(out) :: xu, yu
      integer, intent(in)          :: jsferic
      integer, intent(in)          :: jasfer3D

      double precision              :: xx1, yy1, zz1, xx2, yy2, zz2

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         call sphertoCart3D(x1,y1,xx1,yy1,zz1)
         call sphertoCart3D(x2,y2,xx2,yy2,zz2)
         call Cart3Dtospher(0.5d0*(xx1+xx2),0.5d0*(yy1+yy2),0.5d0*(zz1+zz2),xu,yu,max(x1,x2))
      else
         xu = 0.5d0*(x1+x2)
         yu = 0.5d0*(y1+y2)
      end if

      return
      end subroutine half

      !
      ! xpav
      !
      ! compute coordinates (xu, yu) from coordinates (x,y) and vector (vx,vy) with
      !    xu = x + alpha v, with v in reference frame of x
      subroutine xpav(x,y,alpha,vx,vy,xu,yu, jsferic, jasfer3D)

      use mathconsts, only: degrad_hp

      implicit none

      double precision, intent(in)  :: x, y
      double precision, intent(in)  :: alpha
      double precision, intent(in)  :: vx, vy
      double precision, intent(out) :: xu, yu

      double precision, dimension(3) :: elambda
      double precision, dimension(3) :: ephi

      double precision               :: vxx, vyy, vzz
      double precision               :: xx, yy, zz
      double precision               :: xxu, yyu, zzu
      double precision               :: lambda, phi
      integer, intent(in)            :: jsferic, jasfer3D

      if ( jsferic == 0 ) then
         xu = x + alpha*vx
         yu = y + alpha*vy
      else
         if ( jasfer3D == 1 ) then
         !     compute global base vectors at other point in 3D (xx,yy,zz) frame
         lambda = x*degrad_hp
         phi    = y*degrad_hp
         elambda = (/          -sin(lambda),           cos(lambda), 0d0 /)
         ephi    = (/ -sin(phi)*cos(lambda), -sin(phi)*sin(lambda), cos(phi) /)
         vxx = (vx*elambda(1) + vy*ephi(1))
         vyy = (vx*elambda(2) + vy*ephi(2))
         vzz = (vx*elambda(3) + vy*ephi(3))

         call sphertoCart3D(x,y,xx,yy,zz)
         xxu = xx + alpha*vxx
         yyu = yy + alpha*vyy
         zzu = zz + alpha*vzz
         call Cart3Dtospher(xxu,yyu,zzu,xu,yu,x)
         else
            ! LC to re-enable call mess(LEVEL_ERROR, 'xpav: not supported')
         end if
      end if

      return
      end subroutine xpav

      !> compute area and mass center of polygon, in two-dimensional space or three-dimensional space depending on "jsferic" and "jasfer3D"
      subroutine comp_masscenter(N, xin , y, xcg, ycg, area, jacounterclockwise, jsferic, jasfer3D, dmiss)

      implicit none

      integer,                        intent(in)    :: N          !< polygon size
      double precision, dimension(N), intent(in)    :: xin, y     !< polygon coordinates
      double precision,               intent(out)   :: xcg, ycg   !< polygon mass center coordinates
      double precision,               intent(out)   :: area        !< polygon area
      integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)
      integer,                        intent(in )   :: jsferic, jasfer3D
      double precision,               intent(in)    :: dmiss

      if ( jsferic == 1 .and. jasfer3D == 1 ) then
         call comp_masscenter3D(N, xin , y, xcg, ycg, area, jacounterclockwise, jsferic, jasfer3D, dmiss)
      else
         call comp_masscenter2D(N, xin , y, xcg, ycg, area, jacounterclockwise, jsferic, dmiss)
      end if

      return
      end subroutine comp_masscenter


      !> compute area and mass center of polygon in two-dimensional space
      subroutine comp_masscenter2D(N, xin , y, xcg, ycg, area, jacounterclockwise, jsferic, dmiss)

      use physicalconsts, only: earth_radius
      use mathconsts, only: degrad_hp

      implicit none

      integer,                        intent(in)    :: N                   !< polygon size
      double precision, dimension(N), intent(in)    :: xin, y              !< polygon coordinates
      double precision,               intent(out)   :: xcg, ycg            !< polygon mass center coordinates
      double precision,               intent(out)   :: area                !< polygon area
      integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)
      integer,                        intent(in)    :: jsferic
      double precision,               intent(in)    :: dmiss

      double precision, dimension(N)                :: x                                  !< Copy of xin, with possibly periodic fixes.
      double precision                              :: dsx, dsy, xc, yc, xds, fac, x0, y0, x1, dx0, dx1, dy0, dy1
      double precision                              :: xdum

      integer                                       :: i, ip1

      double precision, external                    :: getdx, getdy

      double precision, parameter                   :: dtol=1d-8

      area = 0d0
      xcg  = 0d0
      ycg  = 0d0
      jacounterclockwise = 1

      if ( N < 1 ) goto 1234

      x = xin

      !  set reference point (furthest away from poles)
      x0 = minval(x(1:N))
      y0 = y(1)
      do i=2,N
         if ( abs(y(i)) < abs(y0) ) then
            y0 = y(i)
         end if
      end do

      !  fix for periodic, spherical coordinates
      if ( jsferic == 1 ) then
         x1 = maxval(x(1:N))
         if ( x1-x0 > 180d0 ) then
            !        determine cutline
            xdum = x1-180d0
            do i=1,N
               if ( x(i) < xdum ) then
                  x(i) = x(i) + 360d0
               end if
            end do
            x0 = minval(x(1:N))
         end if
      end if

      do i=1,N
         ip1 = i+1; if ( ip1 > N ) ip1=ip1-N


         call getdxdy(x0,y0,x(i),y(i), dx0,dy0, jsferic)
         call getdxdy(x0,y0,x(ip1),y(ip1), dx1, dy1, jsferic)
         xc = 0.5d0*(dx0 + dx1)
         yc = 0.5d0*(dy0 + dy1)

         ! xc = 0.5d0*(getdx(x0,y0,x(i),y(i)) + getdx(x0,y0,x(ip1),y(ip1)))
         ! yc = 0.5d0*(getdy(x0,y0,x(i),y(i)) + getdy(x0,y0,x(ip1),y(ip1)))

         call getdxdy(x(i), y(i), x(ip1), y(ip1), dx0, dy0, jsferic)
         dsx = dy0 ; dsy = -dx0

         !dsx =  getdy(x(i), y(i), x(ip1), y(ip1))
         !dsy = -getdx(x(i), y(i), x(ip1), y(ip1))

         xds  = xc*dsx+yc*dsy
         area = area + 0.5d0*xds
         xcg  = xcg  + xds * xc
         ycg  = ycg  + xds * yc
      end do

      !  for clockwise oriented cells, the normal will be inward, and consequently the area negative
      !  it must stay negative in the computation of the cell center (xcg,ycg)
      area = sign(max(abs(area),dtol),area)

      fac = 1d0/(3d0*area)

      xcg = fac * xcg
      ycg = fac * ycg

      if ( JSFERIC /= 0 ) then
         ycg = ycg / (earth_radius*degrad_hp)
         xcg = xcg / (earth_radius*degrad_hp*cos((ycg+y0)*degrad_hp))
      end if

      xcg = xcg + x0
      ycg = ycg + y0

      !  output cell orientation
      if ( area > 0d0 ) then
         jacounterclockwise = 1
      else
         jacounterclockwise = 0
      end if

      !  fix for inward normals (clockwise oriented cells)
      area = abs(area)

1234  continue

      return
      end subroutine comp_masscenter2D


      !> compute area and mass center of polygon
      subroutine comp_masscenter3D(N, x, y, xcg, ycg, area, jacounterclockwise, jsferic, jasfer3D, dmiss)

      use physicalconsts, only: earth_radius

      implicit none

      integer,                        intent(in)    :: N        !< polygon size
      double precision, dimension(N), intent(in)    :: x, y     !< polygon coordinates
      double precision,               intent(out)   :: xcg, ycg !< polygon mass center coordinates
      double precision,               intent(out)   :: area     !< polygon area
      integer,                        intent(out)   :: jacounterclockwise  !< counterclockwise (1) or not (0)
      integer,                        intent(in)    :: jsferic
      integer,                        intent(in)    :: jasfer3D
      double precision,               intent(in)    :: dmiss

      !   double precision, dimension(N)                :: x  ! Copy of xin, with possibly periodic fixes.

      double precision, dimension(N)                :: xx, yy, zz ! 3D coordinates

      double precision, dimension(4,4)              :: A
      double precision, dimension(4)                :: rhs

      double precision, dimension(N)                :: DvolDx, DvolDy, DvolDz

      double precision                              :: xx0, yy0, zz0, alpha, xx00, yy00, zz00
      double precision                              :: xxcg, yycg, zzcg
      double precision                              :: dvol, vol, voli
      double precision                              :: Jx, Jy, Jz
      double precision                              :: Rai
      double precision                              :: sx, sy, sz
      double precision                              :: xmin, xmax

      integer                                       :: i, ip1, iter


      !   double precision, external                    :: getdx, getdy

      integer,          parameter                   :: MAXITER=100
      double precision, parameter                   :: dtol=1d-8
      double precision, parameter                   :: deps=1d-8
      double precision, parameter                   :: onesixth = 0.166666666666666667d0
      integer                             :: mout=0 ,k

      area = 0d0
      xcg = 0d0
      ycg = 0d0
      jacounterclockwise = 1

      if ( N < 1 ) goto 1234

      if ( N == 2 ) then
         call half(x(1),y(1),x(2),y(2),xcg,ycg, jsferic, jasfer3D)
         goto 1234
      end if

      do i=1,N
         call sphertocart3D(x(i), y(i), xx(i), yy(i), zz(i))
      end do

      Rai = 1d0/earth_radius

      !  first iterate
      xx0 = 0
      yy0 = 0
      zz0 = 0
      do i=1,N
         xx0 = xx0 + xx(i)
         yy0 = yy0 + yy(i)
         zz0 = zz0 + zz(i)
      end do
      xx0 = xx0/N
      yy0 = yy0/N
      zz0 = zz0/N
      xx00  = xx0
      yy00  = yy0
      zz00  = zz0
      alpha = 0.75d0

      !  Newton iterations
      do iter=1,MAXITER

         !     compute volume
         vol = 0d0
         do i=1,N
            ip1 = i+1; if ( ip1 > N ) ip1=ip1-N

            DvolDx(i) = onesixth * ( yy(i)*zz(ip1) - zz(i)*yy(ip1))
            DvolDy(i) = onesixth * ( zz(i)*xx(ip1) - xx(i)*zz(ip1))
            DvolDz(i) = onesixth * ( xx(i)*yy(ip1) - yy(i)*xx(ip1))

            dvol = DvolDx(i)*xx0 + DvolDy(i)*yy0 + DvolDz(i)*zz0
            vol = vol + dvol
         end do

         if ( abs(vol) < dtol) then 
            if (iter == 1 ) then  ! no mass center can be defined, use first iterate
               
               exit
            else                   ! also use first iterate
               !if (mout == 0) call newfil(mout,'dump.pli')
               !write(mout,*) 'bl01'
               !write(mout,*) n, ' 2 '
               !do k = 1,n
               !   write (mout,*) x(k), y(k) 
               !enddo
               xx0  = xx00
               yy0  = yy00
               zz0  = zz00
               exit
            endif
         end if

         voli = 1d0/vol

         A    = 0d0
         rhs  = 0d0
         Jx = (0.25d0-alpha)*xx0 !*vol
         Jy = (0.25d0-alpha)*yy0 !*vol
         Jz = (0.25d0-alpha)*zz0 !*vol
         do i=1,N
            ip1 = i+1; if ( ip1 > N ) ip1=ip1-N

            dvol = (DvolDx(i)*xx0 + DvolDy(i)*yy0 + DvolDz(i)*zz0) * voli  ! *vol

            Jx = Jx + 0.25d0*dvol*(xx(i)+xx(ip1))
            Jy = Jy + 0.25d0*dvol*(yy(i)+yy(ip1))
            Jz = Jz + 0.25d0*dvol*(zz(i)+zz(ip1))

            xxcg = 0.25d0*(xx0+xx(i)+xx(ip1))
            yycg = 0.25d0*(yy0+yy(i)+yy(ip1))
            zzcg = 0.25d0*(zz0+zz(i)+zz(ip1))

            A(1,1) = A(1,1) + xxcg * dvoldx(i)
            A(1,2) = A(1,2) + xxcg * dvoldy(i)
            A(1,3) = A(1,3) + xxcg * dvoldz(i)

            A(2,1) = A(2,1) + yycg * dvoldx(i)
            A(2,2) = A(2,2) + yycg * dvoldy(i)
            A(2,3) = A(2,3) + yycg * dvoldz(i)

            A(3,1) = A(3,1) + zzcg * dvoldx(i)
            A(3,2) = A(3,2) + zzcg * dvoldy(i)
            A(3,3) = A(3,3) + zzcg * dvoldz(i)
         end do

         A(1,1) = voli*A(1,1) + 0.25-alpha
         A(1,2) = voli*A(1,2)
         A(1,3) = voli*A(1,3)
         A(1,4) = -xx0*Rai

         A(2,1) = voli*A(2,1)
         A(2,2) = voli*A(2,2) + 0.25-alpha
         A(2,3) = voli*A(2,3)
         A(2,4) = -yy0*Rai

         A(3,1) = voli*A(3,1)
         A(3,2) = voli*A(3,2)
         A(3,3) = voli*A(3,3) + 0.25-alpha
         A(3,4) = -zz0*Rai

         A(4,1) = -xx0*Rai
         A(4,2) = -yy0*Rai
         A(4,3) = -zz0*Rai
         A(4,4) = 0d0

         rhs(1) = -Jx   ! *dvoli
         rhs(2) = -Jy   ! *dvoli
         rhs(3) = -Jz   ! *dvoli
         rhs(4) = -0.5*(earth_radius**2 - (xx0**2+yy0**2+zz0**2))*Rai

         !     solve system
         call gaussj(A,4,4,rhs,1,1) ! rhs contains solution

         !     update coordinates of centerpoint
         xx0 = xx0 + rhs(1)
         yy0 = yy0 + rhs(2)
         zz0 = zz0 + rhs(3)
         alpha = alpha + rhs(4)*Rai

         !     check convergence
         if ( rhs(1)**2 + rhs(2)**2 + rhs(3)**2 + rhs(4)**2 < deps ) then
            exit
         end if

      end do

      !  check convergence
      if ( iter >= MAXITER ) then
         call msgbox('', 'comp_masscenter: no convergence', LEVEL_ERROR)
         write(1234,*) 'L1'
         write(1234,*) N, 2
         do i=1,N
            write(1234,*) x(i), y(i)
         end do
      end if

      !  compute area
      Area = 0d0
      do i=1,N
         ip1 = i+1; if ( ip1 > N ) ip1=ip1-N
         sx = 0.5d0*( (yy(i)-yy0) * (zz(ip1)-zz0) - (zz(i)-zz0) * (yy(ip1)-yy0) )
         sy = 0.5d0*( (zz(i)-zz0) * (xx(ip1)-xx0) - (xx(i)-xx0) * (zz(ip1)-zz0) )
         sz = 0.5d0*( (xx(i)-xx0) * (yy(ip1)-yy0) - (yy(i)-yy0) * (xx(ip1)-xx0) )

         Area = Area + sqrt(sx**2+sy**2+sz**2)

      end do

      !   write(6,*) Area*(Ra/3d0*voli)

      call Cart3Dtospher(xx0,yy0,zz0,xcg,ycg,maxval(x(1:N)))

      !  output cell orientation
      if ( vol > 0d0 ) then
         jacounterclockwise = 1
      else
         jacounterclockwise = 0
      end if

      !!  fix for inward normals (clockwise oriented cells)
      !   area = abs(area)

1234  continue

      return
      end subroutine comp_masscenter3D


      !
      ! comp_circumcenter3D
      !
      !> compute circumcenter using 3D coordinates
      subroutine comp_circumcenter3D(N, xv, yv, xz, yz, jsferic, dmiss, dcenterinside)

      use physicalconsts, only: earth_radius

      implicit none
      integer, intent(in)              :: N            !< Nr. of vertices
      double precision, intent(inout)  :: xv(N), yv(N) !< Coordinates of vertices (may be changed to avoid alloc overhead)
      double precision, intent(out)    :: xz, yz       !< Circumcenter coordinates

      double precision, dimension(N)   :: xx, yy, zz

      double precision, dimension(N)   :: ttx, tty, ttz      ! tangential vector in 3D coordinates
      double precision, dimension(N)   :: xxe, yye, zze      ! edge midpoint in 3D coordinates
      double precision, dimension(N)   :: ds                 ! edge lengths

      double precision, dimension(4,4) :: A           ! matrix
      double precision, dimension(4)   :: rhs         ! right-hand side
      double precision, dimension(4)   :: update      ! update of (xxc,yyc,zzc,lambda)

      double precision                 :: xxc, yyc, zzc      ! circumcenter in 3D coordinates
      double precision                 :: lambda             ! Lagrange multiplier to enforce circumcenter on the sphere

      double precision                 :: dsi, dinpr
      double precision                 :: xmin, xmax

      integer                          :: i, ip1

      integer                          :: iter

      double precision, parameter      :: dtol=1d-8    ! tolerance for ignoring edges
      double precision, parameter      :: deps=1d-8    ! convergence tolerance (relative)

      integer,          parameter      :: MAXITER=100

      integer, intent(in)              :: jsferic
      double precision, intent(in)     :: dmiss
      double precision, intent(in)     :: dcenterinside
                                       
      double precision                 :: xzw, yzw
      double precision                 :: SL,SM,XCR,YCR,CRP
      
      integer                          :: jacros, in

      !  compute 3D coordinates and first iterate of circumcenter in 3D coordinates and Lagrange multiplier lambda
      xxc = 0d0
      yyc = 0d0
      zzc = 0d0
      do i=1,N
         call sphertocart3D(xv(i), yv(i), xx(i), yy(i), zz(i))
         xxc = xxc + xx(i)
         yyc = yyc + yy(i)
         zzc = zzc + zz(i)
      end do
      lambda = 0d0
      xxc = xxc/N
      yyc = yyc/N
      zzc = zzc/N
      
      call Cart3Dtospher(xxc,yyc,zzc,xzw,yzw,xv(1))

      !  compute tangential vectors and edge midpoints, edge i is from nodes i to i+1, and convergence tolerance
      do i=1,N
         ip1 = i+1; if ( ip1 > N) ip1=ip1-N

         !     tangential vector
         ttx(i) = xx(ip1)-xx(i)
         tty(i) = yy(ip1)-yy(i)
         ttz(i) = zz(ip1)-zz(i)

         ds(i) = sqrt(ttx(i)**2 + tty(i)**2 + ttz(i)**2)

         if ( ds(i) < dtol ) cycle

         dsi = 1d0/ds(i)

         ttx(i) = ttx(i) * dsi
         tty(i) = tty(i) * dsi
         ttz(i) = ttz(i) * dsi

         !     edge midpoint
         xxe(i) = 0.5d0*(xx(i)+xx(ip1))
         yye(i) = 0.5d0*(yy(i)+yy(ip1))
         zze(i) = 0.5d0*(zz(i)+zz(ip1))
      end do


      !  Newton iterations
      do iter=1,MAXITER

         !     build system
         A   = 0d0
         rhs = 0d0
         do i=1,N
            if ( ds(i) < dtol ) cycle ! no contribution

            !        add to upper triangular part and right-hand side
            A(1,1) = A(1,1) + ttx(i)*ttx(i)
            A(1,2) = A(1,2) + ttx(i)*tty(i)
            A(1,3) = A(1,3) + ttx(i)*ttz(i)

            A(2,2) = A(2,2) + tty(i)*tty(i)
            A(2,3) = A(2,3) + tty(i)*ttz(i)

            A(3,3) = A(3,3) + ttz(i)*ttz(i)

            dinpr = (xxc-xxe(i))*ttx(i) + (yyc-yye(i))*tty(i) + (zzc-zze(i))*ttz(i)

            rhs(1) = rhs(1) - dinpr*ttx(i)
            rhs(2) = rhs(2) - dinpr*tty(i)
            rhs(3) = rhs(3) - dinpr*ttz(i)
         end do

         if ( jsferic == 1 ) then
            !        add contribution of constraint
            A(1,1) = A(1,1) - 2d0*lambda
            A(2,2) = A(2,2) - 2d0*lambda
            A(3,3) = A(3,3) - 2d0*lambda

            A(1,4) = -2d0*xxc
            A(2,4) = -2d0*yyc
            A(3,4) = -2d0*zzc

            A(4,4) = 0d0

            rhs(1) = rhs(1) + 2d0 * lambda * xxc
            rhs(2) = rhs(2) + 2d0 * lambda * yyc
            rhs(3) = rhs(3) + 2d0 * lambda * zzc
            rhs(4) = xxc**2 + yyc**2 + zzc**2 - earth_radius**2
         else  ! no constraints, enforce lambda=0
            A(1,4) = 0d0
            A(2,4) = 0d0
            A(3,4) = 0d0
            A(4,4) = 1d0

            rhs(4) = 0d0
         end if

         !     use symmetry of matrix
         A(2,1) = A(1,2)

         A(3,1) = A(1,3)
         A(3,2) = A(2,3)

         A(4,1) = A(1,4)
         A(4,2) = A(2,4)
         A(4,3) = A(3,4)

         !     solve system
         call gaussj(A,4,4,rhs,1,1) ! rhs contains solution

         !     update circumcenter and Lagrange multiplier
         xxc = xxc + rhs(1)
         yyc = yyc + rhs(2)
         zzc = zzc + rhs(3)
         lambda = lambda + rhs(4)


         !     check convergence
         if ( rhs(1)**2 + rhs(2)**2 + rhs(3)**2 < deps ) then
            exit
         end if
      end do

      !  check convergence
      if ( iter >= MAXITER ) then
         call msgbox('', 'comp_circumcenter3D: no convergence', LEVEL_ERROR)
         ! TODO: SvdP: consider adding 'call mess' to stop the simulation.
      end if

      !  project circumcenter back to spherical coordinates
      call Cart3Dtospher(xxc,yyc,zzc,xz,yz,maxval(xv(1:N)))
      
!     check if circumcenter is inside cell
      if ( dcenterinside <= 1d0 .and. dcenterinside >= 0d0 ) then
         call pinpok3D(xz,yz,N,xv,yv,in, dmiss, 1, jsferic, 1)                    ! circumcentre may not lie outside cell
         if (in == 0) then
            do i  = 1,N
               ip1 = i + 1; if ( ip1 > N ) ip1=ip1-N
               call CROSS3D(xzw, yzw, xz, yz, xv(i), yv(i), xv(ip1), yv(ip1),&
                  JACROS,SL,SM,xcr,ycr,jsferic, dmiss)

               if (jacros == 1) then
                  !               xz = 0.5d0*( xh(m) + xh(m2) ) ! xcr
                  !               yz = 0.5d0*( yh(m) + yh(m2) ) ! ycr
                  xz = xcr
                  yz = ycr

                  exit
               endif
            enddo
         endif
      endif

      return
      end subroutine comp_circumcenter3D

      !
      ! GETCIRCUMCENTER
      !
      !> circumcenter of a polygon defined by set of vertices.
      !! See also getcellcircumcenter
      subroutine GETCIRCUMCENTER( nn, xv, yv, lnnl, xz, yz, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)

      use mathconsts, only: degrad_hp, raddeg_hp
      use physicalconsts, only: earth_radius

      implicit none
      integer, intent(in)             :: nn             !< Nr. of vertices
      double precision, intent(inout) :: xv(nn), yv(nn) !< Coordinates of vertices (may be changed to avoid alloc overhead)
      integer,          intent(in)    :: lnnl(nn)       !< Local lnn codes for all netlinks between vertices.
      double precision, intent(out)   :: xz, yz         !< Circumcenter coordinates
      integer, intent(in)             :: jsferic
      integer, intent(in)             :: jasfer3D
      integer, intent(in)             :: jglobe
      integer, intent(in)             :: jins
      double precision, intent(in)    :: dmiss
      double precision, intent(in)    :: dxymis
      double precision, intent(in)    :: dcenterinside

      ! locals
      double precision :: xzw, yzw                        ! zwaartepunt
      double precision :: xn, yn                          ! normal out
      double precision :: dis
      integer          :: m,k,k1,k2
      double precision :: xz2, yz2                        ! only for help 4 corners
      double precision :: xe3,ye3,xe1,ye1,xe2,ye2,tex,tey,ds, &
         xccf,yccf,xccc,yccc,xcccf,ycccf,xccfo,yccfo,alf

      integer, parameter :: num_columns=10

      double precision :: xh(num_columns), yh(num_columns)
      double precision :: xr(num_columns), yr(num_columns), SL,SM,XCR,YCR,CRP
      double precision :: eps = 1d-3, xcc3, ycc3, xf, xmx, xmn
      double precision :: dfac
      integer          :: jacros, in, m2, nintlinks ! nr of internal links = connected edges

      ! integer,          parameter     :: N6=6
      ! double precision, dimension(N6) :: xhalf, yhalf

      double precision, parameter      :: dtol=1d-4

      xzw = 0d0 ; yzw = 0d0

      if (jsferic == 1) then ! jglobe                 ! regularise sferic coordinates
         xmx = maxval(xv(1:nn))
         xmn = minval(xv(1:nn))
         if (xmx - xmn > 180d0) then
            do m  = 1,nn
               if ( xmx - xv(m) > 180d0) then
                  xv(m) = xv(m) + 360d0
               endif
            enddo
         endif
      endif

      do m  = 1,nn
         xzw   = xzw + xv(m)
         yzw   = yzw + yv(m)
      enddo

      xzw = xzw / nn
      yzw = yzw / nn

      !--------------------------
      ! test
      ! if ( nn > N6 ) then
      !    call qnerror('getcircumcenter: nn>N6', ' ', ' ')
      !    stop
      ! end if
      ! xhalf(1:nn) = 0.5d0*(xv(1:nn)+(/ xv(2:nn), xv(1) /))
      ! yhalf(1:nn) = 0.5d0*(yv(1:nn)+(/ yv(2:nn), yv(1) /))
      ! call comp_circumcenter(nn, xv, yv, xhalf, yhalf, xz, yz)
      ! goto 1234
      ! end test
      !--------------------------
      ! if (nn == 333) then
      if (nn == 3 .and. jglobe == 0 ) then ! for triangles
         call circumcenter3(nn, xv, yv, xz, yz, jsferic)
      else
         ! default case
         if (jsferic == 1) then
            eps = 9d-10 ! 111km = 0-e digit.
         endif

         xccf = xzw
         yccf = yzw
         alf  = 0.1d0

         if (jsferic == 1) then
            xf  = 1d0/cos( degrad_hp*yzw )
         endif

         nintlinks = 0
         do m  = 1,nn
            if ( lnnl(m) == 2) then
               nintlinks = nintlinks + 1
            endif
         enddo

         if (nintlinks > 1 .or. nn == 3) then                ! nn == 3: always for triangles
            do k = 1,100                                     ! Zhang, Schmidt and Perot 2002, formula A3
               xccfo = xccf
               yccfo = yccf
               do m  = 1,nn
                  if ( lnnl( m ) == 2 .or. nn == 3 ) then     ! nn == 3: always for triangles
                     xe1= xv(m)
                     ye1= yv(m)
                     m2 = m + 1; if (m == nn) m2 = 1
                     xe2= xv(m2)
                     ye2= yv(m2)
                     ! If two subsequent corners are on top of each other, see them as one.
                     if (xe1 == xe2 .and. ye1 == ye2) then
                        cycle
                     end if
                     xe3= 0.5d0*(xe1+xe2)
                     ye3= 0.5d0*(ye1+ye2)
                     call normalin(xe1,ye1,xe2,ye2,tex,tey,xe3,ye3, jsferic, jasfer3D, dxymis)
                     xcc3 =  getdx(xe3,ye3,xccf,yccf,jsferic)
                     ycc3 =  getdy(xe3,ye3,xccf,yccf,jsferic)
                     ds   = -alf*dotp(xcc3,ycc3,tex,tey)  ! - sign not present in given formula
                     if (jsferic == 1) then
                        ds   = raddeg_hp*ds/earth_radius
                        xccf = xccf + tex*ds*xf           ! sign present for best result
                        yccf = yccf + tey*ds
                     else
                        xccf = xccf + tex*ds
                        yccf = yccf + tey*ds
                     endif
                     ! ds   = -alf*dotp(xccf - xe3,yccf - ye3, tex, tey)  ! - sign not present in given formula
                     ! call cirr(xccf,yccf,31)
                     ! call waitesc()
                  endif
               enddo
               if (k > 1 .and. abs(xccf-xccfo) < eps .and. abs(yccf-yccfo) < eps) then
                  m = 1
                  exit
               endif
            enddo

            xz = xccf
            yz = yccf

         else

            xz = xzw
            yz = yzw

         endif
      endif

1234  continue

      ! if (jsferic == 1) then ! jglobe   ! regularisatie tbv tidal force routine
      !    if ( xz < -180d0 ) then
      !         xz = xz + 360d0
      !    endif
      ! ENDIF

      if ( dcenterinside <= 1d0 .and. dcenterinside >= 0d0 ) then
         if ( nn <= 3 ) then ! triangles
            dfac = 1d0
         else
            dfac = dcenterinside
         end if

         do m=1,nn
            xh(m) = dfac*xv(m)+(1-dfac)*xzw
            yh(m) = dfac*yv(m)+(1-dfac)*yzw
         end do

         call pinpok(xz,yz,nn,xh,yh,in, jins, dmiss)                    ! circumcentre may not lie outside cell
         if (in == 0) then
            do m  = 1,nn
               m2 = m + 1; if (m == nn) m2 = 1
               call CROSS(xzw, yzw, xz, yz, xh(m ), yh(m ), xh(m2), yh(m2),&
                  JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)

               if (jacros == 1) then
                  !               xz = 0.5d0*( xh(m) + xh(m2) ) ! xcr
                  !               yz = 0.5d0*( yh(m) + yh(m2) ) ! ycr
                  xz = xcr
                  yz = ycr

                  exit
               endif
            enddo
         endif
      endif


      end subroutine GETCIRCUMCENTER

      !> computes dot product of two two-dimensional vectors defined by (x1,y1) and (x2,y2) respectively
      double precision function dotp(x1,y1,x2,y2)         ! dot produkt
      implicit none
      double precision :: x1, y1, x2, y2
      dotp = x1*x2 + y1*y2
      end function dotp

      !> compute circumcenter of a triangle
      subroutine circumcenter3(nn, x, y, xz, yz, jsferic )             ! of triangle n                      ! todo : sferic

      use mathconsts, only: degrad_hp, raddeg_hp
      use physicalconsts, only: earth_radius

      implicit none
      integer             :: nn
      double precision    :: x(nn), y(nn), xz, yz, xf, phi
      integer, intent(in) :: jsferic

      ! locals
      double precision :: z,den,dx2,dx3,dy2,dy3

      dx2 = x(2)-x(1)
      dx3 = x(3)-x(1)

      dy2 = y(2)-y(1)
      dy3 = y(3)-y(1)

      dx2 = getdx( x(1),y(1),x(2),y(2), jsferic )
      dy2 = getdy( x(1),y(1),x(2),y(2), jsferic )

      dx3 = getdx( x(1),y(1),x(3),y(3), jsferic )
      dy3 = getdy( x(1),y(1),x(3),y(3), jsferic )

      den = dy2*dx3-dy3*dx2
      if (den /= 0) then
         z=(dx2*(dx2-dx3)+dy2*(dy2-dy3))/den
      else
         ! call qnerror('coinciding points',' ',' ')
         z = 0d0
      endif
      if (jsferic == 1) then
         phi = (y(1)+y(2)+y(3))/3d0
         xf  = 1d0/cos( degrad_hp*phi )
         xz  = x(1) + xf*0.5d0*(dx3-z*dy3)*raddeg_hp/earth_radius
         yz  = y(1) +    0.5d0*(dy3+z*dx3)*raddeg_hp/earth_radius
      else
         xz = x(1) + 0.5d0*(dx3-z*dy3)
         yz = y(1) + 0.5d0*(dy3+z*dx3)
      endif

      end subroutine circumcenter3

end module geometry_module
