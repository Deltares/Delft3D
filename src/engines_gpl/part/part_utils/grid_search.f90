!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module grid_search_mod
!
!  module procedure for grid searching routines, including
!
!  1. routine findcircle
!  2. routine findcell
!  3. routine part07
!
!  (for description, see routine header)
!
!
!
!  module declarations
!
!  data definition module(s)
!
use m_waq_precision               ! single/double precision
use timers
use random_generator
!
!  module procedure(s)
!
implicit none               ! force explicit typing
!
contains
! -------------------------------------------------------------------------------------
!     Routine findcircle
! -------------------------------------------------------------------------------------
      subroutine findcircle   ( x    , y  , radius , np     , mp , &
                          lgrid, dx , dy     , lcircl )
!
!                        Deltares
!
!                        d e l p a r    v3.10
!
!
!     system administration : m. zeeuw
!
!
!     created               : july     1991, by l. postma
!
!
!     function              : finds a suitable place for a particle
!
!     modified              : june 1996 by rj vos: option on circle (lcircl)
!
!     logical unit numbers  : none.
!
!
!     subroutines called    : none.
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     dx      real      mnmaxk    input   dx of the cells
!     dy      real      mnmaxk    input   dy of the cells
!     lcircl  locigal     1       input   if true particles on circle
!     lgrid   integer  nmax*mmax  input   active grid table
!     mp      integer     1       in/out  m-values of particle
!     nmax    integer     1       input   dimension of lgrid
!     np      integer     1       in/out  n-values of particle
!     radius  real        1       input   radius of the load
!     x       real        1       in/out  x-value of particle
!     y       real        1       in/out  y-value of particle
!     ----    ----     ------     ------  -----------
!     angle   real        1       local   randomized angle
!     cflag   logical     1       local   .false. when ready
!     movx    logical     1       local   .true. when move in x dir.
!     movy    logical     1       local   .true. when move in y dir.
!     n0      integer     1       local   lgrid value
!     radiuh  real        1       local   randomized radius
!     rseed   real        1       local   seed for randomizer
!
!
!     save values between invocations
!
      save
!
!     declarations
!
      real(dp)     ::  rseed =  0.5d+00
      logical      ::  cflag , movx  , movy
      logical      ::  lcircl
!
!     parameters
!
      real(sp), parameter   :: twopi  = 6.2831853
!
!     dimensioning
!
      real(sp)   , dimension(:)    :: dx   , dy
      integer(sp), dimension(:,:)  :: lgrid
!
!     local scalars
!
      integer (int_wp ) ::   mp   , n0     , np
      real    (sp) ::   angle, radiuh , radius , x   ,    y
!
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "findcircle", ithndl )
!
      if (radius  >=  0.1) then
!
!       test for correct x- and y-values
!
        n0      = lgrid(np, mp)
        if(.not.lcircl) then
           radiuh = radius * sqrt(rnd(rseed))
        else
           radiuh = radius
        endif
        angle  = rnd(rseed) * twopi
        x = x + radiuh * sin(angle) / dx(n0)
        y = y + radiuh * cos(angle) / dy(n0)
!
  100   cflag = .true.
        movx  = .false.
        movy  = .false.
!
        if     (x  >  1.0) then
!
          cflag = .false.
          movx  = .true.
          if (lgrid(np    , mp + 1)  < 1) then
            x  = 2.0 - x
          else
            x  = x - 1.0
            mp = mp + 1
          endif
          x = x * dx(n0)
!
        elseif (x  < 0.0) then
!
          cflag = .false.
          movx  = .true.
          if (lgrid(np    , mp - 1)  < 1) then
            x  = -x * dx(n0)
          else
            x  =  x * dx(n0)
            mp =  mp - 1
            n0  =  lgrid(np, mp)
            x  =  x + dx(n0)
          endif
!
        endif
!
        if     (y  >  1.0) then
!
          cflag = .false.
          movy  = .true.
          if (lgrid(np + 1, mp    )  < 1) then
            y  = 2.0 - y
          else
            y  = y - 1.0
            np = np + 1
          endif
          y = y * dy(n0)
!
        elseif (y  < 0.0) then
!
          cflag = .false.
          movy  = .true.
          if (lgrid(np - 1, mp    )  < 1) then
            y  = -y * dy(n0)
          else
            y  =  y * dy(n0)
            np =  np - 1
            n0  =  lgrid(np, mp)
            y  =  y + dy(n0)
          endif
!
        endif
!
        if (.not. cflag) then
          n0 = lgrid(np, mp)
          if (movx) then
            x = x / dx(n0)
          endif
          if (movy) then
            y = y / dy(n0)
          endif
          goto 100
        endif
      endif
!
!     end of subroutine
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine findcircle


! -------------------------------------------------------------------------------------
!     Routine findpoly
! -------------------------------------------------------------------------------------
!     function              : finds a suitable place for a particle in a polygon

      subroutine findpoly   (nmax    , mmax    , lgrid   , lgrid2  , xcor    ,     &
                             ycor    , nrowsmax, xpol    , ypol    , xpart   ,     &
                             ypart   , npart   , mpart)

      use m_waq_precision ! single/double precision
      use pinpok_mod
      use timers

      integer  ( int_wp ), intent(in   ) :: nmax                    !< first dimension matrix
      integer  ( int_wp ), intent(in   ) :: mmax                    !< second dimension matrix
      integer  ( int_wp ), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( int_wp ), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( real_wp), intent(in   ) :: xcor  (nmax*mmax)
      real     ( real_wp), intent(in   ) :: ycor  (nmax*mmax)
      integer  ( int_wp ), intent(in   ) :: nrowsmax                !< dimension of polygons
      real     ( real_wp), intent(in   ) :: xpol  (nrowsmax)        !< xvalues polygons
      real     ( real_wp), intent(in   ) :: ypol  (nrowsmax)        !< yvalues polygons
      real     ( real_wp), intent(  out) :: xpart                   !< x of the particle
      real     ( real_wp), intent(  out) :: ypart                   !< y of the particle
      integer  ( int_wp ), intent(  out) :: npart                   !< n of the particle
      integer  ( int_wp ), intent(  out) :: mpart                   !< m of the particle

      real   (dp), save             :: rseed = 0.5d0

      real   (sp)                   :: xmin,xmax,ymin,ymax,xx,yy
      integer(int_wp )                   :: nnpart, mmpart
      real   (sp)                   :: xxcel, yycel
      integer(int_wp )                   :: i, ier, try, inside

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "findpoly", ithndl )

      xmin = minval(xpol)
      xmax = maxval(xpol)
      ymin = minval(ypol)
      ymax = maxval(ypol)

      try = 0
      inside = 0
      do while (inside == 0 .and. try .lt. 1000)
         xx = rnd(rseed)*(xmax-xmin) + xmin
         yy = rnd(rseed)*(ymax-ymin) + ymin
         call pinpok(xx, yy, nrowsmax, xpol, ypol , inside )
         if (inside == 1) then

!        transform world coordinates (xx,yy) into cell-coordinates(xxcel,yycel)
            call part07 (lgrid  , lgrid2 , nmax   , mmax   , xcor  ,       &
                         ycor   , xx     , yy     , nnpart , mmpart,       &
                         xxcel  , yycel  , ier )

            if ( ier == 0 ) then
               xpart = xxcel
               ypart = yycel
               npart = nnpart
               mpart = mmpart
            else
               inside = 0
               try = try + 1
            end if
         else
            try = try + 1
         endif
      enddo

      if (try.eq.1000) then
         continue
         ! something went wrong
      end if

      if ( timon ) call timstop ( ithndl )
      end subroutine findpoly


! -------------------------------------------------------------------------------------
!     Routine findcell
! -------------------------------------------------------------------------------------

      subroutine findcell ( nmax   , mmax   , xnloc  , ynloc  , lgrid  ,   &
                            lgrid2 , xb     , yb     , nmloc  )

!     Deltares Software Centre

      use pinpok_mod
      implicit none

!     Arguments

!     kind            function         name                description

      integer  ( int_wp ), intent(in   ) :: nmax              !< 1st dimension of the flow grid
      integer  ( int_wp ), intent(in   ) :: mmax              !< 2nd dimension of the flow grid
      real     ( real_wp), intent(in   ) :: xnloc             !< x-value of the point
      real     ( real_wp), intent(in   ) :: ynloc             !< y-value of the point
      integer  ( int_wp ), intent(in   ) :: lgrid (nmax,mmax) !< active grid matrix
      integer  ( int_wp ), intent(in   ) :: lgrid2(nmax,mmax) !< total grid matrix
      real     ( real_wp), intent(in   ) :: xb    (nmax*mmax) !< x-values of the grid
      real     ( real_wp), intent(in   ) :: yb    (nmax*mmax) !< y-values of the grid
      integer  ( int_wp ), intent(  out) :: nmloc             !< linear index of grid cell

!     Locals

      real     ( real_wp) amin                !  used to find the minimum
      integer  ( int_wp ) ns, ms              !  location of the minimum
      integer  ( int_wp ) n , m               !  loop variables for the grid
      real     ( real_wp) distpow2            !  distance ** 2
      integer  ( int_wp ) i , j               !  loop variables for the grid
      real     ( real_wp) x(4), y(4)          !  x,y of cell corners
      integer  ( int_wp ) nm, nmd, ndm, ndmd  !  help variables indices in the grid
      integer  ( int_wp ) inside              !  1 if point is inside the grid-cell
      integer  ( int_wp ) ierror              !  0 if successful, 1 if on error

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "findcell", ithndl )

      ierror = 1
      amin   = 1.0e+30
      ns   = 0
      ms   = 0
      do m = 2, mmax
         do n = 2, nmax
            nm   = lgrid2( n  , m   )
            ndmd = lgrid2( n-1, m-1 )
            ndm  = lgrid2( n-1, m   )
            nmd  = lgrid2( n  , m-1 )

            ! active bed level point

            if ( ndmd > 1 .or. ndm > 1 .or. nm > 1 .or. nmd > 1 ) then
               distpow2 = (xb(ndmd)-xnloc)**2 + (yb(ndmd)-ynloc)**2
               if ( distpow2 .lt. amin ) then
                  amin   = distpow2
                  ms     = m-1
                  ns     = n-1
               endif
            endif
         enddo
      enddo
!
outer1:do j = 0, 1
         do i = 0, 1

            n = ns + i
            m = ms + j
            if ( lgrid( n, m ) .le. 0 ) cycle
            nm   = lgrid2( n  , m   )
            ndmd = lgrid2( n-1, m-1 )
            ndm  = lgrid2( n-1, m   )
            nmd  = lgrid2( n  , m-1 )
            x(1) = xb(ndmd)
            x(2) = xb(ndm )
            x(3) = xb(nm  )
            x(4) = xb(nmd )
            y(1) = yb(ndmd)
            y(2) = yb(ndm )
            y(3) = yb(nm  )
            y(4) = yb(nmd)
            call pinpok( xnloc, ynloc, 4, x, y, inside )
            if ( inside .eq. 1 ) then
               ierror = 0
               nmloc  = nm
               exit outer1
            endif

         enddo
      enddo  outer1

      if ( ierror .ne. 0 ) then ! the method above is faster, but doesn't always work with DD. When the cell was not found use, pinpok for all cells until found
outer2:  do m = 2, mmax
            do n = 2, nmax
               if ( lgrid( n, m ) .le. 0 ) cycle
               nm   = lgrid2( n  , m   )
               ndmd = lgrid2( n-1, m-1 )
               ndm  = lgrid2( n-1, m   )
               nmd  = lgrid2( n  , m-1 )

               x(1) = xb(ndmd)
               x(2) = xb(ndm )
               x(3) = xb(nm  )
               x(4) = xb(nmd )
               y(1) = yb(ndmd)
               y(2) = yb(ndm )
               y(3) = yb(nm  )
               y(4) = yb(nmd)
               call pinpok( xnloc, ynloc, 4, x, y, inside )
               if ( inside .eq. 1 ) then
                  ierror = 0
                  nmloc  = nm
                  exit outer2
               endif
            enddo
         enddo outer2
      end if

      if ( ierror .ne. 0 ) nmloc = 1           ! still not found, so probably outside the grid?!?

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine findcell

! -------------------------------------------------------------------------------------
!     Routine part07
! -------------------------------------------------------------------------------------

      subroutine part07 ( lgrid  , lgrid2 , nmax   , mmax   , xb     ,  &
                          yb     , xnloc  , ynloc  , nmloc  , mmloc  ,  &
                          xmloc  , ymloc  , ierror )

!       Deltares Software Centre

!>\file
!>            Determines the grid cell and relative coordinates of a give point in global coordinates

!     System administration : Antoon Koster

!     created               : July 1994, by Robert Vos

!     logical unit numbers  : none.

!     subroutines called    : bilin5 - bilinear coefficients within the grid

!     functions   called    : none.

!     Arguments

!     kind            function         name                   description

      integer  ( int_wp ), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( int_wp ), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( int_wp ), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( int_wp ), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( real_wp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( real_wp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      real     ( real_wp), intent(in   ) :: xnloc                !< x of the point to locate
      real     ( real_wp), intent(in   ) :: ynloc                !< y of the point to locate
      integer  ( int_wp ), intent(  out) :: nmloc                !< first grid index of the point
      integer  ( int_wp ), intent(  out) :: mmloc                !< second grid index of the point
      real     ( real_wp), intent(  out) :: xmloc                !< local x in the grid of the point
      real     ( real_wp), intent(  out) :: ymloc                !< local y in the grid of the point
      integer  ( int_wp ), intent(  out) :: ierror               !< if 1 if no valid location was found

!     locals

      real   (real_wp) :: x(4), y(4)      !   x,y of the corner points of the found cell
      real   (real_wp) :: w(6)            !   work array of bilin5
      integer(int_wp ) :: n0, n1, n2, n3  !   4 linear grid indices of the cornerpoints of the cell
      integer(int_wp ) :: ier             !   error variable of bilin5

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part07", ithndl )

      x = 0
      y = 0
      w = 0
      ierror = 0

!     find the grid cel n0 in (nmloc,mmloc) in which (xnloc,ynloc) is located

      call findcell ( nmax   , mmax   , xnloc  , ynloc  , lgrid  ,  &
                      lgrid2 , xb     , yb     , n0     )

!     point in active grid cel n0 = lgrid2(nmloc,mmloc)

      if ( n0 .eq. 1 ) then          ! ( nloc  , mloc   )
         ierror = 1
         goto 9999
      endif
      n1 = n0 - 1                    ! ( nloc-1, mloc   )
      n2 = n0 - nmax                 ! ( nloc  , mloc-1 )
      n3 = n0 - nmax - 1             ! ( nloc-1, mloc-1 )
      nmloc = mod(n0-1, nmax) + 1
      mmloc =    (n0-1)/nmax  + 1

      x(1) = xb(n3)
      x(2) = xb(n1)
      x(3) = xb(n0)
      x(4) = xb(n2)
      y(1) = yb(n3)
      y(2) = yb(n1)
      y(3) = yb(n0)
      y(4) = yb(n2)
      call bilin5 ( x, y, xnloc, ynloc, w, ier )
      xmloc = w(5)                   ! local weight factors m-direction
      ymloc = w(6)                   ! local weight factors n-direction

 9999 if ( timon ) call timstop ( ithndl )
      return

      end subroutine part07

! -------------------------------------------------------------------------------------
!     Routine part07nm
! -------------------------------------------------------------------------------------

      subroutine part07nm ( lgrid  , lgrid2 , nmax   , mmax   , xb     ,  &
                            yb     , xnloc  , ynloc  , nmloc  , mmloc  ,  &
                            xmloc  , ymloc  , ierror )

!       Deltares Software Centre

!>\file
!>            Determines the relative coordinates to a given grid cell of a given point in global coordinates

!     System administration : Antoon Koster

!     created by            : Michelle Jeuken

!     logical unit numbers  : none.

!     subroutines called    : bilin5 - bilinear coefficients within the grid

!     functions   called    : none.

!     Arguments

!     kind            function         name                   description

      integer  ( int_wp ), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( int_wp ), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( int_wp ), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( int_wp ), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( real_wp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( real_wp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      real     ( real_wp), intent(in   ) :: xnloc                !< x of the point to locate
      real     ( real_wp), intent(in   ) :: ynloc                !< y of the point to locate
      integer  ( int_wp ), intent(in   ) :: nmloc                !< first grid index of the point
      integer  ( int_wp ), intent(in   ) :: mmloc                !< second grid index of the point
      real     ( real_wp), intent(  out) :: xmloc                !< local x in the grid of the point
      real     ( real_wp), intent(  out) :: ymloc                !< local y in the grid of the point
      integer  ( int_wp ), intent(  out) :: ierror               !< if 1 if no valid location was found

!     locals

      real   (real_wp) :: x(4), y(4)      !   x,y of the corner points of the found cell
      real   (real_wp) :: w(6)            !   work array of bilin5
      integer(int_wp ) :: n0, n1, n2, n3  !   4 linear grid indices of the cornerpoints of the cell
      integer(int_wp ) :: ier             !   error variable of bilin5

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part07nm", ithndl )

      x = 0
      y = 0
      w = 0
      ierror = 0

!     point in active grid cel n0 = lgrid2(nmloc,mmloc)
      n0   = lgrid2( nmloc  , mmloc   )
      if ( n0 .eq. 1 ) then          ! ( nloc  , mloc   )
         ierror = 1
         goto 9999
      endif
      n1 = n0 - 1                    ! ( nloc-1, mloc   )
      n2 = n0 - nmax                 ! ( nloc  , mloc-1 )
      n3 = n0 - nmax - 1             ! ( nloc-1, mloc-1 )

      x(1) = xb(n3)
      x(2) = xb(n1)
      x(3) = xb(n0)
      x(4) = xb(n2)
      y(1) = yb(n3)
      y(2) = yb(n1)
      y(3) = yb(n0)
      y(4) = yb(n2)
      call bilin5 ( x, y, xnloc, ynloc, w, ier )
      xmloc = w(5)                   ! local weight factors m-direction
      ymloc = w(6)                   ! local weight factors n-direction

 9999 if ( timon ) call timstop ( ithndl )
      return

      end subroutine part07nm


subroutine bilin5(xa        ,ya        ,x0        ,y0        ,w         , &
                & ier       )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2012-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
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
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use m_waq_precision
    implicit none
!
! Global variables
!
    integer, intent(out)           :: ier
    real(sp), intent(in)               :: x0
    real(sp), intent(in)               :: y0
    real(sp), dimension(6), intent(out) :: w
    real(sp), dimension(4), intent(in) :: xa
    real(sp), dimension(4), intent(in) :: ya
!
!
! Local variables
!
    real(sp)                           :: a
    real(sp)                           :: a21
    real(sp)                           :: a22
    real(sp)                           :: a31
    real(sp)                           :: a32
    real(sp)                           :: a41
    real(sp)                           :: a42
    real(sp)                           :: b
    real(sp)                           :: c
    real(sp)                           :: det
    real(sp)                           :: discr
    real(sp)                           :: eta
    real(sp)                           :: x
    real(sp)                           :: x1
    real(sp)                           :: x2
    real(sp)                           :: x3
    real(sp)                           :: x3t
    real(sp)                           :: x4
    real(sp)                           :: xi
    real(sp)                           :: xt
    real(sp)                           :: y
    real(sp)                           :: y1
    real(sp)                           :: y2
    real(sp)                           :: y3
    real(sp)                           :: y3t
    real(sp)                           :: y4
    real(sp)                           :: yt
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! Author: H. Petit
    !
    !
    !     read(12,*)x1,y1,f1
    x1 = xa(1)
    y1 = ya(1)
    !     read(12,*)x2,y2,f2
    x2 = xa(2)
    y2 = ya(2)
    !     read(12,*)x3,y3,f3
    x3 = xa(3)
    y3 = ya(3)
    !     read(12,*)x4,y4,f4
    x4 = xa(4)
    y4 = ya(4)
    x = x0
    y = y0
    ! The bilinear interpolation problem is first transformed
    ! to the quadrangle with nodes
    ! (0,0),(1,0),(x3t,y3t),(0,1)
    ! and required location (xt,yt)
    a21 = x2 - x1
    a22 = y2 - y1
    a31 = x3 - x1
    a32 = y3 - y1
    a41 = x4 - x1
    a42 = y4 - y1
    det = a21*a42 - a22*a41
    if (abs(det)<1D-20) then
       ! write (*, *) 'surface is zero'
       ier = 1
       goto 99999
    endif
    x3t = (a42*a31 - a41*a32)/det
    y3t = ( - a22*a31 + a21*a32)/det
    xt = (a42*(x - x1) - a41*(y - y1))/det
    yt = ( - a22*(x - x1) + a21*(y - y1))/det
    if ((x3t<0.0) .or. (y3t<0.0)) then
       ! write (*, *) 'distorted quadrangle'
       ier = 1
       goto 99999
    endif
    if (abs(x3t - 1.0)<1.0D-7) then
       xi = xt
       if (abs(y3t - 1.0)<1.0D-7) then
          eta = yt
       elseif (abs(1.0 + (y3t - 1.0)*xt)<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          eta = yt/(1.0 + (y3t - 1.0)*xt)
       endif
    elseif (abs(y3t - 1.0)<1.0D-6) then
       eta = yt
       if (abs(1.0 + (x3t - 1.0)*yt)<1.D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          xi = xt/(1.0 + (x3t - 1.0)*yt)
       endif
    else
       a = y3t - 1.0
       b = 1.0 + (x3t - 1.0)*yt - (y3t - 1.0)*xt
       c = -xt
       discr = b*b - 4.0*a*c
       if (discr<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       endif
       xi = ( - b + sqrt(discr))/(2.0*a)
       eta = ((y3t - 1.0)*(xi - xt) + (x3t - 1.0)*yt)/(x3t - 1.0)
    endif
    w(1) = (1.0 - xi)*(1.0 - eta)
    w(2) = xi*(1.0 - eta)
    w(3) = xi*eta
    w(4) = eta*(1.0 - xi)
    w(5) = xi  ! local weight factor in direction from point 1 to 2
    w(6) = eta ! local weight factor in direction from point 1 to 4
    return
99999 continue
end subroutine bilin5


end module grid_search_mod
