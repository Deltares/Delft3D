!pinpol
subroutine pinpol (x0    , y0    , n     , x     , y     , inside)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : pinpol
! version            : v1.0
! date               : 11 May 1995
! specs last update  : clean up from 10-11-1987 version from Jan Mooiman
! programmer         : Lamber Hulsen / Jan Mooiman
!
! function           : determination whether a point is inside a
!                      concave/convex polygon or not
! method             : a vertical line is drawn through the point in
!                      question; if it crosses the polygon an odd number
!                      of times then the point is inside the polygon.
!                      Only the upper-half of the vertical-line is tested.
! reference          : k van smeden, Deltares
!
!                      quatre administration
!
!                      iquadr = 1;  if  xt >= 0  and  yt >= 0
!                      iquadr = 2;  if  xt >  0  and  yt <  0
!                      iquadr = 4;  if  xt <  0  and  yt >  0
!                      iquadr = 5;  if  xt <= 0  and  yt <= 0
!
! limitations        :
! error messages     : see below
! subroutines called : none
!
! global variables
!
! name     type      length   description
! ------   -------   ------   -----------
! det      real      1        orientation of triangle
! x0       real      1        X co-ordinate of specified location
! y0       real      1        Y co-ordinate of specified location
! x        real      n        X co-ordinates of vertices
! y        real      n        Y co-ordinates of vertices
! xt       real      nmax     transformed X co-ordiantes of vertices
! yt       real      nmax     transformed Y co-ordiantes of vertices
! inside   logical            true : point on edge or at vertex
!                             true : point inside polygon
!                             false: point outside polygon
! iquadr   integer   nmax     quatre administration
! lundia   integer   1        lun diagnostics
! n        integer   1        number of vertices
! ncross   integer   1        counter of intersection
! nerror   integer   1        number of error messages
! nqua     integer   1        added quatres
!***********************************************************************
!
   parameter    (nmax  = 10    )
!
   integer       lundia, nerror, i     , j     , nqua  , ncross,&
   &n
!
   real          x     ( n     ), y     ( n     ),&
   &xt    ( nmax  ), yt    ( nmax  )
!
   real          x0    , y0    , det
!
   integer       iquadr( nmax  )
!
   logical       inside
!
!-----------------------------------------------------------------------
!---- initialisation
!-----------------------------------------------------------------------
!
   if( n .gt. nmax )then
      write (*,'(a, i5)')&
      &'*** ERROR *** Increase NMAX in pinpol to ', n
      stop
   endif
!
   inside = .false.
   ncross = 0
!
!-----------------------------------------------------------------------
!---- translate all vertex coordinates, associate vertex with quatre
!-----------------------------------------------------------------------
!
   do 100 i = 1, n
!
      xt (i) = x (i) - x0
      yt (i) = y (i) - y0
!
      if( xt(i) .gt. 0.0 )then
!
         if( yt(i) .lt. 0.0 )then
            iquadr (i) = 2
         else
            iquadr (i) = 1
         endif
!
      else if( xt(i) .lt. 0.0 )then
!
         if( yt(i) .gt. 0.0 )then
            iquadr (i) = 4
         else
            iquadr (i) = 5
         endif
!
      else
!
         if( yt(i) .gt. 0.0 )then
            iquadr (i) = 1
         else if( yt (i) .lt. 0.0 )then
            iquadr (i) = 5
         else
            inside = .true.
            goto 999
         endif
!
      endif
!
100 continue
!
!-----------------------------------------------------------------------
!---- compute intersections with positive y-ax
!-----------------------------------------------------------------------
!
   do 200 i = 1, n
!
      j    = 1 + mod (i,n)
      nqua = iquadr (i) + iquadr (j)
!
      if( nqua .eq. 5 )then
!
         ncross = ncross + 1
!
      else if( nqua .eq. 6 )then
!
         det = xt (i) * yt (j) - xt (j) * yt (i)
         if( det .gt. 0.0 )then
            if( iquadr (i) .lt. iquadr (j) ) ncross = ncross + 1
         else if( det .lt. 0.0 )then
            if( iquadr (i) .gt. iquadr (j) ) ncross = ncross + 1
         else
            inside = .true.
            goto 999
         endif
!
      endif
!
200 continue
!
   if( mod (ncross,2) .ne. 0 ) inside = .true.
!
!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------
!
999 continue
!
   return
end
