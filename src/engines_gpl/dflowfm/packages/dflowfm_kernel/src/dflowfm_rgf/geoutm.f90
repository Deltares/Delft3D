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

      subroutine GEOUTM (xgeo,ygeo,xutm,yutm,Izone,nzone,IERR)
      implicit none
      integer :: nzone
! ----------------------------------------------------------------------
!
!     conversion of geographical (lat, lon) --> UTM coordinates (x, y, zone)
!     geographical coordinates (lat, lon) expressed in decimal degrees.
!
! ----------------------------------------------------------------------
!     arguments:
!     xgeo    i    double precision ::    longitude (geographical coordinate)
!     ygeo    i    double precision ::    lattitude (geographical coordinate)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xutm    o    double precision ::    easting (UTM)
!     yutm    o    double precision ::    northing (UTM)
!     zone    o    integer   zone (UTM)
!     ierr    o    integer   error code (zero for no error)
!
      double precision :: xgeo,ygeo,a,e,xutm,yutm
      integer      Izone,ierr
!
!     local variables:
!     pi           double precision ::    3.14....
!     fn           double precision ::    false northing
!     fe           double precision ::    false easting
!     fi           double precision ::    geographic lattitude (equivalent to lat)
!     dl           double precision ::    longitude within zone
!     dl2          double precision ::    dl*dl
!     s            double precision ::    sin(fi)
!     ss           double precision ::    s*s
!     sc           double precision ::    sin(fi)*cos(fi)
!     c            double precision ::    cos(fi)
!     cc           double precision ::    c*c
!     cccc         double precision ::    c*c*c*c
!     f1           double precision ::    coefficient in function dm(fi)
!     f2           double precision ::    coefficient in function dm(fi)
!     f3           double precision ::    coefficient in function dm(fi)
!     f4           double precision ::    coefficient in function dm(fi)
!     e2           double precision ::    e*e
!     e4           double precision ::    e2*e2
!     e6           double precision ::    e2*e4
!     n            double precision ::    e*e/(1-e*e)
!     nn           double precision ::    n*n
!     x            double precision ::    UTM easting (similar to xutm)
!     y            double precision ::    UTM northing (similar to yutm)
!     rp           double precision ::    function rp(fi)
!     dm           double precision ::    function dm(fi)
!     gx           double precision ::    function gx(fi,dl)
!     gy           double precision ::    function gy(fi,dl)
!
      double precision :: pi,fn,fe
      double precision :: fi,dl,dl2,s,ss,sc,c,cc,cccc,f1,f2,f3,f4,e2,e4,e6
      double precision :: n,nn,x,y,rp,dm,gx,gy
      COMMON /ELLIPS/ A,E
!
! -----------------------------------------------------------------------------
!     t.j.zitman                                  last update: 10 december 1990
! -----------------------------------------------------------------------------
!
!     initialize constants
!
      pi       = 4d0*ATAN(1d0)
!
      e2     = e**2
      e4     = e2**2
      e6     = e2*e4
      n      = e2/(1d0-e2)
      nn     = n**2
      f1     = 1d0 - (1d0/4d0)*e2 - (3d0/64d0)*e4 -  ( 5d0/256d0)*e6
      f2     =       (3d0/8d0)*e2 + (3d0/32d0)*e4 + (45d0/1024d0)*e6
      f3     =                    (15d0/256d0)*e4 + (45d0/1024d0)*e6
      f4     =                                      (35d0/3072d0)*e6
!
!     set false northing and false easting
!
   !  IF (ygeo.LT.0.0) then
   !    ygeo = -ygeo
   !    fn   = 1.0E+07
   !    fn   = Y_offset
   !  else
   !    fn   = 0.0d0
   !  endif

      fn = 0D0
      fe = 5d+05
!
!     determine zone
!
      Nzone = INT( (xgeo+180)/6 ) + 1
      if (IZONE .EQ. 0) then
         IZONE = NZONE
      endif
!
!     set fi and dl
!
      fi = ygeo*pi/180d0
      dl = (xgeo + 177d0 - 6d0*FLOAT(Izone-1))*pi/180d0
!
!     constants, related to fi
!
      s      = SIN(fi)
      ss     = s**2
      c      = COS(fi)
      cc     = c**2
      cccc   = cc**2
      sc     = s*c
!
!     values of sub-functions
!
      rp     = a/SQRT(1d0-e2*ss)
      dm     = a*( f1*fi - f2*SIN(2d0*fi)  + f3*SIN(4d0*fi) - f4*SIN(6d0*fi) )
      dl2    = dl**2
      gx     = dl2*(2d0*cc - 1d0 + nn*cccc)/6d0
      gy     = dl2*(6d0*cc - 1d0 + 9d0*nn*cccc)/12d0
!
!     function values x and y
!
      x      = rp*dl*c*(1d0+gx)
      y      = dm + rp*0.5d0*dl2*sc*(1d0+gy)
!
!     set UTM x- and y-coordinates
!
      xutm   = 0.9996d0*x + fe
      yutm   = 0.9996d0*y + fn
!
!     set no error
!
      ierr   = 0
!
      continue
      return
      end subroutine geoutm
