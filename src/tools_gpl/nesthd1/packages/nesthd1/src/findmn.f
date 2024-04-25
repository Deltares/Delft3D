!findmn
      subroutine findmn (xp    , yp    , x     , y     , mmax  , nmax  ,
     *                   mc    , nc    , mp    , np    , rmp   , rnp   ,
     *                   inside, spher                                 )
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
! subroutine         : findmn
! version            : v0.1
! date               : 6 October 1995
! specs last update  : derived from findnm subroutine of Herman Kernkamp
!                      changes with other versions of findnm:
!                      - subroutine name changed to findmn
!                      - depth points instead of zeta points
!                      - search starts at the grid cell determined by the
!                        relative M,N co-ordinates of the next boundary
!                        point with respect to the last found grid cell
! programmer         : Lamber Hulsen
!
! function           : given a pair of X,Y co-ordinates, find the
!                      M,N co-ordinates of the grid cell containing
!                      this specified location
! method             : 1. initialisation
!                      2. identify candidate grid cell starting from
!                         last found grid cell by relative M,N indices
!                         of specified location in last found cell
!                      3. check grid cell when first shot else search
!                         for grid cell
! limitations        :
! error messages     : see subroutine pinpol
! subroutines called : pinpol
!
! global variables
!
! name     type      length   description
! ------   -------   ------   -----------
! inside   logical   1        true if found
! lundia   integer   1        lun diagnostics
! mc       integer   1        overall grid dimension in M-direction
! mmax     integer   1        maximum grid dimension in M-direction
! mp       integer   1        M co-ordinate grid cell with location inside
! nc       integer   1        overall grid dimension in N-direction
! nerror   integer   1        number of error messages
! nmax     integer   1        maximum grid dimension in N-direction
! np       integer   1        N co-ordinate grid cell with location inside
! rmp      real      1        relative M co-ordinate of spec location
! rnp      real      1        relative N co-ordinate of spec location
! x        real      mmax,    X co-ordinates depth points overall model
!                    nmax
! xp       real      1        X co-ordinate of specified location
! y        real      mmax,    Y co-ordinates depth points overall model
!                    nmax
! yp       real      1        Y co-ordinate of specified location
!***********************************************************************
!
      integer       mmax  , nmax  , mc    , nc    ,
     *              mp    , np    , mb    , me    , nb    , ne    ,
     *              i     , j
!
      real          x     ( mmax  , nmax  )       ,
     *              y     ( mmax  , nmax  )
!
      real          xx    ( 5     )       , yy    ( 5     )
!
      real          xp    , yp    , rmp   , rnp   , xymiss,
     *              a     , b     , c
!
      logical       inside, spher
!
!-----------------------------------------------------------------------
!---- 1. initialisation
!-----------------------------------------------------------------------
!
      xymiss = 0.0
      mb = 1
      me = mc - 1
      nb = 1
      ne = nc - 1
!
!-----------------------------------------------------------------------
!---- 3. check all grid cells
!-----------------------------------------------------------------------
!
      do 30 i = mb, me
!
         do 20 j = nb, ne
!
!---------- set corner points
!
            xx (1) = x (i  ,j  )
            yy (1) = y (i  ,j  )
!
            xx (2) = x (i+1,j  )
            yy (2) = y (i+1,j  )
!
            xx (3) = x (i+1,j+1)
            yy (3) = y (i+1,j+1)
!
            xx (4) = x (i  ,j+1)
            yy (4) = y (i  ,j+1)
!
            xx (5) = x (i  ,j  )
            yy (5) = y (i  ,j  )
!
!---------- continue only if all corners are active
!
            if (( xx(1) /= xymiss .or. yy(1) /= xymiss ) .and.
     *          ( xx(2) /= xymiss .or. yy(2) /= xymiss ) .and.
     *          ( xx(3) /= xymiss .or. yy(3) /= xymiss ) .and.
     *          ( xx(4) /= xymiss .or. yy(4) /= xymiss )      ) then
!
!------------- specified location in grid cell ?
!
               call pinpol (xp    , yp    , 5    , xx   , yy   , inside)
!
!------------- if inside set M,N co-ordinates and compute relative M,N
!
               if( inside )then
!
                  mp  = i
                  np  = j
!
!                 xb1 = xx (2) - xx (1)
!                 xb2 = yy (2) - yy (1)
!                 xbl = sqrt (xb1 * xb1 + xb2 * xb2)
!                 yb1 = xx (4) - xx (1)
!                 yb2 = yy (4) - yy (1)
!                 ybl = sqrt (yb1 * yb1 + yb2 * yb2)
!                 r1  = xp - xx (1)
!                 r2  = yp - yy (1)
!                 rmp = (xb1 * r1 + xb2 * r2) / (xbl * xbl)
!                 rnp = (yb1 * r1 + yb2 * r2) / (ybl * ybl)

                  call  distance(spher ,xx(1),yy(1),xx(2),yy(2),a)
                  call  distance(spher ,xx(1),yy(1),xp   ,yp   ,b)
                  call  distance(spher ,xx(2),yy(2),xp   ,yp   ,c)
                  rmp = 0.5 + (b*b - c*c)/(2*a*a)

                  call  distance(spher ,xx(1),yy(1),xx(4),yy(4),a)
                  call  distance(spher ,xx(4),yy(4),xp   ,yp   ,c)
                  rnp = 0.5 + (b*b - c*c)/(2*a*a)
!
                  goto 999
!
               endif
!
            endif
!
   20    continue
!
   30 continue
!
!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------
!
  999 continue
!
      return
      end
