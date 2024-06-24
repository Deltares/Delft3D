!nearmn
subroutine nearmn (xp    , yp    , x     , y     , mmax  , nmax  ,&
&mc    , nc    , mp    , np    , icom          )
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
! subroutine         : nearmn
! version            : v1.0
! date               : 28 October 1995
! specs last update  :
! programmer         : Lamber Hulsen
!
! function           : given a pair of X,Y co-ordinates, find the
!                      M,N co-ordinates of the nearest grid cell
! method             : 1. start from the last found grid cell
!                      2. find grid cell with shortest distance
! limitations        :
! error messages     : none
! subroutines called : none
!
! global variables
!
! name     type      length   description
! ------   -------   ------   -----------
! mc       integer   1        overall grid dimension in M-direction
! mmax     integer   1        maximum grid dimension in M-direction
! mp       integer   1        M co-ordinate of specified location
! nc       integer   1        overall grid dimension in N-direction
! nmax     integer   1        maximum grid dimension in N-direction
! np       integer   1        N co-ordinate of specified location
! x        real      mmax,    X co-ordinates depth points overall model
!                    nmax
! xp       real      1        X co-ordinate of specified location
! y        real      mmax,    Y co-ordinates depth points overall model
!                    nmax
! yp       real      1        Y co-ordinate of specified location
!***********************************************************************
!
   integer       mmax  , nmax  , mc    , nc    ,&
   &mp    , np    , i     , j
!
   integer       icom  ( mmax  , *  )
!
   real          x     ( mmax  , nmax  )       ,&
   &y     ( mmax  , nmax  )
!
   real          xp    , yp    , eps   , dist0 , dist  ,&
   &xld   , xlu   , xrd   , xru   , xc    , xdif  ,&
   &yld   , ylu   , yrd   , yru   , yc    , ydif
!
!-----------------------------------------------------------------------
!---- initialisation
!-----------------------------------------------------------------------
!
   eps   = 1.0e-6
   dist0 = 1.0e37
!
!-----------------------------------------------------------------------
!---- 2. find grid cell with shortest distance
!-----------------------------------------------------------------------
!
   do 30 i = 2, mc
!
      do 20 j = 2, nc
!
!---------- set corner points
!
         xld = x (i-1,j-1)
         yld = y (i-1,j-1)
!
         xrd = x (i  ,j-1)
         yrd = y (i  ,j-1)
!
         xru = x (i  ,j  )
         yru = y (i  ,j  )
!
         xlu = x (i-1,j  )
         ylu = y (i-1,j  )
!
!---------- continue only if all corners are active
!
         if( abs(xld) .gt. eps .and. abs(xrd) .gt. eps .and.&
         &abs(xru) .gt. eps .and. abs(xlu) .gt. eps .and.&
         &icom(i,j) .eq. 1 ) then
!
!------------- centre of grid cell
!
            xc = 0.25 * (xld + xrd + xru + xlu)
            yc = 0.25 * (yld + yrd + yru + ylu)
!
!------------- distance from specified location and centre of grid cell
!
            xdif = xp - xc
            ydif = yp - yc
            dist = sqrt (xdif * xdif + ydif * ydif)
!
!------------- if closer set M,N co-ordinates and reset minimal distance
!
            if( dist .lt. dist0 )then
               mp  = i
               np  = j
               dist0 = dist
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
   return
end
