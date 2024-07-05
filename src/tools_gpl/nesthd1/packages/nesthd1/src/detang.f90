subroutine detang (x     , y     , angle , m     , n     ,&
&mmax  , nmax  , maxbnd, nobnd         )
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
! subroutine         : detang
! version            : v1.0
! date               : June 1997
! programmer         : Theo van der Kaaij
!
! function           : determines orientation of a boundary segment
! notes              :
!***********************************************************************

   integer m     ( nobnd ,   2   ), n     ( nobnd ,   2   )

   real    pi

   real    angle ( maxbnd)

   real    x     ( maxbnd, 2 ), y    (maxbnd, 2 )

   pi = acos (-1.)
!
! -   for all boundary sections
!
   do 10 ibnd = 1, nobnd
!
! -------determine orientation (angle) (not for diagonal boundaries)
!
      if (x (ibnd,1) .lt. 1.0e19) then
         if ((m   (ibnd,2) .gt. m   (ibnd,1)) .or.&
         &(n   (ibnd,2) .gt. n   (ibnd,1)) ) then
            dx = x   (ibnd,2) - x   (ibnd,1)
            dy = y   (ibnd,2) - y   (ibnd,1)
         else
            dx = x   (ibnd,1) - x   (ibnd,2)
            dy = y   (ibnd,1) - y   (ibnd,2)
         endif

         angle (ibnd) = atan2(dy,dx)*180./pi
      else
         angle (ibnd) = 0.0
      endif
!
! -------add 180 degr. for upper or lower boundary (n direction)
!

      if (n(ibnd,1) .eq. n(ibnd,2)) then
         angle (ibnd) = angle (ibnd) + 180
      endif

10 continue

   return

end
