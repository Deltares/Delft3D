!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      function lenstr(line,maxlen)

      use precision_part   ! single and double precision
      use timers

      implicit none   ! force explicit typing

      character(len=*) :: line
!
!     local scalars
!
      integer(ip) :: lenstr, i, maxlen
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "lenstr", ithndl )

!
!     programmer : antoon koster
!     function   : calculates actual length of string
!     date       : nov. 2001
!
      do i=maxlen,1,-1
         if (line(i:i) /= ' ') exit
      end do
      lenstr=i
      if ( timon ) call timstop ( ithndl )
      return
      end function
