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
      module m_cnvtim

      implicit none

      contains


      subroutine cnvtim ( ibrk  , ifact , dtflg , dtflg3 )

!     Deltares Software Centre

!>\File
!>        Conversion of a relative integer DDHHMMSS or YYDDDHH time to seconds
!>
!>        DDDDHHMMSS allows up to 2146 days or some 5.8 years\n
!>        YYDDDHH allows more than needed (NB: a year is 365 days !)\n
!>        If date is false, result is multiplied by ifact.\n
!>        Absolute offset time should be chosen such that all relative
!>        times fit into 68 years from the absolute offset !

!     Created       : July  1991 by Jan van Beek

!     Modified      : April 2011 by Leo Postma
!                                   Fortran 90 look and feel

!     Logical units : none

      implicit none

!     Parameters

!     kind           function         name       Descriptipon

      integer   (4), intent(inout) :: ibrk     !< breakpoint to convert
      integer   (4), intent(in   ) :: ifact    !< factor between time scales
      logical      , intent(in   ) :: dtflg    !< if true then 'date'-format
      logical      , intent(in   ) :: dtflg3   !< if true then YYDDDHH

!     Local

      integer   (4) isec    ! seconds
      integer   (4) imin    ! minutes
      integer   (4) ihour   ! hours
      integer   (4) iday    ! days
      integer   (4) iyear   ! years

      if ( dtflg ) then
         if ( dtflg3 ) then
            ihour   = mod (    ibrk,100)
            iday    = mod (int(ibrk/100),1000)
            iyear   =      int(ibrk/100000)
            ibrk    = 3600*ihour + 86400*iday + 31536000*iyear
         else
            isec    = mod (ibrk,100)
            imin    = mod (int(ibrk/100),100)
            ihour   = mod (int(ibrk/10000),100)
            iday    = int (int(ibrk/1000000))
            ibrk    = isec + 60*imin + 3600*ihour + 86400*iday
         endif

      else

         ibrk = ifact * ibrk

      endif

      return
      end
      end module m_cnvtim
