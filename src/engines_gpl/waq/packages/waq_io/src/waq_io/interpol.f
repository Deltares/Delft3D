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

      subroutine interpol ( result , higher , lower  , tset   , thigh  ,
     &                      tlow   , nvar   , ndim2  , iftyp  )

!       Deltares Software Centre

!>\file
!>       interpolates a (ndim2,nvar) array
!>
!>       Depending on sign of iftyp routine does:
!>       - if negative, choses lower ( block wave, propagate first )
!>       - if positive, linear interpolation
!>       Note that iftyp may differ per variable in the matrix

!     Created       : May   1988  by Leo Postma
!     Modified      : March 1993  by Jan van Beek
!                   : May   2011  by Leo Postma   : Fortran 90 look and feel

!     Logical units : none

      use timers       !   performance timers

      implicit none

!     Parameters

!     kind           function         name                   Descriptipon

      integer  ( 4), intent(in   ) :: nvar                 !< number of variables
      integer  ( 4), intent(in   ) :: ndim2                !< data per variable
      integer  ( 4), intent(in   ) :: tset                 !< interpolation time
      integer  ( 4), intent(in   ) :: thigh                !< time at end of interval
      integer  ( 4), intent(in   ) :: tlow                 !< time at start of interval
      real     ( 4), intent(  out) :: result(ndim2,nvar)   !< resulting array
      real     ( 4), intent(in   ) :: lower (ndim2,nvar)   !< lower end array
      real     ( 4), intent(in   ) :: higher(ndim2,nvar)   !< higher end array
      integer  ( 4), intent(in   ) :: iftyp (nvar )        !< interpolation type per variable

!     local decalations

      real     ( 4)  factor1      ! weight of the higher end
      real     ( 4)  factor2      ! weight of the lower end
      integer  ( 4)  ivar         ! loop counter
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "interpol", ithndl )

!        interpolate

      factor1 = float( tset - tlow ) / float( thigh - tlow )
      factor2 = 1.0 - factor1

      do ivar = 1, nvar
         if ( iftyp(ivar) .lt. 0 ) then           !    block function
            result(:,ivar) = lower(:,ivar)
         else                                     !    linear interpolation
            result(:,ivar) = lower(:,ivar)*factor2 + higher(:,ivar)*factor1
         endif
      enddo

      if (timon) call timstop( ithndl )
      return
      end
