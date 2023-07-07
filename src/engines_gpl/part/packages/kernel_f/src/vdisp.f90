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

module vdisp_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision_part  ! single/double precision
      use timers
!
implicit none  ! force explicit typing
!
contains
      real function vdisp( velo   , depth  , wstres )
!
!
!                   Deltares
!
!                        d e l p a r    v1.30
!
!     system administration : m. zeeuw
!
!
!     created               : july 1991, by a. markus (standard version)
!
!     function              : calculates the vertical diffusion within a
!                             layer
!
!
!     subroutines called    : none.
!
!
!     functions   called    : none.
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     depth   real        1       input   total depth in cell
!     velo    real        1       input   absolute velocity in cell
!     wstres  real        1       input   wind stress
!
      implicit none  !   force explicit typing
!
!     local scalars
!
      real(sp) :: depth, velo, wstres

!     save values between invocations
!
      save
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "vdisp", ithndl )
!
!     ubstar = sqrt(9.8/50.0/50.0) * velo
!
!     sqrt(9.8/50.0/50.0) = 0.0626099
!     ubstar = 0.0626099 * velo
!
!
!     vdisp  = 0.83*0.41*depth*sqrt(3.0) * (ubstar/6.0 + twopi*wstres/32.0)
!
!     0.83*0.41*sqrt(3.0) = 0.5894169
!     vdisp  = 0.5894169 * depth * (ubstar/6.0 + twopi * wstres / 32.0)
!     ubstar/6.0 = 0.010435 * velo
!     vdisp  = 0.5894169 * depth * (0.010435 * velo + twopi * wstres / 32.0)
!     twopi * wstres / 32.0 = 0.1963495 * twopi
!     vdisp  = 0.5894169 * depth * (0.010435 * velo + 0.1963495 * wstres)
!     0.5894169 * 0.010435  = 0.0061506
!     0.5894169 * 0.1963495 = 0.1157317
!     vdisp = depth * (0.0061506 * velo + 0.1157317 * wstres)
!
      vdisp = depth * (0.0061506_sp * velo + 0.1157317_sp * wstres)
!
!     end of function
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end function
end module

