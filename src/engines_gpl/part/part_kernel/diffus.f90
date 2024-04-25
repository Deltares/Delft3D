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
module m_diffus

implicit none

contains


      real function diffus( densb  , denst  , pblay   , depth  ,  &
                            diff0  , ufric  , gamma  )
!
!
!     Deltares
!
!
!     created             : january 1991, by a. markus
!
!
!     function            : calculates the diffusion coefficient in
!                           a two-layers system
!
!     based on study by gerard delvigne:
!     journal of hydroaulic engineering, 112 (1986) 11, november
!     or:
!     Deltares communications no. 373
!     december 1986
!
!
!     subroutines called  : none.
!
!
!     functions called    : none.
!
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     densb   real        1       input   density of bottom layer
!     denst   real        1       input   density of top layer
!     pblay   real        1       input   relative thickness pycnocline
!     depth   real        1       input   total water depth
!     diff0   real        1       input   vertical diffusion (no
!                                         stratification)
!     ufric   real        1       input   friction velocity
!     gamma   real        1       input   second empirical coefficient
!                                         (circa 12.)
!     ----    ----     ------     ------  -----------
!     diffsv  real        1       local   save-value diffus if pblay = pbold
!
!   data definition module(s)
!
      use m_waq_precision    ! single/double precision
      use timers
!
      implicit none    ! force explicit typing
!
!     save values between invocations
!
      real(sp), save   :: pbold , z1    , z2
!
!     declarations
!
!     parameters
!
      real(sp), parameter  :: grav2 = 2.0 * 9.8
!
!
      logical :: first = .true.
!
!     local scalars
!
      real(sp) ::  diff0,     densb,   denst, depth,  gamma
      real(sp) ::  pblay,  richb,   richrd,   ufric,  zistar
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "diffus", ithndl )

!
      if (first) then
        first = .false.
      else
!
!       check whether the relative thickness is the same as before
!
        if (pblay == pbold) then
          goto 100
        endif
      endif
!
!     save input relative thickness pycnocline
!
      pbold = pblay
!
!     calculate the coefficients for two domains:
!     k1/k3 and m domain (no: m domain has almost trivial relation)
!
      if (pblay  >  0.5) then
        zistar = 1.0 - pblay
      else
        zistar = pblay
      endif
!
      z1    = zistar  - zistar**2
      z2    = 1.0/3.0 - 2.0*zistar + 3.0*zistar**2 - 4.0/3.0*zistar**3
      richb = 1.0 / (1.0 - zistar)
!
!     calculate the diffusion coefficient:
!     (zi = pblay)
!     1. if richardson number < 1/(1-zi) - k domain
!     2. else m domain
!     calculate the value for a relative z coordinate equal to zi
!
  100 richrd = gamma * grav2 * depth * (densb - denst)  &
               / (ufric**2 * (densb + denst))
!
!     assign value to function
!
      if     (richrd <=   0.0) then
        diffus = diff0 * z1 * (1.0 - richrd)
      elseif (richrd  < richb) then
        diffus = diff0 * (z1 + z2 * richrd)
      else
        diffus = diff0 * (3.0 * richrd - 2.0) / (6.0 * richrd**2)
      endif
!
!     end of function
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end function



end module m_diffus
