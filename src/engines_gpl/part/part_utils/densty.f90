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
module m_densty

implicit none

contains


      real function densty( salin  , temp   )
!
!
!     Deltares
!
!
!     created             : january 1991, by a. markus
!
!
!     function            : calculates the density of seawater at a
!                           pressure of 1 atm
!
!     data supplied by gerard delvigne:
!     frank j. millero en alain poisson:
!     international one-atmosphere equation of state of seawater
!     deep sea research 28a, pp. 625-629 (1981)
!
!
!     subroutines called  : none.
!
!
!     functions   called  : none.
!
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     salin   real        1       input   salinity in g/l
!     temp    real        1       input   temperature [c]
!     tmpold  real        1       input   temperature [c] of prev.entry
!     ----    ----     ------     ------  -----------
!     a       real        1       local   temp.dependent coefficient
!     b       real        1       local   temp.dependent coefficient
!     c       real        1       local   temp.dependent coefficient
!     dens0   real        1       local   density zero
!
      use m_waq_precision     ! single/double precision
!
      implicit none     ! force explicit typing
!
!     local scalars
!
      real(sp) :: a     , b      , c
      real(sp) :: dens0 , salin
      real(sp) :: temp  , tmpold
!
!     declarations
!
!
!     calculate the temperature dependent coefficients
!
      tmpold = temp
      temp = min(100.0e0, max(0.0e0, tmpold))

      dens0  =   999.842594                       &
                +  6.793952e-02 * temp            &
                -  9.095290e-03 * temp**2         &
                +  1.001685e-04 * temp**3         &
                -  1.120083e-06 * temp**4         &
                +  6.536336e-09 * temp**5
!
      a      =     8.244930e-01                   &
                -  4.089900e-03 * temp            &
                +  7.643800e-05 * temp**2         &
                -  8.246700e-07 * temp**3         &
                +  5.387500e-09 * temp**4
!
      b      =  -  5.724660e-03                   &
                +  1.022700e-04 * temp            &
                -  1.654600e-06 * temp**2
!
      c      =     4.831400e-04
!
!     calculate the density
!
      densty = dens0 + a*salin + b*salin*sqrt(salin) + c*salin**2

      end function

end module m_densty
