!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module random_generator
    implicit none
contains

    real function rnd(rseed)
        !
        !
        !                   Deltares
        !
        !                        d e l p a r    v1.30
        !
        !
        !     system administration : m. zeeuw
        !
        !
        !     created               : april 1990, by m. zeeuw
        !
        !
        !     function              : the imsl random generator
        !
        !
        !     logical unit numbers  : none.
        !
        !
        !     subroutines called    : none.
        !
        !
        !     functions   called    : none.
        !
        !
        !     parameters            :
        !
        !     name    kind     length     funct.  description
        !     ====    ====     ======     ======  ===========
        !     rseed   double      1       in/out  randomizer seed
        !     ----    ----     ------     ------  -----------
        !     help    double      1       local   help variable
        !     s       double      1       local   help variable for rseed
        !
        !
        !  module declarations
        !
        !
        !  data definition module(s)
        !
        use m_waq_precision    ! single and double precision
        !
        !
        !     save values between invocations
        !
        save
        !
        !     declarations
        !
        real(dp), intent(inout) :: rseed
        real(dp) :: help, s
        !
        s = rseed
        rseed = dmod(1.6807d+04 * s, 2.147483647d+09)
        help = 4.656612873d-10 * rseed
        rnd = real(help, kind = kind(rnd))
        !
        !     end of function
        !
    end function
end module random_generator
