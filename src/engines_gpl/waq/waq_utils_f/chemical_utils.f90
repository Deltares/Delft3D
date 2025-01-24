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
module chemical_utils
    use m_waq_precision

    implicit none

    private
    public :: salinity_from_chloride, chlorinity_from_sal

    real(kind = real_wp), parameter  :: sal0 = 0.03  ! g/kg
    real(kind = real_wp), parameter  :: gtcl = 1.805 ! l/kg

contains

    ! Auxiliary function: convert chlorinity to salinity
    ! based on temperature
    !
    subroutine salinity_from_chloride( cl, temp, sal, density )
        real(kind = real_wp), intent(in)  :: cl
        real(kind = real_wp), intent(in)  :: temp
        real(kind = real_wp), intent(out) :: sal
        real(kind = real_wp), intent(out) :: density

        density = 1000.0 + 0.7 * cl / 1000.0 * gtcl &
                  - 0.0061 * (temp - 4.0) * (temp - 4.0)
        sal = cl * gtcl / density + sal0

    end subroutine salinity_from_chloride

    function chlorinity_from_sal( sal, temp ) result(cl)
        real(kind = real_wp), intent(in)  :: sal
        real(kind = real_wp), intent(in)  :: temp
        real(kind = real_wp)              :: cl

        real(kind = real_wp)              :: density, salcorr

        salcorr = max( 0.0_real_wp, sal - sal0 )
        density = ( 1000.0 - 0.0061 * (temp - 4.0)**2 ) / &
                  ( 1.0 - 0.7 * salcorr / 1000.0 )
        cl = salcorr * density / gtcl

    end function chlorinity_from_sal

end module chemical_utils
