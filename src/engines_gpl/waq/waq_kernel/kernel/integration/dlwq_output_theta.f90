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
module m_dlwq_output_theta
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine dlwq_output_theta (nrvart, ounam, ipoint, nocons, nopa, &
            nofun, nosfun, notot, noseg, noloc, &
            proloc, nodef, theta)

        !     Deltares - Delft Software Department

        !     Created   :          2007 by Pauline van Slingerland

        !     Function  : puts the theta array in the process local array for output

        !     Modified  :

        use timers
        implicit none

        !     Arguments           :

        !     Kind           Function         Name                  Description

        integer(kind = int_wp), intent(in) :: nrvart              ! number of output parameters
        character(20), intent(in) :: ounam(nrvart)       ! output parameter names
        integer(kind = int_wp), intent(in) :: ipoint(nrvart)      ! output parameter pointers
        integer(kind = int_wp), intent(in) :: nocons              ! number of constants
        integer(kind = int_wp), intent(in) :: nopa                ! number of parameters
        integer(kind = int_wp), intent(in) :: nofun               ! number of functions
        integer(kind = int_wp), intent(in) :: nosfun              ! number of segment functions
        integer(kind = int_wp), intent(in) :: notot               ! number of substances
        integer(kind = int_wp), intent(in) :: noseg               ! number of default values
        integer(kind = int_wp), intent(in) :: noloc               ! number of default values
        real(kind = real_wp), intent(out) :: proloc(noloc, noseg) ! process local array
        integer(kind = int_wp), intent(in) :: nodef               ! number of default values
        real(kind = real_wp), intent(in) :: theta(noseg)        ! theta array

        ! local declarations

        character(20) :: parnam                   ! output parameter name
        logical, save :: first = .true.           ! initialisation flag
        integer(kind = int_wp) :: parindx                  ! index in output parameter name array
        integer(kind = int_wp), save :: ip_theta                 ! index of theta in process local array
        integer(kind = int_wp), parameter :: nopred = 6               ! number of predefined outputs
        integer(kind = int_wp) :: iocons                   ! offset to the constants
        integer(kind = int_wp) :: iopa                     ! offset to the parameters
        integer(kind = int_wp) :: iofunc                   ! offset to the functions
        integer(kind = int_wp) :: iosfun                   ! offset to the segment functions
        integer(kind = int_wp) :: ioconc                   ! offset to the concentrations
        integer(kind = int_wp) :: ioloc                    ! offset to the process local array
        integer(kind = int_wp) :: iodef                    ! offset to the process default array
        integer(kind = int_wp) :: iseg                     ! segment loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq_output_theta", ithandl)

        !        initialise

        if (first) then

            first = .false.

            !        pointer offsets

            iocons = nopred + 1
            iopa = iocons + nocons
            iofunc = iopa + nopa
            iosfun = iofunc + nofun
            ioconc = iosfun + nosfun
            ioloc = ioconc + notot
            iodef = ioloc + noloc

            !        look for parameter theta in output

            parnam = 'theta'
            parindx = index_in_array(parnam, ounam)
            if (parindx > 0) then
                ip_theta = ipoint(parindx)
                if (ip_theta >= ioloc .and. ip_theta < iodef) then
                    ip_theta = ip_theta - ioloc + 1
                else
                    ip_theta = -1
                endif
            endif

        endif

        !     fill output array

        if (ip_theta > 0) then
            do iseg = 1, noseg
                proloc(ip_theta, iseg) = theta(iseg)
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq_output_theta
