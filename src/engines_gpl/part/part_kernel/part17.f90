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
module m_part17

    implicit none

contains


    subroutine part17 (itime, nosubs, idtset, idtime, decay, &
            decays)

        !       Deltares Software Centre

        !>\file
        !>         calculates decay coefficients of substances each time step
        !>
        !>         Interpolates linearly in the decay(nosubs,idtset) array.\n
        !>         Time values come in idtime(idtset).\n
        !>         In rdpart it has been checked that first and last time of the
        !>         array equal first and last time of simulation

        !     system administration : Antoon Koster

        !     Created               : February  1990 by Leo Postma

        !     Modified              : July      1992 by Robert Vos : nosubs substances
        !                             July      2011 by Leo Postma : cosmetics

        !     Subroutines called    : none

        !     Functions called      : none

        !     Logical units         : none

        use m_waq_precision    ! single/double precision
        use timers       ! to time the performance
        implicit none    ! explicit typing

        !     Arguments

        !     kind            function         name                    description

        integer  (int_wp), intent(in) :: itime                 !< actual time
        integer  (int_wp), intent(in) :: nosubs                !< number of substances
        integer  (int_wp), intent(in) :: idtset                !< number of time breakpoints
        integer  (int_wp), intent(in) :: idtime(idtset)        !< time breakpoint values
        real     (real_wp), intent(in) :: decay (nosubs, idtset) !< time series of decay factors
        real     (real_wp), intent(out) :: decays(nosubs)        !< interpolated decay factors

        !     locals

        integer(int_wp) :: id, isub    ! loop variables time and substances
        real   (sp) :: fract          ! interpolation factor

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part17", ithndl)

        !     find the moment

        do id = 2, idtset
            if (itime < idtime(id) .or. id == idtset) exit
        enddo

        !     determine interpolation factors

        fract = real(itime - idtime(id - 1)) / real(idtime(id) - idtime(id - 1))

        !     interpolate

        do isub = 1, nosubs
            decays(isub) = decay(isub, id - 1) + fract * (decay(isub, id) - decay(isub, id - 1))
        enddo

        !     end of subroutine

        if (timon) call timstop (ithndl)
        return
        !
    end subroutine

end module m_part17
