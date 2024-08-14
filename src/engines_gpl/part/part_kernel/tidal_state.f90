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

module tidal_state_mod

    use m_waq_precision               ! single/double precision
    implicit none

contains
    subroutine tidal_state(lunrep, itime, zmodel, itstrtp, nosegl, tide_opt, vol1, vol2, vel1, vel2, ebb_flow)

        integer(int_wp), intent(in) :: lunrep              ! report file
        integer(int_wp), intent(in) :: itime               ! time in seconds
        integer(int_wp), intent(in) :: itstrtp             ! start time
        integer(int_wp), intent(in) :: nosegl              ! number segments per layer
        integer(int_wp), intent(in) :: tide_opt            ! option in tidal state determination
        real   (sp), pointer :: vol1  (:)         ! volume begin hydr step
        real   (sp), pointer :: vol2  (:)         ! volume end hydr step
        real   (sp), pointer :: vel1  (:)         ! velocity begin hydr step
        real   (sp), pointer :: vel2  (:)         ! velocity end hydr step
        logical, pointer :: ebb_flow(:)       ! true if flow is ebb

        logical :: zmodel              ! indicates zmodel


        ! local declarations

        logical :: ebb_tide           ! true if waterlevel is ebb
        integer (int_wp) :: iseg
        integer, parameter :: tide_opt_waterlevel = 1
        integer, parameter :: tide_opt_velocity = 2

        if (tide_opt == tide_opt_waterlevel) then
            do iseg = 1, nosegl

                ! eb or flood tide? only looking at volume misses phase difference between waterlevel and the turning of the tidal flow

                if (vol1(iseg) > vol2(iseg)) then
                    ebb_flow(iseg) = .true.
                else
                    ebb_flow(iseg) = .false.
                endif
            enddo
        elseif (tide_opt == tide_opt_velocity) then

            ! initialise ebb_flow

            if (itime == itstrtp) then
                do iseg = 1, nosegl

                    ! eb or flood tide? only looking at volume misses phase difference between waterlevel and the turning of the tidal flow

                    if (vol1(iseg) > vol2(iseg)) then
                        ebb_flow(iseg) = .true.
                    else
                        ebb_flow(iseg) = .false.
                    endif
                enddo
            endif

            do iseg = 1, nosegl

                ! eb or flood tide? only looking at volume misses phase difference between waterlevel and the turning of the tidal flow

                if (vol1(iseg) > vol2(iseg)) then
                    ebb_tide = .true.
                else
                    ebb_tide = .false.
                endif

                ! is the flow turning check by increase of velocity

                if (ebb_tide .and. .not. ebb_flow(iseg)) then
                    if (vel1(iseg) < vel2(iseg)) then
                        ebb_flow(iseg) = .true.
                    endif
                elseif (.not. ebb_tide .and. ebb_flow(iseg)) then
                    if (vel1(iseg) < vel2(iseg)) then
                        ebb_flow(iseg) = .false.
                    endif
                endif

            enddo
        else
            do iseg = 1, nosegl
                ebb_flow(iseg) = .true.
            enddo
            write(lunrep, *) ' no tidal mode selected, fixed on ebb tide'
        endif

        return
    end subroutine
end module
