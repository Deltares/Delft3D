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
module m_delwaq2_main_finalise
    use m_waq_precision

    implicit none

contains
    subroutine delwaq2_main_finalise(action, lunrep, rundat)

        use m_monsys
        use m_actions
        use m_dattim

        implicit none
        integer(kind = int_wp), intent(in) :: action
        character(len = 20), intent(in) :: rundat
        integer(kind = int_wp), intent(in) :: lunrep

        !     Finalise - only if the full computation was done
        if ((action == action_fullcomputation) .or. (action == action_finalisation)) then

            call getmlu(lunrep)
            write (*, *)
            write (*, *) ' SIMULATION ENDED '
            write (*, *)
            write (lunrep, *)
            write (lunrep, '(A)') ' Simulation ended normal'
            call dattim(rundat)
            write (lunrep, '(2A)') ' Execution stop : ', rundat

            close (lunrep)

        end if

    END subroutine delwaq2_main_finalise

end module m_delwaq2_main_finalise
