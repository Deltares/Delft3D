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
module m_delwaq2_main
    use m_waq_precision

    implicit none
    
    private
    public :: dlwqmain

contains

    subroutine dlwqmain(action, argc, argv, dlwqd)
        !! MAIN module for DELWAQ2 , dimensioning of the work array's.

        !DEC$ ATTRIBUTES DLLEXPORT::dlwqmain

        use integration_schemes, only : run_integration_schemes
        use delwaq2_data
        use m_actions, only : action_initialisation, action_fullcomputation
        use m_sysn
        use m_sysi

        integer(kind = int_wp), intent(in) :: action        !! Action to be taken
        integer(kind = int_wp), intent(in) :: argc          !! Number of simulated command-line arguments
        character(len = *), dimension(argc), intent(in) :: argv !! Simulated command-line arguments
        type(delwaq_data) :: dlwqd

        character(len = 20) :: rundat
        logical :: init
        integer(kind = int_wp) :: file_unit

        integer(kind = int_wp), save :: itota
        integer(kind = int_wp), save :: itoti
        integer(kind = int_wp), save :: itotc

        init = action == action_initialisation .or. action == action_fullcomputation

        if (init) then
            call delwaq2_main_init(dlwqd, itota, itoti, itotc, argc, argv)
        end if

        call run_integration_schemes(dlwqd%buffer, itota, itoti, itotc, init, action, dlwqd)
        call delwaq2_main_finalise(action, file_unit, rundat)

    end subroutine dlwqmain

    subroutine delwaq2_main_init(dlwqd, itota, itoti, itotc, argc, argv)

        use delwaq2_data
        use m_cli_utils, only : store_command_arguments
        use m_sysn
        use m_sysi

        ! Arguments
        integer(kind = int_wp) :: max_real_arr_size, max_int_arr_size, max_char_arr_size

        integer(kind = int_wp), intent(in) :: argc
        character(len = *), dimension(argc), intent(in) :: argv
        type(delwaq_data), target :: dlwqd
        type(GridPointerColl), pointer :: GridPs               ! collection of all grid definitions

        integer(kind = int_wp), intent(inout) :: itota
        integer(kind = int_wp), intent(inout) :: itoti
        integer(kind = int_wp), intent(inout) :: itotc
        
        itota = 0
        itoti = 0
        itotc = 0
        
        call store_command_arguments(argv)

        call dlwqd%buffer%intialize()

    end subroutine delwaq2_main_init

    subroutine delwaq2_main_finalise(action, file_unit, rundat)

        use m_monsys
        use m_actions
        use m_dattim

        integer(kind = int_wp), intent(in) :: action
        character(len = 20), intent(in) :: rundat
        integer(kind = int_wp), intent(in) :: file_unit

        !     Finalise - only if the full computation was done
        if ((action == action_fullcomputation) .or. (action == action_finalisation)) then

            call getmlu(file_unit)
            write (*, *)
            write (*, *) ' SIMULATION ENDED '
            write (*, *)
            write (file_unit, *)
            write (file_unit, '(A)') ' Simulation ended normal'
            call dattim(rundat)
            write (file_unit, '(2A)') ' Execution stop : ', rundat

            close (file_unit)

        end if

    end subroutine delwaq2_main_finalise

end module m_delwaq2_main
